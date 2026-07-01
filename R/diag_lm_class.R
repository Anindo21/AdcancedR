# diag_lm_class.R
# Defines a small S3 class that wraps a fitted lm() model together with the
# two diagnostics we care about in this project: the Breusch-Pagan test for
# heteroskedasticity and VIF for multicollinearity. Bundling everything into
# one object means the rest of the app never has to re-run the statistics.

library(lmtest)  # bptest() - Breusch-Pagan test
library(car)     # vif() - variance inflation factors
library(gt)      # nicely formatted summary tables
library(ggplot2) # diagnostic plots
library(dplyr)   # used in the summary pipeline
library(tibble)  # rownames_to_column()

#' Constructor function for 'diag_lm' S3 class
#' 
#' @param model A fitted 'lm' object
#' @return An object of class 'diag_lm'
new_diag_lm <- function(model) {
  
  # Check the input first so we get a clear error instead of a confusing one
  # further down if someone passes, say, a data frame by mistake.
  if (!inherits(model, "lm")) {
    stop("Error: Input must be a fitted linear model of class 'lm'.", call. = FALSE)
  }
  
  # Pull out the pieces we need later for printing and plotting.
  model_formula <- formula(model)
  coef_table <- summary(model)$coefficients
  
  # Breusch-Pagan test for non-constant error variance.
  bp_result <- lmtest::bptest(model)
  
  # VIF needs at least two predictors, otherwise car::vif() errors out.
  # tryCatch lets a single-predictor model still work and just store NA.
  vif_result <- tryCatch({
    car::vif(model)
  }, error = function(e) {
    message("Notice: VIF could not be computed (likely a single-predictor model).")
    return(NA) 
  })
  
  # Store everything in one list. The diagnostics live in their own sub-list
  # so they are easy to reach from the print/summary/plot methods.
  obj <- list(
    raw_model     = model,                  # keep the original model around
    formula       = model_formula,
    coefficients  = coef_table,
    residuals     = model$residuals,
    fitted_values = model$fitted.values,
    diagnostics   = list(
      bp_statistic = unname(bp_result$statistic),
      bp_pvalue    = unname(bp_result$p.value),
      vif_scores   = vif_result
    )
  )
  
  # Tagging the list with a class is what lets print()/summary()/plot()
  # dispatch to our custom methods below.
  class(obj) <- "diag_lm"
  
  return(obj)
}


#' Print method for 'diag_lm' objects
#'
#' @param x An object of class 'diag_lm'
#' @param ... Further arguments passed to or from other methods
#' @return Invisibly returns the object
print.diag_lm <- function(x, ...) {
  
  if (!inherits(x, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # A compact text overview for quick checks in the console.
  cat("Diagnostic Linear Model (diag_lm)\n")
  cat("---------------------------------\n")
  cat("Formula: ")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(round(x$coefficients, 4))
  
  cat("\nDiagnostics:\n")
  cat(sprintf("  Breusch-Pagan statistic : %.4f\n", x$diagnostics$bp_statistic))
  cat(sprintf("  Breusch-Pagan p-value   : %.4f", x$diagnostics$bp_pvalue))
  
  # A small p-value means we reject constant variance.
  if (x$diagnostics$bp_pvalue < 0.05) {
    cat("  (evidence of heteroskedasticity)\n")
  } else {
    cat("  (no evidence of heteroskedasticity)\n")
  }
  
  cat("  VIF scores              :\n")
  # vif_scores is NA when the model only has one predictor.
  if (length(x$diagnostics$vif_scores) == 1 && is.na(x$diagnostics$vif_scores)) {
    cat("    Not available (single-predictor model).\n")
  } else {
    print(round(x$diagnostics$vif_scores, 4))
  }
  
  invisible(x)
}


#' Custom summary method for 'diag_lm'
#'
#' Returns a formatted gt table of the coefficients with the Breusch-Pagan
#' p-value shown in the subtitle, so it can be dropped straight into the app.
#'
#' @param object An object of class 'diag_lm'
#' @param ... Additional arguments
#' @return A 'gt_tbl' object
#' @export
summary.diag_lm <- function(object, ...) {
  
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # The coefficient table is a matrix with row names; turn it into a data
  # frame with a proper "Term" column so gt can display the predictor names.
  coef_df <- as.data.frame(object$coefficients) |>
    rownames_to_column("Term")
  
  bp_pval <- round(object$diagnostics$bp_pvalue, 4)
  
  # Build the table: title, BP p-value subtitle, 3-decimal numbers and
  # bold column headers.
  summary_table <- gt(coef_df) |>
    tab_header(
      title = "OLS Regression & Diagnostics Summary",
      subtitle = paste("Breusch-Pagan Test p-value:", bp_pval)
    ) |>
    fmt_number(
      columns = -Term, # every column except the term names is numeric
      decimals = 3
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
  
  return(summary_table)
}


#' Custom plot method for 'diag_lm'
#'
#' Draws a diagnostic plot from the residuals/fitted values stored in the
#' object. The 'type' argument leaves room to add more plots later.
#'
#' @param x An object of class 'diag_lm'
#' Custom plot method for 'diag_lm'
#'
#' Draws a diagnostic plot from the residuals/fitted values stored in the
#' object. The 'type' argument picks which one:
#'   "residuals"      - residuals vs fitted (heteroskedasticity / non-linearity)
#'   "qq"             - normal Q-Q plot (normality of residuals)
#'   "scale_location" - scale-location plot (spread of residuals)
#'   "histogram"      - histogram of the residuals (normality / skew)
#'
#' @param x An object of class 'diag_lm'
#' @param type String indicating which diagnostic plot to show
#' @param ... Additional arguments
#' @return A 'ggplot' object
#' @export
plot.diag_lm <- function(x, type = "residuals", ...) {
  
  if (!inherits(x, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # ggplot wants a data frame, so combine the two stored vectors.
  # Standardised residuals are handy for the Q-Q and scale-location plots.
  std_resid <- x$residuals / sd(x$residuals)
  plot_data <- data.frame(
    fitted     = x$fitted_values,
    residuals  = x$residuals,
    std_resid  = std_resid
  )
  
  # Residuals vs fitted: a funnel shape here is a sign of heteroskedasticity.
  if (type == "residuals") {
    
    p <- ggplot(plot_data, aes(x = fitted, y = residuals)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Residuals vs Fitted",
        subtitle = "Checks for non-linear patterns and heteroskedasticity",
        x = "Fitted Values",
        y = "Residuals"
      )
    
    return(p)
    
  } else if (type == "qq") {
    
    # Q-Q plot: points should follow the diagonal line if the residuals
    # are roughly normal. Curving away at the ends means heavy tails / skew.
    p <- ggplot(plot_data, aes(sample = std_resid)) +
      stat_qq(alpha = 0.6, color = "steelblue", size = 2) +
      stat_qq_line(color = "darkred", linetype = "dashed") +
      theme_minimal() +
      labs(
        title = "Normal Q-Q",
        subtitle = "Checks whether the residuals are normally distributed",
        x = "Theoretical Quantiles",
        y = "Standardised Residuals"
      )
    
    return(p)
    
  } else if (type == "scale_location") {
    
    # Scale-location: square root of the absolute standardised residuals.
    # A rising trend means the spread grows with the fitted value
    # (another way to spot heteroskedasticity).
    plot_data$root_abs_resid <- sqrt(abs(plot_data$std_resid))
    
    p <- ggplot(plot_data, aes(x = fitted, y = root_abs_resid)) +
      geom_point(alpha = 0.6, color = "steelblue", size = 2) +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
      theme_minimal() +
      labs(
        title = "Scale-Location",
        subtitle = "Checks whether residual spread is constant",
        x = "Fitted Values",
        y = expression(sqrt("|Standardised Residuals|"))
      )
    
    return(p)
    
  } else if (type == "histogram") {
    
    # Histogram of the raw residuals - a quick look at their distribution.
    p <- ggplot(plot_data, aes(x = residuals)) +
      geom_histogram(aes(y = after_stat(density)),
                     bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
      geom_density(color = "darkorange", linewidth = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Distribution of Residuals",
        subtitle = "Checks for skew and departures from normality",
        x = "Residuals",
        y = "Density"
      )
    
    return(p)
    
  } else {
    # Guard against typos / not-yet-added plot types.
    stop("Error: Plot type '", type, "' is not implemented yet.", call. = FALSE)
  }
}
