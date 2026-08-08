# diag_lm_class.R
# S3 container plus methods for model diagnostics used by the app.

library(lmtest)  # bptest() - Breusch-Pagan test
library(plm)     # panel models and panel diagnostics
library(car)     # vif() - variance inflation factors
library(gt)      # nicely formatted summary tables
library(ggplot2) # diagnostic plots
library(dplyr)   # used in the summary pipeline
library(tibble)  # rownames_to_column()

# ---- Internal helpers for logistic-regression diagnostics ------------------
# These use only base R (stats) so no extra package dependency is required.

#' Hosmer-Lemeshow goodness-of-fit test (internal helper)
#' @noRd
.hosmer_lemeshow <- function(y, prob, g = 10) {
  ord  <- order(prob)
  y    <- y[ord]
  prob <- prob[ord]
  n <- length(y)
  g <- max(2, min(g, n))
  
  breaks <- unique(stats::quantile(seq_len(n), probs = seq(0, 1, length.out = g + 1)))
  group  <- cut(seq_len(n), breaks = breaks, include.lowest = TRUE, labels = FALSE)
  
  obs1 <- tapply(y, group, sum)
  exp1 <- tapply(prob, group, sum)
  n_g  <- tapply(y, group, length)
  obs0 <- n_g - obs1
  exp0 <- n_g - exp1
  
  chisq <- sum((obs1 - exp1)^2 / exp1 + (obs0 - exp0)^2 / exp0, na.rm = TRUE)
  df    <- length(unique(group)) - 2
  pval  <- if (df > 0) stats::pchisq(chisq, df = df, lower.tail = FALSE) else NA_real_
  
  list(statistic = chisq, df = df, p.value = pval)
}

#' McFadden / Cox-Snell / Nagelkerke pseudo R-squared (internal helper)
#' @noRd
.pseudo_r2 <- function(model) {
  mf <- model.frame(model)
  null_model <- stats::glm(mf[[1]] ~ 1, family = stats::binomial())
  
  ll_model <- as.numeric(stats::logLik(model))
  ll_null  <- as.numeric(stats::logLik(null_model))
  n        <- stats::nobs(model)
  
  mcfadden   <- 1 - ll_model / ll_null
  coxsnell   <- 1 - exp((2 / n) * (ll_null - ll_model))
  nagelkerke <- coxsnell / (1 - exp((2 / n) * ll_null))
  
  list(mcfadden = mcfadden, coxsnell = coxsnell, nagelkerke = nagelkerke)
}

#' Area under the ROC curve via the Mann-Whitney U statistic (internal helper)
#' @noRd
.binary_auc <- function(y, prob) {
  pos <- prob[y == 1]
  neg <- prob[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  w <- suppressWarnings(stats::wilcox.test(pos, neg, exact = FALSE))
  as.numeric(w$statistic) / (length(pos) * length(neg))
}

#' Confusion-matrix-based classification metrics (internal helper)
#' @noRd
.confusion_stats <- function(y, prob, threshold = 0.5) {
  pred <- as.numeric(prob >= threshold)
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  
  list(
    accuracy    = (tp + tn) / length(y),
    sensitivity = if ((tp + fn) > 0) tp / (tp + fn) else NA_real_,
    specificity = if ((tn + fp) > 0) tn / (tn + fp) else NA_real_,
    threshold   = threshold,
    tp = tp, tn = tn, fp = fp, fn = fn
  )
}

#' Crude (quasi-)complete separation heuristic (internal helper)
#' @noRd
.check_separation <- function(model) {
  se <- summary(model)$coefficients[, "Std. Error"]
  any(!is.finite(se)) || any(se > 15, na.rm = TRUE)
}

#' Constructor function for 'diag_lm' S3 class
#' 
#' @param model A fitted 'lm', 'plm', or binomial 'glm' object
#' @param data_type Character string: "cross-section", "time-series", "panel",
#'   or "logistic" (aliases "cs", "ts", "binary" are also accepted)
#' @return An object of class 'diag_lm'
new_diag_lm <- function(model, data_type = "cross-section") {
  data_type <- match.arg(
    data_type,
    c("cross-section", "time-series", "panel", "cs", "ts", "logistic", "binary")
  )
  data_type <- switch(
    data_type,
    cs = "cross-section",
    ts = "time-series",
    logistic = "binary",
    data_type
  )
  
  is_binary <- identical(data_type, "binary")
  
  # Validate early so failures are easy to understand. Logistic models need a
  # binomial glm; every other data type still expects an lm/plm object.
  if (is_binary) {
    if (!inherits(model, "glm") || !identical(family(model)$family, "binomial")) {
      stop("Error: data_type = 'logistic' requires a fitted glm(family = binomial(...)) object.", call. = FALSE)
    }
  } else if (!inherits(model, c("lm", "plm"))) {
    stop("Error: Input must be a fitted model of class 'lm' or 'plm'.", call. = FALSE)
  }
  
  # Keep core model pieces for downstream methods.
  model_formula <- formula(model)
  coef_table <- summary(model)$coefficients
  
  # Breusch-Pagan, Durbin-Watson and Shapiro-Wilk all assume a continuous,
  # (approximately) normal residual, so they are not meaningful for a binary
  # response and are skipped for logistic models.
  if (is_binary) {
    bp_result <- NA
    dw_result <- NA
    shapiro_res <- NA
  } else {
    # Breusch-Pagan for heteroskedasticity.
    bp_result <- tryCatch({
      lmtest::bptest(model)
    }, error = function(e) {
      message("Notice: Breusch-Pagan test could not be computed.")
      return(NA)
    })
    
    # Durbin-Watson for serial correlation.
    dw_result <- tryCatch({
      lmtest::dwtest(model)
    }, error = function(e) {
      message("Notice: Durbin-Watson test could not be computed.")
      return(NA)
    })
    
    # Shapiro-Wilk can fail for constant residuals or very large samples.
    shapiro_res <- tryCatch({
      shapiro.test(residuals(model))
    }, error = function(e) {
      return(NA)
    })
  }
  
  # VIF can fail for one-predictor models; keep NA in that case. Valid for
  # both lm/plm and binomial glm objects.
  vif_result <- tryCatch({
    car::vif(model)
  }, error = function(e) {
    message("Notice: VIF could not be computed (likely a single-predictor model).")
    return(NA) 
  })
  
  # Optional diagnostics by data structure.
  ts_diagnostics <- NULL
  panel_diagnostics <- NULL
  
  if (data_type == "time-series") {
    bg_result <- tryCatch({
      lmtest::bgtest(model)
    }, error = function(e) {
      message("Notice: Breusch-Godfrey test could not be computed.")
      return(NA)
    })
    
    lb_result <- tryCatch({
      Box.test(residuals(model), type = "Ljung-Box")
    }, error = function(e) {
      message("Notice: Ljung-Box test could not be computed.")
      return(NA)
    })
    
    ts_diagnostics <- list(
      bg_statistic = if (is.list(bg_result)) unname(bg_result$statistic) else NA,
      bg_pvalue = if (is.list(bg_result)) unname(bg_result$p.value) else NA,
      ljung_box_statistic = if (is.list(lb_result)) unname(lb_result$statistic) else NA,
      ljung_box_pvalue = if (is.list(lb_result)) unname(lb_result$p.value) else NA
    )
  }
  
  if (data_type == "panel") {
    panel_diagnostics <- tryCatch({
      if (!inherits(model, "plm")) {
        stop("Panel diagnostics require a 'plm' pooling model.")
      }
      if (!identical(model$args$model, "pooling")) {
        stop("For data_type = 'panel', pass a pooling plm model.")
      }
      
      panel_formula <- formula(model)
      panel_data <- model.frame(model)
      panel_index <- names(plm::index(model))
      
      fe_model <- plm::plm(
        formula = panel_formula,
        data = panel_data,
        model = "within",
        index = panel_index
      )
      re_model <- plm::plm(
        formula = panel_formula,
        data = panel_data,
        model = "random",
        index = panel_index
      )
      
      panel_bp <- plm::plmtest(model, type = "bp")
      hausman <- plm::phtest(fe_model, re_model)
      wooldridge <- plm::pbgtest(fe_model)
      
      list(
        bp_statistic = unname(panel_bp$statistic),
        bp_pvalue = unname(panel_bp$p.value),
        hausman_statistic = unname(hausman$statistic),
        hausman_pvalue = unname(hausman$p.value),
        wooldridge_statistic = unname(wooldridge$statistic),
        wooldridge_pvalue = unname(wooldridge$p.value)
      )
    }, error = function(e) {
      message("Notice: Panel diagnostics could not be computed.")
      list(
        bp_statistic = NA,
        bp_pvalue = NA,
        hausman_statistic = NA,
        hausman_pvalue = NA,
        wooldridge_statistic = NA,
        wooldridge_pvalue = NA
      )
    })
  }
  
  logistic_diagnostics <- NULL
  
  if (is_binary) {
    y_raw <- model.frame(model)[[1]]
    # Binomial response can arrive as 0/1 numeric, logical, or a 2-level factor.
    y <- if (is.factor(y_raw)) as.numeric(y_raw) - 1 else as.numeric(y_raw)
    fitted_p <- unname(fitted(model))
    
    hl_result <- tryCatch({
      .hosmer_lemeshow(y, fitted_p, g = 10)
    }, error = function(e) {
      message("Notice: Hosmer-Lemeshow test could not be computed.")
      list(statistic = NA, df = NA, p.value = NA)
    })
    
    pr2 <- tryCatch({
      .pseudo_r2(model)
    }, error = function(e) {
      message("Notice: Pseudo R-squared measures could not be computed.")
      list(mcfadden = NA, coxsnell = NA, nagelkerke = NA)
    })
    
    auc_val <- tryCatch({
      .binary_auc(y, fitted_p)
    }, error = function(e) {
      message("Notice: AUC could not be computed.")
      NA_real_
    })
    
    conf <- .confusion_stats(y, fitted_p, threshold = 0.5)
    
    separation_flag <- tryCatch({
      .check_separation(model)
    }, error = function(e) FALSE)
    
    logistic_diagnostics <- list(
      hl_statistic    = hl_result$statistic,
      hl_df           = hl_result$df,
      hl_pvalue       = hl_result$p.value,
      mcfadden_r2     = pr2$mcfadden,
      coxsnell_r2     = pr2$coxsnell,
      nagelkerke_r2   = pr2$nagelkerke,
      auc             = auc_val,
      accuracy        = conf$accuracy,
      sensitivity     = conf$sensitivity,
      specificity     = conf$specificity,
      threshold       = conf$threshold,
      separation_flag = separation_flag
    )
  }
  
  # Store everything in one object.
  obj <- list(
    raw_model     = model,
    data_type     = data_type,
    formula       = model_formula,
    coefficients  = coef_table,
    residuals     = residuals(model),
    fitted_values = fitted(model),
    diagnostics   = list(
      bp_statistic = if (is.list(bp_result)) unname(bp_result$statistic) else NA,
      bp_pvalue    = if (is.list(bp_result)) unname(bp_result$p.value) else NA,
      dw_statistic = if (is.list(dw_result)) unname(dw_result$statistic) else NA,
      dw_pvalue    = if (is.list(dw_result)) unname(dw_result$p.value) else NA,
      vif_scores   = vif_result,
      shapiro_pvalue = if (is.list(shapiro_res)) unname(shapiro_res$p.value) else NA,
      ts           = ts_diagnostics,
      panel        = panel_diagnostics,
      logistic     = logistic_diagnostics
    )
  )
  
  # Set class for S3 dispatch.
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
  
  # Compact console overview.
  cat("Diagnostic Linear Model (diag_lm)\n")
  cat("---------------------------------\n")
  cat("Formula: ")
  print(x$formula)
  cat("\nCoefficients:\n")
  print(round(x$coefficients, 4))
  
  cat("\nDiagnostics:\n")
  
  if (identical(x$data_type, "binary")) {
    lg <- x$diagnostics$logistic
    fmt <- function(v) if (is.null(v) || is.na(v)) "N/A" else sprintf("%.4f", v)
    
    cat(sprintf("  Hosmer-Lemeshow p-value : %s\n", fmt(lg$hl_pvalue)))
    cat(sprintf("  McFadden pseudo R^2     : %s\n", fmt(lg$mcfadden_r2)))
    cat(sprintf("  AUC                     : %s\n", fmt(lg$auc)))
    cat(sprintf("  Accuracy (0.5 cutoff)   : %s\n", fmt(lg$accuracy)))
    if (isTRUE(lg$separation_flag)) {
      cat("  Warning: possible (quasi-)complete separation detected.\n")
    }
  } else {
    cat(sprintf("  Breusch-Pagan statistic : %.4f\n", x$diagnostics$bp_statistic))
    cat(sprintf("  Breusch-Pagan p-value   : %.4f", x$diagnostics$bp_pvalue))
    
    # Small p-value suggests heteroskedasticity.
    if (!is.na(x$diagnostics$bp_pvalue) && x$diagnostics$bp_pvalue < 0.05) {
      cat("  (evidence of heteroskedasticity)\n")
    } else {
      cat("  (no evidence of heteroskedasticity)\n")
    }
  }
  
  cat("  VIF scores              :\n")
  # VIF is NA for one-predictor models.
  if (length(x$diagnostics$vif_scores) == 1 && is.na(x$diagnostics$vif_scores)) {
    cat("    Not available (single-predictor model).\n")
  } else {
    print(round(x$diagnostics$vif_scores, 4))
  }
  
  invisible(x)
}


#' Custom summary method for 'diag_lm'
#'
#' Returns a formatted gt summary table that adapts to the selected
#' data structure.
#'
#' @param object An object of class 'diag_lm'
#' @param ... Additional arguments
#' @return A 'gt_tbl' object
#' @export
summary.diag_lm <- function(object, ...) {
  
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  # Keep term names as a regular column for gt.
  coef_df <- as.data.frame(object$coefficients) |>
    rownames_to_column("Term")
  
  data_type <- if (is.null(object$data_type)) "cross-section" else object$data_type
  bp_pval <- round(object$diagnostics$bp_pvalue, 4)
  dw_pval <- round(object$diagnostics$dw_pvalue, 4)
  shapiro_pval <- if (is.null(object$diagnostics$shapiro_pvalue)) {
    NA
  } else {
    object$diagnostics$shapiro_pvalue
  }
  shapiro_text <- if (is.na(shapiro_pval)) "N/A" else round(shapiro_pval, 4)
  
  # Logistic (binary) layout with fit / discrimination diagnostics.
  if (identical(data_type, "binary")) {
    lg <- object$diagnostics$logistic
    
    coef_df$`Odds Ratio` <- exp(coef_df$Estimate)
    
    fmt <- function(v) if (is.null(v) || is.na(v)) "N/A" else round(v, 4)
    
    summary_table <- gt(coef_df) |>
      tab_header(
        title = "Logistic Regression & Diagnostics Summary",
        subtitle = paste(
          "Hosmer-Lemeshow p-value:", fmt(lg$hl_pvalue),
          "| McFadden R2:", fmt(lg$mcfadden_r2),
          "| AUC:", fmt(lg$auc),
          "| Accuracy (0.5 cutoff):", fmt(lg$accuracy)
        )
      ) |>
      fmt_number(
        columns = setdiff(names(coef_df), "Term"),
        decimals = 3
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
    
    if (!is.null(lg) && isTRUE(lg$separation_flag)) {
      summary_table <- summary_table |>
        tab_source_note(source_note = "Warning: possible (quasi-)complete separation detected (very large standard errors).")
    }
    
    return(summary_table)
  }
  
  # Cross-section layout.
  if (identical(data_type, "cross-section")) {
    summary_table <- gt(coef_df) |>
      tab_header(
        title = "OLS Regression & Diagnostics Summary",
        subtitle = paste(
          "Breusch-Pagan Test p-value:", bp_pval,
          "| Durbin-Watson Test p-value:", dw_pval,
          "| Shapiro-Wilk Test p-value:", shapiro_text
        )
      ) |>
      fmt_number(
        columns = 2:ncol(coef_df),
        decimals = 3
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      )
    
    return(summary_table)
  }
  
  # Time-series layout with BG and Ljung-Box notes.
  if (identical(data_type, "time-series")) {
    bg_stat <- object$diagnostics$ts$bg_statistic
    bg_pval <- object$diagnostics$ts$bg_pvalue
    lb_stat <- object$diagnostics$ts$ljung_box_statistic
    lb_pval <- object$diagnostics$ts$ljung_box_pvalue
    
    bg_outcome <- if (!is.na(bg_pval) && bg_pval < 0.05) {
      "Serial correlation detected"
    } else {
      "No serial correlation evidence"
    }
    
    lb_outcome <- if (!is.na(lb_pval) && lb_pval < 0.05) {
      "Autocorrelation detected"
    } else {
      "No autocorrelation evidence"
    }
    
    summary_table <- gt(coef_df) |>
      tab_header(
        title = "Time-Series OLS Summary",
        subtitle = "Coefficient estimates with serial-correlation checks"
      ) |>
      fmt_number(
        columns = 2:ncol(coef_df),
        decimals = 3
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_source_note(
        source_note = paste0(
          "Breusch-Godfrey: statistic = ", round(bg_stat, 4),
          ", p-value = ", round(bg_pval, 4),
          " (", bg_outcome, ")"
        )
      ) |>
      tab_source_note(
        source_note = paste0(
          "Ljung-Box: statistic = ", round(lb_stat, 4),
          ", p-value = ", round(lb_pval, 4),
          " (", lb_outcome, ")"
        )
      ) |>
      tab_source_note(
        source_note = paste0(
          "Shapiro-Wilk normality p-value: ", shapiro_text
        )
      )
    
    return(summary_table)
  }
  
  # Panel layout focused on model-specification tests.
  if (identical(data_type, "panel")) {
    panel_df <- data.frame(
      Test = c("Panel Breusch-Pagan (LM)", "Hausman"),
      Statistic = c(
        object$diagnostics$panel$bp_statistic,
        object$diagnostics$panel$hausman_statistic
      ),
      `p-value` = c(
        object$diagnostics$panel$bp_pvalue,
        object$diagnostics$panel$hausman_pvalue
      ),
      Outcome = c(
        if (!is.na(object$diagnostics$panel$bp_pvalue) && object$diagnostics$panel$bp_pvalue < 0.05) {
          "Panel effects detected"
        } else {
          "No panel effects evidence"
        },
        if (!is.na(object$diagnostics$panel$hausman_pvalue) && object$diagnostics$panel$hausman_pvalue < 0.05) {
          "Prefer Fixed Effects"
        } else {
          "Random Effects remains admissible"
        }
      ),
      check.names = FALSE
    )
    
    summary_table <- gt(panel_df) |>
      tab_header(
        title = "Panel Model Specification Summary",
        subtitle = "Hausman and Panel Breusch-Pagan diagnostic checks"
      ) |>
      fmt_number(
        columns = 2:3,
        decimals = 4
      ) |>
      tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
      ) |>
      tab_source_note(
        source_note = paste0(
          "Shapiro-Wilk normality p-value: ", shapiro_text
        )
      )
    
    return(summary_table)
  }
  
  # Fallback layout.
  summary_table <- gt(coef_df) |>
    tab_header(
      title = "OLS Regression & Diagnostics Summary",
      subtitle = paste(
        "Breusch-Pagan Test p-value:", bp_pval,
        "| Durbin-Watson Test p-value:", dw_pval,
        "| Shapiro-Wilk Test p-value:", shapiro_text
      )
    ) |>
    fmt_number(
      columns = 2:ncol(coef_df),
      decimals = 3
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
  
  return(summary_table)
}


#' S3 generic for remediation advice
#'
#' @param object Model diagnostics object
#' @param ... Additional arguments
#' @return Character vector of HTML-formatted messages
#' @export
remediation_advice <- function(object, ...) {
  UseMethod("remediation_advice")
}


#' Remediation advice for diag_lm objects
#'
#' Returns HTML alert messages based on stored diagnostic p-values.
#'
#' @param object An object of class 'diag_lm'
#' @param ... Additional arguments
#' @return Character vector of HTML-formatted messages
#' @export
remediation_advice.diag_lm <- function(object, ...) {
  if (!inherits(object, "diag_lm")) {
    stop("Error: Object must be of class 'diag_lm'.", call. = FALSE)
  }
  
  alerts <- character(0)
  
  if (object$data_type %in% c("cross-section", "cs")) {
    bp_pvalue <- object$diagnostics$bp_pvalue
    if (!is.na(bp_pvalue) && bp_pvalue < 0.05) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'>Warning: Heteroskedasticity detected. Consider using robust standard errors.</div>"
      )
    }
  }
  
  if (object$data_type %in% c("time-series", "ts")) {
    bg_pvalue <- object$diagnostics$ts$bg_pvalue
    ljung_box_pvalue <- object$diagnostics$ts$ljung_box_pvalue
    
    if ((!is.na(bg_pvalue) && bg_pvalue < 0.05) ||
        (!is.na(ljung_box_pvalue) && ljung_box_pvalue < 0.05)) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'>Warning: Residual autocorrelation detected. Consider adjusting lagging variables.</div>"
      )
    }
  }
  
  if (identical(object$data_type, "panel")) {
    hausman_pvalue <- object$diagnostics$panel$hausman_pvalue
    if (!is.na(hausman_pvalue) && hausman_pvalue < 0.05) {
      alerts <- c(
        alerts,
        "<div class='alert alert-info'>Advice: Hausman test rejects Random Effects. Use a Fixed Effects (Within) model specification.</div>"
      )
    }
  }
  
  if (object$data_type %in% c("binary", "logistic")) {
    lg <- object$diagnostics$logistic
    
    if (!is.null(lg) && !is.na(lg$hl_pvalue) && lg$hl_pvalue < 0.05) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'>Warning: Hosmer-Lemeshow test rejects adequate fit. Consider adding non-linear terms or interactions.</div>"
      )
    }
    
    if (!is.null(lg) && isTRUE(lg$separation_flag)) {
      alerts <- c(
        alerts,
        "<div class='alert alert-warning'><b>Warning: Possible (quasi-)complete separation.</b><br/>Coefficient standard errors are implausibly large. Consider Firth's penalized logistic regression or removing a perfectly-separating predictor.</div>"
      )
    }
    
    if (!is.null(lg) && !is.na(lg$auc) && lg$auc < 0.7) {
      alerts <- c(
        alerts,
        paste0(
          "<div class='alert alert-info'>Advice: AUC = ", round(lg$auc, 3),
          " indicates weak discrimination. Consider additional predictors or interaction terms.</div>"
        )
      )
    }
  }
  
  shapiro_pval <- object$diagnostics$shapiro_pvalue
  if (!is.null(shapiro_pval) && !is.na(shapiro_pval) && shapiro_pval < 0.05) {
    alerts <- c(
      alerts,
      paste0(
        "<div class='alert alert-warning'><b>Warning: Normality Assumption Violated!</b><br/>",
        "The Shapiro-Wilk test rejects residual normality (p = ",
        round(shapiro_pval, 4),
        "). Coefficients can remain unbiased, but t/F tests may be unreliable in small samples. ",
        "Check the Q-Q plot for heavy tails or outliers, and consider a log or other transformation.</div>"
      )
    )
  }
  
  return(alerts)
}


#' Custom plot method for 'diag_lm'
#'
#' Draws a diagnostic plot from the stored residuals/fitted values.
#' The 'type' argument picks one of:
#'   "residuals"        - residuals vs fitted (heteroskedasticity / non-linearity)
#'   "qq"               - normal Q-Q plot (normality of residuals)
#'   "scale_location"   - scale-location plot (spread of residuals)
#'   "histogram"        - histogram of the residuals (normality / skew)
#'   "acf"              - residual ACF bars (serial correlation)
#'   "pacf"             - residual PACF bars (time-series only)
#'   "residuals_time"   - residuals over time (time-series only)
#'   "binned_residuals" - binned residual plot (logistic only)
#'   "roc"              - ROC curve (logistic only)
#'   "calibration"      - calibration plot (logistic only)
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
  
  # Build a plotting frame once and reuse it.
  std_resid <- x$residuals / sd(x$residuals)
  plot_data <- data.frame(
    fitted     = x$fitted_values,
    residuals  = x$residuals,
    std_resid  = std_resid
  )
  
  # Residuals vs fitted.
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
    
    # Normal Q-Q.
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
    
    # Scale-location view.
    plot_data$root_abs_resid <- sqrt(abs(plot_data$std_resid))
    
    p <- ggplot(plot_data, aes_string(x = "fitted", y = "root_abs_resid")) +
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
    
    # Residual histogram.
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
    
  } else if (type == "acf") {
    
    # Residual autocorrelation bars.
    acf_obj <- acf(x$residuals, plot = FALSE, na.action = na.pass)
    acf_df <- data.frame(
      lag = as.numeric(acf_obj$lag),
      acf = as.numeric(acf_obj$acf)
    )
    
    p <- ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Residual Autocorrelation (ACF)",
        subtitle = "Visual check for serial correlation across lags",
        x = "Lag",
        y = "ACF"
      )
    
    return(p)
    
  } else if (type == "pacf") {
    
    # Residual partial-autocorrelation bars: isolates the correlation at each
    # lag after removing the effect of shorter lags, which is often a clearer
    # signal of remaining serial-correlation order than the ACF alone.
    pacf_obj <- pacf(x$residuals, plot = FALSE, na.action = na.pass)
    pacf_df <- data.frame(
      lag  = as.numeric(pacf_obj$lag),
      pacf = as.numeric(pacf_obj$acf)
    )
    
    p <- ggplot(pacf_df, aes(x = lag, y = pacf)) +
      geom_col(fill = "steelblue", alpha = 0.85) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Residual Partial Autocorrelation (PACF)",
        subtitle = "Visual check for the order of remaining serial correlation",
        x = "Lag",
        y = "PACF"
      )
    
    return(p)
    
  } else if (type == "residuals_time") {
    
    # Residuals plotted against their time index: the primary visual check
    # for serial structure, which residuals-vs-fitted cannot show.
    plot_data$time <- seq_along(x$residuals)
    
    p <- ggplot(plot_data, aes(x = time, y = residuals)) +
      geom_line(color = "steelblue", alpha = 0.7) +
      geom_point(color = "steelblue", size = 1.2, alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      theme_minimal() +
      labs(
        title = "Residuals over Time",
        subtitle = "Checks for serial patterns that indicate autocorrelation",
        x = "Time Index",
        y = "Residuals"
      )
    
    return(p)
    
  } else if (type == "binned_residuals") {
    
    if (!identical(x$data_type, "binary")) {
      stop("Error: The 'binned_residuals' plot is only available for logistic (binary) models.", call. = FALSE)
    }
    
    prob <- x$fitted_values
    resp_resid <- residuals(x$raw_model, type = "response")
    n <- length(prob)
    n_bins <- max(4, round(sqrt(n)))
    
    bin_df <- data.frame(prob = prob, resid = resp_resid)
    bin_df <- bin_df[order(bin_df$prob), ]
    bin_df$bin <- cut(seq_len(n), breaks = n_bins, labels = FALSE)
    
    binned <- stats::aggregate(cbind(prob, resid) ~ bin, data = bin_df, FUN = mean)
    bin_n  <- stats::aggregate(resid ~ bin, data = bin_df, FUN = length)$resid
    binned$se2 <- 2 * sqrt(pmax(binned$prob * (1 - binned$prob), 1e-6) / bin_n)
    
    p <- ggplot(binned, aes(x = prob, y = resid)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkred") +
      geom_line(aes(y = se2), color = "grey60", linetype = "dotted") +
      geom_line(aes(y = -se2), color = "grey60", linetype = "dotted") +
      geom_point(color = "steelblue", size = 2) +
      theme_minimal() +
      labs(
        title = "Binned Residual Plot",
        subtitle = "Average residual per bin of predicted probability (dotted = approx. 95% band)",
        x = "Average Predicted Probability",
        y = "Average Residual (Observed - Predicted)"
      )
    
    return(p)
    
  } else if (type == "roc") {
    
    if (!identical(x$data_type, "binary")) {
      stop("Error: The 'roc' plot is only available for logistic (binary) models.", call. = FALSE)
    }
    
    y_raw <- model.frame(x$raw_model)[[1]]
    y <- if (is.factor(y_raw)) as.numeric(y_raw) - 1 else as.numeric(y_raw)
    prob <- x$fitted_values
    
    thresholds <- sort(unique(c(0, prob, 1)), decreasing = TRUE)
    roc_points <- do.call(rbind, lapply(thresholds, function(th) {
      pred <- as.numeric(prob >= th)
      tp <- sum(pred == 1 & y == 1)
      fn <- sum(pred == 0 & y == 1)
      fp <- sum(pred == 1 & y == 0)
      tn <- sum(pred == 0 & y == 0)
      data.frame(
        tpr = if ((tp + fn) > 0) tp / (tp + fn) else 0,
        fpr = if ((fp + tn) > 0) fp / (fp + tn) else 0
      )
    }))
    roc_points <- roc_points[order(roc_points$fpr, roc_points$tpr), ]
    
    p <- ggplot(roc_points, aes(x = fpr, y = tpr)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
      coord_equal() +
      theme_minimal() +
      labs(
        title = "ROC Curve",
        subtitle = "Discrimination of the fitted logistic model (dashed = chance line)",
        x = "False Positive Rate",
        y = "True Positive Rate"
      )
    
    return(p)
    
  } else if (type == "calibration") {
    
    if (!identical(x$data_type, "binary")) {
      stop("Error: The 'calibration' plot is only available for logistic (binary) models.", call. = FALSE)
    }
    
    y_raw <- model.frame(x$raw_model)[[1]]
    y <- if (is.factor(y_raw)) as.numeric(y_raw) - 1 else as.numeric(y_raw)
    prob <- x$fitted_values
    n <- length(prob)
    n_bins <- min(10, max(4, round(n / 20)))
    
    cal_df <- data.frame(y = y, prob = prob)
    cal_df <- cal_df[order(cal_df$prob), ]
    cal_df$bin <- cut(seq_len(n), breaks = n_bins, labels = FALSE)
    
    cal_summary <- stats::aggregate(cbind(prob, y) ~ bin, data = cal_df, FUN = mean)
    
    p <- ggplot(cal_summary, aes(x = prob, y = y)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkred") +
      geom_point(color = "steelblue", size = 2.5) +
      geom_line(color = "steelblue", alpha = 0.5) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      theme_minimal() +
      labs(
        title = "Calibration Plot",
        subtitle = "Mean predicted probability vs. observed proportion, by bin (dashed = perfect calibration)",
        x = "Mean Predicted Probability",
        y = "Observed Proportion of Y = 1"
      )
    
    return(p)
    
  } else {
    # Unknown plot type.
    stop("Error: Plot type '", type, "' is not implemented yet.", call. = FALSE)
  }
}
