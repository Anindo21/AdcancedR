# simulate_data.R
# Small data generator for demo/testing.
# You can optionally inject heteroskedasticity or multicollinearity.

#' Simulate OLS Data with Optional Violations
#'
#' @param n Integer. Number of observations.
#' @param heteroskedastic Logical. If TRUE, error variance scales with X1.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param seed Optional integer. If given, sets the RNG seed so the data is
#'   reproducible; leave as NULL for a fresh random draw each call.
#' @return A data.frame containing Y, X1, and X2.
#' @export
simulate_ols_data <- function(n = 500, heteroskedastic = FALSE,
                              multicollinear = FALSE, seed = NULL) {
  
  # Use a fixed seed when reproducibility is needed.
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # First predictor.
  X1 <- rnorm(n, mean = 5, sd = 2)
  
  # If requested, make X2 highly correlated with X1.
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.5)
  } else {
    X2 <- rnorm(n, mean = 10, sd = 3)
  }
  
  # If requested, error spread grows with X1.
  if (heteroskedastic) {
    errors <- rnorm(n, mean = 0, sd = 1 + 1.5 * abs(X1))
  } else {
    errors <- rnorm(n, mean = 0, sd = 2)
  }
  
  # Data generating process.
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + errors
  
  data.frame(Y = Y, X1 = X1, X2 = X2)
}


#' Simulate Time-Series Data with Optional Violations
#'
#' Generates a univariate time index and predictors with optional
#' multicollinearity. The disturbance follows an AR(1) process so residual
#' autocorrelation is present in a realistic way.
#'
#' @param n Integer. Number of time points.
#' @param heteroskedastic Logical. If TRUE, innovation variance scales with X1.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param seed Optional integer for reproducibility.
#' @param phi Numeric AR(1) coefficient for the error process.
#' @return A data.frame containing t, Y, X1, and X2.
#' @export
simulate_ts_data <- function(n = 500, heteroskedastic = FALSE,
                             multicollinear = FALSE, seed = NULL,
                             phi = 0.6) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  t <- seq_len(n)
  
  # Add mild persistence to predictors so the series looks time-like.
  X1 <- as.numeric(arima.sim(model = list(ar = 0.4), n = n, sd = 1.2)) + 5
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.35)
  } else {
    X2 <- as.numeric(arima.sim(model = list(ar = 0.25), n = n, sd = 1.5)) + 10
  }
  
  # Innovation process, optionally heteroskedastic.
  if (heteroskedastic) {
    innov <- rnorm(n, mean = 0, sd = 0.7 + 0.45 * abs(X1 - mean(X1)))
  } else {
    innov <- rnorm(n, mean = 0, sd = 1.2)
  }
  
  # AR(1) error process to induce serial correlation.
  e <- numeric(n)
  e[1] <- innov[1]
  for (i in 2:n) {
    e[i] <- phi * e[i - 1] + innov[i]
  }
  
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + e
  
  data.frame(t = t, Y = Y, X1 = X1, X2 = X2)
}


#' Simulate Binary (Logistic) Data with Optional Violations
#'
#' Generates a binary outcome from a known logistic data-generating process,
#' Y ~ Bernoulli(plogis(b0 + b1*X1 + b2*X2)), with optional violations that
#' the diag_lm logistic diagnostics are designed to detect.
#'
#' @param n Integer. Number of observations.
#' @param multicollinear Logical. If TRUE, X2 is highly correlated with X1.
#' @param nonlinear Logical. If TRUE, an omitted quadratic term in X1 is added
#'   to the true linear predictor, so the fitted (linear) logit model is
#'   mis-specified. Detectable via the Hosmer-Lemeshow test / binned residual
#'   plot.
#' @param imbalance Logical. If TRUE, shifts the intercept so the positive
#'   class is rare (~10 percent), producing a class-imbalanced outcome.
#' @param seed Optional integer. If given, sets the RNG seed so the data is
#'   reproducible; leave as NULL for a fresh random draw each call.
#' @return A data.frame containing Y (0/1), X1, and X2.
#' @export
simulate_logit_data <- function(n = 500, multicollinear = FALSE,
                                nonlinear = FALSE, imbalance = FALSE,
                                seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  X1 <- rnorm(n, mean = 0, sd = 1.5)
  
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.3)
  } else {
    X2 <- rnorm(n, mean = 0, sd = 1.5)
  }
  
  intercept <- if (imbalance) -3 else 0
  
  linear_predictor <- intercept + (1.4 * X1) - (1.1 * X2)
  
  # An omitted non-linear term breaks the linearity-in-the-logit assumption
  # while the fitted model (Y ~ X1 + X2) stays linear, so the mis-specification
  # shows up in the Hosmer-Lemeshow test and the binned residual plot.
  if (nonlinear) {
    linear_predictor <- linear_predictor + 0.6 * X1^2
  }
  
  prob <- plogis(linear_predictor)
  Y <- rbinom(n, size = 1, prob = prob)
  
  data.frame(Y = Y, X1 = X1, X2 = X2)
}
