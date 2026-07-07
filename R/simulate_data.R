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
