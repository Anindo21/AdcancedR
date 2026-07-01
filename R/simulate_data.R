# simulate_data.R
# Generates fake OLS data so we can demonstrate the diagnostics. The two
# switches let us deliberately break a classical assumption: either make the
# error variance depend on X1 (heteroskedasticity) or make X2 nearly a copy
# of X1 (multicollinearity). With both switches off the data satisfies the
# usual assumptions and the tests should come back clean.

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
  
  # Fix the random numbers when a seed is supplied so the same inputs always
  # give the same dataset (handy for demos and grading).
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # First predictor.
  X1 <- rnorm(n, mean = 5, sd = 2)
  
  # Second predictor. When multicollinear is on, X2 is X1 with only a little
  # noise added, so the two columns carry almost the same information.
  if (multicollinear) {
    X2 <- X1 + rnorm(n, mean = 0, sd = 0.5)
  } else {
    X2 <- rnorm(n, mean = 10, sd = 3)
  }
  
  # Errors. With heteroskedasticity the standard deviation grows with X1,
  # which produces the fan shape in the residual plot. abs() keeps the sd
  # positive.
  if (heteroskedastic) {
    errors <- rnorm(n, mean = 0, sd = 1 + 1.5 * abs(X1))
  } else {
    errors <- rnorm(n, mean = 0, sd = 2)
  }
  
  # True model: intercept 10, slope on X1 = 2.5, slope on X2 = -1.5.
  Y <- 10 + (2.5 * X1) - (1.5 * X2) + errors
  
  data.frame(Y = Y, X1 = X1, X2 = X2)
}
