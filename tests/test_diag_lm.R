# Quick checks for the diag_lm class. These are not a formal testing
# framework, just a script that exercises the main paths (a normal model,
# the single-predictor edge case, and the bad-input error) and stops with
# an error via stopifnot() if anything is off.
# Run from the project root with: source("tests/test_diag_lm.R")

source("R/diag_lm_class.R")

# Normal case: a model with several predictors.
cat("== Test 1: Multi-predictor model ==\n")
my_model <- lm(mpg ~ wt + hp + disp, data = mtcars)
my_diag_obj <- new_diag_lm(my_model)

stopifnot(inherits(my_diag_obj, "diag_lm"))
cat("Class check passed:", class(my_diag_obj), "\n\n")

print(my_diag_obj)

cat("\n== Test: summary() returns a gt table ==\n")
tbl <- summary(my_diag_obj)
stopifnot(inherits(tbl, "gt_tbl"))
cat("summary() returned a gt_tbl object.\n")

cat("\n== Test: plot() returns a ggplot object ==\n")
for (t in c("residuals", "qq", "scale_location", "histogram")) {
  p <- plot(my_diag_obj, type = t)
  stopifnot(inherits(p, "ggplot"))
  cat("plot(type =", t, ") returned a ggplot object.\n")
}

cat("\n== Test: unknown plot type errors ==\n")
bad_type <- tryCatch(
  plot(my_diag_obj, type = "qqplot"),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", bad_type, "\n")

cat("\n== Test 2: Single-predictor model (VIF should be NA) ==\n")
simple_model <- lm(mpg ~ wt, data = mtcars)
simple_diag <- new_diag_lm(simple_model)
stopifnot(is.na(simple_diag$diagnostics$vif_scores))
cat("VIF correctly set to NA for single predictor.\n\n")

cat("== Test 3: Input validation (should error) ==\n")
bad_input <- tryCatch(
  new_diag_lm(mtcars),
  error = function(e) conditionMessage(e)
)
cat("Caught expected error:", bad_input, "\n")

cat("\nAll tests passed.\n")
