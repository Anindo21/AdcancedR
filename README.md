# Interactive Linear Model Diagnostics

An R Shiny application for exploring two classical OLS assumption violations —
**heteroskedasticity** and **multicollinearity** — on simulated data. The user
generates a dataset (optionally with a violation built in), the app fits a
linear model, runs the relevant diagnostic tests, and presents the results as a
formatted table and a residual plot.

The project is built around a small custom **S3 class** (`diag_lm`) that keeps
the statistical logic separate from the user interface.

## Project structure

```
.
├── app.R                     # Shiny app (UI + server)
├── R/
│   ├── diag_lm_class.R       # diag_lm S3 class: constructor + print/summary/plot methods
│   └── simulate_data.R       # simulate_ols_data(): generates data with optional violations
├── data/
│   └── sample_cars.csv       # example CSV for trying the upload feature
└── tests/
    └── test_diag_lm.R        # quick checks for the diag_lm class
```

## How it works

### 1. Simulating data (`R/simulate_data.R`)

`simulate_ols_data(n, heteroskedastic, multicollinear)` returns a data frame
with one outcome `Y` and two predictors `X1`, `X2`, generated from a known model:

$$Y = 10 + 2.5 \cdot X_1 - 1.5 \cdot X_2 + \varepsilon$$

The two flags deliberately break an assumption so the diagnostics have something
to detect:

- `heteroskedastic = TRUE` — the error standard deviation grows with `X1`
  (`sd = 1 + 1.5 * |X1|`), producing the fan-shaped residual pattern.
- `multicollinear = TRUE` — `X2` is set to `X1` plus a small amount of noise,
  so the two predictors carry almost the same information.

With both flags off the data satisfies the usual OLS assumptions.

### 2. The `diag_lm` class (`R/diag_lm_class.R`)

`new_diag_lm(model)` takes a fitted `lm` object and returns a `diag_lm` object.
It:

1. Validates the input with `inherits(model, "lm")` and stops early on a bad
   argument.
2. Stores the formula, coefficient table, residuals and fitted values.
3. Runs the **Breusch–Pagan test** (`lmtest::bptest`) for heteroskedasticity.
4. Computes **VIF** (`car::vif`) for multicollinearity. Because `car::vif()`
   errors on a single-predictor model, this is wrapped in `tryCatch()` and falls
   back to `NA`.

Three S3 methods are provided:

| Method | Returns | Purpose |
| --- | --- | --- |
| `print.diag_lm` | console text | quick overview when typing the object name |
| `summary.diag_lm` | `gt` table | coefficient table with the BP p-value in the subtitle |
| `plot.diag_lm` | `ggplot` object | diagnostic plot selected by `type` (see below) |

`plot.diag_lm` supports four plot types via its `type` argument:

| `type` | Plot | What to look for |
| --- | --- | --- |
| `"residuals"` | Residuals vs Fitted | a funnel shape suggests heteroskedasticity |
| `"qq"` | Normal Q-Q | points off the diagonal suggest non-normal residuals |
| `"scale_location"` | Scale-Location | a rising trend suggests non-constant variance |
| `"histogram"` | Residual histogram | skew or heavy tails in the residual distribution |

### 3. The Shiny app (`app.R`)

The server keeps almost no statistics of its own — it maps the UI controls to
`simulate_ols_data()` and `new_diag_lm()` and then calls `summary()` / `plot()`.

The **Data Source** control lets the user choose between two inputs:

- **Simulate data** — uses `simulate_ols_data()` with the sample size and the
  two violation switches.
- **Upload a CSV** — reads a file with `read.csv()`, then offers dropdowns to
  pick the outcome and predictor columns (only numeric columns are listed).
  The model formula is built on the fly from those choices. `read.csv()` is
  wrapped in `tryCatch()`, and `validate()`/`need()` show friendly messages
  (e.g. "select at least one predictor") instead of red errors.

Reactivity details:

- `eventReactive(input$sim_btn, ...)` regenerates the simulated data **only**
  when the "Simulate & Fit Model" button is pressed, so dragging the
  sample-size slider does not trigger a re-fit. `ignoreNULL = FALSE` makes it
  also run once at startup so the app is never blank.
- `req(...)` makes the model step wait until the data and column choices exist,
  avoiding errors on the very first render.

## Interpreting the output

- **Breusch–Pagan p-value** (Summary tab subtitle): a small value (< 0.05)
  indicates heteroskedasticity. With the heteroskedasticity box ticked it drops
  close to zero.
- **VIF** (in the `diag_lm` object / print method): values above 5 suggest
  problematic multicollinearity. With the multicollinearity box ticked the VIFs
  rise well above this threshold.
- **Residuals vs Fitted plot** (Diagnostic Plots tab): a horizontal band of
  points is healthy; a widening funnel indicates heteroskedasticity.

## The diagnostics in more detail

### Breusch–Pagan test (heteroskedasticity)

The OLS model assumes the error term has constant variance,
$\mathrm{Var}(\varepsilon_i) = \sigma^2$ for every observation. The
Breusch–Pagan test checks this by regressing the squared residuals on the
predictors and seeing whether they explain any of the variation. The test
statistic is

$$BP = n \cdot R^2_{\text{aux}},$$

where $R^2_{\text{aux}}$ is the R-squared from the auxiliary regression of the
squared residuals on the predictors, and $n$ is the sample size. Under the null
hypothesis of constant variance (homoskedasticity), $BP$ follows a
$\chi^2$ distribution with degrees of freedom equal to the number of
predictors. A small p-value means we reject constant variance, i.e. there is
evidence of heteroskedasticity. In the app this statistic comes from
`lmtest::bptest()`.

### Variance Inflation Factor (multicollinearity)

Multicollinearity happens when predictors are strongly correlated with each
other, which inflates the variance of the estimated coefficients and makes them
unstable. For predictor $j$ the VIF is

$$VIF_j = \frac{1}{1 - R_j^2},$$

where $R_j^2$ is the R-squared from regressing predictor $j$ on all the other
predictors. If $j$ is unrelated to the others, $R_j^2 \approx 0$ and
$VIF_j \approx 1$; if it is almost a linear combination of them, $R_j^2$
approaches 1 and the VIF explodes. A common rule of thumb flags
$VIF > 5$ (sometimes 10) as problematic. VIF needs at least two predictors,
which is why `new_diag_lm()` returns `NA` for a single-predictor model. In the
app these values come from `car::vif()`.

### Residual plots

- **Residuals vs Fitted** — should look like a structureless horizontal band.
  A funnel (spread growing with the fitted value) points to heteroskedasticity;
  a curve points to a missing non-linear term.
- **Normal Q-Q** — standardised residuals plotted against theoretical normal
  quantiles. Points on the diagonal mean the residuals are roughly normal;
  curving tails mean skew or heavy tails.
- **Scale-Location** — $\sqrt{|\text{standardised residuals}|}$ against fitted
  values. A flat trend supports constant variance; an upward slope is another
  sign of heteroskedasticity.
- **Residual histogram** — a quick visual check of the residual distribution
  for skew or departures from normality.

## Running the project

The required packages are `shiny`, `lmtest`, `car`, `gt`, `ggplot2`, `dplyr` and
`tibble`:

```r
install.packages(c("shiny", "lmtest", "car", "gt", "ggplot2", "dplyr", "tibble"))
```

Run the app from the project root:

```r
shiny::runApp(".")
```

Run the checks for the `diag_lm` class:

```r
source("tests/test_diag_lm.R")
```

## Notes

- Files placed in `R/` are automatically sourced by Shiny at startup, which is
  why the test script lives in `tests/` rather than `R/` (otherwise it would run
  every time the app launches).
- Simulated results change on each run by default. Enter a value in the
  "Random seed" box (or pass `seed = ` to `simulate_ols_data()`) to get a
  reproducible dataset.
