# GLM Utils

An R toolkit for **GLM-based non-life insurance pricing analysis**. It covers the
standard workflow around a frequency/severity model pair: one-way exploration,
actual-vs-expected checks, partial dependence plots, model diagnostics
(dispersion, binned residuals), the construction of a multiplicative
rating table with credibility flags, premium-impact (dislocation) analysis,
and deliverables — a formatted Excel rating workbook and a one-call HTML
report — all with interactive plotly visualisations.

All plots follow a consistent house style (navy/blue/gold palette, exposure bars
on a secondary axis, horizontal legends) and export to PNG via the plotly mode bar.

---

## Contents

- [Requirements](#requirements)
- [Getting started](#getting-started)
- [Data conventions](#data-conventions)
- [Quick start example](#quick-start-example)
- [Function reference](#function-reference)
  - [`agg_all()`](#agg_all)
  - [`make_plot()`](#make_plot)
  - [`make_pdp()`](#make_pdp)
  - [`plot_glm_predictor()`](#plot_glm_predictor)
  - [`glm_diagnostics()`](#glm_diagnostics)
  - [`plot_glm_residuals()`](#plot_glm_residuals)
  - [`make_rating_table()`](#make_rating_table)
  - [`make_rating_plot()`](#make_rating_plot)
  - [`premium_impact()`](#premium_impact)
  - [`export_rating_table()`](#export_rating_table)
  - [`pricing_report()`](#pricing_report)
- [Actuarial methodology](#actuarial-methodology)
- [Input validation and warnings](#input-validation-and-warnings)
- [Tests](#tests)
- [Known limitations](#known-limitations)

---

## Requirements

- R (developed and tested on R 4.2.1)
- Packages: `dplyr`, `ggplot2`, `plotly`, `data.table`, `splines` (base R)
- Optional: `openxlsx` (only for `export_rating_table()`); `htmltools`
  (only for `pricing_report()`, installed automatically with plotly)

`splines` is attached by the utils file itself so that `predict()` works on
models that use `ns()`/`bs()` in their formula.

## Getting started

There is no package; simply source the file:

```r
source("GLM UTILS.R")
```

This attaches the required libraries and defines six functions plus the
house-style colour constants (`ta_navy`, `ta_blue`, `ta_lightblue`, `ta_gold`,
`ta_muted`, and the palette helper `ta_year_palette(n)`).

## Data conventions

The functions operate on a policy(-year) level dataset with, at minimum:

| Concept          | Default column name | Role                                                        |
|------------------|---------------------|-------------------------------------------------------------|
| Exposure         | `Exposure`          | earned exposure (policy years)                              |
| Claim count      | `AantalClaims`      | response (y) for frequency **and** weight for severity      |
| Loss amount      | `SCHADELAST`        | response (y) for severity                                   |
| Accounting year  | `BOEKJAAR`          | optional split dimension                                    |

> The defaults reflect the original (Dutch) dataset naming. Every function
> accepts `exposure_col`, `claims_col`, `loss_col` and (where relevant)
> `year_col` arguments, so datasets with different column names work by
> overriding these. Column presence is validated up front with a clear error.

**All functions take raw data** and default to the raw dataset names above;
aggregation happens internally where needed. `agg_all()` is the aggregation
engine behind `make_plot()` and returns English, standardised column names
regardless of the input naming: `Exposure`, `ClaimCount`, `Loss`,
`Frequency`, `Severity` and (when split) `Year`. It remains available
standalone for building your own summary tables.

**Model conventions.** The toolkit assumes the classical frequency/severity
setup:

```r
# Frequency: Poisson (or quasi/negbin) with log link and an exposure offset
m_freq <- glm(AantalClaims ~ ... + offset(log(Exposure)),
              family = poisson(), data = dat)

# Severity: Gamma with log link on average cost per claim,
# weighted by the number of claims
m_sev  <- glm(AvgLoss ~ ..., family = Gamma(link = "log"),
              data = claims_data, weights = AantalClaims)
```

Two things matter throughout:

1. **Fit models with a `data =` argument.** Several functions recover the
   training data from `model$data` (robustly, via rowname matching); models
   fitted on variables floating in the global environment are rejected with
   a clear error.
2. **Use a log link** for anything feeding `make_rating_table()`. The
   multiplicative factor decomposition is only valid for log-link models;
   a non-log link triggers a warning.

## Quick start example

```r
source("GLM UTILS.R")

# --- Fit models -------------------------------------------------------
m_freq <- glm(AantalClaims ~ ns(AGE, 4) + REGION + factor(BOEKJAAR) +
                offset(log(Exposure)),
              family = poisson(), data = dat)

claims <- dat[dat$AantalClaims > 0, ]
claims$AvgLoss <- claims$SCHADELAST / claims$AantalClaims
m_sev <- glm(AvgLoss ~ AGE + REGION, family = Gamma(link = "log"),
             data = claims, weights = AantalClaims)

# --- One-way exploration ----------------------------------------------
# make_plot takes raw rows and aggregates internally (via agg_all):
make_plot(dat, "REGION", "Frequency", ta_blue,
          y_label = "Frequency", display = "color", by_year = TRUE)
make_plot(dat, "REGION", "Severity", ta_gold,
          y_label = "Severity", display = "facet", by_year = TRUE)

# --- Model vs data ------------------------------------------------------
plot_glm_predictor(m_freq, "AGE", n_bins = 50)        # actual vs expected
make_pdp(m_freq, dat, "REGION", metric = "Frequency") # partial dependence

# --- Diagnostics ---------------------------------------------------------
glm_diagnostics(m_freq, m_sev)      # deviance, AIC, dispersion per model
plot_glm_residuals(m_freq)          # binned residuals vs fitted value
plot_glm_residuals(m_freq, "AGE")   # binned residuals vs a predictor

# --- Rating table --------------------------------------------------------
tbl <- make_rating_table(m_freq, m_sev, data = dat,
                         base_level = "exposure",     # base = largest level
                         trim = c(0.005, 0.995))      # trim outlier tails

attr(tbl, "intercept_premium")   # base premium per unit of exposure
make_rating_plot(tbl, "AGE")     # factor curves
make_rating_plot(tbl, "AGE:REGION", metric = "Premium")  # interaction

# --- Deliverables --------------------------------------------------------
export_rating_table(tbl, "rating_table.xlsx")   # formatted Excel workbook

impact <- premium_impact(dat,                    # dislocation vs current tariff
                         model_freq_new = m_freq, model_sev_new = m_sev,
                         old_premium_col = "PREMIUM", by = "REGION")
impact$summary; impact$plot

pricing_report(m_freq, m_sev, dat,               # everything in one HTML
               file = "pricing_report.html")
```

---

## Function reference

### `agg_all()`

Aggregates raw policy rows to one row per level of a grouping column
(optionally per accounting year), with frequency and severity computed on the
aggregate. This is the aggregation engine used internally by `make_plot()`;
call it directly when you want the summary table itself.

```r
agg_all(d, col, by_year,
        exposure_col = "Exposure", claims_col = "AantalClaims",
        loss_col = "SCHADELAST", year_col = "BOEKJAAR")
```

| Argument | Description |
|---|---|
| `d` | data.frame or data.table with raw rows |
| `col` | grouping column name (string) |
| `by_year` | `TRUE` = also split by accounting year |
| `*_col` | column-name overrides (see [Data conventions](#data-conventions)) |

**Returns** a data.frame with `col`, `[Year]`, `Exposure`, `ClaimCount`,
`Loss`, `Frequency` (`ClaimCount / Exposure`, `NA` when exposure ≤ 0) and
`Severity` (`Loss / ClaimCount`, `NA` when there are no claims).

Warnings are raised for `NA` values in the input columns and for groups with
non-positive exposure, so data-quality issues are visible instead of silently
averaged away.

### `make_plot()`

One-way plot of frequency or severity with exposure bars on a secondary
axis. Takes **raw policy rows** and aggregates internally via `agg_all()`.

```r
make_plot(data, col, metric = c("Frequency", "Severity"),
          color_single, y_label,
          display = c("color", "facet"), by_year, metric_fmt = 4,
          exposure_col = "Exposure", claims_col = "AantalClaims",
          loss_col = "SCHADELAST", year_col = "BOEKJAAR",
          discrete_cutoff = 25)
```

- `metric` selects what is plotted: `"Frequency"` (claims / exposure) or
  `"Severity"` (loss / claim), both computed on the internal aggregate.
- `display = "color"`: one coloured line per year in a single panel
  (native plotly). Note: the exposure bars then show the **sum over all
  years** — the trace is labelled "Exposure (all years)" accordingly.
- `display = "facet"`: one panel per year (ggplot2 → ggplotly). Exposure bars
  are rescaled per facet to the metric range; the tooltip shows true values.
- Categorical X-axes (and numeric columns with ≤ `discrete_cutoff` unique
  values) are drawn as markers only, so categories are never connected by a
  line.

**Returns** a plotly object.

### `make_pdp()`

Partial dependence plot for a fitted `glm`, overlaid with the observed
one-way statistic and an exposure (or claim-count) histogram.

```r
make_pdp(model, raw_data, pred_var,
         metric = c("Frequency", "Severity"),
         transform = NULL,          # deprecated, ignored
         grid_res = 50, y_label = NULL, metric_fmt = 4,
         exposure_col = "Exposure", claims_col = "AantalClaims",
         loss_col = "SCHADELAST", discrete_cutoff = 25)
```

How the PDP is computed (see [Actuarial methodology](#actuarial-methodology)
for the rationale):

1. The training data is recovered from the model (aligned with the fitted
   rows).
2. For each grid value of `pred_var`, the entire training set is set to that
   value and predicted with `predict(type = "response")`.
3. If the model has an offset, the exposure column is set to 1 first, so
   predictions are **per unit of exposure**.
4. The predictions are averaged with a **weighted arithmetic mean** —
   exposure-weighted for frequency, claim-count-weighted for severity.

**Returns** a plotly object with three traces: exposure/claim bars,
"Observed" (one-way marginal from `raw_data`) and "PDP (model)".

> Interpretation caveat: the observed line is a one-way marginal (it includes
> correlations with all other rating factors), while the PDP is a partial
> effect. A gap between the two does not by itself indicate misfit.

**Performance.** Before predicting, the training data is collapsed to the
unique profiles of the model's other predictors (with summed weights), so
the cost is `grid_res × #profiles` predictions rather than
`grid_res × nrow(data)` — with an exactly identical result. On a 1M-row
portfolio with ~54k profiles this runs in seconds; on typical tariff data
(banded/categorical rating factors, far fewer profiles) it is sub-second.
If it is still slow, the number of unique profiles is the driver: band very
granular continuous covariates or lower `grid_res`.

### `plot_glm_predictor()`

Actual-vs-expected plot per predictor, computed on the model's own training
data.

```r
plot_glm_predictor(model, predictor, n_bins = 150,
                   weight_var = NULL, weight_label = NULL,
                   color = ta_year_palette(1), color_pred = ta_gold,
                   title = NULL, ylab = NULL, xlab = NULL,
                   metric_fmt = 4, bin_type = c("quantile", "width"))
```

Mode is detected automatically:

- **Offset model with log link** (typical frequency model): the response and
  predictions are counts; both are divided by the exposure recovered from
  `exp(offset)`, giving observed and predicted **frequency** per bin.
- **Otherwise** (typical severity model): the response is already an average;
  observed and predicted are **prior-weight-weighted means** per bin (e.g.
  claim-count-weighted for severity).
- `weight_var` overrides the weight column explicitly (must exist in
  `model$data`; an unknown name is an error, not silently ignored).

Numeric predictors are binned with **quantile bins** by default (each bin has
roughly the same number of observations, avoiding noisy thin tails);
`bin_type = "width"` restores equal-width binning.

**Returns** a plotly object with weight bars, an "Observed" line and a
"Predicted" line.

### `glm_diagnostics()`

Compact fit summary for a frequency and/or severity model.

```r
glm_diagnostics(model_freq = NULL, model_sev = NULL)
```

**Returns** a data.frame with one row per model: `Model`, `Family`, `Link`,
`N`, `Deviance`, `DFResidual`, `AIC`, `Dispersion` (Pearson χ² / df) and
`DevianceExplained` (1 − deviance / null deviance, a pseudo-R²).

For Poisson/binomial families a **dispersion above 1.2 raises an
overdispersion warning**: the point estimates are still consistent, but
standard errors are understated, so consider a quasi-Poisson or negative
binomial family before reading anything into term significance.

### `plot_glm_residuals()`

Binned residual plot — the readable alternative to raw residual plots, which
are dominated by the 0/1 claim pattern on policy-level count data.

```r
plot_glm_residuals(model, predictor = NULL, n_bins = 50,
                   residual_type = c("pearson", "deviance"))
```

Residuals are averaged per quantile bin of the **fitted value** (default) or
of a **predictor** (`predictor = "AGE"`), and drawn together with a ±2·SE
band (scaled by the estimated dispersion). Under a correctly specified model
roughly 95% of the bin means should fall inside the band; a systematic
pattern outside it (e.g. a curve over a predictor) signals missed structure —
a candidate spline, banding or interaction. Categorical predictors get error
bars per level instead of a ribbon.

**Returns** a plotly object.

### `make_rating_table()`

Builds a table of multiplicative rating factors from a frequency and/or
severity model, including two-way interactions.

```r
make_rating_table(model_freq = NULL, model_sev = NULL, data,
                  grid_res = 50,
                  exposure_col = "Exposure", claims_col = "AantalClaims",
                  base_level = c("first", "exposure"),
                  trim = c(0, 1),
                  min_claims = 30, full_cred_claims = 1082)
```

| Argument | Description |
|---|---|
| `model_freq`, `model_sev` | fitted glm objects (at least one; log link expected) |
| `data` | original dataset — used for base values, grids and exposure |
| `grid_res` | number of grid points for continuous variables |
| `base_level` | `"first"` = first factor level is the reference; `"exposure"` = the level with the largest exposure is the reference |
| `trim` | quantile range for continuous grids, e.g. `c(0.005, 0.995)` to avoid outlier tails and spline extrapolation |
| `min_claims` | thin-cell threshold: levels with fewer claims get `IsThin = TRUE` (categorical thin levels also raise a warning) |
| `full_cred_claims` | full-credibility claim standard for the `Credibility` column (default 1,082) |

**Returns** a data.frame with one row per level/grid point per variable:

| Column | Meaning |
|---|---|
| `Variable` | variable name, or `"A:B"` for an interaction |
| `Type` | `"categorical"` / `"continuous"` (of the X variable) |
| `Level`, `LevelNum` | level as text and (when applicable) numeric |
| `Group`, `GroupVar`, `XVar` | interaction bookkeeping (`NA` for main effects) |
| `IsBase` | `TRUE` on the base row/cell (factor exactly 1) |
| `Exposure`, `ClaimCount` | data volume per level (per cell for cat × cat interactions) |
| `Factor_Frequency`, `Factor_Severity`, `Factor_Premium` | multiplicative relativities vs the base; `Premium = Frequency × Severity`; a variable absent from a model gets the neutral factor 1 there |
| `Uplift_Frequency`, `Uplift_Severity`, `Uplift_Premium` | interaction rows only: pure interaction effect = joint / (main<sub>x</sub> × main<sub>group</sub>); equals 1 everywhere when there is no interaction |
| `Credibility` | limited-fluctuation credibility of the level's own experience: `min(1, sqrt(ClaimCount / full_cred_claims))` |
| `IsThin` | `TRUE` when `ClaimCount < min_claims`; these levels are dimmed and flagged in `make_rating_plot()` |

Attributes on the returned table:

- `intercept_frequency`, `intercept_severity`, `intercept_premium` — the
  model prediction at the base point, **per unit of exposure**;
- `base_values` — named list with the base value used per variable.

Inline formula transformations such as `factor(YEAR)` and `ns(AGE, 4)` are
resolved to their underlying columns; terms whose base variable cannot be
found in `data` are skipped with a warning rather than crashing the call.

### `make_rating_plot()`

Plots one variable (or interaction) from a rating table.

```r
make_rating_plot(rating_tbl, var, metric_fmt = 4, metric = NULL)
```

- **Main effects**: factor curves for Frequency, Severity and Premium on the
  primary axis (with a dotted reference line at 1.0), exposure bars on the
  secondary axis.
- **Interactions** (`var = "A:B"`): one series per level of the group
  variable. Shows one metric at a time — `metric` picks
  `"Frequency"`, `"Severity"` or `"Premium"` (default: Premium when both
  models are present). Exposure bars are included for categorical ×
  categorical interactions.
- **Thin cells**: levels flagged `IsThin` in the rating table are drawn with
  dimmed markers and a "low claim volume" note in the tooltip, so
  low-credibility factors are visually distinct from well-supported ones.

**Returns** a plotly object.

### `premium_impact()`

Dislocation analysis: per-policy comparison of premiums under a new model
set against old models or an existing premium column.

```r
premium_impact(data,
               model_freq_new = NULL, model_sev_new = NULL,
               model_freq_old = NULL, model_sev_old = NULL,
               old_premium_col = NULL,
               old_premium_basis = c("amount", "rate"),
               rebase = TRUE, by = NULL, n_show = 10,
               exposure_col = "Exposure")
```

Premiums are computed as **rates per unit of exposure** (offsets neutralised
at exposure = 1); the premium is the product of the models supplied. The old
side is either a model pair or `old_premium_col` — a column with the current
premium, interpreted as an amount for the record (`"amount"`, divided by
exposure internally) or as a rate (`"rate"`).

With `rebase = TRUE` (default) the new premiums are scaled so the
exposure-weighted totals match the old ones. That separates the two questions
a dislocation analysis answers: the overall **rate-level change** (reported
in the summary) and the **redistribution** across the portfolio (the
histogram and quantiles).

**Returns** a list: `summary` (display table), `stats` (the same numbers as
a named list), `policy` (per-row old/new/percent change),
`by_level` (exposure-weighted mean change per level of the `by` columns),
`largest_increases`/`largest_decreases` (top-`n_show` dislocations) and
`plot` (exposure-weighted histogram of premium changes with the median
marked).

### `export_rating_table()`

Formatted Excel export of a `make_rating_table()` result (requires
`openxlsx`).

```r
export_rating_table(rating_tbl, file = "rating_table.xlsx",
                    overwrite = TRUE, digits = 4)
```

The workbook contains:

- **Overview** — timestamp, the intercepts (per unit of exposure) and the
  base value per variable;
- **one sheet per main-effect variable** — level, exposure, claim count,
  credibility and the factor columns; the base row is highlighted, thin
  rows are greyed out;
- **one sheet per interaction** — the long table plus a Level × Group
  matrix of the premium factor, ready for tariff implementation.

### `pricing_report()`

One call that bundles the whole analysis into a single HTML report.

```r
pricing_report(model_freq = NULL, model_sev = NULL, data,
               file = "pricing_report.html",
               title = "GLM Pricing Report",
               variables = NULL,
               include = c("diagnostics", "oneway", "ae", "pdp", "rating"),
               by_year = FALSE, grid_res = 50,
               base_level = c("first", "exposure"), trim = c(0, 1), ...)
```

The report contains the diagnostics table with binned residual plots, and —
per variable (default: all base variables of both models) — the one-way
observed plot, actual vs expected (frequency and severity), partial
dependence and the rating-factor plot, plus a section with all interaction
plots. Individual plot failures are shown as a note instead of aborting the
report.

It is built with `htmltools` (no pandoc needed); all plots remain fully
interactive. Next to the `.html` a `<name>_files/` folder is written with
the JavaScript dependencies — keep the two together when sharing.

---

## Actuarial methodology

### Multiplicative rating and the reconstruction identity

For log-link GLMs the prediction factorises multiplicatively. The rating
table exploits this with a single consistent **base point**:

- categorical variables → reference level (`base_level`),
- continuous variables → the **median** (inserted as an explicit grid point),
- the exposure column → **1**.

Every factor is `prediction(level, others at base) / prediction(base)`, and
the intercept attributes are the prediction at the base point itself. For a
log-link model **without interactions** this makes the identity exact:

```
prediction(x₁, …, xₖ, exposure = 1)
  = intercept × Factor(x₁) × … × Factor(xₖ)
```

(verified to 1e-10 in the test suite). With interactions, the joint
relativity of a pair decomposes as `main_x × main_group × Uplift`, so the
uplift columns isolate what the interaction adds on top of the main effects.

Because the intercept is evaluated at exposure = 1, it is a true base
frequency (respectively base severity / base risk premium) per unit of
exposure, not a value at some arbitrary portfolio exposure.

### Why the PDP is computed on the response scale

A naive GLM PDP averages link-scale predictions and back-transforms:
`exp(mean(link))`. That is a *geometric* mean, and with an exposure offset it
also absorbs the average log-exposure. Both effects bias the curve low
relative to observed frequencies (Jensen's inequality), making the
model-vs-data comparison misleading.

`make_pdp()` therefore:

1. neutralises the offset (exposure = 1),
2. predicts on the response scale, and
3. takes the exposure-weighted (frequency) or claim-weighted (severity)
   arithmetic mean,

which is the same estimand as the observed `sum(claims) / sum(exposure)`
line it is plotted against.

### Actual vs expected

`plot_glm_predictor()` compares like with like: for offset models it
aggregates counts and exposure before dividing (`Σ observed / Σ exposure` vs
`Σ predicted / Σ exposure`); for weighted models it uses prior-weight-weighted
means. Quantile binning keeps each bin credible instead of leaving near-empty
tail bins that look like misfit but are noise.

### Thin cells and credibility

The `Credibility` column uses the classical **limited-fluctuation
(square-root) standard**: full credibility at `full_cred_claims` claims
(default 1,082, i.e. the observed frequency lies within ±5% of the true value
with 90% confidence for a Poisson process), and partial credibility
`Z = min(1, √(claims / 1082))` below that.

Read `Z` as *how much standalone experience backs this level*. The GLM
factor itself already pools information across the whole portfolio through
the model structure, so a low `Z` does not invalidate the factor — it means
the factor leans on the model rather than on that level's own data, and it
deserves scrutiny before being used in a tariff. The `IsThin` flag (claims
below `min_claims`) marks the levels where this is acute; `make_rating_plot()`
dims them so a factor of 1.4 on 12 claims is never read with the same
confidence as one on 12,000.

### Overdispersion

Poisson frequency models on real portfolios are almost always overdispersed
(unobserved heterogeneity). `glm_diagnostics()` estimates the Pearson
dispersion and warns above 1.2: coefficient estimates remain consistent, but
standard errors scale with √dispersion, so term selection based on naive
Poisson p-values is anti-conservative. Refit with `quasipoisson()` or a
negative binomial family when flagged.

---

## Input validation and warnings

The functions fail fast with explicit messages rather than producing silently
wrong numbers:

- **Errors**: missing columns (named per function), non-glm model objects,
  models fitted without `data =` where training data is needed, unknown
  `weight_var`, unmatched predictors, invalid `trim`.
- **Warnings**:
  - `NA`s in exposure/claims/loss inputs and groups with exposure ≤ 0
    (`agg_all`);
  - non-log link where multiplicativity or `exp(offset)` is assumed
    (`make_rating_table`, `make_pdp`, `plot_glm_predictor`);
  - an offset that cannot be neutralised because the exposure column is not
    in the training data (`make_pdp`);
  - unweighted PDP averaging when no offset/weights are found;
  - skipped rating-table terms/interactions, with the reason;
  - overdispersion in Poisson/binomial models (`glm_diagnostics`);
  - categorical rating-table levels with fewer than `min_claims` claims
    (`make_rating_table`).

## Tests

`test_glm_utils.R` contains an executable validation suite (51 checks) that
simulates a portfolio, fits Poisson/Gamma models and verifies among other
things: exact premium reconstruction from the table, factor = 1 on base rows,
uplift = 1 at reference levels, PDP = exposure-weighted mean response at
exposure 1, offset-neutralised intercepts, inline `factor()` handling, custom
column names, the non-log-link warning, the overdispersion warning, binned
residuals staying within the ±2·SE band for a correct model, the
credibility/thin-cell columns, zero dislocation for identical models and
exact rebase behaviour in `premium_impact()`, the Excel workbook structure,
and the generated HTML report. Run it with:

```sh
Rscript test_glm_utils.R
```

All checks should print `[OK ]`.

## Known limitations

Deliberately out of scope (for now):

- **No uncertainty quantification** — the rating factors carry no standard
  errors or confidence intervals.
- **No holdout / train-test support** — all comparisons are in-sample on the
  training data.
- **No portfolio-level model comparison** — lift charts, Lorenz/Gini and
  double-lift are not included (fit statistics and residual diagnostics are:
  see `glm_diagnostics()` and `plot_glm_residuals()`).
- **Frequency × severity only** — direct risk-premium (e.g. Tweedie) models
  do not fit the rating-table structure.
- **Two-way interactions only** — higher-order interaction terms are skipped
  (with a warning).
- `make_pdp()` and `plot_glm_predictor()` support `glm` objects only.
