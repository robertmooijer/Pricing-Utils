# pricingtoolsRmO

[![R-CMD-check](https://github.com/robertmooijer/Pricing-Utils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/robertmooijer/Pricing-Utils/actions/workflows/R-CMD-check.yaml)

An R package for **GLM-based non-life insurance pricing analysis**. It covers the
standard workflow around a frequency/severity model pair: one-way exploration,
actual-vs-expected checks, partial dependence plots, model diagnostics
(dispersion, binned residuals, interaction detection), the construction of a
multiplicative rating table with thin-cell flags, premium-impact (dislocation)
analysis,
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
- [Gallery](#gallery)
- [Function reference](#function-reference)
  - [`agg_all()`](#agg_all)
  - [`make_plot()`](#make_plot)
  - [`make_pdp()`](#make_pdp)
  - [`plot_glm_predictor()`](#plot_glm_predictor)
  - [`glm_diagnostics()`](#glm_diagnostics)
  - [`plot_glm_residuals()`](#plot_glm_residuals)
  - [`detect_interactions()`](#detect_interactions)
  - [`plot_residual_heatmap()`](#plot_residual_heatmap)
  - [`screen_features()`](#screen_features)
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

- R >= 4.1 (developed and tested on R 4.2.1)
- Hard dependencies (installed automatically): `dplyr`, `ggplot2`, `plotly`,
  `data.table`
- Optional: `openxlsx` (only for `export_rating_table()`); `htmltools`
  (only for `pricing_report()`, installed automatically with plotly);
  `xgboost` (only for `screen_features()`)

## Getting started

Install from GitHub and attach the package:

```r
# install.packages("remotes")
remotes::install_github("robertmooijer/Pricing-Utils")

library(pricingtoolsRmO)
```

This exports the functions listed above plus the house-style colour
constants (`ta_navy`, `ta_blue`, `ta_lightblue`, `ta_gold`, `ta_muted`,
and the palette helper `ta_year_palette(n)`). The spline basis functions
`ns()` and `bs()` are re-exported from the `splines` package, so formulas
like `y ~ ns(AGE, 4)` work without loading `splines` yourself.

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
library(pricingtoolsRmO)

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

## Gallery

All figures below are real output, generated from a simulated 40k-policy
motor portfolio by `demo/make_readme_figures.R`. In practice they are
interactive plotly widgets (hover, zoom, toggle series); these are static
screenshots.

**One-way exploration** — observed frequency per level with exposure bars,
split by accounting year:

![One-way plot](man/figures/README-oneway.png)

**Actual vs expected** — observed and predicted frequency per quantile bin
of a predictor, the core model-fit check:

![Actual vs expected](man/figures/README-actual-vs-expected.png)

**Rating factors** — the multiplicative relativities that go into the
tariff, relative to the base level (dotted line at 1.0):

![Rating factors](man/figures/README-rating-factors.png)

**Interactions** — one curve per level of the group variable; here young
drivers are materially worse in the Randstad than elsewhere:

![Interaction plot](man/figures/README-interaction.png)

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

![Partial dependence plot](man/figures/README-pdp.png)

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
                   metric_fmt = 4, bin_type = c("quantile", "width"),
                   y_range = NULL)
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

**Binning.** A numeric predictor with at most `n_bins` distinct values is
not binned at all: every value gets its own point, on its exact position.
That matters for variables like vehicle age or number of claims — binning
would merge neighbouring values (ages 0 and 1 into one point at, say,
0.78) and, because equal-width bins span the observed range, a single
outlier would silently shift every point on the plot.

Only when there are more distinct values than `n_bins` does binning kick
in: **quantile bins** by default (each bin holds roughly the same number
of observations, avoiding noisy thin tails), or equal-width bins with
`bin_type = "width"`. A binned point then sits at the weight-weighted mean
of the values in its bin, not at the bin edge.

**Shared y-axis across variables.** By default the primary y-axis
auto-scales to the values of that one plot, so two predictors on very
different scales can look equally volatile. Pass `y_range = c(lo, hi)` to
pin the axis, and reuse the same value across predictors to make them
directly comparable:

```r
rng <- c(0, 0.55)
plot_glm_predictor(m_freq, "LEEFTIJD", y_range = rng)
plot_glm_predictor(m_freq, "REGIO",    y_range = rng)
```

| | |
|---|---|
| ![Fixed y-axis, age](man/figures/README-yrange-age.png) | ![Fixed y-axis, region](man/figures/README-yrange-regio.png) |

Both panels share one axis, so the age effect is visibly the stronger
driver. Note the trade-off: a shared axis flattens variables with a narrow
spread of their own, so use auto-scaling when inspecting one variable in
detail and a fixed range when comparing variables side by side.

`y_range` is available on every plotting function in the package
(`make_plot()`, `make_pdp()`, `make_rating_plot()`,
`plot_glm_residuals()`, and `premium_impact()`, which additionally takes
an `x_range` for its change axis). It is `NULL` everywhere by default,
which keeps the usual auto-scaling.

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

![Binned residual plot](man/figures/README-residuals.png)

### `detect_interactions()`

Scans every two-way combination of variables and ranks them by the
interaction structure the GLM has not captured.

```r
detect_interactions(model, vars = NULL, n_bins = 10, min_claims = 30,
                    n_sim = 200, top_n = NULL, seed = NULL)
```

For each pair it builds the two-way table of actual versus expected
claims, matches the row and column margins by **iterative proportional
fitting** — so main-effect misfit (a spline needing another knot, a level
that is simply mispriced) is scaled away first — and takes the remaining
deviance as the statistic. That is the likelihood-ratio test between the
additive and the saturated model on the table.

Because that deviance is unreliable in sparse tables, the reference
distribution comes from **simulation** rather than chi-square asymptotics
whenever the model is a Poisson count model: claims are resampled from the
raked expected values under the additive null, `n_sim` times. Other
families fall back to the dispersion-scaled chi-square (reported in the
`Method` column).

`vars` defaults to the model's own predictors, but you can pass variables
that are *not* in the model — which is how a completely omitted rating
factor surfaces.

**Returns** a data.frame, strongest first: `VarX`, `VarY`, `Cells`,
`Claims`, `Deviance`, `DF`, `Z`, `P`, `Method`, `MaxAE`, `MaxAE_Claims`,
`MaxAE_ExposureShare`.

> The ranking is by statistical signal (`Z`), which is not the same as
> materiality. Judge the two separately: `MaxAE` is the A/E of the worst
> cell that has enough claims, and `MaxAE_ExposureShare` says how much of
> the portfolio sits in it.

### `plot_residual_heatmap()`

The A/E ratio per cell of two variables — the view that shows *where* a
model leaks.

```r
plot_residual_heatmap(model, var_x, var_y, n_bins = 20,
                      min_claims = 30, z_range = NULL, title = NULL)
```

Below, a frequency GLM with clean main effects for age and region but no
interaction between them. Young drivers in the city are underpriced by
27% while the same age group elsewhere is overpriced — and every one-way
check on this model is spotless:

![Residual heatmap](man/figures/README-residual-heatmap.png)

- Values come from the model's own rows, so they always align with the
  fitted values; both variables must be columns of the data the model was
  fitted on.
- The colour scale is **diverging around 1.0** — blue overpriced, red
  underpriced, neutral grey at break-even. `z_range` fixes it across
  several heatmaps.
- Cells with fewer than `min_claims` claims are **left blank and marked
  with a grey cross** rather than coloured: in a thin cell the A/E is
  mostly noise, and colouring it would make it the loudest thing on the
  plot. Their volume is still in the tooltip.
- The cell table (groups, actual, expected, A/E, exposure, claims, thin
  flag) is attached to the returned plot as the `"cells"` attribute.

### `screen_features()`

Fits a boosted challenger with the GLM as an offset, so it can only model
what the GLM leaves behind. Requires `xgboost` (in `Suggests`).

```r
screen_features(model, features = NULL, split = c(0.6, 0.2, 0.2),
                max_depth = 2, eta = 0.05, nrounds = 2000,
                early_stopping_rounds = 40, n_shap = 4000,
                cor_threshold = 0.95, max_levels = 50, seed = NULL)
```

The baseline you pass decides which question it answers. With a **minimal
model** it screens candidates *before* you choose what goes into the
tariff — "what does this data add on top of what I already price on".
With your **full model** it asks what the finished tariff still misses.
The baseline is refitted on the training split, so both sides of the
comparison are out of sample. No scorable model is returned.

```
$summary
                                     Stage Deviance Change
                              baseline GLM    16720
        + booster, depth 1 (additive only)    16650 -0.41%
 + booster, depth 2 (may use interactions)    16653 +0.02%

$features
       Feature InModel PermDeviance   Gain
   KILOMETRAGE   FALSE        66.83 0.4291
       GEWICHT   FALSE        16.78 0.1460
 GEWICHT_PROXY   FALSE        14.21 0.2058
         KLEUR   FALSE         3.04 0.0414
      LEEFTIJD    TRUE         0.64 0.0381
         REGIO    TRUE        -0.14 0.0045
          RUIS   FALSE        -3.57 0.1351
```

Three things to read carefully:

- **The verdict is staged.** While the depth-1 step still improves the
  fit, main effects are missing and the interaction ranking cannot be
  trusted. Fix the main effects, then run again.
- **Rank on `PermDeviance`, not `Gain`.** `PermDeviance` is the increase
  in out-of-sample deviance when the feature is shuffled with the baseline
  held fixed, so it is the *incremental* contribution; at or below zero
  means no usable signal. `Gain` is biased towards continuous and
  high-cardinality features — in the run above it gives pure noise
  (`RUIS`) a 13.5% share while the permutation test correctly puts it at
  −3.57.
- **Near-duplicates split their importance.** `GEWICHT` and
  `GEWICHT_PROXY` each get roughly half, and which one comes out on top is
  arbitrary. Pairs above `cor_threshold` are listed in `$correlated` with
  a warning.

Also returns `$interactions` (SHAP ranking), `$stats` and `$plot`.

> **Sensitivity.** A booster is *less* sensitive to a two-way interaction
> between known rating factors than
> [`detect_interactions()`](#detect_interactions): the cell-based test
> concentrates the signal into one statistic with a known null, while the
> booster must discover the split structure and pays a variance cost for
> its flexibility. On the same data, a case where the cell test reaches
> Z = 7.7 can leave the booster reporting nothing at all. Use this
> function to screen features and for the global verdict; use
> `detect_interactions()` to hunt interactions.

### `make_rating_table()`

Builds a table of multiplicative rating factors from a frequency and/or
severity model, including two-way interactions.

```r
make_rating_table(model_freq = NULL, model_sev = NULL, data,
                  grid_res = 50,
                  exposure_col = "Exposure", claims_col = "AantalClaims",
                  base_level = c("first", "exposure"),
                  trim = c(0, 1),
                  min_claims = 30)
```

| Argument | Description |
|---|---|
| `model_freq`, `model_sev` | fitted glm objects (at least one; log link expected) |
| `data` | original dataset — used for base values, grids and exposure |
| `grid_res` | number of grid points for continuous variables |
| `base_level` | `"first"` = first factor level is the reference; `"exposure"` = the level with the largest exposure is the reference |
| `trim` | quantile range for continuous grids, e.g. `c(0.005, 0.995)` to avoid outlier tails and spline extrapolation |
| `min_claims` | thin-cell threshold: levels with fewer claims get `IsThin = TRUE` (categorical thin levels also raise a warning) |

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
| `IsThin` | `TRUE` when `ClaimCount < min_claims`; these levels are dimmed in `make_rating_plot()` and greyed out in the Excel export — see [Thin cells](#thin-cells) |

Attributes on the returned table:

- `intercept_frequency`, `intercept_severity`, `intercept_premium` — the
  model prediction at the base point, **per unit of exposure**;
- `base_values` — named list with the base value used per variable.

Inline formula transformations such as `factor(YEAR)` and `ns(AGE, 4)` are
resolved to their underlying columns; terms whose base variable cannot be
found in `data` are skipped with a warning rather than crashing the call.

The rows for one categorical variable look like this (base row
highlighted, thin row greyed out, as in the Excel export). Note the last
level: 5 claims behind a premium factor of 1.22, so that factor is
essentially noise — exactly what the `IsThin` flag is there to surface:

![Rating table excerpt](man/figures/README-rating-table.png)

### `make_rating_plot()`

Plots one variable (or interaction) from a rating table.

```r
make_rating_plot(rating_tbl, var, metric_fmt = 4, metric = NULL,
                 y_range = NULL)
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
  dimmed markers and a "low claim volume" note in the tooltip, so factors
  built on few claims are visually distinct from well-supported ones.

**Returns** a plotly object.

The same variable as the table above — markers only (categories are never
connected by a line), with the thin level's markers faded:

![Categorical rating plot](man/figures/README-rating-plot-categorical.png)

For a continuous variable and an interaction, see the
[Gallery](#gallery).

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

![Premium impact histogram](man/figures/README-premium-impact.png)

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
- **one sheet per main-effect variable** — level, exposure, claim count
  and the factor columns; the base row is highlighted, thin rows are
  greyed out;
- **one sheet per interaction** — the long table plus a Level × Group
  matrix of the premium factor, ready for tariff implementation.

### `pricing_report()`

One call that bundles the whole analysis into a single HTML report.

```r
pricing_report(model_freq = NULL, model_sev = NULL, data,
               file = "pricing_report.html",
               title = "GLM Pricing Report",
               variables = NULL,
               include = c("diagnostics", "oneway", "ae", "pdp", "rating",
                           "interactions"),
               by_year = FALSE, top_interactions = 3, grid_res = 50,
               base_level = c("first", "exposure"), trim = c(0, 1), ...)
```

The report contains the diagnostics table with binned residual plots, and —
per variable (default: all base variables of both models) — the one-way
observed plot, actual vs expected (frequency and severity), partial
dependence and the rating-factor plot, plus a section with all interaction
plots. The `"interactions"` block additionally runs
[`detect_interactions()`](#detect_interactions) and plots the strongest
`top_interactions` pairs as A/E heatmaps. Individual plot failures are
shown as a note instead of aborting the report.

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

### Why a one-way check cannot see an interaction

This is not a matter of bad luck or too little data — it is structural.
For a canonical link, the score equations of the GLM force the fitted
totals to equal the observed totals for **every column of the design
matrix**. So for a categorical variable that is in the model, the A/E is
exactly 1.000 at every level, no matter what happens inside the cells:

```
A/E per REGIO:   Dorp 1.0000   Platteland 1.0000   Rand 1.0000   Stad 1.0000
```

A missing interaction cancels out in the margins by construction. No
amount of one-way plotting will reveal it, and an actual-vs-expected plot
per predictor will look perfect while whole cells of the portfolio are
mispriced by tens of percent. That is precisely the blind spot
[`detect_interactions()`](#detect_interactions) and
[`plot_residual_heatmap()`](#plot_residual_heatmap) exist to cover.

Two things to keep in mind when acting on what they find:

- **A candidate is a hypothesis, not a conclusion.** It only counts if
  adding the term to the GLM improves out-of-sample fit, the resulting
  factor pattern is explainable, and it holds up in another accounting
  year. The scan proposes; the GLM decides.
- **Beware of proxies and mix.** A detected `A × B` can be standing in for
  an unmodelled `C`, and an "interaction" with the accounting year is
  usually portfolio mix or trend rather than risk structure.

`demo/detect_interactions.R` walks through the whole cycle on a simulated
portfolio where the true interaction is known.

### Thin cells

Levels backed by very few claims get `IsThin = TRUE` (claim count below
`min_claims`, default 30). This matters because a GLM applies **no
shrinkage** to a categorical level: its coefficient comes essentially from
that level's own claims, so a factor built on a handful of them is an
unstable estimate rather than a signal. (Continuous variables fitted with
splines do borrow strength from neighbouring grid points, so a sparse
stretch of the curve is still supported by the data around it.)

The flag is surfaced everywhere the factors are shown —
`make_rating_plot()` dims the markers and adds a "low claim volume" note
to the tooltip, `export_rating_table()` greys out the row, and
`make_rating_table()` warns when a categorical level trips the threshold —
so a factor of 1.4 on 12 claims is never read with the same confidence as
one on 12,000. What to do about it (grouping the level with a related one,
capping the factor, or leaving it out of the tariff) stays a judgement
call; the package only flags it.

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

The package ships with a `testthat` suite (`tests/testthat/`) that simulates
a portfolio, fits Poisson/Gamma models and verifies among other things:
exact premium reconstruction from the table, factor = 1 on base rows,
uplift = 1 at reference levels, PDP = exposure-weighted mean response at
exposure 1, offset-neutralised intercepts, inline `factor()` handling, custom
column names, the non-log-link warning, the overdispersion warning, binned
residuals staying within the ±2·SE band for a correct model, the
thin-cell flag, zero dislocation for identical models and
exact rebase behaviour in `premium_impact()`, the Excel workbook structure,
and the generated HTML report. For the interaction tools it simulates a
portfolio with a *known* missing interaction and checks that
`detect_interactions()` ranks the true pair first — and that a portfolio
without any interaction produces no signal. Run it with:

```r
devtools::test()        # or:
devtools::check()       # full R CMD check, as run in CI on every push
```

The GitHub Actions workflow (`R-CMD-check`) runs the full check on every
push to `main`.

## Demo and figures

`demo/run_demo.R` simulates a 100k-policy motor portfolio (with a
deliberately rare fuel type to show the thin-cell flags, and a real
age x region interaction) and writes every deliverable: the HTML report,
the Excel rating workbook and the impact analysis.

`demo/detect_interactions.R` simulates a portfolio where young drivers are
extra risky in the city, fits a GLM *without* that interaction, and walks
through the full cycle: the one-way checks coming back clean, the scan
ranking the true pair far above the rest, the heatmap showing which cells
are mispriced, and the out-of-sample deviance confirming the fix.

`demo/make_readme_figures.R` regenerates the PNGs in `man/figures/` used
above, by rendering the plotly widgets in headless Chrome (requires
`webshot2` and a Chrome installation). Run them from the project root:

```sh
Rscript demo/run_demo.R
Rscript demo/detect_interactions.R
Rscript demo/make_readme_figures.R
```

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
