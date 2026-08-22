# pricingtoolsRmO

[![R-CMD-check](https://github.com/robertmooijer/Pricing-Utils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/robertmooijer/Pricing-Utils/actions/workflows/R-CMD-check.yaml)

An R package for **GLM-based non-life insurance pricing analysis**. It covers the
standard workflow around a frequency/severity model pair: one-way exploration,
actual-vs-expected checks, partial dependence plots, model diagnostics
(dispersion, binned residuals, collinearity, interaction detection),
portfolio-level model comparison (lift, Gini, double lift), the construction
of a multiplicative rating table with thin-cell flags, premium-impact
(dislocation) analysis, and deliverables — a formatted Excel rating workbook
and a one-call HTML report — all with interactive plotly visualisations.

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
  - [`glm_collinearity()`](#glm_collinearity)
  - [`plot_glm_residuals()`](#plot_glm_residuals)
  - [`detect_interactions()`](#detect_interactions)
  - [`plot_residual_heatmap()`](#plot_residual_heatmap)
  - [`screen_features()`](#screen_features)
  - [`make_rating_table()`](#make_rating_table)
  - [`make_rating_plot()`](#make_rating_plot)
  - [`model_lift()`](#model_lift)
  - [`double_lift()`](#double_lift)
  - [`premium_impact()`](#premium_impact)
  - [`export_rating_table()`](#export_rating_table)
  - [`pricing_report()`](#pricing_report)
- [Actuarial methodology](#actuarial-methodology)
  - [Methods at a glance](#methods-at-a-glance) — every function in one table
- [Input validation and warnings](#input-validation-and-warnings)
- [Tests](#tests)
- [Demo and figures](#demo-and-figures)
- [Known limitations](#known-limitations)
- [Appendix: full function specification](#appendix-full-function-specification)
  — arguments, algorithm, return structure, assumptions and cost, per function

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

What each function does, how to call it and how to read its output. For
the full technical specification — every argument, the algorithm step by
step, the exact return structure, the assumptions and the cost — see the
[appendix](#appendix-full-function-specification).

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

### `glm_collinearity()`

Generalised variance inflation factor per **term**.

```r
glm_collinearity(model, threshold = 3)
```

A plain VIF is meaningless for a pricing model, where a spline or a factor
spans several columns. This uses the generalised form of Fox and Monette,
with `GVIF^(1/(2·DF))` putting multi-column terms back on a comparable
scale. Read that column like a VIF on the square-root scale: the default
threshold of 3 corresponds to a VIF of about 9.

A high value means the term is largely explained by the others, so its
coefficient is unstable and **its rating factors cannot be read on their
own** — even though the model's overall predictions may be perfectly fine.
That distinction matters: collinearity damages interpretation, not
prediction, and a rating table is an interpretation.

**Returns** a data.frame with `Term`, `DF`, `GVIF`, `GVIF_scaled` and
`Flag`, ordered by `GVIF_scaled`, plus a warning naming the flagged terms.

> This is the model-level counterpart of the near-duplicate warning in
> [`screen_features()`](#screen_features): that one catches correlated
> *candidates* before they go in, this one catches them once both are in.

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

### `model_lift()`

Lift chart and Gini — does the model separate risk across the whole book?

```r
model_lift(model_freq = NULL, model_sev = NULL, data = NULL,
           actual_col = "SCHADELAST", exposure_col = "Exposure",
           n_bins = 10, y_range = NULL)
```

Every other diagnostic in the package looks at one variable or one pair.
This one asks the portfolio-level question: sort the book by predicted risk
premium, split it into bins of **equal exposure**, and compare actual with
predicted in each. A rising actual line that tracks the predicted one means
the model orders risk; a flat actual line means it orders nothing.

Equal exposure rather than equal policy counts, so every point carries the
same weight — and since both series are in the same units they share one
axis, with no exposure bars needed.

`gini` is computed on the exposure-weighted Lorenz curve. It measures
**ordering only**: a model can have an excellent Gini and still be badly
calibrated, which is exactly what the lift chart itself reveals.

**Returns** `table` (one row per bin), `gini`, `stats`, `plot` (the lift
chart) and `plot_lorenz`.

![Lift chart](man/figures/README-lift.png)

> **Units must match.** Use claim counts with a frequency model and loss
> amounts with a severity or risk premium model. If the overall A/E comes
> out far from 1 the function warns, because that nearly always means
> `actual_col` and the prediction are different quantities.

> `data = NULL` evaluates on the model's own rows — in-sample, and labelled
> as such on the plot. Pass a holdout to `data` for an honest
> out-of-sample lift.

### `double_lift()`

Which of two tariffs is right where they disagree.

```r
double_lift(data, model_freq_new = NULL, model_sev_new = NULL,
            model_freq_old = NULL, model_sev_old = NULL,
            old_premium_col = NULL,
            old_premium_basis = c("amount", "rate"),
            actual_col = "SCHADELAST", exposure_col = "Exposure",
            n_bins = 10, rebase = TRUE, y_range = NULL)
```

Takes the same arguments as [`premium_impact()`](#premium_impact) — either
two model sets or a current premium column. It sorts by the **ratio** of
the two predicted rates, bins by equal exposure, and plots the A/E of each
model per bin with a reference line at 1.0.

This is the test a single lift chart cannot do. Both models can look
convincing on their own lift chart while disagreeing sharply about
individual policies, and only their disagreement decides which is right.
The end bins are where they differ most; the model whose line stays closer
to 1.0 across the range wins.

Both sides are rebased to the same total by default, so the chart is about
**differentiation, not rate level**.

**Returns** `table` (per bin: exposure, mean rate ratio, and the A/E of
each model), `stats` (including `mad_new`, `mad_old` and `winner` — the
mean absolute distance from 1.0 of each model) and `plot`.

Below, the full frequency model against a region-only tariff. The old
tariff drifts from 0.78 to 1.36 across the bins — it is systematically
wrong exactly where the two disagree — while the candidate stays near 1.0:

![Double lift chart](man/figures/README-double-lift.png)

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
               exposure_col = "Exposure", spotlight = NULL,
               x_range = NULL, y_range = NULL)
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

**Who drives the change.** With `by`, each level gets `ExposureShare`,
`MeanChangePct` and `Contribution = share × change`. The contributions of
one variable's levels **sum to the portfolio change**, which answers the
question a pricing committee actually asks: not "how much does this segment
move" but "how much of our +10.5% comes from it".

**Spotlighting a subset.** `spotlight` takes an expression evaluated
against `data`, or a logical vector:

```r
imp <- premium_impact(dat, model_freq_new = m_freq, model_sev_new = m_sev,
                      old_premium_col = "PREMIUM",
                      spotlight = REGION == "City" & AGE < 25)
imp$spotlight$summary
```

It is deliberately a **spotlight and not a filter**: the whole book is
still analysed, the subset appears as a second series on the histogram, and
`$spotlight` reports its exposure share, mean and median change, and its
contribution to the portfolio total. Filtering would remove exactly the
context you need — that a segment moves +18% while the book moves +2%.

Its statistics are computed **after** the book-level rebase, because the
rebase is a decision about the whole portfolio. Rebasing within the subset
would force every segment to average out at zero and tell you nothing.

> **No mix effect here.** Old and new premiums are compared on the *same*
> policies, so the portfolio composition is identical on both sides and
> there is nothing to decompose into rate versus mix. `Contribution`
> answers which segment drives the change, not how much of it is mix. A
> genuine mix-shift analysis needs two portfolio snapshots and is a
> different calculation.

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

The [function reference](#function-reference) above covers *how to use* each
function and how to read its output. This section covers *why* things are
computed the way they are — the reasoning you would want before putting a
number in front of a pricing committee.

### Methods at a glance

One line per function: what it computes, the expression behind it, and the
assumption it relies on. `y` is the response, `μ` the fitted value, `w` the
exposure or prior weight, `a` and `e` actual and expected claims in a cell.

| Function | Computes | Key expression | Relies on |
|---|---|---|---|
| [`agg_all()`](#agg_all) | frequency and severity per level | `Σclaims / Σexposure` and `Σloss / Σclaims` | — |
| [`make_plot()`](#make_plot) | the observed one-way | as above, per level of one variable | — |
| [`plot_glm_predictor()`](#plot_glm_predictor) | actual vs expected per bin | counts: `Σy/Σw` against `Σμ/Σw`; weighted models: prior-weight-weighted means | log link, to recover exposure as `exp(offset)` |
| [`make_pdp()`](#make_pdp) | the partial effect on the response scale | weighted mean of `predict(type = "response")` per grid point, exposure set to 1 → [why](#why-the-pdp-is-computed-on-the-response-scale) | offset is `log(exposure)` |
| [`glm_diagnostics()`](#glm_diagnostics) | fit and dispersion | Pearson `χ²/df`; `1 − D/D₀` | → [overdispersion](#overdispersion) |
| [`glm_collinearity()`](#glm_collinearity) | how far each term is explained by the others | generalised VIF, `GVIF^(1/(2·DF))` | terms, not coefficients |
| [`model_lift()`](#model_lift) | risk separation across the book | `Σactual/Σw` against `Σpred/Σw` per equal-exposure bin; Gini on the Lorenz curve | → [what lift measures](#what-lift-and-gini-do-and-do-not-measure) |
| [`double_lift()`](#double_lift) | which of two tariffs is right where they differ | A/E of each model per bin of the rate ratio `new/old` | rebased to equal totals |
| [`plot_glm_residuals()`](#plot_glm_residuals) | binned residuals | mean residual per bin, band `±2·√(φ/n)` | the dispersion estimate `φ` |
| [`detect_interactions()`](#detect_interactions) | interaction structure the GLM missed | `D = 2·Σ[a·log(a/e*) − (a − e*)]`, where `e*` is `e` raked to `a`'s margins; null by simulation → [why](#why-a-one-way-check-cannot-see-an-interaction) | Poisson counts (otherwise `χ²` scaled by `φ`) |
| [`plot_residual_heatmap()`](#plot_residual_heatmap) | A/E per cell | `Σa / Σe` per cell of two variables | — |
| [`screen_features()`](#screen_features) | incremental value of a candidate | increase in out-of-sample deviance when the feature is shuffled, baseline margin held fixed | a holdout split; log link |
| [`make_rating_table()`](#make_rating_table) | multiplicative relativities | `pred(level, rest at base) / pred(base)`; interaction uplift `joint / (mainₓ · main_g)` → [why](#multiplicative-rating-and-the-reconstruction-identity) | log link |
| [`make_rating_plot()`](#make_rating_plot) | those relativities, plotted | — | → [thin cells](#thin-cells) |
| [`premium_impact()`](#premium_impact) | dislocation against the current tariff | `new/old − 1` per policy, optionally rebased so the totals match | log link, to price at exposure = 1 |
| [`export_rating_table()`](#export_rating_table) / [`pricing_report()`](#pricing_report) | deliverables | — | — |

Two assumptions run through almost the whole table and are worth stating
once. **The log link** is what makes relativities multiplicative and lets
`exp(offset)` be read back as exposure; a different link makes several of
these numbers something other than what their names suggest, which is why
the functions warn about it. **Exposure weighting** is done by summing
numerator and denominator separately (`Σa / Σe`), never by averaging
ratios — an average of ratios would give a policy with a month of exposure
the same say as one with a full year.

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

### What lift and Gini do and do not measure

A Gini measures **ordering**, nothing else. It asks whether the policies
the model calls expensive really are, relative to the ones it calls cheap.
It says nothing about whether the level is right: multiply every prediction
by three and the Gini is unchanged while the tariff is ruinous. The lift
chart is what catches that, because it puts actual and predicted on the
same axis, per bin.

So read them together: **Gini for discrimination, the lift chart for
calibration.** A model can be excellent at one and poor at the other.

A single lift chart also cannot rank two candidates. Both can look
convincing separately while disagreeing sharply about individual policies,
and the disagreement is the whole question. That is what
[`double_lift()`](#double_lift) isolates: it bins on the ratio of the two
predictions, so the end bins contain exactly the policies where the models
part company, and the A/E lines show who is right there.

Two cautions. Lift on the training data flatters both models, which is why
`data` is exposed for a holdout — it will not stop you, but it will label
the plot in-sample. And `double_lift()` rebases the two sides to the same
total by default: without that, a candidate that is simply 10% cheaper
everywhere would look systematically better, when the comparison is about
differentiation rather than rate level.

### Collinearity

Collinearity does not damage predictions; it damages **interpretation**. A
GLM with two near-duplicate predictors can fit and forecast perfectly well
while splitting the effect between them arbitrarily, so the individual
coefficients — and therefore the rating factors — swing wildly with small
changes in the data.

That matters here specifically because a rating table *is* an
interpretation: [`make_rating_table()`](#make_rating_table) reads each
term's coefficients back out as relativities. If two terms are collinear,
those two columns of the table are not trustworthy even though the model
is fine. [`glm_collinearity()`](#glm_collinearity) is the check, on the
generalised VIF scale because pricing terms span multiple columns.

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

---

## Appendix: full function specification

A complete technical specification of every exported function: signature,
every argument, the algorithm step by step, the exact return structure,
the assumptions it rests on, its error and warning behaviour, and its
cost. The [function reference](#function-reference) above is the practical
guide; this appendix is the reference you check when you need to know
precisely what a number is.

Conventions used throughout: `y` is the response, `μ` the fitted value,
`w` the exposure or prior weight, `a` and `e` the actual and expected
claims in a cell, `n` the number of rows, `p` the number of features.
"Model rows" means the rows the model was actually fitted on — the rows of
`model.frame(model)`, which excludes anything dropped by `na.action`.

### Shared behaviour

These rules hold for every function that takes a fitted `glm`.

**Row alignment.** Values are read from the model's own rows. Internally
`model.frame(model)` is matched to `model$data` by row *name*, never by
position, so a model fitted on a filtered subset (`dat[dat$x > 0, ]`) or
one that dropped `NA` rows still lines up with its fitted values. A model
fitted without a `data =` argument cannot be aligned; functions that need
the original columns stop with that message.

**Frequency versus severity mode.** Detected from the model, never
declared by the user:

| Condition | Mode | Actual | Expected | Weight |
|---|---|---|---|---|
| offset present *and* link is `log` | counts | `y` (claim counts) | `μ` (fitted counts, exposure included) | `exp(offset)` |
| otherwise | weighted | `w · y` (e.g. total loss) | `w · μ` | `model$prior.weights` |

Taking totals in both modes means `Σactual / Σexpected` is the correctly
weighted ratio without a separate division step.

**Grouping rule.** Wherever a variable has to be grouped, the same rule
applies: a factor or character keeps its levels; a numeric with at most
`n_bins` distinct values is grouped on its **exact values**; only above
that are quantile bins used. This is why a vehicle age of 0 stays at 0
instead of being merged with 1 into a point at 0.78, and why one outlier
cannot shift every other point.

**Exposure weighting.** Always `Σnumerator / Σdenominator`, never the mean
of per-row ratios: averaging ratios would give a policy with one month of
exposure the same weight as one with a full year.

---

### `agg_all()` specification

**Purpose.** Aggregate raw policy rows to one row per level of a grouping
column, with frequency and severity computed on the aggregate.

```r
agg_all(d, col, by_year,
        exposure_col = "Exposure", claims_col = "AantalClaims",
        loss_col = "SCHADELAST", year_col = "BOEKJAAR")
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `d` | data.frame / data.table | — | raw rows |
| `col` | character(1) | — | grouping column |
| `by_year` | logical(1) | — | also split by accounting year |
| `exposure_col` | character(1) | `"Exposure"` | exposure column |
| `claims_col` | character(1) | `"AantalClaims"` | claim count column |
| `loss_col` | character(1) | `"SCHADELAST"` | loss amount column |
| `year_col` | character(1) | `"BOEKJAAR"` | accounting-year column |

**Algorithm.** Coerce to `data.table`; sum exposure, claims and loss by
`col` (and `year_col`); derive `Frequency = ClaimCount / Exposure` where
exposure is positive and `NA` elsewhere; derive
`Severity = Loss / ClaimCount` where claims are positive and `NA`
elsewhere; return as a `data.frame`.

**Returns.** A data.frame with standardised English names regardless of
the input naming: `col`, `[Year,]` `Exposure`, `ClaimCount`, `Loss`,
`Frequency`, `Severity`. `Year` is a factor.

**Warnings.** One per input column containing `NA` (stating how many), and
one if any group has non-positive exposure (stating how many).

**Errors.** Any named column absent from `d`, listing all missing names.

**Cost.** One `data.table` group-by, O(n).

---

### `make_plot()` specification

**Purpose.** One-way plot of observed frequency or severity with exposure
bars on a secondary axis.

```r
make_plot(data, col, metric = c("Frequency", "Severity"),
          color_single, y_label, display = c("color", "facet"), by_year,
          metric_fmt = 4, exposure_col = "Exposure",
          claims_col = "AantalClaims", loss_col = "SCHADELAST",
          year_col = "BOEKJAAR", discrete_cutoff = 25, y_range = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `data` | data.frame | — | **raw** rows; aggregation happens inside |
| `col` | character(1) | — | X-axis column |
| `metric` | character(1) | `"Frequency"` | quantity to plot |
| `color_single` | colour | — | line colour when `by_year = FALSE` |
| `y_label` | character(1) | — | Y-axis label |
| `display` | character(1) | `"color"` | `"color"` = one line per year; `"facet"` = one panel per year |
| `by_year` | logical(1) | — | split by accounting year |
| `metric_fmt` | integer | `4` | tooltip decimals |
| `discrete_cutoff` | integer | `25` | numeric X with at most this many distinct values is drawn as markers only |
| `y_range` | numeric(2) or NULL | `NULL` | fix the metric axis; `NULL` auto-scales |

**Algorithm.** Call `agg_all()` on the raw rows; rename the X column to an
internal name; decide discrete versus continuous; sort by X (and year);
for the bars, sum exposure over years when `by_year = TRUE`. In `"facet"`
mode the exposure bars are rescaled per panel to the metric range so both
fit one axis (the tooltip shows the true value); in `"color"` mode plotly
draws the bars on a genuine secondary axis.

**Returns.** A plotly object.

**Notes.** In `"color"` mode with `by_year = TRUE` the bars show the sum
over all years, and are labelled "Exposure (all years)" to say so. A fixed
`y_range` in `"facet"` mode switches the facets from free to fixed scales,
so every panel shares it.

**Errors.** Missing columns; `metric` or `display` outside their allowed
values; `y_range` not `c(lo, hi)` with `lo < hi`.

**Cost.** One aggregation plus rendering, O(n).

---

### `make_pdp()` specification

**Purpose.** Partial dependence of a fitted `glm` on the response scale,
overlaid with the observed one-way statistic.

```r
make_pdp(model, raw_data, pred_var, metric = c("Frequency", "Severity"),
         transform = NULL, grid_res = 50, y_label = NULL, metric_fmt = 4,
         exposure_col = "Exposure", claims_col = "AantalClaims",
         loss_col = "SCHADELAST", discrete_cutoff = 25, y_range = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `model` | glm | — | fitted with `data =` |
| `raw_data` | data.frame | — | data for the observed line and the bars |
| `pred_var` | character(1) | — | predictor to profile |
| `metric` | character(1) | `"Frequency"` | which observed statistic to overlay |
| `transform` | function or NULL | `NULL` | **deprecated**, ignored with a warning |
| `grid_res` | integer | `50` | grid points for a continuous predictor |
| `y_label`, `metric_fmt` | | `NULL`, `4` | axis label (auto) and tooltip decimals |
| `y_range` | numeric(2) or NULL | `NULL` | fix the primary axis |

**Algorithm.**

1. Aggregate `raw_data` to the observed line: frequency
   `Σclaims / Σexposure` per level, or severity `Σloss / Σclaims` over
   rows with claims.
2. Recover the training data and the averaging weights: `exp(offset)` for
   frequency, prior weights for severity.
3. **Neutralise the offset** by setting the exposure column to 1, so
   predictions are per unit of exposure.
4. Build the grid: factor levels, or the sorted distinct values when there
   are at most `grid_res` of them, otherwise an equally spaced sequence.
5. **Collapse to unique profiles.** Rows sharing identical values on all
   other predictors give identical predictions at every grid point, so
   they are collapsed with summed weights.
6. For each grid value, set `pred_var` to it across the profiles, predict
   with `type = "response"`, and take the **weighted arithmetic mean**.

**Returns.** A plotly object with three traces: exposure (or claim-count)
bars, "Observed", and "PDP (model)".

**Assumptions.** The offset is `log(exposure)` and the link is `log`;
otherwise `exp(offset)` is not an exposure and a warning is raised.

**Interpretation.** The observed line is a one-way marginal and includes
correlations with every other rating factor; the PDP is a partial effect.
A gap between the two is not by itself evidence of misfit.

**Errors.** Not a `glm`; training data not recoverable; `pred_var` absent;
a column needed for prediction missing from the training data.

**Cost.** `grid_res × (number of unique profiles)` predictions rather than
`grid_res × n`. On a 1M-row portfolio with ~54k profiles this was measured
at 9 s against ~75 s for the naive loop, with identical results to machine
precision. If it is slow, the profile count is the driver: band very
granular covariates or lower `grid_res`.

---

### `plot_glm_predictor()` specification

**Purpose.** Actual versus expected per (binned) level of one predictor,
on the model's own rows.

```r
plot_glm_predictor(model, predictor, n_bins = 150, weight_var = NULL,
                   weight_label = NULL, color = ta_year_palette(1),
                   color_pred = ta_gold, title = NULL, ylab = NULL,
                   xlab = NULL, metric_fmt = 4,
                   bin_type = c("quantile", "width"), y_range = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `n_bins` | integer | `150` | **maximum number of points**, not a fixed bin count |
| `weight_var` | character(1) or NULL | `NULL` | override the weight column (must exist in `model$data`) |
| `bin_type` | character(1) | `"quantile"` | `"quantile"` = equal counts per bin; `"width"` = equal width |
| `y_range` | numeric(2) or NULL | `NULL` | fix the primary axis, e.g. to compare predictors |

**Algorithm.** Detect the mode (see [shared behaviour](#shared-behaviour));
build a frame of predictor, observed, predicted and weight; group by the
[grouping rule](#shared-behaviour); per group compute
`Σobserved / Σweight` against `Σpredicted / Σweight` in counts mode, or
prior-weight-weighted means otherwise. A binned point is positioned at the
weighted mean of its bin; an unbinned point at its exact value.

**Returns.** A plotly object with weight bars, "Observed" and "Predicted".

**Warnings.** An offset with a non-log link (falls back to prior weights).

**Errors.** Not a `glm`; `n_bins < 2`; predictor not found; unknown
`weight_var`; invalid `y_range`.

**Cost.** One `predict()` over the model rows plus a group-by, O(n).

---

### `glm_diagnostics()` specification

**Purpose.** Compact fit summary for a frequency and/or severity model.

```r
glm_diagnostics(model_freq = NULL, model_sev = NULL)
```

**Returns.** One row per supplied model:

| Column | Meaning |
|---|---|
| `Model` | `"frequency"` / `"severity"` |
| `Family`, `Link` | from `family(model)` |
| `N`, `Deviance`, `DFResidual`, `AIC` | standard fit quantities |
| `Dispersion` | Pearson `χ² / df` |
| `DevianceExplained` | `1 − deviance / null deviance` |

**Warnings.** Poisson or binomial with `Dispersion > 1.2`: point estimates
stay consistent but standard errors are understated by roughly `√φ`, so
term selection on naive p-values is anti-conservative.

**Errors.** No model supplied; a supplied object is not a `glm`.

**Cost.** O(n) for the Pearson residuals.

---

### `glm_collinearity()` specification

**Purpose.** Generalised variance inflation factor per model term.

```r
glm_collinearity(model, threshold = 3)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `model` | glm | — | must have at least two terms |
| `threshold` | numeric(1) | `3` | flag terms at or above this `GVIF_scaled` |

**Algorithm.** Take `vcov(model)`, drop the intercept and any aliased
coefficients (matched by name, so a rank-deficient fit does not break it),
and convert to a correlation matrix `R`. For each term with column set
`S`:

```
GVIF = det(R[S,S]) · det(R[-S,-S]) / det(R)
GVIF_scaled = GVIF^(1/(2·DF))
```

This is Fox and Monette's generalisation: for a single-column term it
reduces to the ordinary VIF, and the scaled form makes terms of different
width comparable.

**Returns.** A data.frame `Term`, `DF`, `GVIF`, `GVIF_scaled`, `Flag`,
sorted by `GVIF_scaled` descending. Zero rows with a message when the
model has fewer than two estimable terms.

**Warnings.** Flagged terms are named. A singular correlation matrix —
itself a sign of exact collinearity — returns zero rows with a warning
rather than a numerical error.

**Interpretation.** `GVIF_scaled` is on the square-root scale, so 3
corresponds to a VIF of roughly 9. High values invalidate the *individual*
factors of that term, not the model's predictions.

**Cost.** One `vcov()` plus a determinant per term; negligible.

---

### `model_lift()` specification

**Purpose.** Lift chart and Gini for a risk premium model.

```r
model_lift(model_freq = NULL, model_sev = NULL, data = NULL,
           actual_col = "SCHADELAST", exposure_col = "Exposure",
           n_bins = 10, y_range = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `model_freq`, `model_sev` | glm or NULL | `NULL` | supply both for a risk premium, one for a frequency- or severity-only lift |
| `data` | data.frame or NULL | `NULL` | evaluation data; `NULL` uses the model's own rows (in-sample, labelled on the plot) |
| `actual_col` | character(1) | `"SCHADELAST"` | realised amount: loss for a premium model, claim count for a frequency model |
| `n_bins` | integer | `10` | equal-exposure bins |

**Algorithm.**

1. Predict a rate per unit of exposure from each model with the offset
   neutralised, and multiply them.
2. Drop rows with non-finite values or non-positive exposure, with a
   warning counting them.
3. Order by predicted rate and assign bins by **cumulative exposure**, so
   each bin holds `1/n_bins` of the book.
4. Per bin: `ActualRate = Σactual / Σw`, `PredictedRate = Σ(rate·w) / Σw`,
   `AE` their ratio.
5. Gini from the exposure-weighted Lorenz curve: with `x` the cumulative
   exposure share and `y` the cumulative loss share, both ordered by the
   prediction, `Gini = 1 − 2·∫y dx` by the trapezoid rule.

**Returns.** `table` (`Bin`, `Exposure`, `ExposureShare`, `ActualRate`,
`PredictedRate`, `AE`), `gini`, `stats` (including `in_sample`), `plot`,
`plot_lorenz`.

**Assumptions.** Log link, so `exp(offset)` is an exposure. The actual and
the prediction must be in the same units — pass `actual_col` accordingly.

**Cost.** One prediction pass plus a sort, O(n log n).

---

### `double_lift()` specification

**Purpose.** Compare two tariffs where they disagree.

```r
double_lift(data, model_freq_new = NULL, model_sev_new = NULL,
            model_freq_old = NULL, model_sev_old = NULL,
            old_premium_col = NULL,
            old_premium_basis = c("amount", "rate"),
            actual_col = "SCHADELAST", exposure_col = "Exposure",
            n_bins = 10, rebase = TRUE, y_range = NULL)
```

**Algorithm.** Compute both rates through the same helper
[`premium_impact()`](#premium_impact) uses, so the two cannot drift apart.
Rebase the new rates by `Σ(old·w) / Σ(new·w)` when `rebase = TRUE`. Order
by `ratio = new/old`, bin by equal exposure, and per bin compute the A/E
of each model. `mad_new` and `mad_old` are the mean absolute distance of
those A/E series from 1.0, and `winner` names the smaller.

**Returns.** `table` (`Bin`, `Exposure`, `RateRatio`, `ActualRate`,
`AE_New`, `AE_Old`), `stats` (`mad_new`, `mad_old`, `winner`,
`rate_level_change`), `plot`.

**Interpretation.** The end bins carry the policies where the tariffs
differ most; that is where the comparison is decided. `mad_*` is a summary
of the picture, not a substitute for it — look at *where* a model drifts,
not only how far.

**Errors.** No new model; neither old models nor `old_premium_col`; both
supplied at once; `n_bins < 2`; no usable rows.

---

### `plot_glm_residuals()` specification

**Purpose.** Binned residual plot — readable where a raw residual plot is
not, because policy-level count residuals are dominated by the 0/1 claim
pattern.

```r
plot_glm_residuals(model, predictor = NULL, n_bins = 50,
                   residual_type = c("pearson", "deviance"), y_range = NULL)
```

**Algorithm.** Take residuals of the chosen type; bin by fitted value
(default) or by a predictor using the [grouping rule](#shared-behaviour);
per bin compute the mean residual and the band `±2·√(φ/n)` with `φ` the
Pearson dispersion.

**Returns.** A plotly object: a shaded band with the mean-residual line
for a numeric axis, error bars per level for a categorical one.

**Interpretation.** Under a correctly specified model roughly 95% of bin
means fall inside the band. A systematic run outside it — a curve over a
predictor, a drift with the fitted value — points to missed structure.

**Cost.** O(n).

---

### `detect_interactions()` specification

**Purpose.** Rank every two-way pair by the interaction structure the GLM
has not captured.

```r
detect_interactions(model, vars = NULL, n_bins = 10, min_claims = 30,
                    n_sim = 200, top_n = NULL, seed = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `vars` | character or NULL | `NULL` | candidates; `NULL` = the model's own predictors minus the offset. Variables **not** in the model are allowed, which is how an omitted factor surfaces |
| `n_bins` | integer | `10` | maximum groups per variable |
| `min_claims` | integer | `30` | cells below this are ignored when determining `MaxAE` |
| `n_sim` | integer | `200` | draws for the reference distribution |
| `top_n` | integer or NULL | `NULL` | keep only the strongest pairs |
| `seed` | integer or NULL | `NULL` | reproducible reference distribution |

**Algorithm.** For each pair:

1. Cross-tabulate actual `A` and expected `E` claims over the two grouped
   variables.
2. **Rake `E` to the margins of `A`** by iterative proportional fitting:
   scale each row by `rowSums(A)/rowSums(E)`, then each column likewise,
   until convergence. The raked `E*` then has exactly the row and column
   totals of `A`, so main-effect misfit is scaled away. This is the fit of
   the log-linear model `log(E*ᵢⱼ) = log(Eᵢⱼ) + αᵢ + βⱼ`.
3. `D = 2·Σ[a·log(a/e*) − (a − e*)]` over cells with `e* > 0` — the
   likelihood-ratio statistic between the additive and the saturated model
   on the table, with `df = (R−1)(C−1)` over non-empty rows and columns.
4. Reference distribution. For a Poisson count model: draw
   `A* ~ Poisson(E*)`, re-rake to the margins of `A*` (necessary, since
   those margins differ per draw), recompute `D`, `n_sim` times. Otherwise
   the dispersion-scaled chi-square is used.
5. `Z = (D − mean(D*)) / sd(D*)`; `P = (1 + #{D* ≥ D}) / (n_sim + 1)`.

**Returns.** A data.frame, strongest `Z` first:

| Column | Meaning |
|---|---|
| `VarX`, `VarY` | the pair |
| `Cells`, `Claims` | non-empty cells and total claims behind the test |
| `Deviance`, `DF` | the raw statistic and its degrees of freedom — **not comparable across pairs**, since `Deviance` grows with `DF` |
| `Z` | the standardised statistic; **this is the ranking column** |
| `P` | bootstrap p-value, floored at `1/(n_sim+1)` |
| `Method` | `"simulation"` or `"chisq"` |
| `MaxAE` | A/E of the worst cell holding at least `min_claims` claims |
| `MaxAE_Claims`, `MaxAE_ExposureShare` | that cell's volume and its share of total exposure |

**Interpretation.** `Z` answers "is it real", `MaxAE` and
`MaxAE_ExposureShare` answer "does it matter" — judge them separately. A
negative `Z` means the cells fit better than chance and is not evidence of
anything. Look at the *gap* between the top `Z` and the rest; a top value
only marginally above the others is likely the upper tail of noise.

**Limitation.** The simulation assumes Poisson noise. On an overdispersed
portfolio the null is too narrow and `Z` is optimistic — check
`glm_diagnostics()` and treat `Z` as an upper bound when the dispersion is
well above 1.

**Cost.** `O(p²)` pairs, each `n_sim` rakes of a small table. Cheap: the
tables are at most `n_bins × n_bins`.

---

### `plot_residual_heatmap()` specification

**Purpose.** The A/E ratio per cell of two variables — where a model
leaks.

```r
plot_residual_heatmap(model, var_x, var_y, n_bins = 20,
                      min_claims = 30, z_range = NULL, title = NULL)
```

**Algorithm.** Group both variables by the
[grouping rule](#shared-behaviour); sum actual, expected, exposure and
claims per cell; `AE = ΣActual / ΣExpected`; flag cells below
`min_claims`; build the `z` matrix with thin cells set to `NA` so they are
not coloured, and overlay them as grey crosses.

**Returns.** A plotly heatmap. The cell table — `gx`, `gy`, `Actual`,
`Expected`, `Exposure`, `Claims`, `AE`, `IsThin` — is attached as the
`"cells"` attribute.

**Colour.** Diverging around 1.0 with `zmid = 1`: blue below (overpriced),
red above (underpriced), neutral grey at break-even. Two hues and a
neutral midpoint, never a rainbow, so a colour change cannot be mistaken
for a change of direction. `zmid` keeps the scale centred on break-even
whatever the data range; `z_range` fixes it across several heatmaps.

**Design note.** Thin cells are deliberately left blank. Their A/E is
mostly noise, and colouring them would make the least reliable cell the
loudest thing on the plot. Their volume remains in the tooltip.

**Cost.** O(n) plus a small cross-tabulation.

---

### `screen_features()` specification

**Purpose.** Fit a boosted challenger with the GLM as an offset, so it can
only model what the GLM leaves behind. Returns diagnostics only — no
scorable model. Requires `xgboost`.

```r
screen_features(model, features = NULL, split = c(0.6, 0.2, 0.2),
                max_depth = 2, eta = 0.05, nrounds = 2000,
                early_stopping_rounds = 40, n_shap = 4000,
                cor_threshold = 0.95, max_levels = 50, seed = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `model` | glm | — | the baseline. May be as small as `y ~ 1 + offset(log(Exposure))` |
| `features` | character or NULL | `NULL` | candidates; `NULL` = every usable column except response and offset |
| `split` | numeric(3) | `c(.6,.2,.2)` | train / validation (early stopping) / test (reporting); must sum to 1 |
| `max_depth` | integer | `2` | depth of the second booster |
| `eta`, `nrounds`, `early_stopping_rounds` | | `.05`, `2000`, `40` | boosting parameters |
| `n_shap` | integer | `4000` | rows for the SHAP interaction ranking; `0` skips it |
| `cor_threshold` | numeric(1) | `0.95` | report numeric candidates above this as near-duplicates |
| `max_levels` | integer | `50` | factors with more levels are skipped |
| `max_rows` | integer or NULL | `1e6` | work on a random sample of at most this many rows; `NULL` uses all |
| `nthread` | integer or NULL | `NULL` | boosting threads; `NULL` = available cores minus one |
| `seed` | integer or NULL | `NULL` | reproducible sample, split and boosting |

**Performance.** Boosting dominates the runtime and everything scales with
the row count, so on a production portfolio the two levers are `max_rows`
and `nthread`. Measured on a simulated 1.5M-row set with 10 candidates:
148 s on all rows against 7 s at `max_rows = 1e5`, with the signal/noise
ordering identical at every size. Screening ranks candidates rather than
estimating them precisely, which is why sampling is cheap here — but very
aggressive sampling can let a noise feature climb past a weak real one, so
treat a surprising ranking on a small sample with suspicion before
believing it.

**Algorithm.**

1. Classify candidates and drop the unusable, one warning per reason:
   `constant`, `too many levels`, `unsupported type`.
2. Fix the factor level map **once on the full data**, so a column has the
   same matrix width in every split.
3. Split rows, then **refit the baseline on the training split**, so both
   sides of the comparison are out of sample.
4. Fit a depth-1 booster (additive corrections only) and a
   depth-`max_depth` booster, both with `base_margin = log(μ_baseline)`
   and early stopping on the validation split.
5. Deviance of baseline, depth 1 and depth `max_depth` on the test split,
   under the model's own family.
6. Permutation importance: shuffle one candidate in the test split, keep
   `base_margin` **fixed at the unpermuted value**, and record the
   deviance increase. Holding the margin fixed is what makes this the
   *incremental* contribution rather than the importance in the combined
   model.
7. SHAP interaction values on a sample, aggregated to feature level.

**Returns.** A list:

| Element | Contents |
|---|---|
| `summary` | staged deviance comparison, three rows |
| `verdict` | one sentence, staged (see below) |
| `features` | `Feature`, `InModel`, `PermDeviance`, `Gain`, sorted by `PermDeviance` |
| `gain_note` | why `Gain` must not be used as the ranking |
| `interactions` | SHAP ranking per pair, or `NULL` |
| `correlated` | near-duplicate pairs, or `NULL` |
| `stats` | raw deviances, percentages, objective, split sizes, best iterations |
| `plot` | ranked incremental importance |

**Reading it.** The verdict is staged deliberately: while the depth-1 step
still improves the fit, main effects are missing and the interaction
ranking cannot be trusted — fix those first and run again. Rank on
`PermDeviance`, never on `Gain`: `Gain` is biased towards continuous and
high-cardinality features and in testing gave a pure-noise column a 13.5%
share while the permutation test correctly placed it below zero. A
`PermDeviance` at or below zero means no usable signal. Near-duplicates
split their importance, so which of a correlated pair comes out on top is
arbitrary.

**Sensitivity.** A booster is *less* sensitive to a two-way interaction
between known rating factors than `detect_interactions()`: on identical
data a case where the cell test reached `Z = 7.7` left the booster
reporting nothing at all. Use this to screen features and for the global
verdict; use `detect_interactions()` to hunt interactions.

**Assumptions.** Poisson/quasipoisson counts with a log-link offset, or
Gamma with a log link; anything else is refused. The split is internal and
does not propagate to the rest of the package.

**Warnings.** Dropped candidates (with the reason); near-duplicate pairs;
a baseline that could not be refitted, naming the variable that lost its
variation.

**Cost.** Two boosting runs plus `p` permutation passes over the test
split, plus `O(n_shap · k²)` for the SHAP array with `k` matrix columns.

---

### `make_rating_table()` specification

**Purpose.** Multiplicative rating factors from a frequency and/or
severity model, including two-way interactions.

```r
make_rating_table(model_freq = NULL, model_sev = NULL, data,
                  grid_res = 50, exposure_col = "Exposure",
                  claims_col = "AantalClaims",
                  base_level = c("first", "exposure"),
                  trim = c(0, 1), min_claims = 30)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `base_level` | character(1) | `"first"` | reference level: first `xlevel`, or the level with the largest exposure |
| `trim` | numeric(2) | `c(0, 1)` | quantile range for continuous grids, e.g. `c(.005, .995)` against outlier extrapolation |
| `min_claims` | integer | `30` | thin-cell threshold for `IsThin` |

**The base point.** One consistent convention: categorical variables at
their reference level, continuous variables at their **median** (inserted
as an explicit grid point so a row with factor exactly 1 exists), and the
exposure column at **1**. Hence, for log-link models without interactions,
the identity holds exactly:

```
prediction(x₁, …, xₖ, exposure = 1) = intercept × Factor(x₁) × … × Factor(xₖ)
```

verified to 1e-10 in the test suite.

**Algorithm.** Resolve each term label to its underlying column
(`ns(AGE, 4)` → `AGE`), keyed so that inline transformations such as
`factor(YEAR)` are still recognised as categorical; build a grid per
variable; predict on the grid with all other predictors at base and divide
by the prediction at base; a variable absent from one model gets the
neutral factor 1 there. For each two-way term, predict over the 2D grid
and scale to the base cell, then derive the uplift
`joint / (mainₓ · main_g)`.

**Returns.** One row per level or grid point:

| Column | Meaning |
|---|---|
| `Variable` | variable name, or `"A:B"` for an interaction — **named by R's own term ordering**, which may differ from what you typed, so look it up rather than assume |
| `Type` | `"categorical"` / `"continuous"` |
| `Level`, `LevelNum` | level as text and, where applicable, numeric |
| `Group`, `XVar`, `GroupVar` | interaction bookkeeping (`NA` for main effects) |
| `IsBase` | `TRUE` on the base row or cell, where the factor is exactly 1 |
| `Exposure`, `ClaimCount` | data volume |
| `Factor_Frequency`, `Factor_Severity`, `Factor_Premium` | relativities; `Premium = Frequency × Severity` |
| `Uplift_*` | interaction rows only: the pure interaction effect, 1 everywhere when there is none |
| `IsThin` | `ClaimCount < min_claims` |

Attributes: `intercept_frequency`, `intercept_severity`,
`intercept_premium` (predictions at the base point, per unit of exposure)
and `base_values`.

**Warnings.** A non-log link (factors are then not pure relativities);
skipped variables or interactions, with the reason; thin categorical
levels.

**Cost.** A handful of small `predict()` calls per variable — grids are
tens to hundreds of rows, never `n`.

---

### `make_rating_plot()` specification

**Purpose.** Plot one variable or interaction from a rating table.

```r
make_rating_plot(rating_tbl, var, metric_fmt = 4, metric = NULL,
                 y_range = NULL)
```

**Behaviour.** Main effects show the frequency, severity and premium
curves with a dotted reference line at 1.0 and exposure bars on the
secondary axis. Interactions (`var = "A:B"`) show one series per level of
the group variable for a single metric — `metric` picks it, defaulting to
`"Premium"` when both models are present. Levels flagged `IsThin` are
drawn with dimmed markers and a "low claim volume" note in the tooltip.
Categorical axes get markers only, never a connecting line.

**Errors.** Variable not present in the table; invalid `y_range`.

---

### `premium_impact()` specification

**Purpose.** Per-policy dislocation analysis of a new model set against
old models or an existing premium column.

```r
premium_impact(data, model_freq_new = NULL, model_sev_new = NULL,
               model_freq_old = NULL, model_sev_old = NULL,
               old_premium_col = NULL,
               old_premium_basis = c("amount", "rate"),
               rebase = TRUE, by = NULL, n_show = 10,
               exposure_col = "Exposure", x_range = NULL, y_range = NULL)
```

| Argument | Type | Default | Meaning |
|---|---|---|---|
| `old_premium_basis` | character(1) | `"amount"` | `"amount"` = premium for the record, divided by exposure internally; `"rate"` = already per unit of exposure |
| `rebase` | logical(1) | `TRUE` | scale new premiums so the exposure-weighted totals match the old |
| `by` | character or NULL | `NULL` | columns for a per-level breakdown |
| `n_show` | integer | `10` | rows in the winners/losers tables |
| `spotlight` | expression, logical or NULL | `NULL` | subset to report separately, evaluated against `data`; the book itself is still analysed in full |
| `x_range`, `y_range` | numeric(2) or NULL | `NULL` | fix the histogram axes; `x_range` bounds the change axis and only sets the visible window — the bins always span the full range, so the statistics are unaffected |

**Algorithm.** Predict both sides as rates per unit of exposure (offsets
neutralised at exposure = 1); the premium is the product of the models
supplied. Drop rows with a non-finite or non-positive premium or exposure.
Compute the overall rate-level change, apply the rebase factor when asked,
then the per-policy change. Quantiles are exposure-weighted. The histogram
is pre-binned in R into 60 bars, because a plotly histogram trace would
embed every raw per-policy value in the widget (tens of MB on a large
portfolio).

**Returns.** `summary`, `stats`, `policy` (per row: `OldRate`, `NewRate`,
`NewRateRebased`, `ChangePct`), `by_level`, `largest_increases`,
`largest_decreases`, `plot`.

**Why rebase.** It separates the two questions a dislocation analysis
answers: the overall rate-level change, and the redistribution across the
portfolio. Without it the two are entangled in one number.

**Errors.** No new model; neither old models nor `old_premium_col`; both
of those supplied at once; no usable rows left; invalid ranges.

**Cost.** Two or four `predict()` passes over `data`, O(n).

---

### `export_rating_table()` specification

**Purpose.** Formatted Excel export of a rating table. Requires
`openxlsx`.

```r
export_rating_table(rating_tbl, file = "rating_table.xlsx",
                    overwrite = TRUE, digits = 4)
```

**Workbook.** An **Overview** sheet with a timestamp, the intercepts per
unit of exposure and the base value per variable; one sheet per
main-effect variable with the base row highlighted and thin rows greyed
out; one sheet per interaction with the long table plus a Level × Group
matrix of the premium factor. Sheet names are sanitised to Excel's rules
and deduplicated (`LEEFTIJD:REGIO` → `LEEFTIJD_REGIO`).

**Returns.** Invisibly, the normalised path.

---

### `pricing_report()` specification

**Purpose.** Bundle the whole analysis into one self-browsable HTML
report.

```r
pricing_report(model_freq = NULL, model_sev = NULL, data,
               file = "pricing_report.html", title = "GLM Pricing Report",
               variables = NULL,
               include = c("diagnostics", "oneway", "ae", "pdp", "rating",
                           "interactions"),
               by_year = FALSE, top_interactions = 3,
               exposure_col = "Exposure", claims_col = "AantalClaims",
               loss_col = "SCHADELAST", year_col = "BOEKJAAR",
               grid_res = 50, base_level = c("first", "exposure"),
               trim = c(0, 1))
```

**Blocks.** `include` selects them: `"diagnostics"` (fit table and binned
residuals), `"oneway"`, `"ae"`, `"pdp"` and `"rating"` per variable,
and `"interactions"` (the scan table plus the strongest
`top_interactions` pairs as heatmaps). `variables` defaults to every base
variable of both models.

**Robustness.** Each plot is wrapped: a failure becomes a visible note in
the report instead of aborting the run.

**Output.** Built with `htmltools::save_html()`, so **no pandoc is
required** and the plots stay interactive. A `<name>_files/` folder is
written alongside the `.html` with the JavaScript dependencies — keep the
two together when sharing.

**Returns.** Invisibly, the normalised path.

---

### House-style colours

`ta_navy` `#00365E`, `ta_blue` `#0073AB`, `ta_lightblue` `#A8C8E0`,
`ta_gold` `#D39F27`, `ta_muted` `#6B7A8D`, and `ta_years_base`, a
nine-colour vector. `ta_year_palette(n)` returns `n` colours from it,
interpolating beyond nine.

`ns()` and `bs()` are re-exported from `splines`, so spline terms work in
model formulas without attaching `splines` yourself.
