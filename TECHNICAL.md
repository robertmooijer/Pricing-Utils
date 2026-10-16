# pricingtoolsRmO: technical documentation

This document records **how the package is built and why**: every
assumption it makes about the models and the data, every choice between two
defensible options, what it refuses and what it tolerates, and where in the
code each of those lives. It complements the README, which explains how to
use the functions and how to read their output.

It is written to be searched, by a person or by an AI assistant. Every
decision has a stable identifier such as `OFF-05`. The same identifier
appears as a comment tag, `# [OFF-05]`, at the place in the code that
implements it, so one search finds both the reasoning and the code:

```sh
grep -rn "OFF-05" R/ TECHNICAL.md
```

The cross-cutting conventions in section 2 apply everywhere and are tagged
only where they have one concrete place (`CONV-02`, `CONV-04`, `CONV-08`).
`AE-04` documents behaviour of the statistics rather than of the code, and
has no tag.

Each register entry has the same fields:

- **Decision**: what the code does.
- **Why**: the reasoning, including the alternative that was rejected.
- **Where**: file and function. Line numbers are deliberately not given,
  because they drift; the tag is the anchor.
- **Evidence**: measurements behind the choice, where there are any.
- **Pinned by**: the tests that fail if the behaviour changes.
- **History**: only where a later change altered results, with the commit.

Contents

1. [Architecture](#1-architecture)
2. [Cross-cutting conventions](#2-cross-cutting-conventions)
3. [Decision and assumption register](#3-decision-and-assumption-register)
   - [OFF: offsets and the link](#off-offsets-and-the-link)
   - [RT: rating table](#rt-rating-table)
   - [MV: merged and grouped columns](#mv-merged-and-grouped-columns)
   - [SCORE: data a model was not fitted on](#score-data-a-model-was-not-fitted-on)
   - [RATE: premium impact and lift](#rate-premium-impact-and-lift)
   - [AE: actual versus expected](#ae-actual-versus-expected)
   - [INT: interactions](#int-interactions)
   - [PDP: partial dependence](#pdp-partial-dependence)
   - [DIAG: diagnostics, Q-Q and influence](#diag-diagnostics-q-q-and-influence)
   - [SCR: feature screening](#scr-feature-screening)
   - [EXP: Excel export](#exp-excel-export)
   - [REP: HTML report](#rep-html-report)
   - [DATA: data handling and one-way aggregation](#data-data-handling-and-one-way-aggregation)
4. [Known limitations and open points](#4-known-limitations-and-open-points)
5. [Test map](#5-test-map)
6. [Glossary](#6-glossary)

---

## 1. Architecture

### 1.1 What the package is

An R package for GLM-based non-life pricing built around a frequency and
severity model pair: Poisson (or quasi-Poisson) counts with an exposure
offset, and a Gamma severity on the average claim weighted by the claim
count, both with a log link. Everything works on fitted `glm` objects. The
package never fits a pricing model itself; it explains, checks, tabulates
and compares models the user has fitted.

All plots are interactive `plotly` widgets. There is no state between
calls: every function takes models and data and returns a value.

### 1.2 File map

| File | Exported | Internal | Responsibility |
|---|---|---|---|
| `R/utils-internal.R` | none | all shared helpers (section 1.4) | offset handling, rates, training data, grids, grouped columns, binning, Gini |
| `R/rating-table.R` | `make_rating_table()` | closures inside the function | multiplicative factors, intercepts, interactions, thin cells, `base`, `merge_vars` |
| `R/groupings.R` | `add_groupings()` | none | adds grouped columns to data to be scored, sharing code with `merge_vars` |
| `R/rating-plot.R` | `make_rating_plot()` | none | plots one variable or interaction of a rating table |
| `R/export.R` | `export_rating_table()` | `.numeric_levels()` | formatted `.xlsx` with an Overview, a Tariff lookup sheet, one sheet per variable |
| `R/impact.R` | `premium_impact()` | none | per-policy dislocation, rebase, contribution, spotlight |
| `R/lift.R` | `model_lift()`, `double_lift()` | none | lift chart, Gini with a bootstrap interval, Lorenz curve, double lift with an interval behind its verdict |
| `R/ae.R` | `plot_glm_predictor()` | none | actual versus expected per level or bin, in-sample or on new data, levels or ratio |
| `R/pdp.R` | `make_pdp()` | none | partial dependence on the response scale with an observed overlay |
| `R/agg.R` | `agg_all()` | none | raw rows to frequency and severity per level |
| `R/plot-oneway.R` | `make_plot()` | none | observed one-way plot with volume bars |
| `R/diagnostics.R` | `glm_diagnostics()`, `glm_collinearity()`, `plot_glm_residuals()` | none | fit table, generalised VIF, binned residuals |
| `R/qq-influence.R` | `plot_glm_qq()`, `plot_glm_influence()` | `.qr_dispersion()`, `.quantile_residuals()` | randomised quantile residuals, leverage and Cook's distance |
| `R/interactions.R` | `plot_residual_heatmap()`, `detect_interactions()` | `.ta_diverging` | cell-level A/E, interaction ranking by raking and simulation |
| `R/screen.R` | `screen_features()` | `.fam_deviance()`, `.screen_levels()`, `.screen_matrix()` | boosted challenger on the GLM margin |
| `R/report.R` | `pricing_report()` | closures | one HTML file with diagnostics, one-ways, A/E, PDP, rating plots, interaction scan |
| `R/colors.R` | `ta_navy`, `ta_blue`, `ta_lightblue`, `ta_gold`, `ta_muted`, `ta_years_base`, `ta_year_palette()` | none | house colours |
| `R/reexports.R` | `ns`, `bs` | none | re-export of `splines::ns` and `splines::bs` |
| `R/pricingtoolsRmO-package.R` | none | `globalVariables` | imports, data.table awareness |

### 1.3 How the main calls flow

**Rating table.** `make_rating_table()` validates `merge_vars` and derives or
checks grouped columns (`.grouping_column()`), resolves every model term to
its base column (`.term_base_var()`), picks the base value per variable,
builds a grid per continuous variable (`.nice_step()`, `.list_values()`,
`.step_grid()`), predicts each grid row against a base row with every other
variable at its base, divides, and adds two-way interaction blocks. The
intercepts are predictions of the base row with the whole offset taken out
(`.predict_no_offset()`).

**Rates for comparison.** `premium_impact()` and `double_lift()` call
`.old_new_rates()`, which applies `at` (`.apply_at()`) and calls
`.model_rate()` per model; `model_lift()` calls `.apply_at()` and
`.model_rate()` directly. `.model_rate()` first names any value the model
was not fitted on (`.unseen_levels()`, an error) and any numeric value
outside the fitted range (`.range_warning()`), then asks `.offset_split()`
which offset expressions to subtract and calls `.predict_no_offset()` with
only those.

**Models read as fitted with na.omit.** Every function that mixes
`residuals()`, `fitted()` or `predict()` with the model frame first calls
`.as_na_omit()` on its local copy of the model (DATA-06).

**Actual versus expected.** `plot_glm_predictor()` takes its rows from
`.glm_training_data()` (in-sample) or `.glm_newdata_parts()` (new data).
`plot_residual_heatmap()` and `detect_interactions()` take actual, expected
and volume per row from `.glm_ae_parts()` and group them with
`.group_values()`.

**Grouped columns.** `add_groupings()` and the `merge_vars` part of
`make_rating_table()` both call `.grouping_column()`, which reads the
source-to-group relation from the data each model carries and decides
between exact lookup and banding with `.source_kind()`.

### 1.4 Internal helper index

All in `R/utils-internal.R` unless stated otherwise.

| Helper | Purpose | Called from |
|---|---|---|
| `.check_cols(d, cols, fn)` | stop with a named message when columns are missing | most exported functions |
| `.as_df(x)` | normalise data.table and tibble to a plain data.frame | most exported functions |
| `.model_data(model)` | the data a model was fitted on: `model$data`, or the call's `data` evaluated where the formula was written | `.glm_training_data()`, `.source_kind()`, `.grouping_column()`, `pricing_report()` |
| `.as_na_omit(model)` | read a model fitted with `na.exclude` as if fitted with `na.omit` | every function that mixes residuals or fitted values with the model frame |
| `.glm_training_data(model, fn)` | model frame, the training data aligned by row name, prior weights, offset | `ae.R`, `diagnostics.R`, `lift.R`, `pdp.R`, `qq-influence.R`, `screen.R`, `.glm_ae_parts()` |
| `.glm_newdata_parts(model, newdata, fn)` | the same pieces evaluated on other data | `plot_glm_predictor()` |
| `.check_range(rng, fn, arg)` | validate `c(lo, hi)` axis ranges | every plot with `y_range`, `x_range` or `z_range` |
| `.coerce_like(vals, template)` | coerce grid values to a column's type and factor levels; a logical stays logical | `make_rating_table()`, `make_pdp()`, `.apply_at()` |
| `.unseen_levels(model, data)` | values of a factor term the model was not fitted on, per column, with a row mask | `.model_rate()`, `.glm_newdata_parts()`, `.predictable_rows()`, `make_rating_table()` |
| `.unseen_message(un, label, fn, at_hint)` | the error text for those values | `.model_rate()`, `.glm_newdata_parts()` |
| `.model_label(label)` | "model" or "frequency model" in a message | `.unseen_message()`, `.range_warning()` |
| `.range_warning(model, data, label, fn)` | warn when numeric predictors leave the fitted range | `.model_rate()` |
| `.apply_at(data, at, fn)` | set columns to one value for every row (`at = list(BOEKJAAR = 2024)`) | `.old_new_rates()`, `model_lift()` |
| `.rhs_vars(model)` | variables on the right-hand side, response excluded | many |
| `.glm_ae_parts(model, fn)` | actual, expected, volume and claim weight per fitted row | `plot_residual_heatmap()`, `detect_interactions()` |
| `.glm_var_values(parts, var, fn)` | a variable aligned with the fitted rows | `interactions.R` |
| `.group_values(x, n_bins)` | categorical as is, few numeric values exact, else quantile bins | `interactions.R` |
| `.offset_exprs(model)` | every offset expression, from the terms and from `model$call$offset` | the offset helpers below |
| `.offset_value(model, data, idx)` | evaluated offset, optionally only some expressions | `.predict_no_offset()`, `.glm_newdata_parts()` |
| `.predict_no_offset(model, newdata, idx)` | `linkinv(eta - offset)` | `.model_rate()`, `make_pdp()`, `make_rating_table()` |
| `.offset_split(model, exposure_col, keep_offsets)` | which offset expressions to subtract and which stay | `.model_rate()`, `.kept_offsets()` |
| `.kept_offsets(models, exposure_col, keep_offsets)` | names of the offsets that stay in a rate | `.old_new_rates()`, `model_lift()` |
| `.kept_label(kept)` | text for the summary line | `premium_impact()` |
| `.offset_is_log(model)` | is every offset a `log()` | `ae.R`, `pdp.R`, `.glm_ae_parts()` |
| `.offset_vars(model)` | variables appearing in any offset | many |
| `.default_nthread()` | thread count respecting R CMD check and `OMP_THREAD_LIMIT` | `screen_features()` |
| `.model_rate(model, data, exposure_col, fn, keep_offsets, label)` | predicted rate per unit of exposure, after the unseen-level and range checks | `model_lift()`, `.old_new_rates()` |
| `.old_new_rates(..., at, divisor_col)` | old and new rate per policy from models or a premium column | `premium_impact()`, `double_lift()` |
| `.check_ae_scale(actual, predicted, fn, actual_col)` | warn when the overall A/E suggests mismatched units | `model_lift()`, `double_lift()` |
| `.exposure_bins(score, weight, n_bins, fn)` | equal-weight bins ordered by a score | `model_lift()`, `double_lift()` |
| `.gini_exposure(score, actual, weight)` | exposure-weighted Gini | `model_lift()` |
| `.gini_ordered(w, a)` | the same on vectors already in score order | `.gini_exposure()`, `.gini_boot()` |
| `.gini_boot(score, actual, weight, n_boot, conf)` | Poisson-weight bootstrap interval for the Gini | `model_lift()` |
| `.top_share(actual, k)` | share of the total held by the `k` largest records | `.large_loss_note()` |
| `.large_loss_note(actual, actual_col)` | the sentence naming large losses, when the ten largest carry 10% or more | `model_lift()`, `double_lift()` |
| `.check_boot(n_boot, conf_level, fn)` | validate the bootstrap arguments | `model_lift()`, `double_lift()` |
| `.drop_reasons(flags)` | count dropped rows per reason | `premium_impact()`, `model_lift()`, `double_lift()` |
| `.nil_claims_warning(...)` | warn when claims without payment make frequency x severity too high | `make_rating_table()`, `model_lift()` |
| `.year_warning(models, year_col, at, fn)` | warn when a model prices each policy at its own accounting year | `premium_impact()` |
| `.predictable_rows(model, newd)` | rows of a prediction frame without an unseen level | `make_rating_table()` |
| `.dev_resid_moments(model)` | mean and variance of each row's deviance residual under the fitted family | `plot_glm_residuals()` |
| `.nb_theta(model)` | theta of a negative binomial fit, or `NULL` | `.dev_resid_moments()`, `.quantile_residuals()` |
| `.nice_step(rng, n, integer_data)` | a 1, 2 or 5 times 10^k step | `make_rating_table()` |
| `.list_values(uv, n, integer_data)` | list a whole-number column value by value | `make_rating_table()` |
| `.step_grid(lo, hi, step, base)` | grid on whole multiples of the step, base included | `make_rating_table()` |
| `.level_chr(x)` | grid values as text without scientific notation | `make_rating_table()` |
| `.snap_to_step(x, step, lo, hi)` | round a value onto the grid, clamped | `make_rating_table()` |
| `.term_base_var(term, data)` | the column behind a term label | `make_rating_table()`, `.model_base_vars()`, `.source_kind()`, `plot_glm_predictor()` |
| `.source_kind(pv, models, src)` | is a grouping source categorical or continuous | `.grouping_column()` |
| `.grouping_column(data, pv, a, model_freq, model_sev, fn)` | derive or verify one grouped column | `make_rating_table()`, `add_groupings()` |
| `.model_base_vars(model, data)` | base columns of a model's terms | `pricing_report()`, `detect_interactions()`, `plot_glm_influence()` |

### 1.5 Dependencies

| Package | Kind | Used for |
|---|---|---|
| `data.table` | Imports, fully imported | fast group-by and unique on large books; `.datatable.aware <- TRUE` |
| `dplyr`, `magrittr`, `rlang` | Imports | group-by in the plotting code, the pipe, `.data` |
| `ggplot2` | Imports | the facet mode of `make_plot()` only, converted with `ggplotly()` |
| `plotly` | Imports | every plot |
| `splines` | Imports | `ns()` and `bs()`, re-exported |
| `stats`, `utils`, `grDevices`, `parallel` | Imports | base functionality, `detectCores()` |
| `openxlsx` | Suggests | `export_rating_table()` only |
| `htmltools` | Suggests | `pricing_report()` only |
| `xgboost` | Suggests | `screen_features()` only |
| `statmod` | Suggests | tests only: the reference for quantile residuals |
| `MASS` | Suggests | tests only: a `glm.nb()` fit, which keeps no `model$data` |
| `tibble`, `testthat` | Suggests | tests only |

Optional packages are checked with `requireNamespace()` at the top of the
function that needs them, with an install hint in the error.

---

## 2. Cross-cutting conventions

**CONV-01 Dutch column defaults.** The column arguments default to
`Exposure`, `AantalClaims`, `SCHADELAST` and `BOEKJAAR`. That is the
owner's data, and the defaults are deliberately kept rather than
translated. Every function that reads such a column takes it as an argument.

**CONV-02 Normalise once at the entry point.** A data.table reads
`d[, cols]` as an expression and a tibble returns a tibble where a vector is
expected, so input is converted with `.as_df()` once, at the start, instead
of guarded at every call site. Where a data.table is the natural input
(`agg_all()`), or must come back as one (`add_groupings()`), that is handled
explicitly. Pinned by `test-data-table-input.R`.

**CONV-03 Refuse rather than guess; warn when the result is still right.**
The line between an error and a warning is whether the output would still
be correct. An unknown factor level, an offset that cannot be evaluated per
row, a grouping that means two things, or a column that is missing are
errors, because any number produced would be a guess. A non-log link, a
thin cell, a misspelled `grid_step` name, an undeclared regrouping, or a
value outside the fitted range of a banding are warnings, because the result
is still correct and only needs reading with care. Errors and warnings are
prefixed with the function name and raised with `call. = FALSE`, so the
message says where it comes from and nothing else.

**CONV-04 Partial output over no output.** Where a function builds many
independent pieces, one failing piece becomes a warning and is skipped: a
variable in `make_rating_table()`, an interaction pair in
`make_rating_table()` and `detect_interactions()`, a plot in
`pricing_report()`. The function fails only when nothing at all could be
produced. The cost is that a skipped piece is easy to miss in a long run, so
the warning always names it.

**CONV-05 The model is read, not assumed.** Link, inverse link, offset
expressions, prior weights, factor levels and the training data are all
taken from the fitted object (`family()`, `terms()`, `model$call`,
`model$prior.weights`, `model$xlevels`, `model$data`). Several functions
therefore need a model fitted with a `data =` argument, and say so.

**CONV-06 ASCII-only source.** All R files are ASCII; non-ASCII characters
in messages and plot titles are written as `\uXXXX` escapes. Required for a
clean `R CMD check`.

**CONV-07 Plots.** Every plot is a `plotly` widget in the house colours
(`R/colors.R`), with a volume bar on a secondary axis where that applies,
and a `y_range` (or `x_range`, `z_range`) argument so that plots of
different variables can share a scale. Categorical axes show markers only,
never a line through categories.

**CONV-08 Randomness.** Functions with a `seed` argument
(`plot_glm_qq()`, `plot_glm_influence()`, `detect_interactions()`,
`screen_features()`, `model_lift()`, `double_lift()`) call `set.seed()` and
so change the global random state. Without `seed` they use whatever state
there is.

**CONV-09 Documentation style.** The README is in English and uses no em
dashes. Each register entry here is written so it can be read on its own.

---

## 3. Decision and assumption register

### OFF: offsets and the link

#### OFF-01 Offsets are read from both places predict() reads them

- **Decision.** An offset is collected from the formula terms
  (`attr(terms, "offset")`) and from the `offset =` argument
  (`model$call$offset`). A model can carry both; they add.
- **Why.** `glm(y ~ x, offset = log(E))` never reaches `terms()`. Reading
  only the terms left that offset in the prediction, silently and 70% out.
  `predict.lm` reads exactly these two places, so following it keeps the
  package in step with what `predict()` itself puts in the linear predictor.
- **Where.** `R/utils-internal.R`, `.offset_exprs()`.
- **Pinned by.** `test-offset-link.R`: "an offset passed as an argument is
  neutralised too", "offsets from the formula and the argument add up".

#### OFF-02 An offset is removed by subtraction on the link scale

- **Decision.** `rate = linkinv(eta - offset)`, with the offset evaluated
  from the model's own expressions. The column is never simply set to 1.
- **Why.** Setting the exposure column to 1 only neutralises
  `offset(log(Exposure))`. With `offset(Exposure)` it leaves 1 instead of
  0, a factor e too high; with `offset(log(Maanden / 12))` it leaves
  `log(1/12)`, a factor 12 out. Subtraction is exact for any link and any
  expression.
- **Where.** `R/utils-internal.R`, `.offset_value()`,
  `.predict_no_offset()`.
- **Evidence.** Rate per unit of exposure on one model and data:
  `offset(log(Exposure))` 0.13063 both ways; `offset(Exposure)` 0.11764
  with the shortcut against 0.04328 correct; `offset(log(Maanden/12))`
  0.01089 against 0.13063.
- **Pinned by.** `test-offset-link.R`: "an offset without a log is
  neutralised correctly", "an offset on an expression is neutralised
  correctly", "the usual log link with a log offset is unchanged".

#### OFF-03 A pre-computed offset vector is refused

- **Decision.** If an offset expression does not evaluate to one value per
  row of the data being scored, the function stops.
- **Why.** `offset = off_vec` stores only the name. Evaluated on other data
  it returns the fitted vector, of the fitted length, and `predict()`
  recycles it without a word. A wrong number delivered confidently is worse
  than an error that says to put the offset in the formula.
- **Where.** `R/utils-internal.R`, `.offset_value()`.
- **Pinned by.** `test-offset-link.R`: "an offset that cannot be evaluated
  per row is refused".

#### OFF-04 exp(offset) is an exposure only for a log offset under a log link

- **Decision.** Code that recovers a volume as `exp(offset)` does so only
  when every offset term is a `log()` and the link is log. Otherwise it
  falls back to the prior weights, with a warning.
- **Why.** With `offset(Exposure)`, `exp(offset)` is `exp(Exposure)`, which
  is not a volume, and a counts-per-exposure computation weighted by it is
  nonsense.
- **Where.** `R/utils-internal.R`, `.offset_is_log()`, `.glm_ae_parts()`;
  `R/ae.R`, `plot_glm_predictor()`; `R/pdp.R`, `make_pdp()`.
- **Pinned by.** `test-offset-link.R`: ".offset_is_log distinguishes a log
  offset from any other", "exp(offset) is not read as an exposure when it
  is not one".

#### OFF-05 Rates for comparison keep every offset except the exposure

- **Decision.** In `premium_impact()`, `model_lift()`, `double_lift()` and,
  since PDP-05, `make_pdp()`, only the offset expressions that mention
  `exposure_col` are subtracted.
  Any other offset, typically a bonus-malus scale, stays in the rate.
  `keep_offsets = FALSE` subtracts everything. Two cases fall back to
  subtracting everything: a model whose offset does not mention
  `exposure_col` at all (it offsets on `Maanden` or `Ex`, which is an
  exposure under another name; see OFF-09), and one expression holding the
  exposure and something else, `offset(log(Exposure * BM))`, which cannot be
  split and raises a warning.
- **Why.** The exposure says how long a policy was on risk, the BM how much
  it pays. Every comparison these functions make is against something that
  includes the BM: the premium a customer pays, the claims that happened.
  Removing it puts every policy at the class without discount.
- **Where.** `R/utils-internal.R`, `.offset_split()`, `.model_rate()`,
  `.kept_offsets()`, `.old_new_rates()`; `R/impact.R`, `R/lift.R`.
- **Evidence.** On a book with an average discount of 20%, a correct model
  with an in-sample A/E of exactly 1.000 read 0.800 on `model_lift()` with
  the BM removed, in every bin. `premium_impact()` against a current premium
  including discount made an unchanged tariff look like a shift: -20.0% for
  no discount, -11.1%, 0.0%, +14.3%, +33.3% for a 40% discount. With the BM
  kept, 0.0% everywhere.
- **Consequence if changed.** An old premium column must be on the same
  footing as the kept offsets. With the default, that is the premium
  including discount. Dividing such a premium by the BM yourself now takes
  the discount out twice (+25% and -25% at the extremes, measured).
- **Pinned by.** `test-offset-link.R`: "a bonus-malus offset stays in the
  rate, the exposure goes", "model_lift reads a BM model against its own
  claims", "premium_impact compares a premium incl. discount like for like",
  "offsets from the formula and the argument add up".
- **History.** Before commit `626bb2a` every offset was removed in these
  three functions. Results change only for models with a second offset.

#### OFF-06 The rating table removes the whole offset

- **Decision.** The rating table's base row holds every offset variable at
  1 and its intercepts are computed with the whole offset subtracted, so
  the table is at BM = 1.
- **Why.** A tariff table describes a base policy, and the BM discount is
  applied per policy on top, the same way the exposure is. A BM factor is
  not estimated, so it has no block in the table.
- **Where.** `R/rating-table.R`, `base_value_raw()` (offset variables
  return 1) and `intercept_val()`.
- **Consequence.** A premium built from the table is
  `intercept_premium x factors x BM`. Verified per policy against
  `predict()` with Exposure = 1 and the policy's BM.
- **Pinned by.** `test-offset-link.R`: "the rating table intercept follows
  the model's own offset", "a second offset does not end up on the exposure
  axis" (premium chain with BM on top).
- **History.** Until the review fixes of October 2026 the PDP removed the
  whole offset too, on the argument that a partial effect is cleaner
  without a policy-specific discount. It was then drawn against an
  observed line at each policy's own BM and sat 35% above it on a correct
  model. It now keeps the BM (PDP-05).

#### OFF-07 A non-log link is tolerated, with a warning

- **Decision.** With a link other than log, offsets are still removed, and
  the rating table is still built, but a warning says the result is not
  multiplicative and should not be multiplied by a second model.
- **Why.** Refusing would block an inspection that is still informative;
  silence would let a sum be read as a product.
- **Where.** `R/utils-internal.R`, `.model_rate()`; `R/rating-table.R`,
  `check_link()`.
- **Pinned by.** `test-offset-link.R`: "a non-log link removes the offset
  and says what that means"; `test-rating-table.R`: "non-log link raises a
  warning".

#### OFF-08 Volumes are divided by the exposure column, not by exp(offset)

- **Decision.** Where a plot shows a frequency or a volume bar, it divides
  by `exposure_col` when that column exists, and only falls back to
  `exp(offset)` without it.
- **Why.** With a second offset, `exp(offset)` is `Exposure x BM`. The A/E
  is the same either way, but the axis would read "claims per BM-adjusted
  policy-year", which nobody reads off a chart.
- **Where.** `R/ae.R`, `plot_glm_predictor()`; `R/pdp.R`, `make_pdp()`
  (the PDP average is weighted by the same column as its observed line).
- **Known gap.** `.glm_ae_parts()`, behind `plot_residual_heatmap()` and
  `detect_interactions()`, still takes `exp(offset)` as the volume. With a
  BM offset, the tooltip exposure and `MaxAE_ExposureShare` are BM-weighted;
  the A/E itself is unaffected. See section 4.
- **Pinned by.** `test-offset-link.R`: "a second offset does not end up on
  the exposure axis", "the exposure column falls back and can be named".

#### OFF-09 An offset on another column is followed, and said out loud

- **Decision.** When the model offsets on a column other than
  `exposure_col` (`offset(log(Ex))`, `offset(log(Maanden / 12))`), that
  offset is the one removed, and a warning says the rates are per unit of
  that variable while the weighting uses `exposure_col`.
- **Why.** Setting `exposure_col` to 1 would leave the real offset in place
  and return a prediction per record instead of a rate, silently.
- **Where.** `R/utils-internal.R`, `.model_rate()`; `R/rating-table.R`,
  the `offset_vars` warning; `R/pdp.R`.
- **Pinned by.** `test-impact.R`: "an offset on another column is
  neutralised, and said out loud"; `test-awkward-names.R`: "the intercept is
  per unit of exposure whatever the offset is called".

### RT: rating table

#### RT-01 A factor is a ratio of two predictions sharing one base row

- **Decision.** `Factor(level) = predict(level, everything else at base) /
  predict(base row)`. The intercept is the base row's prediction with the
  offset removed. `Factor_Premium = Factor_Frequency x Factor_Severity`.
- **Why.** Predicting rather than exponentiating coefficients handles
  splines, inline transforms, interactions and any parametrisation the same
  way, and guarantees the reconstruction identity:
  `intercept x product of factors = predict()` at exposure 1, for a log
  link. With an interaction, the interaction block's joint factor replaces
  the two main-effect factors of that pair.
- **Where.** `R/rating-table.R`, `predict_factor()`, `predict_factor_2d()`,
  `intercept_val()`.
- **Pinned by.** `test-rating-table.R`: "intercept x factors reconstructs
  the model prediction exactly", "intercept is per unit of exposure",
  "every variable has exactly one base row with factor 1".

#### RT-02 The base point

- **Decision.** Categorical: the first level of the model's `xlevels`, or
  with `base_level = "exposure"` the level with the most exposure; the
  accounting year (`year_col`) its latest level (RT-15).
  Continuous: the median, snapped onto the grid. Offset variables: 1. A
  `base` argument overrides any of these per variable; a categorical value
  must be an existing level, a continuous value is used exactly as given
  (with a warning outside the data). A merged column follows its source
  (MV-09).
- **Why.** The base is a reference point, not a recommendation: moving it
  moves the factors and the intercept against each other and leaves every
  premium unchanged. The defaults give a row with factor 1 that is a value
  someone would quote.
- **Where.** `R/rating-table.R`, `base_value_raw()`, `grid_of()`, the
  `base` validation block.
- **Pinned by.** `test-rating-table.R`: "base_level = 'exposure' picks the
  largest level as reference", "the base can be chosen per variable,
  default unchanged", "a wrong base is refused or reported".

#### RT-03 A variable missing from one model gets a factor of 1; a missing model gets NA

- **Decision.** A variable in the frequency model but not in the severity
  model gets `Factor_Severity = 1`, and vice versa. A model that is not
  supplied at all gives `NA` in its factor column, so with one model only,
  `Factor_Premium` and `intercept_premium` are `NA` and the single model's
  own columns are the ones to use.
- **Why.** A neutral 1 keeps `Factor_Premium` correct when both models are
  there. When one is absent there is no premium to report, and an `NA`
  says so rather than pretending the missing side is 1.
- **Where.** `R/rating-table.R`, `predict_factor()`; the
  `intercept_premium` attribute at the end of `make_rating_table()`.

#### RT-04 Readable grids for continuous variables

- **Decision.** The step is about `grid_res` points across the range,
  rounded to 1, 2 or 5 times a power of ten, never fractional for a column
  of whole numbers. A whole-number column with at most `2 x grid_res`
  distinct values is listed on its own values. The base is inserted into
  the grid if the clamping left it off. `grid_step` overrides globally or
  per variable; a name that matches no continuous variable is a warning.
- **Why.** A tariff lists ages 18, 19, 20 and weights 800, 850, 900, not
  19.2653. Listing "values" rather than "a step of 1" keeps a coded column
  (five sums insured spread over a million) to five rows instead of a
  million.
- **Where.** `R/utils-internal.R`, `.nice_step()`, `.list_values()`,
  `.step_grid()`, `.snap_to_step()`; `R/rating-table.R`, `grid_of()`.
- **Pinned by.** `test-rating-table.R`: "continuous grids run in readable
  steps", "grid_step can be set globally or per variable", "a wide integer
  column with few values is listed, not stepped".

#### RT-05 Volumes per grid point, and trimming

- **Decision.** Categorical: exposure and claims per level. Continuous:
  each policy goes to its nearest grid point, splitting on the midpoints.
  With `trim` other than `c(0, 1)`, policies outside the trimmed range are
  left out of the volumes rather than swept into the edge points.
- **Why.** Sweeping them in would make the outer bars overstate their
  volume for values the grid does not cover. The trimmed exposure is
  therefore below the portfolio total by design.
- **Where.** `R/rating-table.R`, `cell_index()`.
- **Pinned by.** `test-awkward-names.R`: "a trimmed grid excludes the
  policies it does not cover".

#### RT-06 Two-way interactions

- **Decision.** Only two-way terms get a block. The continuous variable is
  the x-axis when exactly one is continuous; with two categoricals the one
  with more levels is. A continuous group variable is shown in three
  slices: its 10% and 90% quantiles (rounded to three decimals) and its
  base. The factor is the joint relativity against the base cell; the
  uplift is `joint / (main_x x main_group)`. Without any interaction term
  the three `Uplift_*` columns are dropped.
- **Why.** Higher-order blocks cannot be read in a table. Uplift columns
  that are `NA` in every row would only need explaining. `Group` and
  `GroupVar` always stay: `is.na(Group)` is how a main effect is told from
  an interaction.
- **Where.** `R/rating-table.R`, `gather_pairs()`, the interaction block.
- **Pinned by.** `test-rating-table.R`: "interaction rows have a unit base
  cell and unit uplift at reference", "interaction cells carry exposure and
  claims for any variable type", "the uplift columns are absent without an
  interaction".

#### RT-07 Thin cells

- **Decision.** `IsThin = ClaimCount < min_claims` (default 30). Only thin
  categorical main-effect levels raise a warning, which names the levels
  without any claim separately. The flag stays a volume flag; the
  precision of a factor is in its standard error (RT-12).
- **Why.** A GLM applies no shrinkage to a categorical level, so its factor
  rests on those few claims. Thin tail points of a continuous grid are
  normal and would make the warning noise. A level without claims gets a
  frequency factor that is the fit running towards zero, not an
  estimate.
- **Where.** `R/rating-table.R`, the thin-cell block.
- **Pinned by.** `test-rating-table.R`: "the thin-cell column is correct".

#### RT-08 Terms are resolved to the column behind them

- **Decision.** `ns(LEEFTIJD, 4)`, `factor(BOEKJAAR)`, `I(KM^2)`,
  `log(KM)` and backticked names resolve to their data column. Inline
  `factor()` makes a numeric column categorical through `xlevels`.
- **Why.** A tariff is per column, not per term.
- **Where.** `R/utils-internal.R`, `.term_base_var()`;
  `R/rating-table.R`, `xlev_map()`, `var_is_cat()`.
- **Pinned by.** `test-rating-table.R`: "rating table builds, incl. inline
  factor(BOEKJAAR)"; `test-awkward-names.R`: "I() terms resolve to their
  underlying column", "a column name with a space survives the whole rating
  table".

#### RT-09 Small robustness choices

- **Decision.** Levels are written with `format(scientific = FALSE)`; base
  rows are built with `check.names = FALSE`.
- **Why.** `as.character(1e5)` is `"1e+05"`, unusable as a tariff level and
  as a lookup key. Without `check.names = FALSE` a column called
  `AUTO GEWICHT` becomes `AUTO.GEWICHT` and every prediction fails.
- **Where.** `R/utils-internal.R`, `.level_chr()`; `R/rating-table.R`,
  `base_df_for()`.
- **Pinned by.** `test-rating-table.R`: "round large levels are not written
  in scientific notation"; `test-awkward-names.R`.

#### RT-10 A level a model was not fitted on is an empty cell

- **Decision.** A level that is in `data` but not in a model's `xlevels`
  keeps its row: that model's factor is `NA` there, and so is
  `Factor_Premium`, with one warning per model naming the cells. A base
  row the model does not know is an error, raised once before any factor
  is computed, since every factor is a ratio against it.
- **Why.** A level with exposure and no claims is the usual case: the
  frequency model knows it, the severity data has no row for it. Predicting
  the whole variable failed on that one level, and the variable then left
  the table with a warning, so the frequency factors of every other level
  went with it.
- **Where.** `R/rating-table.R`, `predict_rows()`, the base-row check, the
  NA-cell warning; `R/utils-internal.R`, `.predictable_rows()`.
- **Evidence.** The review's WA book: Waterstof with 121 exposure and no
  claims dropped `BRANDSTOF` from the table entirely.
- **Pinned by.** `test-review-findings.R`: "a level without claims does not
  drop the variable from the table".

#### RT-11 A term that bands a column is read on its bands

- **Decision.** When a model's factor levels are not values of the column
  behind them, as with `cut(LEEFTIJD, ...)` or an `ifelse()`, the variable
  is reported on those levels. Each level is put into the prediction frame
  as the most frequent value of the column within it, which falls in it by
  definition, and the volumes are counted on the evaluated term.
  `factor(BOEKJAAR)` needs none of this, since `"2021"` is a year.
- **Why.** Predicting on the label put `"(24,40]"` into `LEEFTIJD`, which
  gave `NA` in every row, the intercept included, because every prediction
  shares the base row.
- **Where.** `R/rating-table.R`, the decoder block, `to_model()`,
  `row_level()`.
- **Evidence.** Intercept and every factor `NA` before; afterwards the
  reconstruction of `predict()` holds to 2e-16.
- **Pinned by.** `test-review-findings.R`: "cut() in the formula does not
  give a table full of NA".

#### RT-12 Standard errors of the factors

- **Decision.** `SE_Frequency` and `SE_Severity` hold the standard error of
  `log(Factor)` in each row, `sqrt(d' V d)` with `d` the difference of the
  row's and the base's model-matrix rows and `V` the model's covariance
  matrix. 0 at the base and for a variable the model does not use, `NA`
  where there is no factor or the link is not log. On an interaction row
  it belongs to the joint relativity against the base cell.
- **Why.** The model matrix is built exactly as `predict()` builds it, so
  this holds for splines, inline factors and interactions alike. A volume
  flag cannot say how precise a factor is: a level with 31 claims passes
  `min_claims = 30` with a standard error of 0.18, a 95% interval of x0.70
  to x1.42.
- **Where.** `R/rating-table.R`, `se_of()`.
- **Evidence.** For a categorical level against the reference the column
  equals the coefficient's own standard error (0.07675 for one region);
  Waterstof on the review's book: 31 claims, estimate 1.167 against a true
  1.000, `IsThin = FALSE`.
- **Limitation.** `vcov()` of a Poisson model assumes a dispersion of 1;
  fit a quasipoisson for standard errors that carry the overdispersion.
- **Pinned by.** `test-review-findings.R`: "the rating table carries
  standard errors of its factors".

#### RT-13 Claims without payment are pointed out

- **Decision.** When the frequency response counts `claims_col` and at
  least 1% of those claims sit in records with `loss_col = 0`, a warning
  says that frequency x severity overstates the risk premium by about that
  share. Raised by `make_rating_table()` and `model_lift()`.
- **Why.** A Gamma severity cannot take a zero, so the zero amounts leave
  the severity data while the frequency model still counts them. Only
  records whose every claim is nil are visible, so the share is a lower
  bound.
- **Where.** `R/utils-internal.R`, `.nil_claims_warning()`.
- **Evidence.** With 15% nil claims: predicted over actual losses 1.153.
- **Pinned by.** `test-review-findings.R`: "claims without payment are
  pointed out".

#### RT-14 A logical predictor stays logical

- **Decision.** Grid values for a logical column are coerced back to
  `TRUE` / `FALSE`.
- **Why.** `predict()` refuses a character where the model was fitted on a
  logical, so a column such as `GARAGE = TRUE` broke the table.
- **Where.** `R/utils-internal.R`, `.coerce_like()`.
- **Pinned by.** `test-review-findings.R`: "a logical predictor does not
  break the table".

#### RT-15 The accounting year sits at its latest level

- **Decision.** When a model uses `year_col` (default `"BOEKJAAR"`), its
  default base is the latest year, for a `factor(BOEKJAAR)` as for a
  numeric trend term. `base` still overrides it; `year_col = NULL` returns
  to the general rule.
- **Why.** The intercept is then the current price level instead of the
  oldest one, and the year factors say how much cheaper the past was. The
  other factors do not change.
- **Where.** `R/rating-table.R`, `base_value_raw()`, `grid_of()`,
  `latest_of()`.
- **Evidence.** On the review's WA book the premium intercept was 207.2 at
  the first year and 508.9 at the latest (with the region base moving as
  well).
- **Pinned by.** `test-review-findings.R`: "the accounting year sits at its
  latest level by default"; `test-rating-table.R`: "intercept is per unit
  of exposure".

#### RT-16 Trimming leaves the interaction slices alone

- **Decision.** `trim` keeps policies outside the trimmed range out of the
  volumes of a grid that spans that range (RT-05), but not out of the three
  slices of a continuous group variable in an interaction.
- **Why.** The slices stand for the whole distribution. Cutting at the
  outer slices threw away everything below the 10% and above the 90%
  quantile.
- **Where.** `R/rating-table.R`, `cell_index(drop_outside = FALSE)` in the
  interaction block.
- **Evidence.** The review's book with `trim = c(0.005, 0.995)`: 24,815
  exposure in the main effect against 19,992 in the interaction cells.
- **Pinned by.** `test-review-findings.R`: "trim does not take exposure out
  of the slices of a group variable".

### MV: merged and grouped columns

#### MV-01 One tariff field under two column names becomes one block

- **Decision.** `merge_vars = list(Stedelijkheid = "Stedelijkheid_groep")`
  reports the grouped column inside the block of its source. Every
  prediction frame moves the source and all its groupings together.
- **Why.** Without it the table is correct but holds two blocks, each
  padded with neutral factors, and anyone building the tariff from one
  block silently drops the other.
- **Where.** `R/rating-table.R`, `set_var()`, `canon()`, `spellings()`,
  `alias_values()`.
- **Evidence.** One policy in class 5: 506.20 with both blocks or with the
  merged block, 401.76 with the source block alone (20.6% too low).
- **Pinned by.** `test-rating-table.R`: "two columns for one rating factor
  can be merged into one block".

#### MV-02 A grouping must be a function of its source

- **Decision.** Every source value maps to exactly one group, checked in
  `data` and in each model's own data.
- **Why.** If a value occurs with two groups there is no single factor for
  it, and taking the first would be wrong for part of the book.
- **Where.** `R/rating-table.R`, the `merge_vars` loop;
  `R/utils-internal.R`, `.grouping_column()`.
- **Pinned by.** `test-rating-table.R`: "a merge declaration that cannot
  hold is refused".

#### MV-03 A grouped column missing from the data is derived from the model

- **Decision.** When `data` lacks the grouped column, it is derived from
  the data of the model that uses it, which a `glm` fitted with `data =`
  carries. The caller's data frame is not changed.
- **Why.** The grouping is made where it is needed, next to the average
  claim, so it lives in the severity data and nowhere else. Asking the user
  to rebuild it for every dataset invites exactly the inconsistencies MV-04
  guards against.
- **Where.** `R/utils-internal.R`, `.grouping_column()`.
- **Pinned by.** `test-rating-table.R`: "a regrouped column missing from
  'data' is taken from the model"; `test-groupings.R`: "the grouped columns
  are derived from the source columns".
- **History.** Added in commit `2779b8b`; shared with `add_groupings()`
  since `094d345`.

#### MV-04 One column name may not hold two groupings

- **Decision.** When both models use a column of the same name, the
  mapping each one's data defines is compared, and a difference is an error
  naming the value it turns on.
- **Why.** A grouping made twice need not agree, and a scored dataset can
  hold only one of the two. With different labels the result is a
  prediction failure; with the same labels nothing fails and one model is
  read on the other's grouping.
- **Where.** `R/utils-internal.R`, `.grouping_column()`.
- **Evidence.** On a bench of ten scenarios, before this check: one brand's
  severity factor 36% out (same labels G1/G2, different membership) and the
  ages between two cut points 16% out (same labels LAAG/HOOG, cuts at 40
  and 50). Both silent. Six sound scenarios, including a spline against
  bands and an interaction, reconstruct the models to machine precision
  before and after.
- **Pinned by.** `test-rating-table.R`: "one column name with two groupings
  is refused, not averaged over", "two bandings of one continuous variable
  are kept apart"; `test-groupings.R`: "one name grouped two ways by the
  two models is refused".
- **History.** Added in commit `9245289`. Since `094d345` the message names
  both models rather than `'data'` and one model.

#### MV-05 A grouped column already present is checked, never overwritten

- **Decision.** If the data has the grouped column, every row is compared
  with what the model's data says, and a disagreement is an error with the
  row and the values.
- **Why.** A wrong mapping in a source system would otherwise be scored.
- **Where.** `R/utils-internal.R`, `.grouping_column()`.
- **Pinned by.** `test-groupings.R`: "anything that would be a guess is
  refused".

#### MV-06 Categorical or continuous source

- **Decision.** Read off the models first: a factor term on the source
  means categorical, a numeric term means continuous. A source no model
  uses directly falls back on its own column: numeric with more than 25
  distinct values is continuous, anything else categorical.
- **Why.** A numeric code (urbanisation 1 to 5) must be looked up exactly,
  so an unknown code is an error and not a neighbour's group. The
  threshold of 25 is the same `discrete_cutoff` the one-way plots use.
- **Where.** `R/utils-internal.R`, `.source_kind()`.
- **Pinned by.** `test-groupings.R`: "anything that would be a guess is
  refused" (numeric code 6).
- **Limitation.** A genuine measurement with at most 25 distinct values,
  used only through a band, is treated as a code. See section 4.

#### MV-07 What happens to a value the model's data never saw

- **Decision.** Categorical source: error, with the values. Continuous
  source inside the fitted range: the band of the nearest observed value.
  Continuous source outside the range: the outer band, with a warning; if
  the same source is also a continuous term of a model, the warning names
  that model's extrapolation and the capping remedy.
- **Why.** The outer bands of a banding are open in practice; nobody over
  80 in the claims data does not make 85 a new class. The real cost of
  going outside the range is in a spline, which `predict()` extrapolates
  linearly on the log scale without a word, and no grouping can prevent
  that.
- **Where.** `R/utils-internal.R`, `.grouping_column()`;
  `R/rating-table.R`, `alias_values()` for grid points.
- **Pinned by.** `test-groupings.R`: "a continuous source outside the
  fitted range takes the outer band"; `test-rating-table.R`: "a band of a
  continuous variable can be merged into it", "a band missing from 'data'
  is derived per nearest observed value".

#### MV-08 An undeclared regrouping across the two models is pointed out

- **Decision.** When one column is a function of another and each is used
  by one model only, a warning suggests the `merge_vars` line. Skipped when
  both columns are numeric, when either has fewer than two or more than
  1000 distinct values, or when both have the same number of values.
- **Why.** The two-block table is right but misleading. Only across models:
  two nested factors inside one model (a postcode and its region) are
  genuinely two rating factors.
- **Where.** `R/rating-table.R`, the regrouping-hint block.
- **Pinned by.** `test-rating-table.R`: "two columns for one rating factor
  can be merged into one block" (the warning when undeclared).

#### MV-09 What a merged column may not do

- **Decision.** A merged column cannot be named in `base` (set it on the
  source; both move together and the base row stays a real policy). An
  interaction on it is reported under the source's name; an interaction
  between a column and its own grouping disappears. Chains (a grouping of
  a grouping) are refused. A declaration that neither model uses is a
  warning.
- **Where.** `R/rating-table.R`, `base_value_raw()`, the `base` and
  `merge_vars` validation, `gather_pairs()`.
- **Pinned by.** `test-rating-table.R`: "an interaction on a merged column
  is reported under one name", "a merge declaration that cannot hold is
  refused".

#### MV-10 add_groupings() is the same derivation for data to be scored

- **Decision.** `add_groupings(data, model_freq, model_sev, merge_vars)`
  runs MV-02 to MV-07 on any dataset and returns it with the grouped
  columns added, typed and levelled as in the model's data. A data.table
  comes back as a data.table; the input is copied, not modified.
- **Why.** `predict()` needs every grouped column; one declaration then
  serves the rating table and every scored dataset, and the two cannot
  disagree about which policy sits in which group.
- **Where.** `R/groupings.R`, `add_groupings()`.
- **Pinned by.** `test-groupings.R`, all tests; the first one checks that
  `predict()` after `add_groupings()` equals the rating table rebuilt with
  the same declaration, for every policy.

### SCORE: data a model was not fitted on

#### SCORE-01 A value the model never saw is named before predict()

- **Decision.** Before scoring, every factor term of the model is evaluated
  on the data and compared with its `xlevels` (`.unseen_levels()`). A value
  the model does not know stops the function with the column, the values
  and their row count, and the advice to map them or leave the rows out;
  where the function has `at`, also to price a later year at the latest
  known one. Used by `.model_rate()` (so `premium_impact()`, `model_lift()`,
  `double_lift()`) and by `plot_glm_predictor(data = )`. The rating table
  turns the same check into empty cells (RT-10).
- **Why.** `predict()` raised "factor has new levels" from inside the
  offset evaluation, and the message then blamed the offset.
- **Where.** `R/utils-internal.R`, `.unseen_levels()`, `.unseen_message()`,
  `.model_rate()`, `.glm_newdata_parts()`.
- **Evidence.** The review: scoring 2025 gave "the model has an offset that
  could not be evaluated on this data ... factor factor(BOEKJAAR) has new
  level 2025".
- **Pinned by.** `test-review-findings.R`: "an unknown level at scoring is
  not reported as an offset problem", "at prices every policy at one
  accounting year"; `test-agg-plots.R`: "unusable monitoring data is
  refused rather than guessed at".

#### SCORE-02 Scoring outside the fitted range is said once

- **Decision.** A numeric predictor with values outside the range of the
  model's training data raises one warning per call, with the row count
  and the range. A column the model turns into a factor is left to
  SCORE-01.
- **Why.** `predict()` extrapolates without a word: a spline linearly on
  the link scale beyond its last knot, a log term without limit.
- **Where.** `R/utils-internal.R`, `.range_warning()`.
- **Pinned by.** `test-review-findings.R`: "scoring outside the fitted
  range is pointed out".

### RATE: premium impact and lift

#### RATE-01 A rate is the product of the models, per unit of exposure

- **Decision.** Rate = frequency rate x severity rate, offsets handled per
  OFF-05; a model that is not supplied contributes 1.
- **Where.** `R/utils-internal.R`, `.model_rate()`, `.old_new_rates()`.

#### RATE-02 An old premium column

- **Decision.** `old_premium_basis = "amount"` divides by `exposure_col`
  (an earned premium for the record); `"rate"` takes it as a premium per
  unit of exposure. It must be on the footing of the kept offsets (OFF-05).
  `double_lift()` divides by its weight column instead, which is the
  exposure unless both sides are severity models (RATE-12), so that rate x
  weight gives the amount back on both sides. An old premium column is
  read as it is: `at` (RATE-10) only changes what the models score.
- **Where.** `R/utils-internal.R`, `.old_new_rates()`, `divisor_col`.
- **Pinned by.** `test-impact.R`: "rebase separates rate-level change from
  dislocation".

#### RATE-03 Rebase and rate-level change

- **Decision.** With `rebase = TRUE` the new rates are scaled so the
  exposure-weighted totals match. The rate-level change is always measured
  before scaling.
- **Why.** That separates the two questions a dislocation analysis answers:
  the overall level and the redistribution. Deriving the level change from
  the scale factor would report 0 exactly when `rebase = FALSE`.
- **Where.** `R/impact.R`; `R/lift.R`, `double_lift()`.
- **Pinned by.** `test-impact.R`; `test-lift.R`: "the rate level change is
  reported whether or not we rebase".

#### RATE-04 Contribution, not mix

- **Decision.** Per level of a `by` column:
  `Contribution = 100 x (NewPremium - OldPremium) / old total premium`,
  which is `PremiumShare x TotalChangePct`. The contributions of one
  variable sum to the change in the total premium: exactly 0 after a
  rebase, the rate-level change without one. `MeanChangePct` (what a
  policy in the level sees on average) is reported next to it, and so are
  `OldPremium`, `NewPremium`, `PremiumShare` and `TotalChangePct`. The
  spotlight's contribution is computed the same way.
- **Why.** Old and new are compared on the same policies, so there is no
  mix effect to isolate; contribution answers which segment drives the
  change. A segment's part of the total is its premium change, not its
  exposure times its mean percentage, because a percentage on a cheap
  policy moves less money than the same percentage on an expensive one.
- **Where.** `R/impact.R`, `by_level` and the spotlight block.
- **Evidence.** On the review's WA book after a rebase (total premium
  change +0.000%), the old definition summed to -4.008% for every `by`
  variable, the exposure-weighted mean change. Per region the two
  definitions disagreed by up to 2.1 points (Randstad +2.715 against
  +4.777).
- **Pinned by.** `test-collinearity-spotlight.R`: "contributions sum to the
  portfolio change"; `test-review-findings.R`: "premium_impact
  contributions add up to the change in total premium".
- **History.** Until the review fixes of October 2026,
  `Contribution = ExposureShare x MeanChangePct`. That sums to the
  exposure-weighted mean change, which equals the change in total premium
  only when the old premium is flat per unit of exposure.

#### RATE-05 Spotlight, dropped rows, histogram

- **Decision.** A spotlight is evaluated against `data` (or taken as a
  logical vector) and reported after the book-level rebase. Rows without a
  usable new or old rate, a positive weight or an actual value are dropped
  with a warning that counts them per reason (`.drop_reasons()`), in
  `premium_impact()`, `model_lift()` and `double_lift()`. A row can count
  under more than one reason. The histogram is pre-binned into 60 bars.
- **Why.** The spotlight shows how a segment moves relative to the book.
  One count for every reason reported a missing loss amount as an exposure
  problem. Pre-binning keeps the widget small: a plotly histogram would
  embed every policy.
- **Where.** `R/impact.R`; `R/lift.R`; `R/utils-internal.R`,
  `.drop_reasons()`.
- **Pinned by.** `test-collinearity-spotlight.R`: "the spotlight reports a
  subset without filtering the book", "spotlight input is validated";
  `test-review-findings.R`: "dropped rows are reported per reason".

#### RATE-06 Lift bins hold equal exposure

- **Decision.** Policies are ordered by predicted rate and cut at equal
  cumulative weight. Fewer distinct predictions than bins raises a warning;
  ties are counted at 12 significant digits.
- **Why.** Equal policy counts would give bins unequal weight. Ties are
  counted at a tolerance because removing an offset leaves rounding noise
  of order 1e-16, which would otherwise make equal predictions look
  distinct.
- **Where.** `R/utils-internal.R`, `.exposure_bins()`.
- **Pinned by.** `test-lift.R`: "bins hold roughly equal exposure".

#### RATE-07 The lift weight follows the model

- **Decision.** `weight_col = NULL` resolves to `claims_col` for a
  severity-only lift and `exposure_col` otherwise.
- **Why.** A severity predicts a mean per claim; weighting it by exposure
  multiplies a per-claim amount by a per-year one and the totals do not
  reconcile.
- **Where.** `R/lift.R`, `model_lift()`.
- **Pinned by.** `test-lift.R`: "a severity-only lift weights by claims,
  not exposure".

#### RATE-08 Gini and the units check

- **Decision.** The Gini is computed on the exposure-weighted Lorenz curve
  of actual losses ordered by prediction, and is `NA` without losses. An
  overall A/E outside 0.5 to 2 raises a warning about mismatched units.
- **Why.** The Gini measures ordering only, not calibration. A wildly off
  A/E nearly always means counts were compared with amounts.
- **Where.** `R/utils-internal.R`, `.gini_exposure()`, `.gini_ordered()`,
  `.check_ae_scale()`.
- **Pinned by.** `test-lift.R`: "a portfolio without losses gives NA rather
  than NaN for the Gini", "mismatched units are caught".

#### RATE-09 Double lift

- **Decision.** Bins are ordered by the new/old rate ratio. Each model is
  scored by the mean absolute distance of its A/E from 1 across the bins
  (`mad_new`, `mad_old`). `winner` is `"tie"` when the two are equal;
  otherwise it names a model only when the bootstrap interval of
  `mad_old - mad_new` (RATE-11) excludes 0, and is `"inconclusive"` when
  it does not. With `n_boot = 0` the point values decide, as they did
  before the interval existed.
- **Why.** The point comparison is decided by a few large claims: on the
  review's 16,000-policy holdout it said "old" on the losses as recorded
  (0.543 against 0.405) and "new" on the same losses capped at 50,000
  (0.222 against 0.271). A verdict that flips with the treatment of ten
  claims is not a verdict.
- **Where.** `R/lift.R`, `double_lift()`.
- **Pinned by.** `test-lift.R`: "double lift prefers the model that is
  right where they differ", "double lift against itself shows no winner and
  a flat A/E"; `test-review-findings.R`: "the double lift names a winner
  only when the interval excludes 0".
- **History.** Until the review fixes of October 2026 the winner was the
  smaller point value, always.

#### RATE-10 Pricing at one accounting year

- **Decision.** `premium_impact()`, `model_lift()` and `double_lift()` take
  `at`, a named list of values every policy is priced at, for example
  `at = list(BOEKJAAR = 2024)`. It is applied to the data the models score
  and never to an old premium column. Each value must be valid for its
  column (`.coerce_like()`); a level the model does not know is refused by
  SCORE-01. `premium_impact()` warns when the new models use `year_col`
  against an old premium column, or when only one of two model sets uses
  it, and `at` does not set it. The lifts do not warn.
- **Why.** With the accounting year as a rating factor, every policy is
  priced at the level of its own historic year, and against a current
  premium that mixes inflation into the dislocation. Two model sets that
  both use the year price each policy at the same year on both sides,
  which is like for like. A lift compares with the claims of each
  policy's own year, which is exactly what pricing at its own year does.
- **Where.** `R/utils-internal.R`, `.apply_at()`, `.year_warning()`,
  `.old_new_rates()`; `R/impact.R`; `R/lift.R`.
- **Evidence.** On the review's WA book, with `BOEKJAAR` in the frequency
  model: a rate-level change of +11.1% with every policy at its own year,
  +34.7% with every policy at 2024.
- **Pinned by.** `test-review-findings.R`: "at prices every policy at one
  accounting year"; `test-impact.R`: "rebase separates rate-level change
  from dislocation" (the warning).

#### RATE-11 Bootstrap intervals for the Gini and the double-lift verdict

- **Decision.** `model_lift()` reports `gini_ci`, a bootstrap interval for
  the Gini (`n_boot = 200`, `conf_level = 0.90`), and `double_lift()` one
  for `mad_old - mad_new`. Both resample policies with Poisson(1) counts,
  the large-sample stand-in for resampling with replacement, so the book is
  sorted once (`.gini_boot()`) or the bins are kept as cut on the full data
  and every replicate reweights the policies within them (rebased again
  when `rebase = TRUE`). The interval is in the title and the return value.
  A warning is raised only when large losses are the reason: the ten
  largest records carry 10% or more of `actual_col` (`.large_loss_note()`)
  and, for the Gini, the interval is wider than half the Gini and than
  0.05; for the double lift, the verdict is inconclusive.
- **Why.** A Gini on raw losses moves with a handful of large claims, and
  so does the double-lift verdict (RATE-09). Capping the losses is the
  remedy for that, and the warning says so. A wide interval on a small book
  has no such remedy; warning about it fired on an ordinary frequency lift
  of 20,000 policies and was noise.
- **Where.** `R/utils-internal.R`, `.gini_boot()`, `.gini_ordered()`,
  `.top_share()`, `.large_loss_note()`, `.check_boot()`; `R/lift.R`.
- **Evidence.** The review's 16,000-policy holdout: Gini 0.230, 90%
  interval 0.143 to 0.326 (0.133 to 0.302 for the true risk premium), so
  the model and the truth could not be told apart on it.
- **Limitation.** The interval resamples policies only. The models are
  held fixed, so an in-sample interval ignores that they were fitted on
  the same rows.
- **Pinned by.** `test-review-findings.R`: "model_lift reports a bootstrap
  interval around the Gini", "large losses that decide the Gini or the
  verdict are named", "the double lift names a winner only when the
  interval excludes 0".

#### RATE-12 The double-lift weight follows the models

- **Decision.** `double_lift()` resolves `weight_col = NULL` the way
  `model_lift()` does (RATE-07): `claims_col` when both sides are severity
  models, otherwise `exposure_col`. The bins hold equal weight, the
  `Exposure` column of the table holds the weight, and an old premium
  amount is divided by it (RATE-02). A severity model on one side and a
  frequency or risk premium on the other is refused.
- **Why.** A severity predicts a mean per claim. Weighted by exposure, the
  predicted and actual totals do not reconcile, and the A/E per bin says
  more about claim frequency than about severity. The ratio of a rate per
  claim to a rate per exposure unit orders the book by neither tariff.
- **Where.** `R/lift.R`, `double_lift()`.
- **Evidence.** Two severity models on the review's claim data, weighted
  by exposure: the old model's A/E per bin ran from 0.76 to 1.50.
- **Pinned by.** `test-review-findings.R`: "a double lift between two
  severity models is weighted by claims", "a severity model against a
  frequency model is refused".

### AE: actual versus expected

#### AE-01 Counts mode and weighted-mean mode

- **Decision.** For a log-link model with a log offset, response and
  prediction are counts, divided by the exposure per bin. Otherwise both are
  prior-weight-weighted means.
- **Where.** `R/ae.R`, `plot_glm_predictor()`.

#### AE-02 Binning

- **Decision.** A numeric predictor with at most `n_bins` distinct values
  is not binned; above that, quantile bins (or equal width), each point at
  the weight-weighted mean of its bin.
- **Why.** Binning a few values merges neighbours, and a single outlier
  would move every equal-width bin.
- **Where.** `R/ae.R`.
- **Pinned by.** `test-agg-plots.R`: "fewer distinct values than n_bins
  means no binning at all".

#### AE-03 Error bars on the observed point only

- **Decision.** The interval is on the observed point, from the family's
  variance function times the Pearson dispersion; Poisson and binomial hold
  the dispersion at 1.
- **Why.** The predicted line is the model; its parameter uncertainty is a
  different and smaller quantity. Holding the dispersion at 1 for Poisson
  is the assumption being examined.
- **Evidence.** Measured coverage on a correctly specified Poisson: 0.96
  for a binned continuous predictor, 0.98 for a categorical one outside the
  model.
- **Where.** `R/ae.R`.
- **Pinned by.** `test-agg-plots.R`: "observed points carry error bars that
  match the theory", "the ci switches behave".

#### AE-04 A categorical term in the model is pinned in-sample

- **Decision.** Documented, not corrected: in-sample, under a canonical
  link, observed equals predicted at every level of a categorical term in
  the model. The bar length still says how much evidence the level carries.
- **Where.** `R/ae.R` (documentation); see INT-01 for the derivation.

#### AE-05 Monitoring on new data

- **Decision.** With `data`, the comparison runs on those rows. The
  response, weights and offsets are re-evaluated on them; rows with a
  missing value in a model variable are dropped as `glm()` would; a level
  the model never saw is an error that names it (SCORE-01); the dispersion
  behind the bars is the fitted model's.
- **Why.** Recycling the fitted weights would pair one period's claim
  counts with another's rows.
- **Where.** `R/utils-internal.R`, `.glm_newdata_parts()`; `R/ae.R`.
- **Pinned by.** `test-agg-plots.R`: "plot_glm_predictor evaluates another
  period when 'data' is given", "plot_glm_predictor on new data re-evaluates
  weights and offsets", "unusable monitoring data is refused rather than
  guessed at".

#### AE-06 The A/E ratio itself

- **Decision.** `ratio = TRUE` draws one A/E line with a reference at 1;
  the bars are the observed bars divided by the prediction.
- **Where.** `R/ae.R`.
- **Pinned by.** `test-agg-plots.R`: "ratio = TRUE plots A/E against a
  reference line at 1".

#### AE-07 A multi-column term is not a predictor

- **Decision.** Asking for `"ns(LEEFTIJD, 4)"` is an error that names the
  column to ask for instead.
- **Where.** `R/ae.R`.
- **Pinned by.** `test-agg-plots.R`: "a multi-column model term is refused
  with a usable message".

### INT: interactions

#### INT-01 Why a one-way check cannot see an interaction

- **Decision.** The interaction tools look at cells, never at margins.
- **Why.** Under a canonical link the score equations force observed to
  equal predicted at every level of a categorical term in the model, so a
  Poisson frequency model is blind by identity. A Gamma log-link severity
  is not canonical, but its score pins the weighted mean of `y / mu` to 1,
  which is nearly the same: an omitted interaction of 35% moves the one-way
  A/E by 0.26%.
- **Where.** `R/interactions.R`, header comment.
- **Pinned by.** `test-interactions.R`: "one-way A/E is blind to the
  missing interaction", "each link pins a different quantity to exactly
  1", "the severity residue is far too small to price on".

#### INT-02 The heatmap

- **Decision.** A/E per cell is `sum(actual) / sum(expected)`. Cells with
  fewer than `min_claims` claims are left uncoloured and marked. The scale
  diverges around 1 with a neutral midpoint. When every cell is thin, the
  colour layer is left out, with a warning, and the crosses with their
  tooltips are the plot.
- **Why.** A thin cell's A/E is noise, and colouring it would make it the
  loudest thing on the plot. plotly cannot build a heatmap whose values are
  all `NA`, whatever its colour range: it failed with "Wasn't able to
  determine range of domain", and on a 400-policy book that took the whole
  report down.
- **Where.** `R/interactions.R`, `plot_residual_heatmap()`,
  `.ta_diverging`.
- **Pinned by.** `test-interactions.R`: "plot_residual_heatmap returns a
  plot plus its cell table", "thin cells are not coloured and z_range is
  validated"; `test-review-findings.R`: "a heatmap with every cell thin
  still builds".

#### INT-03 The interaction statistic

- **Decision.** Per pair, expected is raked to actual's row and column
  margins (iterative proportional fitting, 50 iterations, tolerance 1e-9),
  and the likelihood-ratio deviance against the saturated table is the
  statistic. For Poisson counts the null comes from `n_sim` simulations
  under the raked expected, scaled for overdispersion (INT-05); otherwise
  from a chi-square scaled by the Pearson dispersion.
- **Why.** Raking removes main-effect misfit first, so what remains is
  interaction. The deviance is unreliable in sparse tables, hence the
  simulation where the response allows it.
- **Where.** `R/interactions.R`, `detect_interactions()`, `rake()`,
  `dev_of()`; `.glm_ae_parts()` decides `poisson_counts`.
- **Pinned by.** `test-interactions.R`: "detect_interactions ranks the true
  pair first", "no interaction present means no signal";
  `test-awkward-names.R`: "a Poisson model without an offset still gets the
  simulated null".

#### INT-04 Signal and materiality are reported separately

- **Decision.** Pairs are ranked by `Z`; `MaxAE` (worst non-thin cell),
  its claims and its exposure share are reported alongside.
- **Why.** Significant and material are different questions.
- **Where.** `R/interactions.R`.

#### INT-05 The simulated null carries the overdispersion

- **Decision.** For count models the deviance is divided by the dispersion
  of a cell total, `phi = sum((y - mu)^2) / sum(mu) x n / df`, and the
  simulation draws Poisson counts with means `E / phi`. A dispersion below 1
  is left at 1. The value is in the `Dispersion` column, and a message
  names it from 1.2 upwards.
- **Why.** A Poisson null has no room for overdispersion, so the extra
  variance read as interaction. A cell total of variance `phi x mean`
  behaves like a Poisson count after dividing by `phi`, and the deviance is
  homogeneous of degree one, so `D / phi` against a null on `E / phi` is
  the same test on rescaled counts. The `phi` is the mean-weighted
  Pearson, not the row-level one: a total adds rows by their mean, and
  under a gamma frailty the rows with a larger mean carry more of the extra
  variance.
- **Where.** `R/interactions.R`, `detect_interactions()`.
- **Evidence.** Before: ten books with a dispersion of 1.9 and no
  interaction, 33 of 60 pairs at `P < 0.05`. With the row-level Pearson
  (1.72 against 1.91 for the cell totals of random partitions of the same
  rows) still 8 of 60. With the cell-total dispersion, 100 books per case:
  10 of 300 pairs (3.3%) at a dispersion of 1.94, 20 of 300 (6.7%) on pure
  Poisson; an interaction of 0.15 on the log scale found in 83% of the
  books on pure Poisson and 48% at a dispersion of 2.0.
- **Limitation.** One dispersion for the whole book. Under a gamma frailty
  the cells with larger expected counts carry more overdispersion than the
  average, so the test is slightly liberal there and slightly conservative
  elsewhere.
- **Pinned by.** `test-review-findings.R`: "the interaction scan is not
  fooled by overdispersion".

### PDP: partial dependence

#### PDP-01 An arithmetic mean on the response scale

- **Decision.** For each grid value, the whole training data is set to it,
  predicted on the response scale with the exposure offset removed
  (PDP-05), and averaged with weights.
- **Why.** `exp(mean(link))` is a geometric mean and biased low.
- **Where.** `R/pdp.R`, `make_pdp()`.
- **Pinned by.** `test-pdp.R`: "make_pdp computes the exposure-weighted
  mean at exposure 1".

#### PDP-02 The weights

- **Decision.** Frequency: `exposure_col` (falling back to `exp(offset)`),
  or the prior weights for a rate model, or unweighted with a warning.
  Severity: the prior weights, normally the claim count.
- **Where.** `R/pdp.R`.

#### PDP-03 Collapse to profiles

- **Decision.** The data is collapsed to unique profiles of the other
  predictors with summed weights before predicting.
- **Why.** `grid_res x profiles` predictions instead of `grid_res x rows`,
  with an identical result.
- **Where.** `R/pdp.R`.

#### PDP-04 Its own grid

- **Decision.** Factor levels, the unique values when there are at most
  `grid_res`, or an evenly spaced sequence over the range. Not the rounded
  grid of the rating table.
- **Where.** `R/pdp.R`. See section 4.

#### PDP-05 The PDP keeps every offset except the exposure

- **Decision.** `keep_offsets = TRUE` (default): only the offset
  expressions on `exposure_col` are subtracted, as in OFF-05, and any other
  offset stays in at each policy's own value. The variables of the
  subtracted offsets are pinned to 1 for the collapse to profiles; the kept
  ones become part of the profile. `FALSE` subtracts everything. The
  default y-axis title names the offsets kept.
- **Why.** The observed line is at each policy's own BM. A PDP at BM = 1
  against it showed a gap on a model that was exactly right.
- **Where.** `R/pdp.R`, `make_pdp()`.
- **Evidence.** The review's book with an average BM of 0.74: the PDP ran
  1.347 to 1.359 times the observed line at every level before; on the
  regression test it now sits within 5% of it.
- **Pinned by.** `test-review-findings.R`: "the PDP sits on the observed
  line with a bonus-malus offset"; `test-offset-link.R`: "a second offset
  does not end up on the exposure axis".
- **History.** Until the review fixes of October 2026 the PDP removed the
  whole offset (OFF-06).

### DIAG: diagnostics, Q-Q and influence

#### DIAG-01 Dispersion

- **Decision.** Pearson chi-squared over residual degrees of freedom; above
  1.2 for Poisson or binomial raises an overdispersion warning.
- **Where.** `R/diagnostics.R`, `glm_diagnostics()`.
- **Pinned by.** `test-diagnostics.R`: "overdispersion raises a warning".

#### DIAG-02 Collinearity per term

- **Decision.** Generalised VIF per term (Fox and Monette), on
  log-determinants of the coefficient correlation matrix, scaled to
  `GVIF^(1/(2 DF))`, flagged at 3. Aliased coefficients are dropped with a
  warning.
- **Why.** A plain VIF is meaningless for a spline or a factor spanning
  several columns. Log-determinants because a spline's correlation matrix
  underflows to zero long before the model is singular.
- **Where.** `R/diagnostics.R`, `glm_collinearity()`.
- **Pinned by.** `test-collinearity-spotlight.R`.

#### DIAG-03 Binned residuals

- **Decision.** Mean residual per quantile bin of the fitted value or a
  predictor, with a band of `2 sqrt(sum of the row variances) / n`, which
  is `2 sqrt(phi / n)` for Pearson residuals; deviance residuals are
  centred first (DIAG-06). Points, never a line.
- **Why.** Bin means are independent draws; a line invites a trend.
- **Where.** `R/diagnostics.R`, `plot_glm_residuals()`.
- **Pinned by.** `test-agg-plots.R`: "binned residuals are drawn as points,
  not a line"; `test-diagnostics.R`.

#### DIAG-04 Randomised quantile residuals for the Q-Q plot

- **Decision.** Dunn and Smyth (1996): each observation through its own
  fitted CDF, uniformly within the jump for a discrete response, then
  through `qnorm()`. Dispersion held at 1 for Poisson and binomial;
  estimated for Gamma and gaussian. Tails guarded at machine epsilon. A
  pointwise Beta band; at most `n_max` points drawn, extremes kept.
  Families without a closed-form CDF fall back to deviance residuals with a
  warning; a quasipoisson gets a caveat.
- **Why.** Classical residuals are not normal for counts.
- **Evidence.** On a correct Poisson with 20,000 rows: std. Pearson 8.34%
  outside a 5% band, std. deviance 1.31%, quantile residual 5.10% (KS
  p = 0.86). Identical to `statmod::qresid()` for Poisson, to 6e-14 for
  Gamma.
- **Where.** `R/qq-influence.R`, `.qr_dispersion()`,
  `.quantile_residuals()`, `plot_glm_qq()`.
- **Pinned by.** `test-qq-influence.R`.

#### DIAG-05 Influence is ranked, not thresholded

- **Decision.** Rows are ranked by Cook's distance and the top `n_label`
  labelled; `2p/n` is drawn as a leverage reference. Above `max_rows` (2e5)
  the model is refitted on a sample, with a message.
- **Why.** On a large book `D > 1` never fires and `D > 4/n` flags
  thousands. Cook's distance cannot see a low-exposure keying error (an
  exposure of 0.001 with two claims: worst residual in the book, about 30th
  on Cook's D), so the plot has two axes.
- **Where.** `R/qq-influence.R`, `plot_glm_influence()`.
- **Pinned by.** `test-qq-influence.R`: "a low-exposure keying error is
  invisible to Cook's distance", "an extreme predictor value tops the
  ranking, and masks itself", "plot_glm_influence caps the row count with
  a message".

#### DIAG-06 Deviance residuals are centred on their expectation

- **Decision.** With `residual_type = "deviance"`, each row's residual is
  shown minus its expectation under the fitted model, and the band per bin
  uses the rows' own variances (`.dev_resid_moments()`). The moments are
  exact sums over the response distribution, computed on a grid of the one
  parameter they depend on and interpolated: Poisson and quasipoisson on
  the mean count, negative binomial with unit weights, binomial for up to
  20 distinct trial counts, Gamma per prior weight (closed-form second
  moment, integrated first), Gaussian. Count families scale the variance
  by the Pearson dispersion. Other families are shown uncentred with a
  warning. The axis title says "centred".
- **Why.** A deviance residual is not centred: a claim-free row has
  `-sqrt(2 mu)`, so for small means the residuals are negative on average
  and a correct model sat below the band in every bin.
- **Where.** `R/utils-internal.R`, `.dev_resid_moments()`;
  `R/diagnostics.R`, `plot_glm_residuals()`.
- **Evidence.** A correct Poisson book of 60,000 rows: mean binned deviance
  residual -0.220 against a band of 0.058, 50 of 50 bins outside it.
- **Pinned by.** `test-review-findings.R`: "binned deviance residuals of a
  correct Poisson model stay in the band".

#### DIAG-07 A term in an interaction is reported, not flagged

- **Decision.** `glm_collinearity()` marks every interaction term and every
  term whose variables appear in one (`InInteraction`), reports their GVIF
  and leaves `Flag` at `NA` for them, with a warning naming them.
- **Why.** Their GVIF depends on how the variables are coded rather than on
  the data. Centring the numeric side of an interaction moves the
  coefficients of the other variable's main effect to another reference
  point, and with it their correlation with the interaction's. A term
  outside the interaction is unaffected, because its GVIF is invariant to
  any reparametrisation of the remaining terms.
- **Where.** `R/diagnostics.R`, `glm_collinearity()`.
- **Evidence.** The review: with `pmin(LEEFTIJD, 30):STEDELIJKHEID`,
  writing the age part as `pmin(LEEFTIJD, 30) - 30` moved the scaled GVIF
  of `STEDELIJKHEID` from 14.12 to 1.11, with the fitted values equal to
  2e-14.
- **Pinned by.** `test-collinearity-spotlight.R`: "a GVIF that depends on
  the coding is not flagged", "splines and rank deficiency do not empty the
  table".

#### DIAG-08 Negative binomial models

- **Decision.** A negative binomial fit, from `MASS::glm.nb()` or a `glm()`
  with `MASS::negative.binomial(theta)`, gets randomised quantile residuals
  from `pnbinom()` and centred deviance residuals, when its prior weights
  are 1. Theta is read from `model$theta` or the family's environment.
- **Why.** Its family name carries theta, "Negative Binomial(1.4)", so it
  fell through to the deviance fallback, which is not normal for counts.
- **Where.** `R/utils-internal.R`, `.nb_theta()`;
  `R/qq-influence.R`, `.quantile_residuals()`.
- **Pinned by.** `test-review-findings.R`: "a glm.nb model works in
  make_pdp and the A/E plot".

### SCR: feature screening

#### SCR-01 The booster is a diagnostic, never a pricing model

- **Decision.** xgboost is fitted with the GLM's log prediction as
  `base_margin`, so it can only model what the GLM leaves behind. No
  scorable object is returned.
- **Where.** `R/screen.R`, `screen_features()`.

#### SCR-02 Supported models

- **Decision.** Poisson or quasi-Poisson counts with a log link and an
  offset (`count:poisson`, row weights 1, the offset in the margin), and
  Gamma with a log link (`reg:gamma`, prior weights). Anything else is
  refused.
- **Where.** `R/screen.R`.
- **Pinned by.** `test-screen.R`: "unsupported families and bad splits are
  refused".

#### SCR-03 Out of sample on both sides

- **Decision.** The baseline is refitted on the training split; if that
  fails (usually a factor level missing from the split), the supplied model
  is used with a warning that its deviance is then in-sample.
- **Where.** `R/screen.R`.

#### SCR-04 A staged verdict

- **Decision.** Baseline, depth-1 booster, depth-`max_depth` booster. A
  depth-1 improvement of more than 0.1% means main effects are missing and
  the interaction ranking is not to be trusted yet.
- **Where.** `R/screen.R`.
- **Pinned by.** `test-screen.R`: "the verdict points at the missing main
  effect".

#### SCR-05 PermDeviance is a ranking, not a test

- **Decision.** Rank on the increase in test deviance when a feature is
  shuffled with the baseline held fixed. `Gain` is shown but not ranked on.
- **Evidence.** On 150,000 rows over three seeds, noise columns ranged
  from -3.8 to +2.3, a weak real predictor from -1.9 to +2.0, strong ones
  30 to 48.
- **Where.** `R/screen.R`.
- **Pinned by.** `test-screen.R`: "a genuinely predictive feature outranks
  pure noise", "features already in the baseline show no incremental
  value".

#### SCR-06 Performance levers

- **Decision.** `tree_method = "hist"` by default; `nthread` one fewer than
  the cores, 2 under R CMD check, capped by `OMP_THREAD_LIMIT`;
  `max_rows = Inf` by default (`NULL` means the same), with a message above
  500,000 rows; the SHAP interaction array capped near 1 GB.
- **Evidence.** 2,000,000 rows, 6 candidates: 291.6 s under xgboost's old
  default against 108.4 s with `hist`, identical ranking.
- **Where.** `R/screen.R`; `R/utils-internal.R`, `.default_nthread()`.
- **Pinned by.** `test-screen.R`: "max_rows defaults to every row and
  accepts Inf or NULL", "tree_method is hist by default and passed through".

#### SCR-07 Candidates and near-duplicates

- **Decision.** Constant columns, factors with more than `max_levels` (50)
  levels and unsupported types are skipped with a warning naming them.
  Numeric pairs with `|cor| >= 0.95` are reported as near-duplicates.
  Factors are one-hot encoded by hand, so a single-level factor does not
  break the matrix.
- **Where.** `R/screen.R`, `.screen_levels()`, `.screen_matrix()`.
- **Pinned by.** `test-screen.R`: "awkward columns are dropped instead of
  crashing", "a single-level factor no longer breaks the design matrix",
  "near-duplicate candidates are reported", "an uncorrelatable pair does
  not corrupt the near-duplicate table".

#### SCR-08 Outcome columns are not candidates

- **Decision.** With `features = NULL`, the candidates exclude the
  response, the offset variables, the prior-weight variables and
  `claims_col`, `loss_col` and `exposure_col`, with a message naming the
  columns left out. Named explicitly in `features`, such a column stays,
  with a warning.
- **Why.** The booster takes whatever predicts the response, and the loss
  amount predicts the claim count perfectly: the ranking and the verdict
  were then about leakage, not risk.
- **Where.** `R/screen.R`, `screen_features()`.
- **Evidence.** The review's WA book: `SCHADELAST` ranked first with a
  `PermDeviance` of 7,188 and 97% of the gain, and the verdict said the
  baseline missed main effects worth 95.9% of its deviance. On a severity
  model the claim count, its weight, ranked second.
- **Pinned by.** `test-review-findings.R`: "screen_features does not take
  the loss amount as a candidate".

### EXP: Excel export

#### EXP-01 Workbook layout

- **Decision.** An Overview sheet (timestamp, the three intercepts, the
  base value per variable), a Tariff sheet with every factor under a lookup
  key `Variable|Level` or `Variable|Level|Group` (an interaction key holds
  the uplift, EXP-04), one sheet per main effect
  (base row highlighted, thin rows greyed), and one per interaction with a
  Level x Group matrix of `Factor_Premium` (falling back to frequency or
  severity when that is empty).
- **Where.** `R/export.R`, `export_rating_table()`.
- **Pinned by.** `test-export-report.R`: "export_rating_table writes a
  structured workbook", "the Tariff sheet rebuilds a premium by lookup".

#### EXP-02 Numbers stay numbers; names stay valid

- **Decision.** Continuous levels are written as numbers, also in the
  interaction matrix. Sheet names are sanitised, cut to 31 characters and
  deduplicated case-insensitively.
- **Why.** Excel flags numbers stored as text, and treats sheet names
  case-insensitively, so `REGIO` and `Regio` would corrupt the workbook.
- **Where.** `R/export.R`, `.numeric_levels()`, `sheet_name()`.
- **Pinned by.** `test-export-report.R`; `test-awkward-names.R`: "export
  sheet names are deduplicated case-insensitively".

#### EXP-03 Offsets are not in the export

- **Decision.** The BM scale and the exposure are not factors and do not
  appear; the Overview lists the base value of each variable in the table.
- **Consequence.** A premium from the workbook is
  `Intercept premium x VLOOKUP factors x BM_KORTING`.

#### EXP-04 An interaction key on the Tariff sheet holds the uplift

- **Decision.** The factor columns of an interaction row on the Tariff
  sheet hold `Uplift_*`, the joint factor over the two main factors, and
  the instruction on the sheet says so. The interaction sheets keep the
  joint factor.
- **Why.** The instruction is "every variable's factor, times each
  interaction key". With the joint factor under the key, the two main
  effects were counted twice. With the uplift, main x main x uplift is the
  joint factor and the instruction holds literally.
- **Where.** `R/export.R`, the Tariff block.
- **Evidence.** The review rebuilt premiums by the instruction on a sample
  of policies: 16% too high on average, 5th to 95th percentile -21% to
  +79%, at most 3.4 times.
- **Pinned by.** `test-review-findings.R`: "the Tariff sheet instruction
  rebuilds the premium with an interaction".

#### EXP-05 The intercepts on the Overview sheet are numbers

- **Decision.** The Overview's value column is written as text and the
  intercepts and numeric base values are then written over it as numbers.
- **Why.** The column mixes a timestamp, levels and numbers. A number
  stored as text is what a formula referring to the intercept picks up, and
  in a locale with a decimal comma "0.110053" is not a number at all.
- **Where.** `R/export.R`, the Overview block.
- **Pinned by.** `test-review-findings.R`: "the intercepts on the Overview
  sheet are numbers, not text".

### REP: HTML report

#### REP-01 One rating table, with the table's own arguments

- **Decision.** The report builds one rating table and passes `grid_res`,
  `base_level`, `trim`, `merge_vars`, `base`, `grid_step`, `year_col`,
  `loss_col`, `exposure_col` and `claims_col` on to it.
- **Why.** The report should show the tariff a table built with the same
  arguments shows.
- **Where.** `R/report.R`, `pricing_report()`.
- **Pinned by.** `test-review-findings.R`: "the report passes merge_vars
  and base to its rating table".
- **History.** Until the review fixes of October 2026 the report passed no
  `merge_vars`, `base` or `grid_step`, so a merged field showed as two
  blocks.

#### REP-02 What the report contains and how it fails

- **Decision.** Diagnostics (fit table, collinearity, binned residuals);
  per variable the one-way (severity only alongside a severity model), A/E,
  PDP and rating plot; the rating table's interaction plots and an
  interaction scan on the frequency model (`n_bins = 10`, `n_sim = 100`,
  top 3 heatmaps). A failing plot is replaced by a note (REP-03).
- **Where.** `R/report.R`.
- **Pinned by.** `test-export-report.R`: "pricing_report writes a complete
  HTML report", "the one-way block covers severity, not just frequency".

#### REP-03 Every plot is built inside the guard

- **Decision.** Each plot is passed through `plotly::plotly_build()`
  inside the error guard, so a plot that cannot be built becomes a note. A
  number of 1e9 or more in a table is printed in scientific notation, and
  `NA` in a table cell prints as empty.
- **Why.** plotly builds a widget only when the page is written, so an
  error in one plot escaped the guard and took the whole report down at
  `save_html()`. `formatC(format = "d")` goes through an integer, so a
  large GVIF printed as "NA".
- **Where.** `R/report.R`, `safe()`, `fmt_num()`, `html_table()`.
- **Evidence.** The review: "Wasn't able to determine range of domain" on a
  400-policy book.
- **Pinned by.** `test-review-findings.R`: "pricing_report writes a report
  for a small book too".

### DATA: data handling and one-way aggregation

#### DATA-01 Training data is aligned by row name

- **Decision.** The training data (`.model_data()`, DATA-05) is matched to
  the model frame by row name, not by position.
- **Why.** Rows dropped by `na.action` and non-numeric row names would
  otherwise misalign values and fitted values.
- **Where.** `R/utils-internal.R`, `.glm_training_data()`.
- **Pinned by.** `test-data-table-input.R`: "model$data as a data.table is
  handled".

#### DATA-02 Aggregation rules

- **Decision.** `Frequency = sum(claims) / sum(exposure)` (NA at zero
  exposure), `Severity = sum(loss) / sum(claims)` (NA without claims).
  Missing values are counted in a warning and ignored. Grouping on a column
  name that `agg_all()` creates is refused.
- **Why.** The severity of a group is total loss over total claims, which
  weights each policy by its claim count, the same weight as the severity
  model. A clash of output names would silently produce two columns of one
  name.
- **Where.** `R/agg.R`, `agg_all()`.
- **Pinned by.** `test-agg-plots.R`: "agg_all works with custom column
  names and validates input", "grouping on a column agg_all creates is
  refused".

#### DATA-03 The volume bar is the metric's own denominator

- **Decision.** Under a frequency the bar is exposure; under a severity it
  is the claim count.
- **Why.** The level with the most exposure is often the one with the
  fewest claims, so exposure bars under a severity line point the wrong
  way.
- **Where.** `R/plot-oneway.R`, `make_plot()`; `R/pdp.R`.
- **Pinned by.** `test-agg-plots.R`: "the volume bars follow the metric's
  own denominator".

#### DATA-04 Discrete or continuous axis

- **Decision.** A character or factor axis, or a numeric one with at most
  `discrete_cutoff` (25) values, is drawn with markers only.
- **Where.** `R/plot-oneway.R`, `R/pdp.R`.

#### DATA-05 The training data of a model without model$data

- **Decision.** `.model_data()` returns `model$data` when that is a data
  frame, and otherwise evaluates the data the call names in the
  environment where the formula was written. Data found that way is
  checked against the model frame: if a plain column no longer matches,
  the data has changed since the fit and the function stops.
- **Why.** `MASS::glm.nb()` keeps no `model$data`, and a `glm()` fitted
  without `data =` keeps an environment there. The call still names the
  data, so PDP, A/E and grouped columns can be read. A name that has been
  reassigned since would silently pair other rows with the fit.
- **Where.** `R/utils-internal.R`, `.model_data()`, `.glm_training_data()`.
- **Pinned by.** `test-review-findings.R`: "a glm.nb model works in
  make_pdp and the A/E plot"; `test-rating-table.R`: "a regrouped column
  missing from 'data' is taken from the model".

#### DATA-06 A model fitted with na.exclude is read as na.omit

- **Decision.** Every function that mixes residuals, fitted values or
  influence measures with the model frame reads the model through
  `.as_na_omit()`, which sets the class of its `na.action` to `"omit"` on
  the local copy.
- **Why.** `na.exclude` pads `residuals()`, `fitted()` and `predict()`
  back to the full data with `NA`, while the model frame holds only the
  rows the fit used. Five functions failed on that, and the heatmap and
  the Q-Q plot were silently wrong because the vectors were recycled
  against each other. The fit itself is identical to `na.omit`.
- **Where.** `R/utils-internal.R`, `.as_na_omit()`; `R/ae.R`,
  `R/diagnostics.R`, `R/interactions.R`, `R/qq-influence.R`, `R/pdp.R`.
- **Pinned by.** `test-review-findings.R`: "na.exclude gives the same
  result as na.omit".

---

## 4. Known limitations and open points

These are known, deliberate or not yet addressed. Each says what to do
about it today.

1. **The bootstrap intervals hold the models fixed** (RATE-11). They
   resample policies, not the fit, so an in-sample interval is narrower
   than an honest one. Use a holdout for a Gini or a double lift you want
   to act on.
2. **Heatmap and interaction scan take `exp(offset)` as the volume**
   (OFF-08). With a BM offset, the exposure in the tooltip and
   `MaxAE_ExposureShare` are BM-weighted. The A/E is unaffected.
3. **Same-name columns with two meanings are only caught through
   `merge_vars`** (MV-04). Without a declared source column there is
   nothing to compare the two models' columns against. Declaring the
   relation is the protection.
4. **A measurement with at most 25 distinct values, used only through a
   band, is treated as a code** (MV-06). A value between two observed ones
   then errors instead of taking the nearer band. Make the source a
   continuous term of a model, or put the grouped column in the data.
5. **A spline extrapolates beyond its range in `predict()`** (MV-07). The
   package warns about it in `add_groupings()` but does not cap. Cap the
   values before scoring: `pmin(pmax(x, lo), hi)`.
6. **Only two-way interactions** get a rating-table block or an
   interaction scan (RT-06, INT-03).
7. **With one model, `Factor_Premium` and `intercept_premium` are `NA`**
   (RT-03). Use that model's own factor and intercept.
8. **The PDP grid is not the rating table's grid** (PDP-04), so the two
   are not point for point comparable.
9. **`seed` changes the global random state** (CONV-08).
10. **Only `glm` objects are supported.** GAMs, GBMs and mixed models are
    not read.
11. **A model fitted without `data =`** is read through the data its call
    names (DATA-05). When that name no longer exists, or holds other data,
    it cannot be used where the training data or a grouping has to be
    recovered (PDP, lift in-sample, screening, influence on a sample,
    deriving grouped columns); each says so.
12. **The standard errors in the rating table assume the model's
    dispersion** (RT-12). For a Poisson that is 1, so on overdispersed
    claim counts they are too small; fit a quasipoisson for honest ones.
13. **The interaction scan uses one dispersion for the whole book**
    (INT-05). Under a gamma frailty, cells with a large expected count
    carry more overdispersion than the average.
14. **Deviance residuals are centred for the common families only**
    (DIAG-06): Poisson, quasipoisson, negative binomial with unit weights,
    binomial with at most 20 distinct trial counts, Gamma, Gaussian. Other
    families are shown uncentred with a warning; use Pearson residuals
    there.

---

## 5. Test map

| Area | Test file |
|---|---|
| offsets, link, BM in rates | `test-offset-link.R` |
| rating table, `base`, `merge_vars`, grouped columns | `test-rating-table.R` |
| `add_groupings()` | `test-groupings.R` |
| `premium_impact()` | `test-impact.R`, `test-collinearity-spotlight.R` (contribution, spotlight), `test-offset-link.R` (BM) |
| `model_lift()`, `double_lift()` | `test-lift.R`, `test-offset-link.R` (BM) |
| `plot_glm_predictor()`, `agg_all()`, `make_plot()`, `make_rating_plot()`, binned residuals | `test-agg-plots.R` |
| `make_pdp()` | `test-pdp.R`, `test-offset-link.R` |
| diagnostics, collinearity | `test-diagnostics.R`, `test-collinearity-spotlight.R` |
| Q-Q and influence | `test-qq-influence.R` |
| interactions | `test-interactions.R` |
| screening | `test-screen.R` |
| export and report | `test-export-report.R` |
| awkward names, trimming, sheet names | `test-awkward-names.R` |
| the external review of October 2026: NA cells, `cut()`, logical predictors, `na.exclude`, the Tariff uplift, contribution, PDP with BM, overdispersion in the scan, deviance residuals, screening leakage, scoring messages, `at`, intervals, standard errors | `test-review-findings.R` |
| data.table and tibble input | `test-data-table-input.R` |
| axis ranges | `test-y-range.R` |

Shared fixtures live in `tests/testthat/setup.R`: a 20,000-row simulated
portfolio, a Poisson model with `ns(LEEFTIJD, 3) + REGIO +
factor(BOEKJAAR)`, a Gamma severity, and two rating tables. `setup.R` does
not attach `splines`, on purpose: `ns()` must work through the package's
re-export.

Run everything with `devtools::test()`; CI runs `R CMD check --as-cran` on
every push to `main` (`.github/workflows/R-CMD-check.yaml`).

---

## 6. Glossary

| Term | Meaning |
|---|---|
| `Exposure` | years on risk of a policy record; the default `exposure_col` |
| `AantalClaims` | claim count; the default `claims_col` |
| `SCHADELAST` | total loss amount of the record; the default `loss_col` |
| `BOEKJAAR` | accounting year; the default `year_col` |
| `BM_KORTING` | bonus-malus discount factor, typically carried as `offset(log(BM_KORTING))` |
| `AvgLoss` | average claim, `SCHADELAST / AantalClaims`, the usual severity response, weighted by `AantalClaims` |
| base | the policy every factor is measured against (RT-02) |
| intercept | the base policy's prediction per unit of exposure, offsets removed |
| factor | a prediction relative to the base, everything else at base (RT-01) |
| uplift | an interaction's joint factor over the product of its two main factors |
| thin cell | a level or cell with fewer than `min_claims` claims (RT-07) |
| `SE_Frequency`, `SE_Severity` | standard error of the logarithm of a factor (RT-12) |
| inconclusive | a double-lift verdict whose interval includes 0 (RATE-09) |
| cell-total dispersion | `sum((y - mu)^2) / sum(mu)`, the overdispersion of a sum of rows (INT-05) |
| A/E | actual over expected; for counts `sum(y) / sum(mu)` |
| source / grouped column | a column and a regrouping of it, such as `Stedelijkheid` and `Stedelijkheid_groep` (MV) |
| kept offset | an offset that stays in a comparison rate, such as the BM (OFF-05) |
| dislocation | the per-policy premium change between two tariffs |
| rebase | scaling the new rates to the old total, to separate level from redistribution |
| raking | iterative proportional fitting of expected to actual's margins (INT-03) |
