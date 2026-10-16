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
| `R/lift.R` | `model_lift()`, `double_lift()` | none | lift chart, Gini, Lorenz curve, double lift |
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
`.old_new_rates()`, which calls `.model_rate()` per model; `model_lift()`
calls `.model_rate()` directly. `.model_rate()` asks `.offset_split()` which
offset expressions to subtract and calls `.predict_no_offset()` with only
those.

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
| `.glm_training_data(model, fn)` | model frame, `model$data` aligned by row name, prior weights, offset | `ae.R`, `diagnostics.R`, `lift.R`, `pdp.R`, `qq-influence.R`, `screen.R`, `.glm_ae_parts()` |
| `.glm_newdata_parts(model, newdata, fn)` | the same pieces evaluated on other data | `plot_glm_predictor()` |
| `.check_range(rng, fn, arg)` | validate `c(lo, hi)` axis ranges | every plot with `y_range`, `x_range` or `z_range` |
| `.coerce_like(vals, template)` | coerce grid values to a column's type and factor levels | `make_rating_table()`, `make_pdp()` |
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
| `.model_rate(model, data, exposure_col, fn, keep_offsets)` | predicted rate per unit of exposure | `model_lift()`, `.old_new_rates()` |
| `.old_new_rates(...)` | old and new rate per policy from models or a premium column | `premium_impact()`, `double_lift()` |
| `.check_ae_scale(actual, predicted, fn, actual_col)` | warn when the overall A/E suggests mismatched units | `model_lift()`, `double_lift()` |
| `.exposure_bins(score, weight, n_bins, fn)` | equal-weight bins ordered by a score | `model_lift()`, `double_lift()` |
| `.gini_exposure(score, actual, weight)` | exposure-weighted Gini | `model_lift()` |
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
`screen_features()`) call `set.seed()` and so change the global random
state. Without `seed` they use whatever state there is.

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

- **Decision.** In `premium_impact()`, `model_lift()` and `double_lift()`,
  only the offset expressions that mention `exposure_col` are subtracted.
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

#### OFF-06 Rating table and PDP remove the whole offset

- **Decision.** The rating table's base row holds every offset variable at
  1 and its intercepts are computed with the whole offset subtracted; the
  PDP subtracts the whole offset too. Both are therefore at BM = 1.
- **Why.** A tariff table describes a base policy, and the BM discount is
  applied per policy on top, the same way the exposure is. A BM factor is
  not estimated, so it has no block in the table. The PDP shows a partial
  effect, which is cleaner without a policy-specific discount mixed into
  the average.
- **Where.** `R/rating-table.R`, `base_value_raw()` (offset variables
  return 1) and `intercept_val()`; `R/pdp.R`, `make_pdp()`.
- **Consequence.** A premium built from the table is
  `intercept_premium x factors x BM`. Verified per policy against
  `predict()` with Exposure = 1 and the policy's BM.
- **Pinned by.** `test-offset-link.R`: "the rating table intercept follows
  the model's own offset", "a second offset does not end up on the exposure
  axis" (premium chain with BM on top).

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
  with `base_level = "exposure"` the level with the most exposure.
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
  categorical main-effect levels raise a warning.
- **Why.** A GLM applies no shrinkage to a categorical level, so its factor
  rests on those few claims. Thin tail points of a continuous grid are
  normal and would make the warning noise.
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

### RATE: premium impact and lift

#### RATE-01 A rate is the product of the models, per unit of exposure

- **Decision.** Rate = frequency rate x severity rate, offsets handled per
  OFF-05; a model that is not supplied contributes 1.
- **Where.** `R/utils-internal.R`, `.model_rate()`, `.old_new_rates()`.

#### RATE-02 An old premium column

- **Decision.** `old_premium_basis = "amount"` divides by `exposure_col`
  (an earned premium for the record); `"rate"` takes it as a premium per
  unit of exposure. It must be on the footing of the kept offsets (OFF-05).
- **Where.** `R/utils-internal.R`, `.old_new_rates()`.
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
  `Contribution = ExposureShare x MeanChangePct`, which sums to the
  portfolio change.
- **Why.** Old and new are compared on the same policies, so there is no
  mix effect to isolate; contribution answers which segment drives the
  change.
- **Where.** `R/impact.R`.
- **Pinned by.** `test-collinearity-spotlight.R`: "contributions sum to the
  portfolio change".

#### RATE-05 Spotlight, dropped rows, histogram

- **Decision.** A spotlight is evaluated against `data` (or taken as a
  logical vector) and reported after the book-level rebase. Rows with a
  non-finite or non-positive premium or exposure are dropped with a
  warning. The histogram is pre-binned into 60 bars.
- **Why.** The spotlight shows how a segment moves relative to the book.
  Pre-binning keeps the widget small: a plotly histogram would embed every
  policy.
- **Where.** `R/impact.R`.
- **Pinned by.** `test-collinearity-spotlight.R`: "the spotlight reports a
  subset without filtering the book", "spotlight input is validated".

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
- **Where.** `R/utils-internal.R`, `.gini_exposure()`,
  `.check_ae_scale()`.
- **Pinned by.** `test-lift.R`: "a portfolio without losses gives NA rather
  than NaN for the Gini", "mismatched units are caught".

#### RATE-09 Double lift

- **Decision.** Bins are ordered by the new/old rate ratio; the winner is
  the model with the smaller mean absolute distance of its A/E from 1, with
  a tie when the two are equal.
- **Where.** `R/lift.R`, `double_lift()`.
- **Pinned by.** `test-lift.R`: "double lift prefers the model that is
  right where they differ", "double lift against itself shows no winner and
  a flat A/E".

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
  the model never saw is an error; the dispersion behind the bars is the
  fitted model's.
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
  diverges around 1 with a neutral midpoint.
- **Why.** A thin cell's A/E is noise, and colouring it would make it the
  loudest thing on the plot.
- **Where.** `R/interactions.R`, `plot_residual_heatmap()`,
  `.ta_diverging`.
- **Pinned by.** `test-interactions.R`: "plot_residual_heatmap returns a
  plot plus its cell table", "thin cells are not coloured and z_range is
  validated".

#### INT-03 The interaction statistic

- **Decision.** Per pair, expected is raked to actual's row and column
  margins (iterative proportional fitting, 50 iterations, tolerance 1e-9),
  and the likelihood-ratio deviance against the saturated table is the
  statistic. For Poisson counts the null comes from `n_sim` simulations
  under the raked expected; otherwise from a chi-square scaled by the
  Pearson dispersion.
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

### PDP: partial dependence

#### PDP-01 An arithmetic mean on the response scale

- **Decision.** For each grid value, the whole training data is set to it,
  predicted on the response scale with the offset removed, and averaged
  with weights.
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
  predictor, with a band of `2 sqrt(phi / n)` from the Pearson dispersion;
  points, never a line.
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

### EXP: Excel export

#### EXP-01 Workbook layout

- **Decision.** An Overview sheet (timestamp, the three intercepts, the
  base value per variable), a Tariff sheet with every factor under a lookup
  key `Variable|Level` or `Variable|Level|Group`, one sheet per main effect
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

### REP: HTML report

#### REP-01 One rating table, default arguments

- **Decision.** The report builds one rating table with `grid_res`,
  `base_level` and `trim`, and passes no `merge_vars`, `base` or
  `grid_step`.
- **Consequence.** A tariff with merged columns shows two blocks in the
  report. See section 4.
- **Where.** `R/report.R`, `pricing_report()`.

#### REP-02 What the report contains and how it fails

- **Decision.** Diagnostics (fit table, collinearity, binned residuals);
  per variable the one-way (severity only alongside a severity model), A/E,
  PDP and rating plot; the rating table's interaction plots and an
  interaction scan on the frequency model (`n_bins = 10`, `n_sim = 100`,
  top 3 heatmaps). A failing plot is replaced by a note.
- **Where.** `R/report.R`.
- **Pinned by.** `test-export-report.R`: "pricing_report writes a complete
  HTML report", "the one-way block covers severity, not just frequency".

### DATA: data handling and one-way aggregation

#### DATA-01 Training data is aligned by row name

- **Decision.** `model$data` is matched to the model frame by row name,
  not by position.
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

---

## 4. Known limitations and open points

These are known, deliberate or not yet addressed. Each says what to do
about it today.

1. **The report does not pass `merge_vars`, `base` or `grid_step`**
   (REP-01). Build the rating table yourself for those; the report's own
   table shows two blocks for a merged field and the regrouping warning.
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
11. **A model fitted without `data =`** cannot be used where the training
    data or a grouping has to be recovered (PDP, lift in-sample, screening,
    influence on a sample, deriving grouped columns); each says so.

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
| A/E | actual over expected; for counts `sum(y) / sum(mu)` |
| source / grouped column | a column and a regrouping of it, such as `Stedelijkheid` and `Stedelijkheid_groep` (MV) |
| kept offset | an offset that stays in a comparison rate, such as the BM (OFF-05) |
| dislocation | the per-policy premium change between two tariffs |
| rebase | scaling the new rates to the old total, to separate level from redistribution |
| raking | iterative proportional fitting of expected to actual's margins (INT-03) |
