# Internal helpers (not exported) ------------------------------------------

# Check that columns are present; clear error message per function.
.check_cols <- function(d, cols, fn) {
  cols <- unique(cols[!vapply(cols, is.null, logical(1))])
  missing_cols <- setdiff(unlist(cols), names(d))
  if (length(missing_cols))
    stop(fn, ": column(s) not found in the data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Normalise to a plain data.frame. A data.table reads `d[, cols]` as an
# expression rather than a column selection, and a tibble returns a
# one-column tibble where a vector is expected, so anything that is not
# exactly a data.frame is converted once at the entry point rather than
# guarded at every call site.
.as_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (!identical(class(x), "data.frame")) as.data.frame(x) else x
}

# Training data (original columns) aligned with the fitted rows of a glm.
# Uses rownames matching instead of as.integer(rownames), so it also works
# with non-numeric rownames and rows dropped by na.action.
.glm_training_data <- function(model, fn = "pricingtoolsRmO") {
  mf <- .as_df(stats::model.frame(model))
  d  <- .as_df(model$data)
  if (!is.null(d) && is.data.frame(d)) {
    idx <- match(rownames(mf), rownames(d))
    if (anyNA(idx))
      stop(fn, ": the rows of model.frame() cannot be matched to ",
           "model$data; fit the model with a 'data =' argument.",
           call. = FALSE)
    d <- d[idx, , drop = FALSE]
  } else {
    d <- NULL
  }
  pw <- model$prior.weights
  if (is.null(pw)) pw <- rep(1, nrow(mf))
  list(mf = mf, data = d, weights = as.numeric(pw), offset = model$offset)
}

# The same pieces as .glm_training_data(), but evaluated on other data, so
# a model can be held against a period it was never fitted on. The rows
# kept are the ones the model's variables can be evaluated on; a row with
# a missing value in one of them is dropped, exactly as glm() dropped it
# at fit time.
.glm_newdata_parts <- function(model, newdata, fn = "pricingtoolsRmO") {
  newdata <- .as_df(newdata)
  if (!is.data.frame(newdata) || !nrow(newdata))
    stop(fn, ": 'data' must be a data frame with at least one row.",
         call. = FALSE)

  tt   <- stats::terms(model)
  wexp <- if (!is.null(model$call)) model$call$weights else NULL
  need <- unique(c(all.vars(tt), .offset_vars(model), all.vars(wexp)))
  miss <- setdiff(need, names(newdata))
  if (length(miss))
    stop(fn, ": 'data' has no column(s) ", paste(miss, collapse = ", "),
         ". Besides the predictors, the response and any weight or offset ",
         "column are needed as well, because the point of the plot is to ",
         "hold the realised figure against the prediction.", call. = FALSE)

  # xlev makes a level the model never saw an error here, with the name of
  # the variable in it, rather than a bare predict() failure further down.
  mf <- tryCatch(
    .as_df(stats::model.frame(tt, data = newdata,
                              na.action = stats::na.omit,
                              xlev = model$xlevels)),
    error = function(e)
      stop(fn, ": 'data' cannot be run through the model: ",
           conditionMessage(e), call. = FALSE))
  if (!nrow(mf))
    stop(fn, ": no row of 'data' is usable; every row has a missing value ",
         "in one of the model's variables.", call. = FALSE)
  d <- newdata[match(rownames(mf), rownames(newdata)), , drop = FALSE]

  # Prior weights live in the call, not in the terms, so they have to be
  # re-evaluated on the new rows. Recycling the fitted weights instead
  # would silently pair one period's claim counts with another's rows.
  pw <- if (is.null(wexp)) rep(1, nrow(d)) else {
    v <- suppressWarnings(as.numeric(
      eval(wexp, d, environment(formula(model)))))
    if (length(v) != nrow(d) || anyNA(v))
      stop(fn, ": the model's weights (", deparse(wexp)[1], ") do not give ",
           "one usable value per row of 'data'.", call. = FALSE)
    v
  }
  off <- if (length(.offset_exprs(model))) .offset_value(model, d) else NULL
  list(mf = mf, data = d, weights = pw, offset = off)
}

# Validate an optional axis range c(lo, hi). Returns it as numeric, or
# NULL when not supplied (in which case the axis auto-scales).
.check_range <- function(rng, fn, arg = "y_range") {
  if (is.null(rng)) return(NULL)
  if (length(rng) != 2 || !all(is.finite(rng)) || rng[1] >= rng[2])
    stop(fn, ": '", arg, "' must be c(lo, hi) with lo < hi.", call. = FALSE)
  as.numeric(rng)
}

# Coerce values to the type of an existing column, so that inline
# transformations in the formula (e.g. factor(YEAR) on a numeric column)
# can be predicted correctly.
.coerce_like <- function(vals, template) {
  if (is.factor(template)) {
    factor(as.character(vals), levels = levels(template))
  } else if (is.numeric(template)) {
    suppressWarnings(as.numeric(as.character(vals)))
  } else {
    as.character(vals)
  }
}

# Predictor variables (RHS) of a model; robust for composite responses
# (cbind, ratios) because the full LHS is excluded.
.rhs_vars <- function(model) {
  f <- formula(model)
  setdiff(all.vars(f), all.vars(f[[2]]))
}

# Actual and expected per fitted row, on a scale where A/E is meaningful.
# Offset model with log link (typical frequency): actual and expected are
# counts. Otherwise (typical severity): actual and expected are weighted
# totals, so the ratio is the weighted-mean ratio.
.glm_ae_parts <- function(model, fn = "pricingtoolsRmO") {
  tr <- .glm_training_data(model, fn)
  y  <- as.numeric(tr$mf[[1]])
  mu <- as.numeric(predict(model, type = "response"))
  has_offset <- !is.null(tr$offset) && any(tr$offset != 0)

  # exp(offset) is only an exposure when the offset is a logarithm. With
  # offset(Exposure) it is exp(Exposure), which is not a volume at all, so
  # the counts branch below would weight by nonsense.
  if (has_offset && identical(family(model)$link, "log") &&
      isTRUE(!.offset_is_log(model)))
    warning(fn, ": the model's offset is not a logarithm, so exp(offset) ",
            "is not an exposure. The prior weights are used instead.",
            call. = FALSE)
  has_offset <- has_offset && !isTRUE(!.offset_is_log(model))

  out <- if (has_offset && identical(family(model)$link, "log")) {
    c(tr, list(actual = y, expected = mu, exposure = exp(tr$offset),
               claims = y, exposure_label = "Exposure", counts = TRUE))
  } else {
    w <- tr$weights
    c(tr, list(actual = w * y, expected = w * mu, exposure = w,
               claims = w, exposure_label = "Weight", counts = FALSE))
  }
  # Whether a Poisson resampling null is valid depends on the response
  # being counts, not on an offset being present: a Poisson model fitted
  # on aggregated cells without an offset qualifies just as well.
  out$poisson_counts <-
    family(model)$family %in% c("poisson", "quasipoisson") &&
    all(is.finite(out$actual)) &&
    all(abs(out$actual - round(out$actual)) < 1e-8)
  out
}

# Values of one variable, aligned with the model's fitted rows
.glm_var_values <- function(parts, var, fn) {
  if (var %in% names(parts$mf)) return(parts$mf[[var]])
  if (!is.null(parts$data) && var %in% names(parts$data))
    return(parts$data[[var]])
  stop(fn, ": variable '", var, "' not found in the model data. It must be ",
       "a column of the data the model was fitted on.", call. = FALSE)
}

# Group a vector for cell-wise summaries: categorical as is, numeric with
# at most n_bins distinct values on its exact values, otherwise quantile
# bins (so cells hold a comparable number of observations).
.group_values <- function(x, n_bins) {
  if (!is.numeric(x)) return(factor(x))
  u <- sort(unique(x[!is.na(x)]))
  if (length(u) <= n_bins) return(factor(x, levels = u))
  brks <- unique(stats::quantile(x, probs = seq(0, 1, length.out = n_bins + 1),
                                 na.rm = TRUE, names = FALSE))
  if (length(brks) < 2) brks <- range(x, na.rm = TRUE) + c(-0.5, 0.5)
  cut(x, breaks = brks, include.lowest = TRUE, dig.lab = 4)
}

# Every unevaluated offset expression the model carries.
#
# There are two places one can hide. Inside the formula, as
# offset(log(Exposure)), it lands in attr(terms, "offset"). Passed as a
# separate argument, glm(y ~ x, offset = log(Exposure)), it never reaches
# the terms at all and only survives as the unevaluated call in
# model$call$offset. A model can carry both, in which case they add.
# predict.lm reads exactly these two places, so following it keeps us in
# step with what predict() itself puts into the linear predictor.
.offset_exprs <- function(model) {
  out <- list()
  tt  <- terms(model)
  idx <- attr(tt, "offset")
  if (!is.null(idx) && length(idx)) {
    vars <- attr(tt, "variables")
    out <- c(out, lapply(idx, function(i) vars[[i + 1L]]))
  }
  co <- if (!is.null(model$call)) model$call$offset else NULL
  if (!is.null(co)) out <- c(out, list(co))
  out
}

# The offset's contribution to the linear predictor, evaluated on new data.
# Derived rather than assumed, so offset(Exposure), offset(log(Months / 12)),
# several offset terms at once and the offset = argument all come out right.
#
# `idx` limits it to some of the offset expressions, so the exposure part
# can be taken out while a known relativity such as a bonus-malus scale
# stays in.
.offset_value <- function(model, data, idx = NULL) {
  ex <- .offset_exprs(model)
  if (!is.null(idx)) ex <- ex[idx]
  if (!length(ex)) return(rep(0, nrow(data)))
  env <- environment(formula(model))
  v <- lapply(ex, function(e) as.numeric(eval(e, data, env)))
  # An offset handed over as an already-computed vector cannot be
  # re-evaluated per row of other data: its expression is just a name, and
  # what it resolves to belongs to the rows it was fitted on. Recycling it
  # would produce a confidently wrong number, so refuse instead.
  bad <- vapply(v, function(x) length(x) != nrow(data), logical(1))
  if (any(bad))
    stop("pricingtoolsRmO: the model's offset (",
         paste(vapply(ex[bad], function(e) deparse(e)[1], character(1)),
               collapse = ", "), ") does not evaluate to one value per row ",
         "of this data, so it cannot be neutralised. Put the offset in the ",
         "formula, as offset(log(Exposure)), and refit.", call. = FALSE)
  Reduce(`+`, v)
}

# Prediction on the response scale with the offset taken back out.
#
# Setting the exposure column to 1 only neutralises an offset of the form
# offset(log(Exposure)): with offset(Exposure) it leaves 1 instead of 0
# (a factor e too high) and with offset(log(Months / 12)) it leaves
# log(1 / 12) (a factor 12 out). Subtracting the offset the model actually
# carries, on the link scale, is right for any offset and any link.
#
# Callers may still pin the offset columns to a constant before calling
# this - make_pdp() does, because that is what lets it collapse the data to
# unique predictor profiles - and the result is the same either way.
.predict_no_offset <- function(model, newdata, idx = NULL) {
  eta <- as.numeric(predict(model, newdata = newdata, type = "link"))
  family(model)$linkinv(eta - .offset_value(model, newdata, idx))
}

# Which offset expressions to take out of a rate, and which stay in.
#
# The exposure offset says how long a policy was on risk, which is not a
# property of the risk: it goes. Any other offset is a known relativity of
# that policy - a bonus-malus scale is the usual one - and every comparison
# a rate is used for holds it against something that includes it: the
# premium the customer pays, the claims that actually happened. Taking it
# out as well put every policy at the no-discount class, so a correct model
# read about 20% too expensive against its own claims on a book with an
# average discount of 20%.
#
# Two cases cannot be split and fall back to taking everything out: an
# offset that is not on `exposure_col` at all (the model offsets on months,
# say, which is an exposure under another name), and one expression that
# holds the exposure together with something else, log(Exposure * BM).
.offset_split <- function(model, exposure_col, keep_offsets = TRUE) {
  ex <- .offset_exprs(model)
  n  <- length(ex)
  if (!n) return(list(drop = integer(0), kept = character(0), mixed = NULL))
  vars    <- lapply(ex, all.vars)
  on_expo <- vapply(vars, function(v) exposure_col %in% v, logical(1))
  if (!isTRUE(keep_offsets) || !any(on_expo))
    return(list(drop = seq_len(n), kept = character(0), mixed = NULL))
  mixed <- which(on_expo & lengths(vars) > 1L)
  kept  <- unique(unlist(vars[!on_expo]))
  list(drop  = which(on_expo),
       kept  = if (is.null(kept)) character(0) else kept,
       mixed = if (length(mixed))
         setdiff(unique(unlist(vars[mixed])), exposure_col) else NULL)
}

# The offsets that stay in the rate, for the output to say what it is on.
.kept_offsets <- function(models, exposure_col, keep_offsets = TRUE) {
  out <- unlist(lapply(Filter(Negate(is.null), models), function(m)
    .offset_split(m, exposure_col, keep_offsets)$kept))
  unique(as.character(out))
}

# Is every offset term a logarithm? Only then is exp(offset) an exposure,
# which the A/E and weighting code relies on.
.offset_is_log <- function(model) {
  ex <- .offset_exprs(model)
  if (!length(ex)) return(NA)
  all(vapply(ex, function(e) {
    if (is.call(e) && identical(as.character(e[[1L]])[1], "offset"))
      e <- e[[2L]]
    is.call(e) && identical(as.character(e[[1L]])[1], "log")
  }, logical(1)))
}

# Variables appearing in the model's offset term, e.g. "Exposure" in
# offset(log(Exposure)); never candidates for an interaction scan.
.offset_vars <- function(model) {
  if (is.null(model)) return(character(0))
  ex <- .offset_exprs(model)
  if (!length(ex)) return(character(0))
  unique(unlist(lapply(ex, all.vars)))
}

# Threads to hand to the booster. R CMD check and CRAN cap what a package
# may claim, and grabbing every core inside a check can exhaust the
# process's thread budget outright, so honour those limits rather than
# assume the machine is ours alone.
.default_nthread <- function() {
  lim <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(lim) && !identical(tolower(lim), "false")) return(2L)
  n <- suppressWarnings(parallel::detectCores())
  if (!isTRUE(is.finite(n)) || n < 1L) n <- 1L
  n <- max(1L, n - 1L)
  omp <- suppressWarnings(as.integer(Sys.getenv("OMP_THREAD_LIMIT", "")))
  if (isTRUE(is.finite(omp)) && omp >= 1L) n <- min(n, omp)
  as.integer(n)
}

# Predicted rate per unit of exposure. The offset is neutralised by setting
# the variable the model actually offsets on to 1, so a frequency and a
# severity model multiply to a risk premium per exposure unit. A NULL model
# contributes a factor 1.
#
# Neutralising `exposure_col` regardless would be wrong whenever the model
# offsets on something else: a model fitted with offset(log(Ex)) alongside
# an Exposure column would keep its offset intact and return a prediction
# per record instead of a rate, silently, because the column being set to 1
# is not the one in the linear predictor.
#
# With `keep_offsets = TRUE` only the exposure offset is taken out and any
# other offset, such as a bonus-malus scale, stays in; see .offset_split().
.model_rate <- function(model, data, exposure_col, fn, keep_offsets = TRUE) {
  if (is.null(model)) return(rep(1, nrow(data)))
  if (is.null(model$offset))
    return(as.numeric(predict(model, newdata = data, type = "response")))
  sp <- .offset_split(model, exposure_col, keep_offsets)
  if (length(sp$mixed))
    warning(fn, ": the exposure offset also holds '",
            paste(sp$mixed, collapse = ", "), "' in the same expression, so ",
            "the two cannot be separated and both are taken out of the rate. ",
            "Write them as separate terms, offset(log(", exposure_col,
            ")) + offset(log(", sp$mixed[1], ")), to keep the second in.",
            call. = FALSE)

  ov <- .offset_vars(model)
  lk <- family(model)$link

  # A rate per unit of exposure is a multiplicative idea, and only a log
  # link makes the offset multiplicative. With any other link the offset
  # can still be removed - and is - but what comes back is the prediction
  # at zero offset, not something a second model can be multiplied onto.
  if (!identical(lk, "log"))
    warning(fn, ": the model has a '", lk, "' link, so the offset is not a ",
            "multiplicative exposure. The prediction is returned with the ",
            "offset removed, which is not a rate per unit of exposure and ",
            "should not be multiplied by a second model.", call. = FALSE)
  else if (length(ov) && !exposure_col %in% ov)
    warning(fn, ": the model offsets on '", paste(ov, collapse = ", "),
            "' rather than on '", exposure_col, "'. The rates are therefore ",
            "per unit of that variable, while the weighting uses '",
            exposure_col, "'.", call. = FALSE)

  # Whether the offset can be neutralised is not a question of its
  # variables being columns of `data`: offset = rep(0.1, n) resolves `n`
  # from the formula's environment and is perfectly evaluable. Let the
  # evaluation itself be the test, and fail loudly rather than hand back a
  # prediction that still has the offset in it.
  out <- tryCatch(.predict_no_offset(model, data, sp$drop),
                  error = function(e) e)
  if (inherits(out, "error"))
    stop(fn, ": the model has an offset that could not be evaluated on this ",
         "data, so the result would not be a rate per unit of exposure. ",
         conditionMessage(out), call. = FALSE)
  out
}

# Old and new rate per policy, from either two model sets or an existing
# premium column. Shared by premium_impact() and double_lift() so the two
# cannot drift apart.
.old_new_rates <- function(data, model_freq_new, model_sev_new,
                           model_freq_old, model_sev_old,
                           old_premium_col, old_premium_basis,
                           exposure_col, fn, keep_offsets = TRUE) {
  if (is.null(model_freq_new) && is.null(model_sev_new))
    stop(fn, ": provide at least one NEW model.", call. = FALSE)
  has_old <- !is.null(model_freq_old) || !is.null(model_sev_old)
  if (!has_old && is.null(old_premium_col))
    stop(fn, ": provide old models or 'old_premium_col'.", call. = FALSE)
  if (has_old && !is.null(old_premium_col))
    stop(fn, ": provide either old models or 'old_premium_col', not both.",
         call. = FALSE)

  # Both sides get the same treatment of their offsets, so a bonus-malus
  # scale either stays in on both or comes out on both
  new_rate <- .model_rate(model_freq_new, data, exposure_col, fn, keep_offsets) *
              .model_rate(model_sev_new,  data, exposure_col, fn, keep_offsets)
  old_rate <- if (has_old) {
    .model_rate(model_freq_old, data, exposure_col, fn, keep_offsets) *
      .model_rate(model_sev_old, data, exposure_col, fn, keep_offsets)
  } else if (old_premium_basis == "amount") {
    data[[old_premium_col]] / data[[exposure_col]]
  } else {
    data[[old_premium_col]]
  }
  list(new = new_rate, old = old_rate,
       kept = .kept_offsets(list(model_freq_new, model_sev_new,
                                 model_freq_old, model_sev_old),
                            exposure_col, keep_offsets))
}

# One line for an output to say what its rates are on.
.kept_label <- function(kept) {
  if (length(kept)) paste(kept, collapse = ", ") else "none"
}

# An overall A/E far from 1 nearly always means the prediction and
# `actual_col` are not the same quantity: a frequency model compared
# against loss amounts, or a risk premium against claim counts. Cheap to
# check and it catches the mistake before anyone reads the chart.
.check_ae_scale <- function(actual, predicted, fn, actual_col) {
  if (predicted <= 0 || actual <= 0) return(invisible(NULL))
  ae <- actual / predicted
  if (ae > 2 || ae < 0.5)
    warning(fn, ": the overall actual/expected is ", signif(ae, 3),
            ", so '", actual_col, "' and the model predictions are ",
            "probably not the same quantity. Use claim counts with a ",
            "frequency model and loss amounts with a severity or risk ",
            "premium model.", call. = FALSE)
  invisible(NULL)
}

# Bins holding roughly equal exposure, ordered by `score`. Equal exposure
# rather than equal policy counts, so every bin carries the same weight in
# the comparison.
#
# Policies sharing a score are split across bins in whatever order they
# happen to sit in, so with fewer distinct scores than bins the chart
# invents a resolution the model does not have. That is worth saying out
# loud rather than drawing.
.exposure_bins <- function(score, weight, n_bins, fn = NULL) {
  # Count ties at a tolerance, not bit for bit. Removing an offset on the
  # link scale leaves rounding noise of order 1e-16, and predictions that
  # are equal for every practical purpose would otherwise be counted as
  # distinct and the warning would never fire where it matters most.
  nd <- length(unique(signif(score, 12)))
  if (!is.null(fn) && nd < n_bins)
    warning(fn, ": only ", nd, " distinct predicted value(s) for ", n_bins,
            " bins, so policies with an identical prediction are split ",
            "across bins in arbitrary order. Use n_bins <= ", nd,
            " for bins that mean something.", call. = FALSE)
  o  <- order(score)
  cw <- cumsum(weight[o]) / sum(weight)
  b  <- pmin(n_bins, floor(cw * n_bins) + 1L)
  out <- integer(length(score))
  out[o] <- b
  out
}

# Gini of the exposure-weighted Lorenz curve: cumulative share of actual
# losses against cumulative share of exposure, ordered by the prediction.
# 0 means no discrimination, higher means the model separates risk better.
# Not the classical income Gini, which orders by the variable itself.
.gini_exposure <- function(score, actual, weight) {
  # Without losses the Lorenz curve has no vertical scale, and the naive
  # formula returns NaN, which then prints as "NaN" in a chart title
  if (!isTRUE(sum(actual) > 0)) return(NA_real_)
  o <- order(score)
  x <- cumsum(weight[o]) / sum(weight)
  y <- cumsum(actual[o]) / sum(actual)
  x <- c(0, x); y <- c(0, y)
  auc <- sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
  1 - 2 * auc
}

# A readable step for a continuous rating grid. Aims at roughly `n` points
# across the range and then rounds to a 1 / 2 / 5 x 10^k value, the same
# rule an axis uses for its ticks, so a tariff lists ages 18, 19, 20 and
# weights 800, 850, 900 rather than 19.2653 and 832.65. Integer-valued
# columns never get a fractional step.
.nice_step <- function(rng, n, integer_data) {
  span <- diff(rng)
  if (!is.finite(span) || span <= 0) return(NA_real_)
  raw  <- span / max(1, n)
  mag  <- 10^floor(log10(raw))
  step <- mag * c(1, 2, 5, 10)[which(c(1, 2, 5, 10) >= raw / mag)[1]]
  if (integer_data) step <- max(1, round(step))
  step
}

# Should a whole-number column be listed value by value? A tariff wants
# every age, not every second age. The decision is on the values, never on
# a step of 1 across the range: a coded column such as a sum insured can
# hold five values spread over a million, and stepping through that one
# unit at a time would build a grid of a million rows.
.list_values <- function(uv, n, integer_data)
  integer_data && length(uv) && length(uv) <= 2 * max(1, n)

# Grid over [lo, hi] in whole multiples of `step`, so the values read as
# round numbers rather than as an offset sequence. `base` is added if the
# clamping left it off the grid: it is the point the factors are divided
# by, so a grid that skips it would have no row with a factor of 1.
.step_grid <- function(lo, hi, step, base) {
  if (!is.finite(step) || step <= 0) return(sort(unique(c(lo, hi, base))))
  k_lo <- ceiling((lo - 1e-9) / step)
  k_hi <- floor((hi + 1e-9) / step)
  g    <- if (k_hi >= k_lo) seq(k_lo, k_hi) * step else numeric(0)
  sort(unique(c(g, base)))
}

# Grid values as text for the Level column. as.character() switches a round
# number to scientific notation whenever that is shorter, so a sum-insured
# grid would read "1e+05", "2e+05" - unusable in a tariff and, worse, in
# the lookup key built from it.
.level_chr <- function(x) {
  if (!is.numeric(x)) return(as.character(x))
  format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
}

# Snap a value onto the same multiples-of-step grid, clamped to the range
.snap_to_step <- function(x, step, lo, hi) {
  if (!is.finite(step) || step <= 0) return(x)
  s <- round(x / step) * step
  min(max(s, lo), hi)
}

# Underlying column of a term label: "ns(AGE, 4)" -> "AGE".
# Backticks are stripped first, so a non-syntactic name such as
# `AUTO GEWICHT` still matches its column. Parsing the term and taking the
# first variable that is a column handles I(KM^2) and log(KM) too, where
# the old bracket-matching returned "KM^2" and lost the variable.
.term_base_var <- function(term, data) {
  term <- gsub("`", "", term, fixed = TRUE)
  if (term %in% names(data)) return(term)
  v <- tryCatch(all.vars(str2lang(term)), error = function(e) character(0))
  hit <- v[v %in% names(data)]
  if (length(hit)) return(hit[1])
  m <- regmatches(term, regexpr("\\(([^,\\)]+)", term))
  if (length(m) == 0) return(term)
  gsub("[()`]", "", m)
}

# ---- grouped columns ---------------------------------------------------------
# A grouping is made where it is needed - urbanisation 4 and 5 together next
# to the average claim - so it lives in the data a model was fitted on and
# often nowhere else. A fitted glm carries that data, so the relation can be
# read off the model instead of being rebuilt by hand for every dataset that
# has to be scored. make_rating_table() and add_groupings() share this, so
# the two cannot drift apart.

# Is a source variable looked up exactly (categorical) or banded
# (continuous)? The models say so where they can: a factor term is
# categorical, a numeric term continuous. A source no model uses directly
# falls back on its own column, where a numeric with few distinct values is
# a code - urbanisation 1 to 5 - and not a measurement.
.source_kind <- function(pv, models, src) {
  for (m in models) {
    xl <- names(m$xlevels)
    if (length(xl) && any(vapply(xl, .term_base_var, character(1),
                                 data = src) == pv)) return("cat")
  }
  for (m in models) if (pv %in% .rhs_vars(m)) {
    md <- if (is.data.frame(m$data)) m$data else NULL
    if (!is.null(md) && pv %in% names(md) && is.numeric(md[[pv]]))
      return("cont")
  }
  x <- src[[pv]]
  if (is.numeric(x) && data.table::uniqueN(x, na.rm = TRUE) > 25) "cont"
  else "cat"
}

# The grouped column `a` of source `pv`, for the rows of `data`: derived from
# the data of the model that uses it when `data` does not have it, and
# checked against that model when it does. Returns the column.
#
# Refused rather than guessed: a source value the model's data never saw
# (a level with no claims has no group there), a grouping that is not a
# function of its source, and one column name that two models group
# differently. A continuous source outside the fitted range is not refused:
# the outer bands of a banding are open in practice - nobody older than 80
# in the claims data does not make 85 a new class - so it takes the outer
# band, with a warning.
.grouping_column <- function(data, pv, a, model_freq, model_sev, fn) {
  models <- list(frequency = model_freq, severity = model_sev)
  models <- models[!vapply(models, is.null, logical(1))]
  owners <- models[vapply(models, function(m) a %in% .rhs_vars(m),
                          logical(1))]
  have   <- a %in% names(data)
  if (!length(owners)) {
    if (have) return(data[[a]])
    stop(fn, ": '", a, "' is not a column of 'data' and no model uses it, ",
         "so there is nothing to merge.", call. = FALSE)
  }

  # the relation as each model that uses the column knows it
  maps <- list()
  for (nm in names(owners)) {
    m   <- owners[[nm]]
    # a model fitted without data = carries its environment here, not a table
    src <- if (is.data.frame(m$data)) .as_df(m$data) else NULL
    if (is.null(src)) {
      if (have) next
      stop(fn, ": '", a, "' is not a column of 'data'. It could be taken ",
           "from the data the ", nm, " model was fitted on, but that model ",
           "carries none; fit it with a 'data =' argument, or add the column ",
           "to 'data' yourself.", call. = FALSE)
    }
    if (!all(c(pv, a) %in% names(src))) {
      if (have) next
      stop(fn, ": '", a, "' is not a column of 'data', and the data the ", nm,
           " model was fitted on has no column '",
           if (!pv %in% names(src)) pv else a, "', so the relation between ",
           "the two cannot be read off anywhere. Add the column to 'data'.",
           call. = FALSE)
    }
    ok <- !is.na(src[[pv]]) & !is.na(src[[a]])
    pr <- as.data.frame(unique(data.table::data.table(
      p = as.character(src[[pv]])[ok], g = as.character(src[[a]])[ok])))
    dup <- unique(pr$p[duplicated(pr$p)])
    if (length(dup))
      stop(fn, ": '", a, "' is not a regrouping of '", pv, "' in the data ",
           "the ", nm, " model was fitted on: the value(s) ",
           paste(utils::head(dup, 5), collapse = ", "),
           " occur there with more than one group.", call. = FALSE)
    maps[[nm]] <- list(map = stats::setNames(pr$g, pr$p), src = src)
  }
  if (!length(maps)) return(data[[a]])     # nothing to check it against

  # One column name, two meanings. A grouping made twice, once per dataset,
  # need not agree, and only one of the two can be in a dataset that is
  # scored. With different labels that ends in a prediction failure; with
  # the same labels nothing fails and one model is quietly read on the
  # other's grouping.
  if (length(maps) == 2) {
    m1  <- maps[[1]]$map; m2 <- maps[[2]]$map
    sh  <- intersect(names(m1), names(m2))
    off <- sh[m1[sh] != m2[sh]]
    if (length(off))
      stop(fn, ": '", a, "' does not mean the same thing everywhere: '",
           off[1], "' of '", pv, "' is '", m1[[off[1]]], "' in the data the ",
           names(maps)[1], " model was fitted on and '", m2[[off[1]]],
           "' in the data the ", names(maps)[2], " model was fitted on",
           if (length(off) > 1) paste0(" (", length(off), " values differ)"),
           ". Two groupings of one variable cannot share a column name: give ",
           "them separate names and name both in 'merge_vars', as ",
           "merge_vars = list(", pv, " = c(\"", a, "_freq\", \"", a,
           "_sev\")).", call. = FALSE)
  }
  owner_nm <- names(maps)[1]
  map  <- maps[[1]]$map
  if (length(maps) == 2) {
    extra <- setdiff(names(maps[[2]]$map), names(map))
    map   <- c(map, maps[[2]]$map[extra])
  }
  src  <- maps[[1]]$src
  kind <- .source_kind(pv, models, src)
  x    <- data[[pv]]

  if (kind == "cont") {
    keys <- suppressWarnings(as.numeric(names(map)))
    o    <- order(keys); keys <- keys[o]; band <- unname(map)[o]
    xv   <- suppressWarnings(as.numeric(as.character(x)))
    brks <- c(-Inf, keys[-1] - diff(keys) / 2, Inf)
    vals <- band[findInterval(xv, brks)]
    vals[is.na(xv)] <- NA
    lo <- !is.na(xv) & xv < keys[1]
    hi <- !is.na(xv) & xv > keys[length(keys)]
    if (!have && (any(lo) || any(hi))) {
      rows <- function(k) paste(k, if (k == 1) "row" else "rows")
      bits <- c(if (any(hi)) sprintf("%s above %s got '%s'", rows(sum(hi)),
                                     format(keys[length(keys)]),
                                     band[length(band)]),
                if (any(lo)) sprintf("%s below %s got '%s'", rows(sum(lo)),
                                     format(keys[1]), band[1]))
      also <- names(models)[vapply(models, function(m)
        pv %in% .rhs_vars(m) && is.null(m$xlevels[[pv]]), logical(1))]
      warning(fn, ": '", pv, "' lies outside the range the ", owner_nm,
              " model was fitted on (", format(keys[1]), " to ",
              format(keys[length(keys)]), "); ",
              paste(bits, collapse = " and "), ", the outer band(s) of '",
              a, "'.",
              if (length(also))
                paste0(" '", pv, "' is also a continuous term of the ",
                       paste(also, collapse = " and "), " model, which ",
                       "extrapolates beyond the range it was fitted on; ",
                       "capping the values, pmin(pmax(", pv, ", ",
                       format(keys[1]), "), ", format(keys[length(keys)]),
                       "), keeps it from doing so."),
              call. = FALSE)
    }
  } else {
    vals <- unname(map[as.character(x)])
    miss <- unique(as.character(x)[is.na(vals) & !is.na(x)])
    if (length(miss))
      stop(fn, ": the value(s) ", paste(utils::head(miss, 5), collapse = ", "),
           if (length(miss) > 5) paste0(" and ", length(miss) - 5, " more"),
           " of '", pv, "' do not occur in the data the ", owner_nm,
           " model was fitted on, so their group in '", a, "' is unknown. ",
           if (owner_nm == "severity")
             paste0("A level with no claims has no group there: give it one ",
                    "in that model's data, or put '", a, "' in 'data' ",
                    "yourself.")
           else
             paste0("Map them to a value the model knows first, or put '", a,
                    "' in 'data' yourself."),
           call. = FALSE)
  }

  tmpl <- src[[a]]
  vals <- if (is.factor(tmpl)) factor(vals, levels = levels(tmpl))
          else if (is.numeric(tmpl)) as.numeric(vals)
          else vals

  if (!have) return(vals)
  # Already there: checked, not overwritten. A column that disagrees with
  # the model would have that model scored on a grouping it never saw.
  cur <- as.character(data[[a]])
  bad <- which(!is.na(cur) & !is.na(vals) & cur != as.character(vals))
  if (length(bad))
    stop(fn, ": '", a, "' does not mean the same thing everywhere: in ",
         length(bad), " row(s) of 'data' it disagrees with the data the ",
         owner_nm, " model was fitted on; row ", bad[1], " has '", pv,
         "' = ", as.character(x[bad[1]]), " and '", a, "' = ", cur[bad[1]],
         ", where the model's data says ", as.character(vals[bad[1]]), ".",
         call. = FALSE)
  data[[a]]
}

# Base variables of a model that exist as columns in `data`
# (interaction terms split into their components, deduplicated)
.model_base_vars <- function(model, data) {
  if (is.null(model)) return(character(0))
  labs  <- attr(terms(model), "term.labels")
  comps <- unique(unlist(strsplit(labs, ":", fixed = TRUE)))
  bv    <- unique(vapply(comps, .term_base_var, character(1), data = data))
  intersect(bv, names(data))
}
