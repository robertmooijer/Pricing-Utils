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

# [CONV-02]
# Normalise to a plain data.frame. A data.table reads `d[, cols]` as an
# expression rather than a column selection, and a tibble returns a
# one-column tibble where a vector is expected, so anything that is not
# exactly a data.frame is converted once at the entry point rather than
# guarded at every call site.
.as_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (!identical(class(x), "data.frame")) as.data.frame(x) else x
}

# [DATA-05]
# The data a model was fitted on. glm() keeps it as model$data. MASS::glm.nb()
# does not, and a model fitted without data = keeps its environment there,
# but the call still names the data, so it is evaluated where the formula
# was written. Returns NULL when there is nothing to find. Rows are matched
# by name afterwards in .glm_training_data(), which also checks that the
# data has not changed since the fit.
.model_data <- function(model) {
  if (is.null(model)) return(NULL)
  if (is.data.frame(model$data)) return(model$data)
  cd <- if (!is.null(model$call)) model$call$data else NULL
  if (is.null(cd)) return(NULL)
  env <- tryCatch(environment(formula(model)), error = function(e) NULL)
  if (is.null(env)) return(NULL)
  d <- tryCatch(eval(cd, env), error = function(e) NULL)
  if (is.data.frame(d)) d else NULL
}

# [DATA-06]
# na.exclude pads residuals(), fitted(), predict() and the influence measures
# back to the full data with NA, while model.frame() holds only the rows the
# fit used. Every function that mixes the two misaligned: five of them
# failed and two (the heatmap, the Q-Q plot) went silently wrong. The fit
# itself is identical to na.omit, so the model is read as if fitted with
# na.omit. Only this local copy changes; the caller's object does not.
.as_na_omit <- function(model) {
  if (!is.null(model) && inherits(model$na.action, "exclude"))
    class(model$na.action) <- "omit"
  model
}

# [DATA-01]
# Training data (original columns) aligned with the fitted rows of a glm.
# Uses rownames matching instead of as.integer(rownames), so it also works
# with non-numeric rownames and rows dropped by na.action.
.glm_training_data <- function(model, fn = "pricingtoolsRmO") {
  mf <- .as_df(stats::model.frame(model))
  from_call <- !is.data.frame(model$data)
  d  <- .as_df(.model_data(model))
  if (!is.null(d) && is.data.frame(d)) {
    idx <- match(rownames(mf), rownames(d))
    if (anyNA(idx))
      stop(fn, ": the rows of model.frame() cannot be matched to ",
           "model$data; fit the model with a 'data =' argument.",
           call. = FALSE)
    d <- d[idx, , drop = FALSE]
    # Data found through the call is whatever that name holds now. If it
    # was changed after the fit, the plain columns of the model frame no
    # longer match it, and that is said rather than read past.
    if (from_call) {
      shared <- intersect(names(mf)[vapply(mf, function(x) is.null(dim(x)),
                                           logical(1))], names(d))
      changed <- shared[!vapply(shared, function(v)
        isTRUE(all.equal(as.character(mf[[v]]), as.character(d[[v]]))),
        logical(1))]
      if (length(changed))
        stop(fn, ": the data the model's call names has changed since the ",
             "fit (column ", paste(utils::head(changed, 3), collapse = ", "),
             " differs); refit the model, or fit it with data = so the data ",
             "is kept with it.", call. = FALSE)
    }
  } else {
    d <- NULL
  }
  pw <- model$prior.weights
  if (is.null(pw)) pw <- rep(1, nrow(mf))
  list(mf = mf, data = d, weights = as.numeric(pw), offset = model$offset)
}

# [AE-05]
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

  # [SCORE-01] A level the model never saw is named with its column and
  # rows; xlev below is the backstop for anything this check cannot
  # evaluate, rather than a bare predict() failure further down.
  un <- .unseen_levels(model, newdata)
  if (length(un)) stop(.unseen_message(un, "", fn, at_hint = FALSE),
                       call. = FALSE)
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

# [RT-14]
# Coerce values to the type of an existing column, so that inline
# transformations in the formula (e.g. factor(YEAR) on a numeric column)
# can be predicted correctly. A logical column stays logical: predict()
# refuses a character where the model was fitted on TRUE/FALSE.
.coerce_like <- function(vals, template) {
  if (is.factor(template)) {
    factor(as.character(vals), levels = levels(template))
  } else if (is.logical(template)) {
    as.logical(as.character(vals))
  } else if (is.numeric(template)) {
    suppressWarnings(as.numeric(as.character(vals)))
  } else {
    as.character(vals)
  }
}

# [SCORE-01]
# Values of a factor term that a model was not fitted on, per data column.
# Checked before predict(), so the message names the column and the values,
# instead of predict()'s "factor has new levels" surfacing from inside an
# offset evaluation or a model.frame() call, where it reads as something
# else. The term is evaluated on the data, so factor(BOEKJAAR) and a cut()
# banding are compared on the levels the model actually has. Returns a list
# per column with the unseen values, their row count and a logical row mask.
.unseen_levels <- function(model, data) {
  xl  <- model$xlevels
  out <- list()
  if (is.null(xl) || !length(xl)) return(out)
  env <- tryCatch(environment(formula(model)), error = function(e) NULL)
  for (nm in names(xl)) {
    col <- .term_base_var(nm, data)
    if (!col %in% names(data)) next
    v <- tryCatch(as.character(eval(str2lang(nm), data, env)),
                  error = function(e) NULL)
    if (is.null(v) || length(v) != nrow(data)) next
    bad <- !is.na(v) & !v %in% xl[[nm]]
    if (any(bad))
      out[[col]] <- list(values = unique(v[bad]), n = sum(bad), rows = bad)
  }
  out
}

# The message for those values, shared by every function that scores data.
# `label` names the model ("new frequency") or is "" for a lone model;
# `at_hint` points at the 'at' argument where the function has one.
.unseen_message <- function(un, label, fn, at_hint = TRUE) {
  parts <- vapply(names(un), function(cl)
    sprintf("'%s' = %s (%d row%s)", cl,
            paste(utils::head(un[[cl]]$values, 5), collapse = ", "),
            un[[cl]]$n, if (un[[cl]]$n == 1) "" else "s"), character(1))
  paste0(fn, ": the data holds value(s) the ", .model_label(label),
         "was not fitted on: ", paste(parts, collapse = "; "), ". A model ",
         "has no factor for a level it never saw. Map those values to a ",
         "level it knows or leave the rows out",
         if (at_hint) paste0("; for an accounting year beyond the data, ",
                             "price at the latest known year with 'at' and ",
                             "index the result yourself") else "",
         ".")
}

# "model " or "frequency model ", for the messages above and below.
.model_label <- function(label) {
  if (is.null(label) || !nzchar(label) || identical(label, "the"))
    "model " else paste0(label, " model ")
}

# [SCORE-02]
# Numeric predictors outside the range the model was fitted on. predict()
# extrapolates there without a word - linearly on the link scale beyond the
# last knot of a spline, without limit for a log term - so it is said once
# per call. A column the model turns into a factor (factor(BOEKJAAR)) is a
# level question, not a range one, and is left to .unseen_levels().
.range_warning <- function(model, data, label, fn) {
  tr <- tryCatch(.glm_training_data(model, fn)$data, error = function(e) NULL)
  if (is.null(tr)) return(invisible(NULL))
  vars <- setdiff(.rhs_vars(model), .offset_vars(model))
  fac  <- if (length(model$xlevels))
    vapply(names(model$xlevels), .term_base_var, character(1), data = tr)
    else character(0)
  vars <- vars[vars %in% names(tr) & vars %in% names(data) & !vars %in% fac]
  msgs <- character(0)
  for (v in vars) {
    if (!is.numeric(tr[[v]]) || !is.numeric(data[[v]])) next
    r   <- range(tr[[v]], na.rm = TRUE)
    out <- sum(data[[v]] < r[1] | data[[v]] > r[2], na.rm = TRUE)
    if (out)
      msgs <- c(msgs, sprintf("'%s' (%d row%s outside %s to %s)", v, out,
                              if (out == 1) "" else "s", format(r[1]),
                              format(r[2])))
  }
  if (length(msgs))
    warning(fn, ": the data goes beyond the range the ", .model_label(label),
            "was fitted on for ", paste(msgs, collapse = ", "), "; the model ",
            "extrapolates there (a spline linearly on the log scale, a log ",
            "term without limit). Cap the values to the fitted range if that ",
            "is not intended.", call. = FALSE)
  invisible(NULL)
}

# [RATE-10]
# Price every row at one value of some columns, e.g. at = list(BOEKJAAR =
# 2024): with the accounting year as a rating factor each policy is
# otherwise priced at the level of its own historic year. Validated like
# every other column argument; a value must be valid for the column.
.apply_at <- function(data, at, fn) {
  if (is.null(at)) return(data)
  at <- as.list(at)
  if (is.null(names(at)) || any(!nzchar(names(at))))
    stop(fn, ": 'at' must be named, e.g. list(BOEKJAAR = 2024).",
         call. = FALSE)
  .check_cols(data, names(at), fn)
  for (v in names(at)) {
    val <- at[[v]]
    if (length(val) != 1)
      stop(fn, ": 'at' for '", v, "' must be one value.", call. = FALSE)
    new <- .coerce_like(rep(val, nrow(data)), data[[v]])
    if (anyNA(new))
      stop(fn, ": '", val, "' is not a valid value for '", v, "'",
           if (is.factor(data[[v]]))
             paste0("; its levels are ", paste(levels(data[[v]]),
                                                collapse = ", ")) else "",
           ".", call. = FALSE)
    data[[v]] <- new
  }
  data
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

  # [OFF-04]
  # exp(offset) is only an exposure when the offset is a logarithm. With
  # offset(Exposure) it is exp(Exposure), which is not a volume at all, so
  # the counts branch below would weight by nonsense.
  if (has_offset && identical(family(model)$link, "log") &&
      isTRUE(!.offset_is_log(model)))
    warning(fn, ": the model's offset is not a logarithm, so exp(offset) ",
            "is not an exposure. The prior weights are used instead.",
            call. = FALSE)
  has_offset <- has_offset && !isTRUE(!.offset_is_log(model))

  # [OFF-08] Known gap: the volume here is exp(offset). With a second offset,
  # a bonus-malus scale, that is Exposure x BM, so the exposure the heatmap
  # shows and MaxAE_ExposureShare in detect_interactions() are BM-weighted.
  # The A/E is not affected: actual and expected are both counts. See
  # TECHNICAL.md, section 4.
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

# [OFF-01]
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

# [OFF-02]
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
  # [OFF-03]
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

# [OFF-02]
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

# [OFF-05]
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

# [OFF-04]
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

# [SCR-06]
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

# [OFF-05] [OFF-07] [OFF-09] [RATE-01]
# Predicted rate per unit of exposure. The offset is subtracted on the link
# scale by .predict_no_offset(), so a frequency and a severity model multiply
# to a risk premium per exposure unit. A NULL model contributes a factor 1.
#
# Which part of the offset is subtracted is decided by .offset_split(): with
# `keep_offsets = TRUE` only the expressions that mention `exposure_col`, so
# a bonus-malus scale stays in; with FALSE, or when no offset mentions
# `exposure_col` at all, every offset expression.
#
# Setting `exposure_col` to 1 instead would be wrong whenever the model
# offsets on something else: a model fitted with offset(log(Ex)) alongside
# an Exposure column would keep its offset intact and return a prediction
# per record instead of a rate, silently, because the column set to 1 is not
# the one in the linear predictor. Following the model's own offset
# expressions avoids that, and the warning below says the rate is then per
# unit of that other variable.
.model_rate <- function(model, data, exposure_col, fn, keep_offsets = TRUE,
                        label = "") {
  if (is.null(model)) return(rep(1, nrow(data)))
  # [SCORE-01] [SCORE-02] what the model cannot price, named before predict()
  un <- .unseen_levels(model, data)
  if (length(un)) stop(.unseen_message(un, label, fn), call. = FALSE)
  .range_warning(model, data, label, fn)
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

# [RATE-01] [RATE-02]
# Old and new rate per policy, from either two model sets or an existing
# premium column. Shared by premium_impact() and double_lift() so the two
# cannot drift apart.
.old_new_rates <- function(data, model_freq_new, model_sev_new,
                           model_freq_old, model_sev_old,
                           old_premium_col, old_premium_basis,
                           exposure_col, fn, keep_offsets = TRUE,
                           at = NULL, divisor_col = exposure_col) {
  if (is.null(model_freq_new) && is.null(model_sev_new))
    stop(fn, ": provide at least one NEW model.", call. = FALSE)
  has_old <- !is.null(model_freq_old) || !is.null(model_sev_old)
  if (!has_old && is.null(old_premium_col))
    stop(fn, ": provide old models or 'old_premium_col'.", call. = FALSE)
  if (has_old && !is.null(old_premium_col))
    stop(fn, ": provide either old models or 'old_premium_col', not both.",
         call. = FALSE)

  # The models price the data with `at` applied; an old premium column is
  # what was charged and is read as it is
  pd <- .apply_at(data, at, fn)

  # Both sides get the same treatment of their offsets, so a bonus-malus
  # scale either stays in on both or comes out on both
  new_rate <- .model_rate(model_freq_new, pd, exposure_col, fn, keep_offsets,
                          "new frequency") *
              .model_rate(model_sev_new,  pd, exposure_col, fn, keep_offsets,
                          "new severity")
  old_rate <- if (has_old) {
    .model_rate(model_freq_old, pd, exposure_col, fn, keep_offsets,
                "old frequency") *
      .model_rate(model_sev_old, pd, exposure_col, fn, keep_offsets,
                  "old severity")
  } else if (old_premium_basis == "amount") {
    data[[old_premium_col]] / data[[divisor_col]]
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

# [RATE-08]
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

# [RATE-06]
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

# [RATE-08]
# Gini of the exposure-weighted Lorenz curve: cumulative share of actual
# losses against cumulative share of exposure, ordered by the prediction.
# 0 means no discrimination, higher means the model separates risk better.
# Not the classical income Gini, which orders by the variable itself.
.gini_exposure <- function(score, actual, weight) {
  o <- order(score)
  .gini_ordered(weight[o], actual[o])
}

# The same on vectors already in score order, so a bootstrap can reuse one
# sort. Without losses the Lorenz curve has no vertical scale, and the naive
# formula returns NaN, which then prints as "NaN" in a chart title.
.gini_ordered <- function(w, a) {
  if (!isTRUE(sum(a) > 0) || !isTRUE(sum(w) > 0)) return(NA_real_)
  x <- c(0, cumsum(w) / sum(w))
  y <- c(0, cumsum(a) / sum(a))
  1 - 2 * sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
}

# [RATE-11]
# Bootstrap interval for the Gini. A Gini on raw losses is dominated by a
# few large claims: on a 16,000-policy holdout the 90% interval ran from
# 0.14 to 0.33 around a point value of 0.23, wider than the gap between two
# competing models. Policies are resampled with Poisson(1) counts, the usual
# large-sample stand-in for resampling with replacement, so the book is
# sorted once and every replicate is one weighted pass over it.
.gini_boot <- function(score, actual, weight, n_boot, conf) {
  if (n_boot < 1) return(c(NA_real_, NA_real_))
  o <- order(score); a <- actual[o]; w <- weight[o]
  sims <- vapply(seq_len(n_boot), function(b) {
    k <- stats::rpois(length(a), 1)
    .gini_ordered(w * k, a * k)
  }, numeric(1))
  stats::quantile(sims, c((1 - conf) / 2, 1 - (1 - conf) / 2),
                  na.rm = TRUE, names = FALSE)
}

# Share of the total held by the largest few records, to say why an interval
# is wide.
.top_share <- function(actual, k = 10) {
  tot <- sum(actual, na.rm = TRUE)
  if (!isTRUE(tot > 0)) return(NA_real_)
  sum(utils::head(sort(actual, decreasing = TRUE), k)) / tot
}

# [RATE-11]
# The sentence that names large losses as the reason, only when they are:
# the ten largest records carrying 10% or more of the total. A wide interval
# on a small book or between two close models has another cause, and
# capping is then no remedy.
.large_loss_note <- function(actual, actual_col) {
  top <- .top_share(actual)
  if (!isTRUE(top >= 0.10)) return("")
  sprintf(paste0(" The ten largest records carry %.0f%% of '%s'; capping ",
                 "large losses narrows the interval, provided the models ",
                 "are compared on the same capped losses."), 100 * top,
          actual_col)
}

# [RATE-11] The bootstrap arguments, checked once for every function that
# takes them.
.check_boot <- function(n_boot, conf_level, fn) {
  if (!is.numeric(n_boot) || length(n_boot) != 1 || !is.finite(n_boot) ||
      n_boot < 0 || n_boot != round(n_boot))
    stop(fn, ": 'n_boot' must be a whole number, 0 or more.", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop(fn, ": 'conf_level' must be a number between 0 and 1.",
         call. = FALSE)
  invisible(TRUE)
}

# [RATE-05]
# Why rows were left out, counted per reason. A single "non-finite or
# non-positive rate/exposure" reported a missing loss as an exposure
# problem. `flags` is a named list of logical vectors, TRUE where the row
# fails that test; a row can count under more than one reason.
.drop_reasons <- function(flags) {
  why <- vapply(flags, function(f) sum(f, na.rm = TRUE), numeric(1))
  why <- why[why > 0]
  paste(sprintf("%s: %d", names(why), as.integer(why)), collapse = ", ")
}

# [RT-04]
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

# [RT-04]
# Should a whole-number column be listed value by value? A tariff wants
# every age, not every second age. The decision is on the values, never on
# a step of 1 across the range: a coded column such as a sum insured can
# hold five values spread over a million, and stepping through that one
# unit at a time would build a grid of a million rows.
.list_values <- function(uv, n, integer_data)
  integer_data && length(uv) && length(uv) <= 2 * max(1, n)

# [RT-04]
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

# [RT-09]
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

# [RT-08]
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

# [MV-06]
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
    md <- .model_data(m)
    if (!is.null(md) && pv %in% names(md) && is.numeric(md[[pv]]))
      return("cont")
  }
  x <- src[[pv]]
  if (is.numeric(x) && data.table::uniqueN(x, na.rm = TRUE) > 25) "cont"
  else "cat"
}

# [MV-03] [MV-10]
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
    # the data the model was fitted on, also for glm.nb() [DATA-05]
    src <- .as_df(.model_data(m))
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
    # [MV-02]
    dup <- unique(pr$p[duplicated(pr$p)])
    if (length(dup))
      stop(fn, ": '", a, "' is not a regrouping of '", pv, "' in the data ",
           "the ", nm, " model was fitted on: the value(s) ",
           paste(utils::head(dup, 5), collapse = ", "),
           " occur there with more than one group.", call. = FALSE)
    maps[[nm]] <- list(map = stats::setNames(pr$g, pr$p), src = src)
  }
  if (!length(maps)) return(data[[a]])     # nothing to check it against

  # [MV-04]
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

  # [MV-07]
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
  # [MV-05]
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

# [RT-10]
# Rows of a prediction frame a model can be asked about: none of its factor
# terms holds a value the model was not fitted on.
.predictable_rows <- function(model, newd) {
  un <- .unseen_levels(model, newd)
  if (!length(un)) return(rep(TRUE, nrow(newd)))
  !Reduce(`|`, lapply(un, `[[`, "rows"))
}

# [DIAG-06]
# Mean and variance of each row's deviance residual under the fitted model.
# A deviance residual is not centred: for a Poisson count with a small mean
# it is negative on average (a claim-free row has d = -sqrt(2 mu)), so the
# binned mean of a correct model sits below zero, and with a band of
# 2 sqrt(phi / n) every bin falls outside it. Measured on a correct Poisson
# book of 60,000 rows: mean binned residual -0.220 against a band of 0.058,
# 50 of 50 bins outside. The moments depend on one parameter per family,
# so they are computed exactly on a grid of it and interpolated. Returns
# NULL for a family without a closed form here.
.dev_resid_moments <- function(model) {
  fam <- family(model)
  fname <- fam$family
  mu  <- as.numeric(stats::fitted(model))
  w   <- as.numeric(model$prior.weights)
  if (length(w) != length(mu)) w <- rep(1, length(mu))
  phi <- sum(stats::residuals(model, type = "pearson")^2) / model$df.residual

  on_grid <- function(par, fun, n_grid = 200) {
    lo <- min(par); hi <- max(par)
    g  <- if (hi / lo < 1 + 1e-9) lo else
      exp(seq(log(lo), log(hi), length.out = n_grid))
    m  <- matrix(vapply(g, fun, numeric(2)), nrow = 2)
    if (length(g) == 1)
      return(list(mean = rep(m[1, 1], length(par)),
                  var  = rep(m[2, 1], length(par))))
    list(mean = stats::approx(log(g), m[1, ], log(par), rule = 2)$y,
         var  = stats::approx(log(g), m[2, ], log(par), rule = 2)$y)
  }
  moments <- function(d, p) {
    m1 <- sum(p * d)
    c(m1, max(sum(p * d^2) - m1^2, 0))
  }

  if (fname %in% c("poisson", "quasipoisson")) {
    lam <- pmax(mu * w, 1e-12)
    out <- on_grid(lam, function(l) {
      y <- 0:(stats::qpois(1 - 1e-12, l) + 5)
      d <- sign(y - l) * sqrt(pmax(2 * (ifelse(y > 0, y * log(y / l), 0) -
                                          (y - l)), 0))
      moments(d, stats::dpois(y, l))
    })
    out$var <- out$var * phi
    return(out)
  }
  th <- .nb_theta(model)
  if (!is.null(th) && all(abs(w - 1) < 1e-12)) {
    out <- on_grid(pmax(mu, 1e-12), function(m) {
      y <- 0:(stats::qnbinom(1 - 1e-12, size = th, mu = m) + 5)
      d <- sign(y - m) * sqrt(pmax(2 * (ifelse(y > 0, y * log(y / m), 0) -
                                          (y + th) * log((y + th) / (m + th))),
                                   0))
      moments(d, stats::dnbinom(y, size = th, mu = m))
    })
    out$var <- out$var * phi
    return(out)
  }
  if (identical(fname, "binomial")) {
    mm <- pmax(round(w), 1)
    if (length(unique(mm)) > 20) return(NULL)
    mean_ <- var_ <- numeric(length(mu))
    for (m in unique(mm)) {
      i <- mm == m
      k <- 0:m
      r <- on_grid(pmin(pmax(mu[i], 1e-9), 1 - 1e-9), function(p) {
        yk <- k / m
        d  <- sign(yk - p) * sqrt(pmax(2 * m * (
          ifelse(yk > 0, yk * log(yk / p), 0) +
          ifelse(yk < 1, (1 - yk) * log((1 - yk) / (1 - p)), 0)), 0))
        moments(d, stats::dbinom(k, m, p))
      })
      mean_[i] <- r$mean; var_[i] <- r$var
    }
    return(list(mean = mean_, var = var_ * phi))
  }
  if (identical(fname, "Gamma")) {
    # y / mu ~ Gamma(shape = w / phi, rate = w / phi), so the moments depend
    # on the prior weight alone: E[d^2] has a closed form, E[d] is integrated
    gam <- function(wt) {
      k  <- wt / phi
      f  <- function(r) sign(r - 1) * sqrt(pmax(2 * wt * (r - 1 - log(r)), 0)) *
        stats::dgamma(r, shape = k, rate = k)
      e1 <- tryCatch(stats::integrate(f, 0, 1)$value +
                       stats::integrate(f, 1, Inf)$value,
                     error = function(e) NA_real_)
      e2 <- -2 * wt * (digamma(k) - log(k))
      c(e1, max(e2 - e1^2, 0))
    }
    uw <- unique(w)
    if (length(uw) <= 50) {
      m <- vapply(uw, gam, numeric(2))
      idx <- match(w, uw)
      return(list(mean = m[1, idx], var = m[2, idx]))
    }
    return(on_grid(pmax(w, 1e-12), gam, n_grid = 60))
  }
  if (identical(fname, "gaussian"))
    return(list(mean = rep(0, length(mu)), var = rep(phi, length(mu))))
  NULL
}

# [DIAG-08]
# Theta of a negative binomial fit, or NULL. MASS::glm.nb() stores it as
# model$theta; a glm() with family = MASS::negative.binomial(theta) only
# has it in the family's environment, which is where MASS keeps it as well.
.nb_theta <- function(model) {
  fam <- family(model)
  if (!grepl("^Negative Binomial", fam$family)) return(NULL)
  th <- if (is.numeric(model$theta)) model$theta else
    tryCatch(get(".Theta", envir = environment(fam$variance),
                 inherits = FALSE), error = function(e) NULL)
  if (is.numeric(th) && length(th) == 1 && is.finite(th) && th > 0) th
  else NULL
}

# [RT-13]
# Claims without payment. A record with claims but no loss is counted by a
# frequency model on the claim count, while a Gamma severity cannot take a
# zero, so the zero amounts leave the severity data and frequency x severity
# overstates the risk premium by their share. Measured with 15% nil claims:
# predicted over actual losses 1.153. Only records whose every claim is nil
# are visible here, so the share reported is a lower bound.
.nil_claims_warning <- function(model_freq, model_sev, data, claims_col,
                                loss_col, fn) {
  if (is.null(model_freq) || is.null(model_sev)) return(invisible(NULL))
  if (!all(c(claims_col, loss_col) %in% names(data))) return(invisible(NULL))
  if (!claims_col %in% all.vars(formula(model_freq)[[2]]))
    return(invisible(NULL))
  cl  <- suppressWarnings(as.numeric(data[[claims_col]]))
  ls  <- suppressWarnings(as.numeric(data[[loss_col]]))
  tot <- sum(cl, na.rm = TRUE)
  if (!isTRUE(tot > 0)) return(invisible(NULL))
  nil <- !is.na(cl) & !is.na(ls) & cl > 0 & ls == 0
  share <- sum(cl[nil]) / tot
  if (share >= 0.01)
    warning(fn, ": at least ", sprintf("%.1f%%", 100 * share), " of the ",
            "claims in '", claims_col, "' sit in records without payment ('",
            loss_col, "' = 0). The frequency model counts them, while a ",
            "severity model fitted on positive amounts never predicts a zero, ",
            "so frequency x severity overstates the risk premium by about ",
            "that share. Count only paid claims in the frequency model, or ",
            "model the share of claims without payment separately.",
            call. = FALSE)
  invisible(NULL)
}

# [RATE-10]
# With the accounting year as a rating factor every policy is priced at the
# level of its own year. Against a current premium that puts inflation into
# the dislocation: measured, a rate-level change of +11.1% on the historic
# years against +34.7% with every policy at the tariff year. Two model sets
# that both use the year price each policy at the same year on both sides,
# which is like for like, so the warning is for a premium column or for a
# year on one side only. `old_models` is NULL for a premium column.
.year_warning <- function(new_models, old_models, year_col, at, fn) {
  if (is.null(year_col) || (!is.null(at) && year_col %in% names(at)))
    return(invisible(NULL))
  uses <- function(ms) any(vapply(Filter(Negate(is.null), ms), function(m)
    year_col %in% .rhs_vars(m), logical(1)))
  u_new <- uses(new_models)
  u_old <- !is.null(old_models) && uses(old_models)
  against <- if (is.null(old_models)) "a current premium"
             else "a tariff without the year"
  if ((u_new && !u_old) || (u_old && !u_new))
    warning(fn, ": the ", if (u_new) "new" else "old", " models use '",
            year_col, "' as a rating factor, so every policy is priced at ",
            "the level of its own year. Against ", against, " that mixes ",
            "inflation into the comparison; price every policy at the tariff ",
            "year with at = list(", year_col, " = <year>).", call. = FALSE)
  invisible(NULL)
}
