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

  if (has_offset && identical(family(model)$link, "log")) {
    c(tr, list(actual = y, expected = mu, exposure = exp(tr$offset),
               claims = y, exposure_label = "Exposure", counts = TRUE))
  } else {
    w <- tr$weights
    c(tr, list(actual = w * y, expected = w * mu, exposure = w,
               claims = w, exposure_label = "Weight", counts = FALSE))
  }
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

# Variables appearing in the model's offset term, e.g. "Exposure" in
# offset(log(Exposure)); never candidates for an interaction scan.
.offset_vars <- function(model) {
  tt  <- terms(model)
  idx <- attr(tt, "offset")
  if (is.null(idx) || !length(idx)) return(character(0))
  vars <- attr(tt, "variables")
  unique(unlist(lapply(idx, function(i) all.vars(vars[[i + 1L]]))))
}

# Underlying column of a term label: "ns(AGE, 4)" -> "AGE"
.term_base_var <- function(term, data) {
  if (term %in% names(data)) return(term)
  m <- regmatches(term, regexpr("\\(([^,\\)]+)", term))
  if (length(m) == 0) return(term)
  gsub("[()]", "", m)
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
