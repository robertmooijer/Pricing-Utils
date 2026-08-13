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

# Training data (original columns) aligned with the fitted rows of a glm.
# Uses rownames matching instead of as.integer(rownames), so it also works
# with non-numeric rownames and rows dropped by na.action.
.glm_training_data <- function(model, fn = "pricingtoolsRmO") {
  mf <- stats::model.frame(model)
  d  <- model$data
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
