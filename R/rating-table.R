#' Build a multiplicative rating table from frequency/severity models
#'
#' Builds a table of multiplicative rating factors from a frequency and/or
#' severity model (both log link), including two-way interactions.
#'
#' Base convention: the base level is the reference level for categorical
#' variables (first xlevel, or the level with the largest exposure when
#' `base_level = "exposure"`), the median for continuous variables (added
#' as an explicit grid point, `IsBase = TRUE`), and 1 for the exposure
#' column. As a result, for log-link models without interactions the
#' identity holds exactly: intercept x product(factors) = model prediction
#' per exposure unit. A variable that is absent from one of the two models
#' gets the neutral factor 1 there (so `Factor_Premium` remains correct).
#'
#' Interaction rows contain the joint relativity relative to the base cell
#' and the pure uplift: joint / (main_factor_x * main_factor_group). They
#' carry exposure and claim counts per cell whatever the types of the two
#' variables: a continuous variable is binned onto its own grid first, the
#' same way the one-way exposure histogram is built. Interaction cells are
#' where a portfolio runs out of data, so `IsThin` matters most there.
#'
#' Levels backed by fewer than `min_claims` claims get `IsThin = TRUE`
#' (thin categorical levels also raise a warning) and are dimmed in
#' [make_rating_plot()].
#'
#' @param model_freq,model_sev Fitted glm objects (at least one; log link
#'   expected, a warning is raised otherwise).
#' @param data The original dataset, used for base values, grids and
#'   exposure.
#' @param grid_res Number of grid points for continuous variables
#'   (default 50).
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param claims_col Claim count column (default `"AantalClaims"`).
#' @param base_level `"first"` = first xlevel as reference (default);
#'   `"exposure"` = level with the largest exposure as reference.
#' @param trim Quantiles for the continuous grid, e.g. `c(0.005, 0.995)` to
#'   avoid outliers/extrapolation in the tails (default `c(0, 1)` = full
#'   range).
#' @param grid_step Step size for continuous grids. `NULL` (default)
#'   picks a readable step per variable: roughly `grid_res` points across
#'   the range, rounded to a 1 / 2 / 5 times a power of ten, and never
#'   fractional for a column that holds whole numbers. So an age runs 18,
#'   19, 20 and a vehicle weight 800, 850, 900 rather than 19.2653 and
#'   832.65. A whole-number column with at most `2 * grid_res` distinct
#'   values is listed on those values instead of on a step, which is what
#'   gives an age per year and keeps a coded column (five sums insured
#'   spread over a million) to five rows. Override it with a single number
#'   to force one step
#'   everywhere, or with a named vector or list to set steps per variable:
#'   `list(LEEFTIJD = 1, GEWICHT = 100)`. Naming only some variables is
#'   fine, the rest keep the automatic step; a name that matches no
#'   continuous variable is reported rather than ignored. The base point
#'   is snapped onto the grid, so the row with factor 1 is a value you
#'   would actually quote.
#' @param min_claims Thin-cell threshold: levels/grid points backed by
#'   fewer claims get `IsThin = TRUE` (default 30). A GLM applies no
#'   shrinkage to a categorical level, so such a factor comes essentially
#'   from those few claims; it is dimmed by [make_rating_plot()] and
#'   greyed out by [export_rating_table()]. Thin categorical levels also
#'   raise a warning.
#'
#' @return A data.frame with one row per level/grid point per variable
#'   (columns `Variable`, `Type`, `Level`, `LevelNum`, `Group`, `IsBase`,
#'   `Exposure`, `ClaimCount`, `Factor_*`, `Uplift_*`, `IsThin`, `XVar`,
#'   `GroupVar`) and attributes `intercept_frequency`,
#'   `intercept_severity`, `intercept_premium` (predictions at the base
#'   point, per unit of exposure) and `base_values` (named list with the
#'   base value per variable).
#' @export
make_rating_table <- function(model_freq = NULL,
                              model_sev  = NULL,
                              data,
                              grid_res   = 50,
                              exposure_col = "Exposure",
                              claims_col   = "AantalClaims",
                              base_level   = c("first", "exposure"),
                              trim         = c(0, 1),
                              min_claims   = 30,
                              grid_step    = NULL) {

  base_level <- match.arg(base_level)
  if (is.null(model_freq) && is.null(model_sev)) {
    stop("Provide at least one model.")
  }
  .check_cols(data, c(exposure_col, claims_col), "make_rating_table")
  data <- .as_df(data)
  if (length(trim) != 2 || any(trim < 0) || any(trim > 1) || trim[1] >= trim[2])
    stop("make_rating_table: 'trim' must be two quantiles with trim[1] < trim[2].")
  if (min_claims < 0)
    stop("make_rating_table: 'min_claims' must not be negative.")

  # Multiplicative factors are only valid for log-link models
  check_link <- function(model, model_name) {
    if (is.null(model)) return(invisible(NULL))
    l <- family(model)$link
    if (!identical(l, "log"))
      warning("make_rating_table: model '", model_name, "' has link '", l,
              "' (not a log link); the factors are then not pure ",
              "multiplicative relativities.", call. = FALSE)
  }
  check_link(model_freq, "frequency")
  check_link(model_sev,  "severity")

  # Every offset variable is held at 1 in the base row, so the intercept is
  # per unit of whatever the model actually offsets on. If that is not
  # `exposure_col`, the Exposure column of the table measures something
  # else than the model does, which is worth saying out loud.
  offset_vars <- unique(c(.offset_vars(model_freq), .offset_vars(model_sev)))
  if (length(offset_vars) && !exposure_col %in% offset_vars)
    warning("make_rating_table: the model offsets on '",
            paste(offset_vars, collapse = ", "), "' while exposure_col = '",
            exposure_col, "'. The intercepts are per unit of the offset ",
            "variable, but the Exposure and volume columns are summed from '",
            exposure_col, "'.", call. = FALSE)

  # Helpers -------------------------------------------------------------------
  # Underlying column of a term label (shared helper, bound to `data`)
  base_var <- function(term) .term_base_var(term, data)

  # xlevels per model, keyed by the underlying column name, so that inline
  # transformations such as factor(YEAR) are also recognised as categorical.
  xlev_map <- function(model) {
    if (is.null(model) || is.null(model$xlevels) || !length(model$xlevels))
      return(list())
    xl <- model$xlevels
    stats::setNames(xl, vapply(names(xl), base_var, character(1)))
  }
  xlev_all <- c(xlev_map(model_freq), xlev_map(model_sev))
  xlev_all <- xlev_all[!duplicated(names(xlev_all))]

  d_dt <- data.table::as.data.table(data)

  var_is_cat <- function(bv) !is.null(xlev_all[[bv]]) || is.factor(data[[bv]])
  var_levels <- function(bv) {
    if (!is.null(xlev_all[[bv]])) xlev_all[[bv]] else levels(factor(data[[bv]]))
  }

  # Base value per variable (raw value, not yet coerced):
  #   categorical -> reference level; continuous -> median; exposure -> 1
  base_value_raw <- function(bv) {
    # Any offset variable sits at 1, not just the one named by
    # exposure_col, so the intercept is a rate per unit of exposure
    # whatever that column happens to be called
    if (bv == exposure_col || bv %in% offset_vars) return(1)
    if (var_is_cat(bv)) {
      lv <- var_levels(bv)
      if (base_level == "exposure") {
        ag   <- d_dt[, .(E = sum(get(exposure_col), na.rm = TRUE)), by = bv]
        cand <- as.character(ag[[bv]])[order(-ag$E)]
        lev  <- cand[cand %in% lv][1]
        if (is.na(lev)) lev <- lv[1]
        lev
      } else {
        lv[1]
      }
    } else if (is.numeric(data[[bv]])) {
      grid_of(bv)$base
    } else {
      names(which.max(table(data[[bv]])))
    }
  }

  # Base row for a model: all predictors at their base value, with the
  # exposure column set to 1 so the intercept is a value per exposure unit.
  base_df_for <- function(model) {
    vars <- .rhs_vars(model)
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars))
      stop("variable(s) from the model formula not in 'data': ",
           paste(missing_vars, collapse = ", "))
    row <- lapply(vars, function(nm) .coerce_like(base_value_raw(nm), data[[nm]]))
    names(row) <- vars
    # check.names = FALSE, or a column such as "AUTO GEWICHT" is silently
    # renamed to "AUTO.GEWICHT" here and every predict() on this row fails
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }

  # Factors for one variable: prediction on grid / prediction on base row.
  # Both with all other predictors at their base value, hence consistent
  # with intercept_val. Variable not in the model -> neutral factor 1.
  predict_factor <- function(model, var_name, grid_vals) {
    n <- length(grid_vals)
    if (is.null(model)) return(rep(NA_real_, n))
    if (!var_name %in% .rhs_vars(model)) return(rep(1, n))
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    newd[[var_name]] <- .coerce_like(grid_vals, data[[var_name]])
    preds <- as.numeric(predict(model, newdata = newd,   type = "response"))
    ref   <- as.numeric(predict(model, newdata = base_df, type = "response"))
    preds / ref
  }

  # Joint relativity over a 2D grid (vx x vg), rest at base, scaled to the
  # base cell (index i_base). vx varies fastest.
  predict_factor_2d <- function(model, vx, gx, vg, gg, i_base) {
    n <- length(gx) * length(gg)
    if (is.null(model)) return(rep(NA_real_, n))
    vars    <- .rhs_vars(model)
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    if (vx %in% vars)
      newd[[vx]] <- .coerce_like(rep(gx, times = length(gg)), data[[vx]])
    if (vg %in% vars)
      newd[[vg]] <- .coerce_like(rep(gg, each  = length(gx)), data[[vg]])
    preds <- as.numeric(predict(model, newdata = newd, type = "response"))
    preds / preds[i_base]
  }

  # Grid and base point for one continuous variable, computed once and
  # reused, so the factors, the intercept and the interaction grids all
  # agree on where the base sits.
  grid_cache <- new.env(parent = emptyenv())
  grid_of <- function(bv, n_points = grid_res) {
    key <- paste0(bv, "|", n_points)
    if (!is.null(grid_cache[[key]])) return(grid_cache[[key]])
    x   <- data[[bv]]
    rng <- stats::quantile(x, probs = trim, na.rm = TRUE, names = FALSE)
    med <- stats::median(x, na.rm = TRUE)
    step <- if (!is.null(grid_step)) {
      s <- if (!is.null(names(grid_step)) && bv %in% names(grid_step))
        grid_step[[bv]] else if (is.null(names(grid_step))) grid_step[[1]]
      else NA_real_
      as.numeric(s)
    } else NA_real_
    int_data <- all(abs(x - round(x)) < 1e-9, na.rm = TRUE)
    inr <- x >= rng[1] & x <= rng[2]
    uv  <- sort(unique(x[inr %in% TRUE]))
    # A whole-number column with few enough distinct values is listed value
    # by value, using those values rather than a step of 1 across the
    # range: a coded column such as a sum insured can hold five values
    # spread over a million, where stepping by 1 would build a grid of a
    # million rows.
    if (!is.finite(step) && .list_values(uv, n_points, int_data)) {
      out <- list(values = uv,
                  base = uv[which.min(abs(uv - med))],
                  step = NA_real_)
      assign(key, out, envir = grid_cache)
      return(out)
    }
    if (!is.finite(step)) step <- .nice_step(rng, n_points, int_data)
    # snap the base onto the same grid, so the sequence stays readable and
    # the row with factor 1 is a value someone would actually quote
    base <- .snap_to_step(med, step, rng[1], rng[2])
    out <- list(values = .step_grid(rng[1], rng[2], step, base),
                base = base, step = step)
    assign(key, out, envir = grid_cache)
    out
  }

  # Which grid point does each policy belong to? Categorical: its level.
  # Continuous: the nearest grid value, splitting on the midpoints between
  # them, the same rule the main-effect exposure histogram uses. Returns
  # NA outside a trimmed range, so the volume of policies the grid does not
  # cover is not swept into the edge cells.
  cell_index <- function(bv, grid_vals, is_cat) {
    if (is_cat) return(match(as.character(data[[bv]]),
                             as.character(grid_vals)))
    x <- data[[bv]]
    brks <- c(-Inf, grid_vals[-1] - diff(grid_vals) / 2, Inf)
    i <- findInterval(x, brks)
    outside <- x < min(grid_vals) | x > max(grid_vals)
    if (!isTRUE(all.equal(trim, c(0, 1)))) i[outside %in% TRUE] <- NA_integer_
    i
  }

  # Base variables per model (interactions split up, deduplicated)
  model_bases <- function(model) {
    if (is.null(model)) return(character(0))
    labs  <- attr(terms(model), "term.labels")
    comps <- unique(unlist(strsplit(labs, ":", fixed = TRUE)))
    unique(vapply(comps, base_var, character(1)))
  }

  uniq_bases <- unique(c(model_bases(model_freq), model_bases(model_sev)))
  unknown    <- setdiff(uniq_bases, names(data))
  if (length(unknown)) {
    warning("make_rating_table: term(s) skipped because the base variable ",
            "is not in 'data': ", paste(unknown, collapse = ", "),
            call. = FALSE)
    uniq_bases <- intersect(uniq_bases, names(data))
  }
  if (!length(uniq_bases))
    stop("make_rating_table: no usable variables found.")

  # A misspelled name in grid_step would otherwise be ignored in silence,
  # leaving the user convinced they had set a step they had not
  if (!is.null(grid_step) && !is.null(names(grid_step))) {
    named <- names(grid_step)[nzchar(names(grid_step))]
    cont  <- Filter(function(b) !var_is_cat(b) && is.numeric(data[[b]]),
                    uniq_bases)
    unused <- setdiff(named, cont)
    if (length(unused))
      warning("make_rating_table: 'grid_step' names with no continuous ",
              "variable to apply to, and therefore ignored: ",
              paste(unused, collapse = ", "),
              ". Continuous variables in this model: ",
              if (length(cont)) paste(cont, collapse = ", ") else "none",
              ".", call. = FALSE)
  }

  # Two-way interaction pairs (base variables) from both models, deduplicated
  gather_pairs <- function() {
    labs <- unique(c(if (!is.null(model_freq)) attr(terms(model_freq), "term.labels"),
                     if (!is.null(model_sev))  attr(terms(model_sev),  "term.labels")))
    ints <- labs[grepl(":", labs, fixed = TRUE)]
    seen <- character(0); res <- list()
    for (lab in ints) {
      comps <- strsplit(lab, ":", fixed = TRUE)[[1]]
      if (length(comps) != 2) next                       # two-way only
      bvs <- unname(vapply(comps, base_var, character(1)))
      key <- paste(sort(bvs), collapse = ":")
      if (key %in% seen) next
      seen <- c(seen, key); res[[length(res) + 1L]] <- bvs
    }
    res
  }

  # Build the rows per base variable ------------------------------------------
  rows <- lapply(uniq_bases, function(bv) tryCatch({

    is_cat   <- var_is_cat(bv)
    base_raw <- base_value_raw(bv)

    # Grid: categorical = all levels; continuous = (trimmed) range with the
    # median as an explicit extra grid point so there is a row with factor 1
    if (is_cat) {
      grid_vals <- var_levels(bv)
      is_base   <- grid_vals == as.character(base_raw)
    } else {
      grid_vals <- grid_of(bv)$values
      is_base   <- grid_vals == base_raw
    }

    # Exposure / claims aggregation
    if (is_cat) {
      ag <- d_dt[, .(Exposure   = sum(get(exposure_col), na.rm = TRUE),
                     ClaimCount = sum(get(claims_col),   na.rm = TRUE)),
                 by = bv]
      data.table::setnames(ag, bv, "Level")
      ag$Level <- as.character(ag$Level)
      exposure    <- ag$Exposure[match(as.character(grid_vals), ag$Level)]
      claim_count <- ag$ClaimCount[match(as.character(grid_vals), ag$Level)]
      exposure[is.na(exposure)]       <- 0
      claim_count[is.na(claim_count)] <- 0
    } else {
      # Continuous variables: exposure histogram via binning on the grid.
      # Values outside a trimmed range are excluded rather than swept into
      # the edge bins, which would make the outer bars overstate their
      # volume: the grid does not cover those policies, so neither should
      # the bars. The trimmed exposure is therefore below the portfolio
      # total by design.
      ag   <- data.table::data.table(
        bin        = cell_index(bv, grid_vals, FALSE),
        Exposure   = data[[exposure_col]],
        ClaimCount = data[[claims_col]]
      )[, .(Exposure   = sum(Exposure,   na.rm = TRUE),
            ClaimCount = sum(ClaimCount, na.rm = TRUE)),
        by = bin]
      exposure    <- ag$Exposure[match(seq_along(grid_vals),   ag$bin)]
      claim_count <- ag$ClaimCount[match(seq_along(grid_vals), ag$bin)]
      exposure[is.na(exposure)]       <- 0
      claim_count[is.na(claim_count)] <- 0
    }

    # Retrieve factors per model
    f_freq <- predict_factor(model_freq, bv, grid_vals)
    f_sev  <- predict_factor(model_sev,  bv, grid_vals)

    data.frame(
      Variable         = bv,
      Type             = if (is_cat) "categorical" else "continuous",
      Level            = .level_chr(grid_vals),
      LevelNum         = suppressWarnings(as.numeric(as.character(grid_vals))),
      Group            = NA_character_,
      IsBase           = is_base,
      Exposure         = exposure,
      ClaimCount       = claim_count,
      Factor_Frequency = f_freq,
      Factor_Severity  = f_sev,
      Factor_Premium   = f_freq * f_sev,
      Uplift_Frequency = NA_real_,
      Uplift_Severity  = NA_real_,
      Uplift_Premium   = NA_real_,
      XVar             = bv,
      GroupVar         = NA_character_,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning("make_rating_table: variable '", bv, "' skipped: ",
            conditionMessage(e), call. = FALSE)
    NULL
  }))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows))
    stop("make_rating_table: none of the variables could be processed.")

  out <- do.call(rbind, rows)

  # Build the (two-way) interaction rows ---------------------------------------
  # Per pair: one variable becomes the X-axis, the other the "Group". The
  # factor is the joint relativity relative to the base cell (both at
  # base); the uplift is joint / (main_factor_x * main_factor_group).
  int_rows <- lapply(gather_pairs(), function(bvs) tryCatch({
    b1 <- bvs[1]; b2 <- bvs[2]
    if (!all(c(b1, b2) %in% names(data)))
      stop("base variable not in 'data'")
    c1 <- var_is_cat(b1); c2 <- var_is_cat(b2)

    # X = continuous when exactly one is continuous; both equal: most levels = X
    if (c1 != c2) {
      if (c1) { xv <- b2; gv <- b1 } else { xv <- b1; gv <- b2 }
    } else if (c1 && c2) {
      if (length(var_levels(b2)) > length(var_levels(b1))) { xv <- b2; gv <- b1 }
      else                                                 { xv <- b1; gv <- b2 }
    } else { xv <- b1; gv <- b2 }
    cx <- var_is_cat(xv); cg <- var_is_cat(gv)

    base_x <- base_value_raw(xv)
    base_g <- base_value_raw(gv)

    gx <- if (cx) var_levels(xv) else grid_of(xv)$values
    # Continuous group variable: 3 slices = q10, median (= base), q90
    gg <- if (cg) var_levels(gv) else {
      sort(unique(c(round(as.numeric(stats::quantile(data[[gv]], c(0.1, 0.9),
                                                     na.rm = TRUE)), 3),
                    base_g)))
    }

    nx <- length(gx); ng <- length(gg)
    ix <- if (cx) match(as.character(base_x), as.character(gx)) else which(gx == base_x)[1]
    ig <- if (cg) match(as.character(base_g), as.character(gg)) else which(gg == base_g)[1]
    if (is.na(ix) || is.na(ig)) stop("base cell not found in the grid")
    i_base <- (ig - 1L) * nx + ix

    xvals <- rep(gx, times = ng)        # X varies fastest (= predict_factor_2d)
    gvals <- rep(gg, each  = nx)

    # Exposure and claims per cell. A continuous variable is binned onto its
    # own grid first (same midpoint rule as the main-effect histogram), so
    # the volume columns are filled for every combination of types rather
    # than only for categorical x categorical. Interaction cells are exactly
    # where a portfolio runs out of data, so this is the place the thin-cell
    # flag earns its keep.
    ci <- cell_index(xv, gx, cx)
    cj <- cell_index(gv, gg, cg)
    ag <- data.table::data.table(
      ci = ci, cj = cj,
      E  = data[[exposure_col]],
      C  = data[[claims_col]]
    )[!is.na(ci) & !is.na(cj),
      .(E = sum(E, na.rm = TRUE), C = sum(C, na.rm = TRUE)), by = .(ci, cj)]
    lin  <- (ag$cj - 1L) * nx + ag$ci          # X varies fastest
    expo <- rep(0, nx * ng); clm <- rep(0, nx * ng)
    expo[lin] <- ag$E
    clm[lin]  <- ag$C

    ff <- predict_factor_2d(model_freq, xv, gx, gv, gg, i_base)
    fs <- predict_factor_2d(model_sev,  xv, gx, gv, gg, i_base)

    # Pure interaction uplift: joint / (main_factor_x * main_factor_group)
    uf <- ff / (rep(predict_factor(model_freq, xv, gx), times = ng) *
                rep(predict_factor(model_freq, gv, gg), each  = nx))
    us <- fs / (rep(predict_factor(model_sev,  xv, gx), times = ng) *
                rep(predict_factor(model_sev,  gv, gg), each  = nx))

    data.frame(
      Variable         = paste(b1, b2, sep = ":"),
      Type             = if (cx) "categorical" else "continuous",
      Level            = .level_chr(xvals),
      LevelNum         = suppressWarnings(as.numeric(as.character(xvals))),
      Group            = .level_chr(gvals),
      IsBase           = seq_len(nx * ng) == i_base,
      Exposure         = expo,
      ClaimCount       = clm,
      Factor_Frequency = ff,
      Factor_Severity  = fs,
      Factor_Premium   = ff * fs,
      Uplift_Frequency = uf,
      Uplift_Severity  = us,
      Uplift_Premium   = uf * us,
      XVar             = xv,
      GroupVar         = gv,
      stringsAsFactors = FALSE
    )
  }, error = function(e) {
    warning("Interaction ", paste(bvs, collapse = ":"), " skipped: ",
            conditionMessage(e), call. = FALSE)
    NULL
  }))
  int_rows <- Filter(Negate(is.null), int_rows)
  if (length(int_rows)) out <- rbind(out, do.call(rbind, int_rows))

  # Thin-cell flag -------------------------------------------------------------
  # Levels backed by very few claims: the GLM applies no shrinkage to a
  # categorical level, so its factor comes essentially from those few
  # claims and is correspondingly unstable. Flagged here, dimmed by
  # make_rating_plot() and greyed out by export_rating_table().
  out$IsThin <- ifelse(is.na(out$ClaimCount), NA, out$ClaimCount < min_claims)

  # Warn for thin categorical main-effect levels (thin tail bins are
  # normal for continuous grids and would make the warning noisy)
  n_thin <- sum(out$IsThin & is.na(out$Group) & out$Type == "categorical",
                na.rm = TRUE)
  if (n_thin > 0)
    warning("make_rating_table: ", n_thin, " categorical level(s) have ",
            "fewer than ", min_claims, " claims; the corresponding factors ",
            "rest on very little experience (see the IsThin column).",
            call. = FALSE)

  # Intercepts (base values on the response scale, exposure = 1) ---------------
  # The factors are ratios of two predictions that share the same base row,
  # so whatever the offset contributes cancels there. The intercept is the
  # one number that carries it, so it is the one that has to have the
  # offset taken back out on the link scale rather than trusted to vanish
  # because a column was set to 1 - which only works for offset(log(x)).
  intercept_val <- function(model) {
    if (is.null(model)) return(NA_real_)
    as.numeric(.predict_no_offset(model, base_df_for(model)))
  }

  i_freq <- intercept_val(model_freq)
  i_sev  <- intercept_val(model_sev)

  attr(out, "intercept_frequency") <- i_freq
  attr(out, "intercept_severity")  <- i_sev
  attr(out, "intercept_premium")   <- if (!is.na(i_freq) && !is.na(i_sev))
    i_freq * i_sev else NA_real_
  attr(out, "base_values") <- stats::setNames(
    lapply(uniq_bases, base_value_raw), uniq_bases)

  rownames(out) <- NULL
  out
}
