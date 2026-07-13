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
#' and the pure uplift: joint / (main_factor_x * main_factor_group).
#'
#' The `Credibility` column contains the limited-fluctuation (square-root)
#' credibility of each level's own experience:
#' `min(1, sqrt(ClaimCount / full_cred_claims))`. Levels with fewer than
#' `min_claims` claims get `IsThin = TRUE` (thin categorical levels also
#' raise a warning) and are dimmed in [make_rating_plot()].
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
#' @param min_claims Thin-cell threshold: levels/grid points with fewer
#'   claims get `IsThin = TRUE` (default 30).
#' @param full_cred_claims Full-credibility claim standard for the
#'   `Credibility` column (default 1082 = observed frequency within 5
#'   percent of the true value with 90 percent confidence).
#'
#' @return A data.frame with one row per level/grid point per variable
#'   (columns `Variable`, `Type`, `Level`, `LevelNum`, `Group`, `IsBase`,
#'   `Exposure`, `ClaimCount`, `Factor_*`, `Uplift_*`, `Credibility`,
#'   `IsThin`, `XVar`, `GroupVar`) and attributes `intercept_frequency`,
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
                              full_cred_claims = 1082) {

  base_level <- match.arg(base_level)
  if (is.null(model_freq) && is.null(model_sev)) {
    stop("Provide at least one model.")
  }
  .check_cols(data, c(exposure_col, claims_col), "make_rating_table")
  if (length(trim) != 2 || any(trim < 0) || any(trim > 1) || trim[1] >= trim[2])
    stop("make_rating_table: 'trim' must be two quantiles with trim[1] < trim[2].")
  if (min_claims < 0 || full_cred_claims <= 0)
    stop("make_rating_table: 'min_claims' and 'full_cred_claims' must be positive.")

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
    if (bv == exposure_col) return(1)
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
      stats::median(data[[bv]], na.rm = TRUE)
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
    as.data.frame(row, stringsAsFactors = FALSE)
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
      rng       <- stats::quantile(data[[bv]], probs = trim,
                                   na.rm = TRUE, names = FALSE)
      grid_vals <- sort(unique(c(seq(rng[1], rng[2], length.out = grid_res),
                                 base_raw)))
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
      # Continuous variables: exposure histogram via binning on the grid
      brks <- c(-Inf, grid_vals[-1] - diff(grid_vals)/2, Inf)
      bin  <- findInterval(data[[bv]], brks)
      ag   <- data.table::data.table(
        bin        = bin,
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
      Level            = as.character(grid_vals),
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

    gx <- if (cx) var_levels(xv) else {
      rng <- stats::quantile(data[[xv]], probs = trim, na.rm = TRUE, names = FALSE)
      sort(unique(c(seq(rng[1], rng[2], length.out = grid_res), base_x)))
    }
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

    # Exposure/claims per cell: only meaningful for categorical x categorical
    expo <- rep(NA_real_, nx * ng); clm <- rep(NA_real_, nx * ng)
    if (cx && cg) {
      ag <- d_dt[, .(E = sum(get(exposure_col), na.rm = TRUE),
                     C = sum(get(claims_col),   na.rm = TRUE)), by = c(xv, gv)]
      k  <- paste(as.character(ag[[xv]]), as.character(ag[[gv]]), sep = "\r")
      ck <- paste(as.character(xvals),    as.character(gvals),    sep = "\r")
      expo <- ag$E[match(ck, k)]; expo[is.na(expo)] <- 0
      clm  <- ag$C[match(ck, k)]; clm[is.na(clm)]   <- 0
    }

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
      Level            = as.character(xvals),
      LevelNum         = suppressWarnings(as.numeric(as.character(xvals))),
      Group            = as.character(gvals),
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

  # Credibility / thin-cell flags ----------------------------------------------
  # Limited-fluctuation (square-root) credibility on the claim count per
  # level: Z = min(1, sqrt(claims / full_cred_claims)). Z quantifies how
  # much standalone experience backs a level; the GLM factor itself
  # already pools information across the portfolio, so a low Z does not
  # invalidate the factor but does mean it leans heavily on the model
  # structure rather than on that level's own data.
  out$Credibility <- ifelse(is.na(out$ClaimCount), NA_real_,
                            pmin(1, sqrt(out$ClaimCount / full_cred_claims)))
  out$IsThin <- ifelse(is.na(out$ClaimCount), NA, out$ClaimCount < min_claims)

  # Warn for thin categorical main-effect levels (thin tail bins are
  # normal for continuous grids and would make the warning noisy)
  n_thin <- sum(out$IsThin & is.na(out$Group) & out$Type == "categorical",
                na.rm = TRUE)
  if (n_thin > 0)
    warning("make_rating_table: ", n_thin, " categorical level(s) have ",
            "fewer than ", min_claims, " claims; the corresponding factors ",
            "rest on little standalone experience (see IsThin/Credibility).",
            call. = FALSE)

  # Intercepts (base values on the response scale, exposure = 1) ---------------
  intercept_val <- function(model) {
    if (is.null(model)) return(NA_real_)
    as.numeric(predict(model, newdata = base_df_for(model), type = "response"))
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
