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
#' @param base Optional: choose the base (factor 1) per variable, as a
#'   named list or vector, e.g. `list(REGIO = "Zuid", LEEFTIJD = 40)`.
#'   `NULL` (default) keeps the rules above. A categorical base must be an
#'   existing level and overrides `base_level` for that variable, without
#'   refitting the model. A continuous base is placed on the grid exactly
#'   as given rather than snapped, with a warning if it lies outside the
#'   data. Variables not named keep their default base; offset and exposure
#'   variables cannot be named, since their base of 1 is what makes the
#'   intercept a rate per unit of exposure. The choice only moves the
#'   factors and the intercept against each other: every premium stays the
#'   same.
#' @param merge_vars Optional: declare that two columns are one rating
#'   factor, as a named list or vector keyed by the variable to keep,
#'   e.g. `list(Stedelijkheid = "Stedelijkheid_groep")`.
#'
#'   This is for the case where the severity model uses a regrouped
#'   version of a frequency variable: urbanisation 1 to 5 in the
#'   frequency model, and 1, 2, 3, 4-5 in the severity model because the
#'   top two classes showed the same average claim. Without it the table
#'   is still correct, but it holds two blocks for one tariff field, each
#'   padded with neutral factors of 1, and both have to be applied to
#'   arrive at the premium. With it there is one block, on the levels of
#'   the variable a policy actually carries, whose severity factor is the
#'   one belonging to the group that level falls in.
#'
#'   The regrouped column does not have to be in `data`. It is usually made
#'   next to the average claim, in the dataset the severity model is fitted
#'   on, while `data` is the whole portfolio. A fitted model carries its own
#'   data, so the relation is read off the model that uses the column and
#'   the column is derived for `data` here. Your data frame is not changed.
#'   A level that occurs in `data` but in no row of that model's data has no
#'   group to derive, which is an error naming those levels: a level with no
#'   claims has no severity factor either way.
#'
#'   One grouping per column name. A grouping is often made twice, once in
#'   each dataset, and the two need not agree: the frequency data may put
#'   brand C with D where the severity data puts it with A and B. Only one
#'   of the two can be in `data`, so the other model would be read on a
#'   grouping it was not fitted on. The mapping in `data` is therefore
#'   compared with the one each model's own data defines, and a difference
#'   is an error naming the value it turns on. When the two models really
#'   do group differently, give the columns separate names and name both:
#'   `list(MERK = c("MERK_GROEP_freq", "MERK_GROEP_sev"))`, which puts both
#'   groupings in one block on the brands themselves.
#'
#'   The regrouped column must be a function of the variable it regroups:
#'   every value maps to exactly one group, which is checked against
#'   `data`. A band of a continuous variable also works; a grid point that
#'   does not occur in the data takes the band of the nearest value that
#'   does. The merged column keeps no block of its own, cannot be named in
#'   `base`, and an interaction on it is reported under the name of the
#'   variable it is merged into. When the relation is there but not
#'   declared, and each column is used by one of the two models, this is
#'   pointed out in a warning rather than left to be discovered in a
#'   spreadsheet.
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
#'   `GroupVar`). The three `Uplift_*` columns are present **only when the
#'   model contains at least one interaction term**; without one they would
#'   be `NA` in every row, so they are dropped. `Group` and `GroupVar` are
#'   always there, because `is.na(Group)` is how a main-effect row is told
#'   from an interaction row. Attributes: `intercept_frequency`,
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
                              grid_step    = NULL,
                              base         = NULL,
                              merge_vars   = NULL) {

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

  # One rating factor under two column names --------------------------------
  # A variable can enter the frequency model under its own name and the
  # severity model as a regrouping of itself - urbanisation 1 to 5 against
  # 1, 2, 3, 4-5 - because the severity showed no difference between the
  # top two classes. Those are two columns but one tariff field, and
  # `merge_vars` says so, so the table reports one block keyed on the
  # column a policy actually carries.
  merge_map <- list()   # alias -> the variable it is a regrouping of
  alias_of  <- list()   # variable -> its aliases
  level_map <- list()   # alias -> lookup from the variable's value

  # The regrouped column often lives only in the dataset the severity model
  # was fitted on: it was made there, next to the average claim, and `data`
  # is the whole portfolio. A fitted glm carries its own data, so the
  # relation can be read off the model rather than asked of the user a
  # second time. Derived here, once, and from then on it is an ordinary
  # column of `data`.
  derive_alias <- function(pv, a) {
    owner <- NULL; owner_nm <- NULL
    for (mm in list(list(m = model_freq, nm = "frequency"),
                    list(m = model_sev,  nm = "severity")))
      if (!is.null(mm$m) && a %in% .rhs_vars(mm$m)) {
        owner <- mm$m; owner_nm <- mm$nm; break
      }
    if (is.null(owner))
      stop("make_rating_table: '", a, "' is not a column of 'data' and no ",
           "model uses it, so there is nothing to merge.", call. = FALSE)
    # A model fitted without data = carries its environment here, not a
    # table, and there is nothing to read a relation off
    src <- if (is.data.frame(owner$data)) .as_df(owner$data) else NULL
    if (is.null(src))
      stop("make_rating_table: '", a, "' is not a column of 'data'. It could ",
           "be taken from the data the ", owner_nm, " model was fitted on, ",
           "but that model carries none; fit it with a 'data =' argument, or ",
           "add the column to 'data' yourself.", call. = FALSE)
    if (!all(c(pv, a) %in% names(src)))
      stop("make_rating_table: '", a, "' is not a column of 'data', and the ",
           "data the ", owner_nm, " model was fitted on has no column '",
           if (!pv %in% names(src)) pv else a, "', so the relation between ",
           "the two cannot be read off anywhere. Add the column to 'data'.",
           call. = FALSE)

    ok <- !is.na(src[[pv]]) & !is.na(src[[a]])
    pr <- unique(data.frame(p = as.character(src[[pv]])[ok],
                            a = as.character(src[[a]])[ok],
                            stringsAsFactors = FALSE))
    dup <- unique(pr$p[duplicated(pr$p)])
    if (length(dup))
      stop("make_rating_table: '", a, "' is not a regrouping of '", pv,
           "' in the data the ", owner_nm, " model was fitted on: the ",
           "value(s) ", paste(utils::head(dup, 5), collapse = ", "),
           " occur there with more than one group.", call. = FALSE)

    x <- data[[pv]]
    if (!var_is_cat(pv) && is.numeric(x)) {
      # A band covers a run of values, so a value the model never saw takes
      # the band of the nearest one it did
      o    <- order(as.numeric(pr$p))
      num  <- as.numeric(pr$p)[o]; val <- pr$a[o]
      brks <- c(-Inf, num[-1] - diff(num) / 2, Inf)
      vals <- val[findInterval(as.numeric(x), brks)]
    } else {
      vals <- unname(stats::setNames(pr$a, pr$p)[as.character(x)])
      # A level with no claims at all is in no row of the severity data, so
      # it has no group there and there is nothing to assign it.
      miss <- unique(c(as.character(x)[is.na(vals) & !is.na(x)],
                       setdiff(var_levels(pv), pr$p)))
      if (length(miss))
        stop("make_rating_table: the value(s) ",
             paste(utils::head(miss, 5), collapse = ", "), " of '", pv,
             "' do not occur in the data the ", owner_nm, " model was fitted ",
             "on, so their group in '", a, "' is unknown. A level with no ",
             "claims has no group there: give it one in that model's data, ",
             "or put '", a, "' in 'data' yourself.", call. = FALSE)
    }
    tmpl <- src[[a]]
    if (is.factor(tmpl)) factor(vals, levels = levels(tmpl))
    else if (is.numeric(tmpl)) as.numeric(vals)
    else vals
  }

  if (!is.null(merge_vars)) {
    merge_vars <- as.list(merge_vars)
    if (is.null(names(merge_vars)) || any(!nzchar(names(merge_vars))))
      stop("make_rating_table: 'merge_vars' must be named by the variable ",
           "to keep, e.g. list(Stedelijkheid = \"Stedelijkheid_groep\").",
           call. = FALSE)
    for (pv in names(merge_vars)) {
      als <- as.character(unlist(merge_vars[[pv]]))
      .check_cols(data, pv, "make_rating_table")
      for (a in als) {
        if (identical(a, pv))
          stop("make_rating_table: 'merge_vars' maps '", pv, "' onto itself.",
               call. = FALSE)
        if (!is.null(merge_map[[a]]))
          stop("make_rating_table: '", a, "' is given as a regrouping of ",
               "both '", merge_map[[a]], "' and '", pv, "'.", call. = FALSE)
        if (!is.null(alias_of[[a]]))
          stop("make_rating_table: '", a, "' is both a regrouping of '", pv,
               "' and the variable '", paste(alias_of[[a]], collapse = ", "),
               "' is merged into; chains are not supported.", call. = FALSE)
        if (!a %in% names(data)) {
          data[[a]] <- derive_alias(pv, a)
          data.table::set(d_dt, j = a, value = data[[a]])
        }
        # The relation has to be a function of the variable: one group per
        # value. If a value occurs with two groups there is no single
        # factor to put on that row, and silently taking the first would
        # be wrong for part of the portfolio.
        ok <- !is.na(data[[pv]]) & !is.na(data[[a]])
        pr <- as.data.frame(unique(d_dt[ok %in% TRUE, c(pv, a),
                                        with = FALSE]))
        names(pr) <- c("p", "a")
        pr$p <- as.character(pr$p); pr$a <- as.character(pr$a)
        bad <- unique(pr$p[duplicated(pr$p)])
        if (length(bad))
          stop("make_rating_table: '", a, "' is not a regrouping of '", pv,
               "': the value(s) ", paste(utils::head(bad, 5), collapse = ", "),
               " of '", pv, "' occur with more than one value of '", a,
               "', so no single factor belongs to them.", call. = FALSE)
        # One column name, two meanings. A grouping is often made twice,
        # once in each dataset, and the two need not agree: the frequency
        # data may put brand C with D while the severity data puts it with
        # A and B. Only one of the two can go in `data`, so the other model
        # would be read on a grouping it was not fitted on. When the labels
        # differ that ends in a prediction failure, but when both groupings
        # happen to use the same labels nothing fails and the factors are
        # quietly wrong, which is why the two are compared here.
        for (ow in list(list(m = model_freq, nm = "frequency"),
                        list(m = model_sev,  nm = "severity"))) {
          if (is.null(ow$m) || !a %in% .rhs_vars(ow$m)) next
          sd <- if (is.data.frame(ow$m$data)) ow$m$data else NULL
          if (is.null(sd) || !all(c(pv, a) %in% names(sd))) next
          ok2 <- !is.na(sd[[pv]]) & !is.na(sd[[a]])
          mp  <- unique(data.frame(p = as.character(sd[[pv]])[ok2],
                                   a = as.character(sd[[a]])[ok2],
                                   stringsAsFactors = FALSE))
          mp  <- mp[!duplicated(mp$p), ]
          own <- stats::setNames(mp$a, mp$p)
          here <- stats::setNames(pr$a, pr$p)
          shared <- intersect(names(own), names(here))
          off <- shared[own[shared] != here[shared]]
          if (length(off))
            stop("make_rating_table: '", a, "' does not mean the same thing ",
                 "everywhere: '", off[1], "' of '", pv, "' is '",
                 here[[off[1]]], "' in 'data' and '", own[[off[1]]],
                 "' in the data the ", ow$nm, " model was fitted on",
                 if (length(off) > 1)
                   paste0(" (", length(off), " values differ)"),
                 ". Two groupings of one variable cannot share a column ",
                 "name: give them separate names and name both in ",
                 "'merge_vars', as merge_vars = list(", pv, " = c(\"", a,
                 "_freq\", \"", a, "_sev\")).", call. = FALSE)
        }
        if (!var_is_cat(pv) && is.numeric(data[[pv]])) {
          o <- order(as.numeric(pr$p))
          level_map[[a]] <- list(num = as.numeric(pr$p)[o], val = pr$a[o])
        } else {
          level_map[[a]] <- list(num = NULL,
                                 val = stats::setNames(pr$a, pr$p))
        }
        merge_map[[a]]  <- pv
        alias_of[[pv]]  <- c(alias_of[[pv]], a)
      }
    }
  }

  # The alias value belonging to each value of the variable it regroups.
  # A categorical variable looks its level up. A continuous grid can hold a
  # value that does not occur in the data, so there it takes the band of
  # the nearest value that does, which is the band it falls in because a
  # band covers a run of neighbouring values.
  alias_values <- function(a, vals) {
    lm  <- level_map[[a]]
    out <- if (is.null(lm$num)) unname(lm$val[as.character(vals)]) else {
      v    <- suppressWarnings(as.numeric(as.character(vals)))
      brks <- c(-Inf, lm$num[-1] - diff(lm$num) / 2, Inf)
      lm$val[findInterval(v, brks)]
    }
    if (anyNA(out)) {
      miss <- unique(as.character(vals)[is.na(out)])
      stop("the value(s) ", paste(utils::head(miss, 5), collapse = ", "),
           " of '", merge_map[[a]], "' do not occur in 'data', so their ",
           "group in '", a, "' is unknown", call. = FALSE)
    }
    out
  }

  # Put a variable into a prediction frame, together with every column that
  # is a regrouping of it: if the two models use different spellings of one
  # rating factor, both have to move or the merged block would hold one
  # model at its base value while the other moves.
  set_var <- function(newd, vars, bv, vals) {
    if (bv %in% vars) newd[[bv]] <- .coerce_like(vals, data[[bv]])
    for (a in alias_of[[bv]]) if (a %in% vars)
      newd[[a]] <- .coerce_like(alias_values(a, vals), data[[a]])
    newd
  }
  # The name a merged variable is reported under.
  canon <- function(v) if (!is.null(merge_map[[v]])) merge_map[[v]] else v
  # Every spelling of one rating factor, to test a model against.
  spellings <- function(bv) c(bv, alias_of[[bv]])

  # Base value per variable (raw value, not yet coerced):
  #   categorical -> reference level; continuous -> median; exposure -> 1
  base_value_raw <- function(bv) {
    # A regrouped column sits at the group its variable's base value falls
    # in, so the base row is a policy that could exist
    if (!is.null(merge_map[[bv]]))
      return(alias_values(bv, base_value_raw(merge_map[[bv]])))
    # Any offset variable sits at 1, not just the one named by
    # exposure_col, so the intercept is a rate per unit of exposure
    # whatever that column happens to be called
    if (bv == exposure_col || bv %in% offset_vars) return(1)
    if (var_is_cat(bv)) {
      lv <- var_levels(bv)
      # A level named in `base` wins over base_level for that variable
      if (!is.null(base) && bv %in% names(base))
        return(as.character(base[[bv]]))
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
    vars <- .rhs_vars(model)
    if (!any(spellings(var_name) %in% vars)) return(rep(1, n))
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    newd    <- set_var(newd, vars, var_name, grid_vals)
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
    newd <- set_var(newd, vars, vx, rep(gx, times = length(gg)))
    newd <- set_var(newd, vars, vg, rep(gg, each  = length(gx)))
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
    } else {
      if (!is.finite(step)) step <- .nice_step(rng, n_points, int_data)
      # snap the base onto the same grid, so the sequence stays readable and
      # the row with factor 1 is a value someone would actually quote
      b0 <- .snap_to_step(med, step, rng[1], rng[2])
      out <- list(values = .step_grid(rng[1], rng[2], step, b0),
                  base = b0, step = step)
    }
    # A base chosen in `base` replaces the median. It is put on the grid
    # exactly as given - the user asked for 40, not for the nearest grid
    # point - after dropping any grid value that is the same number up to
    # rounding, so there is one row with factor 1 and not two.
    if (!is.null(base) && bv %in% names(base)) {
      ub   <- as.numeric(base[[bv]])
      vals <- out$values[abs(out$values - ub) > 1e-9 * max(1, abs(ub))]
      out$values <- sort(c(vals, ub))
      out$base   <- ub
    }
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
    unique(vapply(comps, function(x) canon(base_var(x)), character(1)))
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

  # A merge declaration that matches no term is as silent a mistake as a
  # misspelled grid step: the two blocks stay, and nothing says why.
  if (length(merge_map)) {
    model_terms <- unique(c(if (!is.null(model_freq)) .rhs_vars(model_freq),
                            if (!is.null(model_sev))  .rhs_vars(model_sev)))
    idle <- names(merge_map)[!vapply(names(merge_map), function(a)
      any(c(a, merge_map[[a]]) %in% model_terms), logical(1))]
    if (length(idle))
      warning("make_rating_table: 'merge_vars' mentions ",
              paste(idle, collapse = ", "), ", which neither model uses, ",
              "so the declaration changes nothing.", call. = FALSE)
  }

  # The same situation left undeclared. Worth one line, because the table
  # is then right but reads as two tariff fields where there is one, and
  # whoever builds the tariff from it has to know they belong together.
  # Only across the two models: two nested factors inside one model (a
  # postcode and its region) are genuinely two rating factors.
  if (!is.null(model_freq) && !is.null(model_sev)) {
    f_only <- setdiff(.rhs_vars(model_freq), .rhs_vars(model_sev))
    s_only <- setdiff(.rhs_vars(model_sev),  .rhs_vars(model_freq))
    cand   <- character(0)
    for (a in intersect(f_only, names(data))) for (b in intersect(s_only, names(data))) {
      if (!is.null(merge_map[[a]]) || !is.null(merge_map[[b]])) next
      if (is.numeric(data[[a]]) && is.numeric(data[[b]])) next
      na <- data.table::uniqueN(d_dt[[a]], na.rm = TRUE)
      nb <- data.table::uniqueN(d_dt[[b]], na.rm = TRUE)
      # A column with very many levels is not a tariff block, and two
      # columns of the same width are not a regrouping of one another
      if (na < 2 || nb < 2 || na == nb || max(na, nb) > 1000) next
      fine   <- if (na > nb) a else b
      coarse <- if (na > nb) b else a
      sub    <- d_dt[!is.na(get(a)) & !is.na(get(b)), c(fine, coarse),
                     with = FALSE]
      if (!nrow(sub)) next
      # One group per value, which is what makes it a regrouping
      if (nrow(unique(sub)) == data.table::uniqueN(sub[[1L]]))
        cand <- c(cand, paste0("merge_vars = list(", fine, " = \"", coarse,
                               "\")"))
    }
    if (length(cand))
      warning("make_rating_table: one column looks like a regrouping of ",
              "another, each used by one of the two models, so the table ",
              "holds two blocks for what is one tariff field. If that is ",
              "what they are, pass ",
              paste(unique(cand), collapse = " or "),
              ".", call. = FALSE)
  }

  # A chosen base per variable. Checked up front, because a wrong one does
  # not fail later: it silently moves every factor of that variable.
  if (!is.null(base)) {
    base <- as.list(base)
    if (is.null(names(base)) || any(!nzchar(names(base))))
      stop("make_rating_table: 'base' must be named, e.g. ",
           "list(REGIO = \"Zuid\", LEEFTIJD = 40).", call. = FALSE)
    merged <- intersect(names(base), names(merge_map))
    if (length(merged))
      stop("make_rating_table: '", paste(merged, collapse = ", "), "' is ",
           "merged into '",
           paste(unique(unlist(merge_map[merged])), collapse = ", "),
           "'; set the base on that variable instead.", call. = FALSE)
    fixed <- intersect(names(base), c(exposure_col, offset_vars))
    if (length(fixed))
      stop("make_rating_table: '", paste(fixed, collapse = ", "), "' is an ",
           "offset or exposure variable; its base is always 1, which is what ",
           "makes the intercept a rate per unit of exposure.", call. = FALSE)
    unused <- setdiff(names(base), uniq_bases)
    if (length(unused)) {
      warning("make_rating_table: 'base' names with no variable in the model, ",
              "and therefore ignored: ", paste(unused, collapse = ", "), ".",
              call. = FALSE)
      base <- base[setdiff(names(base), unused)]
    }
    for (bv in names(base)) {
      val <- base[[bv]]
      if (length(val) != 1)
        stop("make_rating_table: 'base' for '", bv, "' must be one value.",
             call. = FALSE)
      if (var_is_cat(bv)) {
        lv <- var_levels(bv)
        if (!as.character(val) %in% lv)
          stop("make_rating_table: '", val, "' is not a level of '", bv,
               "'. Levels: ", paste(lv, collapse = ", "), ".", call. = FALSE)
      } else {
        num <- suppressWarnings(as.numeric(val))
        if (!is.finite(num))
          stop("make_rating_table: 'base' for '", bv, "' must be a number.",
               call. = FALSE)
        r <- range(data[[bv]], na.rm = TRUE)
        if (num < r[1] || num > r[2])
          warning("make_rating_table: the base ", num, " for '", bv,
                  "' lies outside the data (", r[1], " to ", r[2], "); ",
                  "every factor of that variable is then relative to an ",
                  "extrapolated point.", call. = FALSE)
      }
    }
    if (!length(base)) base <- NULL
  }

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
      bvs <- unname(vapply(comps, function(x) canon(base_var(x)),
                           character(1)))
      # An interaction between a variable and a regrouping of itself is one
      # variable after merging, so there is no two-way table to build
      if (bvs[1] == bvs[2]) next
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
  if (length(int_rows)) {
    out <- rbind(out, do.call(rbind, int_rows))
  } else {
    # An uplift only exists relative to the two main effects it sits
    # between, so on a tariff without a single interaction term these three
    # columns are NA in every row. Drop them rather than ship a block of
    # emptiness that has to be explained. `Group` and `GroupVar` stay:
    # `is.na(Group)` is how the rest of the package tells a main effect
    # from an interaction, so those two are structure, not payload.
    out <- out[, setdiff(names(out), c("Uplift_Frequency", "Uplift_Severity",
                                       "Uplift_Premium")), drop = FALSE]
  }

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
