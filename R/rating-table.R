#' Build a multiplicative rating table from frequency/severity models
#'
#' Builds a table of multiplicative rating factors from a frequency and/or
#' severity model (both log link), including two-way interactions.
#'
#' Base convention: the base level is the reference level for categorical
#' variables (first xlevel, or the level with the largest exposure when
#' `base_level = "exposure"`), the latest year for the accounting year
#' (`year_col`), the median for continuous variables (added as an explicit
#' grid point, `IsBase = TRUE`), and 1 for the exposure column. As a
#' result, for log-link models without interactions the identity holds
#' exactly: intercept x product(factors) = model prediction per exposure
#' unit. A variable that is absent from one of the two models gets the
#' neutral factor 1 there (so `Factor_Premium` remains correct).
#'
#' A term that turns a column into a factor inside the formula, such as
#' `cut(LEEFTIJD, c(17, 24, 40, 65, 99))`, is reported on its bands; a
#' logical column (`GARAGE = TRUE`) on `TRUE` and `FALSE`.
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
#' [make_rating_plot()]. That flag counts claims; the precision of a factor
#' is in `SE_Frequency` and `SE_Severity`, the standard error of its
#' logarithm from the model's covariance matrix. A level with 31 claims
#' passes the flag with a standard error of 0.18, which is a 95% interval
#' of x0.70 to x1.42 around its factor.
#'
#' A level that is in `data` but not in the data a model was fitted on,
#' typically a level without claims that the severity model never saw,
#' keeps its row: the factor of that model is `NA` there, and so is the
#' premium factor, with a warning. Only a base row the model does not know
#' is an error, since every factor is a ratio against it.
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
#'   claims has no severity factor either way. [add_groupings()] runs the
#'   same derivation on a dataset you want to score, from the same
#'   declaration.
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
#'   raise a warning, which names the levels without any claim as well.
#' @param year_col Accounting year column (default `"BOEKJAAR"`), or
#'   `NULL`. When a model uses it as a rating factor, its default base is
#'   the latest year rather than the first level, so the intercept is the
#'   current price level instead of the oldest one; the other factors do
#'   not change. `base` still overrides it, e.g. `list(BOEKJAAR = 2023)`.
#' @param loss_col Loss amount column (default `"SCHADELAST"`), read only
#'   to warn when claims without payment would make frequency x severity
#'   overstate the risk premium.
#'
#' @return A data.frame with one row per level/grid point per variable
#'   (columns `Variable`, `Type`, `Level`, `LevelNum`, `Group`, `IsBase`,
#'   `Exposure`, `ClaimCount`, `Factor_*`, `Uplift_*`, `SE_Frequency`,
#'   `SE_Severity`, `IsThin`, `XVar`, `GroupVar`). `SE_*` is the standard
#'   error of the logarithm of the factor in that row: 0 at the base and
#'   for a variable the model does not use, `NA` where there is no factor.
#'   On an interaction row it belongs to the joint relativity against the
#'   base cell. The three `Uplift_*` columns are present **only when the
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
                              merge_vars   = NULL,
                              year_col     = "BOEKJAAR",
                              loss_col     = "SCHADELAST") {

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

  # [OFF-07]
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

  # [OFF-06] [OFF-09]
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

  # [RT-08]
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

  # [RT-11]
  # An inline term that turns a column into a factor - cut(LEEFTIJD, ...), an
  # ifelse() - has levels that are not values of the column: "(24,30]" cannot
  # be put back into LEEFTIJD. Predicting on the label gave NA in every row,
  # the intercept included, since every prediction shares the base row. Such
  # a variable is run through the model on one representative value per
  # level instead - the most frequent value of the column within the level,
  # which falls in it by definition - and its volumes are counted on the
  # evaluated term. factor(BOEKJAAR) needs none of this: "2021" is a year.
  xterm <- list(); xenv <- list()
  for (m in list(model_freq, model_sev)) {
    if (is.null(m) || !length(m$xlevels)) next
    for (nm in names(m$xlevels)) {
      bv <- base_var(nm)
      if (is.null(xterm[[bv]])) {
        xterm[[bv]] <- nm
        xenv[[bv]]  <- environment(formula(m))
      }
    }
  }
  decoder <- list()
  for (bv in names(xterm)) {
    if (!bv %in% names(data)) next
    lv <- xlev_all[[bv]]
    if (!anyNA(.coerce_like(lv, data[[bv]]))) next
    rows <- tryCatch(as.character(eval(str2lang(xterm[[bv]]), data,
                                       xenv[[bv]])), error = function(e) NULL)
    if (is.null(rows) || length(rows) != nrow(data)) next
    rep_val <- vapply(lv, function(l) {
      x <- data[[bv]][!is.na(rows) & rows == l]
      if (!length(x)) return(NA_character_)
      names(sort(table(as.character(x)), decreasing = TRUE))[1]
    }, character(1))
    decoder[[bv]] <- list(rows = rows, value = rep_val)
  }
  # The values that go into a prediction frame for labels of a variable
  to_model <- function(bv, vals) {
    dc <- decoder[[bv]]
    if (is.null(dc)) return(.coerce_like(vals, data[[bv]]))
    .coerce_like(unname(dc$value[as.character(vals)]), data[[bv]])
  }
  # The level each row of `data` is in, for the volumes
  row_level <- function(bv) {
    dc <- decoder[[bv]]
    if (is.null(dc)) as.character(data[[bv]]) else dc$rows
  }
  # The most recent level of an accounting year, for its default base
  latest_of <- function(lv) {
    num <- suppressWarnings(as.numeric(lv))
    if (!anyNA(num)) lv[which.max(num)] else lv[length(lv)]
  }
  if (!is.null(year_col) &&
      (!is.character(year_col) || length(year_col) != 1 || is.na(year_col)))
    stop("make_rating_table: 'year_col' must be one column name or NULL.",
         call. = FALSE)

  # [MV-01]
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
        # [MV-03]
        # Derived from the model that uses it when `data` lacks it, checked
        # against that model when `data` has it; shared with add_groupings()
        data[[a]] <- .grouping_column(data, pv, a, model_freq, model_sev,
                                      "make_rating_table")
        data.table::set(d_dt, j = a, value = data[[a]])
        # [MV-02]
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

  # [MV-07]
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

  # [MV-01]
  # Put a variable into a prediction frame, together with every column that
  # is a regrouping of it: if the two models use different spellings of one
  # rating factor, both have to move or the merged block would hold one
  # model at its base value while the other moves.
  set_var <- function(newd, vars, bv, vals) {
    if (bv %in% vars) newd[[bv]] <- to_model(bv, vals)
    for (a in alias_of[[bv]]) if (a %in% vars)
      newd[[a]] <- .coerce_like(alias_values(a, vals), data[[a]])
    newd
  }
  # The name a merged variable is reported under.
  canon <- function(v) if (!is.null(merge_map[[v]])) merge_map[[v]] else v
  # Every spelling of one rating factor, to test a model against.
  spellings <- function(bv) c(bv, alias_of[[bv]])

  # [RT-02]
  # Base value per variable (raw value, not yet coerced):
  #   categorical -> reference level; continuous -> median; exposure -> 1
  base_value_raw <- function(bv) {
    # [MV-09]
    # A regrouped column sits at the group its variable's base value falls
    # in, so the base row is a policy that could exist
    if (!is.null(merge_map[[bv]]))
      return(alias_values(bv, base_value_raw(merge_map[[bv]])))
    # [OFF-06]
    # Any offset variable sits at 1, not just the one named by
    # exposure_col, so the intercept is a rate per unit of exposure
    # whatever that column happens to be called
    if (bv == exposure_col || bv %in% offset_vars) return(1)
    if (var_is_cat(bv)) {
      lv <- var_levels(bv)
      # A level named in `base` wins over base_level for that variable
      if (!is.null(base) && bv %in% names(base))
        return(as.character(base[[bv]]))
      # [RT-15] the accounting year sits at its latest level by default, so
      # the intercept is the current price level and not the oldest one
      if (!is.null(year_col) && identical(bv, year_col))
        return(latest_of(lv))
      if (base_level == "exposure") {
        ag   <- data.table::data.table(.lvl = row_level(bv),
                                       .e = data[[exposure_col]])[
          , .(E = sum(.e, na.rm = TRUE)), by = .lvl]
        cand <- as.character(ag$.lvl)[order(-ag$E)]
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

  # [RT-01] [RT-09]
  # Base row for a model: all predictors at their base value and every
  # offset variable at 1 (base_value_raw()). The factors are ratios against
  # this row, so the offset cancels in them; the intercept is this row's
  # prediction with the whole offset subtracted on the link scale
  # (intercept_val()), so it is per unit of exposure whatever form the
  # offset takes.
  base_df_for <- function(model) {
    vars <- .rhs_vars(model)
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars))
      stop("variable(s) from the model formula not in 'data': ",
           paste(missing_vars, collapse = ", "))
    row <- lapply(vars, function(nm)
      if (!is.null(merge_map[[nm]])) .coerce_like(base_value_raw(nm), data[[nm]])
      else to_model(nm, base_value_raw(nm)))
    names(row) <- vars
    # check.names = FALSE, or a column such as "AUTO GEWICHT" is silently
    # renamed to "AUTO.GEWICHT" here and every predict() on this row fails
    as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
  }

  # [RT-01] [RT-03]
  # Factors for one variable: prediction on grid / prediction on base row.
  # Both with all other predictors at their base value, hence consistent
  # with intercept_val. Variable not in the model -> neutral factor 1.
  predict_factor <- function(model, var_name, grid_vals) {
    n <- length(grid_vals)
    if (is.null(model))
      return(list(factor = rep(NA_real_, n), se = rep(NA_real_, n)))
    vars <- .rhs_vars(model)
    if (!any(spellings(var_name) %in% vars))
      return(list(factor = rep(1, n), se = rep(0, n)))
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    newd    <- set_var(newd, vars, var_name, grid_vals)
    ok      <- .predictable_rows(model, newd)
    preds   <- predict_rows(model, newd, ok)
    ref     <- as.numeric(predict(model, newdata = base_df, type = "response"))
    list(factor = preds / ref, se = se_of(model, newd, base_df, ok))
  }

  # [RT-10]
  # Predictions with the rows the model cannot be asked about left as NA. A
  # level that exists in the data but not in the data a model was fitted on
  # - a fuel type with exposure but no claims, which the severity model
  # therefore never saw - has no factor in that model. That is one empty
  # cell, not a reason to lose the whole variable from the table.
  predict_rows <- function(model, newd, ok) {
    out <- rep(NA_real_, nrow(newd))
    if (any(ok))
      out[ok] <- as.numeric(predict(model, newdata = newd[ok, , drop = FALSE],
                                    type = "response"))
    out
  }

  # [RT-12]
  # Standard error of log(Factor), from the model's own covariance matrix:
  # log(Factor) = (x_row - x_base)' beta, so its variance is d' V d with d
  # the difference of the two model-matrix rows. That holds for any term a
  # model can carry - spline, interaction, inline factor - because the
  # model matrix is built exactly as predict() builds it. A volume flag
  # cannot say this: a level with 31 claims passes min_claims = 30 with a
  # standard error of 0.18, a 95% interval of x0.70 to x1.42. Only for a log
  # link, where log(Factor) is linear in the coefficients.
  se_of <- function(model, newd, base_df, ok) {
    out <- rep(NA_real_, nrow(newd))
    if (is.null(model) || !identical(family(model)$link, "log") || !any(ok))
      return(out)
    V  <- tryCatch(stats::vcov(model), error = function(e) NULL)
    cf <- stats::coef(model)
    cf <- names(cf)[!is.na(cf)]
    if (is.null(V) || !all(cf %in% colnames(V))) return(out)
    tt  <- stats::delete.response(stats::terms(model))
    mmx <- function(nd) {
      mf <- stats::model.frame(tt, nd, xlev = model$xlevels,
                               na.action = stats::na.pass)
      X  <- stats::model.matrix(tt, mf, contrasts.arg = model$contrasts)
      X[, cf, drop = FALSE]
    }
    X <- tryCatch(mmx(newd[ok, , drop = FALSE]), error = function(e) NULL)
    B <- tryCatch(mmx(base_df), error = function(e) NULL)
    if (is.null(X) || is.null(B)) return(out)
    D <- sweep(X, 2, B[1, ])
    out[ok] <- sqrt(pmax(rowSums((D %*% V[cf, cf, drop = FALSE]) * D), 0))
    out
  }

  # [RT-06]
  # Joint relativity over a 2D grid (vx x vg), rest at base, scaled to the
  # base cell (index i_base). vx varies fastest.
  predict_factor_2d <- function(model, vx, gx, vg, gg, i_base) {
    n <- length(gx) * length(gg)
    if (is.null(model))
      return(list(factor = rep(NA_real_, n), se = rep(NA_real_, n)))
    vars    <- .rhs_vars(model)
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    newd <- set_var(newd, vars, vx, rep(gx, times = length(gg)))
    newd <- set_var(newd, vars, vg, rep(gg, each  = length(gx)))
    ok    <- .predictable_rows(model, newd)
    preds <- predict_rows(model, newd, ok)
    list(factor = preds / preds[i_base],
         se = se_of(model, newd, newd[i_base, , drop = FALSE], ok))
  }

  # [RT-04] [RT-02]
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
    # [RT-15] a numeric accounting year (a trend term) sits at its latest
    # grid value, like a factor(BOEKJAAR) at its latest level
    if (!is.null(year_col) && identical(bv, year_col))
      out$base <- max(out$values)
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

  # [RT-05]
  # Which grid point does each policy belong to? Categorical: its level.
  # Continuous: the nearest grid value, splitting on the midpoints between
  # them, the same rule the main-effect exposure histogram uses. Returns
  # NA outside a trimmed range, so the volume of policies the grid does not
  # cover is not swept into the edge cells.
  #
  # [RT-16] The trimming applies to a grid that spans the trimmed range, not
  # to the three slices of a continuous group variable in an interaction:
  # those stand for the whole distribution, and cutting at the outer slices
  # threw away everything below the 10% and above the 90% quantile, a fifth
  # of the exposure of every interaction block.
  cell_index <- function(bv, grid_vals, is_cat, drop_outside = TRUE) {
    if (is_cat) return(match(row_level(bv), as.character(grid_vals)))
    x <- data[[bv]]
    brks <- c(-Inf, grid_vals[-1] - diff(grid_vals) / 2, Inf)
    i <- findInterval(x, brks)
    outside <- x < min(grid_vals) | x > max(grid_vals)
    if (drop_outside && !isTRUE(all.equal(trim, c(0, 1))))
      i[outside %in% TRUE] <- NA_integer_
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

  # [MV-09]
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

  # [MV-08]
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

  # [RT-02] [MV-09]
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

  # [RT-04]
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

  # [RT-10]
  # Every factor is a ratio against the base row, so a base row a model was
  # never fitted on would empty the whole table. Caught once, up front, with
  # the value that causes it.
  for (mm in list(list(m = model_freq, nm = "frequency"),
                  list(m = model_sev,  nm = "severity"))) {
    if (is.null(mm$m)) next
    un <- .unseen_levels(mm$m, base_df_for(mm$m))
    if (length(un))
      stop("make_rating_table: the base row has ",
           paste(vapply(names(un), function(cl)
             paste0("'", cl, "' = ", un[[cl]]$values[1]), character(1)),
             collapse = "; "),
           ", which the ", mm$nm, " model was not fitted on (a level without ",
           "claims, say). Choose a base that model knows, with base = or ",
           "base_level = \"exposure\".", call. = FALSE)
  }

  # [RT-13] claims without payment overstate frequency x severity
  .nil_claims_warning(model_freq, model_sev, data, claims_col, loss_col,
                      "make_rating_table")

  # [RT-06]
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

  # [CONV-04]
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

    # Exposure / claims aggregation, per level as the model sees it (for a
    # cut() term that is the band a row falls in, [RT-11])
    if (is_cat) {
      ag <- data.table::data.table(Level = row_level(bv),
                                   .e = data[[exposure_col]],
                                   .c = data[[claims_col]])[
        , .(Exposure = sum(.e, na.rm = TRUE), ClaimCount = sum(.c, na.rm = TRUE)),
        by = Level]
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
    p_freq <- predict_factor(model_freq, bv, grid_vals)
    p_sev  <- predict_factor(model_sev,  bv, grid_vals)

    data.frame(
      Variable         = bv,
      Type             = if (is_cat) "categorical" else "continuous",
      Level            = .level_chr(grid_vals),
      LevelNum         = suppressWarnings(as.numeric(as.character(grid_vals))),
      Group            = NA_character_,
      IsBase           = is_base,
      Exposure         = exposure,
      ClaimCount       = claim_count,
      Factor_Frequency = p_freq$factor,
      Factor_Severity  = p_sev$factor,
      Factor_Premium   = p_freq$factor * p_sev$factor,
      SE_Frequency     = p_freq$se,
      SE_Severity      = p_sev$se,
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

  # [RT-06] [CONV-04]
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
    cj <- cell_index(gv, gg, cg, drop_outside = FALSE)   # [RT-16]
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

    p2f <- predict_factor_2d(model_freq, xv, gx, gv, gg, i_base)
    p2s <- predict_factor_2d(model_sev,  xv, gx, gv, gg, i_base)
    ff <- p2f$factor; fs <- p2s$factor

    # Pure interaction uplift: joint / (main_factor_x * main_factor_group)
    uf <- ff / (rep(predict_factor(model_freq, xv, gx)$factor, times = ng) *
                rep(predict_factor(model_freq, gv, gg)$factor, each  = nx))
    us <- fs / (rep(predict_factor(model_sev,  xv, gx)$factor, times = ng) *
                rep(predict_factor(model_sev,  gv, gg)$factor, each  = nx))

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
      SE_Frequency     = p2f$se,
      SE_Severity      = p2s$se,
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
    # [RT-06]
    # An uplift only exists relative to the two main effects it sits
    # between, so on a tariff without a single interaction term these three
    # columns are NA in every row. Drop them rather than ship a block of
    # emptiness that has to be explained. `Group` and `GroupVar` stay:
    # `is.na(Group)` is how the rest of the package tells a main effect
    # from an interaction, so those two are structure, not payload.
    out <- out[, setdiff(names(out), c("Uplift_Frequency", "Uplift_Severity",
                                       "Uplift_Premium")), drop = FALSE]
  }

  # [RT-07]
  # Thin-cell flag -------------------------------------------------------------
  # Levels backed by very few claims: the GLM applies no shrinkage to a
  # categorical level, so its factor comes essentially from those few
  # claims and is correspondingly unstable. Flagged here, dimmed by
  # make_rating_plot() and greyed out by export_rating_table().
  out$IsThin <- ifelse(is.na(out$ClaimCount), NA, out$ClaimCount < min_claims)

  # Warn for thin categorical main-effect levels (thin tail bins are
  # normal for continuous grids and would make the warning noisy). A level
  # with exposure and no claims at all is worth its own words: its
  # frequency factor is the fit running towards zero, not an estimate.
  main_cat <- is.na(out$Group) & out$Type == "categorical"
  n_thin <- sum(out$IsThin & main_cat, na.rm = TRUE)
  zero   <- main_cat & out$ClaimCount == 0 & out$Exposure > 0
  if (n_thin > 0)
    warning("make_rating_table: ", n_thin, " categorical level(s) have ",
            "fewer than ", min_claims, " claims; the corresponding factors ",
            "rest on very little experience (see the IsThin and SE_ ",
            "columns).",
            if (any(zero))
              paste0(" ", sum(zero), if (sum(zero) == 1) " of them has" else
                       " of them have", " no claims at all (",
                     paste(utils::head(paste0(out$Variable[zero], " = ",
                                              out$Level[zero]), 5),
                           collapse = ", "),
                     "): their frequency factor is the fit running towards ",
                     "zero, not an estimate; group them with a level that ",
                     "has claims.") else "",
            call. = FALSE)

  # [RT-10]
  # Cells a model has no factor for: a level the model was not fitted on.
  # Said once, naming the cells, because an NA in a tariff table is a value
  # somebody has to supply by hand.
  for (mm in list(list(m = model_freq, col = "Factor_Frequency",
                       nm = "frequency"),
                  list(m = model_sev,  col = "Factor_Severity",
                       nm = "severity"))) {
    if (is.null(mm$m)) next
    i <- which(is.na(out[[mm$col]]))
    if (!length(i)) next
    cells <- unique(paste0(out$Variable[i], " = ", out$Level[i],
                           ifelse(is.na(out$Group[i]), "",
                                  paste0(" x ", out$Group[i]))))
    warning("make_rating_table: no ", mm$nm, " factor for ",
            paste(utils::head(cells, 5), collapse = ", "),
            if (length(cells) > 5) paste0(" and ", length(cells) - 5, " more")
            else "",
            ": the ", mm$nm, " model was not fitted on that level",
            if (mm$nm == "severity")
              " (a level without claims leaves no severity data)" else "",
            ", so ", mm$col, " and Factor_Premium are NA there. Group the ",
            "level with one the model knows, or supply the factor yourself.",
            call. = FALSE)
  }

  # [OFF-06] [RT-01] [RT-03]
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
