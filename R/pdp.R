#' Partial dependence plot with observed overlay
#'
#' Computes the PDP on the response scale: for each grid value of `pred_var`
#' the whole training data is set to that value, predicted with
#' `predict(type = "response")`, and the weighted arithmetic mean is taken
#' (frequency: exposure-weighted; severity: claim-weighted). The offset is
#' neutralised by setting `exposure_col` to 1, so the PDP is a frequency per
#' unit of exposure and directly comparable to the observed line.
#' (`exp(mean(link))` would be a geometric mean, including the offset, and
#' systematically biased low.)
#'
#' The "Observed" line is a one-way marginal (including correlations with
#' other rating factors); the PDP is a partial effect. Differences between
#' the two therefore do not necessarily indicate misfit.
#'
#' Performance: the training data is first collapsed to the unique profiles
#' of the model's other predictors (with summed weights), so the cost is
#' `grid_res` x (number of unique profiles) predictions instead of
#' `grid_res` x n rows, with an exactly identical result.
#'
#' @param model A fitted glm object (fitted with a `data =` argument).
#' @param raw_data Raw data for the observed line and the bars.
#' @param pred_var Name of the predictor (string).
#' @param metric `"Frequency"` or `"Severity"`.
#' @param transform Deprecated and ignored (response scale is the default).
#' @param grid_res Number of grid points for continuous predictors
#'   (default 50).
#' @param y_label Y-axis label (default automatic).
#' @param metric_fmt Number of decimals in the tooltip (default 4).
#' @param exposure_col,claims_col,loss_col Column names, see [agg_all()].
#' @param discrete_cutoff See [make_plot()].
#'
#' @return A plotly object with exposure/claim bars, the observed one-way
#'   line and the PDP line.
#' @export
make_pdp <- function(model,
                     raw_data,
                     pred_var,
                     metric       = c("Frequency", "Severity"),
                     transform    = NULL,
                     grid_res     = 50,
                     y_label      = NULL,
                     metric_fmt   = 4,
                     exposure_col = "Exposure",
                     claims_col   = "AantalClaims",
                     loss_col     = "SCHADELAST",
                     discrete_cutoff = 25) {

  metric <- match.arg(metric)
  if (!inherits(model, "glm"))
    stop("make_pdp: 'model' must be a glm object.", call. = FALSE)
  if (!is.null(transform))
    warning("make_pdp: 'transform' is deprecated and ignored; the PDP is ",
            "computed directly on the response scale.", call. = FALSE)
  .check_cols(raw_data, c(pred_var, exposure_col, claims_col,
                          if (metric == "Severity") loss_col), "make_pdp")

  fam <- family(model)

  # Observed aggregation depending on metric --------------------------------
  obs <- data.table::as.data.table(raw_data)

  obs_agg <- switch(metric,
                    "Frequency" = obs[, .(
                      .obs      = sum(get(claims_col),   na.rm = TRUE) /
                                  sum(get(exposure_col), na.rm = TRUE),
                      .exposure = sum(get(exposure_col), na.rm = TRUE)
                    ), by = pred_var],
                    "Severity" = obs[get(claims_col) > 0, .(
                      .obs      = sum(get(loss_col),   na.rm = TRUE) /
                                  sum(get(claims_col), na.rm = TRUE),
                      .exposure = sum(get(claims_col), na.rm = TRUE)  # weight = claim count
                    ), by = pred_var]
  )
  obs_agg[!is.finite(.obs), .obs := NA_real_]

  if (is.null(y_label)) {
    y_label <- switch(metric,
                      "Frequency" = "Frequency (claims / exposure)",
                      "Severity"  = "Severity (loss / claim)")
  }
  exposure_label <- switch(metric,
                           "Frequency" = "Exposure",
                           "Severity"  = "Number of claims")

  data.table::setnames(obs_agg, pred_var, ".x")
  obs_agg <- obs_agg[order(.x)]

  # Training data, weights and offset ---------------------------------------
  tr <- .glm_training_data(model, "make_pdp")
  if (is.null(tr$data))
    stop("make_pdp: training data cannot be recovered; fit the model with ",
         "a 'data =' argument.", call. = FALSE)
  train_df <- tr$data
  if (!pred_var %in% names(train_df))
    stop("make_pdp: predictor '", pred_var,
         "' not found in the model's training data.", call. = FALSE)

  has_offset <- !is.null(tr$offset) && any(tr$offset != 0)

  w_avg <- if (metric == "Frequency") {
    if (has_offset) {
      if (!identical(fam$link, "log"))
        warning("make_pdp: an offset is present but the link is not 'log'; ",
                "exp(offset) is then not an exposure and the weighting is ",
                "unreliable.", call. = FALSE)
      exp(tr$offset)
    } else if (!all(tr$weights == 1)) {
      tr$weights                      # rate model with weights = exposure
    } else {
      warning("make_pdp: no offset or weights found; the PDP is averaged ",
              "unweighted.", call. = FALSE)
      rep(1, nrow(train_df))
    }
  } else {
    tr$weights                        # severity: prior weights = claim counts
  }

  # Neutralise the offset: predict at exposure = 1, so the PDP is a
  # frequency per unit of exposure (comparable to the observed line).
  nd0 <- train_df
  if (has_offset) {
    if (exposure_col %in% names(nd0)) {
      nd0[[exposure_col]] <- 1
    } else {
      warning("make_pdp: the model has an offset but column '",
              exposure_col, "' is not in the training data; the offset ",
              "cannot be neutralised and the PDP is then NOT a frequency ",
              "per unit of exposure.", call. = FALSE)
    }
  }

  # Grid ---------------------------------------------------------------------
  x_train <- train_df[[pred_var]]
  is_discrete <- is.character(x_train) || is.factor(x_train) ||
                 !is.numeric(x_train) ||
                 length(unique(x_train)) <= discrete_cutoff
  grid_vals <- if (is.factor(x_train)) {
    levels(x_train)
  } else if (!is.numeric(x_train)) {
    sort(unique(x_train))
  } else if (length(unique(x_train)) <= grid_res) {
    sort(unique(x_train))
  } else {
    seq(min(x_train, na.rm = TRUE), max(x_train, na.rm = TRUE),
        length.out = grid_res)
  }

  # Collapse to unique predictor profiles (performance) ----------------------
  # predict() only depends on the model's predictor columns. Rows that
  # share the same values for all predictors other than pred_var receive
  # the same prediction on every grid point, so they are collapsed into
  # one profile with summed weight. On typical pricing data (categorical
  # rating factors, integer ages, exposure already neutralised to 1) this
  # reduces the work from grid_res x n rows to grid_res x (profiles),
  # while leaving the weighted mean exactly unchanged.
  vars_needed  <- .rhs_vars(model)
  missing_vars <- setdiff(vars_needed, names(nd0))
  if (length(missing_vars))
    stop("make_pdp: column(s) required for prediction not found in the ",
         "training data: ", paste(missing_vars, collapse = ", "),
         call. = FALSE)
  key_vars <- setdiff(vars_needed, pred_var)
  if (length(key_vars)) {
    dtc    <- data.table::as.data.table(nd0[, key_vars, drop = FALSE])
    dtc$.w <- w_avg
    prof   <- as.data.frame(dtc[, .(.w = sum(.w)), by = key_vars])
  } else {
    # pred_var is the only predictor: a single profile suffices
    prof <- data.frame(.w = sum(w_avg))
  }
  w_prof   <- prof$.w
  prof$.w  <- NULL

  # PDP: weighted mean response prediction per grid value --------------------
  # Deliberately an arithmetic mean on the response scale; exp(mean(link))
  # would be a geometric mean that is systematically biased low.
  yhat <- vapply(seq_along(grid_vals), function(i) {
    nd <- prof
    nd[[pred_var]] <- .coerce_like(rep(grid_vals[i], nrow(nd)), x_train)
    stats::weighted.mean(predict(model, newdata = nd, type = "response"),
                         w = w_prof, na.rm = TRUE)
  }, numeric(1))

  pd <- data.frame(
    .x    = if (is.factor(x_train)) factor(grid_vals, levels = levels(x_train))
            else grid_vals,
    .yhat = yhat
  )
  pd <- pd[order(pd$.x), ]

  scatter_mode <- if (is_discrete) "markers" else "lines+markers"

  # Plot ----------------------------------------------------------------------
  plot_ly() %>%

    add_bars(
      data          = obs_agg,
      x             = ~.x,
      y             = ~.exposure,
      name          = exposure_label,
      yaxis         = "y2",
      marker        = list(color = ta_lightblue, opacity = 0.5,
                           line = list(width = 0)),
      hovertemplate = paste0("<b>", pred_var, ":</b> %{x}",
                             "<br><b>", exposure_label, ":</b> %{y:,.0f}",
                             "<extra></extra>")
    ) %>%

    add_trace(
      data          = obs_agg,
      x             = ~.x,
      y             = ~.obs,
      type          = "scatter",
      mode          = scatter_mode,
      name          = "Observed",
      yaxis         = "y",
      line          = list(color = ta_gold, width = 1.8, dash = "dot"),
      marker        = list(color = ta_gold, size = 6),
      hovertemplate = paste0("<b>", pred_var, ":</b> %{x}",
                             "<br><b>Observed:</b> %{y:.", metric_fmt, "f}",
                             "<extra></extra>")
    ) %>%

    add_trace(
      data          = pd,
      x             = ~.x,
      y             = ~.yhat,
      type          = "scatter",
      mode          = scatter_mode,
      name          = "PDP (model)",
      yaxis         = "y",
      line          = list(color = ta_blue, width = 2.2),
      marker        = list(color = ta_blue, size = 6),
      hovertemplate = paste0("<b>", pred_var, ":</b> %{x}",
                             "<br><b>PDP:</b> %{y:.", metric_fmt, "f}",
                             "<extra></extra>")
    ) %>%

    layout(
      title  = list(
        text = paste("Partial Dependence \u2013", pred_var, "\u2013", metric),
        font = list(color = ta_navy, size = 14)
      ),
      xaxis  = list(title     = pred_var,
                    tickangle = -45,
                    tickfont  = list(size = 9),
                    showgrid  = FALSE),
      yaxis  = list(title     = y_label,
                    gridcolor = "#D0D8E0",
                    zeroline  = FALSE),
      yaxis2 = list(title      = exposure_label,
                    overlaying = "y",
                    side       = "right",
                    showgrid   = FALSE,
                    tickfont   = list(color = ta_muted),
                    titlefont  = list(color = ta_muted)),
      legend        = list(orientation = "h", y = -0.2),
      hoverlabel    = list(bgcolor = "white", font = list(size = 12)),
      barmode       = "overlay",
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      margin        = list(b = 80, r = 80)
    ) %>%
    config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions   = list(format = "png", filename = "pdp")
    )
}
