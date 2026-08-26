#' Actual vs expected per predictor
#'
#' Compares observed and predicted values of a fitted glm per (binned) level
#' of a predictor, computed on the model's own training data.
#'
#' The mode is detected automatically. For an offset model with log link
#' (the typical frequency model) the response and predictions are counts;
#' both are divided by the exposure recovered from `exp(offset)`, giving
#' observed and predicted frequency per bin. Otherwise (the typical severity
#' model) the response is already an average and prior-weight-weighted means
#' are shown per bin.
#'
#' A numeric predictor with at most `n_bins` distinct values is **not
#' binned at all**: every value gets its own point, on its exact position.
#' Only when there are more distinct values than `n_bins` is binning
#' applied: with quantile bins by default (each bin holds roughly the
#' same number of observations, avoiding noisy thin tails), or equal-width
#' bins with `bin_type = "width"`. A binned point sits at the
#' weight-weighted mean of the values in its bin, not at the bin edge.
#'
#' @param model A fitted glm object.
#' @param predictor Name of the predictor (string).
#' @param n_bins Maximum number of points for a numeric predictor
#'   (default 150). Predictors with at most this many distinct values are
#'   shown unbinned, one point per value.
#' @param weight_var Optional: weight/exposure column in `model$data`
#'   (override; an unknown name is an error).
#' @param weight_label Optional: axis title for the bars.
#' @param color,color_pred Colours for the observed and predicted lines.
#' @param title,ylab,xlab Optional labels.
#' @param metric_fmt Number of decimals in the tooltip (default 4).
#' @param bin_type `"quantile"` (default) or `"width"`.
#' @param ci Draw error bars on the observed points (default `TRUE`). The
#'   bars answer the question the plot is really asking: is the gap between
#'   observed and predicted bigger than the noise this bin carries? A bin
#'   whose bar comfortably spans the predicted line is not evidence of
#'   anything, however far apart the two markers look.
#' @param ci_level Confidence level for those bars (default `0.95`).
#'
#'   The interval is on the **observed** point only. The predicted line is
#'   the model, and its parameter uncertainty is a different and usually
#'   much smaller quantity. The standard error comes from the fitted
#'   family's own variance function, scaled by the Pearson dispersion for
#'   families that estimate one: for counts the bin total has variance
#'   `phi * sum(V(mu))`, so the rate divides that by the exposure, and for
#'   a weighted mean it is the usual `phi * sum(w V(mu)) / (sum w)^2`.
#'   Poisson and binomial hold the dispersion at 1, since that is the
#'   assumption being examined.
#'
#'   Measured coverage on a correctly specified Poisson is 0.96 for a
#'   binned continuous predictor and 0.98 for a categorical one outside
#'   the model, so the bars are honest to mildly conservative.
#'
#'   One case where they cannot be read as a test: for a **categorical
#'   term that is in the model** under a canonical link, the score
#'   equations force observed to equal predicted at every level exactly,
#'   so the bar contains the predicted point by construction and coverage
#'   is 1 by identity rather than by fit. There the bar length is still
#'   worth reading - it says how much evidence the level carries - but the
#'   absence of a gap is arithmetic, not agreement. See
#'   [detect_interactions()] for what does look inside those levels.
#' @param y_range Optional `c(lo, hi)` to fix the primary y-axis range,
#'   e.g. to make multiple `plot_glm_predictor()` calls for different
#'   predictors visually comparable. Left `NULL` by default, in which
#'   case the axis auto-scales to the observed/predicted values of this
#'   plot only (plotly's default behaviour).
#'
#' @return A plotly object with weight bars, an "Observed" line and a
#'   "Predicted" line.
#' @export
plot_glm_predictor <- function(model, predictor,
                               n_bins       = 150,
                               weight_var   = NULL,
                               weight_label = NULL,
                               color        = ta_year_palette(1),
                               color_pred   = ta_gold,
                               title = NULL, ylab = NULL, xlab = NULL,
                               metric_fmt = 4,
                               bin_type = c("quantile", "width"),
                               ci = TRUE, ci_level = 0.95,
                               y_range = NULL) {

  bin_type <- match.arg(bin_type)
  if (!inherits(model, "glm")) stop("'model' must be a glm object.")
  if (n_bins < 2) stop("plot_glm_predictor: 'n_bins' must be at least 2.")
  if (!is.numeric(ci_level) || length(ci_level) != 1 ||
      ci_level <= 0 || ci_level >= 1)
    stop("plot_glm_predictor: 'ci_level' must be a single value strictly ",
         "between 0 and 1.", call. = FALSE)
  y_range <- .check_range(y_range, "plot_glm_predictor")

  tr            <- .glm_training_data(model, "plot_glm_predictor")
  model_data    <- tr$mf
  response_name <- names(model_data)[1]

  # Retrieve the predictor
  if (predictor %in% names(model_data)) {
    x_values <- model_data[[predictor]]
  } else if (!is.null(tr$data) && predictor %in% names(tr$data)) {
    x_values <- tr$data[[predictor]]
  } else {
    stop(paste0("Predictor '", predictor, "' not found."))
  }

  # model.frame() holds a spline or poly term as a matrix column, which
  # would fail several frames further down with "replacement has 0 rows".
  # Name the column to ask for instead.
  if (!is.null(dim(x_values)))
    stop("plot_glm_predictor: '", predictor, "' is a multi-column model ",
         "term, not a single predictor. Plot the underlying variable ",
         "instead, e.g. '", .term_base_var(predictor, tr$data), "'.",
         call. = FALSE)

  # Determine weight/exposure vector + mode
  fam        <- family(model)
  has_offset <- !is.null(tr$offset) && any(tr$offset != 0)

  # exp(offset) recovers an exposure only when the link is log AND the
  # offset is itself a logarithm; with offset(Exposure) it is exp(Exposure),
  # which is not a volume. Both conditions are read off the model.
  off_is_log <- isTRUE(.offset_is_log(model))
  if (has_offset && identical(fam$link, "log") && off_is_log) {
    w        <- exp(tr$offset)           # offset(log(Exposure)) -> Exposure
    use_rate <- TRUE                     # response & predict are COUNTS -> /exposure
    w_title  <- "Exposure"
  } else {
    if (has_offset && !identical(fam$link, "log"))
      warning("plot_glm_predictor: an offset is present but the link is ",
              "not 'log'; exp(offset) is then not an exposure. The prior ",
              "weights are used instead.", call. = FALSE)
    if (has_offset && identical(fam$link, "log") && !off_is_log)
      warning("plot_glm_predictor: the model's offset is not a logarithm, ",
              "so exp(offset) is not an exposure. The prior weights are ",
              "used instead.", call. = FALSE)
    w        <- tr$weights               # GLM weights (e.g. claim counts for severity)
    use_rate <- FALSE                    # response is already an average -> do not divide
    w_title  <- "Weight"
  }
  # Manual override of the weight column
  if (!is.null(weight_var)) {
    if (is.null(tr$data) || !weight_var %in% names(tr$data))
      stop("plot_glm_predictor: weight_var '", weight_var,
           "' not found in model$data.", call. = FALSE)
    w <- tr$data[[weight_var]]
  }
  # Show bars as soon as there is a real weight concept (offset, weights or override)
  has_weight <- has_offset || !is.null(weight_var) ||
                isTRUE(any(w != 1, na.rm = TRUE))
  if (!is.null(weight_label)) w_title <- weight_label

  df <- data.frame(
    x_var     = x_values,
    observed  = as.numeric(model_data[[response_name]]),
    predicted = predict(model, type = "response"),
    weight    = w
  )
  # Sampling variance of the response, from the family's own variance
  # function, so the error bars follow whatever family was fitted rather
  # than assume Poisson. Multiplied by the prior weight because a GLM
  # states Var(y_i) = phi * V(mu_i) / w_i.
  df$var_unit <- fam$variance(df$predicted)
  df$prior_w  <- tr$weights
  phi <- if (fam$family %in% c("poisson", "binomial")) 1 else
    sum(residuals(model, type = "pearson")^2) / model$df.residual

  # Grouping
  if (is.numeric(df$x_var)) {
    x_binned <- length(unique(df$x_var[!is.na(df$x_var)])) > n_bins
    if (!x_binned) {
      # Fewer distinct values than requested bins: do not bin at all. One
      # point per observed value, positioned on that exact value. Binning
      # here would merge neighbouring values (e.g. vehicle ages 0 and 1)
      # into a single point at their weighted mean, and because cut()
      # spans the observed range, a single outlier would silently shift
      # every point.
      df$bin_group <- factor(df$x_var)
    } else if (bin_type == "quantile") {
      # Quantile bins: each bin holds ~equally many observations, so thin
      # tails do not produce a noisy "observed" line.
      brks <- unique(stats::quantile(df$x_var,
                                     probs = seq(0, 1, length.out = n_bins + 1),
                                     na.rm = TRUE, names = FALSE))
      if (length(brks) < 2)
        brks <- range(df$x_var, na.rm = TRUE) + c(-0.5, 0.5)
      df$bin_group <- cut(df$x_var, breaks = brks,
                          include.lowest = TRUE, dig.lab = 4)
    } else {
      df$bin_group <- cut(df$x_var, breaks = n_bins,
                          include.lowest = TRUE, dig.lab = 4)
    }
    x_is_numeric <- TRUE
  } else {
    df$x_var     <- as.factor(df$x_var)
    df$bin_group <- df$x_var
    x_is_numeric <- FALSE
    x_binned     <- FALSE
  }

  # Aggregation:
  # frequency:  sum(counts) / sum(exposure)
  # severity etc.: weighted mean of the response with the GLM weights
  agg <- df %>%
    group_by(bin_group) %>%
    summarise(
      # Binned: the weighted mean position within the bin. Unbinned: the
      # value itself (first() rather than a mean, so a group with zero
      # total weight still gets its exact position instead of NaN).
      x_plot        = if (!x_is_numeric) dplyr::first(as.character(x_var))
                      else if (x_binned) weighted.mean(x_var, weight)
                      else dplyr::first(x_var),
      avg_observed  = if (use_rate) sum(observed)  / sum(weight) else weighted.mean(observed,  weight),
      avg_predicted = if (use_rate) sum(predicted) / sum(weight) else weighted.mean(predicted, weight),
      weight_sum    = sum(weight),
      # Standard error of the OBSERVED point, which is the one that
      # carries sampling noise; the predicted line is the model. Counts:
      # the bin total has variance phi * sum(V(mu_i)), so the rate divides
      # that by the exposure. Weighted means: Var(ybar) =
      # phi * sum(w_i V(mu_i)) / (sum w_i)^2, the usual weighted-mean form.
      se_observed   = if (use_rate) sqrt(sum(var_unit)) / sum(weight)
                      else sqrt(sum(prior_w * var_unit)) / sum(prior_w),
      n             = n(),
      .groups       = "drop"
    ) %>%
    mutate(se_observed = sqrt(phi) * se_observed)

  if (x_is_numeric) {
    agg <- agg %>% arrange(x_plot)
  } else {
    agg <- agg %>%
      mutate(bin_group = factor(bin_group, levels = levels(df$x_var))) %>%
      arrange(bin_group) %>% mutate(x_plot = as.character(bin_group))
  }

  scatter_mode <- if (x_is_numeric) "lines+markers" else "markers"

  # Labels
  default_metric <- if (use_rate) "Frequency" else paste("Avg.", response_name)
  if (is.null(title)) title <- paste(default_metric, "\u2013", predictor)
  if (is.null(xlab))  xlab  <- predictor
  if (is.null(ylab))  ylab  <- default_metric

  xaxis_cfg <- list(title = xlab, tickangle = -45,
                    tickfont = list(size = 9), showgrid = FALSE)
  if (!x_is_numeric) {
    xaxis_cfg$categoryorder <- "array"
    xaxis_cfg$categoryarray <- agg$x_plot
  }

  p <- plot_ly(agg, x = ~x_plot)

  if (has_weight) {
    p <- p %>% add_bars(
      y = ~weight_sum, name = w_title, yaxis = "y2",
      marker = list(color = ta_lightblue, opacity = 0.5, line = list(width = 0)),
      hovertemplate = paste0("<b>", xlab, ":</b> %{x}<br><b>", w_title, ":</b> %{y:,.0f}<extra></extra>")
    )
  }

  # Error bars on the observed points, so a gap between the two lines can
  # be read against the noise the bin actually carries. Thin bins get long
  # bars, which is the honest picture: they were never evidence.
  err_cfg <- if (isTRUE(ci)) list(
    type = "data", array = agg$se_observed * stats::qnorm(1 - (1 - ci_level) / 2),
    color = color, thickness = 1.2, width = 3) else NULL

  p <- p %>% add_trace(
    y = ~avg_observed, name = "Observed", yaxis = "y",
    type = "scatter", mode = scatter_mode, error_y = err_cfg,
    line   = list(color = color, width = 2.2),
    marker = list(color = color, size = 7),
    hovertemplate = paste0("<b>", xlab, ":</b> %{x}",
                           "<br><b>Observed:</b> %{y:.", metric_fmt, "f}<extra></extra>")
  )

  p <- p %>% add_trace(
    y = ~avg_predicted, name = "Predicted", yaxis = "y",
    type = "scatter", mode = scatter_mode,
    line   = list(color = color_pred, width = 2, dash = "dash"),
    marker = list(color = color_pred, size = 6, symbol = "diamond"),
    hovertemplate = paste0("<b>", xlab, ":</b> %{x}",
                           "<br><b>Predicted:</b> %{y:.", metric_fmt, "f}<extra></extra>")
  )

  p %>%
    layout(
      title  = list(text = title, font = list(color = ta_navy, size = 14)),
      xaxis  = xaxis_cfg,
      yaxis  = list(title = ylab, gridcolor = "#D0D8E0", zeroline = FALSE,
                    range = y_range, autorange = is.null(y_range)),
      yaxis2 = list(title = w_title, overlaying = "y", side = "right",
                    showgrid = FALSE,
                    tickfont  = list(color = ta_muted),
                    titlefont = list(color = ta_muted)),
      legend       = list(orientation = "h", y = -0.2),
      hoverlabel   = list(bgcolor = "white", font = list(size = 12)),
      barmode      = "overlay",
      plot_bgcolor = "white",
      paper_bgcolor= "white",
      margin       = list(b = 80, r = 80)
    ) %>%
    config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions   = list(format = "png", filename = "plot")
    )
}
