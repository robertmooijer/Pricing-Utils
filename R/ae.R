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
#' Numeric predictors are binned with quantile bins by default (each bin
#' holds roughly the same number of observations, avoiding noisy thin
#' tails); `bin_type = "width"` restores equal-width binning.
#'
#' @param model A fitted glm object.
#' @param predictor Name of the predictor (string).
#' @param n_bins Number of bins for numeric predictors (default 150).
#' @param weight_var Optional: weight/exposure column in `model$data`
#'   (override; an unknown name is an error).
#' @param weight_label Optional: axis title for the bars.
#' @param color,color_pred Colours for the observed and predicted lines.
#' @param title,ylab,xlab Optional labels.
#' @param metric_fmt Number of decimals in the tooltip (default 4).
#' @param bin_type `"quantile"` (default) or `"width"`.
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
                               bin_type = c("quantile", "width")) {

  bin_type <- match.arg(bin_type)
  if (!inherits(model, "glm")) stop("'model' must be a glm object.")
  if (n_bins < 2) stop("plot_glm_predictor: 'n_bins' must be at least 2.")

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

  # Determine weight/exposure vector + mode
  fam        <- family(model)
  has_offset <- !is.null(tr$offset) && any(tr$offset != 0)

  if (has_offset && identical(fam$link, "log")) {
    w        <- exp(tr$offset)           # offset(log(Exposure)) -> Exposure
    use_rate <- TRUE                     # response & predict are COUNTS -> /exposure
    w_title  <- "Exposure"
  } else {
    if (has_offset)
      warning("plot_glm_predictor: an offset is present but the link is ",
              "not 'log'; exp(offset) is then not an exposure. The prior ",
              "weights are used instead.", call. = FALSE)
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

  # Grouping
  if (is.numeric(df$x_var)) {
    if (bin_type == "quantile") {
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
  }

  # Aggregation:
  # frequency:  sum(counts) / sum(exposure)
  # severity etc.: weighted mean of the response with the GLM weights
  agg <- df %>%
    group_by(bin_group) %>%
    summarise(
      x_plot        = if (x_is_numeric) weighted.mean(x_var, weight) else dplyr::first(as.character(x_var)),
      avg_observed  = if (use_rate) sum(observed)  / sum(weight) else weighted.mean(observed,  weight),
      avg_predicted = if (use_rate) sum(predicted) / sum(weight) else weighted.mean(predicted, weight),
      weight_sum    = sum(weight),
      n             = n(),
      .groups       = "drop"
    )

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

  p <- p %>% add_trace(
    y = ~avg_observed, name = "Observed", yaxis = "y",
    type = "scatter", mode = scatter_mode,
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
      yaxis  = list(title = ylab, gridcolor = "#D0D8E0", zeroline = FALSE),
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
