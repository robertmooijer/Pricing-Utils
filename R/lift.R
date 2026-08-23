# Portfolio-level model comparison -------------------------------------------
#
# Every other diagnostic in the package looks at one variable or one pair of
# variables. These two answer the question those cannot: does the model
# separate risk across the book, and is the candidate better than what it
# replaces.

#' Lift chart and Gini for a risk premium model
#'
#' Sorts the portfolio by predicted risk premium, splits it into bins of
#' equal exposure, and compares the actual with the predicted rate in each.
#' A model that separates risk produces a rising actual line that tracks the
#' predicted one; a flat actual line means the model orders nothing.
#'
#' Bins hold equal **exposure** rather than equal policy counts, so each
#' point carries the same weight in the comparison, and both series are on
#' the same scale so they share one axis.
#'
#' @details
#' `gini` is computed on the exposure-weighted Lorenz curve: cumulative
#' share of actual losses against cumulative share of exposure, ordered by
#' the prediction. Zero means the model does not discriminate at all;
#' higher means it separates risk better. It measures **ordering only** -
#' a model can have an excellent Gini and still be badly calibrated, which
#' is what the lift chart itself shows. Note this is not the classical
#' income Gini, which orders by the variable being measured.
#'
#' @param model_freq,model_sev Fitted glm objects. Supply both for a risk
#'   premium, or one for a frequency- or severity-only lift. Offsets are
#'   neutralised, so the prediction is a rate per unit of exposure.
#' @param data Data to evaluate on. `NULL` (default) uses the model's own
#'   rows, which is in-sample and is labelled as such on the plot; pass a
#'   holdout for an honest out-of-sample lift.
#' @param actual_col Column with the realised amount to compare against:
#'   the loss amount for a risk premium or severity model, the claim count
#'   for a frequency model.
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param n_bins Number of equal-exposure bins (default 10).
#' @param y_range Optional `c(lo, hi)` to fix the rate axis.
#'
#' @return A list with `table` (one row per bin), `gini`, `stats`, `plot`
#'   (the lift chart) and `plot_lorenz` (the curve behind the Gini).
#' @seealso [double_lift()] to compare two candidate tariffs.
#' @export
model_lift <- function(model_freq = NULL, model_sev = NULL, data = NULL,
                       actual_col = "SCHADELAST",
                       exposure_col = "Exposure",
                       n_bins = 10, y_range = NULL) {

  if (is.null(model_freq) && is.null(model_sev))
    stop("model_lift: provide at least one model.", call. = FALSE)
  y_range <- .check_range(y_range, "model_lift")
  if (n_bins < 2) stop("model_lift: 'n_bins' must be at least 2.",
                       call. = FALSE)

  in_sample <- is.null(data)
  if (in_sample) {
    src <- .glm_training_data(if (!is.null(model_freq)) model_freq else
                              model_sev, "model_lift")
    if (is.null(src$data))
      stop("model_lift: training data cannot be recovered; fit the model ",
           "with a 'data =' argument or pass 'data' explicitly.",
           call. = FALSE)
    data <- src$data
  } else {
    data <- .as_df(data)
  }
  .check_cols(data, c(actual_col, exposure_col), "model_lift")

  rate <- .model_rate(model_freq, data, exposure_col, "model_lift") *
          .model_rate(model_sev,  data, exposure_col, "model_lift")
  w    <- as.numeric(data[[exposure_col]])
  act  <- as.numeric(data[[actual_col]])

  keep <- is.finite(rate) & is.finite(w) & w > 0 & is.finite(act)
  n_drop <- sum(!keep)
  if (n_drop > 0)
    warning("model_lift: ", n_drop, " row(s) dropped (non-finite or ",
            "non-positive exposure).", call. = FALSE)
  if (!any(keep)) stop("model_lift: no usable rows left.", call. = FALSE)
  rate <- rate[keep]; w <- w[keep]; act <- act[keep]

  .check_ae_scale(sum(act), sum(rate * w), "model_lift", actual_col)

  bin <- .exposure_bins(rate, w, n_bins)
  agg <- data.table::data.table(bin = bin, w = w, act = act,
                                pred = rate * w)[
    , .(Exposure = sum(w), Actual = sum(act), Predicted = sum(pred)),
    by = bin][order(bin)]
  tbl <- data.frame(
    Bin           = agg$bin,
    Exposure      = agg$Exposure,
    ExposureShare = agg$Exposure / sum(agg$Exposure),
    ActualRate    = agg$Actual / agg$Exposure,
    PredictedRate = agg$Predicted / agg$Exposure,
    AE            = ifelse(agg$Predicted > 0, agg$Actual / agg$Predicted,
                           NA_real_),
    stringsAsFactors = FALSE)

  gini <- .gini_exposure(rate, act, w)
  lbl  <- if (in_sample) " (in-sample)" else " (out-of-sample)"

  ht <- paste0("<b>Bin %{x}</b><br>%{data.name}: %{y:.2f}",
               "<extra></extra>")
  p <- plot_ly() %>%
    add_trace(x = tbl$Bin, y = tbl$PredictedRate, name = "Predicted",
              type = "scatter", mode = "lines+markers",
              line = list(color = ta_gold, width = 2, dash = "dash"),
              marker = list(color = ta_gold, size = 8, symbol = "diamond"),
              hovertemplate = ht) %>%
    add_trace(x = tbl$Bin, y = tbl$ActualRate, name = "Actual",
              type = "scatter", mode = "lines+markers",
              line = list(color = ta_blue, width = 2),
              marker = list(color = ta_blue, size = 8),
              hovertemplate = ht) %>%
    layout(
      title = list(text = paste0("Lift \u2013 ", n_bins,
                                 " equal-exposure bins", lbl,
                                 sprintf("  \u00b7  Gini %.3f", gini)),
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Bin, ordered by predicted rate",
                   dtick = 1, showgrid = FALSE),
      yaxis = list(title = "Rate per unit of exposure",
                   gridcolor = "#D0D8E0", zeroline = FALSE,
                   range = y_range, autorange = is.null(y_range)),
      legend = list(orientation = "h", y = -0.2),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 70, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "lift"))

  o  <- order(rate)
  cx <- c(0, cumsum(w[o]) / sum(w))
  cy <- c(0, cumsum(act[o]) / sum(act))
  step <- unique(round(seq(1, length(cx), length.out = 400)))
  p_lor <- plot_ly() %>%
    add_trace(x = c(0, 1), y = c(0, 1), name = "No discrimination",
              type = "scatter", mode = "lines",
              line = list(color = ta_muted, width = 1, dash = "dot"),
              hoverinfo = "skip") %>%
    add_trace(x = cx[step], y = cy[step], name = "Lorenz",
              type = "scatter", mode = "lines",
              line = list(color = ta_navy, width = 2),
              hovertemplate = paste0("Exposure: %{x:.1%}",
                                     "<br>Losses: %{y:.1%}<extra></extra>")) %>%
    layout(
      title = list(text = sprintf("Lorenz curve%s  \u00b7  Gini %.3f",
                                  lbl, gini),
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Cumulative exposure", tickformat = ".0%",
                   showgrid = FALSE),
      yaxis = list(title = "Cumulative losses", tickformat = ".0%",
                   gridcolor = "#D0D8E0", zeroline = FALSE),
      legend = list(orientation = "h", y = -0.2),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 70, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "lorenz"))

  list(table = tbl, gini = gini,
       stats = list(gini = gini, in_sample = in_sample, n_bins = n_bins,
                    n_rows = length(rate), n_dropped = n_drop,
                    total_actual = sum(act), total_predicted = sum(rate * w)),
       plot = p, plot_lorenz = p_lor)
}

#' Double lift chart: which of two tariffs is right where they disagree
#'
#' Sorts the portfolio by the ratio of the two predicted rates, bins it by
#' equal exposure, and shows the actual-over-expected of **each** model per
#' bin. The bins at the ends are the policies where the two tariffs
#' disagree most, and that is where the comparison is decided: the model
#' whose line stays closer to 1.0 across the range is the better one.
#'
#' This is the test a single lift chart cannot do. Both models can look
#' convincing on their own lift chart while still disagreeing sharply about
#' individual policies, and only their disagreement reveals which one is
#' right.
#'
#' By default both sides are rebased to the same total, so the chart is
#' about differentiation rather than rate level.
#'
#' @param data Data to evaluate on.
#' @param model_freq_new,model_sev_new The candidate models.
#' @param model_freq_old,model_sev_old The incumbent models, or:
#' @param old_premium_col Column holding the current premium.
#' @param old_premium_basis `"amount"` (default; divided by exposure
#'   internally) or `"rate"` (already per unit of exposure).
#' @param actual_col Column with the realised amount (default
#'   `"SCHADELAST"`).
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param n_bins Number of equal-exposure bins (default 10).
#' @param rebase `TRUE` (default) scales the new rates so the
#'   exposure-weighted totals match, isolating differentiation from level.
#' @param y_range Optional `c(lo, hi)` to fix the A/E axis.
#'
#' @return A list with `table` (per bin: exposure, the mean rate ratio, and
#'   the A/E of each model), `stats`, and `plot`.
#' @seealso [model_lift()], [premium_impact()]
#' @export
double_lift <- function(data,
                        model_freq_new = NULL, model_sev_new = NULL,
                        model_freq_old = NULL, model_sev_old = NULL,
                        old_premium_col = NULL,
                        old_premium_basis = c("amount", "rate"),
                        actual_col = "SCHADELAST",
                        exposure_col = "Exposure",
                        n_bins = 10, rebase = TRUE, y_range = NULL) {

  old_premium_basis <- match.arg(old_premium_basis)
  y_range <- .check_range(y_range, "double_lift")
  if (n_bins < 2) stop("double_lift: 'n_bins' must be at least 2.",
                       call. = FALSE)
  .check_cols(data, c(exposure_col, actual_col, old_premium_col),
              "double_lift")
  data <- .as_df(data)

  r <- .old_new_rates(data, model_freq_new, model_sev_new,
                      model_freq_old, model_sev_old, old_premium_col,
                      old_premium_basis, exposure_col, "double_lift")
  w   <- as.numeric(data[[exposure_col]])
  act <- as.numeric(data[[actual_col]])

  keep <- is.finite(r$new) & is.finite(r$old) & r$old > 0 &
          is.finite(w) & w > 0 & is.finite(act)
  n_drop <- sum(!keep)
  if (n_drop > 0)
    warning("double_lift: ", n_drop, " row(s) dropped (non-finite or ",
            "non-positive rate/exposure).", call. = FALSE)
  if (!any(keep)) stop("double_lift: no usable rows left.", call. = FALSE)
  new <- r$new[keep]; old <- r$old[keep]; w <- w[keep]; act <- act[keep]

  .check_ae_scale(sum(act), sum(old * w), "double_lift", actual_col)

  scale_f <- if (rebase) sum(old * w) / sum(new * w) else 1
  new     <- new * scale_f
  ratio   <- new / old

  bin <- .exposure_bins(ratio, w, n_bins)
  agg <- data.table::data.table(
    bin = bin, w = w, act = act, ratio = ratio,
    pn = new * w, po = old * w)[
      , .(Exposure = sum(w), Actual = sum(act),
          PredNew = sum(pn), PredOld = sum(po),
          Ratio = sum(ratio * w) / sum(w)), by = bin][order(bin)]

  tbl <- data.frame(
    Bin        = agg$bin,
    Exposure   = agg$Exposure,
    RateRatio  = agg$Ratio,
    ActualRate = agg$Actual / agg$Exposure,
    AE_New     = ifelse(agg$PredNew > 0, agg$Actual / agg$PredNew, NA_real_),
    AE_Old     = ifelse(agg$PredOld > 0, agg$Actual / agg$PredOld, NA_real_),
    stringsAsFactors = FALSE)

  # Mean absolute distance from 1: the smaller, the better calibrated the
  # model is across the range where the two disagree
  mad_new <- mean(abs(tbl$AE_New - 1), na.rm = TRUE)
  mad_old <- mean(abs(tbl$AE_Old - 1), na.rm = TRUE)
  winner  <- if (isTRUE(all.equal(mad_new, mad_old))) "tie"
             else if (mad_new < mad_old) "new" else "old"

  ht <- function(nm) paste0("<b>Bin %{x}</b><br>", nm, ": %{y:.3f}",
                            "<br>Rate ratio: %{text:.3f}<extra></extra>")
  p <- plot_ly() %>%
    add_trace(x = tbl$Bin, y = tbl$AE_Old, name = "A/E old",
              type = "scatter", mode = "lines+markers",
              line = list(color = ta_muted, width = 2, dash = "dash"),
              marker = list(color = ta_muted, size = 8, symbol = "diamond"),
              text = tbl$RateRatio, hovertemplate = ht("A/E old")) %>%
    add_trace(x = tbl$Bin, y = tbl$AE_New, name = "A/E new",
              type = "scatter", mode = "lines+markers",
              line = list(color = ta_blue, width = 2),
              marker = list(color = ta_blue, size = 8),
              text = tbl$RateRatio, hovertemplate = ht("A/E new")) %>%
    layout(
      title = list(
        text = sprintf(
          "Double lift \u2013 mean |A/E - 1|: new %.3f vs old %.3f",
          mad_new, mad_old),
        font = list(color = ta_navy, size = 14)),
      xaxis = list(title = paste0("Bin, ordered by new/old rate ratio ",
                                  "(left = new is cheaper)"),
                   dtick = 1, showgrid = FALSE),
      yaxis = list(title = "Actual / Expected", gridcolor = "#D0D8E0",
                   zeroline = FALSE, range = y_range,
                   autorange = is.null(y_range)),
      shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                         y0 = 1, y1 = 1,
                         line = list(color = ta_muted, width = 1,
                                     dash = "dot"))),
      legend = list(orientation = "h", y = -0.2),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 80, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png",
                                       filename = "double-lift"))

  list(table = tbl,
       stats = list(mad_new = mad_new, mad_old = mad_old, winner = winner,
                    rebase = rebase, rate_level_change = 1 / scale_f - 1,
                    n_bins = n_bins, n_rows = length(new),
                    n_dropped = n_drop),
       plot = p)
}
