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
#' income Gini, which orders by the variable being measured. A window with
#' no losses at all has no Lorenz curve to measure, and `gini` is then
#' `NA`.
#'
#' Bins are cut on the ordered predictions, so with fewer distinct
#' predicted values than bins the policies sharing a prediction are split
#' across bins in arbitrary order. That is reported with a warning rather
#' than drawn as if the model had that resolution.
#'
#' @param model_freq,model_sev Fitted glm objects. Supply both for a risk
#'   premium, or one for a frequency- or severity-only lift. The exposure
#'   offset is taken out, so the prediction is a rate per unit of exposure;
#'   any other offset stays in by default (see `keep_offsets`).
#' @param data Data to evaluate on. `NULL` (default) uses the model's own
#'   rows, which is in-sample and is labelled as such on the plot; pass a
#'   holdout for an honest out-of-sample lift.
#' @param actual_col Column with the realised amount to compare against:
#'   the loss amount for a risk premium or severity model, the claim count
#'   for a frequency model.
#' @param exposure_col Exposure column (default `"Exposure"`). This is
#'   also the column held at 1 to neutralise a model's offset.
#' @param claims_col Claim count column (default `"AantalClaims"`), used
#'   as the weight for a severity-only lift.
#' @param weight_col Column to bin and weight on. `NULL` (default) picks
#'   it from the models: `claims_col` for a severity-only lift, otherwise
#'   `exposure_col`. A severity model predicts a mean **per claim**, so
#'   weighting it by exposure multiplies a per-claim amount by a per-year
#'   one and the totals do not reconcile; frequency and risk premium are
#'   rates per exposure unit and keep the exposure. Set this explicitly
#'   only to override that choice.
#' @param n_bins Number of equal-weight bins (default 10).
#' @param y_range Optional `c(lo, hi)` to fix the rate axis.
#' @param keep_offsets `TRUE` (default): only the exposure offset is taken
#'   out, and any other offset - a bonus-malus scale carried as
#'   `offset(log(BM))` - stays in the prediction. The claims being compared
#'   against happened at each policy's own BM, so the prediction has to
#'   include it: taking it out as well put every policy at the class
#'   without discount, and a correct model then read about as much too
#'   expensive as the average discount, in every bin. `FALSE` takes every
#'   offset out. `stats$offsets_kept` and the axis title say which stayed
#'   in.
#' @param n_boot Bootstrap replicates for an interval around the Gini
#'   (default 200; 0 switches it off). A Gini on raw losses is dominated by
#'   a few large claims: on a 16,000-policy holdout the 90% interval ran
#'   from 0.14 to 0.33 around 0.23, wider than the difference between two
#'   models one might want to choose between. Policies are resampled with
#'   Poisson(1) counts, so the book is sorted once and a replicate is one
#'   pass over it. The interval is in the title and in `gini_ci`. When it
#'   is wider than half the Gini and wider than 0.05 **and** the ten
#'   largest records carry 10% or more of `actual_col`, a warning says so,
#'   since capping large losses is then the usual remedy; a wide interval
#'   on a small book has no such remedy and only shows in the title.
#' @param conf_level Coverage of that interval (default 0.90).
#' @param seed Optional seed for the bootstrap.
#' @param at Optional named list of values to price every policy at, e.g.
#'   `list(BOEKJAAR = 2024)`; see [premium_impact()]. For a lift against
#'   the claims of the data itself this is normally not wanted: those
#'   claims happened in their own year.
#'
#' @return A list with `table` (one row per bin), `gini`, `gini_ci`,
#'   `stats`, `plot`
#'   (the lift chart) and `plot_lorenz` (the curve behind the Gini). The
#'   `Exposure` and `ExposureShare` columns hold whatever `weight_col`
#'   resolved to, so on a severity-only lift they are claim counts; the
#'   axis titles name it.
#' @seealso [double_lift()] to compare two candidate tariffs.
#' @export
model_lift <- function(model_freq = NULL, model_sev = NULL, data = NULL,
                       actual_col = "SCHADELAST",
                       exposure_col = "Exposure",
                       claims_col = "AantalClaims",
                       weight_col = NULL,
                       n_bins = 10, y_range = NULL,
                       keep_offsets = TRUE,
                       n_boot = 200, conf_level = 0.90, seed = NULL,
                       at = NULL) {

  if (is.null(model_freq) && is.null(model_sev))
    stop("model_lift: provide at least one model.", call. = FALSE)
  if (!is.logical(keep_offsets) || length(keep_offsets) != 1 ||
      is.na(keep_offsets))
    stop("model_lift: 'keep_offsets' must be TRUE or FALSE.", call. = FALSE)
  .check_boot(n_boot, conf_level, "model_lift")
  # [CONV-08]
  if (!is.null(seed)) set.seed(seed)

  # [RATE-07]
  # The weight has to be the denominator the prediction is a rate over. A
  # severity model predicts a mean per claim, so weighting it by exposure
  # multiplies a per-claim amount by a per-year one and the totals do not
  # reconcile at all. Frequency and risk premium are rates per exposure
  # unit, so those keep the exposure.
  sev_only <- is.null(model_freq) && !is.null(model_sev)
  if (is.null(weight_col)) weight_col <- if (sev_only) claims_col
                                         else exposure_col
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
  .check_cols(data, c(actual_col, exposure_col, weight_col), "model_lift")

  # [RT-13]
  .nil_claims_warning(model_freq, model_sev, data, claims_col, actual_col,
                      "model_lift")
  # [OFF-05] [RATE-10]
  pd   <- .apply_at(data, at, "model_lift")
  rate <- .model_rate(model_freq, pd, exposure_col, "model_lift",
                      keep_offsets, "frequency") *
          .model_rate(model_sev,  pd, exposure_col, "model_lift",
                      keep_offsets, "severity")
  kept <- .kept_offsets(list(model_freq, model_sev), exposure_col,
                        keep_offsets)
  w    <- as.numeric(data[[weight_col]])
  act  <- as.numeric(data[[actual_col]])

  keep <- is.finite(rate) & is.finite(w) & w > 0 & is.finite(act)
  n_drop <- sum(!keep)
  # [RATE-05]
  if (n_drop > 0)
    warning("model_lift: ", n_drop, " row(s) dropped (",
            .drop_reasons(stats::setNames(
              list(!is.finite(rate), !(is.finite(w) & w > 0),
                   !is.finite(act)),
              c("no usable prediction",
                paste0("no positive '", weight_col, "'"),
                paste0("missing '", actual_col, "'")))), ").",
            call. = FALSE)
  if (!any(keep)) stop("model_lift: no usable rows left.", call. = FALSE)
  rate <- rate[keep]; w <- w[keep]; act <- act[keep]

  .check_ae_scale(sum(act), sum(rate * w), "model_lift", actual_col)

  bin <- .exposure_bins(rate, w, n_bins, "model_lift")
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
  # [RATE-11]
  # A point Gini on raw losses moves with a handful of large claims, so it
  # comes with an interval. A wide one is worth a warning when large losses
  # are the reason, because capping them is then the remedy; a wide one on
  # a small book is plain sampling noise and shows in the title only.
  gini_ci <- .gini_boot(rate, act, w, n_boot, conf_level)
  note    <- .large_loss_note(act, actual_col)
  if (all(is.finite(c(gini, gini_ci))) && nzchar(note) &&
      diff(gini_ci) > max(0.5 * abs(gini), 0.05))
    warning(sprintf(paste0(
      "model_lift: the %.0f%% interval for the Gini runs from %.3f to %.3f ",
      "around %.3f, so the point value says little on its own."),
      100 * conf_level, gini_ci[1], gini_ci[2], gini), note, call. = FALSE)
  lbl  <- if (in_sample) " (in-sample)" else " (out-of-sample)"
  ci_lab <- if (all(is.finite(gini_ci)))
    sprintf(" (%.0f%%: %.3f to %.3f)", 100 * conf_level, gini_ci[1],
            gini_ci[2]) else ""
  # Every axis names the weight actually in force, so a severity lift does
  # not claim to be per unit of exposure
  w_lab    <- if (identical(weight_col, exposure_col)) "exposure" else weight_col
  rate_lab <- paste0("Rate per unit of ", w_lab,
                     if (length(kept)) paste0(", incl. ",
                                              paste(kept, collapse = ", ")))

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
      title = list(text = paste0("Lift \u2013 ", n_bins, " bins of equal ",
                                 w_lab, lbl,
                                 sprintf("  \u00b7  Gini %.3f", gini),
                                 ci_lab),
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Bin, ordered by predicted rate",
                   dtick = 1, showgrid = FALSE),
      yaxis = list(title = rate_lab,
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
              hovertemplate = paste0(w_lab, ": %{x:.1%}",
                                     "<br>Losses: %{y:.1%}<extra></extra>")) %>%
    layout(
      title = list(text = sprintf("Lorenz curve%s  \u00b7  Gini %.3f%s",
                                  lbl, gini, ci_lab),
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = paste("Cumulative", w_lab), tickformat = ".0%",
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

  list(table = tbl, gini = gini, gini_ci = gini_ci,
       stats = list(gini = gini, gini_ci = gini_ci, conf_level = conf_level,
                    n_boot = n_boot, in_sample = in_sample, n_bins = n_bins,
                    n_rows = length(rate), n_dropped = n_drop,
                    total_actual = sum(act), total_predicted = sum(rate * w),
                    offsets_kept = kept),
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
#' @details
#' The verdict compares the mean absolute distance of each A/E series from
#' 1 across the bins. On raw losses a few large claims decide those means:
#' on a 16,000-policy holdout the verdict was "old" on the losses as
#' recorded and "new" on the same losses capped. The difference
#' `mad_old - mad_new` therefore gets a bootstrap interval, and `winner`
#' names a model only when that interval excludes 0; otherwise it is
#' `"inconclusive"`, with a warning when the ten largest records carry 10%
#' or more of `actual_col`. Policies are resampled with Poisson(1)
#' counts within the bins cut on the full data, and with `rebase = TRUE`
#' every replicate is rebased again.
#'
#' Bins hold equal **weight**. That is the exposure, unless both sides are
#' severity models: a severity model predicts a mean per claim, so it is
#' weighted by claims, as in [model_lift()]. A severity model on one side
#' and a frequency or risk premium on the other is refused, because the
#' ratio of a rate per claim to a rate per exposure unit compares nothing.
#'
#' @param data Data to evaluate on.
#' @param model_freq_new,model_sev_new The candidate models.
#' @param model_freq_old,model_sev_old The incumbent models, or:
#' @param old_premium_col Column holding the current premium.
#' @param old_premium_basis `"amount"` (default; divided by the weight
#'   column, normally the exposure, internally) or `"rate"` (already per
#'   unit of it).
#' @param actual_col Column with the realised amount (default
#'   `"SCHADELAST"`).
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param claims_col Claim count column (default `"AantalClaims"`), the
#'   weight when both sides are severity models.
#' @param weight_col Column to bin and weight on. `NULL` (default) picks it
#'   as [model_lift()] does: `claims_col` when both sides are severity
#'   models, otherwise `exposure_col`. Set it only to override that choice.
#' @param n_bins Number of equal-weight bins (default 10).
#' @param rebase `TRUE` (default) scales the new rates so the weighted
#'   totals match, isolating differentiation from level.
#' @param y_range Optional `c(lo, hi)` to fix the A/E axis.
#' @param keep_offsets `TRUE` (default): only the exposure offset is taken
#'   out of both rates, and any other offset - a bonus-malus scale - stays
#'   in, as it must for an A/E against claims that happened at each
#'   policy's own BM. An old premium column must then include the discount
#'   too. `FALSE` takes every offset out. See [premium_impact()].
#' @param n_boot Bootstrap replicates behind `winner` (default 200). `0`
#'   switches the bootstrap off, and `winner` then compares the two point
#'   values alone.
#' @param conf_level Coverage of the interval (default 0.90).
#' @param seed Optional seed for the bootstrap.
#' @param at Optional named list of values to price every policy at, e.g.
#'   `list(BOEKJAAR = 2024)`; see [premium_impact()]. The models price the
#'   data with these values; an old premium column is read as it is.
#'
#' @return A list with `table` (per bin: weight, the mean rate ratio, and
#'   the A/E of each model; the `Exposure` column holds whatever
#'   `weight_col` resolved to), `stats`, and `plot`.
#'   `stats$rate_level_change` is measured before any rebasing, so it
#'   reports the difference in rate level whether or not `rebase` is in
#'   force. `stats$mad_diff` is `mad_old - mad_new`, positive when the new
#'   model is closer to 1, `stats$mad_diff_ci` its interval, and
#'   `stats$winner` is `"new"`, `"old"`, `"inconclusive"` or `"tie"`.
#' @seealso [model_lift()], [premium_impact()]
#' @export
double_lift <- function(data,
                        model_freq_new = NULL, model_sev_new = NULL,
                        model_freq_old = NULL, model_sev_old = NULL,
                        old_premium_col = NULL,
                        old_premium_basis = c("amount", "rate"),
                        actual_col = "SCHADELAST",
                        exposure_col = "Exposure",
                        claims_col = "AantalClaims",
                        weight_col = NULL,
                        n_bins = 10, rebase = TRUE, y_range = NULL,
                        keep_offsets = TRUE,
                        n_boot = 200, conf_level = 0.90, seed = NULL,
                        at = NULL) {

  old_premium_basis <- match.arg(old_premium_basis)
  if (!is.logical(keep_offsets) || length(keep_offsets) != 1 ||
      is.na(keep_offsets))
    stop("double_lift: 'keep_offsets' must be TRUE or FALSE.", call. = FALSE)
  .check_boot(n_boot, conf_level, "double_lift")
  # [CONV-08]
  if (!is.null(seed)) set.seed(seed)
  y_range <- .check_range(y_range, "double_lift")
  if (n_bins < 2) stop("double_lift: 'n_bins' must be at least 2.",
                       call. = FALSE)

  # [RATE-12]
  # The weight follows the models, as in model_lift(). Both sides must be
  # rates over the same thing: a severity is per claim, a frequency or a
  # risk premium per unit of exposure, and the ratio of the two orders the
  # book by something that is neither tariff.
  sev_new <- is.null(model_freq_new) && !is.null(model_sev_new)
  has_old <- !is.null(model_freq_old) || !is.null(model_sev_old)
  sev_old <- is.null(model_freq_old) && !is.null(model_sev_old)
  if (has_old && sev_new != sev_old)
    stop("double_lift: one side is a severity model and the other is not, ",
         "so one rate is per claim and the other per unit of exposure. ",
         "Compare severity with severity, or frequency x severity with ",
         "frequency x severity.", call. = FALSE)
  if (is.null(weight_col)) weight_col <- if (sev_new) claims_col
                                         else exposure_col
  .check_cols(data, c(exposure_col, weight_col, actual_col, old_premium_col),
              "double_lift")
  data <- .as_df(data)

  # An old premium amount is divided by the weight, so that rate x weight
  # gives the amount back, as it does for the models' side
  r <- .old_new_rates(data, model_freq_new, model_sev_new,
                      model_freq_old, model_sev_old, old_premium_col,
                      old_premium_basis, exposure_col, "double_lift",
                      keep_offsets, at = at, divisor_col = weight_col)
  w   <- as.numeric(data[[weight_col]])
  act <- as.numeric(data[[actual_col]])

  keep <- is.finite(r$new) & is.finite(r$old) & r$old > 0 &
          is.finite(w) & w > 0 & is.finite(act)
  n_drop <- sum(!keep)
  # [RATE-05]
  if (n_drop > 0)
    warning("double_lift: ", n_drop, " row(s) dropped (",
            .drop_reasons(stats::setNames(
              list(!is.finite(r$new), !(is.finite(r$old) & r$old > 0),
                   !(is.finite(w) & w > 0), !is.finite(act)),
              c("no usable new rate", "no usable old rate",
                paste0("no positive '", weight_col, "'"),
                paste0("missing '", actual_col, "'")))), ").",
            call. = FALSE)
  if (!any(keep)) stop("double_lift: no usable rows left.", call. = FALSE)
  new <- r$new[keep]; old <- r$old[keep]; w <- w[keep]; act <- act[keep]

  .check_ae_scale(sum(act), sum(old * w), "double_lift", actual_col)

  # [RATE-03]
  # The rate-level difference is a property of the two tariffs, not of the
  # decision to rebase, so it is measured before any scaling. Deriving it
  # from scale_f would report exactly 0 whenever rebase = FALSE, which is
  # the one case where the level difference is still in the chart.
  rate_level_change <- sum(new * w) / sum(old * w) - 1
  scale_f <- if (rebase) sum(old * w) / sum(new * w) else 1
  new_raw <- new
  new     <- new * scale_f
  ratio   <- new / old

  bin <- .exposure_bins(ratio, w, n_bins, "double_lift")
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

  # [RATE-09]
  # Mean absolute distance from 1: the smaller, the better calibrated the
  # model is across the range where the two disagree
  mad_new  <- mean(abs(tbl$AE_New - 1), na.rm = TRUE)
  mad_old  <- mean(abs(tbl$AE_Old - 1), na.rm = TRUE)
  mad_diff <- mad_old - mad_new

  # [RATE-11]
  # How much of that difference survives resampling the policies. The bins
  # stay as cut on the full data; every replicate reweights the policies
  # within them and, with rebase, rebases again.
  diff_ci <- c(NA_real_, NA_real_)
  if (n_boot > 0) {
    mad_of <- function(a, p) mean(abs(ifelse(p > 0, a / p, NA_real_) - 1),
                                  na.rm = TRUE)
    pn0  <- new_raw * w
    po0  <- old * w
    sims <- vapply(seq_len(n_boot), function(b) {
      k  <- stats::rpois(length(act), 1)
      pn <- pn0 * k
      po <- po0 * k
      if (rebase && sum(pn) > 0) pn <- pn * sum(po) / sum(pn)
      s  <- rowsum(cbind(act * k, pn, po), bin, reorder = FALSE)
      mad_of(s[, 1], s[, 3]) - mad_of(s[, 1], s[, 2])
    }, numeric(1))
    diff_ci <- stats::quantile(sims, c((1 - conf_level) / 2,
                                       1 - (1 - conf_level) / 2),
                               na.rm = TRUE, names = FALSE)
  }

  # [RATE-09] [RATE-11]
  # A winner only where the interval excludes 0. Without a bootstrap the
  # point values decide, as they did before the interval existed.
  boot_ok <- n_boot > 0 && all(is.finite(diff_ci))
  winner  <- if (isTRUE(all.equal(mad_new, mad_old))) "tie"
             else if (boot_ok) {
               if (diff_ci[1] > 0) "new" else if (diff_ci[2] < 0) "old"
               else "inconclusive"
             } else if (mad_new < mad_old) "new" else "old"
  # Said out loud when large losses decide it, the case where the point
  # values flip with capping; otherwise the verdict and the title say it
  note <- .large_loss_note(act, actual_col)
  if (identical(winner, "inconclusive") && nzchar(note))
    warning(sprintf(paste0(
      "double_lift: the point values favour the %s model (mean |A/E - 1| ",
      "new %.3f, old %.3f), but the %.0f%% interval for the difference ",
      "runs from %.3f to %.3f and includes 0, so 'winner' is ",
      "\"inconclusive\"."),
      if (mad_new < mad_old) "new" else "old", mad_new, mad_old,
      100 * conf_level, diff_ci[1], diff_ci[2]), note, call. = FALSE)
  verdict <- if (boot_ok)
    sprintf("<br><sup>old - new %.3f (%.0f%%: %.3f to %.3f): %s</sup>",
            mad_diff, 100 * conf_level, diff_ci[1], diff_ci[2], winner)
    else ""

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
          "Double lift \u2013 mean |A/E - 1|: new %.3f vs old %.3f%s",
          mad_new, mad_old, verdict),
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
       stats = list(mad_new = mad_new, mad_old = mad_old,
                    mad_diff = mad_diff, mad_diff_ci = diff_ci,
                    winner = winner, conf_level = conf_level,
                    n_boot = n_boot, rebase = rebase,
                    rate_level_change = rate_level_change,
                    n_bins = n_bins, n_rows = length(new),
                    n_dropped = n_drop, weight_col = weight_col,
                    offsets_kept = r$kept),
       plot = p)
}
