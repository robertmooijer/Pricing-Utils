#' Premium impact (dislocation) analysis
#'
#' Compares premiums under a new model set against an old model set (or an
#' existing premium column) on a per-policy basis. Premiums are computed as
#' rates per unit of exposure (offsets neutralised at exposure = 1); the
#' premium is the product of the models supplied (frequency x severity, or
#' a single model when only one side is modelled).
#'
#' By default the new premiums are rebased so that the exposure-weighted
#' totals match the old ones. That isolates the redistribution
#' (dislocation) from the overall rate-level change, which is reported
#' separately in the summary.
#'
#' @param data Dataset with raw rows.
#' @param model_freq_new,model_sev_new The new (candidate) models.
#' @param model_freq_old,model_sev_old The old models, or:
#' @param old_premium_col Column in `data` with the current premium.
#' @param old_premium_basis `"amount"` (default; premium for the record,
#'   divided by exposure internally) or `"rate"` (already per unit of
#'   exposure).
#' @param rebase `TRUE` = scale new premiums to the old total (default).
#' @param by Optional character vector of columns for a per-level impact
#'   breakdown (exposure-weighted mean change).
#' @param n_show Number of rows in the winners/losers tables (default 10).
#' @param exposure_col Exposure column (default `"Exposure"`).
#'
#' @return A list with `summary` (display table with the headline numbers),
#'   `stats` (the same numbers as a named list), `policy` (per-row
#'   `OldRate`, `NewRate`, `NewRateRebased`, `ChangePct`), `by_level`
#'   (exposure-weighted mean change per level, when `by` is given),
#'   `largest_increases` / `largest_decreases` (top-`n_show` dislocations)
#'   and `plot` (exposure-weighted histogram of the premium changes).
#' @export
premium_impact <- function(data,
                           model_freq_new = NULL, model_sev_new = NULL,
                           model_freq_old = NULL, model_sev_old = NULL,
                           old_premium_col   = NULL,
                           old_premium_basis = c("amount", "rate"),
                           rebase       = TRUE,
                           by           = NULL,
                           n_show       = 10,
                           exposure_col = "Exposure") {

  old_premium_basis <- match.arg(old_premium_basis)
  if (is.null(model_freq_new) && is.null(model_sev_new))
    stop("premium_impact: provide at least one NEW model.")
  has_old_models <- !is.null(model_freq_old) || !is.null(model_sev_old)
  if (!has_old_models && is.null(old_premium_col))
    stop("premium_impact: provide old models or 'old_premium_col'.")
  if (has_old_models && !is.null(old_premium_col))
    stop("premium_impact: provide either old models or 'old_premium_col', not both.")
  .check_cols(data, c(exposure_col, by, old_premium_col), "premium_impact")

  # Predicted rate per unit of exposure (offset neutralised at exposure = 1)
  rate_of <- function(model) {
    if (is.null(model)) return(rep(1, nrow(data)))
    nd <- data
    if (!is.null(model$offset)) {
      if (exposure_col %in% names(nd)) {
        nd[[exposure_col]] <- 1
      } else {
        warning("premium_impact: a model has an offset but column '",
                exposure_col, "' is not in 'data'; premiums are then not ",
                "per unit of exposure.", call. = FALSE)
      }
    }
    as.numeric(predict(model, newdata = nd, type = "response"))
  }

  new_rate <- rate_of(model_freq_new) * rate_of(model_sev_new)
  old_rate <- if (has_old_models) {
    rate_of(model_freq_old) * rate_of(model_sev_old)
  } else if (old_premium_basis == "amount") {
    data[[old_premium_col]] / data[[exposure_col]]
  } else {
    data[[old_premium_col]]
  }

  expo <- data[[exposure_col]]
  keep <- is.finite(new_rate) & is.finite(old_rate) & old_rate > 0 &
          is.finite(expo) & expo > 0
  n_drop <- sum(!keep)
  if (n_drop > 0)
    warning("premium_impact: ", n_drop, " row(s) dropped (NA or ",
            "non-positive premium/exposure).", call. = FALSE)
  if (!any(keep)) stop("premium_impact: no usable rows left.")

  idx      <- which(keep)
  new_rate <- new_rate[keep]; old_rate <- old_rate[keep]; expo <- expo[keep]

  old_total   <- sum(old_rate * expo)
  new_total   <- sum(new_rate * expo)
  rate_change <- new_total / old_total - 1
  scale_f     <- if (rebase) old_total / new_total else 1
  new_adj     <- new_rate * scale_f
  change      <- (new_adj / old_rate - 1) * 100

  # Exposure-weighted quantiles
  wq <- function(x, w, probs) {
    o <- order(x)
    stats::approx(cumsum(w[o]) / sum(w), x[o], xout = probs, rule = 2,
                  ties = "ordered")$y
  }
  qs      <- wq(change, expo, c(0.05, 0.25, 0.50, 0.75, 0.95))
  share10 <- sum(expo[abs(change) > 10]) / sum(expo)
  share25 <- sum(expo[abs(change) > 25]) / sum(expo)

  policy <- data.frame(Row = idx, stringsAsFactors = FALSE)
  if (!is.null(by)) policy <- cbind(policy, data[idx, by, drop = FALSE])
  policy$Exposure       <- expo
  policy$OldRate        <- old_rate
  policy$NewRate        <- new_rate
  policy$NewRateRebased <- new_adj
  policy$ChangePct      <- change
  rownames(policy) <- NULL

  by_level <- NULL
  if (!is.null(by)) {
    by_level <- do.call(rbind, lapply(by, function(v) {
      dtb <- data.table::data.table(Level = as.character(data[[v]][idx]),
                                    w = expo, ch = change)
      ag  <- dtb[, .(Exposure      = sum(w),
                     MeanChangePct = sum(ch * w) / sum(w)), by = Level]
      data.frame(Variable = v, as.data.frame(ag), stringsAsFactors = FALSE)
    }))
    rownames(by_level) <- NULL
  }

  ord   <- order(policy$ChangePct, decreasing = TRUE)
  n_top <- min(n_show, nrow(policy))
  largest_increases <- policy[ord[seq_len(n_top)], , drop = FALSE]
  largest_decreases <- policy[rev(ord)[seq_len(n_top)], , drop = FALSE]
  rownames(largest_increases) <- rownames(largest_decreases) <- NULL

  summary_df <- data.frame(
    Metric = c("Old total premium (rate x exposure)",
               "New total premium (before rebase)",
               "Overall rate-level change",
               "Rebased to old total",
               "Median change (exposure-weighted)",
               "P5 .. P95 change (exposure-weighted)",
               "Exposure share |change| > 10%",
               "Exposure share |change| > 25%",
               "Rows used / dropped"),
    Value = c(format(round(old_total), big.mark = ","),
              format(round(new_total), big.mark = ","),
              sprintf("%+.1f%%", 100 * rate_change),
              if (rebase) "yes" else "no",
              sprintf("%+.2f%%", qs[3]),
              sprintf("%+.1f%% .. %+.1f%%", qs[1], qs[5]),
              sprintf("%.1f%%", 100 * share10),
              sprintf("%.1f%%", 100 * share25),
              paste0(length(idx), " / ", n_drop)),
    stringsAsFactors = FALSE
  )

  med <- qs[3]

  # Pre-bin the histogram in R: a plotly histogram trace would embed all
  # raw per-policy values in the widget (tens of MB on large portfolios);
  # 60 pre-computed bars keep the file small regardless of n.
  rng <- range(change)
  if (diff(rng) < 1e-9) rng <- rng + c(-0.5, 0.5)
  brks   <- seq(rng[1], rng[2], length.out = 61)
  bin_id <- cut(change, breaks = brks, include.lowest = TRUE)
  h_exp  <- as.numeric(tapply(expo, bin_id, sum))
  h_exp[is.na(h_exp)] <- 0
  mids   <- (utils::head(brks, -1) + utils::tail(brks, -1)) / 2

  p <- plot_ly() %>%
    add_bars(x = mids, y = h_exp, width = diff(brks), name = "Exposure",
             marker = list(color = ta_blue, opacity = 0.75,
                           line = list(color = "white", width = 0.5)),
             hovertemplate = paste0("<b>Change:</b> %{x:.1f}%",
                                    "<br><b>Exposure:</b> %{y:,.0f}",
                                    "<extra></extra>")) %>%
    layout(
      title  = list(text = paste0("Premium impact",
                                  if (rebase) " (rebased to old total)" else "",
                                  sprintf(" \u2013 rate-level change %+.1f%%",
                                          100 * rate_change)),
                    font = list(color = ta_navy, size = 14)),
      xaxis  = list(title = "Premium change (%)", showgrid = FALSE,
                    zeroline = FALSE),
      yaxis  = list(title = "Exposure", gridcolor = "#D0D8E0",
                    zeroline = FALSE),
      shapes = list(
        list(type = "line", xref = "x", yref = "paper",
             x0 = 0, x1 = 0, y0 = 0, y1 = 1,
             line = list(color = ta_muted, width = 1, dash = "dot")),
        list(type = "line", xref = "x", yref = "paper",
             x0 = med, x1 = med, y0 = 0, y1 = 1,
             line = list(color = ta_gold, width = 1.5, dash = "dash"))),
      annotations = list(list(x = med, y = 1, xref = "x", yref = "paper",
                              text = sprintf("median %+.1f%%", med),
                              showarrow = FALSE, yanchor = "bottom",
                              font = list(color = ta_gold, size = 11))),
      hoverlabel   = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin       = list(b = 60, r = 40)
    ) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "impact"))

  list(summary           = summary_df,
       stats             = list(old_total = old_total, new_total = new_total,
                                rate_change = rate_change, rebase = rebase,
                                quantiles = stats::setNames(qs,
                                  c("p5", "p25", "p50", "p75", "p95")),
                                share_gt_10 = share10, share_gt_25 = share25,
                                n_rows = length(idx), n_dropped = n_drop),
       policy            = policy,
       by_level          = by_level,
       largest_increases = largest_increases,
       largest_decreases = largest_decreases,
       plot              = p)
}
