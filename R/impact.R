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
#'   breakdown. Besides the exposure-weighted mean change per level, this
#'   gives `Contribution = ExposureShare x MeanChangePct`, which sums over
#'   the levels of one variable to the portfolio change and so answers
#'   which segment is driving it.
#' @param spotlight Optional subset to report separately: either a logical
#'   vector with one value per row of `data`, or an expression evaluated
#'   against `data`, e.g. `REGIO == "Stad" & LEEFTIJD < 25`. It is a
#'   spotlight rather than a filter: the whole book is still analysed, and
#'   the subset appears as a second series on the histogram and as
#'   `$spotlight` in the result. Its statistics are computed after the
#'   book-level rebase, so they show how the subset moves relative to the
#'   portfolio.
#' @param n_show Number of rows in the winners/losers tables (default 10).
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param x_range,y_range Optional `c(lo, hi)` to fix the axes of the
#'   returned histogram, so impact plots for different scenarios become
#'   directly comparable. `x_range` bounds the premium change (%) - the
#'   axis you normally want to align - and `y_range` the exposure axis.
#'   Both `NULL` (default) auto-scale. Note that `x_range` only sets the
#'   visible window; the bins are always computed over the full range of
#'   changes, so the summary statistics are unaffected.
#'
#' @return A list with `summary` (display table with the headline numbers),
#'   `stats` (the same numbers as a named list), `policy` (per-row
#'   `OldRate`, `NewRate`, `NewRateRebased`, `ChangePct`), `by_level`
#'   (per level: exposure, share, mean change and contribution, when `by`
#'   is given), `spotlight` (the same headline numbers for the subset, when
#'   `spotlight` is given), `largest_increases` / `largest_decreases`
#'   (top-`n_show` dislocations) and `plot` (exposure-weighted histogram of
#'   the premium changes, with the spotlight overlaid).
#'
#' @section What is not decomposed:
#' Old and new premiums are compared on the *same* policies, so the
#' portfolio mix is identical on both sides and there is no mix effect to
#' isolate. `Contribution` therefore answers "which segment drives the
#' overall change", not "how much of the change is mix". A genuine
#' mix-shift analysis needs two portfolio snapshots and is a different
#' calculation.
#' @export
premium_impact <- function(data,
                           model_freq_new = NULL, model_sev_new = NULL,
                           model_freq_old = NULL, model_sev_old = NULL,
                           old_premium_col   = NULL,
                           old_premium_basis = c("amount", "rate"),
                           rebase       = TRUE,
                           by           = NULL,
                           n_show       = 10,
                           exposure_col = "Exposure",
                           spotlight    = NULL,
                           x_range      = NULL,
                           y_range      = NULL) {

  old_premium_basis <- match.arg(old_premium_basis)
  x_range <- .check_range(x_range, "premium_impact", "x_range")
  y_range <- .check_range(y_range, "premium_impact")
  .check_cols(data, c(exposure_col, by, old_premium_col), "premium_impact")
  data <- .as_df(data)

  # The spotlight is evaluated against the full data, before any rows are
  # dropped, so the mask lines up with the rows it was written for
  # Evaluated as an expression against `data` first, falling back to the
  # caller's environment, so both `REGIO == "Stad"` and a logical vector
  # work. The promise must not be forced before this, or a column name
  # would be looked up in the caller and fail.
  spot_expr <- substitute(spotlight)
  spot <- if (is.null(spot_expr)) NULL else {
    s <- tryCatch(eval(spot_expr, data, parent.frame()),
                  error = function(e)
                    stop("premium_impact: 'spotlight' could not be ",
                         "evaluated against 'data': ", conditionMessage(e),
                         call. = FALSE))
    if (!is.logical(s) || length(s) != nrow(data))
      stop("premium_impact: 'spotlight' must give one TRUE/FALSE per row ",
           "of 'data'.", call. = FALSE)
    s & !is.na(s)
  }

  r <- .old_new_rates(data, model_freq_new, model_sev_new,
                      model_freq_old, model_sev_old, old_premium_col,
                      old_premium_basis, exposure_col, "premium_impact")
  new_rate <- r$new
  old_rate <- r$old

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

  # Contribution decomposition. The portfolio is the same on both sides, so
  # there is no mix effect to isolate; what can be decomposed is who causes
  # the overall change. Contribution = share x change, and the levels of one
  # variable sum to the portfolio change.
  by_level <- NULL
  if (!is.null(by)) {
    tot_w <- sum(expo)
    by_level <- do.call(rbind, lapply(by, function(v) {
      dtb <- data.table::data.table(Level = as.character(data[[v]][idx]),
                                    w = expo, ch = change)
      ag  <- dtb[, .(Exposure      = sum(w),
                     MeanChangePct = sum(ch * w) / sum(w)), by = Level]
      ag[, ExposureShare := Exposure / tot_w]
      ag[, Contribution  := ExposureShare * MeanChangePct]
      data.frame(Variable = v, as.data.frame(ag), stringsAsFactors = FALSE)
    }))
    by_level <- by_level[order(by_level$Variable,
                               -abs(by_level$Contribution)), ]
    rownames(by_level) <- NULL
  }

  # Spotlight: the same headline numbers for a subset, computed AFTER the
  # book-level rebase, so it shows how that subset moves relative to the
  # book rather than relative to itself.
  spotlight_res <- NULL
  if (!is.null(spot)) {
    sm <- spot[idx]
    if (!any(sm)) {
      warning("premium_impact: 'spotlight' selects no usable rows.",
              call. = FALSE)
    } else {
      sq <- wq(change[sm], expo[sm], c(0.05, 0.50, 0.95))
      spotlight_res <- list(
        n_rows          = sum(sm),
        exposure        = sum(expo[sm]),
        exposure_share  = sum(expo[sm]) / sum(expo),
        old_total       = sum(old_rate[sm] * expo[sm]),
        new_total       = sum(new_adj[sm] * expo[sm]),
        mean_change     = sum(change[sm] * expo[sm]) / sum(expo[sm]),
        median_change   = sq[2],
        p5_change       = sq[1],
        p95_change      = sq[3],
        share_gt_10     = sum(expo[sm][abs(change[sm]) > 10]) / sum(expo[sm]),
        contribution    = sum(expo[sm]) / sum(expo) *
                          (sum(change[sm] * expo[sm]) / sum(expo[sm])))
      spotlight_res$summary <- data.frame(
        Metric = c("Rows in spotlight", "Exposure share of the book",
                   "Mean change (exposure-weighted)", "Median change",
                   "P5 .. P95 change", "Exposure share |change| > 10%",
                   "Contribution to the portfolio change"),
        Value = c(format(spotlight_res$n_rows, big.mark = ","),
                  sprintf("%.1f%%", 100 * spotlight_res$exposure_share),
                  sprintf("%+.2f%%", spotlight_res$mean_change),
                  sprintf("%+.2f%%", spotlight_res$median_change),
                  sprintf("%+.1f%% .. %+.1f%%", sq[1], sq[3]),
                  sprintf("%.1f%%", 100 * spotlight_res$share_gt_10),
                  sprintf("%+.2f%%", spotlight_res$contribution)),
        stringsAsFactors = FALSE)
    }
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

  bar_name <- if (is.null(spotlight_res)) "Exposure" else "Whole portfolio"
  p <- plot_ly() %>%
    add_bars(x = mids, y = h_exp, width = diff(brks), name = bar_name,
             marker = list(color = ta_blue, opacity = 0.75,
                           line = list(color = "white", width = 0.5)),
             hovertemplate = paste0("<b>Change:</b> %{x:.1f}%",
                                    "<br><b>Exposure:</b> %{y:,.0f}",
                                    "<extra></extra>"))

  if (!is.null(spotlight_res)) {
    sm  <- spot[idx]
    hs  <- as.numeric(tapply(expo[sm], bin_id[sm], sum))
    hs[is.na(hs)] <- 0
    p <- p %>% add_bars(
      x = mids, y = hs, width = diff(brks), name = "Spotlight",
      marker = list(color = ta_gold, opacity = 0.85,
                    line = list(color = "white", width = 0.5)),
      hovertemplate = paste0("<b>Change:</b> %{x:.1f}%",
                             "<br><b>Exposure (spotlight):</b> %{y:,.0f}",
                             "<extra></extra>"))
  }

  p <- p %>%
    layout(
      title  = list(text = paste0("Premium impact",
                                  if (rebase) " (rebased to old total)" else "",
                                  sprintf(" \u2013 rate-level change %+.1f%%",
                                          100 * rate_change)),
                    font = list(color = ta_navy, size = 14)),
      xaxis  = list(title = "Premium change (%)", showgrid = FALSE,
                    zeroline = FALSE,
                    range = x_range, autorange = is.null(x_range)),
      yaxis  = list(title = "Exposure", gridcolor = "#D0D8E0",
                    zeroline = FALSE,
                    range = y_range, autorange = is.null(y_range)),
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
      barmode      = "overlay",
      legend       = list(orientation = "h", y = -0.2),
      showlegend   = !is.null(spotlight_res),
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
       spotlight         = spotlight_res,
       largest_increases = largest_increases,
       largest_decreases = largest_decreases,
       plot              = p)
}
