#' Plot rating factors from a rating table
#'
#' Plots one variable (or interaction) from a [make_rating_table()] result.
#'
#' Main effects show the factor curves for frequency, severity and premium
#' on the primary axis (with a dotted reference line at 1.0) and exposure
#' bars on the secondary axis. Interactions (`var = "A:B"`) show one series
#' per level of the group variable for a single metric. Levels flagged
#' `IsThin` are drawn with dimmed markers and a "low claim volume" note in
#' the tooltip.
#'
#' @param rating_tbl Output of [make_rating_table()].
#' @param var Variable name, or `"A:B"` for an interaction.
#' @param metric_fmt Number of decimals in the tooltip (default 4).
#' @param metric For interaction plots: `"Frequency"`, `"Severity"` or
#'   `"Premium"` (default: Premium when both models are present).
#'
#' @return A plotly object.
#' @export
make_rating_plot <- function(rating_tbl, var, metric_fmt = 4, metric = NULL) {

  d <- rating_tbl[rating_tbl$Variable == var, , drop = FALSE]
  if (nrow(d) == 0) stop("Variable '", var, "' not found.")

  is_interaction <- any(!is.na(d$Group))

  # Regular main-effect plot ---------------------------------------------------
  if (!is_interaction) {
    is_cat <- d$Type[1] == "categorical"
    d$.x <- if (is_cat) factor(d$Level, levels = d$Level) else d$LevelNum

    mode_line <- if (is_cat) "markers" else "lines+markers"

    # Thin cells (IsThin from make_rating_table): dim the markers and add
    # a hover note, so low-credibility levels are visually distinct
    thin <- if ("IsThin" %in% names(d)) d$IsThin %in% TRUE else rep(FALSE, nrow(d))
    d$.thin_label <- ifelse(thin, "<br><i>\u26a0 low claim volume</i>", "")
    op <- ifelse(thin, 0.4, 1)

    ht <- function(series) paste0("<b>", var, ":</b> %{x}",
                                  "<br><b>", series, ":</b> %{y:.",
                                  metric_fmt, "f}%{text}<extra></extra>")

    has_freq <- any(!is.na(d$Factor_Frequency))
    has_sev  <- any(!is.na(d$Factor_Severity))
    has_prem <- has_freq && has_sev

    p <- plot_ly() %>%
      add_bars(
        data   = d, x = ~.x, y = ~Exposure,
        name   = "Exposure", yaxis = "y2",
        marker = list(color = ta_lightblue, opacity = 0.5,
                      line = list(width = 0)),
        hovertemplate = paste0("<b>", var, ":</b> %{x}",
                               "<br><b>Exposure:</b> %{y:,.0f}",
                               "<extra></extra>")
      )

    # With a categorical axis show markers only: do not pass a line (plotly
    # would otherwise reset the mode to "markers+lines" and connect categories).
    if (has_freq) p <- p %>% add_trace(
      data = d, x = ~.x, y = ~Factor_Frequency,
      type = "scatter", mode = mode_line,
      name = "Factor Frequency", yaxis = "y",
      line   = if (is_cat) NULL else list(color = ta_blue, width = 2.2),
      marker = list(color = ta_blue, size = if (is_cat) 8 else 5,
                    opacity = op),
      text = ~.thin_label, hovertemplate = ht("Factor Frequency")
    )
    if (has_sev) p <- p %>% add_trace(
      data = d, x = ~.x, y = ~Factor_Severity,
      type = "scatter", mode = mode_line,
      name = "Factor Severity", yaxis = "y",
      line   = if (is_cat) NULL else list(color = ta_gold, width = 2.2),
      marker = list(color = ta_gold, size = if (is_cat) 8 else 5,
                    opacity = op),
      text = ~.thin_label, hovertemplate = ht("Factor Severity")
    )
    if (has_prem) p <- p %>% add_trace(
      data = d, x = ~.x, y = ~Factor_Premium,
      type = "scatter", mode = mode_line,
      name = "Factor Premium", yaxis = "y",
      line   = if (is_cat) NULL else list(color = ta_navy, width = 2.6, dash = "dash"),
      marker = list(color = ta_navy, size = if (is_cat) 9 else 5,
                    opacity = op),
      text = ~.thin_label, hovertemplate = ht("Factor Premium")
    )

    return(p %>%
             layout(
               title  = list(text = paste("Rating factors \u2013", var),
                             font = list(color = ta_navy, size = 14)),
               xaxis  = list(title = var, tickangle = -45,
                             tickfont = list(size = 9), showgrid = FALSE),
               yaxis  = list(title = "Factor (base = 1.0)",
                             gridcolor = "#D0D8E0", zeroline = FALSE),
               yaxis2 = list(title = "Exposure", overlaying = "y", side = "right",
                             showgrid = FALSE,
                             tickfont = list(color = ta_muted),
                             titlefont = list(color = ta_muted)),
               shapes = list(list(type = "line", xref = "paper",
                                  x0 = 0, x1 = 1, y0 = 1, y1 = 1,
                                  line = list(color = ta_muted, width = 1,
                                              dash = "dot"))),
               legend        = list(orientation = "h", y = -0.2),
               hoverlabel    = list(bgcolor = "white", font = list(size = 12)),
               barmode       = "overlay",
               plot_bgcolor  = "white", paper_bgcolor = "white",
               margin        = list(b = 80, r = 80)
             ) %>%
             config(displayModeBar = TRUE,
                    modeBarButtonsToRemove = c("lasso2d", "select2d"),
                    toImageButtonOptions = list(format = "png", filename = "rating")))
  }

  # Interaction plot: one series per level of the group variable ---------------
  xvar <- d$XVar[1]; gvar <- d$GroupVar[1]
  is_cat_x  <- d$Type[1] == "categorical"
  mode_line <- if (is_cat_x) "markers" else "lines+markers"

  # Too busy to show all three factors at once -> pick one.
  has_freq <- any(!is.na(d$Factor_Frequency))
  has_sev  <- any(!is.na(d$Factor_Severity))
  metric <- if (!is.null(metric)) {
    match.arg(metric, c("Frequency", "Severity", "Premium"))
  } else if (has_freq && has_sev) {
    "Premium"
  } else if (has_freq) {
    "Frequency"
  } else {
    "Severity"
  }
  ycol <- paste0("Factor_", metric)

  groups <- unique(d$Group)
  colors <- ta_year_palette(length(groups))

  p <- plot_ly()

  # Exposure bars per X level (summed over the groups), only available for
  # categorical x categorical (elsewhere Exposure is NA in the table)
  has_expo <- any(!is.na(d$Exposure)) && any(d$Exposure > 0, na.rm = TRUE)
  if (has_expo) {
    ex <- aggregate(Exposure ~ Level, data = d, FUN = sum)
    ex$.x <- if (is_cat_x) factor(ex$Level, levels = unique(d$Level))
             else suppressWarnings(as.numeric(ex$Level))
    ex <- ex[order(ex$.x), ]
    p <- p %>% add_bars(
      data   = ex, x = ~.x, y = ~Exposure,
      name   = "Exposure", yaxis = "y2",
      marker = list(color = ta_lightblue, opacity = 0.5, line = list(width = 0)),
      hovertemplate = paste0("<b>", xvar, ":</b> %{x}",
                             "<br><b>Exposure:</b> %{y:,.0f}",
                             "<extra></extra>")
    )
  }

  for (i in seq_along(groups)) {
    g  <- groups[i]
    dg <- d[d$Group == g, , drop = FALSE]
    dg$.x <- if (is_cat_x) factor(dg$Level, levels = unique(d$Level)) else dg$LevelNum
    if (!is_cat_x) dg <- dg[order(dg$.x), ]
    thin_g <- if ("IsThin" %in% names(dg)) dg$IsThin %in% TRUE else rep(FALSE, nrow(dg))
    dg$.thin_label <- ifelse(thin_g, "<br><i>\u26a0 low claim volume</i>", "")
    p <- p %>% add_trace(
      data = dg, x = ~.x, y = stats::as.formula(paste0("~", ycol)),
      type = "scatter", mode = mode_line,
      name = paste0(gvar, " = ", g), yaxis = "y",
      line   = if (is_cat_x) NULL else list(color = colors[i], width = 2.2),
      marker = list(color = colors[i], size = if (is_cat_x) 9 else 5,
                    opacity = ifelse(thin_g, 0.4, 1)),
      text = ~.thin_label,
      hovertemplate = paste0("<b>", xvar, ":</b> %{x}",
                             "<br><b>", gvar, ":</b> ", g,
                             "<br><b>Factor ", metric, ":</b> %{y:.",
                             metric_fmt, "f}%{text}<extra></extra>")
    )
  }

  p <- p %>%
    layout(
      title = list(text = paste0("Interaction \u2013 ", xvar, " \u00d7 ", gvar,
                                 "  (Factor ", metric, ")"),
                   font = list(color = ta_navy, size = 14)),
      xaxis = list(title = xvar, tickangle = -45,
                   tickfont = list(size = 9), showgrid = FALSE),
      yaxis = list(title = paste0("Factor ", metric, " (base cell = 1.0)"),
                   gridcolor = "#D0D8E0", zeroline = FALSE),
      shapes = list(list(type = "line", xref = "paper",
                         x0 = 0, x1 = 1, y0 = 1, y1 = 1,
                         line = list(color = ta_muted, width = 1, dash = "dot"))),
      legend       = list(orientation = "h", y = -0.2, title = list(text = gvar)),
      hoverlabel   = list(bgcolor = "white", font = list(size = 12)),
      barmode      = "overlay",
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin       = list(b = 80, r = if (has_expo) 80 else 40)
    )
  if (has_expo) {
    p <- p %>% layout(
      yaxis2 = list(title = "Exposure", overlaying = "y", side = "right",
                    showgrid = FALSE,
                    tickfont  = list(color = ta_muted),
                    titlefont = list(color = ta_muted))
    )
  }

  p %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "interaction"))
}
