#' One-way plot of frequency or severity with exposure bars
#'
#' Takes raw policy rows, aggregates them internally via [agg_all()] and
#' plots the chosen metric with exposure bars on a secondary axis.
#'
#' With a character/factor X-axis (or a numeric one with at most
#' `discrete_cutoff` unique values) only markers are shown, so categories are
#' never connected by a line. Note: in color mode with `by_year = TRUE` the
#' exposure bars show the sum over all years, while the lines are per year.
#'
#' @param data A data.frame with raw rows.
#' @param col Name of the X-axis column (string).
#' @param metric `"Frequency"` or `"Severity"` (computed on the internal
#'   aggregate).
#' @param color_single Colour for the metric line when `by_year = FALSE`.
#' @param y_label Label for the Y-axis.
#' @param display `"color"` (coloured lines per year) or `"facet"`.
#' @param by_year `TRUE` = split by accounting year.
#' @param metric_fmt Number of decimals in the tooltip (default 4).
#' @param exposure_col,claims_col,loss_col,year_col Column names, see
#'   [agg_all()].
#' @param discrete_cutoff Maximum number of unique numeric values that is
#'   still treated as discrete (markers only, default 25).
#'
#' @return A plotly object.
#' @export
#' @examples
#' \donttest{
#' d <- data.frame(REGIO = sample(c("N", "Z"), 500, TRUE),
#'                 BOEKJAAR = sample(2023:2024, 500, TRUE),
#'                 Exposure = runif(500, .2, 1),
#'                 AantalClaims = rpois(500, 0.1), SCHADELAST = 0)
#' make_plot(d, "REGIO", "Frequency", ta_blue, "Frequency",
#'           display = "color", by_year = TRUE)
#' }
make_plot <- function(data, col, metric = c("Frequency", "Severity"),
                      color_single, y_label,
                      display = c("color", "facet"), by_year,
                      metric_fmt = 4,
                      exposure_col = "Exposure",
                      claims_col = "AantalClaims",
                      loss_col   = "SCHADELAST",
                      year_col   = "BOEKJAAR",
                      discrete_cutoff = 25) {

  display <- match.arg(display)
  metric  <- match.arg(metric)
  .check_cols(data, c(col, exposure_col, claims_col, loss_col,
                      if (by_year) year_col), "make_plot")

  # Aggregate the raw rows; agg_all returns standardised column names
  # (Exposure, ClaimCount, Loss, Frequency, Severity and, when split, Year)
  agg <- agg_all(data, col, by_year,
                 exposure_col = exposure_col, claims_col = claims_col,
                 loss_col = loss_col, year_col = year_col)

  agg <- agg %>% rename(.x = all_of(col))

  # Robust column reference for plotly (metric may contain spaces)
  y_metric <- stats::as.formula(paste0("~`", metric, "`"))

  # With a character/factor X-axis show markers only (no line connecting
  # categories). Numeric columns with few unique values (e.g. integer-coded
  # categories) are also treated as discrete.
  is_discrete <- is.character(agg$.x) || is.factor(agg$.x) ||
                 !is.numeric(agg$.x) ||
                 length(unique(agg$.x)) <= discrete_cutoff

  # Sort by X (and Year when split) so the line runs cleanly
  agg <- if (by_year) agg %>% arrange(Year, .x) else agg %>% arrange(.x)

  # Exposure aggregate for the bars (sum over years when by_year)
  exp_agg <- if (by_year) {
    agg %>%
      group_by(.x) %>%
      summarise(Exposure = sum(Exposure, na.rm = TRUE), .groups = "drop")
  } else {
    agg
  }
  exp_name <- if (by_year) "Exposure (all years)" else "Exposure"

  # Facet mode: ggplot2 + ggplotly -----------------------------------------
  if (by_year && display == "facet") {

    # Rescale exposure per facet to the range of the metric, so that bars
    # and line visually share the same y-axis.
    agg_facet <- agg %>%
      group_by(Year) %>%
      mutate(
        .metric_max  = suppressWarnings(max(.data[[metric]], na.rm = TRUE)),
        .exp_max     = suppressWarnings(max(Exposure,        na.rm = TRUE)),
        .exp_scaled  = ifelse(is.finite(.exp_max) & .exp_max > 0 &
                              is.finite(.metric_max) & .metric_max > 0,
                              Exposure / .exp_max * .metric_max,
                              0),
        .exp_label   = paste0("Exposure: ",
                              format(round(Exposure), big.mark = ",",
                                     scientific = FALSE)),
        .metric_label = paste0(metric, ": ",
                               formatC(.data[[metric]], format = "f",
                                       digits = metric_fmt, big.mark = ","))
      ) %>%
      ungroup()

    p <- ggplot(agg_facet, aes(x = .x)) +
      geom_col(aes(y = .exp_scaled, fill = "Exposure", text = .exp_label),
               width = 0.6, alpha = 0.38)
    if (!is_discrete) {
      p <- p + geom_line(aes(y = .data[[metric]], color = metric, group = 1),
                         linewidth = 0.8)
    }
    p <- p +
      geom_point(aes(y = .data[[metric]], color = metric,
                     text = .metric_label), size = 2) +
      scale_fill_manual(name = NULL, values = c("Exposure" = ta_lightblue)) +
      scale_color_manual(name = NULL,
                         values = setNames(color_single, metric)) +
      scale_y_continuous(name = y_label) +
      facet_wrap(vars(Year), scales = "free_y") +
      labs(x = col, title = paste(metric, "-", col),
           caption = "Exposure bars are rescaled per facet; the tooltip shows the actual value.") +
      theme_minimal() +
      theme(
        axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
        plot.title       = element_text(color = ta_navy, face = "bold", size = 12),
        plot.caption     = element_text(color = ta_muted, size = 8, hjust = 0),
        strip.background = element_rect(fill = NA, color = "#D0D8E0", linewidth = 0.5),
        strip.text       = element_text(color = ta_navy, face = "bold", size = 9),
        legend.position  = "bottom"
      )
    return(
      ggplotly(p, tooltip = c("x", "text")) %>%
        layout(legend = list(orientation = "h", y = -0.15),
               margin = list(b = 60)) %>%
        config(displayModeBar = TRUE,
               modeBarButtonsToRemove = c("lasso2d", "select2d"),
               toImageButtonOptions = list(format = "png", filename = "plot"))
    )
  }

  # Color mode or no-year mode: native plotly -------------------------------
  p <- plot_ly() %>%
    add_bars(
      data             = exp_agg,
      x                = ~.x,
      y                = ~Exposure,
      name             = exp_name,
      yaxis            = "y2",
      marker           = list(color = ta_lightblue, opacity = 0.5,
                               line = list(width = 0)),
      hovertemplate    = paste0("<b>", col, ":</b> %{x}",
                                "<br><b>", exp_name, ":</b> %{y:,.0f}",
                                "<extra></extra>")
    )

  add_metric_trace <- function(p, d, year_label, colr, marker_size,
                               line_width, legend_name, show_legend = TRUE) {
    ht <- if (is.null(year_label)) {
      paste0("<b>", col, ":</b> %{x}",
             "<br><b>", metric, ":</b> %{y:.", metric_fmt, "f}<extra></extra>")
    } else {
      paste0("<b>", col, ":</b> %{x}",
             "<br><b>Year:</b> ", year_label,
             "<br><b>", metric, ":</b> %{y:.", metric_fmt, "f}<extra></extra>")
    }
    if (is_discrete) {
      add_markers(p, data = d, x = ~.x, y = y_metric,
                  name = legend_name, legendgroup = legend_name, yaxis = "y",
                  marker = list(color = colr, size = marker_size),
                  showlegend = show_legend, hovertemplate = ht)
    } else {
      add_trace(p, data = d, x = ~.x, y = y_metric,
                type = "scatter", mode = "lines+markers",
                name = legend_name, legendgroup = legend_name, yaxis = "y",
                line   = list(color = colr, width = line_width),
                marker = list(color = colr, size = marker_size),
                showlegend = show_legend, hovertemplate = ht)
    }
  }

  if (!by_year) {
    p <- add_metric_trace(p, agg, NULL, color_single,
                          marker_size = 7, line_width = 2.2,
                          legend_name = metric)
  } else {
    years  <- levels(agg$Year)
    colors <- ta_year_palette(length(years))
    for (i in seq_along(years)) {
      yr     <- years[i]
      d_year <- agg %>% filter(Year == yr)
      p <- add_metric_trace(p, d_year, yr, colors[i],
                            marker_size = 6, line_width = 2,
                            legend_name = as.character(yr))
    }
  }

  p %>%
    layout(
      title  = list(text = paste(metric, "\u2013", col),
                    font = list(color = ta_navy, size = 14)),
      xaxis  = list(title      = col,
                    tickangle  = -45,
                    tickfont   = list(size = 9),
                    showgrid   = FALSE),
      yaxis  = list(title      = y_label,
                    gridcolor  = "#D0D8E0",
                    zeroline   = FALSE),
      yaxis2 = list(title      = "Exposure",
                    overlaying = "y",
                    side       = "right",
                    showgrid   = FALSE,
                    tickfont   = list(color = ta_muted),
                    titlefont  = list(color = ta_muted)),
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
