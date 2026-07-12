# ─────────────────────────────────────────────────────────────────────
# GLM UTILS.R  —  helper functions for GLM-based insurance pricing
#
# Functions:
#   agg_all()            aggregate raw rows to Frequency/Severity per level
#   make_plot()          one-way plot with exposure bars (plotly)
#   make_pdp()           partial dependence plot + observed overlay (glm)
#   plot_glm_predictor() actual vs. expected per predictor (glm)
#   glm_diagnostics()    fit summary per model: deviance, AIC, dispersion
#   plot_glm_residuals() binned residual plot vs fitted or a predictor
#   make_rating_table()  rating factors from frequency/severity models
#   make_rating_plot()   plot rating factors (incl. interactions)
#   premium_impact()     dislocation analysis: old vs new premiums
#   export_rating_table() formatted Excel export of the rating table
#   pricing_report()     one-call HTML report bundling all of the above
#
# Column names are configurable everywhere via arguments:
#   exposure_col  exposure column                     (default "Exposure")
#   claims_col    y for frequency AND weight for severity
#                                                     (default "AantalClaims")
#   loss_col      y for severity                      (default "SCHADELAST")
#   year_col      accounting-year column              (default "BOEKJAAR")
# The defaults match the original (Dutch) dataset naming; override them
# for datasets with different column names. All functions take RAW policy
# rows; agg_all() is the aggregation engine behind make_plot() and returns
# standardised English output names (Exposure, ClaimCount, Loss,
# Frequency, Severity, Year).
#
# Actuarial conventions:
#   * make_pdp() computes the PDP on the RESPONSE scale: for each grid
#     value the weighted arithmetic mean of predict(type = "response"),
#     weighted by exposure (frequency) or claim counts (severity). The
#     offset is neutralised by setting the exposure column to 1, so the
#     PDP is a frequency per unit of exposure and directly comparable to
#     the observed line. (exp(mean(link)) would be a geometric mean,
#     including the offset — systematically biased low.)
#   * make_rating_table(): the base level is the reference level for
#     categorical variables (first xlevel, or the level with the largest
#     exposure when base_level = "exposure"), the MEDIAN for continuous
#     variables (added as an explicit grid point, IsBase = TRUE), and 1
#     for the exposure column. As a result, for log-link models without
#     interactions the identity holds exactly:
#       intercept × product(factors) = model prediction per exposure unit.
#     Interaction rows contain both the joint relativity and the pure
#     interaction uplift: joint / (main_factor_x × main_factor_group).
# ─────────────────────────────────────────────────────────────────────

library(dplyr)
library(ggplot2)
library(plotly)
library(data.table)
library(splines)   # required so that predict() works on models with ns()/bs()

# ── Internal helpers ─────────────────────────────────────────────────

# Check that columns are present; clear error message per function.
.check_cols <- function(d, cols, fn) {
  cols <- unique(cols[!vapply(cols, is.null, logical(1))])
  missing_cols <- setdiff(unlist(cols), names(d))
  if (length(missing_cols))
    stop(fn, ": column(s) not found in the data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Training data (original columns) aligned with the fitted rows of a glm.
# Uses rownames matching instead of as.integer(rownames), so it also works
# with non-numeric rownames and rows dropped by na.action.
.glm_training_data <- function(model, fn = "GLM Utils") {
  mf <- stats::model.frame(model)
  d  <- model$data
  if (!is.null(d) && is.data.frame(d)) {
    idx <- match(rownames(mf), rownames(d))
    if (anyNA(idx))
      stop(fn, ": the rows of model.frame() cannot be matched to ",
           "model$data; fit the model with a 'data =' argument.",
           call. = FALSE)
    d <- d[idx, , drop = FALSE]
  } else {
    d <- NULL
  }
  pw <- model$prior.weights
  if (is.null(pw)) pw <- rep(1, nrow(mf))
  list(mf = mf, data = d, weights = as.numeric(pw), offset = model$offset)
}

# Coerce values to the type of an existing column, so that inline
# transformations in the formula (e.g. factor(YEAR) on a numeric column)
# can be predicted correctly.
.coerce_like <- function(vals, template) {
  if (is.factor(template)) {
    factor(as.character(vals), levels = levels(template))
  } else if (is.numeric(template)) {
    suppressWarnings(as.numeric(as.character(vals)))
  } else {
    as.character(vals)
  }
}

# Predictor variables (RHS) of a model; robust for composite responses
# (cbind, ratios) because the full LHS is excluded.
.rhs_vars <- function(model) {
  f <- formula(model)
  setdiff(all.vars(f), all.vars(f[[2]]))
}

# Underlying column of a term label: "ns(AGE, 4)" -> "AGE"
.term_base_var <- function(term, data) {
  if (term %in% names(data)) return(term)
  m <- regmatches(term, regexpr("\\(([^,\\)]+)", term))
  if (length(m) == 0) return(term)
  gsub("[()]", "", m)
}

# Base variables of a model that exist as columns in `data`
# (interaction terms split into their components, deduplicated)
.model_base_vars <- function(model, data) {
  if (is.null(model)) return(character(0))
  labs  <- attr(terms(model), "term.labels")
  comps <- unique(unlist(strsplit(labs, ":", fixed = TRUE)))
  bv    <- unique(vapply(comps, .term_base_var, character(1), data = data))
  intersect(bv, names(data))
}

# ── Aggregation function ─────────────────────────────────────────────
# Goes from raw rows to an aggregate per level of `col`
# (and optionally per accounting year).
#
# Arguments:
#   d             data.frame or data.table
#   col           name of the grouping column (string)
#   by_year       TRUE = also split by accounting year
#   exposure_col  exposure column           (default "Exposure")
#   claims_col    claim count column        (default "AantalClaims")
#   loss_col      loss amount column        (default "SCHADELAST")
#   year_col      accounting-year column    (default "BOEKJAAR")
#
# Returns a data.frame with standardised column names:
#   col, [Year,] Exposure, ClaimCount, Loss, Frequency, Severity
agg_all <- function(d, col, by_year,
                    exposure_col = "Exposure",
                    claims_col   = "AantalClaims",
                    loss_col     = "SCHADELAST",
                    year_col     = "BOEKJAAR") {
  if (!data.table::is.data.table(d)) d <- data.table::as.data.table(d)
  .check_cols(d, c(col, exposure_col, claims_col, loss_col,
                   if (by_year) year_col), "agg_all")

  # Report data-quality issues explicitly instead of silently averaging away
  for (cc in c(exposure_col, claims_col, loss_col)) {
    n_na <- sum(is.na(d[[cc]]))
    if (n_na > 0)
      warning("agg_all: ", n_na, " NA value(s) in column '", cc,
              "' are ignored (na.rm = TRUE).", call. = FALSE)
  }

  grp <- if (by_year) c(col, year_col) else col
  out <- d[, .(Exposure   = sum(get(exposure_col), na.rm = TRUE),
               ClaimCount = sum(get(claims_col),   na.rm = TRUE),
               Loss       = sum(get(loss_col),     na.rm = TRUE)),
           by = grp]
  if (by_year && year_col != "Year") data.table::setnames(out, year_col, "Year")

  n_zero <- sum(out$Exposure <= 0)
  if (n_zero > 0)
    warning("agg_all: ", n_zero, " group(s) with Exposure <= 0; ",
            "Frequency is NA there.", call. = FALSE)

  out[, Frequency := data.table::fifelse(Exposure > 0,
                                         ClaimCount / Exposure,
                                         NA_real_)]
  out[, Severity  := data.table::fifelse(ClaimCount > 0,
                                         Loss / ClaimCount,
                                         NA_real_)]
  if (by_year) out[, Year := factor(Year)]
  as.data.frame(out)
}

# ── House-style colours ──────────────────────────────────────────────
ta_navy      <- "#00365E"
ta_blue      <- "#0073AB"
ta_lightblue <- "#A8C8E0"
ta_gold      <- "#D39F27"
ta_muted     <- "#6B7A8D"

ta_years_base <- c("#00365E","#0073AB","#1A8FC2","#4AADD4",
                   "#A8C8E0","#D39F27","#6B7A8D","#C44536","#8B5E3C")

ta_year_palette <- function(n) {
  if (n <= length(ta_years_base)) ta_years_base[1:n]
  else colorRampPalette(ta_years_base)(n)
}

# ── Plot function ────────────────────────────────────────────────────
# One-way plot of Frequency or Severity with exposure bars. Takes RAW
# policy rows and aggregates internally via agg_all().
#
# Arguments:
#   data             data.frame with raw rows
#   col              name of the X-axis column (string)
#   metric           "Frequency" or "Severity"
#   color_single     colour for the metric line when by_year = FALSE (hex)
#   y_label          label for the Y-axis
#   display          "color" (coloured lines per year) or "facet"
#   by_year          TRUE = split by accounting year
#   metric_fmt       number of decimals in tooltip (default 4)
#   exposure_col     exposure column           (default "Exposure")
#   claims_col       claim count column        (default "AantalClaims")
#   loss_col         loss amount column        (default "SCHADELAST")
#   year_col         accounting-year column    (default "BOEKJAAR")
#   discrete_cutoff  max. number of unique numeric values that is still
#                    treated as discrete (markers only) (default 25)
#
# Note: in color mode with by_year = TRUE the exposure bars show the SUM
# over all years, while the lines are per year.
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

  # ── Facet mode: ggplot2 + ggplotly ─────────────────────────────────
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
               modeBarButtonsToRemove = c("lasso2d","select2d"),
               toImageButtonOptions = list(format = "png", filename = "plot"))
    )
  }

  # ── Color mode or no-year mode: native plotly ──────────────────────
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
      title  = list(text = paste(metric, "–", col),
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
      modeBarButtonsToRemove = c("lasso2d","select2d"),
      toImageButtonOptions   = list(format = "png", filename = "plot")
    )
}

# ── Partial dependence plot ──────────────────────────────────────────
# Computes the PDP on the RESPONSE scale: for each grid value of pred_var
# the whole training data is set to that value, predicted with
# predict(type = "response"), and the weighted arithmetic mean is taken
# (frequency: exposure-weighted; severity: claim-weighted). The offset is
# neutralised by setting exposure_col to 1, so the PDP is a frequency per
# unit of exposure.
#
# Note: the "Observed" line is a one-way marginal (including correlations
# with other rating factors); the PDP is a partial effect. Differences
# between the two therefore do not necessarily indicate misfit.
#
# Performance: the training data is first collapsed to the UNIQUE profiles
# of the model's other predictors (with summed weights), so the cost is
# grid_res × (#unique profiles) predictions instead of grid_res × n rows —
# typically orders of magnitude faster on large portfolios, with an
# EXACTLY identical result.
#
# Arguments:
#   model            fitted glm object (fitted with a 'data =' argument)
#   raw_data         raw data for the observed line and the bars
#   pred_var         name of the predictor (string)
#   metric           "Frequency" or "Severity"
#   transform        DEPRECATED — ignored (response scale is the default)
#   grid_res         number of grid points for continuous predictors (default 50)
#   y_label          Y-axis label (default automatic)
#   metric_fmt       number of decimals in tooltip (default 4)
#   exposure_col     exposure column (default "Exposure")
#   claims_col       claim count column (default "AantalClaims")
#   loss_col         loss amount column (default "SCHADELAST")
#   discrete_cutoff  see make_plot (default 25)
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

  # ── Observed aggregation depending on metric ───────────────────────
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

  setnames(obs_agg, pred_var, ".x")
  obs_agg <- obs_agg[order(.x)]

  # ── Training data, weights and offset ──────────────────────────────
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

  # ── Grid ───────────────────────────────────────────────────────────
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

  # ── Collapse to unique predictor profiles (performance) ────────────
  # predict() only depends on the model's predictor columns. Rows that
  # share the same values for all predictors other than pred_var receive
  # the same prediction on every grid point, so they are collapsed into
  # one profile with summed weight. On typical pricing data (categorical
  # rating factors, integer ages, exposure already neutralised to 1) this
  # reduces the work from grid_res × n rows to grid_res × (#profiles),
  # while leaving the weighted mean EXACTLY unchanged.
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

  # ── PDP: weighted mean response prediction per grid value ──────────
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

  # ── Plot ───────────────────────────────────────────────────────────
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
        text = paste("Partial Dependence –", pred_var, "–", metric),
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

# ── Actual vs. expected per predictor ────────────────────────────────
# Arguments:
#   model         fitted glm object
#   predictor     name of the predictor (string)
#   n_bins        number of bins for numeric predictors (default 150)
#   weight_var    optional: weight/exposure column in model$data (override)
#   weight_label  optional: axis title for the bars
#   bin_type      "quantile" (default; exposure-representative bins) or
#                 "width" (equal-width bins, the old behaviour)
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

  # --- Retrieve the predictor ---
  if (predictor %in% names(model_data)) {
    x_values <- model_data[[predictor]]
  } else if (!is.null(tr$data) && predictor %in% names(tr$data)) {
    x_values <- tr$data[[predictor]]
  } else {
    stop(paste0("Predictor '", predictor, "' not found."))
  }

  # --- Determine weight/exposure vector + mode ---
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

  # --- Data ---
  df <- data.frame(
    x_var     = x_values,
    observed  = as.numeric(model_data[[response_name]]),
    predicted = predict(model, type = "response"),
    weight    = w
  )

  # --- Grouping ---
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

  # --- Aggregation ---
  # frequency:  sum(counts) / sum(exposure)
  # severity etc.: weighted mean of the response with the GLM weights
  agg <- df %>%
    group_by(bin_group) %>%
    summarise(
      x_plot        = if (x_is_numeric) weighted.mean(x_var, weight) else first(as.character(x_var)),
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

  # --- Labels ---
  default_metric <- if (use_rate) "Frequency" else paste("Avg.", response_name)
  if (is.null(title)) title <- paste(default_metric, "–", predictor)
  if (is.null(xlab))  xlab  <- predictor
  if (is.null(ylab))  ylab  <- default_metric

  xaxis_cfg <- list(title = xlab, tickangle = -45,
                    tickfont = list(size = 9), showgrid = FALSE)
  if (!x_is_numeric) {
    xaxis_cfg$categoryorder <- "array"
    xaxis_cfg$categoryarray <- agg$x_plot
  }

  # --- Plot ---
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

# ── Model diagnostics ────────────────────────────────────────────────
# Compact fit summary for a frequency and/or severity model. Flags
# overdispersion for Poisson/binomial families (Pearson dispersion > 1.2),
# in which case standard errors are understated and a quasi- or negative
# binomial family should be considered.
#
# Returns a data.frame with one row per model:
#   Model, Family, Link, N, Deviance, DFResidual, AIC, Dispersion
#   (Pearson chi^2 / df) and DevianceExplained (1 - deviance/null deviance).
glm_diagnostics <- function(model_freq = NULL, model_sev = NULL) {

  if (is.null(model_freq) && is.null(model_sev)) {
    stop("Provide at least one model.")
  }

  one <- function(model, model_name) {
    if (is.null(model)) return(NULL)
    if (!inherits(model, "glm"))
      stop("glm_diagnostics: '", model_name, "' must be a glm object.",
           call. = FALSE)
    fam  <- family(model)
    disp <- sum(residuals(model, type = "pearson")^2) / model$df.residual

    if (fam$family %in% c("poisson", "binomial") && disp > 1.2)
      warning("glm_diagnostics: model '", model_name, "' shows ",
              "overdispersion (Pearson dispersion = ", round(disp, 2),
              " > 1); standard errors are understated. Consider a quasi-",
              fam$family, " or negative binomial family.", call. = FALSE)

    data.frame(
      Model             = model_name,
      Family            = fam$family,
      Link              = fam$link,
      N                 = stats::nobs(model),
      Deviance          = model$deviance,
      DFResidual        = model$df.residual,
      AIC               = suppressWarnings(
                            tryCatch(stats::AIC(model),
                                     error = function(e) NA_real_)),
      Dispersion        = disp,
      DevianceExplained = 1 - model$deviance / model$null.deviance,
      stringsAsFactors  = FALSE
    )
  }

  out <- rbind(one(model_freq, "frequency"), one(model_sev, "severity"))
  rownames(out) <- NULL
  out
}

# ── Binned residual plot ─────────────────────────────────────────────
# For individual policy rows raw residual plots are unreadable (Poisson
# residuals are dominated by the 0/1 claim pattern). This plots the MEAN
# residual per (quantile) bin of the fitted value — or of a predictor —
# together with a ±2·SE band: under a correctly specified model ~95% of
# the bin means should fall inside the band. Systematic patterns outside
# the band indicate missed structure (candidate terms/splines).
#
# Arguments:
#   model          fitted glm object
#   predictor      optional: bin by this predictor instead of fitted values
#   n_bins         number of quantile bins for numeric x (default 50)
#   residual_type  "pearson" (default) or "deviance"
plot_glm_residuals <- function(model, predictor = NULL, n_bins = 50,
                               residual_type = c("pearson", "deviance")) {

  residual_type <- match.arg(residual_type)
  if (!inherits(model, "glm")) stop("'model' must be a glm object.")
  if (n_bins < 2) stop("plot_glm_residuals: 'n_bins' must be at least 2.")

  r    <- as.numeric(residuals(model, type = residual_type))
  disp <- sum(residuals(model, type = "pearson")^2) / model$df.residual

  if (is.null(predictor)) {
    x    <- as.numeric(fitted(model))
    xlab <- "Fitted value"
  } else {
    tr <- .glm_training_data(model, "plot_glm_residuals")
    if (predictor %in% names(tr$mf)) {
      x <- tr$mf[[predictor]]
    } else if (!is.null(tr$data) && predictor %in% names(tr$data)) {
      x <- tr$data[[predictor]]
    } else {
      stop(paste0("Predictor '", predictor, "' not found."))
    }
    xlab <- predictor
  }

  df <- data.frame(x = x, r = r)
  x_numeric <- is.numeric(df$x)
  if (x_numeric && length(unique(df$x)) > n_bins) {
    brks <- unique(stats::quantile(df$x,
                                   probs = seq(0, 1, length.out = n_bins + 1),
                                   na.rm = TRUE, names = FALSE))
    if (length(brks) < 2) brks <- range(df$x, na.rm = TRUE) + c(-0.5, 0.5)
    df$bin_group <- cut(df$x, breaks = brks, include.lowest = TRUE, dig.lab = 4)
  } else {
    df$bin_group <- factor(df$x)
  }

  agg <- df %>%
    group_by(bin_group) %>%
    summarise(
      x_plot = if (x_numeric) mean(x) else first(as.character(x)),
      mean_r = mean(r),
      n      = n(),
      .groups = "drop"
    ) %>%
    mutate(band = 2 * sqrt(disp / n))
  agg <- if (x_numeric) agg %>% arrange(x_plot) else agg %>% arrange(bin_group)

  ht_mean <- paste0("<b>", xlab, ":</b> %{x}",
                    "<br><b>Mean residual:</b> %{y:.3f}",
                    "<br>%{text}<extra></extra>")

  xaxis_cfg <- list(title = xlab, tickangle = -45,
                    tickfont = list(size = 9), showgrid = FALSE)
  if (!x_numeric) {
    xaxis_cfg$categoryorder <- "array"
    xaxis_cfg$categoryarray <- agg$x_plot
  }

  p <- plot_ly()

  if (x_numeric) {
    # ±2·SE band as a shaded ribbon
    p <- p %>%
      add_trace(data = agg, x = ~x_plot, y = ~band,
                type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                name = "±2·SE", legendgroup = "band",
                showlegend = TRUE, hoverinfo = "skip") %>%
      add_trace(data = agg, x = ~x_plot, y = ~-band,
                type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                fill = "tonexty", fillcolor = "rgba(168,200,224,0.25)",
                name = "±2·SE", legendgroup = "band",
                showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(data = agg, x = ~x_plot, y = ~mean_r,
                type = "scatter", mode = "lines+markers",
                name = "Mean residual",
                line   = list(color = ta_blue, width = 1.6),
                marker = list(color = ta_blue, size = 6),
                text = ~paste0("n = ", n), hovertemplate = ht_mean)
  } else {
    # Categorical x: error bars instead of a ribbon
    p <- p %>%
      add_trace(data = agg, x = ~x_plot, y = ~mean_r,
                type = "scatter", mode = "markers",
                name = "Mean residual",
                marker  = list(color = ta_blue, size = 8),
                error_y = list(type = "data", array = agg$band,
                               color = ta_muted, thickness = 1),
                text = ~paste0("n = ", n), hovertemplate = ht_mean)
  }

  p %>%
    layout(
      title  = list(text = paste0("Binned ", residual_type, " residuals – ",
                                  xlab),
                    font = list(color = ta_navy, size = 14)),
      xaxis  = xaxis_cfg,
      yaxis  = list(title = paste("Mean", residual_type, "residual"),
                    gridcolor = "#D0D8E0", zeroline = FALSE),
      shapes = list(list(type = "line", xref = "paper",
                         x0 = 0, x1 = 1, y0 = 0, y1 = 0,
                         line = list(color = ta_muted, width = 1,
                                     dash = "dot"))),
      legend       = list(orientation = "h", y = -0.2),
      hoverlabel   = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white",
      paper_bgcolor= "white",
      margin       = list(b = 80, r = 40)
    ) %>%
    config(
      displayModeBar         = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      toImageButtonOptions   = list(format = "png", filename = "residuals")
    )
}

# ── Rating table ─────────────────────────────────────────────────────
# Builds a table of multiplicative rating factors from a frequency and/or
# severity model (both log link).
#
# Base convention (see header): categorical = reference level, continuous =
# median (extra grid point, IsBase = TRUE), exposure column = 1. As a
# result, for log-link models without interactions the identity holds
# exactly: intercept × product(factors) = model prediction per exposure
# unit. A variable that is absent from one of the two models gets the
# neutral factor 1 there (so Factor_Premium remains correct).
#
# Interaction rows contain the joint relativity relative to the base cell
# AND the pure uplift: Uplift = joint / (main_factor_x × main_factor_group).
#
# Arguments:
#   model_freq    glm frequency model (or NULL)
#   model_sev     glm severity model (or NULL)
#   data          original data (for base values, grids and exposure)
#   grid_res      number of grid points for continuous variables (default 50)
#   exposure_col  exposure column (default "Exposure")
#   claims_col    claim count column (default "AantalClaims")
#   base_level    "first" = first xlevel as reference (default);
#                 "exposure" = level with the largest exposure as reference
#   trim          quantiles for the continuous grid, e.g. c(0.005, 0.995)
#                 to avoid outliers/extrapolation in the tails
#                 (default c(0, 1) = full range)
#   min_claims    thin-cell threshold: levels/grid points with fewer claims
#                 get IsThin = TRUE (default 30)
#   full_cred_claims  full-credibility claim standard for the Credibility
#                 column (default 1082 = observed frequency within ±5% of
#                 the true value with 90% confidence)
make_rating_table <- function(model_freq = NULL,
                              model_sev  = NULL,
                              data,
                              grid_res   = 50,
                              exposure_col = "Exposure",
                              claims_col   = "AantalClaims",
                              base_level   = c("first", "exposure"),
                              trim         = c(0, 1),
                              min_claims   = 30,
                              full_cred_claims = 1082) {

  base_level <- match.arg(base_level)
  if (is.null(model_freq) && is.null(model_sev)) {
    stop("Provide at least one model.")
  }
  .check_cols(data, c(exposure_col, claims_col), "make_rating_table")
  if (length(trim) != 2 || any(trim < 0) || any(trim > 1) || trim[1] >= trim[2])
    stop("make_rating_table: 'trim' must be two quantiles with trim[1] < trim[2].")
  if (min_claims < 0 || full_cred_claims <= 0)
    stop("make_rating_table: 'min_claims' and 'full_cred_claims' must be positive.")

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

  # ── Helpers ───────────────────────────────────────────────────────
  # Underlying column of a term label (shared helper, bound to `data`)
  base_var <- function(term) .term_base_var(term, data)

  # xlevels per model, keyed by the UNDERLYING column name, so that inline
  # transformations such as factor(YEAR) are also recognised as categorical
  # (previously this crashed: the variable was treated as continuous).
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

  # Base value per variable (raw value, not yet coerced):
  #   categorical -> reference level; continuous -> median; exposure -> 1
  base_value_raw <- function(bv) {
    if (bv == exposure_col) return(1)
    if (var_is_cat(bv)) {
      lv <- var_levels(bv)
      if (base_level == "exposure") {
        ag   <- d_dt[, .(E = sum(get(exposure_col), na.rm = TRUE)), by = bv]
        cand <- as.character(ag[[bv]])[order(-ag$E)]
        lev  <- cand[cand %in% lv][1]
        if (is.na(lev)) lev <- lv[1]
        lev
      } else {
        lv[1]
      }
    } else if (is.numeric(data[[bv]])) {
      stats::median(data[[bv]], na.rm = TRUE)
    } else {
      names(which.max(table(data[[bv]])))
    }
  }

  # Base row for a model: all predictors at their base value, with the
  # exposure column set to 1 so the intercept is a value per exposure unit
  # (previously: median exposure -> misscaled).
  base_df_for <- function(model) {
    vars <- .rhs_vars(model)
    missing_vars <- setdiff(vars, names(data))
    if (length(missing_vars))
      stop("variable(s) from the model formula not in 'data': ",
           paste(missing_vars, collapse = ", "))
    row <- lapply(vars, function(nm) .coerce_like(base_value_raw(nm), data[[nm]]))
    names(row) <- vars
    as.data.frame(row, stringsAsFactors = FALSE)
  }

  # Factors for one variable: prediction on grid / prediction on base row.
  # Both with all other predictors at their base value, hence consistent
  # with intercept_val. Variable not in the model -> neutral factor 1.
  predict_factor <- function(model, var_name, grid_vals) {
    n <- length(grid_vals)
    if (is.null(model)) return(rep(NA_real_, n))
    if (!var_name %in% .rhs_vars(model)) return(rep(1, n))
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    newd[[var_name]] <- .coerce_like(grid_vals, data[[var_name]])
    preds <- as.numeric(predict(model, newdata = newd,   type = "response"))
    ref   <- as.numeric(predict(model, newdata = base_df, type = "response"))
    preds / ref
  }

  # Joint relativity over a 2D grid (vx × vg), rest at base, scaled to the
  # base cell (index i_base). vx varies fastest.
  predict_factor_2d <- function(model, vx, gx, vg, gg, i_base) {
    n <- length(gx) * length(gg)
    if (is.null(model)) return(rep(NA_real_, n))
    vars    <- .rhs_vars(model)
    base_df <- base_df_for(model)
    newd    <- base_df[rep(1, n), , drop = FALSE]
    if (vx %in% vars)
      newd[[vx]] <- .coerce_like(rep(gx, times = length(gg)), data[[vx]])
    if (vg %in% vars)
      newd[[vg]] <- .coerce_like(rep(gg, each  = length(gx)), data[[vg]])
    preds <- as.numeric(predict(model, newdata = newd, type = "response"))
    preds / preds[i_base]
  }

  # Base variables per model (interactions split up, deduplicated)
  model_bases <- function(model) {
    if (is.null(model)) return(character(0))
    labs  <- attr(terms(model), "term.labels")
    comps <- unique(unlist(strsplit(labs, ":", fixed = TRUE)))
    unique(vapply(comps, base_var, character(1)))
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

  # Two-way interaction pairs (base variables) from both models, deduplicated
  gather_pairs <- function() {
    labs <- unique(c(if (!is.null(model_freq)) attr(terms(model_freq), "term.labels"),
                     if (!is.null(model_sev))  attr(terms(model_sev),  "term.labels")))
    ints <- labs[grepl(":", labs, fixed = TRUE)]
    seen <- character(0); res <- list()
    for (lab in ints) {
      comps <- strsplit(lab, ":", fixed = TRUE)[[1]]
      if (length(comps) != 2) next                       # two-way only
      bvs <- unname(vapply(comps, base_var, character(1)))
      key <- paste(sort(bvs), collapse = ":")
      if (key %in% seen) next
      seen <- c(seen, key); res[[length(res) + 1L]] <- bvs
    }
    res
  }

  # ── Build the rows per base variable ──────────────────────────────
  rows <- lapply(uniq_bases, function(bv) tryCatch({

    is_cat   <- var_is_cat(bv)
    base_raw <- base_value_raw(bv)

    # Grid: categorical = all levels; continuous = (trimmed) range with the
    # median as an explicit extra grid point so there is a row with factor 1
    if (is_cat) {
      grid_vals <- var_levels(bv)
      is_base   <- grid_vals == as.character(base_raw)
    } else {
      rng       <- stats::quantile(data[[bv]], probs = trim,
                                   na.rm = TRUE, names = FALSE)
      grid_vals <- sort(unique(c(seq(rng[1], rng[2], length.out = grid_res),
                                 base_raw)))
      is_base   <- grid_vals == base_raw
    }

    # Exposure / claims aggregation
    if (is_cat) {
      ag <- d_dt[, .(Exposure   = sum(get(exposure_col), na.rm = TRUE),
                     ClaimCount = sum(get(claims_col),   na.rm = TRUE)),
                 by = bv]
      setnames(ag, bv, "Level")
      ag$Level <- as.character(ag$Level)
      exposure    <- ag$Exposure[match(as.character(grid_vals), ag$Level)]
      claim_count <- ag$ClaimCount[match(as.character(grid_vals), ag$Level)]
      exposure[is.na(exposure)]       <- 0
      claim_count[is.na(claim_count)] <- 0
    } else {
      # Continuous variables: exposure histogram via binning on the grid
      brks <- c(-Inf, grid_vals[-1] - diff(grid_vals)/2, Inf)
      bin  <- findInterval(data[[bv]], brks)
      ag   <- data.table::data.table(
        bin        = bin,
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
    f_freq <- predict_factor(model_freq, bv, grid_vals)
    f_sev  <- predict_factor(model_sev,  bv, grid_vals)

    data.frame(
      Variable         = bv,
      Type             = if (is_cat) "categorical" else "continuous",
      Level            = as.character(grid_vals),
      LevelNum         = suppressWarnings(as.numeric(as.character(grid_vals))),
      Group            = NA_character_,
      IsBase           = is_base,
      Exposure         = exposure,
      ClaimCount       = claim_count,
      Factor_Frequency = f_freq,
      Factor_Severity  = f_sev,
      Factor_Premium   = f_freq * f_sev,
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

  # ── Build the (two-way) interaction rows ──────────────────────────
  # Per pair: one variable becomes the X-axis, the other the "Group". The
  # factor is the joint relativity relative to the base cell (both at
  # base); the uplift is joint / (main_factor_x × main_factor_group).
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

    gx <- if (cx) var_levels(xv) else {
      rng <- stats::quantile(data[[xv]], probs = trim, na.rm = TRUE, names = FALSE)
      sort(unique(c(seq(rng[1], rng[2], length.out = grid_res), base_x)))
    }
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

    # Exposure/claims per cell: only meaningful for categorical × categorical
    expo <- rep(NA_real_, nx * ng); clm <- rep(NA_real_, nx * ng)
    if (cx && cg) {
      ag <- d_dt[, .(E = sum(get(exposure_col), na.rm = TRUE),
                     C = sum(get(claims_col),   na.rm = TRUE)), by = c(xv, gv)]
      k  <- paste(as.character(ag[[xv]]), as.character(ag[[gv]]), sep = "\r")
      ck <- paste(as.character(xvals),    as.character(gvals),    sep = "\r")
      expo <- ag$E[match(ck, k)]; expo[is.na(expo)] <- 0
      clm  <- ag$C[match(ck, k)]; clm[is.na(clm)]   <- 0
    }

    ff <- predict_factor_2d(model_freq, xv, gx, gv, gg, i_base)
    fs <- predict_factor_2d(model_sev,  xv, gx, gv, gg, i_base)

    # Pure interaction uplift: joint / (main_factor_x × main_factor_group)
    uf <- ff / (rep(predict_factor(model_freq, xv, gx), times = ng) *
                rep(predict_factor(model_freq, gv, gg), each  = nx))
    us <- fs / (rep(predict_factor(model_sev,  xv, gx), times = ng) *
                rep(predict_factor(model_sev,  gv, gg), each  = nx))

    data.frame(
      Variable         = paste(b1, b2, sep = ":"),
      Type             = if (cx) "categorical" else "continuous",
      Level            = as.character(xvals),
      LevelNum         = suppressWarnings(as.numeric(as.character(xvals))),
      Group            = as.character(gvals),
      IsBase           = seq_len(nx * ng) == i_base,
      Exposure         = expo,
      ClaimCount       = clm,
      Factor_Frequency = ff,
      Factor_Severity  = fs,
      Factor_Premium   = ff * fs,
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
  if (length(int_rows)) out <- rbind(out, do.call(rbind, int_rows))

  # ── Credibility / thin-cell flags ──────────────────────────────────
  # Limited-fluctuation (square-root) credibility on the claim count per
  # level: Z = min(1, sqrt(claims / full_cred_claims)). Z quantifies how
  # much STANDALONE experience backs a level; the GLM factor itself
  # already pools information across the portfolio, so a low Z does not
  # invalidate the factor but does mean it leans heavily on the model
  # structure rather than on that level's own data.
  out$Credibility <- ifelse(is.na(out$ClaimCount), NA_real_,
                            pmin(1, sqrt(out$ClaimCount / full_cred_claims)))
  out$IsThin <- ifelse(is.na(out$ClaimCount), NA, out$ClaimCount < min_claims)

  # Warn for thin CATEGORICAL main-effect levels (thin tail bins are
  # normal for continuous grids and would make the warning noisy)
  n_thin <- sum(out$IsThin & is.na(out$Group) & out$Type == "categorical",
                na.rm = TRUE)
  if (n_thin > 0)
    warning("make_rating_table: ", n_thin, " categorical level(s) have ",
            "fewer than ", min_claims, " claims; the corresponding factors ",
            "rest on little standalone experience (see IsThin/Credibility).",
            call. = FALSE)

  # ── Intercepts (base values on the response scale, exposure = 1) ──
  intercept_val <- function(model) {
    if (is.null(model)) return(NA_real_)
    as.numeric(predict(model, newdata = base_df_for(model), type = "response"))
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

# ── Rating plot ──────────────────────────────────────────────────────
make_rating_plot <- function(rating_tbl, var, metric_fmt = 4, metric = NULL) {

  d <- rating_tbl[rating_tbl$Variable == var, , drop = FALSE]
  if (nrow(d) == 0) stop("Variable '", var, "' not found.")

  is_interaction <- any(!is.na(d$Group))

  # ── Regular main-effect plot ────────────────────────────────────────
  if (!is_interaction) {
    is_cat <- d$Type[1] == "categorical"
    d$.x <- if (is_cat) factor(d$Level, levels = d$Level) else d$LevelNum

    mode_line <- if (is_cat) "markers" else "lines+markers"

    # Thin cells (IsThin from make_rating_table): dim the markers and add
    # a hover note, so low-credibility levels are visually distinct
    thin <- if ("IsThin" %in% names(d)) d$IsThin %in% TRUE else rep(FALSE, nrow(d))
    d$.thin_label <- ifelse(thin, "<br><i>⚠ low claim volume</i>", "")
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
               title  = list(text = paste("Rating factors –", var),
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
                    modeBarButtonsToRemove = c("lasso2d","select2d"),
                    toImageButtonOptions = list(format = "png", filename = "rating")))
  }

  # ── Interaction plot: one series per level of the group variable ────
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
  # categorical × categorical (elsewhere Exposure is NA in the table)
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
    dg$.thin_label <- ifelse(thin_g, "<br><i>⚠ low claim volume</i>", "")
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
      title = list(text = paste0("Interaction – ", xvar, " × ", gvar,
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
           modeBarButtonsToRemove = c("lasso2d","select2d"),
           toImageButtonOptions = list(format = "png", filename = "interaction"))
}

# ── Premium impact (dislocation) analysis ────────────────────────────
# Compares premiums under a NEW model set against an OLD model set (or an
# existing premium column) on a per-policy basis. Premiums are computed as
# rates per unit of exposure (offsets neutralised at exposure = 1); the
# premium is the product of the models supplied (frequency × severity, or
# a single model when only one side is modelled).
#
# By default the new premiums are REBASED so that the exposure-weighted
# totals match the old ones — that isolates the redistribution
# (dislocation) from the overall rate-level change, which is reported
# separately in the summary.
#
# Arguments:
#   data               dataset with raw rows
#   model_freq_new / model_sev_new   the new (candidate) models
#   model_freq_old / model_sev_old   the old models, OR:
#   old_premium_col    column in `data` with the current premium
#   old_premium_basis  "amount" (default; premium for the record, divided
#                      by exposure internally) or "rate" (already per
#                      unit of exposure)
#   rebase             TRUE = scale new premiums to the old total (default)
#   by                 optional character vector of columns for a per-level
#                      impact breakdown (exposure-weighted mean change)
#   n_show             number of rows in the winners/losers tables
#   exposure_col       exposure column (default "Exposure")
#
# Returns a list:
#   summary            display table with the headline numbers
#   stats              the same numbers as a named list (programmatic use)
#   policy             per-row: OldRate, NewRate, NewRateRebased, ChangePct
#   by_level           exposure-weighted mean change per level (if `by`)
#   largest_increases / largest_decreases   top-n dislocations
#   plot               exposure-weighted histogram of the premium changes
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
                                  sprintf(" — rate-level change %+.1f%%",
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

# ── Excel export of the rating table ─────────────────────────────────
# Writes the make_rating_table() output to a formatted .xlsx:
#   * "Overview" sheet with the intercepts (per exposure unit), the base
#     value per variable and a timestamp;
#   * one sheet per main-effect variable (base row highlighted, thin rows
#     greyed out);
#   * one sheet per interaction: the long table plus a Level × Group
#     matrix of the premium factor.
# Requires the 'openxlsx' package.
export_rating_table <- function(rating_tbl, file = "rating_table.xlsx",
                                overwrite = TRUE, digits = 4) {

  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("export_rating_table: package 'openxlsx' is required ",
         "(install.packages(\"openxlsx\")).", call. = FALSE)
  .check_cols(rating_tbl, c("Variable", "Level", "Group", "IsBase"),
              "export_rating_table")

  st_header <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = ta_navy,
                                     textDecoration = "bold")
  st_base   <- openxlsx::createStyle(fgFill = ta_lightblue,
                                     textDecoration = "bold")
  st_thin   <- openxlsx::createStyle(fontColour = "#999999",
                                     textDecoration = "italic")
  st_fac    <- openxlsx::createStyle(numFmt = paste0("0.",
                                                     strrep("0", digits)))
  st_int    <- openxlsx::createStyle(numFmt = "#,##0")

  wb         <- openxlsx::createWorkbook()
  used_names <- character(0)
  sheet_name <- function(nm) {
    nm   <- substr(gsub("[^A-Za-z0-9 _.-]", "_", nm), 1, 31)
    base <- nm; i <- 1
    while (nm %in% used_names) {
      nm <- paste0(substr(base, 1, 28), "_", i); i <- i + 1
    }
    used_names <<- c(used_names, nm)
    nm
  }
  fmt_att <- function(a) if (is.null(a) || is.na(a)) "-" else
    format(a, digits = 6, scientific = FALSE)

  write_sheet <- function(nm, d) {
    sn <- sheet_name(nm)
    openxlsx::addWorksheet(wb, sn)
    openxlsx::writeData(wb, sn, d, headerStyle = st_header)
    nc <- ncol(d)
    base_rows <- which(d$IsBase %in% TRUE) + 1
    if (length(base_rows))
      openxlsx::addStyle(wb, sn, st_base, rows = base_rows,
                         cols = seq_len(nc), gridExpand = TRUE, stack = TRUE)
    if ("IsThin" %in% names(d)) {
      thin_rows <- which(d$IsThin %in% TRUE) + 1
      if (length(thin_rows))
        openxlsx::addStyle(wb, sn, st_thin, rows = thin_rows,
                           cols = seq_len(nc), gridExpand = TRUE, stack = TRUE)
    }
    fac_cols <- grep("^(Factor|Uplift|Credibility)", names(d))
    if (length(fac_cols))
      openxlsx::addStyle(wb, sn, st_fac, rows = seq_len(nrow(d)) + 1,
                         cols = fac_cols, gridExpand = TRUE, stack = TRUE)
    int_cols <- which(names(d) %in% c("Exposure", "ClaimCount"))
    if (length(int_cols))
      openxlsx::addStyle(wb, sn, st_int, rows = seq_len(nrow(d)) + 1,
                         cols = int_cols, gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, sn, cols = seq_len(nc), widths = "auto")
    openxlsx::freezePane(wb, sn, firstRow = TRUE)
    sn
  }

  # ── Overview sheet ──────────────────────────────────────────────────
  bv <- attr(rating_tbl, "base_values")
  ov <- data.frame(
    Item  = c("Generated",
              "Intercept frequency (per exposure unit)",
              "Intercept severity",
              "Intercept premium"),
    Value = c(format(Sys.time(), "%Y-%m-%d %H:%M"),
              fmt_att(attr(rating_tbl, "intercept_frequency")),
              fmt_att(attr(rating_tbl, "intercept_severity")),
              fmt_att(attr(rating_tbl, "intercept_premium"))),
    stringsAsFactors = FALSE
  )
  if (!is.null(bv))
    ov <- rbind(ov, data.frame(
      Item  = paste("Base value", names(bv)),
      Value = vapply(bv, function(x) paste(format(x), collapse = ", "),
                     character(1)),
      stringsAsFactors = FALSE))
  sn <- sheet_name("Overview")
  openxlsx::addWorksheet(wb, sn)
  openxlsx::writeData(wb, sn, ov, headerStyle = st_header)
  openxlsx::setColWidths(wb, sn, cols = 1:2, widths = "auto")

  # ── Main-effect sheets ──────────────────────────────────────────────
  keep_main <- intersect(c("Level", "IsBase", "Exposure", "ClaimCount",
                           "Credibility", "IsThin", "Factor_Frequency",
                           "Factor_Severity", "Factor_Premium"),
                         names(rating_tbl))
  for (v in unique(rating_tbl$Variable[is.na(rating_tbl$Group)])) {
    d <- rating_tbl[rating_tbl$Variable == v & is.na(rating_tbl$Group),
                    keep_main, drop = FALSE]
    write_sheet(v, d)
  }

  # ── Interaction sheets: long table + premium-factor matrix ─────────
  keep_int <- intersect(c("Level", "Group", "IsBase", "Exposure",
                          "ClaimCount", "IsThin",
                          "Factor_Frequency", "Factor_Severity",
                          "Factor_Premium", "Uplift_Frequency",
                          "Uplift_Severity", "Uplift_Premium"),
                        names(rating_tbl))
  for (v in unique(rating_tbl$Variable[!is.na(rating_tbl$Group)])) {
    d  <- rating_tbl[rating_tbl$Variable == v & !is.na(rating_tbl$Group),
                     keep_int, drop = FALSE]
    sn <- write_sheet(v, d)

    fac_col <- Filter(function(cc) cc %in% names(d) && any(!is.na(d[[cc]])),
                      c("Factor_Premium", "Factor_Frequency",
                        "Factor_Severity"))[1]
    if (!is.na(fac_col) && length(fac_col)) {
      lv <- unique(d$Level); gv <- unique(d$Group)
      m  <- tapply(d[[fac_col]], list(factor(d$Level, levels = lv),
                                      factor(d$Group, levels = gv)), mean)
      piv <- data.frame(Level = rownames(m), as.data.frame(m),
                        check.names = FALSE, stringsAsFactors = FALSE)
      start <- nrow(d) + 3
      openxlsx::writeData(wb, sn,
                          paste0("Matrix ", fac_col, " (rows = Level, ",
                                 "columns = Group)"),
                          startRow = start)
      openxlsx::writeData(wb, sn, piv, startRow = start + 1,
                          headerStyle = st_header)
      openxlsx::addStyle(wb, sn, st_fac,
                         rows = start + 1 + seq_len(nrow(piv)),
                         cols = 1 + seq_len(ncol(piv) - 1),
                         gridExpand = TRUE, stack = TRUE)
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = overwrite)
  invisible(normalizePath(file))
}

# ── Pricing report (HTML) ────────────────────────────────────────────
# Bundles the whole analysis into a single self-browsable HTML report:
# model diagnostics (fit table + binned residuals) and, per variable,
# the one-way observed plot, actual vs expected, partial dependence and
# the rating-factor plot, plus a section with the interaction plots.
#
# Built on htmltools::save_html (no pandoc required); the plots are the
# regular interactive plotly widgets. Next to the .html file a
# "<name>_files/" folder is written with the JavaScript dependencies —
# keep the two together when sharing the report.
#
# Arguments:
#   model_freq / model_sev  fitted glm objects (at least one)
#   data          dataset with raw rows
#   file          output path of the .html file
#   title         report title
#   variables     which variables get a section (default: all base
#                 variables of both models)
#   include       which blocks to render: any of "diagnostics", "oneway",
#                 "ae", "pdp", "rating" (default: all)
#   by_year       split the one-way plots by accounting year
#   grid_res, base_level, trim   passed to make_pdp()/make_rating_table()
#   *_col         column names, as elsewhere
pricing_report <- function(model_freq = NULL, model_sev = NULL, data,
                           file  = "pricing_report.html",
                           title = "GLM Pricing Report",
                           variables = NULL,
                           include = c("diagnostics", "oneway", "ae",
                                       "pdp", "rating"),
                           by_year = FALSE,
                           exposure_col = "Exposure",
                           claims_col   = "AantalClaims",
                           loss_col     = "SCHADELAST",
                           year_col     = "BOEKJAAR",
                           grid_res     = 50,
                           base_level   = c("first", "exposure"),
                           trim         = c(0, 1)) {

  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("pricing_report: package 'htmltools' is required.", call. = FALSE)
  include    <- match.arg(include, several.ok = TRUE)
  base_level <- match.arg(base_level)
  if (is.null(model_freq) && is.null(model_sev))
    stop("Provide at least one model.")
  .check_cols(data, c(exposure_col, claims_col, loss_col,
                      if (by_year) year_col), "pricing_report")

  if (is.null(variables))
    variables <- unique(c(.model_base_vars(model_freq, data),
                          .model_base_vars(model_sev, data)))
  unknown <- setdiff(variables, names(data))
  if (length(unknown)) {
    warning("pricing_report: variable(s) skipped, not in 'data': ",
            paste(unknown, collapse = ", "), call. = FALSE)
    variables <- intersect(variables, names(data))
  }
  variables <- setdiff(variables, exposure_col)
  if (!length(variables)) stop("pricing_report: no variables to report on.")

  h      <- htmltools::tags
  anchor <- function(v) gsub("[^A-Za-z0-9_-]", "_", v)
  safe   <- function(expr) tryCatch(expr, error = function(e)
    h$p(class = "ta-error", paste("Could not render:", conditionMessage(e))))

  fmt_num <- function(x, digits = 4) {
    ifelse(is.na(x), "",
           ifelse(abs(x) >= 1000,
                  formatC(x, format = "d", big.mark = ","),
                  formatC(x, format = "fg", digits = digits)))
  }
  html_table <- function(df, digits = 4) {
    rows <- lapply(seq_len(nrow(df)), function(i) {
      h$tr(lapply(seq_along(df), function(j) {
        x <- df[i, j]
        h$td(if (is.numeric(x)) fmt_num(x, digits) else as.character(x))
      }))
    })
    h$table(class = "ta-table",
            h$thead(h$tr(lapply(names(df), h$th))),
            h$tbody(rows))
  }
  fml <- function(model, name) {
    if (is.null(model)) return(NULL)
    h$p(class = "ta-meta", h$b(name), ": ",
        paste(deparse(formula(model), width.cutoff = 500), collapse = " "))
  }
  in_model_data <- function(model, v) {
    !is.null(model) &&
      (v %in% names(stats::model.frame(model)) ||
         (!is.null(model$data) && is.data.frame(model$data) &&
            v %in% names(model$data)))
  }

  css <- h$style(htmltools::HTML("
    body{font-family:'Segoe UI',Arial,sans-serif;color:#222;margin:0;background:#fff;}
    .ta-wrap{max-width:1100px;margin:0 auto;padding:24px;}
    h1{color:#00365E;}
    h2{color:#00365E;border-bottom:2px solid #A8C8E0;padding-top:18px;}
    h3{color:#0073AB;margin-bottom:4px;}
    .ta-table{border-collapse:collapse;margin:10px 0;font-size:13px;}
    .ta-table th{background:#00365E;color:#fff;padding:6px 10px;text-align:left;}
    .ta-table td{border-bottom:1px solid #D0D8E0;padding:5px 10px;}
    .ta-error{color:#C44536;}
    .ta-meta{color:#6B7A8D;font-size:13px;margin:2px 0;}
    .ta-toc{margin:14px 0;}
    .ta-toc a{color:#0073AB;text-decoration:none;margin-right:14px;}
  "))

  # Rating table (once; feeds the per-variable and interaction plots)
  tbl <- NULL
  if ("rating" %in% include) {
    tbl <- tryCatch(
      make_rating_table(model_freq, model_sev, data = data,
                        grid_res = grid_res, exposure_col = exposure_col,
                        claims_col = claims_col, base_level = base_level,
                        trim = trim),
      error = function(e) {
        warning("pricing_report: rating table failed: ",
                conditionMessage(e), call. = FALSE)
        NULL
      })
  }

  diag_sec <- NULL
  if ("diagnostics" %in% include) {
    dg <- glm_diagnostics(model_freq, model_sev)
    diag_sec <- htmltools::tagList(
      h$h2(id = "diagnostics", "Model diagnostics"),
      html_table(dg),
      if (!is.null(model_freq)) htmltools::tagList(
        h$h3("Binned residuals — frequency"),
        safe(plot_glm_residuals(model_freq))),
      if (!is.null(model_sev)) htmltools::tagList(
        h$h3("Binned residuals — severity"),
        safe(plot_glm_residuals(model_sev)))
    )
  }

  var_secs <- lapply(variables, function(v) {
    parts <- list(h$h2(id = paste0("var-", anchor(v)), v))
    if ("oneway" %in% include)
      parts <- c(parts, list(
        h$h3("One-way observed"),
        safe(make_plot(data, v, "Frequency", ta_blue, "Frequency",
                       display = "color", by_year = by_year,
                       exposure_col = exposure_col, claims_col = claims_col,
                       loss_col = loss_col, year_col = year_col))))
    if ("ae" %in% include && in_model_data(model_freq, v))
      parts <- c(parts, list(
        h$h3("Actual vs expected — frequency"),
        safe(plot_glm_predictor(model_freq, v))))
    if ("ae" %in% include && in_model_data(model_sev, v))
      parts <- c(parts, list(
        h$h3("Actual vs expected — severity"),
        safe(plot_glm_predictor(model_sev, v))))
    if ("pdp" %in% include && !is.null(model_freq) &&
        v %in% .rhs_vars(model_freq))
      parts <- c(parts, list(
        h$h3("Partial dependence — frequency"),
        safe(make_pdp(model_freq, data, v, metric = "Frequency",
                      grid_res = grid_res, exposure_col = exposure_col,
                      claims_col = claims_col, loss_col = loss_col))))
    if ("pdp" %in% include && !is.null(model_sev) &&
        v %in% .rhs_vars(model_sev))
      parts <- c(parts, list(
        h$h3("Partial dependence — severity"),
        safe(make_pdp(model_sev, data, v, metric = "Severity",
                      grid_res = grid_res, exposure_col = exposure_col,
                      claims_col = claims_col, loss_col = loss_col))))
    if (!is.null(tbl) && v %in% tbl$Variable)
      parts <- c(parts, list(h$h3("Rating factors"),
                             safe(make_rating_plot(tbl, v))))
    htmltools::tagList(parts)
  })

  int_sec <- NULL
  if (!is.null(tbl)) {
    ivars <- unique(tbl$Variable[!is.na(tbl$Group)])
    if (length(ivars))
      int_sec <- htmltools::tagList(
        h$h2(id = "interactions", "Interactions"),
        lapply(ivars, function(iv) htmltools::tagList(
          h$h3(iv), safe(make_rating_plot(tbl, iv)))))
  }

  toc <- h$p(class = "ta-toc",
             if ("diagnostics" %in% include)
               h$a(href = "#diagnostics", "Diagnostics"),
             lapply(variables, function(v)
               h$a(href = paste0("#var-", anchor(v)), v)),
             if (!is.null(int_sec)) h$a(href = "#interactions", "Interactions"))

  page <- h$div(class = "ta-wrap", css,
                h$h1(title),
                h$p(class = "ta-meta",
                    paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"),
                           " — ", format(nrow(data), big.mark = ","),
                           " rows")),
                fml(model_freq, "Frequency model"),
                fml(model_sev,  "Severity model"),
                toc, diag_sec, var_secs, int_sec)

  libdir <- paste0(sub("\\.html?$", "", basename(file)), "_files")
  htmltools::save_html(page, file = file, libdir = libdir)
  message("pricing_report: written to ", normalizePath(file),
          " (dependencies in ", libdir, "/)")
  invisible(normalizePath(file))
}
