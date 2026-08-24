# Distributional fit and influence -------------------------------------------
#
# Both functions here take a single model, so they serve a frequency and a
# severity model alike; the family decides what is computed.
#
# A normal Q-Q plot of deviance residuals - what base R's plot(glm) draws -
# is close to useless for a frequency model, because deviance residuals of
# counts are not normal. Measured on a correctly specified Poisson with
# 20,000 rows: 11.2% outside a nominal 5% band, KS p < 2e-16. The plot
# condemns a model that is exactly right. The randomised quantile residual
# of Dunn and Smyth (1996) is exactly standard normal instead, discreteness
# included (5.1%, p = 0.86 on the same data), which is what makes a Q-Q
# plot readable for counts at all. Verified identical to statmod::qresid().

# Dispersion the reference distribution should use. Poisson and binomial
# fix it at 1 by assumption, and that assumption is exactly what the plot
# is testing, so estimating it here would hide the very thing being looked
# for. Gamma and gaussian carry a free dispersion, estimated from Pearson.
.qr_dispersion <- function(model) {
  fam <- family(model)$family
  if (fam %in% c("poisson", "binomial")) return(1)
  sum(stats::residuals(model, type = "pearson")^2) / model$df.residual
}

# Randomised quantile residuals. Returns NULL when the family has no
# closed-form CDF here, so the caller can fall back and say so.
.quantile_residuals <- function(model, fn) {
  fam  <- family(model)
  tr   <- .glm_training_data(model, fn)
  y    <- as.numeric(tr$mf[[1]])
  mu   <- as.numeric(stats::fitted(model))
  w    <- tr$weights
  phi  <- .qr_dispersion(model)

  u <- switch(fam$family,
    "poisson" = ,
    "quasipoisson" = {
      a <- stats::ppois(y - 1, mu); b <- stats::ppois(y, mu)
      stats::runif(length(y), a, b)          # randomised: y is discrete
    },
    "binomial" = {
      m <- pmax(round(w), 1)
      k <- round(y * m)
      a <- stats::pbinom(k - 1, m, mu); b <- stats::pbinom(k, m, mu)
      stats::runif(length(y), a, b)
    },
    "Gamma" = {
      # y | mu ~ Gamma(shape = w/phi, scale = phi*mu/w): mean mu and
      # variance phi*mu^2/w, which is the GLM's own assumption
      stats::pgamma(y, shape = w / phi, scale = phi * mu / w)
    },
    "gaussian" = stats::pnorm(y, mu, sqrt(phi / w)),
    "inverse.gaussian" = NULL,
    NULL)

  if (is.null(u)) return(NULL)
  # Guard the tails: a u of exactly 0 or 1 would become -Inf / Inf and drop
  # the very observation the plot is about
  eps <- .Machine$double.eps
  stats::qnorm(pmin(pmax(u, eps), 1 - eps))
}

#' Normal Q-Q plot of quantile residuals
#'
#' Checks the distributional assumption of a fitted glm: not whether the
#' mean is right (that is what [plot_glm_predictor()] and
#' [plot_glm_residuals()] are for) but whether the spread and shape of the
#' response match the family. Takes a single model, so it serves a
#' frequency and a severity model alike.
#'
#' @details
#' # Why not deviance residuals
#'
#' Base R's `plot(model)` gives a normal Q-Q of standardised deviance
#' residuals, and for count data those are simply not normal. On a
#' correctly specified Poisson with 20,000 rows, 11.2% of the deviance
#' residuals fall outside a nominal 5% band and a Kolmogorov-Smirnov test
#' rejects normality at `p < 2e-16` - the plot condemns a model that is
#' exactly right. Where the fitted mean varies little the same residuals
#' also collapse onto visible bands, because the response only takes a
#' handful of values.
#'
#' This function instead uses the **randomised quantile residual** of Dunn
#' and Smyth (1996): each observation is mapped through its own fitted CDF,
#' uniformly at random within the jump for a discrete response, and then
#' through `qnorm()`. Under a correctly specified model these are exactly
#' standard normal, discreteness and all, so the reference line is the
#' identity and a departure means something. On the same data they give
#' 5.1% outside the band and `p = 0.86`. The implementation is verified
#' against `statmod::qresid()`, the reference by one of the authors:
#' identical for Poisson, equal to 6e-14 for Gamma.
#'
#' The other principled route is a simulated envelope, as `DHARMa` and
#' `hnp` use: simulate from the fitted model and locate each observation
#' in that distribution. It generalises further (mixed models,
#' zero-inflation) but costs `n_sim` simulations per row, so the
#' closed-form residual is the better fit for a large book.
#'
#' Because the count case is randomised, two calls give slightly different
#' points. Pass `seed` when you need the same picture twice.
#'
#' # Reading it
#'
#' Points on the line means the family and its variance function fit. A
#' curved tail on a Gamma severity usually means the tail is heavier than
#' Gamma allows (consider a log-normal, or capping large losses). An
#' S-shape on a frequency model points at over- or underdispersion; a
#' Poisson that is really overdispersed shows heavy tails on both sides,
#' which is the same message [glm_diagnostics()] gives numerically.
#'
#' The band is a pointwise 95% interval, from the Beta distribution of the
#' order statistics of a standard normal sample. Two caveats. It is
#' pointwise, not simultaneous, so with several thousand points a handful
#' will fall outside it even when the model is right - read the shape,
#' do not count the exceptions. And it treats the fitted coefficients as
#' known, while a simulated envelope would account for their estimation,
#' so it is marginally too narrow. Neither matters for reading curvature,
#' which is what the plot is for.
#'
#' @param model A fitted glm object. Poisson, quasipoisson, binomial,
#'   Gamma and gaussian families are supported; for anything else the
#'   function falls back to deviance residuals and says so.
#' @param n_max Plot at most this many points, evenly spaced through the
#'   sorted residuals with both extremes always kept (default 5000). The
#'   residuals are computed on every row regardless; this only thins what
#'   is drawn, so the widget stays small on a large portfolio.
#' @param band Draw the pointwise 95% envelope (default `TRUE`).
#' @param seed Optional seed for the randomisation used on a discrete
#'   response, for a reproducible plot.
#' @param title Optional plot title.
#' @param y_range Optional `c(lo, hi)` to fix the sample-quantile axis, so
#'   Q-Q plots of different models become directly comparable.
#'
#' @return A plotly object. The residuals themselves are attached as the
#'   `"residuals"` attribute, and `"type"` records whether they are
#'   quantile or deviance residuals.
#' @seealso [glm_diagnostics()] for the dispersion behind an S-shape,
#'   [plot_glm_residuals()] for mean structure.
#' @export
plot_glm_qq <- function(model, n_max = 5000, band = TRUE, seed = NULL,
                        title = NULL, y_range = NULL) {

  if (!inherits(model, "glm"))
    stop("plot_glm_qq: 'model' must be a glm object.", call. = FALSE)
  if (n_max < 10)
    stop("plot_glm_qq: 'n_max' must be at least 10.", call. = FALSE)
  y_range <- .check_range(y_range, "plot_glm_qq")
  if (!is.null(seed)) set.seed(seed)

  fam <- family(model)
  r   <- .quantile_residuals(model, "plot_glm_qq")
  rtype <- "quantile"
  if (is.null(r)) {
    warning("plot_glm_qq: no quantile residuals are available for the '",
            fam$family, "' family, so deviance residuals are shown. Those ",
            "are only approximately normal, and not at all so for a ",
            "discrete response.", call. = FALSE)
    r <- as.numeric(stats::residuals(model, type = "deviance"))
    rtype <- "deviance"
  }
  if (identical(fam$family, "quasipoisson"))
    warning("plot_glm_qq: a quasipoisson has no likelihood, so the Poisson ",
            "CDF is used and the estimated overdispersion is not reflected. ",
            "Expect tails that look heavier than they are.", call. = FALSE)

  r <- r[is.finite(r)]
  n <- length(r)
  if (n < 10)
    stop("plot_glm_qq: too few usable residuals (", n, ").", call. = FALSE)

  ord  <- sort(r)
  pp   <- (seq_len(n) - 0.5) / n           # Hazen plotting positions
  theo <- stats::qnorm(pp)

  # Thin for drawing only: keep both extremes, spread the rest evenly
  idx <- if (n > n_max) unique(c(1L, round(seq(1, n, length.out = n_max)), n))
         else seq_len(n)

  lim <- range(c(theo[idx], ord[idx]), finite = TRUE)
  p <- plot_ly()

  if (isTRUE(band)) {
    # The i-th order statistic of n standard normals has a Beta(i, n-i+1)
    # CDF value, so its pointwise interval comes straight from qbeta
    lo <- stats::qnorm(stats::qbeta(0.025, idx, n - idx + 1))
    hi <- stats::qnorm(stats::qbeta(0.975, idx, n - idx + 1))
    p <- p %>%
      add_trace(x = theo[idx], y = hi, type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                name = "95% pointwise", legendgroup = "band",
                hoverinfo = "skip") %>%
      add_trace(x = theo[idx], y = lo, type = "scatter", mode = "lines",
                line = list(color = "rgba(107,122,141,0.4)", width = 1,
                            dash = "dot"),
                fill = "tonexty", fillcolor = "rgba(168,200,224,0.25)",
                name = "95% pointwise", legendgroup = "band",
                showlegend = FALSE, hoverinfo = "skip")
  }

  p <- p %>%
    add_trace(x = lim, y = lim, type = "scatter", mode = "lines",
              name = "Perfect fit",
              line = list(color = ta_muted, width = 1, dash = "dash"),
              hoverinfo = "skip") %>%
    add_trace(x = theo[idx], y = ord[idx], type = "scatter", mode = "markers",
              name = paste0(rtype, " residuals"),
              marker = list(color = ta_blue, size = 5, opacity = 0.75),
              hovertemplate = paste0("<b>Theoretical:</b> %{x:.3f}",
                                     "<br><b>Sample:</b> %{y:.3f}",
                                     "<extra></extra>"))

  if (is.null(title))
    title <- paste0("Normal Q-Q \u2013 ", rtype, " residuals \u2013 ",
                    fam$family, "/", fam$link)

  out <- p %>%
    layout(
      title = list(text = title, font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Theoretical quantile", gridcolor = "#D0D8E0",
                   zeroline = FALSE),
      yaxis = list(title = "Sample quantile", gridcolor = "#D0D8E0",
                   zeroline = FALSE, range = y_range,
                   autorange = is.null(y_range)),
      legend = list(orientation = "h", y = -0.2),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 70, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "qq"))

  attr(out, "residuals") <- r
  attr(out, "type")      <- rtype
  out
}

#' Influence plot: leverage against residual, sized by Cook's distance
#'
#' Finds the rows that move the fit, and shows why they do. Leverage on the
#' x-axis, standardised deviance residual on the y-axis, and Cook's
#' distance as the marker size: a point is influential because it is
#' unusual in the predictors (far right), because it is badly fitted (far
#' up or down), or both.
#'
#' @details
#' # On a large portfolio
#'
#' Cook's distance was designed for regressions with tens of observations,
#' where a single point can genuinely swing a coefficient. On a book of
#' 100,000 policies no single row does, and every absolute rule of thumb
#' breaks down: `D > 1` never triggers, and `D > 4/n` flags thousands of
#' perfectly ordinary rows. So this plot is deliberately **relative**: it
#' ranks rows against each other and labels the worst `n_label`, rather
#' than testing them against a threshold that does not apply.
#'
#' What that ranking is good for is data quality: a vehicle weight of 20
#' tonnes or an age of 200 lands at the top and is worth opening. It is a
#' much weaker tool for model structure, where [detect_interactions()] and
#' [plot_glm_residuals()] have far more power.
#'
#' # What it cannot see
#'
#' Influence is not the same as misfit, and on a Poisson frequency model
#' the two can be opposites. The IRLS weight of a row is its fitted mean,
#' so a policy with a tiny exposure carries almost no weight in the design
#' matrix and has almost no leverage - whatever its response. Implant the
#' classic keying error, an exposure of 0.001 with two claims on it, and
#' it becomes the **worst-fitted row in the book** (Pearson residual 169,
#' rank 1 of 20,000) while ranking around 30th on Cook's distance, with a
#' leverage of 5e-07. Cook's distance structurally cannot see it.
#'
#' The mirror image is just as awkward: a row with extreme predictors gets
#' leverage above 0.9, tops the Cook ranking, and has an *unremarkable*
#' residual - because at that leverage the fit bends to meet it, so the
#' point masks itself.
#'
#' That is exactly why this is a two-axis plot rather than an index plot
#' of `D`. Neither axis alone finds both, and a residual check such as
#' [plot_glm_qq()] or [plot_glm_residuals()] is the complement that
#' catches the low-exposure case.
#'
#' # Cost
#'
#' The leverages come from a QR decomposition of the weighted design
#' matrix, which is the expensive part and grows with rows times
#' coefficients. `max_rows` caps it with a message rather than letting a
#' large model exhaust memory.
#'
#' @param model A fitted glm object.
#' @param n_label Number of most influential rows to highlight and label
#'   (default 10).
#' @param data_cols Extra columns from the model's training data to carry
#'   into the tooltip and the returned table. `NULL` (default) uses the
#'   model's own base variables.
#' @param n_max Plot at most this many ordinary points (default 5000); the
#'   labelled rows are always drawn on top of them.
#' @param max_rows Compute on a random sample of at most this many rows
#'   (default 2e5); `NULL` uses all of them.
#' @param seed Optional seed for that sample.
#' @param title Optional plot title.
#' @param y_range Optional `c(lo, hi)` to fix the residual axis.
#'
#' @return A plotly object, with the `n_label` most influential rows
#'   attached as the `"influential"` attribute: `Row` (row name in the
#'   training data), `CooksD`, `Leverage`, `StdResid`, `Actual`, `Fitted`
#'   and the `data_cols`.
#' @seealso [plot_glm_residuals()], [glm_diagnostics()]
#' @export
plot_glm_influence <- function(model, n_label = 10, data_cols = NULL,
                               n_max = 5000, max_rows = 2e5, seed = NULL,
                               title = NULL, y_range = NULL) {

  if (!inherits(model, "glm"))
    stop("plot_glm_influence: 'model' must be a glm object.", call. = FALSE)
  if (n_label < 1)
    stop("plot_glm_influence: 'n_label' must be at least 1.", call. = FALSE)
  y_range <- .check_range(y_range, "plot_glm_influence")
  if (!is.null(seed)) set.seed(seed)

  tr <- .glm_training_data(model, "plot_glm_influence")
  n_full <- nrow(tr$mf)

  # Sub-sample before the QR, which is what actually costs
  if (!is.null(max_rows) && n_full > max_rows) {
    message("plot_glm_influence: refitting on a sample of ",
            format(max_rows, big.mark = ","), " of ",
            format(n_full, big.mark = ","), " rows (max_rows); leverage ",
            "depends on the whole design matrix, so the values are those ",
            "of the sample. Pass max_rows = NULL to use every row.")
    keep <- sort(sample(n_full, max_rows))
    d_s  <- if (!is.null(tr$data)) tr$data[keep, , drop = FALSE] else NULL
    if (is.null(d_s))
      stop("plot_glm_influence: training data cannot be recovered, so the ",
           "model cannot be refitted on a sample; fit it with a 'data =' ",
           "argument or pass max_rows = NULL.", call. = FALSE)
    model <- stats::update(model, data = d_s)
    tr    <- .glm_training_data(model, "plot_glm_influence")
  }

  cd  <- as.numeric(stats::cooks.distance(model))
  hv  <- as.numeric(stats::hatvalues(model))
  rs  <- as.numeric(stats::rstandard(model, type = "deviance"))
  y   <- as.numeric(tr$mf[[1]])
  mu  <- as.numeric(stats::fitted(model))
  rn  <- rownames(tr$mf)

  ok <- is.finite(cd) & is.finite(hv) & is.finite(rs)
  if (!any(ok))
    stop("plot_glm_influence: no usable influence measures; the model may ",
         "be rank deficient.", call. = FALSE)
  n <- sum(ok)

  if (is.null(data_cols)) data_cols <- .model_base_vars(model, tr$data)
  data_cols <- intersect(data_cols, names(tr$data))

  df <- data.frame(Row = rn, CooksD = cd, Leverage = hv, StdResid = rs,
                   Actual = y, Fitted = mu, stringsAsFactors = FALSE)[ok, ,
                                                             drop = FALSE]
  if (length(data_cols))
    df <- cbind(df, tr$data[ok, data_cols, drop = FALSE])
  rownames(df) <- NULL

  ord <- order(df$CooksD, decreasing = TRUE)
  top <- df[utils::head(ord, min(n_label, n)), , drop = FALSE]
  rest <- df[-utils::head(ord, min(n_label, n)), , drop = FALSE]
  if (nrow(rest) > n_max) rest <- rest[sort(sample(nrow(rest), n_max)), ,
                                       drop = FALSE]

  lbl <- function(d) {
    extra <- if (length(data_cols))
      vapply(seq_len(nrow(d)), function(i)
        paste0("<br>", paste(data_cols, unlist(lapply(data_cols,
               function(cc) format(d[[cc]][i]))), sep = ": ",
               collapse = "<br>")), character(1)) else rep("", nrow(d))
    paste0("<b>Row ", d$Row, "</b>",
           "<br>Cook's D: ", formatC(d$CooksD, format = "g", digits = 3),
           "<br>Leverage: ", formatC(d$Leverage, format = "g", digits = 3),
           "<br>Std. residual: ", formatC(d$StdResid, format = "f", digits = 2),
           "<br>Actual: ", formatC(d$Actual, format = "g", digits = 4),
           " \u00b7 Fitted: ", formatC(d$Fitted, format = "g", digits = 4),
           extra)
  }
  # The on-plot label has to be short: `text` is what plotly draws next to
  # a marker in "markers+text" mode, so putting the tooltip there paints
  # the whole thing across the panel. The detail goes in `hovertext`.
  tag <- function(d) paste0("Row ", d$Row)
  # Area, not radius, follows Cook's D, so a point that is twice as
  # influential does not look four times as big
  size_of <- function(x, mx) 5 + 20 * sqrt(pmax(x, 0) / mx)
  mx <- max(df$CooksD, na.rm = TRUE)
  if (!is.finite(mx) || mx <= 0) mx <- 1

  p <- plot_ly() %>%
    add_trace(x = rest$Leverage, y = rest$StdResid,
              type = "scatter", mode = "markers", name = "Rows",
              marker = list(color = ta_lightblue, opacity = 0.55,
                            size = size_of(rest$CooksD, mx),
                            line = list(width = 0)),
              hovertext = lbl(rest), hoverinfo = "text") %>%
    add_trace(x = top$Leverage, y = top$StdResid,
              type = "scatter", mode = "markers+text",
              name = paste0("Top ", nrow(top)),
              marker = list(color = ta_gold,
                            size = size_of(top$CooksD, mx),
                            line = list(color = ta_navy, width = 1)),
              text = tag(top), textposition = "top center",
              textfont = list(size = 9, color = ta_navy),
              hovertext = lbl(top), hoverinfo = "text")

  # The classic high-leverage rule, 2p/n: it still means something as a
  # reference for leverage even where Cook's own thresholds do not
  p_coef <- sum(!is.na(stats::coef(model)))
  hcut   <- 2 * p_coef / n

  if (is.null(title))
    title <- paste0("Influence \u2013 leverage vs residual, sized by Cook's D",
                    " (", format(n, big.mark = ","), " rows)")

  out <- p %>%
    layout(
      title = list(text = title, font = list(color = ta_navy, size = 14)),
      xaxis = list(title = "Leverage", gridcolor = "#D0D8E0",
                   zeroline = FALSE),
      yaxis = list(title = "Standardised deviance residual",
                   gridcolor = "#D0D8E0", zeroline = FALSE,
                   range = y_range, autorange = is.null(y_range)),
      shapes = list(
        list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
             line = list(color = ta_muted, width = 1, dash = "dot")),
        list(type = "line", xref = "x", yref = "paper",
             x0 = hcut, x1 = hcut, y0 = 0, y1 = 1,
             line = list(color = ta_muted, width = 1, dash = "dash"))),
      annotations = list(list(x = hcut, y = 1, xref = "x", yref = "paper",
                              text = "2p/n", showarrow = FALSE,
                              xanchor = "left", yanchor = "bottom",
                              font = list(color = ta_muted, size = 10))),
      legend = list(orientation = "h", y = -0.2),
      hoverlabel = list(bgcolor = "white", font = list(size = 12)),
      plot_bgcolor = "white", paper_bgcolor = "white",
      margin = list(b = 70, r = 40)) %>%
    config(displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d"),
           toImageButtonOptions = list(format = "png", filename = "influence"))

  attr(out, "influential") <- top
  out
}
