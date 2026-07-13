#' One-call HTML pricing report
#'
#' Bundles the whole analysis into a single self-browsable HTML report:
#' model diagnostics (fit table + binned residuals) and, per variable,
#' the one-way observed plot, actual vs expected, partial dependence and
#' the rating-factor plot, plus a section with the interaction plots.
#' Individual plot failures are shown as a note instead of aborting the
#' report.
#'
#' Built on `htmltools::save_html()` (no pandoc required); the plots are
#' regular interactive plotly widgets. Next to the `.html` file a
#' `<name>_files/` folder is written with the JavaScript dependencies;
#' keep the two together when sharing the report.
#'
#' @param model_freq,model_sev Fitted glm objects (at least one).
#' @param data Dataset with raw rows.
#' @param file Output path of the `.html` file.
#' @param title Report title.
#' @param variables Which variables get a section (default: all base
#'   variables of both models).
#' @param include Which blocks to render: any of `"diagnostics"`,
#'   `"oneway"`, `"ae"`, `"pdp"`, `"rating"` (default: all).
#' @param by_year Split the one-way plots by accounting year.
#' @param exposure_col,claims_col,loss_col,year_col Column names, see
#'   [agg_all()].
#' @param grid_res,base_level,trim Passed to [make_pdp()] /
#'   [make_rating_table()].
#'
#' @return Invisibly, the normalised path of the written file.
#' @export
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
        h$h3("Binned residuals \u2013 frequency"),
        safe(plot_glm_residuals(model_freq))),
      if (!is.null(model_sev)) htmltools::tagList(
        h$h3("Binned residuals \u2013 severity"),
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
        h$h3("Actual vs expected \u2013 frequency"),
        safe(plot_glm_predictor(model_freq, v))))
    if ("ae" %in% include && in_model_data(model_sev, v))
      parts <- c(parts, list(
        h$h3("Actual vs expected \u2013 severity"),
        safe(plot_glm_predictor(model_sev, v))))
    if ("pdp" %in% include && !is.null(model_freq) &&
        v %in% .rhs_vars(model_freq))
      parts <- c(parts, list(
        h$h3("Partial dependence \u2013 frequency"),
        safe(make_pdp(model_freq, data, v, metric = "Frequency",
                      grid_res = grid_res, exposure_col = exposure_col,
                      claims_col = claims_col, loss_col = loss_col))))
    if ("pdp" %in% include && !is.null(model_sev) &&
        v %in% .rhs_vars(model_sev))
      parts <- c(parts, list(
        h$h3("Partial dependence \u2013 severity"),
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
                           " \u2013 ", format(nrow(data), big.mark = ","),
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
