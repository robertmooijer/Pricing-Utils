#' Aggregate raw policy rows to frequency/severity per level
#'
#' Goes from raw rows to an aggregate per level of `col` (and optionally per
#' accounting year). This is the aggregation engine used internally by
#' [make_plot()]; call it directly when you want the summary table itself.
#'
#' NA values in the input columns and groups with non-positive exposure are
#' reported with a warning instead of being silently averaged away.
#'
#' @param d A data.frame or data.table with raw rows.
#' @param col Name of the grouping column (string).
#' @param by_year `TRUE` = also split by accounting year.
#' @param exposure_col Exposure column (default `"Exposure"`).
#' @param claims_col Claim count column (default `"AantalClaims"`).
#' @param loss_col Loss amount column (default `"SCHADELAST"`).
#' @param year_col Accounting-year column (default `"BOEKJAAR"`).
#'
#' @return A data.frame with standardised column names: `col`, `[Year,]`
#'   `Exposure`, `ClaimCount`, `Loss`, `Frequency` (`ClaimCount / Exposure`,
#'   `NA` when exposure is not positive) and `Severity`
#'   (`Loss / ClaimCount`, `NA` when there are no claims).
#' @export
#' @examples
#' d <- data.frame(REGIO = c("N", "N", "Z"), Exposure = c(1, 0.5, 1),
#'                 AantalClaims = c(0, 1, 2), SCHADELAST = c(0, 500, 3000))
#' agg_all(d, "REGIO", by_year = FALSE)
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

  # The output columns are fixed names, so grouping on one of them would
  # produce a data.frame with two columns of the same name rather than an
  # error, and every later rename or lookup would then pick the wrong one.
  clash <- intersect(c(col, if (by_year) year_col),
                     c("Exposure", "ClaimCount", "Loss", "Frequency",
                       "Severity", if (by_year) "Year"))
  if (length(clash))
    stop("agg_all: cannot group on '", paste(clash, collapse = ", "),
         "', which is also the name of a column agg_all creates. Rename it ",
         "first.", call. = FALSE)

  if (by_year && identical(col, year_col))
    stop("agg_all: 'col' and 'year_col' are both '", col,
         "'; pass by_year = FALSE to group on the year itself.",
         call. = FALSE)

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
