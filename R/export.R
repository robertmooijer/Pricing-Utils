#' Export a rating table to a formatted Excel workbook
#'
#' Writes the [make_rating_table()] output to a formatted `.xlsx` file:
#' an "Overview" sheet with the intercepts (per exposure unit), the base
#' value per variable and a timestamp; one sheet per main-effect variable
#' (base row highlighted, thin rows greyed out); and one sheet per
#' interaction with the long table plus a Level x Group matrix of the
#' premium factor. Requires the 'openxlsx' package.
#'
#' @param rating_tbl Output of [make_rating_table()].
#' @param file Output path of the `.xlsx` file.
#' @param overwrite Overwrite an existing file (default `TRUE`).
#' @param digits Number of decimals for the factor columns (default 4).
#'
#' @return Invisibly, the normalised path of the written file.
#' @export
export_rating_table <- function(rating_tbl, file = "rating_table.xlsx",
                                overwrite = TRUE, digits = 4) {

  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("export_rating_table: package 'openxlsx' is required ",
         "(install.packages(\"openxlsx\")).", call. = FALSE)
  .check_cols(rating_tbl, c("Variable", "Level", "Group", "IsBase"),
              "export_rating_table")
  rating_tbl <- .as_df(rating_tbl)

  st_header <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = ta_navy,
                                     textDecoration = "bold")
  st_base   <- openxlsx::createStyle(fgFill = ta_lightblue,
                                     textDecoration = "bold")
  st_thin   <- openxlsx::createStyle(fontColour = "#999999",
                                     textDecoration = "italic")
  st_fac    <- openxlsx::createStyle(
    numFmt = if (digits > 0) paste0("0.", strrep("0", digits)) else "0")
  st_int    <- openxlsx::createStyle(numFmt = "#,##0")

  wb         <- openxlsx::createWorkbook()
  used_names <- character(0)
  # Excel treats sheet names case-insensitively, so REGIO and Regio would
  # otherwise both be accepted here and produce a corrupt workbook
  sheet_name <- function(nm) {
    nm   <- substr(gsub("[^A-Za-z0-9 _.-]", "_", nm), 1, 31)
    base <- nm; i <- 1
    while (tolower(nm) %in% tolower(used_names)) {
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
    fac_cols <- grep("^(Factor|Uplift)", names(d))
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

  # Overview sheet -------------------------------------------------------------
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

  # Main-effect sheets ----------------------------------------------------------
  keep_main <- intersect(c("Level", "IsBase", "Exposure", "ClaimCount",
                           "IsThin", "Factor_Frequency", "Factor_Severity",
                           "Factor_Premium"),
                         names(rating_tbl))
  for (v in unique(rating_tbl$Variable[is.na(rating_tbl$Group)])) {
    d <- rating_tbl[rating_tbl$Variable == v & is.na(rating_tbl$Group),
                    keep_main, drop = FALSE]
    write_sheet(v, d)
  }

  # Interaction sheets: long table + premium-factor matrix ----------------------
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
