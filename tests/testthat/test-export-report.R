test_that("export_rating_table writes a structured workbook", {
  skip_if_not_installed("openxlsx")
  f_x <- file.path(tempdir(), "rating.xlsx")
  export_rating_table(tbl, f_x)
  expect_true(file.exists(f_x))
  sn <- openxlsx::getSheetNames(f_x)
  expect_true(all(c("Overview", "REGIO", "LEEFTIJD", "BOEKJAAR") %in% sn))

  f_x2 <- file.path(tempdir(), "rating_int.xlsx")
  export_rating_table(tbl2, f_x2)
  expect_true(any(grepl("LEEFTIJD.REGIO", openxlsx::getSheetNames(f_x2))))
})

test_that("pricing_report writes a complete HTML report", {
  skip_if_not_installed("htmltools")
  f_h <- file.path(tempdir(), "report.html")
  suppressMessages(
    pricing_report(m_freq, m_sev, dat, file = f_h, grid_res = 20)
  )
  expect_true(file.exists(f_h))
  expect_gt(file.info(f_h)$size, 50000)
  html <- readChar(f_h, file.info(f_h)$size, useBytes = TRUE)
  expect_true(grepl("Model diagnostics", html))
  expect_true(grepl("var-REGIO", html))
  expect_true(grepl("Rating factors", html))
  expect_true(grepl("htmlwidget", html))
})
