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

test_that("the Tariff sheet rebuilds a premium by lookup", {
  skip_if_not_installed("openxlsx")
  f <- file.path(tempdir(), "tariff.xlsx")
  export_rating_table(tbl, f)
  expect_true("Tariff" %in% openxlsx::getSheetNames(f))

  # row 1 holds the how-to note, so the header starts on row 2
  tf <- openxlsx::read.xlsx(f, sheet = "Tariff", startRow = 2)
  expect_true(all(c("Key", "Variable", "Level", "Factor_Premium") %in%
                    names(tf)))
  expect_equal(nrow(tf), nrow(tbl))
  # every main-effect key is unique, which is what VLOOKUP needs
  main <- tf[is.na(tf$Group), ]
  expect_equal(anyDuplicated(main$Key), 0)

  # and the factors multiply back to the model, exactly as on the
  # per-variable sheets
  look <- function(v, lvl) tf$Factor_Frequency[
    tf$Key == paste(v, lvl, sep = "|")][1]
  s   <- tbl[tbl$Variable == "LEEFTIJD" & is.na(tbl$Group), ]
  age <- s$Level[10]
  expect_equal(look("LEEFTIJD", age), s$Factor_Frequency[10])
})

test_that("continuous levels are written as numbers, not text", {
  skip_if_not_installed("openxlsx")
  # Level is character in the table because a categorical level is text,
  # but for a continuous variable Excel would then flag every cell as
  # "number stored as text"
  f <- file.path(tempdir(), "levels.xlsx")
  export_rating_table(tbl, f)

  block <- function(sheet, var) {
    nr <- sum(tbl$Variable == var & is.na(tbl$Group))
    openxlsx::read.xlsx(f, sheet = sheet, rows = seq_len(nr + 1))
  }
  expect_type(block("LEEFTIJD", "LEEFTIJD")$Level, "double")
  expect_type(block("REGIO", "REGIO")$Level, "character")
  # a factor() term keeps its levels as text, which is correct
  expect_type(block("BOEKJAAR", "BOEKJAAR")$Level, "character")

  # interaction sheet: the X axis is the continuous one here
  f2 <- file.path(tempdir(), "levels_int.xlsx")
  export_rating_table(tbl2, f2)
  iv <- unique(tbl2$Variable[!is.na(tbl2$Group)])[1]
  sn <- grep("LEEFTIJD.REGIO", openxlsx::getSheetNames(f2), value = TRUE)[1]
  nr <- sum(tbl2$Variable == iv & !is.na(tbl2$Group))
  xi <- openxlsx::read.xlsx(f2, sheet = sn, rows = seq_len(nr + 1))
  expect_type(xi$Level, "double")
  expect_type(xi$Group, "character")
})

test_that("the matrix under an interaction keeps numeric levels too", {
  skip_if_not_installed("openxlsx")
  # the long table was taken out of "number stored as text" but the pivot
  # below it still came from rownames(), which are always character
  f3 <- file.path(tempdir(), "pivot.xlsx")
  suppressWarnings(export_rating_table(tbl2, f3))
  iv <- unique(tbl2$Variable[!is.na(tbl2$Group)])[1]
  sn <- gsub("[^A-Za-z0-9 _.-]", "_", iv)
  nr <- sum(tbl2$Variable == iv & !is.na(tbl2$Group))
  piv <- openxlsx::read.xlsx(f3, sheet = sn, startRow = nr + 4)
  expect_type(piv$Level, "double")
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
