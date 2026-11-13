context("JASP file results")

writeResultsJaspFile <- function(path, analyses, manifest = NULL) {
  tempDir <- tempfile("jaspSyntax_test_results_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  entries <- "analyses.json"
  jsonlite::write_json(
    list(analyses = analyses),
    file.path(tempDir, "analyses.json"),
    auto_unbox = TRUE,
    digits = NA
  )
  if (!is.null(manifest)) {
    jsonlite::write_json(manifest, file.path(tempDir, "manifest.json"), auto_unbox = TRUE)
    entries <- c(entries, "manifest.json")
  }

  oldWd <- getwd()
  setwd(tempDir)
  on.exit(setwd(oldWd), add = TRUE)
  utils::zip(path, entries, flags = "-q")
  invisible(path)
}

test_that("readAnalysisResultsFromJaspFile returns one record per saved analysis", {
  jaspFile <- tempfile(fileext = ".jasp")
  table <- list(
    title = "Tabl\u00e9",
    schema = list(fields = list(list(name = "Variable", type = "string"), list(name = "mean", type = "number"))),
    data = list(list(Variable = "extr\u00e4", mean = 0.123456789012345))
  )
  writeResultsJaspFile(
    jaspFile,
    analyses = list(
      list(
        id = 3L,
        name = "FirstAnalysis",
        title = "First Analysis",
        status = "complete",
        dynamicModule = list(moduleName = "jaspFirst", moduleVersion = "1.0.0"),
        options = list(variables = "x"),
        results = list(title = "First Analysis", stats = table)
      ),
      list(
        id = 7L,
        name = "SecondAnalysis",
        title = "Second Analysis",
        status = "validationError",
        moduleName = "jaspSecond",
        version = "2.0.0",
        options = list()
      )
    ),
    manifest = list(jaspArchiveVersion = "5", jaspVersion = "0.96")
  )

  records <- jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile)

  expect_named(records, c("FirstAnalysis", "SecondAnalysis"))
  expect_equal(attr(records, "jaspVersion"), "0.96")
  expect_equal(attr(records, "jaspArchiveVersion"), "5")

  first <- records[["FirstAnalysis"]]
  expect_equal(first$id, 3L)
  expect_equal(first$title, "First Analysis")
  expect_equal(first$moduleName, "jaspFirst")
  expect_equal(first$moduleVersion, "1.0.0")
  expect_equal(first$status, "complete")
  expect_equal(first$results$stats$title, "Tabl\u00e9")
  expect_equal(first$results$stats$data[[1]]$Variable, "extr\u00e4")
  expect_identical(first$results$stats$data[[1]]$mean, 0.123456789012345)
  expect_null(first$options)

  second <- records[["SecondAnalysis"]]
  expect_equal(second$id, 7L)
  expect_equal(second$moduleName, "jaspSecond")
  expect_equal(second$moduleVersion, "2.0.0")
  expect_equal(second$status, "validationError")
  expect_null(second$results)
})

test_that("readAnalysisResultsFromJaspFile works without a manifest", {
  jaspFile <- tempfile(fileext = ".jasp")
  writeResultsJaspFile(jaspFile, list(list(id = 0L, name = "OnlyAnalysis", results = list(title = "x"))))

  records <- jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile)

  expect_named(records, "OnlyAnalysis")
  expect_null(attr(records, "jaspVersion"))
  expect_null(attr(records, "jaspArchiveVersion"))
  expect_equal(records$OnlyAnalysis$results$title, "x")
})

test_that("readAnalysisResultsFromJaspFile rejects files without analyses", {
  jaspFile <- tempfile(fileext = ".jasp")
  writeResultsJaspFile(jaspFile, list())

  expect_error(
    jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile),
    "No analyses found"
  )
  expect_error(
    jaspSyntax::readAnalysisResultsFromJaspFile(tempfile(fileext = ".jasp")),
    "File not found"
  )
})

test_that("readAnalysisResultsFromJaspFile reads a real Desktop .jasp file", {
  jaspFile <- testthat::test_path("fixtures", "jasp-files", "descriptives-sleep.jasp")
  testthat::skip_if_not(file.exists(jaspFile), "Desktop .jasp fixture missing")

  records <- jaspSyntax::readAnalysisResultsFromJaspFile(jaspFile)

  expect_named(records, "Descriptives")
  expect_equal(attr(records, "jaspVersion"), "0.96")

  record <- records$Descriptives
  expect_equal(record$id, 0L)
  expect_equal(record$moduleName, "jaspDescriptives")
  expect_equal(record$status, "complete")

  stats <- record$results$stats
  fields <- vapply(stats$schema$fields, function(field) field$name, character(1L))
  expect_equal(
    fields,
    c("Variable", "Level", "Valid", "Missing", "MeanArithmetic", "Std. Deviation", "Minimum", "Maximum")
  )
  expect_length(stats$data, 2L)
  expect_equal(stats$data[[1]]$Variable, "extra")
  expect_equal(stats$data[[1]]$MeanArithmetic, 0.75)
  expect_equal(stats$data[[2]]$`Std. Deviation`, 2.002248735796829)
  expect_equal(stats$data[[1]]$Maximum$value, 3.7)

  plot <- record$results$boxPlot$collection$boxPlot_extra
  expect_match(plot$data, "^resources/0/.*\\.png$")
})
