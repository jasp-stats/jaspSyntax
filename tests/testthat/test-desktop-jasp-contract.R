context("Desktop JASP file contract")

desktopJaspFixture <- function() {
  testthat::test_path("fixtures", "jasp-files", "descriptives-sleep.jasp")
}

readJaspJsonEntry <- function(jaspFile, entry) {
  tempDir <- tempfile("jaspSyntax_contract_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  utils::unzip(jaspFile, files = entry, exdir = tempDir)
  jsonlite::fromJSON(file.path(tempDir, entry), simplifyVector = FALSE)
}

optionValues <- function(x) {
  unlist(x, use.names = FALSE)
}

descriptivesModulePath <- function() {
  envPath <- Sys.getenv("JASP_DESCRIPTIVES_MODULE", unset = "")
  if (nzchar(envPath)) {
    return(envPath)
  }

  NA_character_
}

test_that("Desktop .jasp fixture contains saved Descriptives state", {
  jaspFile <- desktopJaspFixture()
  testthat::skip_if_not(file.exists(jaspFile), "Desktop .jasp fixture missing")

  entries <- utils::unzip(jaspFile, list = TRUE)$Name
  expect_true(all(c("manifest.json", "analyses.json", "internal.sqlite") %in% entries))

  manifest <- readJaspJsonEntry(jaspFile, "manifest.json")
  expect_equal(manifest$jaspArchiveVersion, "5")
  expect_equal(manifest$jaspVersion, "0.96")

  analysis <- readJaspJsonEntry(jaspFile, "analyses.json")$analyses[[1]]
  expect_equal(analysis$name, "Descriptives")
  expect_equal(analysis$title, "Descriptive Statistics")
  expect_equal(analysis$dynamicModule$moduleName, "jaspDescriptives")
  expect_equal(analysis$dynamicModule$moduleVersion, "0.95.5")
  expect_equal(optionValues(analysis$options$variables$value), "extra")
  expect_equal(optionValues(analysis$options$variables$types), "scale")
  expect_equal(optionValues(analysis$options$splitBy$value), "group")
  expect_equal(optionValues(analysis$options$splitBy$types), "nominal")
  expect_true(analysis$options$boxPlot)
})

test_that("readDatasetFromJaspFile reads a real Desktop .jasp dataset", {
  jaspFile <- desktopJaspFixture()
  testthat::skip_if_not(file.exists(jaspFile), "Desktop .jasp fixture missing")

  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  expect_s3_class(dataset, "data.frame")
  expect_equal(names(dataset), c("extra", "group", "ID"))
  expect_equal(dim(dataset), c(20L, 3L))
  expect_equal(dataset$extra[1:5], c(0.7, -1.6, -0.2, -1.2, -0.1))
  expect_equal(as.character(dataset$group[1]), "1")
  expect_equal(as.integer(dataset$ID[1:5]), 1:5)
})

test_that("readAnalysisOptionsFromJaspFile returns saved bound Desktop options", {
  jaspFile <- desktopJaspFixture()
  testthat::skip_if_not(file.exists(jaspFile), "Desktop .jasp fixture missing")

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(jaspFile, runtime = FALSE)

  expect_equal(names(records), "Descriptives")
  expect_equal(records$Descriptives$name, "Descriptives")
  expect_equal(records$Descriptives$title, "Descriptive Statistics")
  expect_equal(records$Descriptives$moduleName, "jaspDescriptives")
  expect_equal(records$Descriptives$moduleVersion, "0.95.5")
  expect_equal(optionValues(records$Descriptives$options$variables$value), "extra")
  expect_equal(optionValues(records$Descriptives$options$variables$types), "scale")
  expect_equal(optionValues(records$Descriptives$options$splitBy$value), "group")
  expect_equal(optionValues(records$Descriptives$options$splitBy$types), "nominal")
  expect_false("variables.types" %in% names(records$Descriptives$options))
})

test_that("readAnalysisOptionsFromJaspFile replays real Desktop options to runtime shape", {
  jaspFile <- desktopJaspFixture()
  testthat::skip_if_not(file.exists(jaspFile), "Desktop .jasp fixture missing")

  modulePath <- descriptivesModulePath()
  testthat::skip_if(is.na(modulePath), "Set JASP_DESCRIPTIVES_MODULE for runtime replay")
  testthat::skip_if_not(dir.exists(modulePath), "jaspDescriptives module path missing")
  modulePath <- c(jaspDescriptives = normalizePath(modulePath, winslash = "/", mustWork = TRUE))

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    modulePath = modulePath,
    runtime = TRUE,
    includeMeta = FALSE,
    isolated = TRUE
  )
  opts <- records$Descriptives$options

  expect_match(unlist(opts$variables, use.names = FALSE)[[1L]], "^JaspColumn_.*_Encoded$")
  expect_equal(optionValues(opts$`variables.types`), "scale")
  expect_match(unlist(opts$splitBy, use.names = FALSE)[[1L]], "^JaspColumn_.*_Encoded$")
  expect_equal(optionValues(opts$`splitBy.types`), "nominal")
  expect_true(opts$boxPlot)
  expect_false(".meta" %in% names(opts))
})

test_that("native and R bridge exports keep the expected consumer formals", {
  expect_named(formals(jaspSyntax::loadDataSetFromJaspFile), "jaspFilePath")
  expect_named(formals(jaspSyntax::analysisOptionsFromJaspFile), c("jaspFilePath", "analysisNr"))
  expect_named(
    formals(jaspSyntax::loadQmlAndParseOptions),
    c("moduleName", "analysisName", "qmlFile", "options", "version", "preloadData")
  )
  expect_named(formals(jaspSyntax::readDatasetFromJaspFile), c("jaspFilePath", "dataSetIndex"))
  expect_identical(formals(jaspSyntax::readDatasetFromJaspFile)$dataSetIndex, 1L)
  expect_named(
    formals(jaspSyntax::loadAnalysisDataset),
    c(
      "dataset", "modulePath", "analysisName", "options", "includeMeta",
      "includeTypeOptions", "decode", "normalize"
    )
  )
  expect_named(formals(jaspSyntax::readLoadedDataset), c("decode", "normalize"))
  expect_named(formals(jaspSyntax::readRequestedDataset), c("decode", "normalize"))
  expect_named(formals(jaspSyntax::readDatasetHeader), "decode")
  expect_named(formals(jaspSyntax::decodeColumnNames), c("columnNames", "strict"))
  expect_named(formals(jaspSyntax::decodeAnalysisResults), c("results", "requestedDataset", "columnMapping"))
  expect_named(formals(jaspSyntax::columnMapping), c("encodedColumnNames", "strict"))
  expect_named(
    formals(jaspSyntax::readAnalysisOptionsFromJaspFile),
    c("jaspFilePath", "modulePath", "runtime", "includeMeta", "includeTypeOptions", "isolated")
  )
  expect_false(formals(jaspSyntax::readAnalysisOptionsFromJaspFile)$runtime)
  expect_true(formals(jaspSyntax::readAnalysisOptionsFromJaspFile)$includeMeta)
  expect_true(formals(jaspSyntax::readAnalysisOptionsFromJaspFile)$includeTypeOptions)
  expect_true(formals(jaspSyntax::readAnalysisOptionsFromJaspFile)$isolated)
})
