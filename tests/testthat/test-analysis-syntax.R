context("analysis syntax")

fixtureModule <- testthat::test_path("fixtures", "minimalModule")

test_that("readAnalysisSyntaxFromQml gives the wrapper call of the defaults", {
  syntax <- jaspSyntax::readAnalysisSyntaxFromQml(fixtureModule, "MinimalAnalysis")

  # The R function of the analysis and the version of its module (Desktop's Version::asString() drops trailing zeros)
  expect_identical(
    syntax,
    "jaspSyntaxTestModule::MinimalAnalysis(\n          data = NULL,\n          version = \"0.1\")"
  )
})

test_that("readAnalysisSyntaxFromQml gives only the options that differ from the defaults", {
  syntax <- jaspSyntax::readAnalysisSyntaxFromQml(
    fixtureModule,
    "MinimalAnalysis",
    options = list(flag = FALSE, threshold = 1.5, choice = "one")
  )

  # threshold is its default (1.5), so it is left out
  expect_identical(
    syntax,
    "jaspSyntaxTestModule::MinimalAnalysis(\n          data = NULL,\n          version = \"0.1\",\n          choice = \"one\",\n          flag = FALSE)"
  )
})

test_that("the syntax gives the same options back when it is parsed", {
  options <- list(flag = FALSE, threshold = 2.5, choice = "one")
  syntax  <- jaspSyntax::readAnalysisSyntaxFromQml(fixtureModule, "MinimalAnalysis", options = options)

  call <- str2lang(syntax)
  expect_identical(deparse(call[[1L]]), "jaspSyntaxTestModule::MinimalAnalysis")

  arguments <- as.list(call)[-1L]
  expect_null(arguments$data)
  expect_identical(arguments$version, "0.1")

  fromSyntax <- arguments[setdiff(names(arguments), c("data", "version"))]
  expect_identical(
    jaspSyntax::readAnalysisOptionsFromQml(fixtureModule, "MinimalAnalysis", options = fromSyntax, includeMeta = FALSE),
    jaspSyntax::readAnalysisOptionsFromQml(fixtureModule, "MinimalAnalysis", options = options, includeMeta = FALSE)
  )
})

test_that("analysisSyntax names the variables of the loaded data", {
  jaspSyntax::cleanUp()
  on.exit(jaspSyntax::cleanUp(), add = TRUE)
  jaspSyntax::loadDataSet(data.frame(x = c(1.1, 2.2, 3.3), group = factor(c("a", "b", "a"))))

  resolved <- jaspSyntax::resolveAnalysisQml(fixtureModule, "VariableAnalysis")
  syntax <- jaspSyntax::analysisSyntax(
    resolved$qmlFile,
    options = list(variables = c("x", "group")),
    moduleName = resolved$moduleName,
    analysisName = resolved$analysisName,
    version = resolved$version
  )

  # plain column names, not the encoded ones the analysis gets
  expect_match(syntax, "variables = ~ x + group", fixed = TRUE)
  expect_false(grepl("JASPColumn_", syntax, fixed = TRUE))
})

test_that("analysisSyntax can run in a separate R process", {
  syntax <- jaspSyntax::readAnalysisSyntaxFromQml(
    fixtureModule,
    "MinimalAnalysis",
    options = list(flag = FALSE),
    isolated = TRUE
  )

  expect_match(syntax, "flag = FALSE", fixed = TRUE)
})

test_that("analysisSyntax validates its arguments", {
  qmlFile <- jaspSyntax::resolveAnalysisQml(fixtureModule, "MinimalAnalysis")$qmlFile

  expect_error(jaspSyntax::analysisSyntax(qmlFile, isolated = NA), "isolated")
  expect_error(jaspSyntax::analysisSyntax(qmlFile, preloadData = "yes"), "preloadData")
  expect_error(jaspSyntax::analysisSyntax(file.path(tempdir(), "missing.qml")))
})
