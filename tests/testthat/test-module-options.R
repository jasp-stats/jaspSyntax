context("module options")

fixtureModule <- testthat::test_path("fixtures", "minimalModule")

test_that("readModuleDescription returns module metadata", {
  desc <- jaspSyntax::readModuleDescription(fixtureModule)

  expect_equal(desc$name, "jaspSyntaxTestModule")
  expect_equal(desc$title, "Syntax Test Module")
  expect_equal(desc$version, "0.1.0")
  expect_length(desc$analyses, 3)
  expect_equal(names(desc$analyses), c("DefaultAnalysis", "MinimalAnalysis", "VariableAnalysis"))
  expect_equal(desc$analyses$DefaultAnalysis$qml, "DefaultAnalysis.qml")
  expect_true(desc$analyses$DefaultAnalysis$preloadData)
  expect_equal(desc$analyses$MinimalAnalysis$qml, "MinimalAnalysis.qml")
  expect_false(desc$analyses$MinimalAnalysis$preloadData)
})

test_that("readModuleDescription handles one-line analysis entries", {
  modulePath <- tempfile("jaspSyntaxInlineModule_")
  dir.create(file.path(modulePath, "inst"), recursive = TRUE)
  on.exit(unlink(modulePath, recursive = TRUE), add = TRUE)
  writeLines(
    c(
      "Package: jaspSyntaxInlineModule",
      "Type: Package",
      "Title: Inline Module",
      "Version: 0.1.0",
      "Description: Inline analysis fixture.",
      "License: GPL (>= 2)"
    ),
    file.path(modulePath, "DESCRIPTION")
  )
  writeLines(
    c(
      "import QtQuick",
      "import JASP.Module",
      "",
      "Description {",
      "  title: qsTr(\"Inline Module\")",
      "  preloadData: false",
      "  hasWrappers: true",
      "  // Analysis { func: \"CommentedOut\" }",
      "  Analysis { title: qsTr(\"ANOVA\"); func: \"Anova\" }",
      "  Analysis { title: qsTr(\"Custom\"); func: \"CustomAnalysis\"; qml: \"CustomForm.qml\"; preloadData: true; hasWrapper: false }",
      "}"
    ),
    file.path(modulePath, "inst", "Description.qml")
  )

  desc <- jaspSyntax::readModuleDescription(modulePath)

  expect_equal(names(desc$analyses), c("Anova", "CustomAnalysis"))
  expect_equal(desc$analyses$Anova$title, "ANOVA")
  expect_equal(desc$analyses$Anova$qml, "Anova.qml")
  expect_false(desc$analyses$Anova$preloadData)
  expect_true(desc$analyses$Anova$hasWrapper)
  expect_equal(desc$analyses$CustomAnalysis$qml, "CustomForm.qml")
  expect_true(desc$analyses$CustomAnalysis$preloadData)
  expect_false(desc$analyses$CustomAnalysis$hasWrapper)
})

test_that("parseModuleDescription accepts Description.qml paths", {
  descPath <- testthat::test_path("fixtures", "minimalModule", "inst", "Description.qml")
  desc <- jaspSyntax::parseModuleDescription(descPath)

  expect_equal(desc$name, "jaspSyntaxTestModule")
  expect_equal(desc$analyses$MinimalAnalysis$name, "MinimalAnalysis")
})

test_that("resolveAnalysisQml resolves qml overrides and preload flags", {
  resolved <- jaspSyntax::resolveAnalysisQml(fixtureModule, "MinimalAnalysis")

  expect_equal(resolved$moduleName, "jaspSyntaxTestModule")
  expect_equal(resolved$qmlFileName, "MinimalAnalysis.qml")
  expect_true(file.exists(resolved$qmlFile))
  expect_false(resolved$preloadData)
})

test_that("readDefaultAnalysisOptions returns QML defaults", {
  opts <- jaspSyntax::readDefaultAnalysisOptions(fixtureModule, "MinimalAnalysis")

  expect_true(opts$flag)
  expect_equal(opts$threshold, 1.5)
  expect_equal(opts$choice, "two")
  expect_equal(opts$plotWidth, 480)
  expect_equal(opts$plotHeight, 320)
  expect_equal(attr(opts, "analysisName"), "MinimalAnalysis")
  expect_equal(attr(opts, "moduleName"), "jaspSyntaxTestModule")
  expect_false(attr(opts, "preloadData"))
})

test_that("readDefaultAnalysisOptions can omit metadata explicitly", {
  opts <- jaspSyntax::readDefaultAnalysisOptions(
    fixtureModule,
    "MinimalAnalysis",
    includeMeta = FALSE
  )

  expect_false(".meta" %in% names(opts))
  expect_false(any(grepl("\\.types$", names(opts))))
  expect_true(opts$flag)
})

test_that("readAnalysisOptionsFromQml applies supplied options", {
  opts <- jaspSyntax::readAnalysisOptionsFromQml(
    fixtureModule,
    "MinimalAnalysis",
    options = list(flag = FALSE, threshold = 2.5, choice = "one")
  )

  expect_false(opts$flag)
  expect_equal(opts$threshold, 2.5)
  expect_equal(opts$choice, "one")
})

test_that("readAnalysisOptionsFromQml returns Desktop runtime-encoded variable options", {
  jaspSyntax::cleanUp()
  on.exit(jaspSyntax::cleanUp(), add = TRUE)
  jaspSyntax::loadDataSet(data.frame(
    x = c(1.1, 2.2, 3.3),
    group = factor(c("a", "b", "a")),
    rating = factor(c("10", "20", "10"), levels = c("10", "20")),
    check.names = FALSE
  ))

  opts <- jaspSyntax::readAnalysisOptionsFromQml(
    fixtureModule,
    "VariableAnalysis",
    options = list(variables = "x"),
    includeMeta = FALSE
  )

  expect_true("variables.types" %in% names(opts))
  expect_equal(opts$`variables.types`, list("scale"))
  expect_match(opts$variables[[1]], "^JaspColumn_.*_Encoded$")

  loadedDataset <- jaspSyntax::readLoadedDataset(decode = FALSE)
  expect_true(any(vapply(
    loadedDataset,
    identical,
    logical(1L),
    c("a", "b", "a")
  )))
  expect_true(any(vapply(
    loadedDataset,
    identical,
    logical(1L),
    c("10", "20", "10")
  )))
})

test_that("fresh parsing resets cached QML state", {
  overridden <- jaspSyntax::readAnalysisOptionsFromQml(
    fixtureModule,
    "MinimalAnalysis",
    options = list(flag = FALSE, threshold = 9.5, choice = "one")
  )
  expect_false(overridden$flag)

  defaults <- jaspSyntax::readDefaultAnalysisOptions(fixtureModule, "MinimalAnalysis")
  expect_true(defaults$flag)
  expect_equal(defaults$threshold, 1.5)
  expect_equal(defaults$choice, "two")
})

test_that("parseQmlOptions supports raw JSON output", {
  resolved <- jaspSyntax::resolveAnalysisQml(fixtureModule, "MinimalAnalysis")
  json <- jaspSyntax::parseQmlOptions(
    resolved$qmlFile,
    moduleName = resolved$moduleName,
    analysisName = resolved$analysisName,
    version = resolved$version,
    preloadData = resolved$preloadData,
    output = "json"
  )

  expect_true(jsonlite::validate(json))
})

test_that("parseQmlOptions requires JSON object options", {
  resolved <- jaspSyntax::resolveAnalysisQml(fixtureModule, "MinimalAnalysis")

  expect_error(
    jaspSyntax::parseQmlOptions(
      resolved$qmlFile,
      options = "[]",
      moduleName = resolved$moduleName,
      analysisName = resolved$analysisName,
      version = resolved$version,
      preloadData = resolved$preloadData
    ),
    "JSON object"
  )
})

test_that("readAnalysisOptionsFromQml validates analysis names", {
  expect_error(
    jaspSyntax::readDefaultAnalysisOptions(fixtureModule, "MissingAnalysis"),
    "Could not locate analysis"
  )
})
