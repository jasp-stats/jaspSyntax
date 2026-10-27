context("generated wrapper: per-slice selection defaults + standalone run")

localWrapperBridge <- function() {
  tryCatch(
    {
      jaspSyntax::clearNativeState()
      jaspSyntax::setParameter("verbose", "none")
      invisible(TRUE)
    },
    error = function(e) {
      testthat::skip(paste0("Native SyntaxInterface bridge unavailable: ", conditionMessage(e)))
    }
  )
}

# The zoo module checkout lives next to jaspSyntax; its aware analysis
# `multiDataSetFunc` (two per-list selection options dataSetA/dataSetB) is the fixture.
zooModulePath <- function() {
  path <- testthat::test_path("..", "..", "..", "jaspTestModule")
  if (dir.exists(path)) path else ""
}

# A minimal runnable copy of the zoo module source: generateAnalysisWrapper writes into
# <module>/R/, and the bridge loads the analysis' QML from <module>/inst/qml - the copy keeps
# the real checkout untouched and the module DIR NAME must stay jaspTestModule (the bridge
# uses it as the module name in the wrapper). The caller owns the cleanup (unlink recursive).
tempZooModuleCopy <- function(zoo) {
  copyDir <- function(from, to) {
    dir.create(to, recursive = TRUE, showWarnings = FALSE)
    for (entry in list.files(from, full.names = TRUE, all.files = FALSE))
      if (dir.exists(entry)) copyDir(entry, file.path(to, basename(entry)))
      else file.copy(entry, to, overwrite = TRUE)
  }
  tmp <- tempfile("jaspWrapperModule_")
  copyDir(zoo, file.path(tmp, "jaspTestModule"))
  dir.create(file.path(tmp, "jaspTestModule", "R"), recursive = TRUE, showWarnings = FALSE)
  # the checkout carries generated wrappers the bridge must not see twice
  unlink(file.path(tmp, "jaspTestModule", "R", "*.Wrapper.R"))
  file.path(tmp, "jaspTestModule")
}

wrapperText <- function(zoo, analysisName) {
  module <- tempZooModuleCopy(zoo)
  on.exit(unlink(dirname(module), recursive = TRUE), add = TRUE)
  jaspSyntax::generateAnalysisWrapper(module, analysisName)
  wrapperFile <- file.path(module, "R", paste0(analysisName, "Wrapper.R"))
  testthat::expect_true(file.exists(wrapperFile), "generateAnalysisWrapper wrote the wrapper file")
  paste(readLines(wrapperFile, warn = FALSE), collapse = "\n")
}

test_that("the aware wrapper carries per-slice selection defaults, no datasets[[1]] pin", {
  localWrapperBridge()
  zoo <- zooModulePath()
  skip_if(zoo == "", "jaspTestModule checkout not available next to jaspSyntax")

  text <- wrapperText(zoo, "multiDataSetFunc")

  # the aware template shape: a datasets argument, no data argument
  expect_match(text, "datasets = NULL,", fixed = TRUE)
  expect_false(grepl("data = NULL,", text, fixed = TRUE))
  expect_match(text, 'options[["datasets"]] <- NULL', fixed = TRUE)

  # the positional pin is gone ...
  expect_false(grepl("data <- if (!is.null(datasets)", text, fixed = TRUE))
  expect_false(grepl("datasets[[1]]", text, fixed = TRUE))   # ... and so is its un-suffixed spelling

  # ... replaced by the selection-title resolution (the zoo form's two selection options,
  # in registration order) with the loud title-keyed contract:
  expect_match(text, 'selectionOptions <- c("dataSetA", "dataSetB")', fixed = TRUE)
  expect_match(text, "for (selectionOption in selectionOptions)", fixed = TRUE)
  expect_match(text, "must be a named list keyed by dataset title", fixed = TRUE)

  # the contract is stated on the wrapper itself
  expect_match(text, "#' @param datasets", fixed = TRUE)
  expect_match(text, "jaspSyntax::loadDataSets", fixed = TRUE)
})

test_that("the non-aware wrapper template is untouched", {
  localWrapperBridge()
  zoo <- zooModulePath()
  skip_if(zoo == "", "jaspTestModule checkout not available next to jaspSyntax")

  text <- wrapperText(zoo, "testOptionNames")   # a plain single-dataset analysis

  expect_match(text, "data = NULL,", fixed = TRUE)
  expect_false(grepl("datasets = NULL,", text, fixed = TRUE))
  expect_match(text, "jaspBase::storeDataSet(data)", fixed = TRUE)
  expect_false(grepl("selectionOptions <- c(", text, fixed = TRUE))
})

test_that("the generated wrapper runs standalone through the bridge (engine-parity proof)", {
  localWrapperBridge()
  zoo <- zooModulePath()
  skip_if(zoo == "", "jaspTestModule checkout not available next to jaspSyntax")
  skip_if_not_installed("jaspBase", "0.20.0")

  module <- tempZooModuleCopy(zoo)
  on.exit(unlink(dirname(module), recursive = TRUE), add = TRUE)
  jaspSyntax::generateAnalysisWrapper(module, "multiDataSetFunc")
  wrapperFile <- file.path(module, "R", "multiDataSetFuncWrapper.R")

  # The wrapper is meant to be sourced and called by a module user in plain R: the datasets
  # argument arrives as a named list keyed by DATASET TITLE, and every entry must be loadable
  # standalone - runWrappedAnalysis stores them via jaspBase::storeDataSets ->
  # jaspSyntax::loadDataSets (each under its own name), the bridge parses the options through
  # the analysis' own QML form, queues both slices and runJaspResults reads them (keyed by
  # filter id) - the exact preparation a desktop run gets. The installed jaspTestModule in the
  # lib path provides formals() + the analysis body; the qml under find.package() is parsed.
  wrapperEnv <- new.env(parent = globalenv())
  source(wrapperFile, local = wrapperEnv)
  testthat::expect_true(is.function(wrapperEnv$multiDataSetFunc))

  res <- suppressWarnings(wrapperEnv$multiDataSetFunc(
    datasets   = list(Alpha = iris, Beta = iris),
    dataSetA   = "Alpha",
    dataSetB   = "Beta",
    dependentA = "Sepal.Length",
    dependentB = "Sepal.Width",
    showMeta   = TRUE
  ))
  testthat::expect_s3_class(res, "jaspResultsR")

  # Decode the results payload the way jaspTools does (jaspSyntax::decodeJsonText) and assert
  # the run actually routed BOTH datasets: the zoo's selected-variables table carries both
  # columns, each attributed to its own dataset, the metaDump prints the PROVENANCE_AUDIT
  # marker (no .filterIds member, no dataSetId/filterId keys in .meta reached R), and
  # no raw encoded column token survives the decode.
  json <- jaspSyntax::decodeJsonText(res$.__enclos_env__$private$getResults())
  results <- jsonlite::fromJSON(json, simplifyVector = TRUE)

  flat <- paste(json, collapse = " ")
  expect_match(flat, "PROVENANCE_AUDIT filterIdsSeen=false metaProvenanceKeys=0", fixed = TRUE)
  expect_false(grepl("JASPColumn_", flat), "no raw encoded column tokens in the decoded results")

  selected <- results$results$selectedVariables$data
  expect_setequal(as.character(selected$variable), c("Sepal.Length", "Sepal.Width"))
  expect_setequal(as.character(selected$dataSetName), c("Alpha", "Beta"))
  expect_match(as.character(selected$values[selected$variable == "Sepal.Length"]), "5\\.8",
               label = "the Alpha slice (iris Sepal.Length) was actually read")
  expect_match(as.character(selected$values[selected$variable == "Sepal.Width"]), "3\\.0",
               label = "the Beta slice (iris Sepal.Width) was actually read")
})
