context("dataset bridge helpers")

localGlobalBinding <- function(name, value) {
  hadValue <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  oldValue <- if (hadValue) get(name, envir = .GlobalEnv, inherits = FALSE) else NULL

  assign(name, value, envir = .GlobalEnv)

  function() {
    if (hadValue) {
      assign(name, oldValue, envir = .GlobalEnv)
    } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = name, envir = .GlobalEnv)
    }
  }
}

localGlobalAbsent <- function(name) {
  hadValue <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  oldValue <- if (hadValue) get(name, envir = .GlobalEnv, inherits = FALSE) else NULL

  if (hadValue) {
    rm(list = name, envir = .GlobalEnv)
  }

  function() {
    if (hadValue) {
      assign(name, oldValue, envir = .GlobalEnv)
    }
  }
}

localNamespaceBinding <- function(name, value, namespace) {
  oldValue <- get(name, envir = namespace, inherits = FALSE)
  wasLocked <- bindingIsLocked(name, namespace)

  if (wasLocked) {
    unlockBinding(name, namespace)
  }
  assign(name, value, envir = namespace)
  if (wasLocked) {
    lockBinding(name, namespace)
  }

  function() {
    if (bindingIsLocked(name, namespace)) {
      unlockBinding(name, namespace)
    }
    assign(name, oldValue, envir = namespace)
    if (wasLocked) {
      lockBinding(name, namespace)
    }
  }
}

test_that("decodeColumnNames delegates to the native bridge decoder", {
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(
        JaspColumn_1_Encoded = "raw score",
        JaspColumn_2_Encoded = "group"
      )[[columnName]]
    }
  )
  on.exit(restoreDecoder(), add = TRUE)

  expect_equal(
    jaspSyntax::decodeColumnNames(c("JaspColumn_1_Encoded", "JaspColumn_2_Encoded")),
    c("raw score", "group")
  )
  expect_equal(
    jaspSyntax::columnMapping(c("JaspColumn_1_Encoded", "JaspColumn_2_Encoded")),
    c(JaspColumn_1_Encoded = "raw score", JaspColumn_2_Encoded = "group")
  )
})

test_that("decodeColumnNames can fall back or fail when the decoder is unavailable", {
  restoreDecoder <- localGlobalAbsent(".decodeColNamesStrict")
  on.exit(restoreDecoder(), add = TRUE)

  expect_equal(
    jaspSyntax::decodeColumnNames(c("plain", "JaspColumn_1_Encoded")),
    c("plain", "JaspColumn_1_Encoded")
  )
  expect_error(
    jaspSyntax::decodeColumnNames("JaspColumn_1_Encoded", strict = TRUE),
    "did not expose `.decodeColNamesStrict`",
    fixed = TRUE
  )
  expect_error(
    jaspSyntax::columnMapping("JaspColumn_1_Encoded", strict = TRUE),
    "did not expose `.decodeColNamesStrict`",
    fixed = TRUE
  )
})

test_that("state readers fail loudly when decode is requested without decoder support", {
  restoreDecoder <- localGlobalAbsent(".decodeColNamesStrict")
  restoreLoaded <- localGlobalBinding(
    ".readFullDatasetToEnd",
    function() {
      data.frame(JaspColumn_1_Encoded = c(1, 2), check.names = FALSE)
    }
  )
  restoreRequested <- localGlobalBinding(
    ".readDataSetRequestedNative",
    function() {
      data.frame(JaspColumn_1_Encoded = c(1, 2), check.names = FALSE)
    }
  )
  restoreNames <- localNamespaceBinding(
    "getVariableNames",
    function() {
      list("JaspColumn_1_Encoded")
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreDecoder(), add = TRUE)
  on.exit(restoreLoaded(), add = TRUE)
  on.exit(restoreRequested(), add = TRUE)
  on.exit(restoreNames(), add = TRUE)

  expect_error(
    jaspSyntax::readLoadedDataset(decode = TRUE),
    "did not expose `.decodeColNamesStrict`",
    fixed = TRUE
  )
  expect_error(
    jaspSyntax::readRequestedDataset(decode = TRUE),
    "did not expose `.decodeColNamesStrict`",
    fixed = TRUE
  )
  expect_error(
    jaspSyntax::readDatasetHeader(decode = TRUE),
    "did not expose `.decodeColNamesStrict`",
    fixed = TRUE
  )

  expect_equal(names(jaspSyntax::readLoadedDataset(decode = FALSE)), "JaspColumn_1_Encoded")
  expect_equal(
    jaspSyntax::readDatasetHeader(decode = FALSE)$name,
    "JaspColumn_1_Encoded"
  )
})

test_that("readLoadedDataset reads, decodes, and normalizes bridge data", {
  restoreDataset <- localGlobalBinding(
    ".readFullDatasetToEnd",
    function() {
      data.frame(
        JaspColumn_1_Encoded = factor(c("1", "2")),
        JaspColumn_2_Encoded = factor(c("control", "treatment")),
        check.names = FALSE
      )
    }
  )
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(
        JaspColumn_1_Encoded = "id",
        JaspColumn_2_Encoded = "condition"
      )[[columnName]]
    }
  )
  on.exit(restoreDataset(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  dataset <- jaspSyntax::readLoadedDataset()

  expect_equal(names(dataset), c("id", "condition"))
  expect_identical(dataset$id, c("1", "2"))
  expect_identical(dataset$condition, c("control", "treatment"))

  rawDataset <- jaspSyntax::readLoadedDataset(decode = FALSE, normalize = FALSE)
  expect_equal(names(rawDataset), c("JaspColumn_1_Encoded", "JaspColumn_2_Encoded"))
  expect_s3_class(rawDataset$JaspColumn_1_Encoded, "factor")
})

test_that("factor normalization preserves numeric-looking category labels", {
  restoreDataset <- localGlobalBinding(
    ".readFullDatasetToEnd",
    function() {
      data.frame(
        response = factor(c("1", "2", "1")),
        check.names = FALSE
      )
    }
  )
  on.exit(restoreDataset(), add = TRUE)

  dataset <- jaspSyntax::readLoadedDataset(decode = FALSE)

  expect_identical(dataset$response, c("1", "2", "1"))
})

test_that("decodeAnalysisResults decodes native column names and factor value tokens", {
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(JaspColumn_1_Encoded = "group")[[columnName]]
    }
  )
  on.exit(restoreDecoder(), add = TRUE)

  requestedDataset <- data.frame(
    JaspColumn_1_Encoded = factor(c("control", "treatment")),
    check.names = FALSE
  )
  results <- list(
    results = list(
      table = list(
        data = list(
          list(
            JaspColumn_1_Encoded = "1",
            label = "JaspColumn_1_Encoded"
          )
        )
      )
    )
  )

  decoded <- jaspSyntax::decodeAnalysisResults(results, requestedDataset = requestedDataset)
  firstRow <- decoded$results$table$data[[1L]]

  expect_equal(names(firstRow), c("group", "label"))
  expect_equal(firstRow$group, "control")
  expect_equal(firstRow$label, "group")
})

test_that("decodeAnalysisResults can use captured column mapping without native decoder", {
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      stop("native decoder should not be called")
    }
  )
  on.exit(restoreDecoder(), add = TRUE)

  requestedDataset <- data.frame(
    JaspColumn_1_Encoded = factor(c("control", "treatment")),
    check.names = FALSE
  )
  results <- list(
    results = list(
      table = list(
        data = list(
          list(
            JaspColumn_1_Encoded = "2",
            Variable = "JaspColumn_1_Encoded"
          )
        )
      )
    )
  )

  decoded <- jaspSyntax::decodeAnalysisResults(
    results,
    requestedDataset = requestedDataset,
    columnMapping = c(JaspColumn_1_Encoded = "group")
  )
  firstRow <- decoded$results$table$data[[1L]]

  expect_equal(names(firstRow), c("group", "Variable"))
  expect_equal(firstRow$group, "treatment")
  expect_equal(firstRow$Variable, "group")
})

test_that("decodeAnalysisResults maps factor values with decoded requested datasets", {
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      stop("native decoder should not be called")
    }
  )
  on.exit(restoreDecoder(), add = TRUE)

  requestedDataset <- data.frame(
    group = factor(c("control", "treatment")),
    check.names = FALSE
  )
  results <- list(
    results = list(
      table = list(
        data = list(
          list(JaspColumn_1_Encoded = "1")
        )
      )
    )
  )

  decoded <- jaspSyntax::decodeAnalysisResults(
    results,
    requestedDataset = requestedDataset,
    columnMapping = c(JaspColumn_1_Encoded = "group")
  )
  firstRow <- decoded$results$table$data[[1L]]

  expect_equal(names(firstRow), "group")
  expect_equal(firstRow$group, "control")
})

test_that("readRequestedDataset exposes requested native dataset state", {
  restoreDataset <- localGlobalBinding(
    ".readDataSetRequestedNative",
    function() {
      data.frame(
        JaspColumn_1_Encoded = c(1.5, 2.5),
        check.names = FALSE
      )
    }
  )
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(JaspColumn_1_Encoded = "requested")[[columnName]]
    }
  )
  on.exit(restoreDataset(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  dataset <- jaspSyntax::readRequestedDataset()

  expect_equal(names(dataset), "requested")
  expect_equal(dataset$requested, c(1.5, 2.5))
})

test_that("readDatasetHeader decodes native header names", {
  restoreNames <- localNamespaceBinding(
    "getVariableNames",
    function() {
      list("JaspColumn_1_Encoded", "JaspColumn_2_Encoded")
    },
    asNamespace("jaspSyntax")
  )
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(
        JaspColumn_1_Encoded = "score",
        JaspColumn_2_Encoded = "group"
      )[[columnName]]
    }
  )
  on.exit(restoreNames(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  header <- jaspSyntax::readDatasetHeader()

  expect_equal(header$name, c("score", "group"))
  expect_equal(header$encodedName, c("JaspColumn_1_Encoded", "JaspColumn_2_Encoded"))
})

test_that("loadAnalysisDataset returns loaded and requested state from native helpers", {
  modulePath <- tempfile("jaspSyntaxDatasetModule_")
  dir.create(modulePath)

  loadedData <- NULL
  replayArgs <- NULL

  restoreClear <- localNamespaceBinding(
    "clearDatasetState",
    function() invisible(NULL),
    asNamespace("jaspSyntax")
  )
  restoreLoad <- localNamespaceBinding(
    "loadDataSet",
    function(data) {
      loadedData <<- data
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreReadQml <- localNamespaceBinding(
    "readAnalysisOptionsFromQml",
    function(modulePath, analysisName, options, fresh,
             includeMeta, includeTypeOptions) {
      replayArgs <<- list(
        modulePath = modulePath,
        analysisName = analysisName,
        options = options,
        fresh = fresh,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(variables = "JaspColumn_1_Encoded", `variables.types` = "scale")
    },
    asNamespace("jaspSyntax")
  )
  restoreLoaded <- localGlobalBinding(
    ".readFullDatasetToEnd",
    function() {
      data.frame(
        JaspColumn_1_Encoded = c(1, 2),
        JaspColumn_2_Encoded = c("a", "b"),
        check.names = FALSE
      )
    }
  )
  restoreRequested <- localGlobalBinding(
    ".readDataSetRequestedNative",
    function() {
      data.frame(
        JaspColumn_1_Encoded = factor(c("control", "treatment")),
        check.names = FALSE
      )
    }
  )
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) {
      c(
        JaspColumn_1_Encoded = "score",
        JaspColumn_2_Encoded = "group"
      )[[columnName]]
    }
  )
  on.exit(restoreClear(), add = TRUE)
  on.exit(restoreLoad(), add = TRUE)
  on.exit(restoreReadQml(), add = TRUE)
  on.exit(restoreLoaded(), add = TRUE)
  on.exit(restoreRequested(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  rawDataset <- data.frame(score = c(1, 2), group = c("a", "b"))
  savedOptions <- list(variables = list(value = "score", types = "scale"))

  state <- jaspSyntax::loadAnalysisDataset(
    rawDataset,
    modulePath = modulePath,
    analysisName = "ExampleAnalysis",
    options = savedOptions,
    includeMeta = FALSE
  )

  expect_equal(loadedData, rawDataset)
  expect_equal(replayArgs$modulePath, normalizePath(modulePath, winslash = "/", mustWork = FALSE))
  expect_equal(replayArgs$analysisName, "ExampleAnalysis")
  expect_equal(replayArgs$options, savedOptions)
  expect_true(replayArgs$fresh)
  expect_false(replayArgs$includeMeta)
  expect_true(replayArgs$includeTypeOptions)
  expect_equal(names(state$loadedDataset), c("score", "group"))
  expect_equal(names(state$requestedDataset), "score")
  expect_equal(state$requestedDataset$score, c("control", "treatment"))
  expect_s3_class(state$resultDecodingDataset$score, "factor")
  expect_equal(levels(state$resultDecodingDataset$score), c("control", "treatment"))
  expect_equal(state$runtimeOptions$variables, "JaspColumn_1_Encoded")
  expect_equal(state$columnMapping, c(JaspColumn_1_Encoded = "score", JaspColumn_2_Encoded = "group"))
  expect_s3_class(state, "jaspSyntax_analysis_dataset_state")
})

test_that("loadAnalysisDataset reuses native .jasp source when provenance is intact", {
  modulePath <- tempfile("jaspSyntaxDatasetModule_")
  dir.create(modulePath)
  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)

  loadedJaspFile <- NULL
  loadedDataFrame <- FALSE

  restoreClear <- localNamespaceBinding(
    "clearDatasetState",
    function() invisible(NULL),
    asNamespace("jaspSyntax")
  )
  restoreLoadDataFrame <- localNamespaceBinding(
    "loadDataSet",
    function(data) {
      loadedDataFrame <<- TRUE
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreLoadJaspFile <- localNamespaceBinding(
    "loadDataSetFromJaspFile",
    function(path) {
      loadedJaspFile <<- path
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreReadQml <- localNamespaceBinding(
    "readAnalysisOptionsFromQml",
    function(...) list(variables = "JaspColumn_1_Encoded"),
    asNamespace("jaspSyntax")
  )
  restoreLoaded <- localGlobalBinding(
    ".readFullDatasetToEnd",
    function() data.frame(JaspColumn_1_Encoded = 1, check.names = FALSE)
  )
  restoreRequested <- localGlobalBinding(
    ".readDataSetRequestedNative",
    function() data.frame(JaspColumn_1_Encoded = 1, check.names = FALSE)
  )
  restoreDecoder <- localGlobalBinding(
    ".decodeColNamesStrict",
    function(columnName) c(JaspColumn_1_Encoded = "score")[[columnName]]
  )
  on.exit(restoreClear(), add = TRUE)
  on.exit(restoreLoadDataFrame(), add = TRUE)
  on.exit(restoreLoadJaspFile(), add = TRUE)
  on.exit(restoreReadQml(), add = TRUE)
  on.exit(restoreLoaded(), add = TRUE)
  on.exit(restoreRequested(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  dataset <- jaspSyntax:::.attachJaspDatasetSource(
    data.frame(score = 1),
    jaspFile,
    1L
  )

  jaspSyntax::loadAnalysisDataset(
    dataset,
    modulePath = modulePath,
    analysisName = "ExampleAnalysis"
  )

  expect_equal(loadedJaspFile, normalizePath(jaspFile, winslash = "/", mustWork = FALSE))
  expect_false(loadedDataFrame)
})

test_that("loadAnalysisDataset clears native state when loading fails", {
  modulePath <- tempfile("jaspSyntaxDatasetModule_")
  dir.create(modulePath)

  clearNativeCalls <- 0L
  restoreClearDataset <- localNamespaceBinding(
    "clearDatasetState",
    function() invisible(NULL),
    asNamespace("jaspSyntax")
  )
  restoreClearNative <- localNamespaceBinding(
    "clearNativeState",
    function() {
      clearNativeCalls <<- clearNativeCalls + 1L
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreLoad <- localNamespaceBinding(
    "loadDataSet",
    function(data) invisible(NULL),
    asNamespace("jaspSyntax")
  )
  restoreReadQml <- localNamespaceBinding(
    "readAnalysisOptionsFromQml",
    function(...) stop("qml failed", call. = FALSE),
    asNamespace("jaspSyntax")
  )
  on.exit(restoreClearDataset(), add = TRUE)
  on.exit(restoreClearNative(), add = TRUE)
  on.exit(restoreLoad(), add = TRUE)
  on.exit(restoreReadQml(), add = TRUE)

  expect_error(
    jaspSyntax::loadAnalysisDataset(
      data.frame(x = 1),
      modulePath = modulePath,
      analysisName = "ExampleAnalysis"
    ),
    "qml failed",
    fixed = TRUE
  )
  expect_equal(clearNativeCalls, 1L)
})

test_that("loadAnalysisDataset validates raw dataset input", {
  expect_error(
    jaspSyntax::loadAnalysisDataset(
      list(x = 1),
      modulePath = tempdir(),
      analysisName = "ExampleAnalysis"
    ),
    "`dataset` must be a data frame",
    fixed = TRUE
  )
})

test_that("lifecycle helpers expose explicit split native controls", {
  expect_null(names(formals(jaspSyntax::clearQmlForms)))
  expect_null(names(formals(jaspSyntax::clearDatasetState)))
  expect_null(names(formals(jaspSyntax::clearNativeState)))
})

test_that("nativeBridgeProvenance parses recorded bridge metadata", {
  provenanceFile <- tempfile("SyntaxInterface", fileext = ".provenance")
  writeLines(
    c(
      "# SyntaxInterface provenance",
      "schema=1",
      "header_origin=C:/jasp/SyntaxInterface/syntaxbridge_interface.h",
      "binary_origin=https://example.invalid/SyntaxInterface.dll",
      "value_with_equals=left=right"
    ),
    provenanceFile
  )

  provenance <- jaspSyntax:::.readNativeBridgeProvenance(provenanceFile)

  expect_equal(provenance[["schema"]], "1")
  expect_equal(
    provenance[["header_origin"]],
    "C:/jasp/SyntaxInterface/syntaxbridge_interface.h"
  )
  expect_equal(provenance[["value_with_equals"]], "left=right")
  expect_equal(
    attr(provenance, "path"),
    normalizePath(provenanceFile, winslash = "/", mustWork = FALSE)
  )
})

test_that("subprocess package loading distinguishes source checkouts from installed libraries", {
  sourceDir <- tempfile("jaspSyntax_source_")
  dir.create(file.path(sourceDir, "R"), recursive = TRUE)
  dir.create(file.path(sourceDir, "src"), recursive = TRUE)
  file.create(file.path(sourceDir, "DESCRIPTION"))
  file.create(file.path(sourceDir, "src", "syntaxfunctions.cpp"))

  installedDir <- tempfile("jaspSyntax_installed_")
  dir.create(file.path(installedDir, "R"), recursive = TRUE)
  file.create(file.path(installedDir, "DESCRIPTION"))

  expect_true(jaspSyntax:::.isSourceCheckoutPath(sourceDir))
  expect_false(jaspSyntax:::.isSourceCheckoutPath(installedDir))
  expect_match(
    paste(jaspSyntax:::.bridgeSubprocessPackageLoaderScript(), collapse = "\n"),
    "pkgload::load_all",
    fixed = TRUE
  )

  descriptionCandidates <- c(
    file.path(getwd(), "DESCRIPTION"),
    file.path(dirname(getwd()), "DESCRIPTION"),
    file.path(dirname(dirname(getwd())), "DESCRIPTION"),
    system.file("DESCRIPTION", package = "jaspSyntax")
  )
  descriptionPath <- descriptionCandidates[file.exists(descriptionCandidates)][1L]
  description <- read.dcf(descriptionPath)
  expect_match(description[1L, "Suggests"], "pkgload", fixed = TRUE)
})

test_that("SyntaxInterface symbol checker fails when DLL exports cannot be inspected", {
  scriptCandidates <- c(
    file.path(getwd(), "tools", "check-syntaxinterface-symbols.sh"),
    file.path(dirname(getwd()), "tools", "check-syntaxinterface-symbols.sh"),
    file.path(dirname(dirname(getwd())), "tools", "check-syntaxinterface-symbols.sh")
  )
  script <- scriptCandidates[file.exists(scriptCandidates)][1L]
  testthat::skip_if(is.na(script), "symbol checker script is not available")

  bash <- Sys.which("bash")
  testthat::skip_if(!nzchar(bash), "bash is not available")
  bashPwd <- suppressWarnings(system2(bash, args = c("-lc", "pwd"), stdout = TRUE, stderr = FALSE))
  bashMountPrefix <- if (length(bashPwd) > 0L && grepl("^/mnt/[A-Za-z]/", bashPwd[[1L]])) {
    "/mnt"
  } else {
    ""
  }
  bashPath <- function(path) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (grepl("^[A-Za-z]:/", path)) {
      return(paste0(bashMountPrefix, "/", tolower(substr(path, 1L, 1L)), substr(path, 3L, nchar(path))))
    }
    path
  }

  tempDir <- tempfile("jaspSyntax_symbol_check_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  header <- file.path(tempDir, "syntaxbridge_interface.h")
  source <- file.path(tempDir, "syntaxfunctions.cpp")
  binary <- file.path(tempDir, "SyntaxInterface.dll")
  writeLines("void syntaxBridgeKnown();", header)
  writeLines("void useBridge() { syntaxBridgeKnown(); }", source)
  writeLines("not a native library", binary)

  oldCheckExports <- Sys.getenv("JASPSYNTAX_CHECK_EXPORTS", unset = NA_character_)
  Sys.setenv(JASPSYNTAX_CHECK_EXPORTS = "true")
  on.exit({
    if (is.na(oldCheckExports)) {
      Sys.unsetenv("JASPSYNTAX_CHECK_EXPORTS")
    } else {
      Sys.setenv(JASPSYNTAX_CHECK_EXPORTS = oldCheckExports)
    }
  }, add = TRUE)

  output <- suppressWarnings(system2(
    bash,
    args = c(bashPath(script), bashPath(header), bashPath(binary), bashPath(source)),
    stdout = TRUE,
    stderr = TRUE
  ))

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  expect_false(identical(status, 0L))
  expect_match(
    paste(output, collapse = "\n"),
    "ERROR: Cannot verify SyntaxInterface binary exports",
    fixed = TRUE
  )
})

test_that("readDatasetFromJaspFile dispatches through the shared bridge subprocess runner", {
  jaspFile <- tempfile(fileext = ".jasp")
  file.create(jaspFile)

  runnerCall <- NULL
  restoreRunner <- localNamespaceBinding(
    ".runBridgeSubprocess",
    function(task, target, input, failureLabel) {
      runnerCall <<- list(
        task = task,
        target = target,
        input = input,
        failureLabel = failureLabel
      )
      data.frame(x = 1)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreRunner(), add = TRUE)

  dataset <- jaspSyntax::readDatasetFromJaspFile(jaspFile)

  expect_s3_class(dataset, "data.frame")
  expect_equal(names(dataset), "x")
  expect_equal(dataset$x, 1)
  expect_equal(
    attr(dataset, "jaspSyntax.jaspFilePath"),
    normalizePath(jaspFile, winslash = "/", mustWork = FALSE)
  )
  expect_equal(attr(dataset, "jaspSyntax.dataSetIndex"), 1L)
  expect_equal(attr(dataset, "jaspSyntax.jaspFileDim"), c(1L, 1L))
  expect_equal(attr(dataset, "jaspSyntax.jaspFileNames"), "x")
  expect_equal(runnerCall$task, "read_dataset")
  expect_equal(runnerCall$target, ".readDatasetFromJaspFileInProcess")
  expect_equal(runnerCall$input$jaspFilePath, jaspFile)
  expect_equal(runnerCall$input$dataSetIndex, 1L)
  expect_true(runnerCall$input$decode)
  expect_true(runnerCall$input$normalize)
  expect_equal(runnerCall$failureLabel, "readDatasetFromJaspFile")
})
