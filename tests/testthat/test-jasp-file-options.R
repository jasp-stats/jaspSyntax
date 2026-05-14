context("JASP file options")

writeTestJaspFile <- function(path, analyses) {
  tempDir <- tempfile("jaspSyntax_test_jasp_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  jsonlite::write_json(
    list(analyses = analyses),
    file.path(tempDir, "analyses.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  oldWd <- getwd()
  setwd(tempDir)
  on.exit(setwd(oldWd), add = TRUE)
  utils::zip(path, "analyses.json")
  invisible(path)
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

test_that("readAnalysisOptionsFromJaspFile returns records with saved bound options", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "FirstAnalysis",
      title = "First Analysis",
      dynamicModule = list(moduleName = "jaspFirst", moduleVersion = "1.0.0")
    ),
    list(
      name = "SecondAnalysis",
      title = "Second Analysis",
      moduleName = "jaspSecond",
      version = "2.0.0"
    )
  )
  writeTestJaspFile(jaspFile, analyses)

  restoreBinding <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      options <- list(
        index = analysisNr,
        option = paste0("option", analysisNr),
        variables = list(types = c("scale", "nominal"), value = c("x", "y")),
        `.meta` = list(variables = list(shouldEncode = TRUE))
      )
      options["emptyOption"] <- list(NULL)
      options
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreBinding(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(jaspFile, isolated = FALSE)

  expect_length(records, 2)
  expect_equal(names(records), c("FirstAnalysis", "SecondAnalysis"))
  expect_equal(records[[1]]$name, "FirstAnalysis")
  expect_equal(records[[1]]$moduleName, "jaspFirst")
  expect_equal(records[[1]]$options$index, 0)
  expect_equal(records[[2]]$options$index, 1)
  expect_equal(records[[2]]$moduleName, "jaspSecond")
  expect_equal(records[[2]]$moduleVersion, "2.0.0")
  expect_true(".meta" %in% names(records[[1]]$options))
  expect_true("emptyOption" %in% names(records[[1]]$options))
  expect_null(records[[1]]$options$emptyOption)
  expect_equal(attr(records[[2]]$options, "analysisName"), "SecondAnalysis")
  expect_equal(attr(records[[2]]$options, "moduleVersion"), "2.0.0")
})

test_that("readAnalysisOptionsFromJaspFile can filter saved metadata", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "FilterAnalysis",
      title = "Filter Analysis",
      dynamicModule = list(moduleName = "jaspFilter", moduleVersion = "1.0.0")
    )
  )
  writeTestJaspFile(jaspFile, analyses)

  restoreBinding <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      list(
        variables = list(types = c("scale", "nominal"), value = c("x", "y")),
        `.meta` = list(variables = list(shouldEncode = TRUE))
      )
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreBinding(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    includeMeta = FALSE,
    includeTypeOptions = FALSE,
    isolated = FALSE
  )

  expect_equal(records[[1]]$options$variables$value, c("x", "y"))
  expect_false("types" %in% names(records[[1]]$options$variables))
  expect_false(".meta" %in% names(records[[1]]$options))
})

test_that("includeTypeOptions false removes nested saved-bound type metadata", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "NestedTypeAnalysis",
      title = "Nested Type Analysis",
      dynamicModule = list(moduleName = "jaspNested", moduleVersion = "1.0.0")
    )
  )
  writeTestJaspFile(jaspFile, analyses)

  restoreBinding <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      list(
        variables = list(types = c("scale", "nominal"), value = c("x", "y")),
        nested = list(
          splitBy = list(value = "group", types = "nominal")
        ),
        `variables.types` = "scale"
      )
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreBinding(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    includeTypeOptions = FALSE,
    isolated = FALSE
  )

  expect_equal(records[[1]]$options$variables, list(value = c("x", "y")))
  expect_equal(records[[1]]$options$nested$splitBy, list(value = "group"))
  expect_false("variables.types" %in% names(records[[1]]$options))
})

test_that("readAnalysisOptionsFromJaspFile can replay saved options through QML runtime path", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "RuntimeAnalysis",
      title = "Runtime Analysis",
      dynamicModule = list(moduleName = "jaspRuntime", moduleVersion = "1.2.3")
    )
  )
  writeTestJaspFile(jaspFile, analyses)
  modulePath <- tempfile("jaspRuntimeModule_")
  dir.create(modulePath)

  loadedData <- FALSE
  replayArgs <- NULL

  restoreAnalysisOptions <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      list(
        variables = list(types = "scale", value = "x"),
        `.meta` = list(variables = list(shouldEncode = TRUE))
      )
    },
    asNamespace("jaspSyntax")
  )
  restoreLoadData <- localNamespaceBinding(
    "loadDataSetFromJaspFile",
    function(jaspFilePath) {
      loadedData <<- TRUE
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreReadQml <- localNamespaceBinding(
    "readAnalysisOptionsFromQml",
    function(modulePath, analysisName, options, version, fresh,
             includeMeta, includeTypeOptions) {
      replayArgs <<- list(
        modulePath = modulePath,
        analysisName = analysisName,
        options = options,
        version = version,
        fresh = fresh,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(variables = "JaspColumn_1_Encoded", `variables.types` = "scale")
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreAnalysisOptions(), add = TRUE)
  on.exit(restoreLoadData(), add = TRUE)
  on.exit(restoreReadQml(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    modulePath = modulePath,
    runtime = TRUE,
    includeMeta = FALSE,
    isolated = FALSE
  )

  expect_true(loadedData)
  expect_equal(replayArgs$modulePath, normalizePath(modulePath, winslash = "/", mustWork = FALSE))
  expect_equal(replayArgs$analysisName, "RuntimeAnalysis")
  expect_equal(replayArgs$version, "1.2.3")
  expect_true(replayArgs$fresh)
  expect_false(replayArgs$includeMeta)
  expect_true(replayArgs$includeTypeOptions)
  expect_equal(records[[1]]$options$variables, "JaspColumn_1_Encoded")
  expect_equal(records[[1]]$options$`variables.types`, "scale")
})

test_that("runtime replay resolves QML metadata through the module description", {
  parseArgs <- NULL
  restoreParseQml <- localNamespaceBinding(
    "parseQmlOptions",
    function(qmlFile, options, moduleName, analysisName, version,
             preloadData, fresh, includeMeta, includeTypeOptions) {
      parseArgs <<- list(
        qmlFile = qmlFile,
        options = options,
        moduleName = moduleName,
        analysisName = analysisName,
        version = version,
        preloadData = preloadData,
        fresh = fresh,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(runtime = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreParseQml(), add = TRUE)

  record <- list(
    name = "MinimalAnalysis",
    moduleName = "jaspSyntaxTestModule",
    moduleVersion = "9.9.9",
    options = list(flag = TRUE)
  )
  modulePath <- testthat::test_path("fixtures", "minimalModule")

  replayed <- jaspSyntax:::.runtimeOptionsForJaspRecord(
    record,
    modulePath = modulePath,
    includeMeta = FALSE,
    includeTypeOptions = TRUE
  )

  expect_equal(basename(parseArgs$qmlFile), "MinimalAnalysis.qml")
  expect_equal(parseArgs$options, record$options)
  expect_equal(parseArgs$analysisName, "MinimalAnalysis")
  expect_equal(parseArgs$version, "9.9.9")
  expect_false(parseArgs$preloadData)
  expect_true(parseArgs$fresh)
  expect_false(parseArgs$includeMeta)
  expect_true(parseArgs$includeTypeOptions)
  expect_true(replayed$options$runtime)
})

test_that("runtime module paths are only reused blindly when unnamed", {
  modulePath <- tempfile("jaspRuntimeModule_")
  dir.create(modulePath)
  normalizedModulePath <- normalizePath(modulePath, winslash = "/", mustWork = FALSE)

  matchedRecord <- list(name = "RuntimeAnalysis", moduleName = "jaspRuntime")
  expect_equal(
    jaspSyntax:::.modulePathForRecord(
      matchedRecord,
      list(jaspRuntime = modulePath)
    ),
    normalizedModulePath
  )

  expect_equal(
    jaspSyntax:::.modulePathForRecord(
      matchedRecord,
      list(RuntimeAnalysis = modulePath)
    ),
    normalizedModulePath
  )

  expect_equal(
    jaspSyntax:::.modulePathForRecord(
      matchedRecord,
      modulePath
    ),
    normalizedModulePath
  )

  unmatchedRecord <- list(
    name = "OtherAnalysis",
    moduleName = "jaspSyntaxDefinitelyMissingModule"
  )
  expect_error(
    jaspSyntax:::.modulePathForRecord(
      unmatchedRecord,
      list(jaspRuntime = modulePath)
    ),
    "Installed-module fallback is only used when `modulePath = NULL`"
  )
})

test_that("runtime replay fails clearly when the resolved module lacks the analysis", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "MissingAnalysis",
      title = "Missing Analysis",
      dynamicModule = list(moduleName = "jaspSyntaxTestModule", moduleVersion = "0.1")
    )
  )
  writeTestJaspFile(jaspFile, analyses)
  modulePath <- testthat::test_path("fixtures", "minimalModule")

  restoreAnalysisOptions <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      list(flag = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  restoreLoadData <- localNamespaceBinding(
    "loadDataSetFromJaspFile",
    function(jaspFilePath) {
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreAnalysisOptions(), add = TRUE)
  on.exit(restoreLoadData(), add = TRUE)

  expect_error(
    jaspSyntax::readAnalysisOptionsFromJaspFile(
      jaspFile,
      modulePath = list(jaspSyntaxTestModule = modulePath),
      runtime = TRUE,
      isolated = FALSE
    ),
    "Could not locate analysis"
  )
})

test_that("multi-analysis runtime replay loads data once and replays each record", {
  jaspFile <- tempfile(fileext = ".jasp")
  analyses <- list(
    list(
      name = "RuntimeOne",
      title = "Runtime One",
      dynamicModule = list(moduleName = "jaspRuntime", moduleVersion = "1.0.0")
    ),
    list(
      name = "RuntimeTwo",
      title = "Runtime Two",
      dynamicModule = list(moduleName = "jaspRuntime", moduleVersion = "1.0.0")
    )
  )
  writeTestJaspFile(jaspFile, analyses)
  modulePath <- tempfile("jaspRuntimeModule_")
  dir.create(modulePath)

  loadCalls <- 0L
  replayCalls <- list()

  restoreAnalysisOptions <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(jaspFilePath, analysisNr) {
      list(source = paste0("saved-", analysisNr))
    },
    asNamespace("jaspSyntax")
  )
  restoreLoadData <- localNamespaceBinding(
    "loadDataSetFromJaspFile",
    function(jaspFilePath) {
      loadCalls <<- loadCalls + 1L
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreReadQml <- localNamespaceBinding(
    "readAnalysisOptionsFromQml",
    function(modulePath, analysisName, options, version, fresh,
             includeMeta, includeTypeOptions) {
      replayCalls[[length(replayCalls) + 1L]] <<- list(
        modulePath = modulePath,
        analysisName = analysisName,
        options = options,
        version = version,
        fresh = fresh,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(replayed = analysisName, source = options$source)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreAnalysisOptions(), add = TRUE)
  on.exit(restoreLoadData(), add = TRUE)
  on.exit(restoreReadQml(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    modulePath = list(jaspRuntime = modulePath),
    runtime = TRUE,
    includeMeta = FALSE,
    isolated = FALSE
  )

  expect_equal(loadCalls, 1L)
  expect_equal(names(records), c("RuntimeOne", "RuntimeTwo"))
  expect_equal(vapply(replayCalls, `[[`, character(1L), "analysisName"),
               c("RuntimeOne", "RuntimeTwo"))
  expect_true(all(vapply(replayCalls, `[[`, logical(1L), "fresh")))
  expect_equal(records$RuntimeOne$options, list(replayed = "RuntimeOne", source = "saved-0"))
  expect_equal(records$RuntimeTwo$options, list(replayed = "RuntimeTwo", source = "saved-1"))
})

test_that("readAnalysisOptionsFromJaspFile isolates native extraction by default", {
  jaspFile <- tempfile(fileext = ".jasp")
  writeTestJaspFile(
    jaspFile,
    list(list(name = "IsolatedAnalysis", title = "Isolated Analysis"))
  )

  runnerCall <- NULL
  restoreBinding <- localNamespaceBinding(
    ".runBridgeSubprocess",
    function(task, target, input, failureLabel) {
      runnerCall <<- list(
        task = task,
        target = target,
        input = input,
        failureLabel = failureLabel
      )
      list(IsolatedAnalysis = list(name = "IsolatedAnalysis", options = list()))
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreBinding(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    modulePath = "C:/fake/module",
    runtime = FALSE,
    includeMeta = FALSE
  )

  expect_equal(names(records), "IsolatedAnalysis")
  expect_equal(runnerCall$task, "read_options")
  expect_equal(runnerCall$target, ".readAnalysisOptionsFromJaspFileInProcess")
  expect_equal(runnerCall$input$jaspFilePath, normalizePath(jaspFile, winslash = "/", mustWork = FALSE))
  expect_equal(runnerCall$input$modulePath, "C:/fake/module")
  expect_false(runnerCall$input$runtime)
  expect_false(runnerCall$input$includeMeta)
  expect_true(runnerCall$input$includeTypeOptions)
  expect_equal(runnerCall$failureLabel, "readAnalysisOptionsFromJaspFile")
})

test_that("isolated runtime .jasp option reads replay from extracted dataset", {
  jaspFile <- tempfile(fileext = ".jasp")
  writeTestJaspFile(
    jaspFile,
    list(list(
      name = "RuntimeAnalysis",
      title = "Runtime Analysis",
      dynamicModule = list(moduleName = "jaspRuntime", moduleVersion = "1.0.0")
    ))
  )

  calls <- list()
  restoreRunner <- localNamespaceBinding(
    ".runReadAnalysisOptionsSubprocess",
    function(jaspFilePath, modulePath, runtime, includeMeta, includeTypeOptions) {
      calls$saved <<- list(
        jaspFilePath = jaspFilePath,
        modulePath = modulePath,
        runtime = runtime,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
      list(RuntimeAnalysis = list(
        name = "RuntimeAnalysis",
        moduleName = "jaspRuntime",
        options = list(variable = list(value = "score", types = "scale"))
      ))
    },
    asNamespace("jaspSyntax")
  )
  restoreDataset <- localNamespaceBinding(
    ".runReadDatasetSubprocess",
    function(jaspFilePath, dataSetIndex, decode, normalize) {
      calls$dataset <<- list(
        jaspFilePath = jaspFilePath,
        dataSetIndex = dataSetIndex,
        decode = decode,
        normalize = normalize
      )
      data.frame(
        score = factor(c("low", "high"), levels = c("low", "high")),
        check.names = FALSE
      )
    },
    asNamespace("jaspSyntax")
  )
  restoreBridge <- localNamespaceBinding(
    ".runBridgeSubprocess",
    function(task, target, input, failureLabel) {
      calls$runtime <<- list(
        task = task,
        target = target,
        input = input,
        failureLabel = failureLabel
      )
      input$records
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreRunner(), add = TRUE)
  on.exit(restoreDataset(), add = TRUE)
  on.exit(restoreBridge(), add = TRUE)

  records <- jaspSyntax::readAnalysisOptionsFromJaspFile(
    jaspFile,
    modulePath = c(jaspRuntime = "C:/fake/module"),
    runtime = TRUE,
    includeMeta = FALSE
  )

  expect_equal(names(records), "RuntimeAnalysis")
  expect_false(calls$saved$runtime)
  expect_true(calls$saved$includeMeta)
  expect_true(calls$saved$includeTypeOptions)
  expect_equal(calls$dataset$jaspFilePath, normalizePath(jaspFile, winslash = "/", mustWork = FALSE))
  expect_equal(calls$dataset$dataSetIndex, 1L)
  expect_true(calls$dataset$decode)
  expect_false(calls$dataset$normalize)
  expect_equal(calls$runtime$task, "read_runtime_options")
  expect_equal(calls$runtime$target, ".runtimeOptionsForJaspRecordsInProcess")
  expect_s3_class(calls$runtime$input$dataset$score, "factor")
  expect_equal(levels(calls$runtime$input$dataset$score), c("low", "high"))
  expect_false(calls$runtime$input$includeMeta)
  expect_true(calls$runtime$input$includeTypeOptions)
  expect_equal(
    calls$runtime$failureLabel,
    "readAnalysisOptionsFromJaspFile(runtime = TRUE)"
  )
})

test_that("in-process .jasp option reads clear native state on exit", {
  jaspFile <- tempfile(fileext = ".jasp")
  writeTestJaspFile(
    jaspFile,
    list(list(name = "CleanupAnalysis", title = "Cleanup Analysis"))
  )

  clearCalls <- 0L
  restoreClear <- localNamespaceBinding(
    "clearNativeState",
    function() {
      clearCalls <<- clearCalls + 1L
      invisible(NULL)
    },
    asNamespace("jaspSyntax")
  )
  restoreOptions <- localNamespaceBinding(
    "analysisOptionsFromJaspFile",
    function(...) stop("forced native read failure", call. = FALSE),
    asNamespace("jaspSyntax")
  )
  on.exit(restoreClear(), add = TRUE)
  on.exit(restoreOptions(), add = TRUE)

  expect_error(
    jaspSyntax:::.readAnalysisOptionsFromJaspFileInProcess(jaspFile),
    "forced native read failure",
    fixed = TRUE
  )
  expect_equal(clearCalls, 2L)
})

test_that("readAnalysisOptionsFromJaspFile validates input", {
  expect_error(
    jaspSyntax::readAnalysisOptionsFromJaspFile("missing.jasp"),
    "File not found"
  )

  csvFile <- tempfile(fileext = ".csv")
  writeLines("x", csvFile)
  expect_error(
    jaspSyntax::readAnalysisOptionsFromJaspFile(csvFile),
    ".jasp extension",
    fixed = TRUE
  )

  jaspFile <- tempfile(fileext = ".jasp")
  writeTestJaspFile(jaspFile, list(list(name = "ValidationAnalysis")))
  expect_error(
    jaspSyntax::readAnalysisOptionsFromJaspFile(jaspFile, isolated = NA),
    "isolated"
  )
})
