.validateScalarString <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a single non-empty string", call. = FALSE)
  }

  x
}

.validateModulePath <- function(modulePath) {
  modulePath <- .validateScalarString(modulePath, "modulePath")
  modulePath <- normalizePath(modulePath, winslash = "/", mustWork = FALSE)

  if (file.exists(modulePath) && !dir.exists(modulePath)) {
    if (!identical(basename(modulePath), "Description.qml")) {
      stop("`modulePath` must be a module directory or Description.qml file", call. = FALSE)
    }

    moduleDir <- dirname(modulePath)
    if (identical(basename(moduleDir), "inst")) {
      modulePath <- dirname(moduleDir)
    } else {
      modulePath <- moduleDir
    }
  }

  if (!dir.exists(modulePath)) {
    stop("Module path not found: ", modulePath, call. = FALSE)
  }

  modulePath
}

.validateJaspFilePath <- function(jaspFilePath) {
  jaspFilePath <- .validateScalarString(jaspFilePath, "jaspFilePath")
  jaspFilePath <- normalizePath(jaspFilePath, winslash = "/", mustWork = FALSE)

  if (!file.exists(jaspFilePath)) {
    stop("File not found: ", jaspFilePath, call. = FALSE)
  }

  if (!grepl("\\.jasp$", jaspFilePath, ignore.case = TRUE)) {
    stop("File must have a .jasp extension", call. = FALSE)
  }

  jaspFilePath
}

.validateAnalysisName <- function(analysisName) {
  .validateScalarString(analysisName, "analysisName")
}

.validateQmlFile <- function(qmlFile) {
  qmlFile <- .validateScalarString(qmlFile, "qmlFile")
  qmlFile <- normalizePath(qmlFile, winslash = "/", mustWork = FALSE)

  if (!file.exists(qmlFile)) {
    stop("QML file not found: ", qmlFile, call. = FALSE)
  }

  qmlFile
}

.toOptionsJson <- function(options) {
  if (is.null(options)) {
    return("{}")
  }

  if (is.character(options) && length(options) == 1L) {
    if (!jsonlite::validate(options)) {
      stop("`options` must be a valid JSON string", call. = FALSE)
    }
    parsedOptions <- tryCatch(
      jsonlite::fromJSON(options, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (!is.list(parsedOptions) || is.null(names(parsedOptions))) {
      stop("`options` JSON string must contain a JSON object", call. = FALSE)
    }
    return(options)
  }

  if (!is.list(options)) {
    stop("`options` must be a named list or a JSON string", call. = FALSE)
  }

  if (length(options) == 0L) {
    return("{}")
  }

  if (is.null(names(options)) || any(!nzchar(names(options)))) {
    stop("`options` must be a named list", call. = FALSE)
  }

  as.character(jsonlite::toJSON(
    options,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

.fromJsonObject <- function(json, what) {
  parsed <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) {
      stop(what, " returned invalid JSON: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!is.list(parsed) || is.null(names(parsed))) {
    stop(what, " must return a JSON object", call. = FALSE)
  }

  parsed
}

.moduleQmlPath <- function(modulePath, qmlFileName) {
  qmlFileName <- .validateScalarString(qmlFileName, "qmlFileName")
  candidates <- c(
    file.path(modulePath, "inst", "qml", qmlFileName),
    file.path(modulePath, "qml", qmlFileName)
  )

  qmlFile <- candidates[file.exists(candidates)][1L]
  if (is.na(qmlFile)) {
    stop(
      "Could not locate QML file `", qmlFileName, "` under module path: ",
      modulePath,
      call. = FALSE
    )
  }

  normalizePath(qmlFile, winslash = "/", mustWork = TRUE)
}

.analysisValue <- function(analysis, name, default = NULL) {
  value <- analysis[[name]]
  if (is.null(value) || length(value) == 0L || is.na(value)) {
    return(default)
  }

  value
}

.findAnalysis <- function(description, analysisName) {
  analyses <- description[["analyses"]]
  if (!is.list(analyses) || length(analyses) == 0L) {
    stop("Module description does not contain analyses", call. = FALSE)
  }

  analysisNames <- vapply(
    analyses,
    function(analysis) .analysisValue(analysis, "name", NA_character_),
    character(1L)
  )

  matchIndex <- match(analysisName, analysisNames)
  if (is.na(matchIndex)) {
    stop(
      "Could not locate analysis `", analysisName, "` in module `",
      .analysisValue(description, "name", "<unknown>"), "`",
      call. = FALSE
    )
  }

  analyses[[matchIndex]]
}

.attachOptionAttributes <- function(options, description, analysis, qmlFile = NULL) {
  attr(options, "analysisName") <- .analysisValue(analysis, "name")
  attr(options, "analysisTitle") <- .analysisValue(analysis, "title")
  attr(options, "moduleName") <- .analysisValue(description, "name")
  attr(options, "moduleVersion") <- .analysisValue(description, "version")
  attr(options, "preloadData") <- .analysisValue(analysis, "preloadData")

  if (!is.null(qmlFile)) {
    attr(options, "qmlFile") <- qmlFile
  }

  options
}

.filterOptionMetadata <- function(options, includeMeta, includeTypeOptions) {
  includeMeta <- .validateFlag(includeMeta, "includeMeta")
  includeTypeOptions <- .validateFlag(includeTypeOptions, "includeTypeOptions")

  if (!includeMeta) {
    options[[".meta"]] <- NULL
  }

  if (!includeTypeOptions) {
    options <- options[!grepl("\\.types$", names(options))]
    options <- .dropNestedTypeOptions(options)
  }

  options
}

.dropNestedTypeOptions <- function(options) {
  if (!is.list(options)) {
    return(options)
  }

  optionNames <- names(options)
  if (!is.null(optionNames) && all(c("value", "types") %in% optionNames)) {
    options[["types"]] <- NULL
    optionNames <- names(options)
  }

  options[] <- lapply(options, .dropNestedTypeOptions)

  options
}

#' Read a JASP Module Description
#'
#' Reads a module's `Description.qml` through the native SyntaxInterface bridge.
#'
#' @param modulePath Path to a JASP module source directory or its
#'   `inst/Description.qml` file.
#' @param byName Whether to name the returned `analyses` list by analysis name.
#'
#' @return A list with module metadata and an `analyses` list.
#'
#' @export
parseModuleDescription <- function(modulePath, byName = TRUE) {
  modulePath <- .validateModulePath(modulePath)
  description <- parseDescription(modulePath)

  if (!is.list(description) || is.null(names(description))) {
    stop("jaspSyntax::parseDescription() returned an unexpected object", call. = FALSE)
  }

  if (isTRUE(byName) && is.list(description[["analyses"]])) {
    analysisNames <- vapply(
      description[["analyses"]],
      function(analysis) .analysisValue(analysis, "name", ""),
      character(1L)
    )
    names(description[["analyses"]]) <- analysisNames
  }

  attr(description, "modulePath") <- modulePath
  description
}

#' @rdname parseModuleDescription
#' @export
readModuleDescription <- function(modulePath, byName = TRUE) {
  parseModuleDescription(modulePath, byName = byName)
}

#' Resolve an Analysis QML File
#'
#' Resolves an analysis name to the QML file and metadata provided by the native
#' module description parser.
#'
#' @inheritParams parseModuleDescription
#' @param analysisName Name of the analysis function.
#'
#' @return A list with module description, analysis metadata, QML file path, and
#'   resolved preload flag.
#'
#' @export
resolveAnalysisQml <- function(modulePath, analysisName) {
  modulePath <- .validateModulePath(modulePath)
  analysisName <- .validateAnalysisName(analysisName)

  description <- parseModuleDescription(modulePath, byName = TRUE)
  analysis <- .findAnalysis(description, analysisName)
  qmlFileName <- .analysisValue(analysis, "qml")

  list(
    modulePath = modulePath,
    moduleName = .analysisValue(description, "name"),
    version = .analysisValue(description, "version", ""),
    description = description,
    analysis = analysis,
    analysisName = .analysisValue(analysis, "name"),
    analysisTitle = .analysisValue(analysis, "title"),
    qmlFileName = qmlFileName,
    qmlFile = .moduleQmlPath(modulePath, qmlFileName),
    preloadData = isTRUE(.analysisValue(analysis, "preloadData", TRUE))
  )
}

#' Parse QML Options
#'
#' Loads a QML form and parses supplied options through the native
#' SyntaxInterface bridge. The returned options are the same R-runtime JSON
#' shape prepared for analyses by JASP Desktop: QML controls are bound,
#' option metadata is applied, and column-name/type encoding is handled by the
#' native `ColumnEncoder`.
#'
#' @param qmlFile Path to an analysis QML file.
#' @param options Named list of options, a JSON object string, or `NULL` for
#'   defaults.
#' @param moduleName Module name passed to the native bridge.
#' @param analysisName Analysis name passed to the native bridge. Defaults to
#'   the QML file basename without extension.
#' @param version Module version passed to the native bridge.
#' @param preloadData Whether the analysis preloads data.
#' @param fresh Whether to clear cached QML/native state before parsing. This
#'   should remain `TRUE` when reading defaults.
#' @param output Return parsed R `list` output or raw `json`.
#' @param includeMeta Whether to retain the `.meta` option in list output.
#' @param includeTypeOptions Whether to retain `*.types` options in list output.
#'
#' @return A named list of parsed options, or a JSON string when
#'   `output = "json"`.
#'
#' @export
parseQmlOptions <- function(qmlFile, options = NULL, moduleName = "jaspModule",
                            analysisName = NULL, version = "0",
                            preloadData = TRUE, fresh = TRUE,
                            output = c("list", "json"),
                            includeMeta = TRUE,
                            includeTypeOptions = TRUE) {
  output <- match.arg(output)
  qmlFile <- .validateQmlFile(qmlFile)

  if (is.null(analysisName)) {
    analysisName <- tools::file_path_sans_ext(basename(qmlFile))
  }

  moduleName <- .validateScalarString(moduleName, "moduleName")
  analysisName <- .validateAnalysisName(analysisName)
  version <- .validateScalarString(version, "version")

  if (!is.logical(preloadData) || length(preloadData) != 1L || is.na(preloadData)) {
    stop("`preloadData` must be a single TRUE/FALSE value", call. = FALSE)
  }

  if (!is.logical(fresh) || length(fresh) != 1L || is.na(fresh)) {
    stop("`fresh` must be a single TRUE/FALSE value", call. = FALSE)
  }

  if (fresh) {
    clearQmlForms()
  }

  rawOptions <- loadQmlAndParseOptions(
    moduleName = moduleName,
    analysisName = analysisName,
    qmlFile = qmlFile,
    options = .toOptionsJson(options),
    version = version,
    preloadData = preloadData
  )

  if (!is.character(rawOptions) || length(rawOptions) != 1L || !nzchar(rawOptions)) {
    stop(
      "jaspSyntax::loadQmlAndParseOptions() failed for QML file `",
      qmlFile,
      "`",
      call. = FALSE
    )
  }

  if (identical(output, "json")) {
    return(rawOptions)
  }

  parsedOptions <- .fromJsonObject(rawOptions, "jaspSyntax::loadQmlAndParseOptions()")
  .filterOptionMetadata(parsedOptions, includeMeta, includeTypeOptions)
}

#' Read Analysis Options Through QML
#'
#' Resolves an analysis in a module, loads its QML form, and parses options
#' through the native SyntaxInterface path.
#'
#' @param modulePath Path to a JASP module source directory.
#' @param analysisName Name of the analysis function.
#' @param options Named list of options, a JSON object string, or `NULL` for
#'   defaults.
#' @param version Optional module version override. Defaults to the version from
#'   `Description.qml`/`DESCRIPTION`.
#' @param preloadData Optional preload flag override. Defaults to the analysis
#'   value from the module description.
#' @param fresh Whether to clear cached QML/native state before parsing.
#' @param includeMeta Whether to retain the `.meta` option in list output.
#' @param includeTypeOptions Whether to retain `*.types` options in list output.
#'
#' @return A named list of parsed options.
#'
#' @export
readAnalysisOptionsFromQml <- function(modulePath, analysisName, options = NULL,
                                       version = NULL, preloadData = NULL,
                                       fresh = TRUE,
                                       includeMeta = TRUE,
                                       includeTypeOptions = TRUE) {
  resolved <- resolveAnalysisQml(modulePath, analysisName)
  description <- resolved$description
  analysis <- resolved$analysis

  if (is.null(version)) {
    version <- resolved$version
  } else {
    version <- .validateScalarString(version, "version")
  }

  if (is.null(preloadData)) {
    preloadData <- resolved$preloadData
  } else if (!is.logical(preloadData) || length(preloadData) != 1L || is.na(preloadData)) {
    stop("`preloadData` must be a single TRUE/FALSE value", call. = FALSE)
  }

  parsedOptions <- parseQmlOptions(
    qmlFile = resolved$qmlFile,
    options = options,
    moduleName = resolved$moduleName,
    analysisName = resolved$analysisName,
    version = version,
    preloadData = preloadData,
    fresh = fresh,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )

  .attachOptionAttributes(parsedOptions, description, analysis, resolved$qmlFile)
}

#' @rdname readAnalysisOptionsFromQml
#' @export
analysisOptionsFromQml <- function(modulePath, analysisName, options = NULL,
                                   version = NULL, preloadData = NULL,
                                   fresh = TRUE,
                                   includeMeta = TRUE,
                                   includeTypeOptions = TRUE) {
  readAnalysisOptionsFromQml(
    modulePath = modulePath,
    analysisName = analysisName,
    options = options,
    version = version,
    preloadData = preloadData,
    fresh = fresh,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )
}

#' Read Default Analysis Options
#'
#' Loads an analysis QML form and returns the options produced by the native
#' SyntaxInterface defaults.
#'
#' @inheritParams readAnalysisOptionsFromQml
#'
#' @return A named list of default options.
#'
#' @export
readDefaultAnalysisOptions <- function(modulePath, analysisName, fresh = TRUE,
                                       includeMeta = TRUE,
                                       includeTypeOptions = TRUE) {
  readAnalysisOptionsFromQml(
    modulePath = modulePath,
    analysisName = analysisName,
    options = NULL,
    fresh = fresh,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )
}

.readJaspAnalysisMetadata <- function(jaspFilePath) {
  jaspFilePath <- .validateJaspFilePath(jaspFilePath)

  tempDir <- tempfile("jaspSyntax_analyses_")
  dir.create(tempDir)
  on.exit(unlink(tempDir, recursive = TRUE), add = TRUE)

  utils::unzip(jaspFilePath, files = "analyses.json", exdir = tempDir)
  analysesPath <- file.path(tempDir, "analyses.json")
  if (!file.exists(analysesPath)) {
    stop("Could not find `analyses.json` inside the JASP file", call. = FALSE)
  }

  contents <- .fromJsonObject(
    paste(readLines(analysesPath, warn = FALSE), collapse = "\n"),
    "`analyses.json`"
  )

  analyses <- contents[["analyses"]]
  if (!is.list(analyses) || length(analyses) == 0L) {
    stop("No analyses found in the provided JASP file", call. = FALSE)
  }

  analyses
}

.analysisRecordFromJaspFile <- function(metadata, options) {
  dynamicModule <- metadata[["dynamicModule"]]
  if (!is.list(dynamicModule)) {
    dynamicModule <- list()
  }

  moduleName <- .analysisValue(dynamicModule, "moduleName")
  if (is.null(moduleName)) {
    moduleName <- .analysisValue(metadata, "moduleName")
  }
  if (is.null(moduleName)) {
    moduleName <- .analysisValue(metadata, "module")
  }

  moduleVersion <- .analysisValue(dynamicModule, "moduleVersion")
  if (is.null(moduleVersion)) {
    moduleVersion <- .analysisValue(metadata, "moduleVersion")
  }
  if (is.null(moduleVersion)) {
    moduleVersion <- .analysisValue(metadata, "version")
  }

  analysis <- list(
    name = .analysisValue(metadata, "name"),
    title = .analysisValue(metadata, "title"),
    moduleName = moduleName,
    moduleVersion = moduleVersion,
    options = options
  )

  attr(analysis$options, "analysisName") <- analysis$name
  attr(analysis$options, "analysisTitle") <- analysis$title
  attr(analysis$options, "moduleName") <- analysis$moduleName
  attr(analysis$options, "moduleVersion") <- analysis$moduleVersion

  analysis
}

.validateFlag <- function(value, name) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be a single TRUE/FALSE value", call. = FALSE)
  }

  value
}

.hasUsableNames <- function(x) {
  nms <- names(x)
  !is.null(nms) && any(nzchar(nms))
}

.modulePathMismatchMessage <- function(record, modulePath) {
  expected <- c(record$moduleName, record$name)
  expected <- expected[!is.na(expected) & nzchar(expected)]
  supplied <- names(modulePath)
  supplied <- supplied[!is.na(supplied) & nzchar(supplied)]

  paste0(
    "`modulePath` was named, but none of its names matched ",
    if (length(expected) > 0L) {
      paste0("module/analysis `", paste(expected, collapse = "` or `"), "`")
    } else {
      "the saved module or analysis"
    },
    ". Supplied names: `", paste(supplied, collapse = "`, `"), "`. ",
    "Installed-module fallback is only used when `modulePath = NULL`."
  )
}

.installedModulePathForRecord <- function(record) {
  if (is.null(record$moduleName) || !nzchar(record$moduleName)) {
    stop(
      "Cannot resolve a module path for analysis `",
      .analysisValue(record, "name", "<unknown>"),
      "` because the JASP file does not record a module name",
      call. = FALSE
    )
  }

  found <- find.package(record$moduleName, quiet = TRUE)
  if (length(found) == 0L) {
    stop(
      "Could not locate installed module `", record$moduleName,
      "`. Supply `modulePath` to replay saved options through QML.",
      call. = FALSE
    )
  }

  .validateModulePath(found[[1L]])
}

.modulePathForRecord <- function(record, modulePath = NULL) {
  if (!is.null(modulePath)) {
    if (is.list(modulePath)) {
      if (!is.null(record$moduleName) && record$moduleName %in% names(modulePath)) {
        return(.validateModulePath(modulePath[[record$moduleName]]))
      }
      if (!is.null(record$name) && record$name %in% names(modulePath)) {
        return(.validateModulePath(modulePath[[record$name]]))
      }
      if (length(modulePath) == 1L && !.hasUsableNames(modulePath)) {
        return(.validateModulePath(modulePath[[1L]]))
      }
    } else {
      if (!is.null(names(modulePath)) && !is.null(record$moduleName) &&
          record$moduleName %in% names(modulePath)) {
        return(.validateModulePath(modulePath[[record$moduleName]]))
      }
      if (!is.null(names(modulePath)) && !is.null(record$name) &&
          record$name %in% names(modulePath)) {
        return(.validateModulePath(modulePath[[record$name]]))
      }
      if (length(modulePath) == 1L && !.hasUsableNames(modulePath)) {
        return(.validateModulePath(modulePath))
      }
    }

    if (.hasUsableNames(modulePath)) {
      stop(.modulePathMismatchMessage(record, modulePath), call. = FALSE)
    }

    stop(
      "`modulePath` must be a single module path or named by module/analysis ",
      "when reading runtime options from a multi-module JASP file",
      call. = FALSE
    )
  }

  .installedModulePathForRecord(record)
}

.runtimeOptionsForJaspRecord <- function(record, modulePath,
                                         includeMeta,
                                         includeTypeOptions) {
  resolvedModulePath <- .modulePathForRecord(record, modulePath)
  version <- record$moduleVersion
  if (is.null(version) || !nzchar(version)) {
    version <- NULL
  }

  runtimeOptions <- readAnalysisOptionsFromQml(
    modulePath = resolvedModulePath,
    analysisName = record$name,
    options = record$options,
    version = version,
    fresh = TRUE,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )

  record$options <- runtimeOptions
  record
}

.readAnalysisOptionsFromJaspFileInProcess <- function(jaspFilePath,
                                                      modulePath = NULL,
                                                      runtime = FALSE,
                                                      includeMeta = TRUE,
                                                      includeTypeOptions = TRUE) {
  jaspFilePath <- .validateJaspFilePath(jaspFilePath)
  runtime <- .validateFlag(runtime, "runtime")
  includeMeta <- .validateFlag(includeMeta, "includeMeta")
  includeTypeOptions <- .validateFlag(includeTypeOptions, "includeTypeOptions")
  analyses <- .readJaspAnalysisMetadata(jaspFilePath)

  clearNativeState()
  on.exit(clearNativeState(), add = TRUE)

  if (runtime) {
    loadDataSetFromJaspFile(jaspFilePath)
  }

  records <- vector("list", length(analyses))
  for (i in seq_along(analyses)) {
    options <- analysisOptionsFromJaspFile(jaspFilePath, i - 1L)
    options <- .filterOptionMetadata(options, includeMeta = TRUE, includeTypeOptions = TRUE)
    records[[i]] <- .analysisRecordFromJaspFile(analyses[[i]], options)

    if (runtime) {
      records[[i]] <- .runtimeOptionsForJaspRecord(
        records[[i]],
        modulePath = modulePath,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
    } else {
      records[[i]]$options <- .filterOptionMetadata(
        records[[i]]$options,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      )
    }
  }

  names(records) <- vapply(
    records,
    function(record) .analysisValue(record, "name", ""),
    character(1L)
  )

  records
}

.runReadAnalysisOptionsSubprocess <- function(jaspFilePath, modulePath, runtime,
                                              includeMeta, includeTypeOptions) {
  .runBridgeSubprocess(
    task = "read_options",
    target = ".readAnalysisOptionsFromJaspFileInProcess",
    input = list(
      jaspFilePath = jaspFilePath,
      modulePath = modulePath,
      runtime = runtime,
      includeMeta = includeMeta,
      includeTypeOptions = includeTypeOptions
    ),
    failureLabel = "readAnalysisOptionsFromJaspFile"
  )
}

.runtimeOptionsForJaspRecordsInProcess <- function(records, dataset,
                                                   modulePath = NULL,
                                                   includeMeta = TRUE,
                                                   includeTypeOptions = TRUE) {
  includeMeta <- .validateFlag(includeMeta, "includeMeta")
  includeTypeOptions <- .validateFlag(includeTypeOptions, "includeTypeOptions")

  if (!is.list(records)) {
    stop("`records` must be a list of saved JASP analysis records", call. = FALSE)
  }
  if (!is.null(dataset) && !is.data.frame(dataset)) {
    stop("`dataset` must be a data frame or NULL", call. = FALSE)
  }

  clearNativeState()
  on.exit(clearNativeState(), add = TRUE)

  if (is.data.frame(dataset)) {
    loadDataSet(dataset)
  }

  recordNames <- names(records)
  records <- lapply(records, function(record) {
    .runtimeOptionsForJaspRecord(
      record,
      modulePath = modulePath,
      includeMeta = includeMeta,
      includeTypeOptions = includeTypeOptions
    )
  })
  names(records) <- recordNames
  records
}

.runReadAnalysisRuntimeOptionsSubprocess <- function(jaspFilePath, modulePath,
                                                     includeMeta,
                                                     includeTypeOptions) {
  savedRecords <- .runReadAnalysisOptionsSubprocess(
    jaspFilePath = jaspFilePath,
    modulePath = modulePath,
    runtime = FALSE,
    includeMeta = TRUE,
    includeTypeOptions = TRUE
  )
  dataset <- .runReadDatasetSubprocess(
    jaspFilePath = jaspFilePath,
    dataSetIndex = 1L,
    decode = TRUE,
    normalize = FALSE
  )

  .runBridgeSubprocess(
    task = "read_runtime_options",
    target = ".runtimeOptionsForJaspRecordsInProcess",
    input = list(
      records = savedRecords,
      dataset = dataset,
      modulePath = modulePath,
      includeMeta = includeMeta,
      includeTypeOptions = includeTypeOptions
    ),
    failureLabel = "readAnalysisOptionsFromJaspFile(runtime = TRUE)"
  )
}

#' Read Analysis Options From a JASP File
#'
#' Reads all saved analyses from a `.jasp` file and returns their metadata
#' together with their saved QML-bound options. With `runtime = TRUE`, saved
#' options are replayed through the resolved QML form and native Desktop
#' option encoder so the result matches the R-runtime options prepared by JASP
#' Desktop before calling the analysis. This helper reads the options stored in
#' the archive; it does not replace Desktop's full archive/module upgrade
#' workflow for older files.
#'
#' @param jaspFilePath Path to a `.jasp` file.
#' @param modulePath Optional module path, or a named list/vector of module
#'   paths keyed by module name or analysis name. Required for
#'   `runtime = TRUE` when the module is not installed.
#' @param runtime Whether to replay saved options through QML and the native
#'   Desktop option encoder. The default `FALSE` returns the saved bound
#'   options from `analyses.json`.
#' @param includeMeta Whether to retain the `.meta` option.
#' @param includeTypeOptions Whether to retain `*.types` options when present.
#' @param isolated Whether to run the native `.jasp` option extraction in a
#'   separate R process. This is the default because the SyntaxInterface bridge
#'   owns process-global native state. In-process reads also clear native state
#'   before returning; use `readDatasetFromJaspFile()` for the saved dataset.
#'
#' @return A list of analysis records. Each record has `name`, `title`,
#'   `moduleName`, `moduleVersion`, and `options`.
#'
#' @export
readAnalysisOptionsFromJaspFile <- function(jaspFilePath,
                                            modulePath = NULL,
                                            runtime = FALSE,
                                            includeMeta = TRUE,
                                            includeTypeOptions = TRUE,
                                            isolated = TRUE) {
  jaspFilePath <- .validateJaspFilePath(jaspFilePath)
  runtime <- .validateFlag(runtime, "runtime")
  includeMeta <- .validateFlag(includeMeta, "includeMeta")
  includeTypeOptions <- .validateFlag(includeTypeOptions, "includeTypeOptions")
  isolated <- .validateFlag(isolated, "isolated")

  if (isolated) {
    if (runtime) {
      return(.runReadAnalysisRuntimeOptionsSubprocess(
        jaspFilePath = jaspFilePath,
        modulePath = modulePath,
        includeMeta = includeMeta,
        includeTypeOptions = includeTypeOptions
      ))
    }

    return(.runReadAnalysisOptionsSubprocess(
      jaspFilePath = jaspFilePath,
      modulePath = modulePath,
      runtime = runtime,
      includeMeta = includeMeta,
      includeTypeOptions = includeTypeOptions
    ))
  }

  .readAnalysisOptionsFromJaspFileInProcess(
    jaspFilePath = jaspFilePath,
    modulePath = modulePath,
    runtime = runtime,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )
}
