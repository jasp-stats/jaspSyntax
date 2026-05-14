.validateReadDatasetFromJaspFileArgs <- function(jaspFilePath, dataSetIndex) {
  if (!is.character(jaspFilePath) || length(jaspFilePath) != 1L || is.na(jaspFilePath)) {
    stop("`jaspFilePath` must be a single string")
  }

  if (!file.exists(jaspFilePath)) {
    stop("File not found: ", jaspFilePath)
  }

  if (!grepl("\\.jasp$", jaspFilePath, ignore.case = TRUE)) {
    stop("File must have a .jasp extension")
  }

  if (length(dataSetIndex) != 1L || is.na(dataSetIndex) || dataSetIndex != as.integer(dataSetIndex) || dataSetIndex < 1L) {
    stop("`dataSetIndex` must be a single positive integer")
  }

  dataSetIndex <- as.integer(dataSetIndex)
  if (dataSetIndex != 1L) {
    stop("Only `dataSetIndex = 1L` is currently supported by the jaspSyntax bridge")
  }

  list(
    jaspFilePath = jaspFilePath,
    dataSetIndex = dataSetIndex
  )
}

.readDatasetFromJaspFileInProcess <- function(jaspFilePath, dataSetIndex = 1L,
                                              decode = TRUE,
                                              normalize = TRUE) {
  args <- .validateReadDatasetFromJaspFileArgs(jaspFilePath, dataSetIndex)
  decode <- .validateFlag(decode, "decode")
  normalize <- .validateFlag(normalize, "normalize")
  jaspFilePath <- args$jaspFilePath
  dataSetIndex <- args$dataSetIndex

  clearNativeState()
  on.exit(clearNativeState(), add = TRUE)

  loadDataSetFromJaspFile(jaspFilePath)

  dataset <- readLoadedDataset(decode = decode, normalize = normalize)

  if (ncol(dataset) == 0L) {
    return(NULL)
  }

  dataset
}

.normalizeBridgeColumn <- function(column) {
  if (!is.factor(column)) {
    return(column)
  }

  as.character(column)
}

.bridgeCallback <- function(name, what) {
  callback <- get0(name, envir = .GlobalEnv, inherits = FALSE)
  if (!is.function(callback)) {
    stop(
      "jaspSyntax bridge did not expose `", name, "` for ", what,
      call. = FALSE
    )
  }

  callback
}

.readBridgeDataset <- function(callbackName, what) {
  dataset <- .bridgeCallback(callbackName, what)()
  if (!is.data.frame(dataset)) {
    stop(
      "jaspSyntax bridge returned an unexpected ", what, " object",
      call. = FALSE
    )
  }

  dataset
}

.prepareBridgeDataset <- function(dataset, decode = TRUE, normalize = TRUE) {
  decode <- .validateFlag(decode, "decode")
  normalize <- .validateFlag(normalize, "normalize")

  if (decode && ncol(dataset) > 0L) {
    names(dataset) <- decodeColumnNames(names(dataset), strict = TRUE)
  }

  if (normalize && ncol(dataset) > 0L) {
    dataset[] <- lapply(dataset, .normalizeBridgeColumn)
  }

  dataset
}

#' Decode Native JASP Column Names
#'
#' Decodes column names using the native bridge decoder installed by
#' SyntaxInterface. When the bridge does not expose a decoder, the default is to
#' return names unchanged so callers can still operate on non-encoded inputs.
#'
#' @param columnNames Character vector of column names.
#' @param strict Whether to fail when the native decoder is unavailable or a
#'   name cannot be decoded.
#'
#' @return A character vector with decoded names.
#'
#' @export
decodeColumnNames <- function(columnNames, strict = FALSE) {
  if (!is.character(columnNames)) {
    stop("`columnNames` must be a character vector", call. = FALSE)
  }

  strict <- .validateFlag(strict, "strict")
  decodeName <- get0(".decodeColNamesStrict", envir = .GlobalEnv, inherits = FALSE)
  if (!is.function(decodeName)) {
    if (strict) {
      stop(
        "jaspSyntax bridge did not expose `.decodeColNamesStrict`",
        call. = FALSE
      )
    }
    return(columnNames)
  }

  vapply(columnNames, function(columnName) {
    tryCatch(
      {
        decoded <- as.character(decodeName(columnName))
        if (length(decoded) != 1L || is.na(decoded)) {
          stop("decoder returned an empty value")
        }
        decoded
      },
      error = function(e) {
        if (strict) {
          stop(
            "Could not decode column name `", columnName, "`: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
        columnName
      }
    )
  }, character(1L), USE.NAMES = FALSE)
}

#' @rdname decodeColumnNames
#' @param encodedColumnNames Optional encoded column names. When omitted, the
#'   current native dataset header is used.
#'
#' @return `columnMapping()` returns a named character vector mapping encoded
#'   names to decoded names.
#'
#' @export
columnMapping <- function(encodedColumnNames = NULL, strict = FALSE) {
  strict <- .validateFlag(strict, "strict")

  if (is.null(encodedColumnNames)) {
    encodedColumnNames <- readDatasetHeader(decode = FALSE)$encodedName
  }

  if (!is.character(encodedColumnNames)) {
    stop("`encodedColumnNames` must be a character vector", call. = FALSE)
  }

  stats::setNames(
    decodeColumnNames(encodedColumnNames, strict = strict),
    encodedColumnNames
  )
}

#' Read the Loaded Native Dataset
#'
#' Reads the full dataset currently loaded into the native SyntaxInterface
#' bridge. This is the explicit high-level API for code that previously reached
#' into bridge callbacks in `.GlobalEnv`.
#'
#' @param decode Whether to decode native/encoded column names.
#' @param normalize Whether to normalize bridge-returned factor columns back to
#'   plain character vectors while preserving numeric-looking factor labels.
#'
#' @return A data frame.
#'
#' @export
readLoadedDataset <- function(decode = TRUE, normalize = TRUE) {
  dataset <- .readBridgeDataset(".readFullDatasetToEnd", "loaded dataset")
  .prepareBridgeDataset(dataset, decode = decode, normalize = normalize)
}

#' Read the Requested Native Dataset
#'
#' Reads the analysis-requested dataset after QML/runtime option preparation has
#' run through the native SyntaxInterface bridge.
#'
#' @inheritParams readLoadedDataset
#'
#' @return A data frame.
#'
#' @export
readRequestedDataset <- function(decode = TRUE, normalize = TRUE) {
  dataset <- .readBridgeDataset(".readDataSetRequestedNative", "requested dataset")
  .prepareBridgeDataset(dataset, decode = decode, normalize = normalize)
}

#' Read the Native Dataset Header
#'
#' Reads the current native dataset header without materializing the full data
#' frame. The native bridge currently exposes names only; type-rich headers need
#' a future Desktop ABI.
#'
#' @param decode Whether to decode native/encoded column names.
#'
#' @return A data frame with `name` and `encodedName` columns.
#'
#' @export
readDatasetHeader <- function(decode = TRUE) {
  decode <- .validateFlag(decode, "decode")

  encodedNames <- getVariableNames()
  if (is.data.frame(encodedNames)) {
    encodedNames <- names(encodedNames)
  } else {
    encodedNames <- unlist(encodedNames, use.names = FALSE)
  }
  encodedNames <- as.character(encodedNames)

  data.frame(
    name = if (decode) decodeColumnNames(encodedNames, strict = TRUE) else encodedNames,
    encodedName = encodedNames,
    stringsAsFactors = FALSE
  )
}

#' Load an Analysis Dataset Through the Native Bridge
#'
#' Loads a raw R data frame, replays saved/QML-bound analysis options through the
#' native QML preparation path, and returns the loaded and requested dataset
#' state owned by SyntaxInterface.
#'
#' @param dataset Raw data frame supplied by the caller.
#' @param modulePath Path to a JASP module source directory.
#' @param analysisName Name of the analysis function.
#' @param options Saved/QML-bound options as a named list, JSON object string, or
#'   `NULL`.
#' @param includeMeta Whether to retain the `.meta` option in runtime options.
#' @param includeTypeOptions Whether to retain `*.types` options in runtime
#'   options.
#' @inheritParams readLoadedDataset
#'
#' @return A list with `loadedDataset`, `requestedDataset`,
#'   `resultDecodingDataset`, `runtimeOptions`, `columnMapping`, `modulePath`,
#'   and `analysisName`.
#'
#' @export
loadAnalysisDataset <- function(dataset, modulePath, analysisName, options = NULL,
                                includeMeta = TRUE,
                                includeTypeOptions = TRUE,
                                decode = TRUE,
                                normalize = TRUE) {
  if (!is.data.frame(dataset)) {
    stop("`dataset` must be a data frame", call. = FALSE)
  }

  includeMeta <- .validateFlag(includeMeta, "includeMeta")
  includeTypeOptions <- .validateFlag(includeTypeOptions, "includeTypeOptions")
  decode <- .validateFlag(decode, "decode")
  normalize <- .validateFlag(normalize, "normalize")
  modulePath <- .validateModulePath(modulePath)
  analysisName <- .validateAnalysisName(analysisName)

  clearDatasetState()
  loaded <- FALSE
  on.exit({
    if (!loaded) {
      clearNativeState()
    }
  }, add = TRUE)

  loadDataSet(dataset)
  runtimeOptions <- readAnalysisOptionsFromQml(
    modulePath = modulePath,
    analysisName = analysisName,
    options = options,
    fresh = TRUE,
    includeMeta = includeMeta,
    includeTypeOptions = includeTypeOptions
  )

  loadedRaw <- .readBridgeDataset(".readFullDatasetToEnd", "loaded dataset")
  requestedRaw <- .readBridgeDataset(".readDataSetRequestedNative", "requested dataset")
  rawColumnNames <- unique(c(names(loadedRaw), names(requestedRaw)))

  state <- list(
    loadedDataset = .prepareBridgeDataset(
      loadedRaw,
      decode = decode,
      normalize = normalize
    ),
    requestedDataset = .prepareBridgeDataset(
      requestedRaw,
      decode = decode,
      normalize = normalize
    ),
    resultDecodingDataset = .prepareBridgeDataset(
      requestedRaw,
      decode = decode,
      normalize = FALSE
    ),
    runtimeOptions = runtimeOptions,
    columnMapping = columnMapping(rawColumnNames, strict = decode),
    modulePath = modulePath,
    analysisName = analysisName
  )
  class(state) <- c("jaspSyntax_analysis_dataset_state", class(state))

  loaded <- TRUE
  state
}

.runReadDatasetSubprocess <- function(jaspFilePath, dataSetIndex,
                                      decode = TRUE,
                                      normalize = TRUE) {
  .runBridgeSubprocess(
    task = "read_dataset",
    target = ".readDatasetFromJaspFileInProcess",
    input = list(
      jaspFilePath = jaspFilePath,
      dataSetIndex = dataSetIndex,
      decode = decode,
      normalize = normalize
    ),
    failureLabel = "readDatasetFromJaspFile"
  )
}

readDatasetFromJaspFile <- function(jaspFilePath, dataSetIndex = 1L) {
  args <- .validateReadDatasetFromJaspFileArgs(jaspFilePath, dataSetIndex)
  .runReadDatasetSubprocess(
    args$jaspFilePath,
    args$dataSetIndex,
    decode = TRUE,
    normalize = TRUE
  )
}
