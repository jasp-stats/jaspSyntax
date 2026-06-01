#' Decode JASP Analysis Result Payloads
#'
#' Decodes native column-name tokens and factor value tokens in analysis results
#' using the current SyntaxInterface dataset state.
#'
#' @param results A result payload list, typically decoded from jaspResults JSON.
#' @param requestedDataset Optional requested dataset to use as the factor-label
#'   source. When omitted, the current native requested dataset is read from the
#'   bridge if available.
#' @param columnMapping Optional named character vector mapping encoded native
#'   column names to decoded user-facing column names. Supplying this avoids a
#'   late native decoder call after analysis execution.
#'
#' @return The result payload with decoded column names and factor values.
#'
#' @export
decodeAnalysisResults <- function(results, requestedDataset = NULL,
                                  columnMapping = NULL) {
  if (!is.list(results)) {
    return(results)
  }

  decodeContext <- .analysisResultDecodeContext(
    requestedDataset,
    columnMapping = columnMapping
  )
  .decodeAnalysisResultObject(results, decodeContext = decodeContext)
}

.analysisResultDecodeContext <- function(requestedDataset = NULL,
                                         columnMapping = NULL) {
  columnMapping <- .validateAnalysisResultColumnMapping(columnMapping)

  if (is.null(requestedDataset)) {
    requestedDataset <- tryCatch(
      readRequestedDataset(decode = FALSE, normalize = FALSE),
      error = function(e) NULL
    )
  }

  if (is.null(columnMapping) && is.data.frame(requestedDataset)) {
    columnMapping <- tryCatch(
      get("columnMapping", envir = asNamespace("jaspSyntax"))(names(requestedDataset), strict = FALSE),
      error = function(e) NULL
    )
  }

  columnDecoder <- .analysisResultColumnDecoder(columnMapping)
  columnDecodeContext <- list(
    columnMapping = columnMapping,
    columnDecoder = columnDecoder
  )

  if (!is.data.frame(requestedDataset)) {
    return(list(
      factorValues = list(),
      columnMapping = columnMapping,
      columnDecoder = columnDecoder
    ))
  }

  factorValues <- list()
  for (columnName in names(requestedDataset)) {
    column <- requestedDataset[[columnName]]
    if (!is.factor(column)) {
      next
    }

    valueMap <- stats::setNames(levels(column), as.character(seq_along(levels(column))))
    decodedName <- tryCatch(
      .decodeAnalysisResultColumnNames(columnName, columnDecodeContext),
      error = function(e) columnName
    )
    columnKeys <- unique(c(
      columnName,
      decodedName,
      .encodedAnalysisResultColumnNames(columnName, columnMapping)
    ))

    for (columnKey in columnKeys) {
      if (is.character(columnKey) && length(columnKey) == 1L && nzchar(columnKey)) {
        factorValues[[columnKey]] <- valueMap
      }
    }
  }

  list(
    factorValues = factorValues,
    columnMapping = columnMapping,
    columnDecoder = columnDecoder
  )
}

.analysisResultColumnDecoder <- function(columnMapping = NULL) {
  tryCatch(
    columnDecoderSnapshot(columnMapping),
    error = function(e) NULL
  )
}

.decodeAnalysisResultObject <- function(x, fieldName = NULL, decodeContext) {
  if (isS4(x) || is.call(x) || is.name(x)) {
    return(x)
  }

  if (is.object(x) && !is.data.frame(x)) {
    return(x)
  }

  if (is.list(x)) {
    oldNames <- names(x)
    for (i in seq_len(length(x))) {
      childName <- if (!is.null(oldNames) && length(oldNames) >= i) oldNames[[i]] else NULL
      child <- tryCatch(x[[i]], error = function(e) NULL)
      x[i] <- list(.decodeAnalysisResultObject(child, fieldName = childName, decodeContext = decodeContext))
    }

    if (!is.null(oldNames)) {
      names(x) <- .decodeAnalysisResultColumnNames(oldNames, decodeContext)
    }

    return(x)
  }

  x <- .decodeAnalysisResultFactorValues(x, fieldName, decodeContext)

  if (is.character(x)) {
    x <- .decodeAnalysisResultColumnNames(x, decodeContext)
  }

  x
}

.decodeAnalysisResultFactorValues <- function(x, fieldName, decodeContext) {
  if (is.null(fieldName) || is.null(decodeContext[["factorValues"]][[fieldName]])) {
    return(x)
  }

  valueMap <- decodeContext[["factorValues"]][[fieldName]]
  key <- as.character(x)
  matched <- key %in% names(valueMap)
  if (!any(matched)) {
    return(x)
  }

  out <- as.character(x)
  out[matched] <- unname(valueMap[key[matched]])
  out
}

.validateAnalysisResultColumnMapping <- function(columnMapping = NULL) {
  if (is.null(columnMapping)) {
    return(NULL)
  }

  if (!is.character(columnMapping) || is.null(names(columnMapping))) {
    stop("`columnMapping` must be a named character vector", call. = FALSE)
  }

  valid <- !is.na(columnMapping) & nzchar(columnMapping) &
    !is.na(names(columnMapping)) & nzchar(names(columnMapping))
  columnMapping[valid]
}

.decodeAnalysisResultColumnNames <- function(columnNames, decodeContext = NULL) {
  if (!is.character(columnNames) || length(columnNames) == 0L) {
    return(columnNames)
  }

  columnMapping <- decodeContext[["columnMapping"]]
  columnDecoder <- decodeContext[["columnDecoder"]]

  if (inherits(columnDecoder, "jaspSyntaxColumnDecoder")) {
    return(decodeColumnText(columnNames, columnDecoder))
  }

  if (length(columnMapping) > 0L) {
    return(.decodeColumnNamesWithMapping(columnNames, columnMapping))
  }

  decodeColumnText(columnNames)
}

.encodedAnalysisResultColumnNames <- function(decodedColumnName,
                                              columnMapping = NULL) {
  if (!is.character(decodedColumnName) || length(decodedColumnName) != 1L ||
      length(columnMapping) == 0L) {
    return(character(0))
  }

  names(columnMapping)[!is.na(columnMapping) & columnMapping == decodedColumnName]
}
