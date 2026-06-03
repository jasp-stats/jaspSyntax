#' Decode JASP Analysis Result Payloads
#'
#' Decodes native column-name tokens through SyntaxInterface and factor value
#' tokens from the requested dataset used by the analysis.
#'
#' @param results A result payload list, typically decoded from jaspResults JSON.
#' @param requestedDataset Optional requested dataset to use as the factor-label
#'   source. When omitted, the current native requested dataset is read from the
#'   bridge if available.
#' @param columnEncoderContext Optional context returned by
#'   `columnEncoderContext()`. Supplying it lets result replay decode with the
#'   dataset/module state that created the result even after native state changes.
#'
#' @return The result payload with decoded column names and factor values.
#'
#' @export
decodeAnalysisResults <- function(results, requestedDataset = NULL,
                                  columnEncoderContext = NULL) {
  if (!is.list(results)) {
    return(results)
  }

  decodeContext <- .analysisResultDecodeContext(
    requestedDataset,
    columnEncoderContext = columnEncoderContext
  )
  .decodeAnalysisResultObject(results, decodeContext = decodeContext)
}

.analysisResultDecodeContext <- function(requestedDataset = NULL,
                                         columnEncoderContext = NULL) {
  if (is.null(requestedDataset)) {
    requestedDataset <- tryCatch(
      readRequestedDataset(decode = FALSE, normalize = FALSE),
      error = function(e) NULL
    )
  }

  factorValues <- .analysisResultFactorValues(
    requestedDataset,
    columnEncoderContext = columnEncoderContext
  )

  list(
    factorValues = factorValues,
    columnEncoderContext = columnEncoderContext
  )
}

.analysisResultFactorValues <- function(requestedDataset = NULL,
                                        columnEncoderContext = NULL) {
  if (!is.data.frame(requestedDataset)) {
    return(list())
  }

  factorValues <- list()
  for (columnName in names(requestedDataset)) {
    column <- requestedDataset[[columnName]]
    if (!is.factor(column)) {
      next
    }

    valueMap <- stats::setNames(levels(column), as.character(seq_along(levels(column))))
    decodedName <- .decodeAnalysisResultColumnNames(columnName, columnEncoderContext)
    columnKeys <- unique(c(columnName, decodedName))

    for (columnKey in columnKeys) {
      if (is.character(columnKey) && length(columnKey) == 1L && nzchar(columnKey)) {
        factorValues[[columnKey]] <- valueMap
      }
    }
  }

  factorValues
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
      names(x) <- .decodeAnalysisResultColumnNames(oldNames, decodeContext[["columnEncoderContext"]])
    }

    return(x)
  }

  x <- .decodeAnalysisResultFactorValues(x, fieldName, decodeContext)

  if (is.character(x)) {
    x <- .decodeAnalysisResultColumnNames(x, decodeContext[["columnEncoderContext"]])
  }

  x
}

.decodeAnalysisResultFactorValues <- function(x, fieldName, decodeContext) {
  if (is.null(fieldName)) {
    return(x)
  }

  candidateFields <- unique(c(
    fieldName,
    .decodeAnalysisResultColumnNames(fieldName, decodeContext[["columnEncoderContext"]])
  ))
  candidateFields <- candidateFields[!is.na(candidateFields) & nzchar(candidateFields)]

  valueMap <- NULL
  for (candidateField in candidateFields) {
    valueMap <- decodeContext[["factorValues"]][[candidateField]]
    if (!is.null(valueMap)) {
      break
    }
  }
  if (is.null(valueMap)) {
    return(x)
  }

  key <- as.character(x)
  matched <- key %in% names(valueMap)
  if (!any(matched)) {
    return(x)
  }

  out <- as.character(x)
  out[matched] <- unname(valueMap[key[matched]])
  out
}

.decodeAnalysisResultColumnNames <- function(columnNames, columnEncoderContext = NULL) {
  if (!is.character(columnNames) || length(columnNames) == 0L) {
    return(columnNames)
  }

  decodeColumnText(columnNames, columnEncoderContext)
}
