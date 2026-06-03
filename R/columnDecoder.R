#' Capture a Native Column Encoder Context
#'
#' Captures the source state needed by SyntaxInterface to reconstruct the native
#' `ColumnEncoder`: dataset column names/types and extra QML option encodings.
#' The context can be reused after the active native dataset changes.
#'
#' @return A serializable column encoder context.
#'
#' @export
columnEncoderContext <- function() {
  rawContext <- columnEncoderContextNative()
  .columnEncoderContextFromJson(rawContext)
}

#' Decode Text With a Native Column Encoder Context
#'
#' Decodes embedded JASP column tokens using SyntaxInterface's native
#' `ColumnEncoder` replacement rules.
#'
#' @param text Character vector to decode.
#' @param encoderContext Optional context returned by `columnEncoderContext()`.
#'   When omitted, the current native bridge encoder state is used.
#'
#' @return A character vector with native column tokens decoded.
#'
#' @export
decodeColumnText <- function(text, encoderContext = NULL) {
  if (!is.character(text)) {
    stop("`text` must be a character vector", call. = FALSE)
  }
  if (!.containsEncodedBridgeColumnTokens(text)) {
    return(text)
  }

  contextJson <- .columnEncoderContextJson(encoderContext)
  decoded <- tryCatch(
    decodeColumnTextNative(text, contextJson),
    error = function(e) {
      stop(
        "Native column decoder failed: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (!is.character(decoded) || length(decoded) != length(text)) {
    stop("Native column decoder returned an invalid result.", call. = FALSE)
  }

  names(decoded) <- names(text)
  decoded
}

.columnEncoderContextFromJson <- function(rawContext) {
  if (!is.character(rawContext) || length(rawContext) != 1L || is.na(rawContext)) {
    stop("Native column encoder context must be a single JSON string.", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(rawContext, simplifyVector = FALSE)
  .newColumnEncoderContext(
    rawContext = rawContext,
    columns = .normalizeColumnEncoderContextColumns(parsed[["columns"]]),
    extra = .normalizeColumnEncoderContextColumns(parsed[["extra"]])
  )
}

.newColumnEncoderContext <- function(rawContext = NULL, columns = list(), extra = list()) {
  columns <- .normalizeColumnEncoderContextColumns(columns)
  extra <- .normalizeColumnEncoderContextColumns(extra)

  if (is.null(rawContext)) {
    rawContext <- as.character(jsonlite::toJSON(
      list(version = 1L, columns = columns, extra = extra),
      auto_unbox = TRUE,
      null = "null"
    ))
  }

  structure(
    list(
      version = 1L,
      columns = columns,
      extra = extra,
      native = rawContext
    ),
    class = "jaspSyntaxColumnEncoderContext"
  )
}

.normalizeColumnEncoderContextColumns <- function(columns = NULL) {
  if (is.null(columns) || length(columns) == 0L) {
    return(list())
  }

  if (is.data.frame(columns)) {
    columns <- split(columns, seq_len(nrow(columns)))
  }

  if (!is.list(columns)) {
    stop("Column encoder context columns must be a list.", call. = FALSE)
  }

  lapply(columns, function(column) {
    if (!is.list(column) || is.null(column[["name"]]) || is.null(column[["type"]])) {
      stop("Column encoder context entries must contain `name` and `type`.", call. = FALSE)
    }

    name <- column[["name"]]
    type <- column[["type"]]
    if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name) ||
        !is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
      stop("Column encoder context `name` and `type` entries must be non-empty strings.", call. = FALSE)
    }

    list(name = name, type = type)
  })
}

.columnEncoderContextJson <- function(encoderContext = NULL) {
  if (is.null(encoderContext)) {
    return("")
  }

  if (inherits(encoderContext, "jaspSyntaxColumnEncoderContext")) {
    return(encoderContext[["native"]])
  }

  if (is.character(encoderContext) && length(encoderContext) == 1L && !is.na(encoderContext)) {
    return(encoderContext)
  }

  if (is.list(encoderContext) &&
      (!is.null(encoderContext[["columns"]]) || !is.null(encoderContext[["extra"]]))) {
    return(.newColumnEncoderContext(
      columns = encoderContext[["columns"]],
      extra = encoderContext[["extra"]]
    )[["native"]])
  }

  stop("`encoderContext` must be a native column encoder context.", call. = FALSE)
}

.containsEncodedBridgeColumnTokens <- function(text) {
  if (!is.character(text) || length(text) == 0L) {
    return(FALSE)
  }

  any(grepl(
    "(JaspColumn_[[:alnum:]_]+_Encoded|JaspExtraOptions_[[:alnum:]_]+_Encoded|jaspColumn[0-9]+)",
    text,
    perl = TRUE
  ), na.rm = TRUE)
}
