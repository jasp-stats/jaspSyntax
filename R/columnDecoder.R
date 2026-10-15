#' Capture a Native Column Encoder Context
#'
#' Captures the source state needed by SyntaxInterface to reconstruct the native
#' `ColumnEncoder`: dataset column names/types, the namespace prefix those
#' tokens live under (the host filter's view prefix since jasp-desktop
#' MDS-PLAN Phase 1), and extra QML option encodings. The context can be reused
#' after the active native dataset changes: replay happens against the process-
#' global native decode table and never mutates live encoder state.
#'
#' A context is opaque: it is captured from the native bridge (this function)
#' and consumed by the native decoder. R never inspects or rebuilds token
#' namespaces itself (MDS-PLAN C14: no token grammar in R).
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
#' `ColumnEncoder` replacement rules. The whole vector goes to the native
#' decoder unconditionally: the native lookup table resolves exactly the tokens
#' the bridge minted and returns everything else unchanged, so plain text is a
#' no-op round trip. Decoding therefore requires a live bridge; R does not
#' guess which strings might be tokens (MDS-PLAN C14).
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
  if (length(text) == 0L) {
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

  structure(
    list(
      version = parsed[["version"]],
      columns = .normalizeColumnEncoderContextColumns(parsed[["columns"]]),
      extra = .normalizeColumnEncoderContextColumns(parsed[["extra"]]),
      prefix = .columnEncoderContextPrefix(parsed),
      native = rawContext
    ),
    class = "jaspSyntaxColumnEncoderContext"
  )
}

.columnEncoderContextPrefix <- function(parsed) {
  prefix <- parsed[["prefix"]]
  if (is.null(prefix)) {
    return("")
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix)) {
    stop("Native column encoder context `prefix` must be a single string.", call. = FALSE)
  }
  prefix
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

  stop(
    "`encoderContext` must be a native column encoder context captured with ",
    "`columnEncoderContext()` (or its JSON string).",
    call. = FALSE
  )
}
