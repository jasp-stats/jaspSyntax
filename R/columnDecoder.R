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
#' The R-side wrapper is the mds-plan D8.4 host-filter-prefix-only surface:
#' `{version, prefix, native}` - `prefix` is the host filter's view prefix
#' (introspection only), `native` is the opaque capture the decoder consumes.
#'
#' @return A serializable column encoder context.
#'
#' @export
columnEncoderContext <- function() {
  rawContext <- columnEncoderContextNative()
  .columnEncoderContextFromJson(rawContext)
}#' Decode Text With a Native Column Encoder Context
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

  # mds-plan D8.4 (context inventory/shrink): the wrapper is the HOST-FILTER-PREFIX-ONLY
  # surface - `{version, prefix, native}`. The per-column `columns`/`extra` payloads the
  # capture also carries were duplicated here (normalized, validated) but read by nothing:
  # `.columnEncoderContextJson` hands the opaque `native` string back to the native decoder
  # (replay against the process-global decode table), and `prefix` is introspection only.
  # The native side validates its own input at replay time, so R does not re-parse the
  # payload - and a malformed capture surfaces at decode, never as a silent wrapper.
  parsed <- tryCatch(jsonlite::fromJSON(rawContext, simplifyVector = FALSE),
                     error = function(e) stop("Native column encoder context is not valid JSON: ",
                                              conditionMessage(e), call. = FALSE))

  structure(
    list(
      version = parsed[["version"]],
      prefix  = .columnEncoderContextPrefix(parsed),
      native  = rawContext
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
