#' Capture a Native Column Decoder
#'
#' Captures the current SyntaxInterface `ColumnEncoder` decode mapping in a
#' serializable object. The captured decoder can be reused after the active
#' native dataset changes.
#'
#' @param columnMapping Optional named character vector mapping encoded column
#'   tokens to decoded user-facing names. When supplied, the mapping is
#'   serialized into a native decoder snapshot; token replacement still happens
#'   in SyntaxInterface.
#'
#' @return A serializable column decoder object.
#'
#' @export
columnDecoderSnapshot <- function(columnMapping = NULL) {
  if (!is.null(columnMapping)) {
    return(.columnDecoderSnapshotFromMapping(columnMapping))
  }

  rawSnapshot <- columnDecoderSnapshotNative()
  .columnDecoderSnapshotFromJson(rawSnapshot)
}

#' Decode Text With a Native Column Decoder
#'
#' Decodes embedded JASP column tokens using SyntaxInterface's native
#' `ColumnEncoder` replacement rules.
#'
#' @param text Character vector to decode.
#' @param decoderSnapshot Optional decoder returned by `columnDecoderSnapshot()`.
#'   When omitted, the current native bridge decoder is used.
#'
#' @return A character vector with native column tokens decoded.
#'
#' @export
decodeColumnText <- function(text, decoderSnapshot = NULL) {
  if (!is.character(text)) {
    stop("`text` must be a character vector", call. = FALSE)
  }
  if (!.containsEncodedBridgeColumnTokens(text)) {
    return(text)
  }

  snapshotJson <- .columnDecoderSnapshotJson(decoderSnapshot)
  decoded <- tryCatch(
    decodeColumnTextNative(text, snapshotJson),
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

.columnDecoderSnapshotFromMapping <- function(columnMapping) {
  columnMapping <- .validateAnalysisResultColumnMapping(columnMapping)
  if (is.null(columnMapping)) {
    columnMapping <- stats::setNames(character(), character())
  }

  columns <- unname(Map(
    function(encoded, decoded) list(encoded = encoded, decoded = decoded),
    names(columnMapping),
    unname(columnMapping)
  ))

  rawSnapshot <- as.character(jsonlite::toJSON(
    list(version = 1L, columns = columns),
    auto_unbox = TRUE,
    null = "null"
  ))

  .newColumnDecoderSnapshot(rawSnapshot, columnMapping)
}

.columnDecoderSnapshotFromJson <- function(rawSnapshot) {
  if (!is.character(rawSnapshot) || length(rawSnapshot) != 1L || is.na(rawSnapshot)) {
    stop("Native column decoder snapshot must be a single JSON string.", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(rawSnapshot, simplifyVector = FALSE)
  columns <- parsed[["columns"]]
  if (is.null(columns) || length(columns) == 0L) {
    return(.newColumnDecoderSnapshot(
      rawSnapshot,
      stats::setNames(character(), character())
    ))
  }

  encoded <- vapply(columns, `[[`, character(1L), "encoded", USE.NAMES = FALSE)
  decoded <- vapply(columns, `[[`, character(1L), "decoded", USE.NAMES = FALSE)
  mapping <- stats::setNames(decoded, encoded)

  .newColumnDecoderSnapshot(rawSnapshot, mapping)
}

.newColumnDecoderSnapshot <- function(rawSnapshot, columnMapping) {
  structure(
    list(
      version = 1L,
      columns = .validateAnalysisResultColumnMapping(columnMapping),
      native = rawSnapshot
    ),
    class = "jaspSyntaxColumnDecoder"
  )
}

.columnDecoderSnapshotJson <- function(decoderSnapshot = NULL) {
  if (is.null(decoderSnapshot)) {
    return("")
  }

  if (inherits(decoderSnapshot, "jaspSyntaxColumnDecoder")) {
    return(decoderSnapshot[["native"]])
  }

  if (is.character(decoderSnapshot) && length(decoderSnapshot) == 1L && !is.na(decoderSnapshot)) {
    return(decoderSnapshot)
  }

  if (is.character(decoderSnapshot) && !is.null(names(decoderSnapshot))) {
    return(.columnDecoderSnapshotFromMapping(decoderSnapshot)[["native"]])
  }

  stop("`decoderSnapshot` must be a native decoder snapshot or named column mapping.", call. = FALSE)
}

.decodeColumnTextWithMapping <- function(text, columnMapping) {
  decodeColumnText(text, .columnDecoderSnapshotFromMapping(columnMapping))
}

.containsEncodedBridgeColumnTokens <- function(text) {
  if (!is.character(text) || length(text) == 0L) {
    return(FALSE)
  }

  any(grepl("(JaspColumn_[[:alnum:]_]+_Encoded|jaspColumn[0-9]+)", text, perl = TRUE), na.rm = TRUE)
}
