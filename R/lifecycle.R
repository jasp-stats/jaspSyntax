#' Native Bridge Lifecycle Helpers
#'
#' These helpers give downstream packages explicit names for the native state
#' they intend to clear. `clearQmlForms()` clears cached QML forms and the QML
#' component cache, `clearDatasetState()` clears bridge-owned dataset state, and
#' `clearNativeState()` clears both.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
clearQmlForms <- function() {
  clearQmlFormsNative()
  invisible(NULL)
}

#' @rdname clearQmlForms
#' @export
clearDatasetState <- function() {
  clearDatasetStateNative()
  invisible(NULL)
}

#' @rdname clearQmlForms
#' @export
clearNativeState <- function() {
  clearNativeStateNative()
  invisible(NULL)
}

.nativeBridgeProvenancePaths <- function() {
  namespacePath <- getNamespaceInfo("jaspSyntax", "path")
  rArch <- sub("^/", "", .Platform$r_arch)

  unique(c(
    file.path(namespacePath, "libs", rArch, "SyntaxInterface.provenance"),
    file.path(namespacePath, "libs", "SyntaxInterface.provenance"),
    file.path(namespacePath, "src", "SyntaxInterface.provenance"),
    file.path(namespacePath, "inst", "libs", "SyntaxInterface.provenance")
  ))
}

.readNativeBridgeProvenance <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]

  values <- strsplit(lines, "=", fixed = TRUE)
  values <- values[lengths(values) >= 2L]
  if (length(values) == 0L) {
    return(structure(character(), path = normalizePath(path, winslash = "/", mustWork = FALSE)))
  }

  keys <- vapply(values, `[[`, character(1L), 1L)
  vals <- vapply(values, function(value) paste(value[-1L], collapse = "="), character(1L))
  vals <- stats::setNames(vals, keys)
  structure(vals, path = normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Read Native Bridge Provenance
#'
#' Returns installation metadata for the bundled SyntaxInterface bridge, when
#' the package was installed by a configure script that recorded it. This is a
#' diagnostic helper for checking whether the header and native binary came from
#' the same Desktop/build source. Recent installs also record SHA-256 hashes for
#' the copied header and binary.
#'
#' @return A named character vector. The `path` attribute points to the
#'   provenance file. An empty vector means the installed package did not record
#'   provenance.
#'
#' @export
nativeBridgeProvenance <- function() {
  paths <- .nativeBridgeProvenancePaths()
  path <- paths[file.exists(paths)][1L]

  if (is.na(path)) {
    return(structure(character(), path = NA_character_))
  }

  .readNativeBridgeProvenance(path)
}
