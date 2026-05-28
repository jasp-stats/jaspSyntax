.normalizeVerboseParameter <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]))
    stop("`verbose` must be one of 'all', 'analysis', 'jasp', 'none', TRUE, or FALSE.", call. = FALSE)

  value <- value[[1L]]
  if (is.logical(value))
    return(if (isTRUE(value)) "all" else "analysis")

  if (is.character(value)) {
    value <- tolower(trimws(value))
    if (value %in% c("true", "yes", "on", "1"))
      return("all")
    if (value %in% c("false", "no", "off", "0"))
      return("analysis")
    if (value %in% c("all", "analysis", "jasp", "none"))
      return(value)
  }

  stop("`verbose` must be one of 'all', 'analysis', 'jasp', 'none', TRUE, or FALSE.", call. = FALSE)
}

.verboseParameterShowsNativeOutput <- function(verbose) {
  verbose %in% c("all", "jasp")
}

#' @export
setParameter <- function(name, value) {
  if (identical(as.character(name), "verbose")) {
    verbose <- .normalizeVerboseParameter(value)
    result <- setParameterNative(name, .verboseParameterShowsNativeOutput(verbose))
    options(jaspSyntax.verbose = verbose)
    return(result)
  }

  setParameterNative(name, value)
}
