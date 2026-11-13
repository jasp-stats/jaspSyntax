#' Read Analysis Results From a JASP File
#'
#' Reads the results JASP Desktop saved for every analysis in a `.jasp` file:
#' the same results tree the analysis sent to Desktop (tables with their
#' schema and raw, unformatted cell values, plots, collections, notes).
#' Desktop stores them decoded, so column names are the user's. The results are
#' returned as stored: Desktop also sweeps the results of older archives for
#' legacy `JaspColumn_<i>_Encoded` tokens when it loads them, which this reader
#' does not do.
#'
#' @param jaspFilePath Path to a `.jasp` file.
#'
#' @return A list of analysis records in file order, named by analysis name.
#'   Each record has `id` (the analysis id, also the folder name of its files
#'   under `resources/` in the archive), `name`, `title`, `moduleName`,
#'   `moduleVersion`, `status` (e.g. `"complete"`, `"validationError"`) and
#'   `results` (`NULL` when the file holds none for that analysis). The JASP
#'   version that wrote the file and its archive version are in the
#'   `jaspVersion` and `jaspArchiveVersion` attributes.
#'
#' @export
readAnalysisResultsFromJaspFile <- function(jaspFilePath) {
  jaspFilePath <- .validateJaspFilePath(jaspFilePath)
  analyses <- .readJaspAnalysisMetadata(jaspFilePath)

  records <- lapply(analyses, function(metadata) {
    identity <- .analysisIdentityFromJaspFile(metadata)
    results <- metadata[["results"]]
    if (!is.list(results) || length(results) == 0L) {
      results <- NULL
    }

    c(
      list(id = .analysisValue(metadata, "id")),
      identity,
      list(
        status = .analysisValue(metadata, "status"),
        results = results
      )
    )
  })

  names(records) <- vapply(
    records,
    function(record) .analysisValue(record, "name", ""),
    character(1L)
  )

  manifest <- .readJaspArchiveJson(jaspFilePath, "manifest.json", mustExist = FALSE)
  attr(records, "jaspVersion") <- .analysisValue(manifest, "jaspVersion")
  attr(records, "jaspArchiveVersion") <- .analysisValue(manifest, "jaspArchiveVersion")

  records
}
