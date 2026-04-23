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

.readDatasetFromJaspFileInProcess <- function(jaspFilePath, dataSetIndex = 1L) {
  args <- .validateReadDatasetFromJaspFileArgs(jaspFilePath, dataSetIndex)
  jaspFilePath <- args$jaspFilePath
  dataSetIndex <- args$dataSetIndex

  cleanUp()
  on.exit(cleanUp(), add = TRUE)

  loadDataSetFromJaspFile(jaspFilePath)

  readFullDataSet <- get0(".readFullDatasetToEnd", envir = .GlobalEnv, inherits = FALSE)
  if (!is.function(readFullDataSet)) {
    stop("jaspSyntax bridge did not expose `.readFullDatasetToEnd`")
  }

  dataset <- readFullDataSet()
  if (!is.data.frame(dataset)) {
    stop("jaspSyntax bridge returned an unexpected dataset object")
  }

  if (ncol(dataset) == 0L) {
    return(NULL)
  }

  decodeName <- get0(".decodeColNamesStrict", envir = .GlobalEnv, inherits = FALSE)
  if (is.function(decodeName)) {
    names(dataset) <- vapply(names(dataset), function(columnName) {
      as.character(decodeName(columnName))
    }, character(1L))
  }

  dataset[] <- lapply(dataset, .normalizeBridgeColumn)

  dataset
}

.getRscriptBinary <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

.normalizeBridgeColumn <- function(column) {
  if (!is.factor(column)) {
    return(column)
  }

  levelValues <- levels(column)
  numericLevels <- suppressWarnings(as.numeric(levelValues))

  if (length(levelValues) > 0L && !any(is.na(numericLevels))) {
    numericValues <- suppressWarnings(as.numeric(as.character(column)))
    nonMissing <- !is.na(numericValues)

    if (all(numericValues[nonMissing] == as.integer(numericValues[nonMissing]))) {
      return(as.integer(numericValues))
    }

    return(numericValues)
  }

  as.character(column)
}

.runReadDatasetSubprocess <- function(jaspFilePath, dataSetIndex) {
  scriptPath <- tempfile("jaspSyntax_read_dataset_", fileext = ".R")
  outputPath <- tempfile("jaspSyntax_read_dataset_", fileext = ".rds")
  on.exit(unlink(c(scriptPath, outputPath)), add = TRUE)

  script <- c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "jaspFilePath <- args[[1L]]",
    "dataSetIndex <- as.integer(args[[2L]])",
    "outputPath <- args[[3L]]",
    "libPaths <- if (length(args) > 3L) args[4:length(args)] else character(0)",
    "if (length(libPaths) > 0L) .libPaths(c(libPaths, .libPaths()))",
    "result <- tryCatch(local({",
    "  suppressPackageStartupMessages(library(jaspSyntax))",
    "  getNamespace('jaspSyntax')[['.readDatasetFromJaspFileInProcess']](jaspFilePath, dataSetIndex)",
    "}), error = function(e) structure(list(message = conditionMessage(e)), class = 'jaspSyntax_subprocess_error'))",
    "saveRDS(result, outputPath)"
  )

  writeLines(script, scriptPath)

  output <- system2(
    .getRscriptBinary(),
    args = c("--vanilla", scriptPath, jaspFilePath, as.character(dataSetIndex), outputPath, .libPaths()),
    stdout = TRUE,
    stderr = TRUE
  )

  status <- attr(output, "status")
  if (!file.exists(outputPath)) {
    stop(
      "readDatasetFromJaspFile failed before producing a result.",
      if (length(output) > 0L) paste0("\n", paste(output, collapse = "\n")) else ""
    )
  }

  result <- readRDS(outputPath)
  if (inherits(result, "jaspSyntax_subprocess_error")) {
    stop(
      "readDatasetFromJaspFile failed: ",
      result$message,
      if (length(output) > 0L) paste0("\n", paste(output, collapse = "\n")) else ""
    )
  }

  if (!is.null(status) && status != 0L) {
    stop(
      "readDatasetFromJaspFile failed with exit status ",
      status,
      ".",
      if (length(output) > 0L) paste0("\n", paste(output, collapse = "\n")) else ""
    )
  }

  result
}

readDatasetFromJaspFile <- function(jaspFilePath, dataSetIndex = 1L) {
  args <- .validateReadDatasetFromJaspFileArgs(jaspFilePath, dataSetIndex)
  .runReadDatasetSubprocess(args$jaspFilePath, args$dataSetIndex)
}
