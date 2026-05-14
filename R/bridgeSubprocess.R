.getRscriptBinary <- function() {
  file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
}

.isSourceCheckoutPath <- function(packagePath) {
  packagePath <- normalizePath(packagePath, winslash = "/", mustWork = FALSE)

  file.exists(file.path(packagePath, "DESCRIPTION")) &&
    dir.exists(file.path(packagePath, "R")) &&
    file.exists(file.path(packagePath, "src", "syntaxfunctions.cpp"))
}

.bridgeSubprocessPackageSpec <- function() {
  packagePath <- normalizePath(
    getNamespaceInfo(asNamespace("jaspSyntax"), "path"),
    winslash = "/",
    mustWork = FALSE
  )

  list(
    packagePath = packagePath,
    sourceCheckout = .isSourceCheckoutPath(packagePath),
    libPaths = .libPaths()
  )
}

.bridgeSubprocessPackageLoaderScript <- function() {
  c(
    "loadJaspSyntaxForSubprocess <- function(packageSpec) {",
    "  libPaths <- packageSpec$libPaths",
    "  if (length(libPaths) > 0L) .libPaths(c(libPaths, .libPaths()))",
    "  packagePath <- packageSpec$packagePath",
    "  if (isTRUE(packageSpec$sourceCheckout)) {",
    "    dllDirs <- c(file.path(packagePath, 'src'), file.path(packagePath, 'libs', R.version$arch), file.path(packagePath, 'libs'))",
    "    dllDirs <- dllDirs[dir.exists(dllDirs)]",
    "    if (.Platform$OS.type == 'windows' && length(dllDirs) > 0L) {",
    "      Sys.setenv(PATH = paste(c(Sys.getenv('PATH'), dllDirs), collapse = .Platform$path.sep))",
    "    }",
    "    if (!requireNamespace('pkgload', quietly = TRUE)) {",
    "      stop('pkgload is required to load source-checkout jaspSyntax in a subprocess')",
    "    }",
    "    suppressPackageStartupMessages(pkgload::load_all(packagePath, quiet = TRUE, recompile = FALSE))",
    "  } else {",
    "    suppressPackageStartupMessages(library(jaspSyntax))",
    "  }",
    "}"
  )
}

.readBridgeSubprocessOutput <- function(stdoutPath, stderrPath) {
  c(
    if (file.exists(stdoutPath)) readLines(stdoutPath, warn = FALSE) else character(0),
    if (file.exists(stderrPath)) readLines(stderrPath, warn = FALSE) else character(0)
  )
}

.bridgeSubprocessOutputSuffix <- function(output) {
  if (length(output) > 0L) {
    paste0("\n", paste(output, collapse = "\n"))
  } else {
    ""
  }
}

.runBridgeSubprocess <- function(task, target, input, failureLabel) {
  scriptPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".R")
  inputPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".rds")
  outputPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".rds")
  stdoutPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".out")
  stderrPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".err")
  on.exit(unlink(c(scriptPath, inputPath, outputPath, stdoutPath, stderrPath)), add = TRUE)

  saveRDS(
    list(
      input = input,
      target = target,
      packageSpec = .bridgeSubprocessPackageSpec()
    ),
    inputPath
  )

  script <- c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "inputPath <- args[[1L]]",
    "outputPath <- args[[2L]]",
    .bridgeSubprocessPackageLoaderScript(),
    "payload <- readRDS(inputPath)",
    "input <- payload$input",
    "target <- payload$target",
    "packageSpec <- payload$packageSpec",
    "result <- tryCatch(local({",
    "  loadJaspSyntaxForSubprocess(packageSpec)",
    "  do.call(getNamespace('jaspSyntax')[[target]], input)",
    "}), error = function(e) structure(list(message = conditionMessage(e)), class = 'jaspSyntax_subprocess_error'))",
    "saveRDS(result, outputPath)"
  )

  writeLines(script, scriptPath)

  status <- system2(
    .getRscriptBinary(),
    args = c("--vanilla", scriptPath, inputPath, outputPath),
    stdout = stdoutPath,
    stderr = stderrPath
  )

  output <- .readBridgeSubprocessOutput(stdoutPath, stderrPath)
  outputSuffix <- .bridgeSubprocessOutputSuffix(output)

  if (!file.exists(outputPath)) {
    stop(
      failureLabel, " failed before producing a result.",
      outputSuffix,
      call. = FALSE
    )
  }

  result <- readRDS(outputPath)
  if (inherits(result, "jaspSyntax_subprocess_error")) {
    stop(
      failureLabel, " failed: ",
      result$message,
      outputSuffix,
      call. = FALSE
    )
  }

  if (!is.null(status) && status != 0L) {
    stop(
      failureLabel, " failed with exit status ",
      status,
      ".",
      outputSuffix,
      call. = FALSE
    )
  }

  result
}
