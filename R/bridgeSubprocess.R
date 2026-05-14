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

.pathEntries <- function(path = Sys.getenv("PATH", unset = "")) {
  entries <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1L]]
  normalizePath(entries[nzchar(entries)], winslash = "/", mustWork = FALSE)
}

.qtRootForPathEntry <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (basename(path) == "bin") {
    path <- dirname(path)
  }

  if (dir.exists(file.path(path, "plugins")) || dir.exists(file.path(path, "qml"))) {
    return(path)
  }

  character(0)
}

.selectedQtRootForSubprocess <- function(pathEntries) {
  explicit <- .qtRootForPathEntry(Sys.getenv("JASPSYNTAX_QT_DIR", unset = ""))
  if (length(explicit) > 0L) {
    return(explicit[[1L]])
  }

  roots <- unique(unlist(lapply(pathEntries, .qtRootForPathEntry), use.names = FALSE))
  roots <- roots[nzchar(roots)]
  msvcRoots <- roots[grepl("/msvc", roots, ignore.case = TRUE)]
  if (length(msvcRoots) > 0L) {
    return(msvcRoots[[1L]])
  }

  siblingMsvcRoots <- unique(unlist(lapply(dirname(roots), function(parent) {
    Sys.glob(file.path(parent, "msvc*"))
  }), use.names = FALSE))
  siblingMsvcRoots <- normalizePath(siblingMsvcRoots[nzchar(siblingMsvcRoots)], winslash = "/", mustWork = FALSE)
  siblingMsvcRoots <- siblingMsvcRoots[dir.exists(file.path(siblingMsvcRoots, "qml"))]
  if (length(siblingMsvcRoots) > 0L) {
    return(siblingMsvcRoots[[1L]])
  }

  if (length(roots) > 0L) {
    roots[[1L]]
  } else {
    character(0)
  }
}

.sanitizeBridgeSubprocessPath <- function(packageSpec) {
  pathEntries <- .pathEntries()
  packagePath <- normalizePath(packageSpec$packagePath, winslash = "/", mustWork = FALSE)
  selectedQtRoot <- .selectedQtRootForSubprocess(pathEntries)
  selectedQtBin <- if (length(selectedQtRoot) > 0L) file.path(selectedQtRoot, "bin") else character(0)

  keep <- vapply(pathEntries, function(path) {
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    isOtherJaspSyntaxRuntime <- grepl("/jaspSyntax/(libs|src)(/|$)", normalized, ignore.case = TRUE) &&
      !startsWith(normalized, packagePath)
    qtRoot <- .qtRootForPathEntry(normalized)
    isOtherQtRuntime <- length(selectedQtRoot) > 0L && length(qtRoot) > 0L &&
      !identical(normalizePath(qtRoot, winslash = "/", mustWork = FALSE), normalizePath(selectedQtRoot, winslash = "/", mustWork = FALSE))

    !isOtherJaspSyntaxRuntime && !isOtherQtRuntime
  }, logical(1L), USE.NAMES = FALSE)

  pathEntries <- pathEntries[keep]
  unique(c(selectedQtBin, pathEntries))
}

.bridgeSubprocessEnv <- function(packageSpec) {
  inherited <- c(
    "JASP_BUILD_DIR",
    "JASPSYNTAX_LIB_DIR",
    "JASPSYNTAX_LIB_PATH",
    "JASPSYNTAX_RUNTIME_DIR",
    "JASPSYNTAX_QT_DIR"
  )
  values <- Sys.getenv(inherited, unset = NA_character_)
  values <- values[!is.na(values)]
  sanitizedPath <- .sanitizeBridgeSubprocessPath(packageSpec)
  selectedQtRoot <- .selectedQtRootForSubprocess(sanitizedPath)
  qtEnv <- character(0)
  if (length(selectedQtRoot) > 0L) {
    qtPlugins <- file.path(selectedQtRoot, "plugins")
    qtQml <- file.path(selectedQtRoot, "qml")
    qtEnv <- c(
      QT_PLUGIN_PATH = if (dir.exists(qtPlugins)) qtPlugins else "",
      QT_QPA_PLATFORM_PLUGIN_PATH = if (dir.exists(file.path(qtPlugins, "platforms"))) file.path(qtPlugins, "platforms") else "",
      QML2_IMPORT_PATH = if (dir.exists(qtQml)) qtQml else "",
      QML_IMPORT_PATH = if (dir.exists(qtQml)) qtQml else ""
    )
  }

  values <- c(PATH = paste(sanitizedPath, collapse = .Platform$path.sep), qtEnv, values)
  values
}

.bridgeSubprocessPackageLoader <- function() {
  function(packageSpec) {
    pathEntries <- function(path = Sys.getenv("PATH", unset = "")) {
      entries <- strsplit(path, .Platform$path.sep, fixed = TRUE)[[1L]]
      normalizePath(entries[nzchar(entries)], winslash = "/", mustWork = FALSE)
    }

    libPaths <- packageSpec$libPaths
    if (length(libPaths) > 0L) {
      .libPaths(c(libPaths, .libPaths()))
    }

    packagePath <- packageSpec$packagePath
    if (isTRUE(packageSpec$sourceCheckout)) {
      dllDirs <- c(
        file.path(packagePath, "src"),
        file.path(packagePath, "libs", R.version$arch),
        file.path(packagePath, "libs")
      )
      dllDirs <- dllDirs[dir.exists(dllDirs)]

      if (.Platform$OS.type == "windows" && length(dllDirs) > 0L) {
        currentPathEntries <- pathEntries()
        buildDir <- Sys.getenv("JASP_BUILD_DIR")
        buildDirs <- if (nzchar(buildDir)) {
          c(file.path(buildDir, "R-Interface"), buildDir)
        } else {
          character(0)
        }
        buildDirs <- buildDirs[dir.exists(buildDirs)]
        Sys.setenv(PATH = paste(unique(c(dllDirs, buildDirs, currentPathEntries)), collapse = .Platform$path.sep))
        message("jaspSyntax subprocess source package: ", packagePath)
        message("jaspSyntax subprocess DLL dirs: ", paste(dllDirs, collapse = ";"))
        message("jaspSyntax subprocess PATH head: ", paste(head(strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1L]], 8L), collapse = ";"))
      }

      if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("pkgload is required to load source-checkout jaspSyntax in a subprocess", call. = FALSE)
      }

      suppressPackageStartupMessages(pkgload::load_all(packagePath, quiet = TRUE, recompile = FALSE))
    } else {
      suppressPackageStartupMessages(library(jaspSyntax))
    }
  }
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
  stdoutPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".out")
  stderrPath <- tempfile(paste0("jaspSyntax_", task, "_"), fileext = ".err")
  on.exit(unlink(c(stdoutPath, stderrPath)), add = TRUE)
  packageSpec <- .bridgeSubprocessPackageSpec()

  result <- tryCatch(
    callr::r(
      func = function(target, input, packageSpec, loadPackage) {
        tryCatch(
          {
            loadPackage(packageSpec)
            do.call(getNamespace("jaspSyntax")[[target]], input)
          },
          error = function(e) {
            structure(list(message = conditionMessage(e)), class = "jaspSyntax_subprocess_error")
          }
        )
      },
      args = list(
        target = target,
        input = input,
        packageSpec = packageSpec,
        loadPackage = .bridgeSubprocessPackageLoader()
      ),
      libpath = .libPaths(),
      stdout = stdoutPath,
      stderr = stderrPath,
      env = .bridgeSubprocessEnv(packageSpec),
      cmdargs = c("--slave", "--no-save", "--no-restore"),
      error = "error"
    ),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "jaspSyntax_subprocess_error")
    }
  )

  output <- .readBridgeSubprocessOutput(stdoutPath, stderrPath)
  outputSuffix <- .bridgeSubprocessOutputSuffix(output)

  if (inherits(result, "jaspSyntax_subprocess_error")) {
    stop(
      failureLabel, " failed: ",
      result$message,
      outputSuffix,
      call. = FALSE
    )
  }

  result
}
