.qtRootFromPath <- function(path) {
  if (!nzchar(path)) {
    return(character(0))
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (basename(path) == "bin") {
    path <- dirname(path)
  }
  if (dir.exists(file.path(path, "plugins")) || dir.exists(file.path(path, "qml"))) {
    return(path)
  }
  character(0)
}

.prioritizeQtRoots <- function(qtRoots, explicitRoots = character(0)) {
  qtRoots <- unique(normalizePath(qtRoots[nzchar(qtRoots)], winslash = "/", mustWork = FALSE))
  explicitRoots <- unique(normalizePath(explicitRoots[nzchar(explicitRoots)], winslash = "/", mustWork = FALSE))
  discoveredRoots <- setdiff(qtRoots, explicitRoots)

  siblingRoots <- unlist(lapply(discoveredRoots, function(qtRoot) {
    parent <- dirname(qtRoot)
    c(Sys.glob(file.path(parent, "msvc*")), Sys.glob(file.path(parent, "mingw*")))
  }), use.names = FALSE)
  siblingRoots <- normalizePath(siblingRoots[nzchar(siblingRoots)], winslash = "/", mustWork = FALSE)
  siblingRoots <- siblingRoots[dir.exists(file.path(siblingRoots, "bin"))]

  unique(c(explicitRoots, siblingRoots, discoveredRoots))
}

.onLoad <- function(libname, pkgname) {
  namespace <- asNamespace(pkgname)
  reg.finalizer(namespace, function(e) {
    try(get("shutdownNative", envir = e)(), silent = TRUE)
  }, onexit = TRUE)

  rArch <- sub("^/", "", .Platform$r_arch)
  namespacePath <- getNamespaceInfo(pkgname, "path")
  packageLibRoot <- file.path(libname, pkgname, "libs", rArch)
  sourceLibRoot <- file.path(namespacePath, "src")
  explicitQtRoots <- .qtRootFromPath(Sys.getenv("JASPSYNTAX_QT_DIR", unset = ""))
  runtimeDirs <- c(
    Sys.getenv("JASPSYNTAX_QT_DIR", unset = ""),
    strsplit(Sys.getenv("PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
  )
  runtimeDirs <- normalizePath(runtimeDirs[nzchar(runtimeDirs)], winslash = "/", mustWork = FALSE)
  qtRoots <- unique(c(
    dirname(runtimeDirs[dir.exists(file.path(dirname(runtimeDirs), "plugins"))]),
    runtimeDirs[dir.exists(file.path(runtimeDirs, "plugins"))]
  ))
  qtRoots <- .prioritizeQtRoots(qtRoots, explicitRoots = explicitQtRoots)
  runtimePathDirs <- c(packageLibRoot, sourceLibRoot, file.path(qtRoots, "bin"))
  runtimePathDirs <- runtimePathDirs[dir.exists(runtimePathDirs)]
  if (length(runtimePathDirs) > 0L) {
    oldPath <- strsplit(Sys.getenv("PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
    pathDirs <- c(runtimePathDirs, oldPath)
    pathDirs <- pathDirs[nzchar(pathDirs)]
    Sys.setenv(PATH = paste(unique(pathDirs), collapse = .Platform$path.sep))
  }

  qtPluginRoots <- c(
    packageLibRoot,
    sourceLibRoot,
    file.path(qtRoots, "plugins")
  )
  qtPluginRoots <- qtPluginRoots[dir.exists(file.path(qtPluginRoots, "platforms"))]
  if (length(qtPluginRoots) > 0L) {
    oldPluginPath <- strsplit(Sys.getenv("QT_PLUGIN_PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
    pluginPaths <- c(qtPluginRoots, oldPluginPath)
    pluginPaths <- pluginPaths[nzchar(pluginPaths)]
    Sys.setenv(QT_PLUGIN_PATH = paste(unique(pluginPaths), collapse = .Platform$path.sep))

    oldQpaPath <- strsplit(Sys.getenv("QT_QPA_PLATFORM_PLUGIN_PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
    platformPaths <- c(file.path(qtPluginRoots, "platforms"), oldQpaPath)
    platformPaths <- platformPaths[nzchar(platformPaths)]
    Sys.setenv(QT_QPA_PLATFORM_PLUGIN_PATH = paste(unique(platformPaths), collapse = .Platform$path.sep))
  }

  qtQmlRoots <- file.path(qtRoots, "qml")
  qtQmlRoots <- qtQmlRoots[dir.exists(qtQmlRoots)]
  if (length(qtQmlRoots) > 0L) {
    oldQmlPath <- strsplit(Sys.getenv("QML2_IMPORT_PATH", unset = ""), .Platform$path.sep, fixed = TRUE)[[1L]]
    qmlPaths <- c(qtQmlRoots, oldQmlPath)
    qmlPaths <- qmlPaths[nzchar(qmlPaths)]
    qmlPaths <- paste(unique(qmlPaths), collapse = .Platform$path.sep)
    Sys.setenv(QML2_IMPORT_PATH = qmlPaths)
    Sys.setenv(QML_IMPORT_PATH = qmlPaths)
  }
}

.onUnload <- function(libpath) {
  try(shutdownNative(), silent = TRUE)
}
