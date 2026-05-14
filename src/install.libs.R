package_library <- paste0(R_PACKAGE_NAME, .Platform$dynlib.ext)

if (!file.exists(package_library)) {
	stop(sprintf("Required compiled library '%s' was not found.", package_library))
}

shared_libraries <- Sys.glob(c("*.dll", "*.so", "*.dylib"))
metadata_files <- Sys.glob("*.provenance")
files <- unique(c(package_library, shared_libraries, metadata_files))
files <- files[file.exists(files)]

dest <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

ok <- file.copy(files, dest, overwrite = TRUE)
if (!all(ok)) {
	stop("Failed to copy compiled libraries into the package libs directory.")
}

plugin_dirs <- Sys.glob("platforms")
if (length(plugin_dirs) > 0L) {
	ok <- file.copy(plugin_dirs, dest, recursive = TRUE, overwrite = TRUE)
	if (!all(ok)) {
		stop("Failed to copy Qt platform plugins into the package libs directory.")
	}
}
