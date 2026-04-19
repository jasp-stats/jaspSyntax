package_library <- paste0(R_PACKAGE_NAME, .Platform$dynlib.ext)

if (!file.exists(package_library)) {
	stop(sprintf("Required compiled library '%s' was not found.", package_library))
}

shared_libraries <- Sys.glob(c("*.dll", "*.so", "*.dylib"))
files <- unique(c(package_library, shared_libraries))
files <- files[file.exists(files)]

dest <- file.path(R_PACKAGE_DIR, paste0("libs", R_ARCH))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

ok <- file.copy(files, dest, overwrite = TRUE)
if (!all(ok)) {
	stop("Failed to copy compiled libraries into the package libs directory.")
}