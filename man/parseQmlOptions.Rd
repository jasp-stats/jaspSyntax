\name{parseQmlOptions}
\alias{parseQmlOptions}
\title{Parse QML Options}
\usage{
parseQmlOptions(
  qmlFile,
  options = NULL,
  moduleName = "jaspModule",
  analysisName = NULL,
  version = "0",
  preloadData = TRUE,
  fresh = TRUE,
  output = c("list", "json"),
  includeMeta = TRUE,
  includeTypeOptions = TRUE,
  isolated = TRUE
)
}
\arguments{
\item{qmlFile}{Path to an analysis QML file.}

\item{options}{Named list of options, a JSON object string, or \code{NULL} for defaults.}

\item{moduleName}{Module name passed to the native bridge.}

\item{analysisName}{Analysis name passed to the native bridge. Defaults to the QML file basename without extension.}

\item{version}{Module version passed to the native bridge.}

\item{preloadData}{Whether the analysis preloads data.}

\item{fresh}{Whether to clear cached QML/native state before parsing. This should remain \code{TRUE} when reading defaults.}

\item{output}{Return parsed R \code{list} output or raw \code{json}.}

\item{includeMeta}{Whether to retain the \code{.meta} option in list output.}

\item{includeTypeOptions}{Whether to retain \code{*.types} options in list output.}

\item{isolated}{Whether to run native QML parsing in a separate R process.}
}
\value{
A named list of parsed options, or a JSON string when \code{output = "json"}.
}
\description{
Loads a QML form and parses supplied options through the native SyntaxInterface bridge. The native bridge binds the QML controls, applies option metadata, and runs the same column-name/type encoding used before JASP Desktop calls the R backend.
}
\details{
This is a low-level/native-facing helper for diagnostics and bridge integration.
Most callers should use \code{\link{readAnalysisOptionsFromQml}} so QML file
names, module versions, and \code{preloadData} come from the parsed module
description.
}
