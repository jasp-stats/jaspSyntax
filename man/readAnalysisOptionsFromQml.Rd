\name{readAnalysisOptionsFromQml}
\alias{analysisOptionsFromQml}
\alias{readAnalysisOptionsFromQml}
\title{Read Analysis Options Through QML}
\usage{
readAnalysisOptionsFromQml(
  modulePath,
  analysisName,
  options = NULL,
  version = NULL,
  preloadData = NULL,
  fresh = TRUE,
  includeMeta = TRUE,
  includeTypeOptions = TRUE
)

analysisOptionsFromQml(
  modulePath,
  analysisName,
  options = NULL,
  version = NULL,
  preloadData = NULL,
  fresh = TRUE,
  includeMeta = TRUE,
  includeTypeOptions = TRUE
)
}
\arguments{
\item{modulePath}{Path to a JASP module source directory.}

\item{analysisName}{Name of the analysis function.}

\item{options}{Named list of options, a JSON object string, or \code{NULL} for defaults.}

\item{version}{Optional module version override. Defaults to the version from the module description.}

\item{preloadData}{Optional preload flag override. Defaults to the analysis value from the module description.}

\item{fresh}{Whether to clear cached QML/native state before parsing.}

\item{includeMeta}{Whether to retain the \code{.meta} option in list output.}

\item{includeTypeOptions}{Whether to retain \code{*.types} options in list output.}
}
\value{
A named list of parsed options.
}
\description{
Resolves an analysis in a module, loads its QML form, and parses options through the native SyntaxInterface path. The returned options are the same R-runtime JSON shape prepared for analyses by JASP Desktop: QML controls are bound, option metadata is applied, and column-name/type encoding is handled by the native \code{ColumnEncoder}.
}
