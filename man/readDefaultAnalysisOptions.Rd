\name{readDefaultAnalysisOptions}
\alias{readDefaultAnalysisOptions}
\title{Read Default Analysis Options}
\usage{
readDefaultAnalysisOptions(
  modulePath,
  analysisName,
  fresh = TRUE,
  includeMeta = TRUE,
  includeTypeOptions = TRUE
)
}
\arguments{
\item{modulePath}{Path to a JASP module source directory.}

\item{analysisName}{Name of the analysis function.}

\item{fresh}{Whether to clear cached QML/native state before parsing.}

\item{includeMeta}{Whether to retain the \code{.meta} option in list output.}

\item{includeTypeOptions}{Whether to retain \code{*.types} options in list output.}
}
\value{
A named list of default options.
}
\description{
Loads an analysis QML form and returns the options produced by the native SyntaxInterface defaults.
}
