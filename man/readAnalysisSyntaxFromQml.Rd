\name{readAnalysisSyntaxFromQml}
\alias{readAnalysisSyntaxFromQml}
\title{Read the R Syntax of a Module Analysis}
\usage{
readAnalysisSyntaxFromQml(
  modulePath,
  analysisName,
  options = NULL,
  version = NULL,
  preloadData = NULL,
  fresh = TRUE,
  isolated = FALSE
)
}
\arguments{
\item{modulePath}{Path to a JASP module source directory.}

\item{analysisName}{Name of the analysis function.}

\item{options}{Named list of options, a JSON object string, or \code{NULL} for
defaults.}

\item{version}{Module version the options were written for. Defaults to the
version from \code{Description.qml}/\code{DESCRIPTION}: the options are then not
upgraded.}

\item{preloadData}{Optional preload flag override. Defaults to the analysis
value from the module description.}

\item{fresh}{Whether to clear cached QML/native state before parsing.}

\item{isolated}{Whether to run native QML parsing in a separate R process.
The data loaded in this process is not available there.}
}
\value{
The R syntax, a single string.
}
\description{
Resolves an analysis in a module, as \code{\link{readAnalysisOptionsFromQml}} does,
and gives its R syntax with \code{\link{analysisSyntax}}.
}
