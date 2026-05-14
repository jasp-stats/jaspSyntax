\name{resolveAnalysisQml}
\alias{resolveAnalysisQml}
\title{Resolve an Analysis QML File}
\usage{
resolveAnalysisQml(modulePath, analysisName)
}
\arguments{
\item{modulePath}{Path to a JASP module source directory or its \code{inst/Description.qml} file.}

\item{analysisName}{Name of the analysis function.}
}
\value{
A list with module description, analysis metadata, QML file path, and resolved preload flag.
}
\description{
Resolves an analysis name to the QML file and metadata provided by the native module description parser.
}
