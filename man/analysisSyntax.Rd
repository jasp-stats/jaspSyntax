\name{analysisSyntax}
\alias{analysisSyntax}
\title{Generate the R Syntax of an Analysis}
\usage{
analysisSyntax(
  qmlFile,
  options = NULL,
  moduleName = "jaspModule",
  analysisName = NULL,
  version = "0",
  preloadData = TRUE,
  fresh = TRUE,
  isolated = FALSE
)
}
\arguments{
\item{qmlFile}{Path to an analysis QML file.}

\item{options}{Named list of options, a JSON object string, or \code{NULL} for
defaults.}

\item{moduleName}{Module name passed to the native bridge.}

\item{analysisName}{Analysis name passed to the native bridge. Defaults to
the QML file basename without extension.}

\item{version}{Module version passed to the native bridge.}

\item{preloadData}{Whether the analysis preloads data.}

\item{fresh}{Whether to clear cached QML/native state before parsing. This
should remain \code{TRUE} when reading defaults.}

\item{isolated}{Whether to run native QML parsing in a separate R process.
The data loaded in this process is not available there.}
}
\value{
The R syntax, a single string.
}
\description{
Binds options to a QML form, as \code{\link{parseQmlOptions}} does, and gives the R
syntax JASP shows for the analysis: the call of its R wrapper
(\code{module::analysis(data = NULL, version = ..., ...)}) with the options that
differ from the defaults of the form.
}
\details{
Options of an older \code{version} are upgraded with the \code{Upgrades.qml} of the
module first, and the syntax is then written for the version of the module
of the form. A form drops variables that are not in the loaded data, so load
the data of the analysis first (and keep \code{isolated = FALSE}) when the options
name variables.
}
