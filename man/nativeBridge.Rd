\name{nativeBridge}
\alias{analysisOptionsFromJaspFile}
\alias{cleanUp}
\alias{generateAnalysisWrapper}
\alias{generateModuleWrappers}
\alias{getVariableNames}
\alias{loadDataSet}
\alias{loadDataSetFromJaspFile}
\alias{loadQmlAndParseOptions}
\alias{parseDescription}
\alias{setParameter}
\title{Low-Level SyntaxInterface Bridge}
\usage{
analysisOptionsFromJaspFile(jaspFilePath, analysisNr)

cleanUp()

generateAnalysisWrapper(modulePath, analysisName)

generateModuleWrappers(modulePath)

getVariableNames()

loadDataSet(data)

loadDataSetFromJaspFile(jaspFilePath)

loadQmlAndParseOptions(
  moduleName,
  analysisName,
  qmlFile,
  options,
  version,
  preloadData
)

parseDescription(modulePath)

setParameter(name, value)
}
\arguments{
\item{jaspFilePath}{Path to a \code{.jasp} file.}

\item{analysisNr}{Zero-based analysis index.}

\item{modulePath}{Path to a JASP module.}

\item{data}{A data frame-like object to load into the native bridge.}

\item{moduleName}{Module name passed to SyntaxInterface.}

\item{analysisName}{Analysis name passed to SyntaxInterface.}

\item{qmlFile}{Path to an analysis QML file.}

\item{options}{JSON object string with analysis options.}

\item{version}{Module version passed to SyntaxInterface.}

\item{preloadData}{Whether the analysis preloads data.}

\item{name}{Native bridge parameter name.}

\item{value}{Native bridge parameter value.}
}
\description{
These functions expose the low-level native SyntaxInterface bridge. They are
exported for compatibility and native integration work. They follow the native
SyntaxInterface ABI and are not the stable package-level contract; ordinary
package code should prefer the higher-level helpers such as
\code{readModuleDescription()},
\code{readAnalysisOptionsFromQml()}, \code{readDefaultAnalysisOptions()},
\code{loadAnalysisDataset()}, \code{readLoadedDataset()},
\code{readRequestedDataset()}, \code{readAnalysisOptionsFromJaspFile()}, and
\code{readDatasetFromJaspFile()}.

\code{cleanUp()} clears the native state and then runs the legacy cleanup hook.
Use \code{\link{clearQmlForms}}, \code{\link{clearDatasetState}}, or
\code{\link{clearNativeState}} when the intended lifecycle scope matters.
Use \code{\link{nativeBridgeProvenance}} to inspect the recorded
SyntaxInterface header/binary source for ABI diagnostics.
}
