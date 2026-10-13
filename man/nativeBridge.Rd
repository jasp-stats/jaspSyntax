\name{nativeBridge}
\alias{nativeBridge}
\alias{analysisOptionsFromJaspFile}
\alias{cleanUp}
\alias{generateAnalysisWrapper}
\alias{generateModuleWrappers}
\alias{getVariableNames}
\alias{loadDataSet}
\alias{loadDataSets}
\alias{loadDataSetFromJaspFile}
\alias{loadQmlAndParseOptions}
\alias{loadQmlAndParseOptionsStatus}
\alias{decodeJsonText}
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

loadDataSets(datasets)

loadDataSetFromJaspFile(jaspFilePath)

loadQmlAndParseOptions(
  moduleName,
  analysisName,
  qmlFile,
  options,
  version,
  preloadData
)

loadQmlAndParseOptionsStatus(
  moduleName,
  analysisName,
  qmlFile,
  options,
  version,
  preloadData
)

decodeJsonText(json)

parseDescription(modulePath)

setParameter(name, value)
}
\arguments{
\item{jaspFilePath}{Path to a \code{.jasp} file.}

\item{analysisNr}{Zero-based analysis index.}

\item{modulePath}{Path to a JASP module.}

\item{data}{A data frame-like object to load into the native bridge.}

\item{datasets}{A named list of data.frames, one dataset per entry (see
\code{loadDataSets}).}

\item{json}{A character string holding a JSON payload (see \code{decodeJsonText}).}

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

\code{setParameter("verbose", TRUE)} or \code{setParameter("verbose", "jasp")}
enables native SyntaxInterface logging for debugging. For wrapped analyses,
\code{setParameter("verbose", value)} also sets the default verbosity honored by
\code{jaspBase} and \code{jaspTools}: \code{"all"} shows native and analysis
output, \code{"analysis"} shows analysis messages and warnings only,
\code{"jasp"} shows native output only, and \code{"none"} suppresses both.
For compatibility with the old native toggle,
\code{setParameter("verbose", FALSE)} disables native logging while keeping
analysis diagnostics visible. Native logging is disabled by default and can also
be enabled before bridge initialization with the \code{JASP_SYNTAX_VERBOSE=1}
environment variable.

\code{cleanUp()} clears the native state and then runs the legacy cleanup hook.
Use \code{\link{clearQmlForms}}, \code{\link{clearDatasetState}}, or
\code{\link{clearNativeState}} when the intended lifecycle scope matters.
Use \code{\link{nativeBridgeProvenance}} to inspect the recorded
SyntaxInterface header/binary source for ABI diagnostics.

\code{loadDataSet()} returns the bridge-assigned database id of the loaded
dataset invisibly; \code{loadDataSets()} loads a whole named list of data.frames
(one DataSet per entry, first entry primary, state cleared first) and returns
their ids as a named integer vector. These ids are INFORMATIONAL (logging,
slicing results): the dataset selection options of a multi-dataset aware
analysis take the syntax surface instead - the bare dataset title for that
dataset's default filter, or "title - filterName" for one of the named filters
saved in a live JASP file (a freshly loaded dataset has only its default
filter, so in full R a selection is the list name). Exact strings only, never
parsed, never ids; an unknown value fails with a message listing what is
selectable (mds-plan C4).

\code{loadQmlAndParseOptionsStatus()} is \code{loadQmlAndParseOptions()} plus
the full bridge status: a list with the parsed \code{options} JSON string and
\code{multiDataSetJson} (the \code{\{ids, names, dataSetIds, primary\}} plan of
the queued per-filter slices, "" for non-aware analyses).

\code{decodeJsonText()} runs the engine's send-string decoder over a whole
JSON payload: every DataSet's own column encoder in the bridge workspace, then
the extra-options encoder, then the process-global one (each encoder only
replaces the exact tokens it minted, so the sweep is safe regardless of which
dataset was shown last). Non-JSON text is returned unchanged, empty input
as-is, \code{NA} as \code{NA}.
}
