\name{loadAnalysisDataset}
\alias{loadAnalysisDataset}
\alias{readLoadedDataset}
\alias{readRequestedDataset}
\alias{readDatasetHeader}
\alias{decodeColumnNames}
\alias{columnMapping}
\title{High-Level Native Dataset Helpers}
\usage{
loadAnalysisDataset(
  dataset,
  modulePath,
  analysisName,
  options = NULL,
  includeMeta = TRUE,
  includeTypeOptions = TRUE,
  decode = TRUE,
  normalize = TRUE
)

readLoadedDataset(decode = TRUE, normalize = TRUE)

readRequestedDataset(decode = TRUE, normalize = TRUE)

readDatasetHeader(decode = TRUE)

decodeColumnNames(columnNames, strict = FALSE)

columnMapping(encodedColumnNames = NULL, strict = FALSE)
}
\arguments{
\item{dataset}{Raw data frame supplied by the caller.}

\item{modulePath}{Path to a JASP module source directory.}

\item{analysisName}{Name of the analysis function.}

\item{options}{Saved/QML-bound options as a named list, JSON object string, or \code{NULL}.}

\item{includeMeta}{Whether to retain the \code{.meta} option in runtime options.}

\item{includeTypeOptions}{Whether to retain \code{*.types} options in runtime options.}

\item{decode}{Whether to decode native/encoded column names.}

\item{normalize}{Whether to normalize bridge-returned factor columns back to plain character vectors while preserving numeric-looking factor labels.}

\item{columnNames}{Character vector of column names.}

\item{strict}{Whether to fail when an encoded bridge name cannot be decoded. Raw/non-encoded names are returned unchanged.}

\item{encodedColumnNames}{Optional encoded column names. When omitted, the current native dataset header is used.}
}
\value{
\code{loadAnalysisDataset()} returns a list with \code{loadedDataset},
\code{requestedDataset}, \code{resultDecodingDataset},
\code{runtimeOptions}, \code{columnMapping}, \code{modulePath}, and
\code{analysisName}.

\code{readLoadedDataset()} and \code{readRequestedDataset()} return data
frames. \code{readDatasetHeader()} returns a data frame with \code{name} and
\code{encodedName} columns. \code{decodeColumnNames()} returns a character
vector. \code{columnMapping()} returns a named character vector mapping encoded
names to decoded names.
}
\description{
These helpers keep native dataset preload, requested-data state, and column-name
decoding behind the jaspSyntax API. They are the preferred interface for
downstream packages that need loaded/requested dataset state after QML/runtime
option preparation.
}
\details{
\code{loadAnalysisDataset()} is the supported high-level entry point for
jaspTools-style replay. The direct state readers and column mapping helpers are
exported for bridge diagnostics and should be treated as native-facing
integration APIs.

\code{loadAnalysisDataset()} loads the raw data frame into SyntaxInterface,
replays saved/QML-bound options through \code{readAnalysisOptionsFromQml()} with
\code{fresh = TRUE}, then reads both the loaded and requested native dataset
state. This keeps QML option \code{value}/\code{types} interpretation in the
native-backed jaspSyntax path.

The current native bridge exposes requested and decoded dataset state through
R callbacks installed in \code{.GlobalEnv}. These helpers centralize that
callback use. A future Desktop ABI can replace the callback implementation
without changing downstream caller code.
}
\seealso{
\code{\link{readAnalysisOptionsFromQml}},
\code{\link{readDatasetFromJaspFile}},
\code{\link{nativeBridge}}
}
