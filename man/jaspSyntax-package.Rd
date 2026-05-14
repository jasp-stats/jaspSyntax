\name{jaspSyntax-package}
\alias{jaspSyntax-package}
\alias{jaspSyntax}
\docType{package}
\title{
Native JASP SyntaxInterface bridge for R
}
\description{
jaspSyntax exposes the native JASP SyntaxInterface bridge to R. It parses
module descriptions, resolves analysis QML files, replays QML option binding,
loads datasets into native state, and reads saved \code{.jasp} files using the
same runtime preparation path as JASP Desktop.
}
\details{
Saved \code{.jasp} options are read from the archive and then replayed through
QML when runtime options are requested. They are not a replacement for Desktop's
full archive/module upgrade workflow for old files.

Use \code{readModuleDescription()} and \code{resolveAnalysisQml()} for module
metadata, \code{readAnalysisOptionsFromQml()} or
\code{readDefaultAnalysisOptions()} for QML/runtime options, and
\code{loadAnalysisDataset()}, \code{readLoadedDataset()}, and
\code{readRequestedDataset()} for native dataset state. Use
\code{readAnalysisOptionsFromJaspFile()} plus \code{readDatasetFromJaspFile()}
for saved \code{.jasp} files. The low-level native bridge calls remain exported
for compatibility, but package code should prefer the higher-level helpers.
Helpers such as \code{parseQmlOptions()}, lifecycle controls, dataset-state
readers, column mapping, and \code{nativeBridgeProvenance()} are
native-facing/experimental integration APIs.
}
\author{
JASP Team

Maintainer: JASP Team <info@jasp-stats.org>
}
\references{
  This optional section can contain literature or other references for
  background information.
}
\keyword{ package }
\seealso{
\code{\link{readAnalysisOptionsFromJaspFile}},
\code{\link{readAnalysisOptionsFromQml}},
\code{\link{loadAnalysisDataset}},
\code{\link{nativeBridgeProvenance}},
\code{\link{readDatasetFromJaspFile}},
\code{\link{readModuleDescription}}
}
\examples{
  \dontrun{
     records <- readAnalysisOptionsFromJaspFile("analysis.jasp")
     dataset <- readDatasetFromJaspFile("analysis.jasp")
  }
}
