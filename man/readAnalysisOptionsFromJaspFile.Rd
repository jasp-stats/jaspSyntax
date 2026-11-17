\name{readAnalysisOptionsFromJaspFile}
\alias{readAnalysisOptionsFromJaspFile}
\title{Read Analysis Options From a JASP File}
\usage{
readAnalysisOptionsFromJaspFile(
  jaspFilePath,
  modulePath = NULL,
  runtime = FALSE,
  includeMeta = TRUE,
  includeTypeOptions = TRUE,
  isolated = TRUE
)
}
\arguments{
\item{jaspFilePath}{Path to a \code{.jasp} file.}

\item{modulePath}{Optional module path, or a named list/vector of module paths keyed by module name or analysis name. Required for \code{runtime = TRUE} when the module is not installed.}

\item{runtime}{Whether to replay saved options through QML and the native Desktop option encoder. The default \code{FALSE} returns the saved bound options from \code{analyses.json}.}

\item{includeMeta}{Whether to retain the \code{.meta} option.}

\item{includeTypeOptions}{Whether to retain \code{*.types} options when present.}

\item{isolated}{Whether to run the native \code{.jasp} option extraction in a separate R process. This is the default because the SyntaxInterface bridge owns process-global native state. In-process reads also clear native state before returning; use \code{readDatasetFromJaspFile()} for the saved dataset.}
}
\value{
A list of analysis records. Each record has \code{name}, \code{title}, \code{moduleName}, \code{moduleVersion}, and \code{options}.
}
\description{
Reads all saved analyses from a \code{.jasp} file and returns their metadata together with their saved QML-bound options. With \code{runtime = TRUE}, saved options are replayed through the resolved QML form and native Desktop option encoder so the result matches the R-runtime options prepared by JASP Desktop before calling the analysis. This helper reads the options stored in the archive; it does not replace Desktop's full archive/module upgrade workflow for older files.
}
