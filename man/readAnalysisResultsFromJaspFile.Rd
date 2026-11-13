\name{readAnalysisResultsFromJaspFile}
\alias{readAnalysisResultsFromJaspFile}
\title{Read Analysis Results From a JASP File}
\usage{
readAnalysisResultsFromJaspFile(jaspFilePath)
}
\arguments{
\item{jaspFilePath}{Path to a \code{.jasp} file.}
}
\value{
A list of analysis records in file order, named by analysis name. Each record
has \code{id} (the analysis id, also the folder name of its files under
\code{resources/} in the archive), \code{name}, \code{title},
\code{moduleName}, \code{moduleVersion}, \code{status} (e.g.
\code{"complete"}, \code{"validationError"}) and \code{results} (\code{NULL}
when the file holds none for that analysis). The JASP version that wrote the
file and its archive version are in the \code{jaspVersion} and
\code{jaspArchiveVersion} attributes.
}
\description{
Reads the results JASP Desktop saved for every analysis in a \code{.jasp}
file: the same results tree the analysis sent to Desktop (tables with their
schema and raw, unformatted cell values, plots, collections, notes). Desktop
stores them decoded, so column names are the user's.
}
\details{
This only reads the archive; it does not touch the native bridge. A plot
entry refers to its image under \code{resources/} (its \code{data} member),
not to a plot object, so plots cannot be compared with \pkg{vdiffr} against
these results.

The results are returned as stored. When JASP Desktop loads an older archive
it also sweeps its results for legacy \code{JaspColumn_<i>_Encoded} tokens;
this reader does not.
}
\seealso{
\code{\link{readAnalysisOptionsFromJaspFile}},
\code{\link{readDatasetFromJaspFile}}
}
\examples{
\dontrun{
records <- readAnalysisResultsFromJaspFile("analysis.jasp")
records[[1]]$results
attr(records, "jaspVersion")
}
}
