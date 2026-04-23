\name{readDatasetFromJaspFile}
\alias{readDatasetFromJaspFile}
\title{Read a Dataset from a JASP File}
\usage{
readDatasetFromJaspFile(jaspFilePath, dataSetIndex = 1L)
}
\arguments{
\item{jaspFilePath}{Character scalar path to a \code{.jasp} file.}

\item{dataSetIndex}{1-based dataset index inside the \code{.jasp} file. Currently only \code{1L} is supported.}
}
\value{
Either a \code{data.frame} containing the dataset or \code{NULL} when the file does not contain tabular data.
}
\description{
Reads the dataset stored inside a saved JASP file and materializes it as an R \code{data.frame}.
}
\details{
This function isolates the native jaspSyntax dataset-loading path in a short-lived subprocess so repeated calls remain safe within the calling R session.

Categorical columns are returned as character vectors. Numeric columns are returned using the bridge's native conversion.
}
\examples{
\dontrun{
dataset <- readDatasetFromJaspFile("path/to/analysis.jasp")
str(dataset)
}
}
