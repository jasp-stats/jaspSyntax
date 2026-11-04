\name{readModuleDescription}
\alias{parseModuleDescription}
\alias{readModuleDescription}
\title{Read a JASP Module Description}
\usage{
parseModuleDescription(modulePath, byName = TRUE)

readModuleDescription(modulePath, byName = TRUE)
}
\arguments{
\item{modulePath}{Path to a JASP module source directory or its \code{inst/Description.qml} file.}

\item{byName}{Whether to name the returned \code{analyses} list by analysis name.}
}
\value{
A list with module metadata and an \code{analyses} list.
}
\description{
Reads a module's \code{Description.qml} metadata through the native
SyntaxInterface bridge, which loads it with JASP Desktop's own module code.
Defaults and derived values (an analysis' \code{preloadData}, the module's
\code{requiresData}, the version string) are therefore exactly what Desktop uses.
}
