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
Reads a module's \code{Description.qml} metadata. Source checkouts are resolved
directly from \code{Description.qml}/\code{DESCRIPTION}; installed or binary modules
fall back to the native SyntaxInterface bridge.
}
