\name{clearQmlForms}
\alias{clearQmlForms}
\alias{clearDatasetState}
\alias{clearNativeState}
\title{Native Bridge Lifecycle Helpers}
\usage{
clearQmlForms()

clearDatasetState()

clearNativeState()
}
\value{
Invisibly returns \code{NULL}.
}
\description{
These helpers give downstream packages explicit names for the native state
they intend to clear. \code{clearQmlForms()} clears cached QML forms and the QML
component cache, \code{clearDatasetState()} clears bridge-owned dataset state, and
\code{clearNativeState()} clears both.
}
\seealso{
\code{\link{nativeBridge}}
}
