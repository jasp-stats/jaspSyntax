\name{nativeBridgeProvenance}
\alias{nativeBridgeProvenance}
\title{Read Native Bridge Provenance}
\usage{
nativeBridgeProvenance()
}
\value{
A named character vector. The \code{path} attribute points to the provenance
file. An empty vector means the installed package did not record provenance.
}
\description{
Returns installation metadata for the bundled SyntaxInterface bridge, when the
package was installed by a configure script that recorded it. This is a
diagnostic helper for checking whether the header and native binary came from
the same Desktop/build source. Recent installs also record SHA-256 hashes for
the copied header and binary.
}
\seealso{
\code{\link{nativeBridge}}
}
