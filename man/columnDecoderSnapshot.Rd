\name{columnDecoderSnapshot}
\alias{columnDecoderSnapshot}
\alias{decodeColumnText}
\title{Native Column Decoder Snapshots}
\usage{
columnDecoderSnapshot(columnMapping = NULL)

decodeColumnText(text, decoderSnapshot = NULL)
}
\arguments{
\item{columnMapping}{Optional named character vector mapping encoded column
tokens to decoded user-facing names.}

\item{text}{Character vector to decode.}

\item{decoderSnapshot}{Optional decoder returned by
\code{columnDecoderSnapshot()}. When omitted, the current native bridge decoder
is used.}
}
\value{
\code{columnDecoderSnapshot()} returns a serializable native decoder snapshot.
\code{decodeColumnText()} returns a character vector with native column tokens
decoded.
}
\description{
Capture and reuse SyntaxInterface's native \code{ColumnEncoder} decoder for
analysis result materialization.
}
