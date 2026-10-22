\name{columnEncoderContext}
\alias{columnEncoderContext}
\alias{decodeColumnText}
\title{Native Column Encoder Context}
\usage{
columnEncoderContext()

decodeColumnText(text, encoderContext = NULL)
}
\arguments{
\item{text}{Character vector to decode.}

\item{encoderContext}{Optional context returned by
\code{columnEncoderContext()}. When omitted, the current native bridge encoder
state is used.}
}
\value{
\code{columnEncoderContext()} returns a serializable native encoder context.
\code{decodeColumnText()} returns a character vector with native column tokens
decoded.
}
\description{
Capture and reuse the source state needed by SyntaxInterface's native
\code{ColumnEncoder}: dataset column names/types, the namespace prefix those
tokens live under, and extra QML option encodings.
}
\details{
The whole input vector goes to the native decoder unconditionally: the native
lookup table resolves exactly the tokens the bridge minted and returns
everything else unchanged, so plain text is a no-op round trip and decoding
requires a live bridge. jaspSyntax never pattern-matches token spellings in R.
}
