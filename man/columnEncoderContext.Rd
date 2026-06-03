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
\code{ColumnEncoder}: dataset column names/types and extra QML option
encodings.
}
