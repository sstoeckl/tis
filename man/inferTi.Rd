\name{inferTi}
\alias{inferTi}
\title{ Create a Time Index to Match a Vector of DateTimes }
\description{
  Attempts to find a \code{tif} (Time Index Frequency) that "fits" the
  supplied dateTimes, and returns a \code{ti} object that hits the same
  dates and/or times.
}
\usage{
inferTi(dateTimes)
}
\arguments{
  \item{dateTimes}{a vector \code{POSIXct} object, or something that
  \code{as.POSIXct} can coerce into one. }
}
\value{ a \code{ti} object as long as the input}
\note{ May fail if there is no \code{tif} that closely matches the inputs.}
\seealso{ \code{\link{as.ti}}}
\examples{
  inferTi(Sys.time() + (1:5)*86400)
}
\keyword{ chron }
