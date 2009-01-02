\name{today}
\alias{today}
\title{Time Index for the Current Date }
\description{Returns a \code{ti} for the current date.}
\usage{
today(tif = "daily")
}
\arguments{
  \item{tif}{
	a ti Frequency, given as either a numerical code or a string.
	\code{tif()} with no arguments returns a list of the allowable
	numerical codes and names. The default "daily" returns a \code{ti}
	object for the current day.
  }
}
\value{
  A \code{ti} object of the specified ti frequency that contains the
  current date in the time interval it represents. For example, if
  \code{tif} is \code{"monthly"}, the returned \code{ti} object will
  be for the current month.
}
\seealso{ \code{\link{ti}}, \code{\link{Sys.Date}}}
\keyword{ chron }
\keyword{ ts }
