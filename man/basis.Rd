\name{basis}
\alias{basis}
\alias{basis<-}
\alias{observed}
\alias{observed<-}
\title{Optional tis attributes }
\description{tis series have (sometimes implicit) \code{basis} and
  \code{observed} attributes, used when aggregating or disaggregating to
  different frequencies.
}
\usage{
basis(x)
basis(x) <- value
observed(x)
observed(x) <- value
}
\arguments{
  \item{x}{a \code{tis} series}
  \item{value}{a character string, see the details}
}
\details{
  These (optional) attributes of a \code{tis} series are used when
  converting a series from one frequency to another.
  
  A series \code{basis} is "business" or "daily",
  indicating whether the data values in a series are associated with a
  5-day business week or a 7-day calendar week.

  The \code{observed} attribute of series is one of the following:
  \describe{
	\item{\code{annualized}}{Specifies that each time series value is the
	annualized sum of observations made throughout the associated time
	interval. For time scale conversion and totaling purposes, this
	attribute is the same as averaged.}
	\item{\code{averaged}}{Specifies that each time series value is the
	average of the observations made throughout the associated time
	interval.}
    \item{\code{beginning}}{Specifies that each time series value
    represents a single observation made at the beginning of the
    associated time interval.}
    \item{\code{end}}{Specifies that each time series value represents a
    single observation made at the end of the associated time interval.}
	\item{\code{formula}}{Specifies that the time series represents a
	transformation of other series. For time scale conversion and
	totaling purposes, this attribute is the same as averaged.}
	\item{\code{high}}{Specifies that each time series value is the
	maximum value for the time interval.}
	\item{\code{low}}{Specifies that each time series value is the minimum
	value for the time interval.}
	\item{\code{summed}}{Specifies that each time series value is the sum
	of observations made throughout the associated time interval.}
  }
}
\value{\code{basis} and \code{observed} return a character string.  The
  assignment forms invisibly return \code{x}.
}
\references{ The FAME documentation, available from Sungard.}
\seealso{\code{\link{convert}}}
\keyword{ts}
