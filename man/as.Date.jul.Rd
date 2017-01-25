\name{as.Date.jul}
\alias{as.Date.jul}
\alias{as.Date.ti}
\title{Convert ti or jul objects to Dates}
\description{
  Methods to convert \code{ti} and \code{jul} objects to class
  \code{"Date"} representing calendar dates. 
}
\usage{
\method{as.Date}{ti}(x, offset = 1, \dots)
\method{as.Date}{jul}(x, origin = "1970-01-01", \dots)
}
\arguments{
  \item{x}{A \code{ti} or \code{jul} object to be converted.}
  \item{offset}{a number between 0 and 1 specifying where in the period
	represented by the \code{ti} object \code{x} the desired date falls.
	\code{offset = 1} gives the first second of the period and
	\code{offset = 1} the last second, \code{offset = 0.5} the middle
	second, and so on.}
  \item{origin}{a Date object, or something which can be coerced by
    \code{as.Date(origin, \dots)} to such an object.}
  \item{\dots}{additional args passed on to
      \code{\link{as.Date.numeric}}}
}
\value{An object of class \code{"\link{Date}"}.
}
\seealso{
  \code{\link{as.Date}} for the generic function,
  \code{\link{as.Date.numeric}} for the method eventually called, and \link{Date} for
  details of the date class.
}
\examples{
as.Date(today())          ## invokes as.Date.ti
as.Date(jul(today() - 7)) ## a week ago, uses as.Date.jul
}  
\keyword{chron}
