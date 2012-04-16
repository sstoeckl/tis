\name{as.list.ti}
\alias{as.list.ti}
\alias{as.list.jul}
\title{ Construct a List from a Time Index or Jul Object }
\description{
  \code{as.list.ti} creates a list of one-element \code{ti} objects from
  the elements of its arguments.
  
  \code{as.list.jul} creates a list of one-element \code{jul} objects from
  the elements of its arguments.
}
\usage{
\method{as.list}{ti}(x, \dots)
\method{as.list}{jul}(x, \dots)
}
\arguments{
  \item{x}{ a \code{ti} or \code{jul} object}
  \item{\dots}{ not used }
}
\details{
  These are the \code{ti} and \code{jul} methods for the generic \code{as.list}.
}
\value{
  a list of one-element \code{ti} or \code{jul} objects.
}
\seealso{ \code{\link{asClassyList}}, \code{\link{as.list}}}
\examples{
as.list(today() + 1:5)
as.list(jul(today()) + 1:5)
}
\keyword{list}
\keyword{manip}
