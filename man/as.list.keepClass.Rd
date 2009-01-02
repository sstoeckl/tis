\name{as.list.keepClass}
\alias{as.list.keepClass}
\title{ Convert a vector into a list of objects with the same class }
\description{
  Turns its argument into a list of elements with the same class as the argument.
}
\usage{as.list.keepClass(x, \dots)}
\arguments{
  \item{x}{object to be coerced or tested.}
  \item{\dots}{objects, possibly named.}
}
\value{
  A list \code{L} of one-element vectors with
  \code{L[[i]] == x[i] for i in 1:length(x)}
}
\note{
  The implementation of this function is identical to
  \code{as.list.factor}.  It is used in \code{as.list.ti} and
  \code{as.list.jul}.
}
\seealso{ \code{\link{as.list}}}
\examples{
  as.list.keepClass(today() + 1:5)
}
\keyword{list}
\keyword{manip}
