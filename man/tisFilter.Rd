\name{tisFilter}
\alias{tisFilter}
\title{Linear Filtering on a Time Series}
\usage{
tisFilter(x, \dots)
}
\arguments{
  \item{x}{a univariate or multivariate time series.}
  \item{\dots}{arguments passed along to \code{filter}.}
}
\description{
  Applies linear filtering to a univariate \code{tis} series or to each
  column separately of a multivariate \code{tis} series.
}
\value{
  A \code{tis} time indexed series with leading and trailing
  \code{NA} values stripped.
}
\note{If ever the \code{filter()} function is made generic, as it should be,
  this function could become the \code{tis} method for it.}
\seealso{ \code{\link{filter}}}
\examples{
x <- tis(1:100, start = c(2000,1), freq = 12)
tisFilter(x, rep(1, 3))
tisFilter(x, rep(1, 3), sides = 1)
tisFilter(x, rep(1, 3), sides = 1, circular = TRUE)
}
\keyword{ts}
