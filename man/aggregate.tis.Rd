\name{aggregate.tis}
\alias{aggregate.tis}
\title{Compute Summary Statistics of Time Series Subsets}
\usage{
\method{aggregate}{tis}(x, FUN = sum, \dots)
}
\description{
  Splits the data into subsets, computes summary statistics for each,
  and returns the result in a convenient form.
}
\arguments{
  \item{x}{a \code{tis} time series.}
  \item{FUN}{a scalar function to compute the summary statistics which
    can be applied to all data subsets.}
  \item{\dots}{further arguments passed to \code{\link{aggregate.ts}}}
}
\details{
  This is a method for the generic \code{aggregate} function.

  The \code{aggregate} function was really designed for \code{ts}
  objects, not \code{tis} objects which may or may not meet the
  assumptions embedded in the function. The \code{convert} function is
  better suited for \code{tis} series.

  \code{aggregate.tis} calls \code{as.ts} on it's \code{x} argument,
  then passes that and all other arguments on to \code{aggregate.ts} and
  then turns the result back into a \code{tis} series. If there is a
  local version of \code{aggregate.ts} that can be found by
  \code{exists("aggregate.ts", envir = globalenv())},
  it will be used in preference to the function in \code{package:stats}.
}
\value{
  the \code{tis} object returned by \code{as.tis()} called on the
  \code{ts} object returned by \code{\link{aggregate.ts}}.
}
\seealso{
  \code{\link{apply}}, \code{\link{lapply}}, \code{\link{tapply}},
  \code{\link{aggregate}}, and \code{\link{convert}}.
}
\examples{
z <- tis(1:24, start = latestJanuary()) ## a monthly series
aggregate(z, nf = 4, FUN = mean)        ## quarterly average
aggregate(z, nf = 1, FUN = function(x) x[length(x)]) ## December is annual level
}
\keyword{ts}
