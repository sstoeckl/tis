\name{mergeSeries}
\alias{mergeSeries}
\title{ Merge Time Indexed Series }
\description{
  Merge two time-indexed series using either the levels or the first differences
  of the second series where the series overlap.
}
\usage{
mergeSeries(x, y, differences = FALSE, naLoses = FALSE)
}
\arguments{
  \item{x, y}{
	\code{tis} objects, or objects that can sensibly be coerced to
	\code{tis} by \code{as.tis}.
  }
  \item{differences}{
	if \code{TRUE}, the first differences of series are merged, and then
	cumulatively summed. The default is \code{FALSE}.
  }
  \item{naLoses}{
	if \code{TRUE}, \code{NA} values in \code{y} do not overwrite
	corresponding values in \code{x}.
  }
}
\details{
  \code{x} and \code{y} must have the same \code{tif} (ti frequency),
  and the same number of column (if they are multivariate). 
}
\value{
  A \code{tis} object series with start and end dates that span those of
  \code{x} and \code{y}.  Where the series overlap, values from \code{y}
  are used, except that if \code{naLoses} is \code{TRUE}, \code{NA}
  values from \code{y} do not overwrite non-\code{NA} values in \code{x}.
}
\seealso{ \code{\link{cbind.tis}} }
\keyword{ ts }
