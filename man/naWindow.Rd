\name{naWindow}
\alias{naWindow}
\title{Exclude NA and Zero Observations}
\description{Windows a \code{tis} or \code{ts} time series to cut off leading and trailing
  NA (and optionally zero) observations.
}
\usage{
naWindow(x, union = F, zero = F)
}
\arguments{
  \item{x}{a \code{tis} or \code{ts} time series}
  \item{union}{see details below}
  \item{zero}{if \code{TRUE}, chop off leading and trailing zeroes also}
}
\details{
  For multivariate (multiple columns) series and \code{union = TRUE}, a
  row of \code{x} is considered to be NA if and only if all entries in
  that row are NA. If \code{union = FALSE} (the default), a row is
  considered to be NA if any of its entries is NA.

  if \code{zero} is \code{TRUE}, the function chops off leading and
  trailing observations that are either \code{NA} or zero.  Otherwise,
  it chops only \code{NA}s.
}
\value{A copy of \code{x} with leading and trailing NA observations deleted.
}
\seealso{ \code{\link{window}}}
\keyword{ ts }
