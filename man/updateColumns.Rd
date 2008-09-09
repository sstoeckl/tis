\name{updateColumns}
\alias{updateColumns}
\alias{updateList}
\title{ Update lists and time series }
\description{
  \code{updateList} compares the names of \code{oldlist} and
  \code{newlist}, deletes the matching elements from a copy of
  \code{oldlist}, then returns the result of concatenating that list
  with \code{newlist}.

  \code{updateColumns} updates columns of first series from same-named
  columns of second series using \code{mergeSeries()}.  If second series
  has columns with names not found in colnames of first series, those
  columns are \code{cbind()} 'ed onto first series. 
}
\usage{
updateColumns(oldmat, newmat)
updateList(oldlist, newlist)
}
\arguments{
  \item{oldmat}{a multivariate \code{tis} series}
  \item{newmat}{a multivariate \code{tis} series }
  \item{oldlist}{a list}
  \item{newlist}{a list}
}
\value{
  \code{updateList} returns the updated list.

  \code{updateColumns} returns a multivariate \code{tis} series
}
\seealso{ \code{\link{tis}},
  \code{\link{mergeSeries}},
  \code{\link{cbind.tis}}}
\keyword{ list }
\keyword{ ts }
