\name{ymdShade}
\alias{ymdShade}
\alias{ymdXy}
\title{Shading Date Ranges}
\description{
  \code{ymdXy} returns a list of x and y coordinates that can be fed
  to \code{\link{polygon}} to draw NBER shadings on the current plot.
  If the last row of the \code{ymds} argument has a Start entry but an "NA"
  End entry and \code{openShade} is \code{FALSE}, the returned list will
  not have coordinates for the last row, but will instead include a
  vLine element that gives the x coordinate of the last Start. If
  \code{openShade} is \code{TRUE} (the default), the list includes x
  and y coordinates for the last row of \code{ymds}, using the second
  element of the horizontal range determined by the \code{xrange}
  parameter as its end time.
  
  \code{ymdShade} shades date ranges on the current tisPlot. 
  on the current plot.  It calls \code{ymdXy} to get x and y
  coordinates for the areas to be shaded and then passes those
  coordinates along with its own arguments to \code{\link{polygon}} to
  do the shading.  It also draws a vertical line at the appropriate
  location if the list returned by \code{ymdXy} has a vLine element.
}
\usage{
ymdShade(ymds, col = grey(0.8), border = FALSE, xpd = FALSE,
          xrange = NULL, openShade = TRUE, ...)
ymdXy(ymds, xrange = NULL, openShade = TRUE)
}
\arguments{All but \code{ymds} are passed along to \code{polygon}:
  \item{ymds}{a matrix of yyyymmdd dates with two columns named 'Start'
  and 'End' that specifies the date ranges to be shaded.
  \code{nberDates()} produces a suitable matrix.}
  \item{col}{color to shade recessionary periods }
  \item{border}{the default (\code{FALSE}) omits borders on the shaded
  regions. \code{TRUE} draws borders in the foreground color.
  Alternatively, specify a border color.}
  \item{xpd}{should clipping take place? }
  \item{\dots}{other args passed to \code{\link{polygon}}}
  \item{xrange}{horizontal range over which recession shading should be
	drawn.  The default value \code{NULL} uses the entire range of the
	plot. Note however the \code{tisPlot} uses the range of the data,
	which will generally differ the plot range unless the \code{tisPlot}
	parameters \code{leftExpandBy} and \code{rightExpandBy} are zero.
	You can force \code{tisPlot} to use the plot range by setting the
	parameter \code{nberArgs = list(xrange = NULL)}.
  }
  \item{openShade}{governs how \code{ymdXy} and consequently
    \code{ymdShade} handle the case where the last row of the \code{ymds}
    matrix has an \code{NA} in the "End" column, indicating that the end
    date of the most recent shaded period is not known.}
}
\value{
  As described above, \code{ymdXy} returns a list.  \code{ymdShade}
  does not return anything useful.
}
\seealso{ \code{\link{polygon}}}
\keyword{ aplot }
\examples{
    require("datasets")
    plot(presidents, type='n', ylab="Presidents approval rating")
    ymdShade(nberDates())
    lines(presidents)
}
