\name{nberShade}
\alias{nberShade}
\alias{nberShade.default}
\alias{nberDates}
\alias{nber.xy}
\alias{romerLines}
\title{Plotting NBER Recesssions}
\description{
  \code{nberDates} returns a matrix with two columns of yyyymmdd dates
  giving the Start and End dates of recessions fixed by the NBER.
  
  \code{nber.xy} returns a list of x and y coordinates that can be fed
  to \code{\link{polygon}} to draw NBER shadings on the current plot.
  It executes \code{get("nberDates", pos = 1)()}  to do so.  This means
  that if you have defined a local version of \code{nberDates}, it will
  be used rather than the one supplied by the \pkg{tis} package.
  If the last row from \code{nberDates()} has a Start entry but an "NA"
  End entry and \code{openShade} is \code{FALSE}, the returned list will
  not have coordinates for the last row, but will instead include a
  vLine element that gives the x coordinate of the last Start. If
  \code{openShade} is \code{TRUE} (the default), the list includes x
  and y coordinates for the most recent recession, using the second
  element of the horizontal range determined by the \code{xrange}
  parameter as its end time.
  
  \code{nberShade} is a generic method for shading recession areas
  on the current plot.  The default version calls
  \code{nber.xy} to get x and y coordinates for the areas to be shaded
  and then passes those coordinates along with its own arguments to
  \code{\link{polygon}} to do the shading.  It also draws a vertical
  line at the appropriate location if the list returned by
  \code{nber.xy} has a vLine element.

  \code{romerLines} draws vertical lines on the current plot at the
  "Romer and Romer" dates when monetary policy is said to have become
  contractionary.
}
\usage{
\method{nberShade}{default}(col = grey(0.8), border = FALSE, xpd = FALSE,
          xrange = NULL, openShade = TRUE, ...)
nberDates()
nber.xy(xrange = NULL, openShade = TRUE)
romerLines()
}
\arguments{All passed along to \code{polygon}:
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
  \item{openShade}{governs how \code{nber.xy} and consequently
  \code{nberShade} handle the case where the last row of the matrix returned by
	\code{nberDates} has an \code{NA} in the "End" column, indicating
	that the end date of the most recent recession is not yet known.
	See the description above for details.} 
}
\value{
  As described above, \code{nber.xy} returns a list.  The other
  functions described do not return anything useful.
}
\references{
  Christina D. Romer and David H. Romer. 1989. "Does Monetary
  Policy Matter? A New Test in the Spirit of Friedman and Schwartz."
  NBER Macroeconomics Annual 4: 121-170.
  
  Christina D. Romer and David H. Romer. 1994. "Monetary Policy
  Matters." Journal ofMonetary Economics 34 (August): 75-88.

  National Bureau of Economic Research. 
  http://www.nber.org. 
}
\note{
  Recessions are dated by the Business Cycle Dating Committee of the
  National Bureau of Economic Research. 

  The Romer dates are October 1947, September 1955, December 1968,
  April 1974, August 1978, October 1979 and December 1988.
}
\seealso{ \code{\link{polygon}}, \code{\link{nberShade.ggplot}}}
\keyword{ aplot }
\examples{
    require("datasets")
    plot(presidents, type='n', ylab="Presidents approval rating")
    nberShade()
    lines(presidents)
}
