\name{nberShade}
\alias{nberShade}
\alias{nberDates}
\alias{nber.xy}
\alias{romerLines}
\title{Plotting NBER Recesssions}
\description{
  \code{nberDates} returns a matrix with two columns of yyyymmdd dates
  giving the Start and End dates of recessions fixed by the NBER.
  
  \code{nber.xy} returns a list of x and y coordinates that can be fed
  to \code{\link{polygon}} to draw NBER shadings on the current plot.
  If the last row from \code{nberDates()} has a Start entry but no End
  entry, the returned list will also include a vLine element that gives
  the x coordinate of the last Start.
  
  \code{nberShade} shades recession areas on the current plot.  It calls
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
nberShade(col = grey(0.8), border = FALSE, xpd = FALSE, ...)
nberDates()
nber.xy()
romerLines()
}
\arguments{All passed along to \code{polygon}:
  \item{col}{color to shade recessionary periods }
  \item{border}{the default (\code{FALSE}) omits borders on the shaded
  regions. \code{TRUE} draws borders in the foreground color.
  Alternatively, specify a border color.}
  \item{xpd}{should clipping take place? }
  \item{\dots}{other args passed to \code{\link{polygon}}}
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
\author{Jeff Hallman}
\note{
  Recessions are dated by the Business Cycle Dating Committee of the
  National Bureau of Economic Research. 

  The Romer dates are October 1947, September 1955, December 1968,
  April 1974, August 1978, October 1979 and December 1988.
}
\seealso{ \code{\link{polygon}}}
\keyword{ aplot }
