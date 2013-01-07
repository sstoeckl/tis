\name{nberShade}
\alias{nberShade}
\alias{nberShade.default}
\alias{nberDates}
\alias{romerLines}
\title{Plotting NBER Recesssions}
\description{
  \code{nberDates} returns a matrix with two columns of yyyymmdd dates
  giving the Start and End dates of recessions fixed by the NBER.
  
  \code{nberShade} is a generic method for shading recession areas
  on the current plot.  The default version calls
  \code{nberDates()} to get a matrix of yyyymmdd dates and then passes
  those dates and all other arguments along to \code{ymdShade}.

  \code{romerLines} draws vertical lines on the current plot at the
  "Romer and Romer" dates when monetary policy is said to have become
  contractionary.
}
\usage{
\method{nberShade}{default}(...)
nberDates()
romerLines()
}
\arguments{
  \item{\dots}{args passed to \code{\link{ymdShade}}}
}
\value{
  \code{nberDates} returns the two column matrix of recession date
  ranges described above.
  Nothing useful is returned by the other functions.
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
