\name{tisLegend}
\alias{tisLegend}
\title{ Add a legend to a tisPlot or scatterPlot }
\description{ The plotting functions \code{tisPlot} and
  \code{scatterPlot} leave an object named \code{latestPlot} in the
  frame from which they were called. \code{tisLegend} uses that object
  to set legend arguments (which you can override) and sets reasonable
  defaults for other arguments. 
}
\usage{
tisLegend(..., xrel = 0.1, yrel = 0.1, xjust = 0, yjust = 1, boxType = "n", ncol = 1, cex = 1)
}
\arguments{
  \item{\dots}{optional arguments to be passed on to \code{\link{legend}}.
	These can include \code{x} and \code{y} arguments to position the
	legend, or a list with components named \code{x} and \code{y}, such as
	the list returned by \code{locator(1)}.}
  \item{xrel, yrel}{Optional numbers between 0 and 1 to specify placement
	relative to the boundaries of the plot.}
  \item{xjust, yjust, ncol}{ passed along to \code{\link{legend}}}
  \item{boxType}{passed along as \code{bty} to \code{\link{legend}}}
  \item{cex}{ gets multiplied by the \code{cex} from \code{latestPlot}
	and then passed on to \code{\link{legend}}}
}
\details{This function is not strictly necessary, in that you could just
  call \code{\link{legend}} directly.  \code{tisLegend} makes things a
  bit easier, however, by using the same argument names as
  \code{tisPlot} and \code{scatterPlot} to specify \code{color},
  \code{lineType}, \code{plotChar} and \code{boxType}, rather than the
  less intuitive \code{col}, \code{lty}, \code{pch} and \code{bty}
  names.  The \code{xrel} and \code{yrel} arguments provide an
  alternative way to specify legend placement, one that is used by the
  ChartMaker program.
}
\value{a list of the arguments that were sent on to \code{legend}, with
  class "tisLegend"}
\seealso{ \code{\link{legend}}, \code{\link{tisPlot}}, \code{\link{scatterPlot}}}
\keyword{ aplot }
