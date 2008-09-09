\name{plotWindow}
\alias{plotWindow}
\title{Set up Coordinates for Graphics Window }
\description{This is a shortcut call to \code{plot.window} that has some
  of the parameters set to different defaults than the usual values. 
}
\usage{
plotWindow(xlim, ylim, log = "", asp = NA, xaxs = "i", yaxs = "i", ...)
}
\arguments{
  \item{xlim, ylim}{numeric of length 2, giving the x and y coordinates
    ranges.}
  \item{log}{character; indicating which axes should be in log scale.}
  \item{asp}{numeric, giving the \bold{asp}ect ratio y/x.}
  \item{xaxs}{style of axis interval calculation for the x-axis}
  \item{yaxs}{style of axis interval calculation for the y-axis}
  \item{\dots}{further graphical parameters as in \code{\link{par}}.}
}
\seealso{\code{\link{plot.window}}}
\keyword{ aplot }
