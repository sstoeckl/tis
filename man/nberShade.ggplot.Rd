\name{nberShade.ggplot}
\alias{nberShade.ggplot}
\title{Plotting NBER Recesssions in the ggplot graphics system}
\description{
  \code{nberShade.ggplot} is a method for shading recession areas
  in a plot of class \code{ggplot}.  It calls
  \code{nberDates} to get x coordinates for the areas to be shaded
  and then passes those coordinates along with its own arguments to
  \code{\link[ggplot2:geom_rect]{geom_rect}} to do the shading.  It may also draw a vertical
  line at the appropriate location if \code{openShade} is false and
  its \code{xrange} is of an appropiate size.
}
\usage{
\method{nberShade}{ggplot}(gg = ggplot2:::last_plot(), fill = "grey80", color = NA, size = 0.5, 
                        alpha = 0.5, xrange = NULL, openShade = TRUE, ...) 
}
\arguments{
  \item{gg}{A \code{ggplot} object that contains a mapping of \code{y} to its \code{data}.
            The default is to use the last \code{ggplot} object that was plotted.}
  \item{fill}{color to shade recessionary periods, passed to \code{geom_rect}. }
  \item{color}{The default (\code{NA}) omits borders on the shaded
  regions.  Alternatively, specify a border color.  Passed to \code{geom_rect}}
  \item{size}{A number specifying how thick should the border lines be.  Passed to \code{geom_rect}}
  \item{alpha}{A number specifying how transparent the shaded regions should be.  Passed to \code{geom_rect}}
  \item{xrange}{Horizontal range over which recession shading should be
	drawn.  The default value \code{NULL} uses the entire range of the
	recession dates.  \code{xlim} and \code{scale_x_date} are convenient
        \code{ggplot2} functions to afterwards restrict the plot to a more sensible range.
  }
  \item{openShade}{governs how \code{nberShade} handles the case where
        the last row of the matrix returned by
	\code{nberDates} has an \code{NA} in the "End" column, indicating
	that the end date of the most recent recession is not yet known.
        If \code{TRUE} and \code{xrange} has been specified and extends far enough 
        into the future it will replace the \code{NA} with \code{xrange[2]}.
        Else if \code{openShade} is \code{TRUE} and \code{xrange} is \code{NULL} then it will 
        replace the \code{NA} with \code{Sys.Date()}.  If \code{openShade} is \code{FALSE} then 
        \code{nberShade} will draw a vertical line at the start of the
        most recent recession.}
	  \item{\dots}{not used, present only to match generic}
}
\value{
    Returns a \code{ggplot} object.
}
\references{
  National Bureau of Economic Research. 
  http://www.nber.org. 
}
\note{
  Recessions are dated by the Business Cycle Dating Committee of the
  National Bureau of Economic Research. 
}
\seealso{ \code{\link[ggplot2:geom_rect]{geom_rect}}, \code{\link{nberDates}}, \code{\link{nberShade}}}
\keyword{ aplot }
\examples{
    \dontrun{
        require("ggplot2")
        ggp <- ggplot(aes(x=date, y=100*unemploy/pop), data=economics)

        nberShade( ggp ) + xlim(as.Date("1967-01-01"), Sys.Date()) + 
        opts(legend.position="none") + geom_line() + theme_bw()

        nberShade( ggp, fill = "yellow", color = "pink",                      
                xrange = c("1969-01-01", "2008-02-01"), openShade = FALSE) + 
        opts(legend.position="none") + geom_line(size=1) + theme_bw()
    }
}
