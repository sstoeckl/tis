\name{frColors}
\alias{frColors}
\title{ FRB Color Palettes }
\description{ Returns a vector of 11 color names that can be used as a
  palette suitably for a light or dark background.
}
\usage{
frColors(dark = FALSE)
}
\arguments{
  \item{dark}{If \code{TRUE} return a set of colors suitable for drawing
  on a black background.  The first color is "white".  If \code{FALSE}
  (the default), return colors suitable for drawing on a white
  background, with the first color set to "black".}
}
\value{A vector of 11 color names.
}
\author{ Jeff Hallman }
\seealso{ \code{\link{setColors}}, \code{\link{colors}}}
\examples{
plot.new()
for(i in 1:11) abline(h = (i-0.5)/11, lwd = 2, lty = 1, col = frColors()[i])
plot.new()
for(i in 1:11) abline(h = (i-0.5)/11, lwd = 2, lty = 1, col = frColors(dark = TRUE)[i])
}
\keyword{color}
\keyword{device}
