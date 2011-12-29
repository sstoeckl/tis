\name{setColors}
\alias{setColors}
\title{Set the Graphics Palette and Default Colors}
\description{Sets the color palette (used when a \code{col=} has a
  numeric index) to a given vector of \code{colors}, and also sets the
  graphics parameters (see \code{\link{par}}) \code{col},
  \code{col.axis}, \code{col.lab}, \code{col.main}, \code{col.sub} and
  \code{fg} to \code{colors[1]}. 
}
\usage{
setColors(colors)
}
\arguments{
  \item{colors}{a character vector}
}
\value{The new color palette.}
\seealso{\code{\link{palette}}, \code{\link{par}}}
\keyword{color}
