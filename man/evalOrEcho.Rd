\name{evalOrEcho}
\alias{evalOrEcho}
\title{ Evaluate an Argument If Possible }
\description{
  This function parses, evaluates and returns the string given as its
  first argument. If it can't, the argument itself is returned. Use of
  \code{evalOrEcho} to process arguments inside a function can make for
  more flexible code.
}
\usage{
evalOrEcho(x, resultMode = NULL, n = 0)
}
\arguments{
  \item{x}{
	a string or other object to attempt to parse and evaluate.
  }
  \item{resultMode}{
	a string or NULL.  If non-NULL, the evaluation of \code{x} is
	considered to have failed if the resulting object is not of this
	mode. 
  }
  \item{n}{
	parent generations to go back.  The evaluation is attempted in the
	enviroment specified by \code{parent.frame(n)} (of the caller).
  }
}
\details{
  Using this function inside another function to process some of its
  arguments can be very useful.  For example, \code{tisPlot} has a
  number or arguments that specify text labels for headers, subheaders,
  footnotes, axis labels, and so on.  One of those arguments is
  \code{sub}, which specifies the subheader.  By doing this:

  \code{sub <- evalOrEcho(sub, resultMode = "character")}

  \code{tisPlot} can handle the \code{sub} argument given in any of
  these forms:
  \enumerate{
	\item \code{sub = "This is a simple subtitle"}.
	\item \code{sub = c("this is a two", "line subtitle")}.
	\item \code{sub = 'c("this is another", "two line subtitle")'}.
  }
}
\value{
  If \code{x} is successfully parsed and evaluated, and its mode matches
  \code{resultMode} (if supplied), the resulting object is returned.
  Otherwise, \code{x} itself is returned.
}
\seealso{ \code{\link{try}}}
\keyword{ programming }
