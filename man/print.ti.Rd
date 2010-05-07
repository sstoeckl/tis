\name{print.ti}
\alias{print.ti}
\title{ Print a Time Index }
\description{
  Print method for time index.
}
\usage{
\method{print}{ti}(x, class = TRUE, ...)
}
\arguments{
  \item{x}{	a time index }
 \item{class}{
   if(\code{TRUE} (the default), prints \code{class:  ti} at the end.
 }
 \item{\dots}{
   additional arguments that may be passed along to generic \code{print}
   function. See details.
 }
}
\details{
  This simply calls
  \code{print(as.character(x), quote = FALSE, ...)}.
}
\seealso{ \code{\link{format.ti}}}
\examples{
print(today(), class = FALSE)
## compare to
today()
}
\keyword{ chron }
