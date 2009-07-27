\name{fortify.tis}
\alias{fortify.tis}
\title{Fortify a tis object}
\description{A fortify method for tis objects }
\usage{
    \method{fortify}{tis}(x, offset = 0.5, dfNames=ifelse(is.null(dim(x)), as.character(substitute(x)), NA))
} 
\arguments{
  \item{x}{A \code{tis} object of time series}
  \item{offset}{ A number between 0 and 1 specifying where in the period of time
    represented by the 'ti(x)' the points should eventually be plotted in \code{ggplot2}.
    'offset = 0' gives the beginning of the period and 'offset
    = 1' the end of the period, 'offset = 0.5' the middle of the period, and
    so on.  For example if \code{x} is a tis object of quarterly time series and \code{offset = 0.5}
    then the resulting plotted points would fall in the middle of each quarter.  
    \code{offset} is passed on to \code{POSIXct(ti(x), offset=offset)} 
    and used to create the field \code{date} in the resulting data frame.
  }
  \item{dfNames}{ A character vector of the names for the \code{tis} objects contained in \code{x}. Defaults to the name of the \code{tis} object in the univariate case and the column names of the \code{tis} object in the multivariate case.}
}
\details{
  This function turns a \code{tis} object into a data frame containing the original time series plus a field of dates, called \code{date}
  and adjusted by an \sQuote{offset},
  so that the time series can be more easily plotted with \code{ggplot2}.
}
\seealso{\link[ggplot2:fortify]{fortify}}
\author{Trevor Davis}
\examples{
    if(require("ggplot2")) {
             # Examples of plotting tis series with ggplot2
             require("datasets")

             # univariate example
             num_discoveries <- as.tis(discoveries)
             ggplot(data = fortify(num_discoveries, offset=0)) + 
                    geom_line(aes(x=date, y=num_discoveries)) + 
                    scale_x_date(major="10 years")
             
             # multivariate example using the "melt trick"
             Seatbelts.tis <- as.tis(Seatbelts[ , c("drivers", "front", "rear")])
             Seatbelts.df <- fortify(Seatbelts.tis)
             Seatbelts.dfm <- melt(Seatbelts.df, id.var = "date", variable_name="type")
             qplot( date, value, data = Seatbelts.dfm, geom="line", 
                    group=type, colour=type, linetype=type ) + 
                 geom_vline(xintercept=as.numeric(as.Date("1983-01-31")), 
                    colour="black", linetype="dashed") +
                 ylab("Road Casulties in the UK")
    }
}
