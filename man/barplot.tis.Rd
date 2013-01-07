\name{barplot.tis}
\alias{barplot.tis}
\title{ Barplot for Time Indexed Series }
\description{
  This is \code{barplot} for \code{tis} objects. If the first argument
  to \code{barplot} is a \code{tis} object, this function is called.
  There may be more than one \code{tis} argument. See details below.
}
\usage{
\method{barplot}{tis}(height, \dots)
}
\arguments{
  \item{height}{ a \code{tis} object. There may, however, be more than
  just one \code{tis} argument in the call.  
  }
  \item{\dots}{ arguments passed on to \code{barplot.default}.}
}
\details{
  \code{barplot.tis} constructs a call to \code{\link{barplot2}}.
  The \code{tis} arguments, including but not limited to \code{height},
  are pulled out and used to construct a \code{height} argument for the
  constructed call, and the \code{width} argument will be calculated as
  described below.

  If the series sent in are multivariate, i.e., have multiple columns,
  the constructed \code{height} argument will also have multiple
  columns, which is how \code{barplot} does stacked bar charts.
  If you supply several multivariate series to
  \code{barplot.tis}, all of the series must have the same number of columns.

  If the \code{beside} argument is supplied in the \dots list, the
  \code{width} argument will be set to \code{d/(NC + 0.5)}, where
  \code{d} is the mean difference in decimal time units (where one year
  = 1, a quarter = 0.25, and so on) of the series observation times,
  \code{NC} is the number of columns in the series arguments, and 
  \code{space} will be set to \code{c(0, 0.5)}.  The effect of
  all this will be to make the total width of the barplot match the
  length (in years) of the series plotted. When combined with the
  calculated \code{x.offset} described below, this will make the plot
  align correctly on the time axis.  However, note that the alignment
  will only really be correct if all of the series plotted have the same
  frequency, as the underlying \code{barplot.default} forces each group
  of bars to have the same width when \code{beside = TRUE}.

  If the series being plotted are of different frequencies, you should
  not set \code{beside}, leaving it at the default value of
  \code{FALSE}. This will cause the widths of the bars for each series
  to be inversely proportional to the series frequencies, and the
  individual observations will align correctly on the time axis. 
  
  \code{barplot.tis} finds the earliest starting date of the
  \code{tis} arguments and shifts the plot rightward along the x-axis by
  that amount, which aligns the first bar with its start date. That is,
  if \code{gdp} is the \code{tis} argument with the earliest start date of all
  the series being plotted, the plot is shifted rightward to make the
  first bar align with \code{time(start(gdp))}. 
  
  You can use \code{tisPlot} to draw axes, axis labels, and tick marks
  for a \code{barplot}.  First call \code{tisPlot} with the series
  you want to plot, and other arguments set to create the range, axes,
  labels, tick marks and so on that you want, but set \code{color = 0}
  to make the series lines invisible. Then call \code{barplot} with the
  series and additional \code{barplot} arguments, but set the \code{add}
  argument to \code{add = TRUE}. This adds the barplot, without axes, to
  the existing tisPlot.

}
\value{
  same as \code{barplot.default}.
}
\seealso{ \code{\link{barplot}}, \code{\link{barplot2}}}
\keyword{ ts }
