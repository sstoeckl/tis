\name{tierChart}
\alias{tierChart}
\alias{adjustableTierChart}
\title{
Create tier charts 
}
\description{
A tier chart plots several years' observations of a series against the
times of year in which they were observed.  Useful for seeing seasonal
patterns in daily, weekly and irregularly-spaced data. 
}
\usage{
tierChart(x,
          startMonth = latestJanuary(end(x)),
          nMonths = 4,
          nYears = 7,
          offsets = 0,
          padDays = 6,
          pch = "year",
          lty = "solid",
          lwd = 1.5,
          col = 1 + (n.y:1),
          type = "b",
          ylim = NULL,
          outlier.trim = 0,
          noTrimLastYear = TRUE,
          extendHorizontalTicks = TRUE,
          circles.ymd = NULL,
          circles.col = 6,
          circles.inches = 0.1,
          vlines.ymd = NULL,
          vlines.col = 2,
          vlines.lty = 4,
          vlines.lwd = 1.5,
          vlines2.ymd = NULL,
          vlines2.col = 3,
          vlines2.lty = "solid",
          vlines2.lwd = 2,
          hlines = NULL,
          hlines.col = 1,
          hlines.lty = 1,
          hlines.lwd = 1,
          tiPoints.1 = NULL,
          tiPoints.2 = NULL,
          pch.1 = "*",
          pch.2 = "+",
          col.1 = 2,
          col.2 = 3,
          nolegend = FALSE,
          main = deparse(substitute(x)),
          topleft.labels = NULL,
          topright.labels = NULL,
          legend.ncol = length(years),
          legend.bg = 0,
          timestamp = TRUE,
          topline = TRUE,
          vlines.periodEnd = TRUE,
          vlines.month = TRUE,
          midperiod = FALSE,
          lwdLastYear = 1.5,
          cex = 1.5,
          boxes = TRUE,
          \dots)

adjustableTierChart(x,
                    \dots,
                    edit = TRUE,
                    changes = numeric(0),
                    verbose = FALSE) 
}
\arguments{
  \item{x}{
    A monhly or higher frequency (such as weekly or daily) time indexed
    series (a \code{tis} object) or something that \code{as.tis} can
    convert to one.
  }
  \item{startMonth}{
    a monthly time index (\code{ti}) object indicating first month to
    show on plot. 
  }
  \item{nMonths}{
    number of months to show on plot.
  }
  \item{nYears}{
    number of years to include in the plot.
  }
  \item{offsets}{
    vector of day offsets for the years in descending order.  If
    \code{offsets[2]} is 3, for example, the points for the
    second-to-last year will be shifted forward 3 days on the plot.
    Negative numbers shift points back.
	If \code{length(offsets) < nYears}, zeroes will be appended to
	\code{offsets} to make it \code{nYears} long. 
  }
  \item{padDays}{
    number of extra days to plot before and after the requested months.
  }
  \item{pch}{
    plotting symbols to be drawn when plotting points.  If pch is a
    character string, such as "a1b2", the first year's points will be
    labeled "a", the second year's with "1", the third with "b", and so
    on.  Alternatively, pch can be a numeric vector giving the numbers of
    plotting symbols to use, as detailed in the documentation for
    \code{lines}.  The default string "year" uses the last digit of the
    year number for each point in that year.
  }
  \item{lty}{
    vector of line types.  The first element is for the first year, the second
    element for the second year, etc., even if lines are not plotted for all
    years. Line types will be used cyclically until all years are drawn. 
  }
  \item{lwd}{
    number specifying line width
  }
  \item{col}{
    vector of colors for the years, specified as numbers or color names.
  }
  \item{type}{
    character string, telling which type of plot
    (\code{"p"}, points; \code{"l"}, lines; \code{"b"}, both;
    \code{"n"}, none; or \code{"h"}, high-density) should be done for
    each year.  The first character of \code{type} defines the first
    year, the second character the second, etc. Characters in
    \code{type} are cycled through; e.g., \code{"pl"} alternately plots
    points and lines.  The default is "b". 
  }
  \item{ylim}{
    ylim is a vector of 2 numbers giving desired y-axis limits.  The
    actual limits on the plot will be the result of \code{pretty(ylim)}.
    If \code{ylim} is not set explicitly, the value of
    \code{outlier.trim} is used to calculate it such that the
    \code{c(outlier.trim, 1 - outlier.trim)} quantiles of the
    plotted points fall within the calculated \code{ylim} before it is
    "prettied".  
  }
  \item{outlier.trim}{
    see \code{ylim} above
  }
  \item{noTrimLastYear}{
    if \code{TRUE} (the default), \code{outlier.trim} is effectively
    set to zero for the most recent year, i.e., the y-axis limits will
	be expanded, if necessary, to insure that all of the points plotted
	for the most recent year fit on the plot.
  }
  \item{extendHorizontalTicks}{
	if \code{TRUE} (the default), extend the horizontal ticks with a
	dotted line all the	way across the chart.
  }
  \item{circles.ymd}{
    draws circles around the plotted points corresponding to these ymd
    dates. The colors and sizes of the circles are given by
    \code{circles.col} and \code{circles.inches}. 
  }
  \item{circles.col}{
    see \code{circles.ymd} above.
  }
  \item{circles.inches}{
    see \code{circles.ymd} above.
  }
  \item{vlines.ymd}{
    numeric vector of yyyymmdd dates, draws vertical lines of type
    \code{vlines.lty} width \code{vlines.lwd} and colors
    \code{vlines.col} at the dates given.
  }
  \item{vlines.col}{
    see \code{vlines.ymd} above
  }
  \item{vlines.lty}{
    see \code{vlines.ymd} above
  }
  \item{vlines.lwd}{
    see \code{vlines.ymd} above
  }
  \item{vlines2.ymd}{
    numeric vector of yyyymmdd dates, draws vertical lines of type
    \code{vlines2.lty} width \code{vlines2.lwd} and colors
    \code{vlines2.col} at the dates given. 
  }
  \item{vlines2.col}{
    see \code{vlines2.ymd} above
  }
  \item{vlines2.lty}{
    see \code{vlines2.ymd} above
  }
  \item{vlines2.lwd}{
    see \code{vlines2.ymd} above
  }
  \item{hlines}{
    numeric vector, draws horizontal lines of type \code{hlines.lty} width
    \code{hlines.lwd} and colors \code{hlines.col} at the locations given.
  }
  \item{hlines.col}{
    see \code{hlines} above
  }
  \item{hlines.lty}{
    see \code{hlines} above
  }
  \item{hlines.lwd}{
    see \code{hlines} above
  }
  \item{tiPoints.1}{
    a \code{ti} object specifying dates for which the corresponding
    points will be marked with the characters in \code{pch.1} in 
    colors \code{col.1}.
  }
  \item{pch.1}{
    see \code{tiPoints.1} above
  }
  \item{col.1}{
    see \code{tiPoints.1} above
  }
  \item{tiPoints.2}{
    a \code{ti} object specifying dates for which the corresponding
    points will be marked with the characters in \code{pch.2} in 
    colors \code{col.2}.
  }
  \item{pch.2}{
    see \code{tiPoints.2} above
  }
  \item{col.2}{
    see \code{tiPoints.2} above
  }
  \item{nolegend}{
    if \code{T} do not plot a legend
  }
  \item{main}{
    character string giving main title for the chart.
  }
  \item{topleft.labels}{
    strings to place in left corner of top margin
  }
  \item{topright.labels}{
    strings to place in right corner of top margin
  }
  \item{legend.ncol}{
    number of columns to use for legend.  Has no effect if
    \code{nolegend} is \code{T}.
  }
  \item{legend.bg}{
    background color for legend
  }
  \item{timestamp}{
    if \code{T} put a timestamp in upper right corner of top margin
  }
  \item{topline}{
    if \code{T} (the default) draw an axis line across the top of the plot
  }
  \item{vlines.periodEnd}{
    if \code{T} (the default) draw a light vertical line at each period end of
    the most recent year plotted.
  }
  \item{vlines.month}{
    if \code{T} (the default) draw a light vertical line at month boundaries
  }
  \item{midperiod}{
    if \code{T} draw the point for each period on the middle day of the
    period.  If \code{F} (the default) draw points on the last day of
    the period.
  }
  \item{lwdLastYear}{
    line width for the last year plotted.
  }
  \item{boxes}{
    if \code{T} (the default) add scroll arrow and PrintMe boxes to the
    plot for use by \code{adjustableTierChart}.
  }
  \item{cex}{
    numeric character expansion factor for the characters denoting the
    points on the plot.
  }
  \item{\dots}{
    for \code{adjustableTierChart}, arguments to be passed on to
    \code{tierChart}.  For \code{tierChart}, \dots denote arguments to
    be passed to \code{matplot}, which does the actual plotting. 
  }
  \item{edit}{
	if \code{T}, the points on the plot are editable. Clicking above or
	below a point draws an arrow from the point to it desired new
	location.
  }
  \item{changes}{
	used internally by the function to remember what points have been
	moved thus far while scrolling.  This argument should never be set
	by the user.
  }
  \item{verbose}{
    if \code{T}, \code{adjustableTierChart} is chattier about what it is
    doing.
  }
}
\value{
  \code{tierChart} invisibly returns a list with the following components:
  
  \item{px}{
    a matrix with \code{nYears} columns containing the x coordinates of
    the points for each year.
  }
  \item{py}{
    a matrix with \code{nYears} columns containing the y coordinates of
    the points for each year.
  }
  \item{ymd}{
    matrix of yyyymmdd dates corresponding to x coordinates in \code{px}
  }
  \item{index}{
    matrix giving positions of the elements of the y matrix in the
    original x series, that is, x[index[i,j]] == y[i,j]
  }
  \item{lBox}{
	vector of 4 numbers giving the c(left, bottom, right, top) bounds of
	the scroll arrow box in the upper left corner of the plot.
  }
  \item{rBox}{
	vector of 4 numbers giving the c(left, bottom, right, top) bounds of
	the scroll arrow box in the upper right corner of the plot.
  }
  \item{printBox}{
	vector of 4 numbers giving the c(left, bottom, right, top) bounds of
	the print box in the adjoining the left scroll box.
  }
  \item{startMonth}{
	the input argument of the same name
  }
  \item{nMonths}{
	number of months wide the plot is
  }
  \item{nYears}{
	number of years plotted
  }

  
  If the input argument code{boxes} is \code{F}, the \code{lBox},
  \code{rBox} and \code{printBox} elements of the list will not be
  present.


  \code{adjustableTierChart} returns the edited input series \code{x} as
  a \code{tis} object, with an additional \code{startMonth} attribute. 
}
\section{Side Effects}{
  a tier chart is drawn on the current graphics device.
}
\details{
  A tier chart shows seasonal patterns in data by superimposing the data
  from the same months of several years onto a single plot.  Tier charts
  can be used both to present a view of a time series and to graphically
  edit its values by pointing and clicking.
  
  For most purposes, \code{adjustableTierChart} is preferred to
  \code{tierChart}, since the former presents a chart that can be
  edited, scrolled and printed via mouse clicks, while the latter simply
  draws a single chart and returns.  However, \code{adjustableTierChart}
  requires user interaction, which may make it unsuitable for some uses.

 When \code{adjustableTierChart} is called, it draws on the current
 graphics device and then waits for mouse clicks to occur.  A left mouse
 button click on one of the scroll arrows changes the display to show
 adjacent months, while a left mouse click on the \bold{PrintMe} box
 causes the current plot to be copied to the printer.  Left mouse clicks
 in the data area of the plot are used to edit the values of the time
 series.  Arrows are drawn from the current data points to the mouse
 location to show where the new data values will be.

 A middle mouse button click causes \code{adjustableTierChart} to
 return.  Closing the graphics window via the windowing system (e.g.,
 clicking on the window's \bold{X} button) has the same effect.
 \emph{Until \code{adjustableTierChart} is told to return, the entire R
 process will appear to be frozen.} It isn't actually frozen, it's just
 waiting for mouse input. Use \code{tierChart} instead if no user
 interaction is desired.
}
\seealso{
  \code{\link{monthplot}} for a nice way to look at seaonality in monthly
  data.
}
\examples{
\dontrun{
tierChart(m1.w)   ## January - April of 7 most recent years

tierChart(m1.w, startMonth=1, nMonths = 12)  ## Tier chart for entire year

tierChart(m1.w, type="l", lty=1)   ## same as first example, but with
                                   ## solid lines and no plotting symbols

xe <- adjustableTierChart(x)    ## xe will be edited version of x
}
}
\keyword{hplot}
\keyword{ts}
