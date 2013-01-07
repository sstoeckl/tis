\name{tisPlot}
\alias{tisPlot}
\title{ Plot time indexed series (tis objects)}
\description{
  \code{tisPlot} is a function with dozens of options for creating
  high quality time series plots. Can be used with \code{screenPage}.
}
\usage{
tisPlot(..., 
        leftAxis = TRUE, plotType = "l",
        lineType = "solid", lineWidth = 1.5,
        plotChar = "*", dataCex = 1, color = 1,
        midPoints = TRUE, dropNA = FALSE, xOffset = 0,
        xAxisMin = NULL, xAxisMax = NULL, xExpandBy = 0.04,
        xTickFreq = "Auto", xTickSkip = 0,
        xUnlabeledTickFreq = "None", xUnlabeledTickSkip = 0,
        xMinorTickFreq = "None", xMinorTickSkip = 0,
        dateFormat = "Auto", xCex = 1,
        midLabels = FALSE, yearLabels = FALSE,
        xAxisLabel = NULL, xSpace = 4, log = FALSE,
        leftAxisMin = NULL, leftAxisMax = NULL, leftExpandBy = 0.04,
        leftTicks = 5, leftTickLocations = NULL,
        labelLeftTicks = FALSE, leftTickLabels = NULL,
        rightAxisMin = NULL, rightAxisMax = NULL, rightExpandBy = 0.04,
        rightTicks = 5, rightTickLocations = NULL,
        labelRightTicks = TRUE, rightTickLabels = NULL,
        yCex = 1, extendTopTick = TRUE,
        cex = 1,
        head = NULL, headAlign = 0.5, headCex = 1.5,
        sub = NULL, subCex = 0.85,
        leftTopLabel = NULL, rightTopLabel = NULL, topLabelAlign = 0,
        labCex = 1,
        leftInsideLabel = NULL, rightInsideLabel = NULL,
        innerLine = 0.5, innerOffset = 0.05, innerCex = 0.8,
        foot = NULL, footColor = "black", footAlign = 0,
        footCex = 0.8, footSpace = -1,
        tck = 0.03,
        axisWidth = 2,
        start = 0, end = 0,
        boxType = "u",
        leftMargin = -1, rightMargin = -1, topMargin = -1, bottomMargin = -1,
        nberShade = FALSE, shadeDates = NULL, shadeColor = "gray",
        shadeBorder = FALSE, polyArgs = list())
}
\arguments{
  \item{\dots}{
	any number of univariate or multivariate \code{tis} series to be plotted.
	Non-\code{tis} arguments will be converted by \code{as.tis}.
  }
  \item{leftAxis}{
	logical. \code{leftAxis[i] = TRUE} means plot the i'th series
	against the left axis, otherwise plot it against the right axis.
  }
  \item{plotType}{
	type of plot desired. Values are \code{"p"} for points, \code{"l"}
	for lines, \code{"b"} for both points and lines (lines miss the
	points), and \code{"o"} for overlaid points and lines.
  }
  \item{lineType}{
	character or numeric vector specifying the line type for each
	series. The default is \code{1:nSeries}, where \code{nSeries} is the
	number of series being plotted.  Normally type \code{1} is solid,
	\code{2} is dotted, 3 and up are a mix of dots and dashes. 
  }
  \item{lineWidth}{
	numeric vector of line widths for the series. The default value is
	\code{1.5}. 
  }
  \item{plotChar}{
	vector of characters (or numbers for plotting symbols -- see the
	help for \code{\link{points}} for details) to be used for plotting
	points. 
	Default is "*".
  }
  \item{dataCex}{
	numeric vector. \code{cex} times these numbers give the character
	expansion factor for the data points.  Default is 1.
  }
  \item{color}{
	character or numeric vector specifies color for each series. Default
	is 1, the device default foreground color.
  }
  \item{midPoints}{
	logical. \code{midPoints[i] = TRUE} aligns the data points of the i'th
	series with the middle day of the periods in which they fall,
	otherwise data points are aligned with the last day of their periods.
  }
  \item{dropNA}{ if \code{TRUE}, observations with \code{NA} values are
	dropped before calling \code{lines.default} to draw the lines and/or
	points on the plot.  See the details section of the help for
	\code{\link{lines.tis}} for why you might or might not want to do
	this.  The default is \code{FALSE}.
  }  
  \item{xOffset}{
	Shifts the data points for series[i] to the right by
	\code{xOffset[i]} days. This argument is replicated to be as long
	as the number of series. 
  }
  \item{xAxisMin}{
	minimum value of the x axis. If non-NULL, this overrides the
	calculation described in \code{xExpandBy}.
  }
  \item{xAxisMax}{
	maximum value of the x axis. If non-NULL, this overrides the
	calculation described in \code{xExpandBy}.
  }
  \item{xExpandBy}{
	calculate \code{xAxisMin} and \code{xAxisMax} by multiplicatively
	extending the data range in both directions by this amount.  Default
	value .04 extends the data range by 4\% in each direction.
  }
  \item{xTickFreq}{
	a string like the ones returned by \code{tifName}. This argument and
	\code{xTickSkip} jointly specify locations for labeled x axis ticks
	as follow: (i) find the \code{ti}'s of the given frequency that
	correspond to \code{xAxisMin} and \code{xAxisMax}, then (ii)
	including those as endpoints, generate a sequence of every
	\code{xTickSkip}'th \code{ti} between them.

	Two special strings can also be given.  "none" means no labelled
	tick marks, while "auto" tries (not always successfully) to come up
	with reasonable tick locations automatically.  "auto" also overrides
	any \code{xTickSkip} setting.

	The default is "auto".
  }
  \item{xTickSkip}{
	a number used with \code{xTickFreq} to specify location of labeled
	tick marks. The default is zero.
  }
  \item{xUnlabeledTickFreq}{
	same as \code{xTickFreq}, but for unlabeled major tick locations.
  }
  \item{xUnlabeledTickSkip}{
	same as \code{xTickSkip}, but for unlabeled major tick locations.
  }
  \item{xMinorTickFreq}{
	same as \code{xTickFreq}, but for minor tick locations.
  }
  \item{xMinorTickSkip}{
	same as \code{xTickSkip}, but for minor tick locations.
  }
  \item{dateFormat}{
  	format string for x axis date labels. See \code{\link{strftime}} for
  	formats.  Default value "auto" tries to come up with a reasonable
  	format automatically, for some bounded value of "reasonable". 
  }
  \item{xCex}{
	\code{cex} times this number gives the character expansion factor
	for the x-axis labels. Default is 1.
  }
  \item{midLabels}{
  	if \code{TRUE} (the default) x axis label are centered between the
  	ticks they label; if \code{FALSE} the label alignment depends on the
  	setting of \code{midPoints}. If \code{midPoints} is \code{TRUE},
  	the labels (which reflect the last day of the period) are centered
  	under the ticks, else they are (almost) right aligned with the
  	ticks.  If rotated labels are specified by nonzero
  	\code{xLabelRotationDegrees}, tick labels are right aligned with
  	their ticks.
  }
  \item{yearLabels}{
  	if \code{TRUE} place year labels centered under the x ticks they
	span. The default is \code{FALSE}.
  }
  \item{xAxisLabel}{
	text to appear centered under the x axis.  Must be a single
	character string, multi-line \code{xAxisLabel} is not supported. No
	default.
  }
  \item{xSpace}{
	lines of space to set aside directly beneath the x-axis to hold
	tick, year and/or axis labels. Default is \code{4}. The space
	created is \code{xSpace} times \code{labCex}. 
  }
  \item{log}{
	if \code{TRUE} use log scaling for y axes.  Default is \code{FALSE}.
  }
  \item{leftAxisMin}{
	minimum value of the left axis. If non-NULL, this overrides the
	calculation described in \code{leftExpandBy}.
  }
  \item{leftAxisMax}{
	maximum value of the left axis. If non-NULL, this overrides the
	calculation described in \code{leftExpandBy}.
  }
  \item{leftExpandBy}{
	calculate \code{leftAxisMin} and \code{leftAxisMax} by multiplicatively
	extending the data range of the leftAxis series in both directions
	by this amount.  Default value .04 extends the data range by 4\% in
	each direction. 
  }
  \item{leftTicks}{
	number of ticks to draw on left axis at "pretty" locations.  Default
	is 5. This argument is ignored if \code{leftTickLocations} is
	non-NULL.
  }
  \item{leftTickLocations}{
	if non-NULL, a vector of desired tick locations or a string that
	evaluates to such a vector.  The default value NULL lets the setting
	for \code{leftTicks} take effect.
  }
  \item{labelLeftTicks}{
	If \code{TRUE}, label ticks on the left axis. Default is \code{FALSE}.
  }
  \item{leftTickLabels}{
	character vector of tick labels or NULL (the default).  If NULL and
	\code{labelLeftTicks} is \code{TRUE}, labels are constructed from
	the tick locations.  This argument has no effect if
	\code{labelLeftTicks} is \code{FALSE}.
  }
  \item{rightAxisMin}{
	minimum value of the right axis. If non-NULL, this overrides the
	calculation described in \code{rightExpandBy}.
  }
  \item{rightAxisMax}{
	maximum value of the right axis. If non-NULL, this overrides the
	calculation described in \code{rightExpandBy}.
  }
  \item{rightExpandBy}{
	calculate \code{rightAxisMin} and \code{rightAxisMax} by multiplicatively
	extending the data range of the rightAxis series in both directions
	by this amount.  Default value .04 extends the data range by 4\% in
	each direction. 
  }
  \item{rightTicks}{
	number of ticks to draw on right axis at "pretty" locations.  Default
	is 5. This argument is ignored if \code{rightTickLocations} is non-NULL. 
  }
  \item{rightTickLocations}{
	if non-NULL, a vector of desired tick locations or a string that
	evaluates to such a vector.  The default value NULL lets the setting
	for \code{rightTicks} take effect.
  }
  \item{labelRightTicks}{
	If \code{TRUE}, label ticks on the right axis. Default is \code{FALSE}.
  }
  \item{rightTickLabels}{
	character vector of tick labels or NULL (the default).  If NULL and
	\code{labelRightTicks} is \code{TRUE}, labels are constructed from
	the tick locations.  This argument has no effect if
	\code{labelRightTicks} is \code{FALSE}.
  }
  \item{yCex}{
  	\code{cex} times this number gives the character expansion factor
	for the left and right axis labels. Default is 1.
  }
  \item{extendTopTick}{
	if \code{TRUE} (the default) the top tick of the y axes encloses the
	panel. \code{leftAxisMax} and \code{rightAxisMax} are increased as
	necessary to include the top tick for enclosing the panel.
  }
  \item{cex}{
	the base character expansion factor by which all of the
	\code{***cex} parameters are scaled. The default setting is the
	value of \code{par("cex")}.
  }
  \item{head}{
  	text to appear at the top of the figure region, with alignment
	determined by \code{headAlign}. No default.
  }
  \item{headAlign}{
  	number indicating justification for the strings in \code{head} and
	\code{sub}. \code{0} means left justify, \code{1} means right
	justify, \code{0.5} (the default) means to center the text.  Other
	numbers are a corresponding distance between the extremes.
  }
  \item{headCex}{
  	\code{cex} times this number gives the character expansion factor
	for \code{head}. Default is 1.5.
  }
  \item{sub}{
  	text to appear just under \code{head}, with alignment determined by
	\code{headAlign}. No default.
  }
  \item{subCex}{
  	\code{cex} times this number gives the character expansion factor
	for \code{sub}. Default is 0.85.
  }
  \item{leftTopLabel}{
  	text to appear at the top of the left axis, with alignment
	determined by \code{topLabelAlign}. No default.
  }
  \item{rightTopLabel}{
  	text to appear at the top of the right axis, with alignment
	determined by \code{topLabelAlign}. No default.
  }
  \item{topLabelAlign}{
  	number indicating alignment for the strings in \code{leftTopLabel}.
	\code{0} (the default) means left justify, \code{1} means right
	justify, \code{0.5} means to center the text.  \code{rightTopLabel},
	if given, is aligned by \code{1 - topLabelAlign}.
  }
  \item{labCex}{
  	\code{cex} times this number gives the character expansion factor
	for \code{leftTopLabel}, \code{rightTopLabel}, and \code{xAxisLabel}.
  }
  \item{leftInsideLabel}{
  	text to appear left justified and just inside the upper left corner
	of the plot region. No default.
  }
  \item{rightInsideLabel}{
  	text to appear right justified and just inside the upper right corner
	of the plot region. No default.
  }
  \item{innerOffset}{
	number between 0 and 1, a fractional offset for the inside labels.
	The left edge of \code{leftInsideLabel} is offset by this fraction
	of the x range from the left edge of the plot, as is the right edge
	of \code{rightInsideLabel} from the right edge of the plot.
  }
  \item{innerLine}{
	Number of lines in from the top edge of the plot to put the first
	line of the inside labels.
  }
  \item{innerCex}{
  	\code{cex} times this number gives the character expansion factor
	for \code{leftInsideLabel} and \code{rightInsideLabel}.
  }
  \item{foot}{
	text to appear at the bottom of the figure region, with alignment
	determined by \code{footAlign}. Use a vector of character strings to
	get several footnotes. No default.
  }
  \item{footAlign}{
	number indicating justification for the strings in \code{foot}.
	\code{0} (the default) means left justify, \code{1} means right
	justify, \code{0.5} means to center the text.  Other numbers are a
	corresponding distance between the extremes.
  }
  \item{footCex}{
	\code{cex} times this number gives the character expansion factor
	for \code{foot}. Default is 0.8.
  }
  \item{footColor}{
	character or numeric vector as long as \code{foot}, specifying the
	color for each footnote. Default is 1, the device default foreground
	color.  The elements of this argument are cyclically repeated, if
	necessary, to make \code{footColor} the same length as \code{foot}.
  }
  \item{footSpace}{
	lines of space to set aside directly beneath the space allocated by
	\code{xSpace} to hold footnotes. The space created is
	\code{footSpace} times \code{footCex}. Default is 
	length(\code{foot}); using a higher value will result in extra space
	in the bottom figure margin.
  }
  \item{tck}{
	The length of \code{xTick}, \code{xUnlabeledTick} and side tick
	marks as a fraction of the smaller of the width or height of the
	plotting region.  Minor ticks (\code{xMinorTicks}) are 2/3 as long.
	If \code{tck >= 0.5} it is interpreted as a fraction of the relevant
	side, so if \code{tck = 1} grid lines are drawn. The default is
	\code{tck = 0.03}. 
  }
  \item{axisWidth}{
  	line width for the axes and box (if any). Default is 2.
  }
  \item{start}{
  	starting date for the plot. The default is the earliest start time
	of all the series. This argument can be supplied in any of the forms
	understood by \code{ti()}.
  }
  \item{end}{
  	end date for the plot. The default is the latest end time
	of all the series. This argument can be supplied in any of the forms
	understood by \code{ti()}.
  }
  \item{boxType}{
  	character representing the type of box. Characters \code{"o"},
	\code{"l"} (ell), \code{"7"}, \code{"c"} will produce boxes which
	resemble the corresponding upper-case letters. The value \code{"n"}
	will suppress boxes. The default is \code{"u"}.
  }
  \item{leftMargin}{
	lines of space for the left margin.  Default value (-1) figures this
	out automatically.
  }
  \item{rightMargin}{
	lines of space for the right margin.  Default value (-1) figures this
	out automatically.
  }
  \item{topMargin}{
	lines of space for the top margin.  Default value (-1) figures this
	out automatically.
  }
  \item{bottomMargin}{
	lines of space for the bottom margin.  Default value (-1) figures this
	out automatically.
  }
  \item{nberShade}{
	if \code{TRUE}, use \code{nberDates()} to set the \code{shadeDates}.
	This will result in shading of NBER recession periods. The default is \code{FALSE}.
  }
  \item{shadeDates}{
    a matrix of yyyymmdd dates with two columns named 'Start'
    and 'End' that specifies the date ranges to be shaded.
    \code{nberDates()} produces a suitable matrix. If this argument is
    \code{NULL} and \code{nberShade} is not \code{TRUE}, no shading is done.}
  \item{shadeColor}{
	color to shade periods specified by \code{shadeDates}. Passed along
	to the \code{ymdShade} function. 
  }
  \item{shadeBorder}{
	if shading is being done, this argument tells what color the border
	of the shaded areas should be drawn in.  It can also be a logical,
	in which case \code{TRUE} draws borders in the foreground color and
	\code{FALSE} does not draw borders on the shaded areas. Default is \code{FALSE}.
  }
  \item{polyArgs}{
	additional args that are passed along to the \code{ymdShade}
	function and eventually to \code{polygon} if shading of date ranges
	is being done.
  }
}
\details{
  \code{leftAxis}, \code{plotType}, \code{lineType}, \code{lineWidth},
  \code{plotChar}, \code{dataCex}, \code{color} and \code{midPoints} are
  all cyclically repeated to make them length \code{nSeries}, the number
  of series plotted.

  Each of the text items \code{head}, \code{sub}, \code{leftTopLabel},
  \code{rightTopLabel}, \code{leftInsideLabel}, \code{rightInsideLabel},
  \code{foot}, and \code{xAxisLabel} can be given as a string, a
  collections of strings, or as a string that gets evaluated to one of
  the first two. (But \code{xAxisLabel} takes only a single string.)
  See the help details for \code{\link{evalOrEcho}} to see how this works.
}
\value{
  \code{tisPlot} invisibly returns a list of class "tisPlot" and
  elements named \code{series}, \code{dateFormat}, \code{plotType},
  \code{lineType}, \code{dataCex}, \code{color}, \code{plotChar},
  \code{lineWidth}, \code{yLegendOffset}, \code{cex}, \code{xRange},
  \code{leftRange}, \code{rightRange}, \code{midPoints} and \code{par}.
  This list is useful mostly as an argument to \code{tisLegend}.
}
\note{
  The arguments for \code{tisPlot} and its sister function
  \code{scatterPlot} have more descriptive names than the corresponding
  arguments in \code{plot}.  They are also all of unique types, unlike,
  for example, the \code{lty} argument in the usual \R plotting
  functions, which can be either character or numeric.  Limiting each
  argument to a single type was done to make it easier to design a user
  interface to drive the functions.

  Use \code{tisLegend} to add legends to a plot created by
  \code{tisPlot} or \code{scatterPlot}. 
}
\seealso{ \code{\link{evalOrEcho}}, \code{\link{scatterPlot}},
  \code{\link{tisLegend}}, \code{\link{nberShade}}
}
\examples{
firstTis <- tis(cumsum(rnorm(120)), start = c(1996,1), freq = 12)
secondTis <- tis(cumsum(rnorm(120)), start = c(1996,1), freq = 12)
tisPlot(firstTis, secondTis, color = c("red", "green"),
           lineType = "solid", head = "Two Random Walks")
tisLegend(legend = c("Random Walk 1", "Random Walk 2"))

series <- tis(cumsum(rnorm(200)), start = c(1960,1), tif = "quarterly")
tisPlot(series, xMinorTickFreq = "annual", nberShade = TRUE,
        head = "A Random Walk", sub = "Looks like an econ series",
        rightTopLabel = "$Billions")
romerLines()
}
\keyword{ts}
\keyword{hplot}
