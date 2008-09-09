\name{scatterPlot}
\alias{scatterPlot}
\title{ Produce high-quality scatter plots }
\description{
  Plotting function with scads of options for creating high quality scatter
  plots.  Usually used with \code{frbpage} for production work.
}
\usage{
scatterPlot(x, y, 
            plotType = "p",
            lineType = "solid", lineWidth = 1.5,
            plotChar = "*", dataCex = 1,
            color = "black",
            xAxisMin = NULL, xAxisMax = NULL, xExpandBy = 0.04,
            xTicks = 5, xTickLocations = NULL,
            labelXTicks = TRUE, xTickLabels = NULL,
            xCex = 1, xAxisLabel = NULL, labelXAxis = TRUE, xSpace = 4,
            yAxisMin = NULL, yAxisMax = NULL, yExpandBy = 0.04,
            yTicks = 5, yTickLocations = NULL,
            yTickLabels = NULL, labelLeftTicks = FALSE, labelRightTicks = TRUE,
            yCex = 1, extendTopTick = TRUE,
            leftAxisLabel = NULL, rightAxisLabel = NULL,
            labelLeftAxis = TRUE, labelRightAxis = FALSE,
            cex = 1,
            head = NULL, headAlign = 0.5, headCex = 1.5,
            sub = NULL, subCex = 0.85,
            leftTopLabel = NULL, rightTopLabel = NULL, topLabelAlign = 0,
            labCex = 1, leftInsideLabel = NULL, rightInsideLabel = NULL,
            innerOffset = 0.05, innerCex = 0.8,
            foot = NULL, footAlign = 0, footCex = 0.8, footSpace = -1,
            tck = 0.03, axisWidth = 2, boxType = "u",
            leftMargin = -1, rightMargin = -1,
            topMargin = -1, bottomMargin = -1)
}
\arguments{
  \item{x}{
	the x coordinates of points in the plot.  If this is a string, the
	function \code{evalOrEcho} will attempt to evaluate the string to
	obtain the x coordinates.
  }
  \item{y}{
	the y coordinates of points in the plot.  If this is a string, the
	function \code{evalOrEcho} will attempt to evaluate the string to
	obtain the y coordinates.
  }
  \item{plotType}{
	type of plot desired. Values are \code{"p"} for points (the
	default), \code{"l"} for lines, \code{"b"} for both points and lines
	(lines miss the points), and \code{"o"} for overlaid points and lines.
  }
  \item{lineType}{
	character or numeric vector specifying the line type if
	\code{plotType} calls for lines. Default is "solid".  For most
	devices, type \code{1} is solid, \code{2} is	dotted, 3 and up are
	a mix of dots and dashes. 
  }
  \item{lineWidth}{
	default is \code{1.5}. 
  }
  \item{plotChar}{
	character (or number for plotting symbols -- see the help for
	\code{\link{points}} for details) to be used for plotting points.  
	Default is "*".
  }
  \item{dataCex}{
	\code{cex} times this number gives the character
	expansion factor for the data points.  Default is 1.
  }
  \item{color}{
	string or number. Default is 1, the device default foreground color. 
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
  \item{xTicks}{
	number of ticks to draw on x axis at "pretty" locations.  Default
	is 5. This argument is ignored if \code{xTickLocations} is
	non-NULL.
  }
  \item{xTickLocations}{
	if non-NULL, a vector of desired tick locations or a string that
	evaluates to such a vector.  The default value NULL lets the setting
	for \code{xTicks} take effect.
  }
  \item{labelXTicks}{
	If \code{TRUE}, label ticks on the x axis. Default is \code{FALSE}.
  }
  \item{xTickLabels}{
	character vector of tick labels or NULL (the default).  If NULL and
	\code{labelXTicks} is \code{TRUE}, labels are constructed from
	the tick locations.  This argument has no effect if
	\code{labelXTicks} is \code{FALSE}.
  }
  \item{xCex}{
	\code{cex} times this number gives the character expansion factor
	for the x-axis labels. Default is 1.
  }
  \item{xAxisLabel}{
	text to appear centered under the x axis. Default value NULL creates
	a string by deparsing the \code{x}argument. This argument is ignored
	if \code{labelXAxis} is \code{FALSE}.
  }
  \item{labelXAxis}{
	if \code{TRUE} (the default), label the x axis according to
	\code{xAxisLabel}. 
  }
  \item{xSpace}{
	lines of space to set aside directly beneath the x-axis to hold
	tick, year and/or axis labels. Default is \code{4}. The space
	created is \code{xSpace} times \code{labCex}. 
  }
  \item{yAxisMin}{
	minimum value of the y axis. If non-NULL, this overrides the
	calculation described in \code{yExpandBy}.
  }
  \item{yAxisMax}{
	maximum value of the y axis. If non-NULL, this overrides the
	calculation described in \code{yExpandBy}.
  }
  \item{yExpandBy}{
	calculate \code{yAxisMin} and \code{yAxisMax} by multiplicatively
	extending the data range in both directions by this amount.  Default
	value .04 extends the data range by 4\% in each direction.
  }
  \item{yTicks}{
	number of ticks to draw on y axis at "pretty" locations.  Default
	is 5. This argument is ignored if \code{yTickLocations} is
	non-NULL.
  }
  \item{yTickLocations}{
	if non-NULL, a vector of desired tick locations or a string that
	evaluates to such a vector.  The default value NULL lets the setting
	for \code{yTicks} take effect.
  }
  \item{yTickLabels}{
	character vector of tick labels or NULL (the default).  If NULL and
	\code{labelLeftTicks} or \code{labelRightTicks} is \code{TRUE},
	labels are constructed from the tick locations.  This argument has
	no effect if \code{labelLeftTicks} and \code{labelRightTicks} are
	both \code{FALSE}. 
  }
  \item{labelLeftTicks}{
	If \code{TRUE}, label ticks on the left axis. Default is \code{FALSE}.
  }
  \item{labelRightTicks}{
	If \code{TRUE}, label ticks on the left axis. Default is \code{TRUE}.
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
  \item{leftAxisLabel}{
	text to appear centered outside the left axis. 
	Default value NULL creates a string by deparsing the \code{y}
	argument. This argument is ignored if \code{labelLeftAxis} is
	\code{FALSE}.
  }
  \item{labelLeftAxis}{
	if \code{TRUE} (the default), label the left axis according to
	\code{leftAxisLabel}. 
  }
  \item{rightAxisLabel}{
	text to appear centered outside the right axis.
	Default value NULL creates a string by deparsing the \code{y}
	argument. This argument is ignored if \code{labelRightAxis} is
	\code{FALSE}.
  }
  \item{labelRightAxis}{
	if \code{TRUE} label the right axis according to
	\code{rightAxisLabel}. Default is \code{FALSE}
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
  \item{innerCex}{
  	\code{cex} times this number gives the character expansion factor
	for \code{leftInsideLabel} and \code{rightInsideLabel}.
  }
  \item{foot}{
	text to appear at the bottom of the figure region, with alignment
	determined by \code{footAlign}. No default.
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
  \item{footSpace}{
	lines of space to set aside directly beneath the space allocated by
	\code{xSpace} to hold footnotes. The space created is
	\code{footSpace} times \code{footCex}. Default is 
	length(\code{foot}); using a higher value will result in extra space
	in the bottom figure margin.
  }
  \item{tck}{
  	length of major tick marks in inches. Minor ticks are 2/3 as long.
  	Default is 0.03.
  }
  \item{axisWidth}{
  	line width for the axes and box (if any). Default is 2.
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
}
\details{
  Each of the text items \code{head}, \code{sub}, \code{leftTopLabel},
  \code{rightTopLabel}, \code{leftInsideLabel}, \code{rightInsideLabel},
  \code{foot}, \code{leftAxisLabel}, \code{rightAxisLabel} and
  \code{xAxisLabel} can be given as a string, a collections of strings,
  or as a string that gets evaluated to one of the first two.  Multiple
  strings are drawn on successive lines.
}
\value{
  \code{scatterPlot} invisibly returns a list of class "scatterPlot" and
  elements named \code{xy} (a matrix containing \code{x} and \code{y} in
  two columns), \code{plotType}, \code{lineType}, \code{color},
  \code{plotChar}, \code{lineWidth}, \code{x} (x coordinate for legend),
  \code{y} (y coordinate for legend), \code{xRange}, \code{yRange},
  \code{innerCex} and \code{par}. 
  This list is useful mostly as an argument to \code{legend}.
}
\author{ Jeff Hallman }
\note{
  \code{scatterPlot} is a companion to \code{tisPlot}.  Both are
  designed to be driven from a graphical user interface.
}
\seealso{ \code{\link{tisPlot}}, \code{\link{evalOrEcho}}}
\examples{
}
\keyword{ hplot }
