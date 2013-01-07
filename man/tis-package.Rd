\name{tis-package}
\alias{tis-package}
\docType{package}
\title{ Time Indices and Time Indexed Series}
\description{
  Functions and S3 classes for time indices and time indexed
  series, a flexible kind of time series compatible with series and
  frequencies understood by the FAME DBMS.
}
\details{
  For a complete list of functions provided by this package, use
  \code{library(help="tis")}.

The \code{ti} (Time Index) and \code{tis} (Time Indexed Series) classes 
provide date arithmetic facilities and an alternative to the somewhat
inflexible \code{ts} class in the standard R stats package. 

Time Indexes (\code{ti} class)

A time index has two parts: a \code{tif} (Time Index Frequency) code and
a period.  \code{tif} codes all lie in the open interval (1000..5000)
and the period is a nonnegative number less than 1e10.  The \code{ti} encodes
both, as for any ti \code{z}

\code{unclass(z) == (tif(z) * 1e10) + period(z)}

Each \code{tif} has a particular base period (the time period where
\code{period(z) == 0}). For example, the base period for an
"anndecember" (annual December) \code{ti} is the year ending December
31, 1599.  Periods before the base period cannot be represented by
instances of the \code{ti} class. 

If \code{x} and \code{y} are \code{ti} objects with the same \code{tif}, then 

\code{x - y == period(x) - period(y)}

and

\code{x + (period(y) - period(x)) == y} 

are both \code{TRUE}, so you can use \code{ti}'s for various calendrical calculations. 

A \code{jul} class is also provided for Julian date-time objects. The \code{jul()}
constructor can create a \code{jul} from an \code{ssDate} (spreadsheet date), a
\code{POSIXct} or \code{POSIXlt} object, a \code{ti} object, a decimal
time (like 2007.5), a yyyymmdd number, a \code{Date}, or anything that
can be coerced to a \code{Date} by \code{as.Date}.  The \code{ymd()}
function and its derivatives (\code{year()}, \code{month()}, \code{day()}, etc.) 
work on anything that \code{jul()} can handle.


Time Indexed Series (\code{tis} class)

The \code{tis} class maps very closely to the FAME (http://www.sungard.com/Fame/) 
database notion of what a time series is.  A \code{tis} (Time Indexed Series) is
vector or matrix indexed by a \code{ti}.  If \code{x} is a \code{tis},
then \code{start(x)} gives the \code{ti} for the first observation, and
\code{[start(x) + k]} is the \code{ti} for the k'th 
observation, while \code{end(x)} gives the \code{ti} for the last observation.  

You can replace, say, the 5'th observation in a \code{tis x} by

\code{x[start(x) + 4] <- 42}

and of course the \code{[} operator also works with a \code{ti}.  So if
you want the value of the daily series \code{x} from July 3, 1998, you can get it with

\code{x[ti(19980703, "daily")]}

provided, of course, that \code{ymd(start(x)) <= 19980703 <= ymd(end(x))}.

Numerous methods for \code{tis} objects are provided:

\preformatted{
> methods(class = "tis")
 [1] aggregate.tis*     as.data.frame.tis* as.matrix.tis*     as.tis.tis*       
 [5] as.ts.tis*         cbind.tis*         cummax.tis*        cummin.tis*       
 [9] cumprod.tis*       cumsum.tis*        cycle.tis*         deltat.tis*       
[13] diff.tis*          edit.tis*          end.tis*           frequency.tis*    
[17] lag.tis*           lines.tis*         Ops.tis*           points.tis*       
[21] print.tis*         RowMeans.tis*      RowSums.tis*       start.tis*        
[25] tifName.tis*       tif.tis*           time.tis*          [<-.tis*          
[29] [.tis*             ti.tis*            t.tis              window.tis*       
}

as well as \code{tisMerge} and \code{tisPlot} functions.  The
\code{convert} function creates series of different frequencies from a
given series in ways both more powerful and more flexible than
\code{aggregate} can. 

Setting Default Frequencies: 

Like the FAME DBMS, the \code{ti} class has seven weekly frequencies for
weeks ending on Sunday, Monday, and it has 12 annual frequencies, for
years ending on the last day of each of the 12 months. There are also
multiple biweekly, bimonthly and semiannual frequencies.

At any time you can use the function \code{setDefaultFrequencies} to change which
actual frequencies the strings "weekly", "biweekly", "bimonthly",
"quarterly", "semiannual" and "annual" refer to.  Note (as shown by
\code{args(setDefaultFrequencies)}) that if you don't specify some other frequencies
to \code{setDefaultFrequencies}, you'll get default weeks ending on Monday and
default biweeks ending on the first Wednesday.  I chose these because they
are the weeks and biweeks used most often (for money and reserves data) in my
section at the Federal Reserve Board.  You might want to use something like

\code{setHook(packageEvent("tis", "onLoad"), tis::setDefaultFrequencies(yourArgsGoHere))}

in your \code{.First()} to automatically set the defaults up the way you want them
whenever this package is loaded.


LAGS:

The \code{stats::lag(x, k)} function says that a series lagged by a positive k starts
earlier.  The opposite is true for the \code{Lag} function in this package, to
maintain consistency with the common usage of 'first lag, second lag' and so
on in econometrics.

\code{tisFilter}:

The \code{stats::filter} function coerces it's argument to a \code{ts} time series and
returns a \code{ts}.  For \code{tis} and \code{zoo} series, this is not right.  Both I and
the author of the \pkg{zoo} package have requested that \code{stats::filter} be made an S3
generic with the current version renamed as \code{filter.default}.  This would allow
\pkg{zoo} and \pkg{tis} to define \code{filter.zoo} and
\code{filter.tis} such that \code{filter(aZooOrTis)} would do the right
thing.  We are hopeful that this will happen soon. Meanwhile,
\code{tisFilter} should be used to filter a \code{tis}. 

}
\author{
  Jeff Hallman \email{jhallman@frb.gov}
  
  Maintainer: ditto
}
\keyword{ package }
