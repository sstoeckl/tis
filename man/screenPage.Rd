\name{screenPage}
\alias{screenPage}
\title{Page Setup for Plots}
\description{
  Places header and footer text items in outer margin of page and splits the
  screen appropriately.  Can also redraw header and footer.
}
\usage{
screenPage(head = NULL, sub = NULL, foot = NULL,
           date = FALSE, dateFormat = "\%x", time = FALSE,
           topLeft = character(0), topRight = character(0),
           headFont = par("font.main"), subFont = par("font.sub"),
           footFont = par("font"),
           cex = 1.001, headCex = 1.5, subCex = 0.85,
           footCex = 0.75, topLeftCex = 0.85, topRightCex = 0.85,
           footAlign = 0,
           leftMargin = 0, rightMargin = leftMargin,
           topMargin = 0, bottomMargin = topMargin)
}
\arguments{
  \item{head}{
    character string or strings to appear centered in the top outer margin
    of the page.  If \code{length(head) > 1}, a multi-line main title results.
  }
  \item{sub}{
    character string or strings to appear centered just under \code{head}.  
  }
  \item{foot}{
    character string or strings to appear in the bottom outer margin of
    the page.
  }
  \item{date}{
    logical: if \code{TRUE}, put the current date in the upper right corner of
    the page.
  }
  \item{dateFormat}{
    \code{strptime}-style format to use if \code{date} is \code{TRUE}.
    The default formats dates like 11/30/2006.  What is actually being
    formatted is the value returned by \code{Sys.time()}, so you can
    also use time formats here.  For example, setting
    \code{dateFormat = "\%c"} will create a string like
    "Thu 30 Nov 2006 02:49:45 PM EST".
  }
  \item{time}{
    logical: if \code{TRUE}, put the current time in the upper right corner of
    the page.  If \code{date} is also true, the time string will be on
    the line below the date string.
  }
  \item{topLeft}{
    character string or strings to appear at top left corner of the page
  }
  \item{topRight}{
    character string or strings to appear at top right corner of the page
  }
  \item{headFont}{
    font to use in writing the main title in \code{head}. The default
    uses whatever par("font.main") is set to.
  }
  \item{subFont}{
    font to use in writing the sub title in \code{sub}. The default uses
    whatever par("font.sub") is set to.
  }
  \item{footFont}{
    font to use in writing the footnotes in \code{foot}. The default
    uses whatever par("font") is set to.
  }
  \item{cex}{
    number by which all of the other "cex" arguments are scaled.  
  }
  \item{headCex}{
    number: Character Expansion Factor (cex) for the string(s) in
    \code{head}. The actual cex used will be \code{cex * headCex}.
  }
  \item{subCex}{
    number: cex for the string(s) in \code{sub}.  The actual cex used will 
    be \code{cex * subCex}. 
  }
  \item{footCex}{
    number: cex for the string(s) in \code{foot}.  The actual cex used will 
    be \code{cex * footCex}. 
  }
  \item{topLeftCex}{
    number: cex for the string(s) appearing in the top left corner
    of the page.  The actual cex used for these strings will be
    \code{cex * topLeftCex}.  
  }
  \item{topRightCex}{
    number: cex for the string(s) appearing in the top right corner
    of the page, including the time and date stamps.
    The actual cex used for these strings will be \code{cex * topRightCex}. 
  }
  \item{footAlign}{
    number: justification for the strings in \code{foot}.  \code{0} means left
    justify, \code{1} means right justify, \code{0.5} means to center
    the text. Other numbers are a corresponding distance between the
    extremes. 
  }
  \item{leftMargin}{left margin of page in inches.}
  \item{rightMargin}{right margin of page in inches.  Default is same
    as \code{leftMargin}}
  \item{topMargin}{top margin of page in inches.}
  \item{bottomMargin}{bottom margin of page in inches.  Default is same
    as \code{topMargin}}
}
\details{
  \code{screenPage} first sets aside space for the margins specified by
  \code{topMargin}, \code{bottomMargin}, \code{leftMargin} and
  \code{rightMargin}.  Then it figures out how much additional space is
  needed for the top and bottom outer margin text elements, places them,
  and then splits the screen in 3, with screen 3 being the middle part
  of the page.  The user is then free either to further subdivide screen
  3 (using \code{split.screen()}) or to use it as is.
  
  On exit, screen 3 is the active screen.
}
\value{
  This function returns a list of all of its arguments, including
  default values for arguments that were not supplied. The return is
  invisible if a graphics device is active.
}
\seealso{ \code{\link{split.screen}}}
\examples{
screenPage(head = "Chart 1", date = TRUE, foot = rep(" ", 4),
           cex = 0.85, headCex = 1)

## then draw charts, possibly after further subdividing the screen
}
\keyword{hplot}
