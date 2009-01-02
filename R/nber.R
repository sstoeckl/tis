nberDates <- function(){
  matrix(ncol = 2,
         dimnames = list(character(0), c("Start", "End")),
         byrow = TRUE,
         data =
         c(18570701, 18581231, 
           18601101, 18610630, 
           18650501, 18671231, 
           18690701, 18701231, 
           18731101, 18790331, 
           18820401, 18850531, 
           18870401, 18880430, 
           18900801, 18910531, 
           18930201, 18940630, 
           18960101, 18970630, 
           18990701, 19001231, 
           19021001, 19040831, 
           19070601, 19080630, 
           19100201, 19120131, 
           19130201, 19141231, 
           19180901, 19190331, 
           19200201, 19210731, 
           19230601, 19240731, 
           19261101, 19271130, 
           19290901, 19330331, 
           19370601, 19380630, 
           19450301, 19451031, 
           19481201, 19491031, 
           19530801, 19540531, 
           19570901, 19580430, 
           19600501, 19610228, 
           19700101, 19701130, 
           19731201, 19750331, 
           19800201, 19800731, 
           19810801, 19821130, 
           19900801, 19910331, 
           20010401, 20011130,
           20080101, NA))
}

nber.xy <- function(xrange = NULL, openShade = TRUE){
  ## Returns a list of x and y coordinates that can be fed to polygon() to
  ## draw nber shadings on the current plot.  If openShade is FALSE and the
  ## last row of nberDates() has a Start but End is NA, and the Start is
  ## within the range of the current plot, the returned list will also include
  ## a vLine element that gives the x coordinate of the last Start. 
  
  if(as.vector(dev.cur()) < 2) ## there is no active graphics device
	return(list(x = NULL, y = NULL, vLine = NULL))
  
  usr <- par("usr")
  if(is.null(xrange))
    xrange <- usr[1:2]
  yrange <- usr[3:4]
  if(par("ylog")) 
    yrange <- 10^yrange

  ## nberDates() returns a matrix of dates in yyyymmdd form with columns
  ## named "Start" and "End". 
  nber <- get("nberDates", pos = 1)()
  naSpots <- is.na(nber)
  nber[!naSpots]   <- time(jul(nber[!naSpots]))

  ## handle case where latest recession end is NA
  finalRow <- nber[nrow(nber),]
  if(is.na(finalRow["End"]) && (finalRow["Start"] < xrange[2]) && !openShade){
    nber <- nber[1:(nrow(nber)-1),]
    vLine <- finalRow["Start"]
  }
  else vLine <- NULL
  
  firstRow <- sum(nber[,"End"] <= xrange[1], na.rm = TRUE) + 1
  lastRow  <- sum(nber[,"Start"] <= xrange[2], na.rm = TRUE)
  if((firstRow > nrow(nber)) || lastRow == 0){
	## entire plot is out of the range covered by nber Dates
	return(list(x = NULL, y = NULL, vLine = NULL))
  }
  nber <- nber[firstRow:lastRow,,drop = FALSE]
  n <- nrow(nber)
  nber[1, "Start"] <- max(xrange[1], nber[1, "Start"])
  if(is.na(nber[n, "End"]) || nber[n, "End"] > xrange[2])
	nber[n, "End"] <- xrange[2]

  poly.x <- poly.y <- numeric(0)
  for(i in 1:n){
	poly.x <- c(poly.x, rep(nber[i, "Start"], 2), rep(nber[i, "End"], 2))
	poly.y <- c(poly.y, yrange, rev(yrange))
	if(i < n){
	  poly.x <- c(poly.x, NA)
	  poly.y <- c(poly.y, NA)
	}
  }
  list(x = poly.x, y = poly.y, vLine = vLine)
}

nberShade <- function(col = grey(0.8), border = FALSE, xpd = FALSE,
                      xrange = NULL, openShade = TRUE, ...){
  dotArgs <- list(...)
  locations <- nber.xy(openShade = openShade, xrange = xrange)
  polyArgs <- list(x = locations$x,
                   y = locations$y,
                   col = col,
                   border = border,
                   xpd = xpd)
  if(length(dotArgs) > 0)
    polyArgs <- updateList(polyArgs, dotArgs)
  do.call("polygon", polyArgs)
  if(!is.null(locations$vLine))
    abline(v = locations$vLine, lwd = 1.5, col = col)
}

romerLines <- function(){
  xrange <- par("usr")[1:2]
  rDates <- c(1947.75, 1955.6667, 1968.91667, 
              1974.25, 1978.5833, 1979.75, 1988.91667)
  rDates <- rDates[ rDates >= xrange[1] & rDates <= xrange[2]]
  abline(v = rDates, lty = 4)
}
