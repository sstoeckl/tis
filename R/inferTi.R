inferTi <- function(dateTimes){
  ## make an educated guess as to the frequency of dateTimes and return a ti
  ## object of the same length
  naSpots <- is.na(dateTimes)
  hasNAs <- any(naSpots)
  if(all(naSpots)) stop("dateTimes are all NA")
    
  dt <- as.POSIXct(dateTimes[!naSpots])
  dtJul <- floor(jul(dt))
  diffSeconds <- median(diff(unique(sort(unclass(dt)))))
  freq <- round((365.25 * 60*60*24)/diffSeconds)
  
  if(freq > 365){  ## maybe intraday
    if((diffSeconds %% 3600) == 0)  tif <- hourly(diffSeconds / 3600)
    else {
      if((diffSeconds %% 60) == 0)  tif <- minutely(diffSeconds / 60)
      else                          tif <- secondly(diffSeconds)
    }
  }
  else {
    if(freq == 365){
      if(all(between(dayOfWeek(dtJul), 2, 6))) tif <- "business"
      else                                     tif <- "daily"
    }
    else tif <- freq2tif(freq)
  }
  
  dtTi <- ti(dt, tif = tif)

  if(freq < 365){
    if(median(abs(jul(dtTi) - dtJul)) > 0.5){ ## could be wrong tif
      maxJul <- max(dtJul) ## the most recent date is most likely correct
      newTif <-
        switch(as.character(freq),
               "52" = tif("wsunday")    + dayOfWeek(maxJul) - 1,
               "26" = tif("bw1sunday")  + dayOfPeriod(maxJul, "bw1sunday") - 1,
                "6" = tif("bmdecember") - (month(maxJul) %% 2),
                "4" = tif("qoctober")   + ((2 + month(maxJul)) %% 4), 
                "2" = tif("sannjuly")   + ((5 + month(maxJul)) %% 6),
                "1" = tif("annjanuary") - 1 + month(maxJul),
                NULL)
      if(is.null(newTif))
        stop(paste("Could not infer tif from apparent frequency:", freq))
      else 
        dtTi <- ti(dtJul, tif = newTif)
    }
  }
  ans <- numeric(length(naSpots)) + NA
  ans[!naSpots] <- dtTi
  asTi(ans)
}
