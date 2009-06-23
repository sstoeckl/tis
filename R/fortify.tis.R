fortify.tis <- function(x, offset = 0.5,
                        dfNames = ifelse(is.null(dim(x)), as.character(substitute(x)), NA)){
  dfNames <- dfNames  ## force evaluation of the ifelse statement 
  date <- as.Date(POSIXct(ti(x), offset = offset))
  x <- as.matrix(x)
  df <- data.frame(as.data.frame(x), date)
  ## in univariate tis case df will have the "wrong name for variable x
  if(!is.na(dfNames))
    names(df) <- c(dfNames, "date")
  return(df)
}

