plotWindow <- function(xlim, ylim,
                       log = "", asp = NA,
                       xaxs = "i", yaxs = "i",
                         ...){
  ## frb version sets xaxs and yaxs to "i" by default
  plot.window(xlim = xlim,
              ylim = ylim,
              log = log,
              asp = asp,
              xaxs = xaxs,
              yaxs = yaxs, ...)
}
