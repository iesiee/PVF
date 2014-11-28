
predVarsLocal <- function(point, seqDays, vrbls, mc.cores = 1, sun = TRUE, ...){
  
  forecastList <- mclapply(vrbls, FUN=function(vrbl){
      
      forecast <- extractForecast(point, seqDays,
                                  vrbl = vrbl, sun = FALSE, ...)
      names(forecast) <- paste(vrbl,
                               names(forecast),
                               sep='.')
      return(forecast)
  }, mc.cores=mc.cores)
  
  forecast <- do.call(cbind, forecastList)
  attr(index(forecast), "tzone") <- "UTC"

  ## Do we need to add sun geometry?
  if (isTRUE(sun)){
      sunGeometry(point, forecast)
  } else {
      forecast
  }
}
