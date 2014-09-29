## Build a time series with several WRF variables (vrbls) from a specified
## source (src) for a time period (seqDays) at a defined location (point).
## Each variable is quantified with the value at the location, the IDW
## interpolation, three terrain indexes (tri, tpi and rough) and one temporal
## index. This function uses parallel computing with mclapply.

predVarsLocal <- function(point, seqDays, vrbls, mc.cores=1,...){
  
  message(mc.cores)
  
  a<- mc.cores+1
  
  message(a)
  
  message(class(mc.cores))
  
  forecastList <- mclapply(vrbls, FUN=function(vrbl){
  
                           forecast <- extractForecast(point, seqDays, vrbl=vrbl,...)
                           names(forecast) <- paste(vrbl, names(forecast), sep='.')
                           return(forecast)
                           }, mc.cores=mc.cores)
  
  forecast <- do.call(cbind, forecastList)

}
