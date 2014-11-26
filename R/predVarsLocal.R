
predVarsLocal <- function(point, seqDays, vrbls, mc.cores = 1,...){
  
  forecastList <- mclapply(vrbls, FUN=function(vrbl){
  
                           forecast <- extractForecast(point, seqDays, vrbl=vrbl,...)
                           names(forecast) <- paste(vrbl, names(forecast), sep='.')
                           return(forecast)
                           }, mc.cores=mc.cores)
  
  forecast <- do.call(cbind, forecastList)

}
