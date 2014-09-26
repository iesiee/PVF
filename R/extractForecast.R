## Build a time series with one WRF variable (vrbl) from a specified
## source (src) for a time period (seqDays) at a defined location (point).
## The variable is quantified with the value at the location, the IDW
## interpolation, three terrain indexes (tri, tpi and rough) and one
## temporal index.

extractForecast <- function(point, seqDays, vrbl, src, remote, dataDir,...){

  fl <- lapply(seqDays, FUN=function(d){
    
    print(paste(d, vrbl, sep=' '))
    
    ### Extract forecast using only the first frames from each run
    forecastDay <- try(getRasterDays(var = vrbl, start = d, end = d, remote = remote,
                                    service = src, dataDir = dataDir,...),
                       silent=FALSE)
    
    if(class(forecastDay) != 'try-error'){
       ### Value at the location
       vals <- as.vector(extract(forecastDay, point))
       ### Spatial interpolation with IDW
       idwVals <- sapply(unstack(forecastDay),
                         idwNear, point=point, nmax=25)
       ### Spatial variability with terrain indexes
       terrIdx <- t(sapply(unstack(forecastDay),
                           terrainIndex, point))
       ### Combine vals, idwVals and terrIdx
       zoo(data.frame(point=vals, idw=idwVals, terrIdx),
           getZ(forecastDay))
     } else {
       ### Bad files result in the elimination of the corresponding day
       NULL
     }
        
  })
  
  ### Value at the point and spacial variability with terrain indexes
  spaceIdx <- do.call(rbind, fl)
  
  ### Temporal variability with temporal index
  forecastRuns <- getPointRuns(point, var = vrbl,
                               start = seqDays[1],
                               end = seqDays[length(seqDays)],
                               service = src)
  tempIdx <- tempIndex(forecastRuns)
  tempIdx <- zoo(data.frame(timeDesv=tempIdx),
                 index(forecastRuns))
  
  ### Combine value at the point and spacial and temporal variabilities
  forecast <- merge(spaceIdx, tempIdx)
  forecast <- na.omit(forecast)
  
}
