extractForecast <- function(point, seqDays, vrbls,
                            sun = FALSE,
                            aux.index = FALSE,
                            service = 'meteogalicia',
                            remote = TRUE,
                            dataDir = '.',
                            mc.cores = 1,
                            ...){
  
  if(!is(point, 'SpatialPoints')){
    ## Some of next methods use spTransform, that works only with SpatialPoints.
    ## Assume longitude-latitude projection (point[1]=lon and point[2]=lat).
    projLL <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
    point <- SpatialPoints(cbind(point[1], point[2]),
                           CRS(projLL))
  }
  
  forecastList <- mclapply(vrbls, FUN=function(vrbl){
      
      message(paste('Calculations with', vrbl, '...'))
      
      fl <- lapply(seqDays, FUN=function(d){
        
        if(aux.index){
          message(paste('Calculating point, IDW and spatial index for', d, '...'))
        } else {
          message(paste('Calculating point and IDW for', d, '...'))
        }
        
        ## Extract forecast using only the first frames from each run
        forecastDay <- try(getRasterDays(var = vrbl,
                                         start = d,
                                         end = d,
                                         service = service,
                                         remote = remote,
                                         dataDir = dataDir,
                                         ...),
                           silent=FALSE)
        
        if(!is(forecastDay, 'try-error')){
          
          ## Value at the location
          vals <- as.vector(extract(forecastDay, point))
          
          ## Spatial interpolation with IDW (using function 'idwNear')
          idwVals <- sapply(unstack(forecastDay),
                            idwNear, point=point, nmax=25)
          
          if(aux.index){
            
            ## Spatial variability with terrain indexes (using function 'terrainIndex')
            terrIdxVals <- t(sapply(unstack(forecastDay),
                                    terrainIndex, point))
            
            ## Combine vals, idwVals and terrIdxVals
            zoo(data.frame(point=vals, idw=idwVals, terrIdxVals),
                getZ(forecastDay))
            
          } else {
            
            ## Combine vals and idwVals
            zoo(data.frame(point=vals, idw=idwVals),
                getZ(forecastDay))
            
          }
          
        } else {
          ## Bad files result in the elimination of the
          ## corresponding day
          NULL
        }
        
      })
      
      ## Value and IDW at the point of interest and spacial variability indexes
      forecast <- do.call(rbind, fl)
      attr(index(forecast), "tzone") <- "UTC"
      
      if(aux.index){
        
        message('Calculating temporal indexes ...')
        
        ## Extract forecast for every run
        forecastRuns <- getPointRuns(point, var = vrbl,
                                     start = seqDays[1],
                                     end = seqDays[length(seqDays)],
                                     service = service)
        
        ## Temporal variability with temporal index (using function 'tempIndex')
        tempIdxVals <- tempIndex(forecastRuns)
        tempIdxVals <- zoo(data.frame(timeDesv=tempIdxVals),
                           index(forecastRuns))
        
        ##  Combine previous forecast and tempIdxVals
        forecast <- merge(forecast, tempIdxVals)
        attr(index(forecast), "tzone") <- "UTC"
        
      }
      
      forecast <- na.omit(forecast)
      names(forecast) <- paste(vrbl, names(forecast), sep='.')
      return(forecast)
      
  }, mc.cores=mc.cores)
  
  forecast <- do.call(cbind, forecastList)
  attr(index(forecast), "tzone") <- "UTC"

  ## Do we need to add sun geometry?
  if(sun){
    message('Calculating Sun Geometry ...')
    sunGeometry(point, forecast)
  } else { forecast }
  
}