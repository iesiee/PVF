extractForecast <- function(point, seqDays, vrbl,
                            src = 'meteogalicia',
                            remote = TRUE, dataDir = '.',
                            sun = FALSE,
                            ...){
    if (!is(point, "SpatialPoints")) {
        ## Some of next methods use spTransform, that works only with
        ## SpatialPoints.
        ## Assume long-lat projection
        projLL <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'
        point <- SpatialPoints(cbind(point[1], point[2]),
                               CRS(projLL))
    }
    
    fl <- lapply(seqDays, FUN=function(d){
        
        message(paste(d, vrbl, sep=' '))
        
        ## Extract forecast using only the first frames from each run
        forecastDay <- try(getRasterDays(var = vrbl,
                                         start = d, end = d,
                                         remote = remote,
                                         service = src,
                                         dataDir = dataDir,
                                         ...),
                           silent=FALSE)
        
        if(class(forecastDay) != 'try-error'){
            ## Value at the location
            vals <- as.vector(extract(forecastDay, point))
            ## Spatial interpolation with IDW
            idwVals <- sapply(unstack(forecastDay),
                              idwNear, point=point, nmax=25)
            ## Spatial variability with terrain indexes
            terrIdx <- t(sapply(unstack(forecastDay),
                                terrainIndex, point))
            ## Combine vals, idwVals and terrIdx
            zoo(data.frame(point=vals, idw=idwVals, terrIdx),
                getZ(forecastDay))
        } else {
            ## Bad files result in the elimination of the
            ## corresponding day
            NULL
        }
    })
    
    ## Value at the point and spacial variability with terrain indexes
    spaceIdx <- do.call(rbind, fl)
    attr(index(spaceIdx), "tzone") <- "UTC"
  
    ## Temporal variability with temporal index
    forecastRuns <- getPointRuns(point, var = vrbl,
                                 start = seqDays[1],
                                 end = seqDays[length(seqDays)],
                                 service = src)
    tempIdx <- tempIndex(forecastRuns)
    tempIdx <- zoo(data.frame(timeDesv=tempIdx),
                   index(forecastRuns))
    
    ##  Combine value at the point and spacial and temporal
    ##  variabilities
    forecast <- merge(spaceIdx, tempIdx)
    forecast <- na.omit(forecast)
    attr(index(forecast), "tzone") <- "UTC"
  
    ## Do we need to add sun geometry?
    if (isTRUE(sun)){
        sunGeometry(point, forecast)
    } else {
        forecast
    }
}
