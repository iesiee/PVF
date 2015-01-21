## Extract lonlat from a point
point2lonlat <- function(point){
    if (is(point, "SpatialPoints")) {
        if (!isLonLat(point)) {
            point <- spTransform(point,
                                 CRS("+proj=longlat +ellps=WGS84"))
        }
        lon <- coordinates(point)[1]
        lat <- coordinates(point)[2]
        return(c(lon, lat))
    }
    else {
        return(point)
    }
}

sunGeometry <- function(point, forecast){
    ## Sun geometry
    lonlat <- point2lonlat(point)
    sol <- calcSol(lat = lonlat[2],
                   BTi=local2Solar(index(forecast), lon=lonlat[1]))
    sol <- as.zooI(sol)[, c('AlS', 'AzS', 'Bo0')]
    sol$Bo0[is.na(sol$Bo0)] <- 0
    index(sol) <- index(forecast)
    
    ## Merge and Time Zone
    forecast <- merge(forecast, sol)
    attr(index(forecast), "tzone") <- "UTC"
    return(forecast)
}

## Dismiss minutes and seconds
## 01:22:24 --> 01:00:00
## 01:32:24 --> 01:00:00
hour <- function(tt)as.POSIXct(trunc(tt, 'hours'))
## Idem, but centering
## 01:22:24 --> 01:00:00
## 01:32:24 --> 02:00:00
hourHalf <- function(tt)as.POSIXct(trunc(tt+30*60, 'hours'))
## Dismiss seconds
minute <- function(tt)as.POSIXct(trunc(tt, 'mins'))
