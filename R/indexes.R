##################################################################
## idwNear(surround, point, nmax)
##################################################################

idwNear <- function(surround, point, nmax,...){
  vals <- as(surround, 'SpatialPointsDataFrame')
  names(vals) <- 'values'
  myPoint <- spTransform(point, CRS(projection(surround)))
  pred <- idw(values ~ 1,
              locations = vals,
              newdata = myPoint,
              nmax = nmax,...)
  pred$var1.pred
}


##################################################################
## terrainIndex(surround, point) - see help(raster::terrain)
##################################################################

terrainIndex <- function(surround, point){
  myPoint <- spTransform(point, CRS(projection(surround)))
  myCell <- cellFromXY(surround, myPoint)
  nearPoints <- adjacent(surround, myCell, directions=8)
  nearVals <- surround[nearPoints[,2]]
  pointVal <- surround[myCell]
  tri <- mean(abs(nearVals - pointVal))
  tpi <- pointVal - mean(nearVals)
  rough <- max(c(pointVal, nearVals)) - min(c(pointVal, nearVals))
  result <- c(tri, tpi, rough) / pointVal
  result[pointVal == 0] <- 0
  names(result) <- c('tri', 'tpi', 'rough')
  result
}


##################################################################
## tempIndex(runVals)
##################################################################

tempIndex <- function(runVals){
  apply(runVals, 1, sd, na.rm=TRUE)
}
