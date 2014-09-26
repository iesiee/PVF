hour <- function(tt)as.POSIXct(trunc(tt, 'hours'))

hourHalf <- function(tt)as.POSIXct(trunc(tt+30*60, 'hours'))

minute <- function(tt)as.POSIXct(trunc(tt, 'mins'))
