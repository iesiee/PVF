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
