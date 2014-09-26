predictOneDay <- function(id, vals, toPredDay, nDays, nDaysKT, method){
  toPredDay <- as.Date(toPredDay)
  
  scnData <- scenarioSet(id, vals)
  
  valsTest <- window(vals,
                     start=as.POSIXct(toPredDay)+3600,
                     end=as.POSIXct(toPredDay + 1))
  
  test <- scenarioSet(id, valsTest)
  
  if(nDays=='all'){
  
  ## Remove 'toPredDay' from 'scnData'
  scnDataIdx <- as.Date(index(scnData))
  scnData <- scnData[!(scnDataIdx %in% toPredDay)]
  
  switch(method,
         'kt'={
           ## Computes the daily clearness index for the predictor set 'scnData'
           Bo0 <- vals$Bo0
           swflx <- vals$swflx.point
           Bo0d <- aggregate(Bo0, by = as.Date)
           G0d <- aggregate(swflx, by = as.Date)
           ktD <- G0d/Bo0d
           ktD[!is.finite(ktD)] <- 0
           ## Computes the daily clearness index for the test day
           ktTest <- sum(valsTest$swflx.point)/sum(valsTest$Bo0)
           ## Find days with similar clearness index in the predictor set
           diffKT <- abs(ktD-ktTest)
           orderKT <- order(diffKT, decreasing = FALSE, na.last = TRUE)
           diffKTorder <- lapply(orderKT, FUN=function(i) diffKT[i])
           diffKTfiltered <- index(do.call(rbind,diffKTorder[1:nDaysKT]))
           scnDataIdx <- as.Date(index(scnData))
           scnData <- scnData[scnDataIdx %in% diffKTfiltered]
         },
         'ks'={
           ## Convert to xts to use its subset capabilities
           scnData <- as.xts(scnData)
           ## Days included in `scnData`. Use `as.character` to work with
           ## the subset mechanism of xts
           days <- as.character(unique(as.Date(index(scnData))))
           ## Use only swflx variables
           idxRad <- grep('swflx', names(test))
           ## Compute KS distance between scnData and test for each day included
           ## in `scnData`
           ksD <- sapply(days, function(d) ksDistance(test[,idxRad], scnData[d,idxRad]))
           ## Average KS distance
           ksDMean <- apply(ksD, 2, mean, na.rm = TRUE)
           ## Order by decreasing KS distance
           best <- sort(ksDMean)
           ## The result is a set of `nDays` with the lowest KS distance.
           scnData <- as.zoo(scnData[names(best[seq_len(nDaysKT)])])
         })
  }
  
  pred <- rfPredict(test[, -1], scnData, nDays=nDays)
  pred <- cbind(Pac = test$Pac, pred)
  
  return(pred)
}
