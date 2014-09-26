predictGoal <- function(point, goal, vrbls, src, remote, dataDir, mc.cores,
                        history, id, nDaysKT, method, ...){
  
  ## Select scenario for the predictor set
  scnData <- scenarioSet(id, history)
  
  ## Create test set
      ## NWP variables
      forecastTest <- predVarsLocal(point, goal, vrbls, src, remote,
                                    dataDir, mc.cores=mc.cores, box=box)
      ## Sun geometry
      sol <- as.zooI(calcSol(lat=point[2],
                             BTi=local2Solar(index(forecastTest), lon=point[1])))
      sol$Bo0[is.na(sol$Bo0)] <- 0
      ## Hourly averages (necessary for Meteogalicia hourly forecast)
      solH <- aggregate(sol[,c('AlS', 'AzS', 'Bo0')], by=hour, FUN='mean')
      ## Merging 'forecastTest' and sun geometry
      valsTest <- merge(forecastTest, solH)
      ## Removing rows with NA values
      valsTest <- valsTest[apply(valsTest, 1, function(x)!any(is.na(x))),]
  
 
  
  ## Select scenario for the test day
  test <- scenarioSet(id, valsTest)
  
  switch(method,
         'kt'={
           ## Computes the daily clearness index for the predictor set 'history'
           Bo0 <- history$Bo0
           swflx <- history$swflx.point
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
           ## Days included in `history`. Use `as.character` to work with
           ## the subset mechanism of xts
           days <- as.character(unique(as.Date(index(scnData))))
           ## Use only swflx variables
           idxRad <- grep('swflx', names(test))
           ## Compute KS distance between history and test for each day included
           ## in `history`
           ksD <- sapply(days, function(d) ksDistance(test[,idxRad], scnData[d,idxRad]))
           ## Average KS distance
           ksDMean <- apply(ksD, 2, mean, na.rm = TRUE)
           ## Order by decreasing KS distance
           best <- sort(ksDMean)
           ## The result is a set of `nDays` with the lowest KS distance.
           scnData <- as.zoo(scnData[names(best[seq_len(nDaysKT)])])
         })
  
  ## Prediction (q1, q5 and q9)
  pred <- rfPredict(test, scnData, nDays='all')
    
}
