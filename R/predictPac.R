predictPac <- function(goal, history, id, nDays, method, lat, lon,...){
  
  goal <- as.Date(goal)
  
  ## Select scenario for the predictor set
  scnData <- scenarioSet(id, history)
  
  if(goal %in% unique(as.Date(index(history)))){  ## 'valsTest' created from 'history'
    
    valsTest <- window(history,
                       start=as.POSIXct(goal)+3600,
                       end=as.POSIXct(goal+1))
    
    ## Remove 'goal' from 'scnData' and 'history' for methods ks and kt
    ## not to choose the 'goal' day as the closest ks or kt
    if(method=='ks' | method=='kt'){
      Idx <- as.Date(index(history))
      scnData <- scnData[!(Idx %in% goal)]
      history <- history[!(Idx %in% goal)]
    }
    
  } else { ## 'valsTest' created with predVarsLocal()
    
    if(method=='previous') stop('Cannot use method previous when goal
                                is not inside history date limits!!!')
    
    ## NWP variables and sun geometry
    valsTest <- predVarsLocal(seqDays=goal, sun = TRUE, ...)
    valsTest <- na.omit(valsTest)
    ## Adding the hourly clearness index
    valsTest$kt <- valsTest$swflx.point/valsTest$Bo0
    valsTest$kt[!is.finite(valsTest$kt)] <- 0
  }
  
  ## Select scenario for the test day
  ## If 'valsTest' was created from 'history', the first column of 'test' is measured Pac
  ## If 'valsTest' was created with predVarsLocal(), test does not have measured Pac
  test <- scenarioSet(id, valsTest)
  
  switch(method,
         'previous'={
           N <- nDays
         },
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
           diffKTfiltered <- index(do.call(rbind,diffKTorder[1:nDays]))
           scnDataIdx <- as.Date(index(scnData))
           scnData <- scnData[scnDataIdx %in% diffKTfiltered]
           N <- 'all'
         },
         'ks'={
           ## Convert to xts to use its subset capabilities
           history <- as.xts(history)
           ## Days included in `scnData`. Use `as.character` to work with
           ## the subset mechanism of xts
           days <- as.character(unique(as.Date(index(history))))
           ## Use only swflx variables
           idxRad <- grep('swflx', names(valsTest))
           ## Compute KS distance between scnData and test for each day included
           ## in `scnData`
           ksD <- sapply(days, function(d) ksDistance(valsTest[,idxRad],
                                                      history[d,idxRad]))
           ## Average KS distance
           ksDMean <- apply(ksD, 2, mean, na.rm = TRUE)
           ## Order by decreasing KS distance
           best <- sort(ksDMean)
           ksDMeanfiltered <- names(best[seq_len(nDays)])
           ## The result is a set of `nDays` with the lowest KS distance.
           scnDataIdx <- as.Date(index(scnData))
           scnData <- scnData[scnDataIdx %in% as.Date(ksDMeanfiltered)]
           N <- 'all'
         })
  
  if(names(test)[1]=='Pac'){
    
    pred <- rfPredict(test[, -1], scnData, N)
    pred <- cbind(Pac = test$Pac, pred)
    
  } else {
    
    pred <- rfPredict(test, scnData, N)
    
  }
  
  return(pred)
  
}
