## Apply Random Forest to every day of 'scnData' separately. If nDays=0,
## it uses all the days but the test one as training. If nDays=N,
## it uses only the previous N days as training (the first N days
## of 'scnData' cannot be predicted).  The result has Pac, q1, q5 and q9
## columns (if typeRes='power') or error statistics, q1q9 and KT
## columns (if typeRes='stats').

rfScenario <- function(id, vals, nDays, typeRes, nDaysKT, method, mc.cores){
  
  ## Computes the daily clearness index for the predictor set 'vals'
  Bo0 <- vals$Bo0
  swflx <- vals$swflx.point
  Bo0d <- aggregate(Bo0, by = as.Date)
  G0d <- aggregate(swflx, by = as.Date)
  ktD <- G0d/Bo0d
  ktD[!is.finite(ktD)] <- 0
  
  ## Select scenario for the predictor set
  scnData <- scenarioSet(id, vals)
  
  ## Defining all available days in 'scnData' (the 'workDays')
      ## The following eliminates empty days due to the 00h
      workDays <- unique(as.Date(index(scnData)-1800))
      ## The following eliminates empty days due to missing 01-23 hours
      workDaysAux <- unique(as.Date(index(scnData)+1800)) 
      workDays <- workDays[workDays %in% workDaysAux]
      ## The following eliminates last day, usualy incomplete
      workDays <- workDays[-length(workDays)]
      ## The following ensures that the number of training days before
      ## the test day is >= nDays
      if(!(nDays=='all')) workDays <- workDays[(nDays+1):length(workDays)]
  
  randForestList <- mclapply(workDays, FUN=function(workDay){
    message(workDay)
    
    ## Computes prediction
    valsTest <- window(vals,
                       start = as.POSIXct(workDay)+3600,
                       end = as.POSIXct(workDay + 1))
    
    if(nDays=='all'){
      
      ## Select scenario for the test day
      test <- scenarioSet(id, valsTest)
      
      ## Remove 'workDay' from 'scnData'
      scnDataIdx <- as.Date(index(scnData))
      scnData2 <- scnData[!(scnDataIdx %in% workDay)]
      
      switch(method,
             'kt'={
               ## Computes the daily clearness index for the test day
               ktTest <- sum(valsTest$swflx.point)/sum(valsTest$Bo0)
               ## Find days with similar clearness index in the predictor set
               diffKT <- abs(ktD-ktTest)
               orderKT <- order(diffKT, decreasing = FALSE, na.last = TRUE)
               diffKTorder <- lapply(orderKT, FUN=function(i) diffKT[i])
               ## +1 due to ktD
               diffKTfiltered <- index(do.call(rbind,diffKTorder[1:(nDaysKT+1)]))
               scnData2Idx <- as.Date(index(scnData2))
               scnData2 <- scnData2[scnData2Idx %in% diffKTfiltered]
             },
             'ks'={
               ## Convert to xts to use its subset capabilities
               scnData2 <- as.xts(scnData2)
               ## Days included in `history`. Use `as.character` to work with
               ## the subset mechanism of xts
               days <- as.character(unique(as.Date(index(scnData2))))
               ## Use only swflx variables
               idxRad <- grep('swflx', names(test))
               ## Compute KS distance between history and test for each day included
               ## in `history`
               ksD <- sapply(days, function(d) ksDistance(test[,idxRad],
                                                          scnData2[d,idxRad]))
               ## Average KS distance
               ksDMean <- apply(ksD, 2, mean, na.rm = TRUE)
               ## Order by decreasing KS distance
               best <- sort(ksDMean)
               ## The result is a set of `nDays` with the lowest KS distance.
               scnData2 <- as.zoo(scnData2[names(best[seq_len(nDaysKT)])])
             })
            
      pred <- rfPredict(test[,-1], scnData2, nDays)
      
    } else {
      
      ## Select scenario for the test day
      test <- scenarioSet(id, valsTest)
      
      pred <- rfPredict(test[,-1], scnData, nDays)
      
    }
    
    pred <- cbind(Pac=test[,1], pred)
    
    switch(typeRes,
           power = return(pred),
           stats = {
    
    if(sum(pred$Pac)==0|sum(pred$q5)==0){
      error <- rep(NA, 15)
      names(error) = c('mo', 'mm', 'sdo', 'sdm',
                       'mbe', 'mae', 'rmse',
                       'nmbe', 'cvmbe',
                       'nmae', 'cvmae',
                       'nrmse', 'cvrmse',
                       'r2','tStone')
      q1q9 <- rep(NA, 6)
      names(q1q9) <- c('difSUM', 'difMED',
                       'uq1NUM', 'uq1MED',
                       'uq9NUM', 'uq9MED')
    } else{
      ## Compute error statistics considering only rows with Pac > 0 and q5 > 0
      error <- with(pred[(pred$Pac > 0 | pred$q5 > 0),],
                    stats(m = q5, o = Pac))
      ## Compute q1 vs Pac and q9 vs Pac
      q1q9 <- with(pred[(pred$Pac > 0 | pred$q5 > 0),],
                   qqStats(Pac, q1, q9))
    }
    
    ## Joining the results
    ktD <- ktD[as.Date(workDay),]
    result <- zoo(t(c(error, q1q9, ktD)), workDay)
    names(result) <- c(names(error), names(q1q9), 'KT')
    
    return(result)
    
           })
    
  }, mc.cores = mc.cores)
  
  randForest <- do.call(rbind, randForestList)
  
  if(typeRes=='stats') randForest$KTclass <- cut(randForest$KT,
                                                 round(quantile(randForest$KT),2))
  
  return(randForest)
  
}
