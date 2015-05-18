predictPac <- function(goal = NULL,
                       history,
                       scnID = 18,
                       nDays = '30',
                       method = 'ks',
                       typeRes = 'stats',
                       mc.cores = 1,
                       ...){
  
  
  ####################
  ## Define the days to be predicted ('workDays')
  ####################
  
  if(is.null(goal)){
    
    ## Defining all available days in 'history' (the 'workDays')
    ## The following eliminates empty days due to the 00h
    workDays <- unique(as.Date(index(history)-1800))
    ## The following eliminates empty days due to missing 01-23 hours
    workDaysAux <- unique(as.Date(index(history)+1800)) 
    workDays <- workDays[workDays %in% workDaysAux]
    ## The following eliminates last day, usualy incomplete
    workDays <- workDays[-length(workDays)]
    ## The following ensures that the number of training days before
    ## the test day is >= nDays for 'method' = previous
    if(method=='previous') workDays <- workDays[(nDays+1):length(workDays)]
  
  } else {
  
    goal <- as.Date(goal)
    workDays <- goal
  
  }
  
  
  ####################
  ## Compute the daily clearness index for 'history'
  ####################
  
  Bo0 <- history$Bo0
  swflx <- history$swflx.point
  Bo0d <- aggregate(Bo0, by = as.Date)
  G0d <- aggregate(swflx, by = as.Date)
  ktD <- G0d/Bo0d
  ktD[!is.finite(ktD)] <- 0
  
  
  ####################
  ## Predict for each day from 'workDays'
  ####################
  
  randForestList <- mclapply(workDays, FUN=function(workDay){
    
    message(workDay)
    
    
    ##########
    ## Create the predictor set 'scnData' from 'history'
    ##########
    
    scnData <- scenarioSet(scnID, history)
    
    
    ##########
    ## Create the test set 'tstData' from 'history' or create a new one
    ##########
    
    if(workDay %in% unique(as.Date(index(history)))){ ## 'valsTest' created from 'history'
      
      valsTest <- window(history,
                         start=as.POSIXct(workDay)+3600,
                         end=as.POSIXct(workDay+1))
            
    } else { ## 'valsTest' created with extractForecast()
      
      if(method=='previous') stop('Cannot use method previous when goal
                                  is not inside history date limits!!!')
      
      ## NWP variables and sun geometry
      ## Needs 'point', 'vrbls', 'aux.index, 'service', 'remote' and 'dataDir'
      valsTest <- extractForecast(start = workDay, end = workDay, sun = TRUE, ...)
      valsTest <- na.omit(valsTest)
      ## Adding the hourly clearness index
      valsTest$kt <- valsTest$swflx.point/valsTest$Bo0
      valsTest$kt[!is.finite(valsTest$kt)] <- 0
    }
          
    ## If 'valsTest' was created from 'history', the first column of 'tstData' is
    ## measured Pac, and if 'valsTest' was created with predVarsLocal(), 'tstData'
    ## does not have measured Pac
    tstData <- scenarioSet(scnID, valsTest)
    
    
    ##########
    ## Remove 'workDay' from 'scnData' and 'history' for methods ks and kt
    ## not to choose the 'workDay' day as the closest ks or kt
    ##########
    
    if(method=='ks' | method=='kt'){
      Idx <- as.Date(index(history))
      scnData <- scnData[!(Idx %in% workDay)]
      history <- history[!(Idx %in% workDay)]
    }
    
    
    ##########
    ## Prepare 'N' and 'scnData' according to 'method'
    ##########
    
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
    
    
    ##########
    ## Predict using rfPredict()
    ##########
    
    if(names(tstData)[1]=='Pac'){
      
      pred <- rfPredict(tstData[, -1], scnData, N)
      pred <- cbind(Pac = tstData$Pac, pred)
      
    } else {
      
      pred <- rfPredict(tstData, scnData, N)
      
    }
    
    
    ##########
    ## Compute statistics if 'typeRes' = stats
    ##########
    
    switch(typeRes,
           
           power = return(pred),

           stats = {
    
               if(sum(pred$Pac)==0 | sum(pred$q5)==0){
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
                   ## Compute error statistics considering only rows
                   ## with Pac > 0 and q5 > 0
                   error <- with(pred[(pred$Pac > 0 | pred$q5 > 0),],
                                 tdStats(m = q5, o = Pac))
                   ## Compute q1 vs Pac and q9 vs Pac
                   q1q9 <- with(pred[(pred$Pac > 0 | pred$q5 > 0),],
                                qqStats(Pac, q1, q9))
               }
               
               ## Joining the results
               ktD <- ktD[as.Date(workDay),]
               result <- zoo(t(c(error, q1q9, ktD)), workDay)
               names(result) <- c(names(error), names(q1q9), 'KT')
               
               ## KT class colum
               result$KTclass <- cut(result$KT, round(quantile(result$KT),2))
               
               return(result)
           })
    
}, mc.cores = mc.cores)
  

  randForest <- do.call(rbind, randForestList)
  
}
