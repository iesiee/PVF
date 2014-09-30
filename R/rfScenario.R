## Apply Random Forest to every day of 'history' separately. If nDays=0,
## it uses all the days but the test one as training. If nDays=N,
## it uses only the previous N days as training (the first N days
## of 'history' cannot be predicted).  The result has Pac, q1, q5 and q9
## columns (if typeRes='power') or error statistics, q1q9 and KT
## columns (if typeRes='stats').

rfScenario <- function(history, id, nDays, method, typeRes, mc.cores=1,...){
  
  ## Defining all available days in 'history' (the 'workDays')
  ## The following eliminates empty days due to the 00h
  workDays <- unique(as.Date(index(history)-1800))
  ## The following eliminates empty days due to missing 01-23 hours
  workDaysAux <- unique(as.Date(index(history)+1800)) 
  workDays <- workDays[workDays %in% workDaysAux]
  ## The following eliminates last day, usualy incomplete
  workDays <- workDays[-length(workDays)]
  ## The following ensures that the number of training days before
  ## the test day is >= nDays for method='previous'
  if(method=='previous') workDays <- workDays[(nDays+1):length(workDays)]
  
  ## Computes the daily clearness index for the predictor set 'history'
  Bo0 <- history$Bo0
  swflx <- history$swflx.point
  Bo0d <- aggregate(Bo0, by = as.Date)
  G0d <- aggregate(swflx, by = as.Date)
  ktD <- G0d/Bo0d
  ktD[!is.finite(ktD)] <- 0
  
  randForestList <- mclapply(workDays, FUN=function(workDay){
    
    message(workDay)
    
    pred <- predictPac(goal=workDay, history, id, nDays, method, mc.cores=mc.cores,...)
    
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
                    PVFstats(m = q5, o = Pac))
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
