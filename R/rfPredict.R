rfPredict <- function(test, history, nDays){
  ## 'test' contains only the predictor set
  ## 'history' contains the predictor set and the output variable in the first column
  n <- as.character(nDays)
  switch(n,
         '0'={
           train <- history[which(!(index(history) %in% index(test))),]
         },
         'all'={
           train <- history
         },{
           d <- unique(as.Date(index(test)))[1]
           train <- window(history,
                           start = as.POSIXct(d-nDays)+3600,
                           end = as.POSIXct(d))
         })
  qrfModelH <- quantregForest(train[,-1], as.numeric(train[,1]))
  qrfPredH <- predict(qrfModelH, test)
  colnames(qrfPredH) <- c('q1', 'q5', 'q9')
  qrfPredH <- zoo(qrfPredH, index(test))
}
