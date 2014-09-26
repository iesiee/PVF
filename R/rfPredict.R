## Returns q1, q5 and q9 for the variable corresponding to the first
## column of 'history', using 'test' as input. If nDays=N, prediction is
## computed using a train set of N days from 'history' before the test
## day (the number of days before the test day must be >= N). When
## nDays=0, prediction is computed using all the days from history,
## but the test day. When nDays='all', prediction is computed using all
## the days from history.

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
