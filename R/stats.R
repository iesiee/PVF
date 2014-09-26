## O = observations
## M = model

mo <- function(m, o) mean(o, na.rm = TRUE)
mm <- function(m, o) mean(m, na.rm = TRUE)

sdo <- function(m, o) sd(o, na.rm=TRUE)
sdm <- function(m, o) sd(m, na.rm=TRUE)

mbe <- function(m, o) mean(o - m, na.rm=TRUE)
mae <- function(m, o) mean(abs(o - m), na.rm=TRUE)
rmse <- function(m, o) sqrt(mean((o - m)^2, na.rm=TRUE))

nmbe <- function(m, o) mbe(m, o) / diff(range(o))
cvmbe <- function(m, o) mbe(m, o) / mean(o, na.rm=TRUE)

nmae <- function(m, o) mae(m, o) / diff(range(o))
cvmae <- function(m, o) mae(m, o) / mean(o, na.rm=TRUE)

nrmse <- function(m, o) rmse(m, o) / diff(range(o))
cvrmse <- function(m, o) rmse(m, o) / mean(o, na.rm=TRUE)


## Stone1993
tStone <- function(m, o) {
    N <- NROW(m)
    MBE <- mbe(m, o)
    RMSE <- rmse(m, o)
    sqrt((N-1) * MBE^2 /(RMSE^2 - MBE^2))
}

r2 <- function(m, o) cor(m, o)^2


## Apply a collection of statistics with only one call
stats <- function(m, o,
                  functions = c('mo', 'mm', 'sdo', 'sdm',
                      'mbe', 'mae', 'rmse',
                      'nmbe', 'cvmbe',
                      'nmae', 'cvmae',
                      'nrmse', 'cvrmse',
                      'r2','tStone')){
  sapply(functions,
         FUN=function(f) do.call(f, list(m, o)))
}

#####################
## Q1 and Q9 error
#####################

qqStats <- function(x, q1, q9){
    ## Area between quantiles
    diffQ1Q9 <- q9 - q1
    diffQ1Q9Sum <- sum(diffQ1Q9, na.rm = TRUE) / sum(x, na.rm = TRUE)
    diffQ1Q9Med <- median(diffQ1Q9)/max(diffQ1Q9)
    ## Observations below Q1
    ultraPQ1 <- q1 - x
    idxUPQ1 <- ultraPQ1 > 0
    ## number of occurrences
    ultraPQ1Num <- sum(idxUPQ1)
    ## Median of excess normalized with x
    ultraPQ1Med <- median(ultraPQ1[idxUPQ1] / x[idxUPQ1], na.rm = TRUE)

    ## Observations above Q9
    ultraPQ9 <- x - q9
    idxUPQ9 <- ultraPQ9 > 0
    ultraPQ9Num <- sum(idxUPQ9)
    ultraPQ9Med <- median(ultraPQ9[idxUPQ9] / x[idxUPQ9], na.rm = TRUE)

    ## Result
    q1q9 <- c(diffQ1Q9Sum, diffQ1Q9Med,
              ultraPQ1Num, ultraPQ1Med,
              ultraPQ9Num, ultraPQ9Med)
    names(q1q9) <- c('difSUM', 'difMED',
                     'uq1NUM', 'uq1MED',
                     'uq9NUM', 'uq9MED')
    return(q1q9)
}
