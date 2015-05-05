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
