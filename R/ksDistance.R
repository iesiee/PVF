## Function to compute the Kolmogorov-Smirnov distance

ksDistance <- function(x, y){
  N <- ncol(x)
  D <- sapply(seq_len(N), function(i){
    ks <- suppressWarnings(
      ks.test(as.vector(x[,i]), as.vector(y[,i]))
    )
    ks$statistic
  })
  D
}