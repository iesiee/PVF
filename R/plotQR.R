plotQR <- function(z, vrbl='ground', col=c('black', 'red'),...){
    if (is.null(vrbl)){
        p <- xyplot(z[,'q5'],
                    ylim=c(min(z$q1), max(z$q9)),
                    col = col,
                    key=list(text=list('q5'),
                        lines=list(col='black'),
                        corner = c(0, 0),
                        x = 0.8, y = 0.8))
    } else {
        p <- xyplot(z[,c('q5', vrbl)],
                    auto.key = list(
                        corner = c(0, 0),
                        x = 0.8, y = 0.8),
                    ylim=c(min(z$q1), max(z$q9)),
                    superpose=TRUE, col=col)
    }
    myData <- as.data.frame(z)
    myData$tt <- as.numeric(index(z))
    ly <- layer_({
        qArea <- c(q1, rev(q9))
        tt <- c(tt, rev(tt))
        panel.polygon(tt, qArea,
                      border='lightgray', col='lightgray')
    }, data=myData)
    p + ly
}

