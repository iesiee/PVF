## `data` must include these columns: nrmse, nmbe, sdm, sdo

targetDiagram <- function(data, class = '',
                          xlab = expression("RMSEc"%.%"sign("*sigma^"*"*")"),
                          ylab = 'MBE',
                          auto.key = list(space = 'right'),
                          ...){
    
    ## Dismiss values of normalized RMSE above 1
    data <- data[data$nrmse <= 1,]

    ## RMSEc and difSD according to Joliff et al.
    data$nrmsec <- with(data, sqrt(nrmse^2 - nmbe^2))
    data$difSD <- with(data, sdm - sdo)

    ## Quantile Circles
    radius <- quantile(data$nrmse, probs = seq(.25, 1, .25), na.rm = TRUE)
    circle <- expand.grid(theta = seq(0, 2*pi,length = 100),
                          r = radius)
    circle$x <- with(circle, r * sin(theta))
    circle$y <- with(circle, r * cos(theta))

    ## Generate graphic
    ff <- as.formula(paste('nmbe ~ nrmsec * sign(difSD)',
                           ifelse(class == '', '', paste('|', class))
                           )
                     )

    xyplot(ff, 
           data = data,
           circle = circle,
           xlab = xlab, ylab = ylab,
           aspect='iso', scales = list(x = list(draw = FALSE)),
           auto.key = auto.key,
           panel = function(..., circle){
               ## Vertical and Horizontal Axis
               panel.abline(h=0,v=0,lty=2,col='gray')
               ## Quantile circles
               panel.xyplot(circle$x, circle$y,
                            lty = 2, type = 'l', col = 'darkgrey')
               ## Labels of circles
               panel.text(0, -radius, labels = signif(radius, 1),
                          pos = 4, cex = 0.6)
               ## Data
               panel.xyplot(...)
           }, ...)

}



