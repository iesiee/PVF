## `data` must include these columns: nrmse, nmbe, sdm, sdo

targetDiagram <- function(data, class = '',
                          xlab = expression("RMSEc"%.%"sign("*sigma^"*"*")"),
                          ylab = 'MBE',
                          auto.key = list(space = 'right'),
                          default.scales = list(cex = 0.6),
                          scales = list(),
                          type = 'quantile', cuts = seq(0.25, 1, .25),
                          ...){
    
    ## Dismiss values of normalized RMSE above 1
    data <- data[data$nrmse <= 1,]

    ## RMSEc and difSD according to Joliff et al.
    data$nrmsec <- with(data, sqrt(nrmse^2 - nmbe^2))
    data$difSD <- with(data, sdm - sdo)

    ## Quantile Circles
    radius <- switch(type,
                     quantile = {
                             quantile(data$nrmse, probs = cuts, na.rm = TRUE)
                     },
                     at = cuts)

    circle <- expand.grid(theta = seq(0, 2*pi,length = 100),
                          r = radius)
    circle$x <- with(circle, r * sin(theta))
    circle$y <- with(circle, r * cos(theta))

    ## Generate graphic
    ff <- as.formula(paste('nmbe ~ nrmsec * sign(difSD)',
                           ifelse(class == '', '', paste('|', class))
                           )
                     )
    scales <- modifyList(default.scales, scales)
    
    xyplot(ff, 
           data = data,
           circle = circle,
           xlab = xlab, ylab = ylab,
           aspect='iso', scales = scales,
           xlim = extendrange(circle$x),
           ylim = extendrange(circle$y, f = 0.1),
           auto.key = auto.key,
           panel = function(..., circle){
               ## Vertical and Horizontal Axis
               panel.abline(h=0,v=0, lwd = 0.6, col='gray')
               ## Data
               panel.xyplot(...)
               ## Quantile circles
               panel.xyplot(circle$x, circle$y,
                            lwd = 0.6, type = 'l', col = 'darkgrey')
               ## Labels of circles
               panel.text(0, -radius, labels = signif(radius, 2),
                          pos = 1, offset = 0.05,
                          cex = scales$cex)
           }, ...)

}



