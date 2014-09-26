PVFTheme <- function(pch=19, cex=0.7,
                     symbol = brewer.pal(n=8, name = "Dark2"),
                     ...){
  theme <- custom.theme.2(pch=pch, cex=cex, ...)
  theme$strip.background$col='transparent'
  theme$strip.shingle$col='transparent'
  theme$strip.border$col='transparent'
  theme$add.line$lwd=.4
  theme
}

xscale.pvf <- function(...){
  ans <- xscale.components.default(...)
  ans$top=FALSE
  ans}

yscale.pvf <- function(...){
  ans <- yscale.components.default(...)
  ans$right=FALSE
  ans}

## `data` must include these columns: nrmse, nmbe, sdm, sdo, KTclass and Scn.
targetDiagram <- function(data, class = 'KTclass', groups = 'Scn',
                          par.settings = PVFTheme,
                          xscale.components = xscale.pvf,
                          yscale.components = yscale.pvf,
                          ...){
    ## Dismiss values of normalized RMSE above 1
    data[data$nrmse > 1,] <- NA
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
    ff <- as.formula(paste('nmbe ~ nrmsec * sign(difSD) |', class))
    xyplot(ff, groups = data[,groups],
           data = data, circle = circle,
           xlab = expression("RMSEc"%.%"sign("*sigma^"*"*")"), 
           ylab = "MBE",
           between = list(x = 0.5, y = 0.2),
           as.table = TRUE,
           aspect='iso', scales = list(x = list(draw = FALSE)),
           par.settings = par.settings,
           auto.key = list(space = 'right'),
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



