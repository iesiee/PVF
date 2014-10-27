panel.radar <- function(x, y, type = 'b',...){
    x <- factor(x)
    nms <- levels(x)
    N <- nlevels(x)

    theta <- 2*pi*(as.numeric(x) - 1)/N
    xtheta <- y * cos(theta)
    ytheta <- y * sin(theta)
    
    ## Axis
    thetaAxis <- seq(0, 2*pi, length = N+1)
    xyAxis <- cbind(cos(thetaAxis), sin(thetaAxis))

    apply(xyAxis, 1, FUN = function(xy){
        panel.lines(c(0, xy[1]), c(0, xy[2]),
                    lwd = .5, col = 'darkgray')
    })
    ## Circles
    radius <- seq(.1, 1, by = .1)
    circle <- expand.grid(theta = seq(0, 2*pi, length = 100),
                          r = radius)
    circle$x <- with(circle, r * sin(theta))
    circle$y <- with(circle, r * cos(theta))

    panel.xyplot(circle$x, circle$y, type = 'l',
                 lwd = 0.5, col = 'darkgrey')
    panel.text(c(0, .5, 1), 0, labels = c(0, .5, 1),
               pos = 1, cex = 1)
    
    ## Data
    mypanel <- function(x, y, ..., type = type){
        panel.xyplot(c(x, x[1]), c(y, y[1]), ..., type = type)}
    panel.superpose(xtheta, ytheta, panel.groups=mypanel, ..., type = type)

    ## Names of variables
    xyLim <- 1.15
    xyNames <- cbind(0.95 * xyLim * cos(thetaAxis[-(N+1)]),
                     0.95 * xyLim * sin(thetaAxis[-(N+1)]))
               
    panel.text(xyNames[,1], xyNames[,2], nms, cex = 0.8)
}

radarDiagram <- function(formula, data, ...){

    xyplot(formula, data = data, 
           xlim = c(-1, 1) * 1.2, ylim = c(-1, 1) * 1.2,
           xlab = '', ylab = '', type = 'l',
           pch = 21, aspect = 'iso',
           scales = list(draw = FALSE),
           auto.key = list(space = 'right', points = FALSE, lines = TRUE),
           panel = panel.radar,
           ...)
          
}

