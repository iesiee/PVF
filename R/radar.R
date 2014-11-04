panel.radar <- function(x, y, type = 'b',...){
    x <- factor(x)
    nms <- levels(x)
    N <- nlevels(x)

    theta <- 2*pi*(as.numeric(x) - 1)/N
    xtheta <- y * cos(theta)
    ytheta <- y * sin(theta)

    ##maxY <- max(y, na.rm = TRUE)
    maxY <- max(current.panel.limits()$ylim)/1.1
    ## Axis
    thetaAxis <- seq(0, 2*pi, length = N+1)
    xyAxis <- maxY * cbind(cos(thetaAxis), sin(thetaAxis))

    apply(xyAxis, 1, FUN = function(xy){
        panel.lines(c(0, xy[1]), c(0, xy[2]),
                    lwd = .5, col = 'darkgray')
    })
    ## "Circles"
    radius <- pretty(c(0, maxY))
    radius <- radius[radius <= maxY]
    
    circle <- expand.grid(theta = thetaAxis,
                          r = radius)
    circle$x <- with(circle, r * cos(theta))
    circle$y <- with(circle, r * sin(theta))

    panel.xyplot(circle$x, circle$y, type = 'l',
                 lwd = 0.5, col = 'darkgrey')
    panel.text(radius, 0, labels = radius,
               pos = 1, cex = .7)
    
    ## Data
    mypanel <- function(x, y, ..., type = type){
        panel.xyplot(c(x, x[1]), c(y, y[1]), ..., type = type)}
    panel.superpose(xtheta, ytheta, panel.groups=mypanel, ..., type = type)

    ## Names of variables
    xyNames <- maxY * cbind(cos(thetaAxis[-(N+1)]),
                            sin(thetaAxis[-(N+1)]))

    ## adjX <- c(1, 1, 0, 0)
    ## adjY <- c(1, 1, 0, 0)
    ## inCircle <- findInterval(thetaAxis[-(N+1)], c(0, pi/2, pi, 3*pi/2, 2*pi))
    ## posLab <- cbind(adjX[inCircle], adjY[inCircle])
    panel.text(xyNames[,1], xyNames[,2], nms, cex = 0.65)
}

prepanel.radar <- function(x, y, ...){
    maxY <- 1.1 * max(y, na.rm = TRUE)
    list(xlim = c(-maxY, maxY),
         ylim = c(-maxY, maxY)
         )
}

radarDiagram <- function(formula, data,
                         auto.key = list(space = 'right',
                             points = FALSE, lines = TRUE),
                         ...){

    xyplot(formula, data = data, 
           xlab = '', ylab = '', type = 'l',
           pch = 21, aspect = 'iso',
           scales = list(draw = FALSE, relation = 'same'),
           auto.key = auto.key,
           prepanel = prepanel.radar,
           panel = panel.radar,
           ...)
          
}

