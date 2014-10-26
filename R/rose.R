roseDiagram <- function(formula, data, ...){

    xyplot(formula, data = data, 
           xlim = c(-1, 1) * 1.1, ylim = c(-1, 1) * 1.1,
           xlab = '', ylab = '',
           type = 'b', pch = 21, aspect = 'iso',
           scales = list(draw = FALSE),
           auto.key = list(space = 'right'),
           panel = function(x, y, ...){
               x <- factor(x)
               nms <- levels(x)
               N <- nlevels(x)

               y <- c(y, y[1])
               theta <- seq(0, 2*pi, length = N+1)

               xtheta <- y * cos(theta)
               ytheta <- y * sin(theta)

               ## Axis
               xyAxis <- cbind(cos(theta), sin(theta))

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

               panel.xyplot(circle$x, circle$y,
                            lty = 2, type = 'l',
                            col = 'darkgrey')
               panel.text(c(0, .5, 1), 0, labels = c(0, .5, 1),
                          pos = 1, cex = 1)
               ## Data
               panel.xyplot(xtheta, ytheta, ...)
               ## Names of variables
               xyLim <- 1.15
               xyNames <- cbind(0.95 * xyLim * cos(theta[-(N+1)]),
                                0.95 * xyLim * sin(theta[-(N+1)]))
               
               panel.text(xyNames[,1], xyNames[,2], nms)
           },
           ...)
          
}

