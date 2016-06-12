## ----fig.width=7, fig.retina=2, fig.align='left', fig.cap="Rectangular Cartogram of the U.S. election 2004; The area corresponds to the number of electors (color indicates the party red: democrats / blue: republican; the color intensity ~ outcome of the vote.). The graphic was computed by using the original implementation of the construction heuristic RecMap MP2 introduced in [@recmap].", echo=FALSE, warning=FALSE, comment="ccc", error=FALSE, message=FALSE----

library(recmap)
op<-par(mar=c(0,0,0,0), bg='black')
recmap:::.draw_recmap_us_state_ev()
par(op)

## ------------------------------------------------------------------------
usa <- data.frame(x=state.center$x, 
    y = state.center$y, 
    # make the rectangles overlapping by correcting lines of longitude distance
    dx = sqrt(state.area) / 2 / (0.8 * 60 * cos(state.center$y*pi/180)), 
    dy = sqrt(state.area) / 2 / (0.8 * 60) , 
    z = sqrt(state.area),
    name = state.name)

## ----fig.width=7, fig.height=3-------------------------------------------
op<-par(mfrow=c(1,1), mar=c(0,0,0,0))
library(recmap)
plot_recmap(M <- usa[!usa$name %in% c("Hawaii", "Alaska"), ],  
            col.text = 'black', lwd=2)

## ----echo=FALSE, fig.width=4, fig.height=3, fig.align='center', fig.retina=3, fig.cap="`a.x = 2, a.y = -5, a.dx = 20, a.dy = 5, b.dx = 1.5, b.dy = 5`"----

draw_and_place_rectangle <- function(alpha=0.0, a.x=2, a.y=-5, a.dx = 20, 
                                     a.dy = 5, b.dx=1.5, b.dy=5, ...){
   
   rect(a.x - a.dx - b.dx, a.y - a.dy - b.dy, 
        a.x + a.dx + b.dx, a.y + a.dy + b.dy, 
        lwd=3, border='grey')
  
   rect(a.x - a.dx, a.y - a.dy, 
        a.x + a.dx, a.y + a.dy)
   
  
  c <- recmap:::place_rectanle(a.x, a.y, a.dx, a.dy, b.dx, b.dy, alpha)

   rect(c$x - c$dx, c$y - c$dy, c$x + c$dx, c$y + c$dy, ...)
}

op <- par(mar=c(4, 4, 0.5, 0.5))
plot(0,0 , 
     xlim=c(-35,25), ylim=c(-35,25), 
     asp=1, xlab='x return value', ylab='y return value', axes=FALSE); abline(v=0,h=0)

n <- 90

cm <- rainbow(n, alpha = 0.5)
alpha <- seq(-pi, pi, length = n)
r <- lapply(1:n,
            function(idx){
    draw_and_place_rectangle(alpha[idx], 
                             col=cm[idx], 
                             border='#88888877')
              })
  
legend("bottomleft", 
       as.character(round(seq(-pi,pi, length=9),2)), 
       pch=22, 
       bty='n',
       fill=rainbow(9, alpha=0.5),
       border=rainbow(9, alpha=0.5),
       col=rainbow(9, alpha=0.5),
       title=expression(paste(alpha, " argument", sep=" ")),
       cex=0.5,horiz = FALSE)


par(op)

## ------------------------------------------------------------------------
head(Cartogram <- recmap(Map <- usa[!usa$name %in% c("Hawaii", "Alaska"), ]))

## ----fig.width=8,  fig.height=4.5, fig.align='left', fig.cap="Rectangular Map Approximation."----

smp<- c(29, 22, 30, 3, 17, 8, 9, 41, 18, 15, 38, 35, 21, 23, 19, 6, 31, 32, 20, 
        28, 48, 4, 13, 14, 42, 37, 5, 16, 36 , 43, 25, 33, 12, 7, 39, 44, 2, 47,
        45, 46, 24, 10, 1,11 ,40 ,26 ,27 ,34)
plot_recmap(Cartogram.Population <- recmap(M[smp, ]), 
            col.text = 'black', lwd=2)

## ----fig.width=8, fig.height=4, fig.align='left', fig.cap="Area ~ population estimate as of July 1, 1975;"----

op<-par(mfrow=c(1,1), mar=c(0,0,0,0))
usa$z <- state.x77[, 'Population']
M <- usa[!usa$name %in% c("Hawaii", "Alaska"), ]
plot_recmap(Cartogram.Population <- recmap(M[order(M$x),]), 
            col.text = 'black', lwd=2)


## ----fig.width=8, fig.height=4, fig.align='left', fig.cap="Area ~ population estimate as of July 1, 1975; a better index order has been chosen to minimize the relative position error."----
op<-par(mfrow=c(1,1), mar=c(0,0,0,0))
# index order

smp <- c(20,47,4,40,9,6,32,33,3,10,34,22,2,28,15,12,39,7,42,45,19,13,43,30,24,
         25,11,17,37,41,26,29,21,35,8,36,14,16,31,48,46,38,23,18,1,5,44,27)

plot_recmap(Cartogram.Population <- recmap(M[smp,]), col.text = 'black', lwd=2)

## ----fig.width=8, fig.height=4, fig.align='left', fig.cap="Area ~ capita income (1974);"----
op<-par(mfrow=c(1,1), mar=c(0,0,0,0))
usa$z <- state.x77[, 'Income']
M <- usa[!usa$name %in% c("Hawaii", "Alaska"), ]
plot_recmap(Cartogram.Income <- recmap(M[order(M$x),]), col.text = 'black', lwd=2)

## ----fig.width=8, fig.height=4, fig.align='left', fig.cap="Area ~ mean number of days with minimum temperature below freezing (1931–1960) in capital or large city;"----
op<-par(mfrow=c(1,1), mar=c(0,0,0,0))
usa$z <- state.x77[, 'Frost'] 
M <- usa[!usa$name %in% c("Hawaii", "Alaska"), ]
plot_recmap(Cartogram.Income <- recmap(M[order(M$x),]), 
            col.text = 'black', lwd=2)

## ----fig.width=7, fig.height=2.5, fig.align='center', fig.retina=2, fig.cap="checkerboard fun - input, area of black regions have to be four times as big as white regions (left); solution found by a greedy random algorithm (middle); solution found by genetic algorithm (right)", fig.align='left'----
op<-par(mar=c(0,0,0,0), mfrow=c(1, 3), bg='white')

plot_recmap(checkerboard8x8 <- recmap:::.checkerboard(8),
            col=c('white','white','white','black')[checkerboard8x8$z])

# found by a greedy randomized search
index.greedy <- c(8, 56, 18, 5, 13, 57, 3, 37, 62, 58, 7, 16, 40, 59, 17, 34,
                  29, 41, 46, 27, 54, 43, 2, 21, 38, 52, 31, 20, 28, 48, 1, 22,
                  55, 11, 25, 19, 50, 10, 24, 53, 47, 30, 45, 44, 32, 35, 51,
                  15, 64, 12, 14, 39, 26, 6, 42, 33, 4, 36, 63, 49, 60, 61, 9,
                  23)

plot_recmap(Cartogram.checkerboard8x8.greedy <- recmap(checkerboard8x8[index.greedy,]),
            col=c('white','white','white','black')[Cartogram.checkerboard8x8.greedy$z])

# found by a genetic algorithm
index.ga <- c(52, 10, 27, 63, 7, 20, 32, 18, 47, 28, 6, 55, 11, 61, 38, 50, 5,
              21, 36, 34, 2, 22, 3, 1, 29, 57, 43, 4, 51, 58, 31, 49, 44, 25,
              59, 33, 17, 40, 8, 41, 26, 37, 19, 56, 45, 35, 62, 53, 24, 64,
              30, 15, 39, 12, 60, 48, 16, 23, 46, 42, 13, 54, 14, 9)

plot_recmap(Cartogram.checkerboard8x8.ga <- recmap(checkerboard8x8[index.ga,]),
            col=c('white','white','white','black')[Cartogram.checkerboard8x8.ga$z])


## ------------------------------------------------------------------------
sessionInfo()

