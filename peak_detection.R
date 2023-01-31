library(zoo)
argmax <- function(x, y, w=1, ...) {
  require(zoo)
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- rollapply(zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

test <- function(w, span) {
  peaks <- argmax(x, y, w=w, span=span)
  png(file=paste0(d,"_Sg_peak_",bScore,".png"), width = 350, height = 350)
  plot(x, y, cex=0.5, col="Gray", main=paste("Date = ", MMDDYYYY, ", Bad score = ", bScore, sep=""),
  xlab ="Bending angle (rad)",ylab="Pressure (mb)",ylim = rev(range(y)),yaxt='n',xaxt='n')
  axis(side=2, las =2,at=seq(0, 1000, by=250))
  axis(side=1, at=seq(0, 0.03, 0.002))
  lines(x, peaks$y.hat,  lwd=2) #$
  y.min <- min(y)
  #sapply(peaks$i, function(i) lines(c(x[i],x[i]), c(y.min, peaks$y.hat[i]), col="Red", lty=2))
  points(x[peaks$i], peaks$y.hat[peaks$i], col="Red", pch=19, cex=1.25)
  text(x[peaks$i], peaks$y.hat[peaks$i],
       labels = paste(round(peaks$y.hat[peaks$i],0)),
       cex = 1, pos = 3, col = "red")
  dev.off()
}
test(2, 0.05)
