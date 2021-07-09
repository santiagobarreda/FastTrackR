

plotffs.internal = function(ffs,xlim=NA,ylim=NA,xlab=NA,ylab=NA,cex=NA,lwd=NA,pch=NA,
                            main=NA,colors=NA,add=FALSE,...){

  if (is.na(colors[1]) & length(colors)==1) colors = c("darkgoldenrod2",3,4,2)
  if (is.na(xlab)) xlab="Time (ms)"
  if (is.na(ylab)) ylab = "Frequency (Hz)"
  if (is.na(cex)) cex = 1
  if (is.na(lwd)) mlwd = 1
  if (is.na(main)) main = ""
  if (is.na(pch[1])) pch = 1

  nf = 4
  if (!("f4" %in% colnames (ffs))) nf = 3

  if (!("time" %in% colnames (ffs))){
    w1 = attr (ffs,"w1")*1000
    timestep = attr (ffs,"timestep")*1000
    n = nrow (ffs)
    time = w1 + timestep*(0:(n-1))
  }
  if ("time" %in% colnames (ffs)) time = ffs$time*1000

  if (length(pch)==1) pch = rep (pch, 4)

  if (is.na(xlim[1])) xlim = range(time)
  if (is.na(ylim[1]) & nf==3) ylim =c(100,max (ffs$f3)+500)
  if (is.na(ylim[1]) & nf==4) ylim =c(100,max (ffs$f4)+500)

  if (!add) plot (0, type="n", xlim=xlim, ylim=ylim, xlab=xlab,
                  ylab=ylab,main=main, ...)

  graphics::points (time, ffs[,"f1"], pch=pch[1], col = colors[1],cex=cex)
  graphics::lines (time, ffs[,"f1"], col = colors[1],lwd=lwd)
  graphics::points (time, ffs[,"f2"], pch=pch[2], col = colors[2],cex=cex)
  graphics::lines (time, ffs[,"f2"], col = colors[2],lwd=lwd)
  graphics::points (time, ffs[,"f3"], pch=pch[3], col = colors[3],cex=cex)
  graphics::lines (time, ffs[,"f3"], col = colors[3],lwd=lwd)
  if (nf==4){
    graphics::points (time, ffs[,"f4"], pch=pch[4], col = colors[4],cex=cex)
    graphics::lines (time, ffs[,"f4"], col = colors[4],lwd=lwd)
  }
}



