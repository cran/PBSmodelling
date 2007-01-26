# ------------------------------------
# Plotting functions to report results
# ------------------------------------

plotTrace <- function(file,clrs=c("blue","red","green","magenta","navy"),...) {
   nc <- ncol(file)
   x  <- file[,1]; xlim <- range(x); ylim <- range(file[,2:nc])
   plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1)
   for (i in 2:nc) {
      y <- file[,i]
      lines(x,y,col=clrs[i-1])
   }
}

plotDens <- function(file,clrs=c("blue","red","green","magenta","navy"),...) {
   if (is.vector(file)) file <- matrix(file,ncol=1);
   nc <- ncol(file)
   dd <- density(unlist(file[,1:nc]),adjust=1.25); xlim <- range(dd$x,na.rm=T); ylim <- range(dd$y,na.rm=T)
   for (i in 1:nc) {
      d <- density(file[,i], adjust=1.25);
      xlim[1] <- min(xlim[1],min(d$x)); xlim[2] <- max(xlim[2],max(d$x));
      ylim[1] <- min(ylim[1],min(d$y)); ylim[2] <- max(ylim[2],max(d$y));
   }
   plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1)
   lines(dd$x,dd$y,col="grey",lwd=2)
   for (i in 1:nc) {
      y <- file[,i]; d <- density(y, adjust=1.25)
      lines(d$x,d$y,col=clrs[i])
   }
}

plotACF <- function(file,lags=20,clrs=c("blue","red","green","magenta","navy"),...) {
   if (is.vector(file)) file <- matrix(file,ncol=1);
   nc   <- ncol(file); nch <- nc; nr <- nrow(file); lags <- min(nr-1,lags);
   clim <- qnorm(c(.025,.975))/sqrt(nr);
   acfout <- acf(file,lag.max=lags,plot=F); acfacf <- acfout$acf
   ymin <- min(diag(apply(acfacf,2:3,min)),-.2); ymax <- max(diag(apply(acfacf,2:3,max)));
   xlim <- c(0,lags+.5); ylim <- c(ymin,ymax); ylim[1] <- min(ylim[1],clim[1]); ylim[2] <- max(ylim[2],clim[2]);
   plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1)
   if (lags<=30) axis(1,at=1:30,tcl=.25,label=F);
   abline(h=clim,col="#400080",lty=2);
   for (i in 1:nc) {
      x <- (0:lags)+(i-1)*(.7/nch); y <- acfacf[,i,i];
      lines(x,y,type="h",lwd=3,col=clrs[i])
   }
   abline(h=0,col="grey40",lty=3); box();
}
