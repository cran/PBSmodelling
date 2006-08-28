# Two linked populations (reserved and fished)
# R code for the Fishery Reserve example

# ************************************************************
# Functions that define and run the model, with parameters
# shared globally
# ************************************************************

# Harvest rate function (sinusoid across n periods)
hFun <- function(t,n,Fmin,Fmax) {
  Fmin + ( 1 + sin(2*pi*(t/n)) )* ((Fmax-Fmin)/2); };

# Model definition with shared global parameters
yModel <- function(t,y,p) {
  y1 <- y[1]; y2 <- y[2];
  Ft <- hFun(t,Fcy,Fmin,Fmax);
  dy1 <- r * y1 * (1 - (y1/K1)) + a * ((y2/K2) - (y1/K1));
  dy2 <- r * y2 * (1 - (y2/K2)) - a * ((y2/K2) - (y1/K1)) - Ft * y2;
  dy <- c(dy1=dy1,dy2=dy2);          # derivatives required for lsoda
  z <- c(dy1,dy2,Ft);                # values saved by lsoda
    names(z) <- c("dy1","dy2","F");
  list(dy,z); };

# Run model using shared global parameters
runModel <- function() {
  p <- getWinVal(scope="G",asvector=T);  # p used in lsoda call
  yinit <- c(y1f*K1,y2f*K2);
  tt <- seq(0,tmax,by=tstp);
  yout <<-
    lsoda(y=yinit,times=tt,func=yModel,parms=p,rtol=rtol,atol=atol);
  colnames(yout) <- c("t","y1","y2","dy1","dy2","F");
  ptype <- getWinVal("ptype");
  tt <- yout[,1]; y1 <- yout[,2]; y2 <- yout[,3];
  dy1 <- yout[,4]; dy2 <- yout[,5]; F <- yout[,6];
  resetGraph(); #expandGraph(mfrow=c(3,1));
  if(ptype=="t") {
    par(mfrow=c(3,1)); ymax=max(y1,y2);
    dymin=min(dy1,dy2); dymax=max(dy1,dy2);
    plot(tt,y1,ylim=c(0,ymax), xlab="t",ylab="B",type="n",
      main="Biomass B: Reserve (green) Fishery (red)");
      lines(tt,y1,col="green"); lines(tt,y2,col="red");
    plot(tt,dy1,ylim=c(dymin,dymax),
      xlab="t",ylab="dB/dt",type="n",main="Rate of change dB/dt");
      lines(tt,dy1,col="green"); lines(tt,dy2,col="red");
    plot(tt,F,xlab="t",ylim=c(0,Fmax),
      type="l",col="blue",main="Fishing Mortality F"); }
  if(ptype=="p") pairs(yout,pch=20,cex=.3);
  invisible(yout); };

# ************************************************************
# Functions required for the GUI
# ************************************************************

if (!require(odesolve, quietly=TRUE)) stop("The odesolve package is required for this example")
if (!require(PBSmodelling, quietly=TRUE)) stop("The PBSmodelling package is required for this example")
createWin("FishResWin.txt");
