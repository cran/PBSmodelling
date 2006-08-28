# Catch-curve model based on Schnute, J.T., and Haigh, R. (in review).
# Compositional analysis of catch curve data with an application to Sebastes maliger.
# ICES Journal of Marine Science.
# Code allows user to perform frequentist (NLM) and Bayesian (BRugs) analyses.

ccaMod  <- function(P) {
   # Catch-Curve Model - Schnute & Haigh 2006
   # ----------------------------------------
   Z<-P[1]; alpha<-P[2]; betak<-P[3]; tau<-P[4]; rho<-P[5:9]
   unpackList (FP,scope="L")
   a      <- k:B
   beta   <- rep(1,length(a))
   Ra     <- rep(1,length(a))
   zk     <- a < b0
   if (any(zk==T))
      beta[zk] <- 1 - (1-betak) * ((b0-a[zk])/(b0-k))^alpha; names(beta) <- a
   zrho   <- match(bi,b5)
   if (length(zrho)>0) {
      rtemp  <- NULL
      for (i in zrho)
         rtemp <- rbind( rtemp, rho[i] * exp(-.5*((a-b5[i])/tau)^2) )
      Ra     <- Ra + apply(rtemp,2,sum,na.rm=T)
   }
   Sa     <- exp(-Z*(a-k))
   quack  <- beta * Ra * Sa
   pa     <- quack / sum(quack)
   zA     <- a >= A
   pA     <- sum(pa[zA]); names(pA) <- A
   paA    <- c(pa[!zA],pA)
   if (round(sum(paA),5)!=1) stop("paA scewy")
   return(paA)
}

Uget <- function() {
   # Get user's data
   # ---------------
   getWinVal(scope="L")
   fnam  <- paste("CCA",spp,type,sep=".")
   source(paste(fnam,"r",sep="."))
   Afile <- get(fnam);
   afile <<- data.frame(age=as.numeric(dimnames(Afile)[[1]]),pa=Afile[,as.character(year)])

   len  <- 5
   flds <- dimnames(Afile)[[2]]; nflds <- length(flds); fldc <- paste(flds,",",sep="")
   temp <- rep("",ceiling(nflds/len)*len); temp[1:nflds] <- fldc
   temp <- matrix(temp,nrow=len,byrow=F);  temp <- rbind(temp,rep("\n",ncol(temp)))
   temp <- paste(as.vector(temp),collapse=""); temp <- substring(temp,1,nchar(temp)-2)
   msg  <- paste("Dim = ",paste(dim(Afile),collapse=" x "),",  Years:",sep="")
   msg  <- paste(msg,temp,sep="\n")

   ii <- dimnames(afile)[[2]]
   x  <- afile[,1]; y  <- afile[,2]
   ysum  <- sum(y); ymax <- max(y); xmax <- max(x);
   mfile <- afile[rev(order(afile[,2])),];
   xmod <- mfile[1:2,1]; ymod <- mfile[1:2,2];
   z <- order(xmod); xmod <- xmod[z]; ymod <-ymod[z]; phi[7:8] <- xmod;

   par(mfrow=c(1,1),mai=c(.6,.6,.1,.1),omi=c(0,0,0,0))
   plot(x,y,xlim=c(0,xmax),ylim=c(0,ymax),type="h",xlab="",ylab="",xaxt="n",cex=.7,las=1,adj=1,mgp=c(0,.4,0),tck=.02)
   lines(xmod,ymod,type="h",col="red",lwd=2)
   text(xmod,ymod+.02*ymax,xmod,col="red",cex=1)
   axis(1,at=seq(0,xmax,10),cex.axis=1,mgp=c(0,.5,0),tck=-.02)
   axis(1,at=1:xmax,labels=F,tck=.01); axis(1,at=seq(0,xmax,5),labels=F,tck=.02);
   addLabel(.95,.92,paste(fnam,"\nyear = ",year,sep="",collapse=""),cex=1.2,col="#400080",adj=1)
   addLabel(.95,.85,paste("n =",ysum),cex=1.2,col="blue",adj=1)
   mtext("Age",side=1,line=1.75,cex=1.5)
   mtext("Frequency",side=2,line=1.75,cex=1.5)

   setWinVal(list(allflds=msg,phi=phi))
}

Uset <- function() {
   # Get and set user's settings
   # ---------------------------
   getWinVal(scope="L")
   #if (phi[1,"b0"]<=phi[1,"k"]) {onoff[2:3] <- c(F,F)} else {onoff[2:3] <- c(T,T)}
   b5   <- phi[7:11]; bi <- b5[onoff[7:11]]
   #if (length(bi)==0) {onoff[4] <- F} else {onoff[4] <- T}
   dvec <- phi[1:6]
   tnam <- dimnames(phi)[[2]][1:6]
   FP   <- as.list(dvec); names(FP) <- tnam;
   m    <- length(bi); FP[["m"]] <- m; phi[[1,"m"]] <- m
   pnam <- dimnames(theta)[[2]];
   FP   <- c(FP,list(b5=b5,bi=bi,pnam=pnam))

   unpackList(FP,scope="L")

   if (!exists("afile",where=1)) Uget();

   Ufile <- get("afile",pos=1)
   Ufile <- Ufile[Ufile[,"age"]>=k & !is.na(Ufile[,"age"]) & !is.na(Ufile[,"pa"]),]
   pa    <- rev(Ufile[,"pa"]); a <- rev(Ufile[,"age"]); zz <- cumsum(pa) > 0;
   pa    <- rev(pa[zz]); a <- rev(a[zz]); names(pa) <- a;
   nspec <- sum(pa); pa <- pa/nspec

   FP    <- c(FP,list(nspec=nspec,ages.raw=a,pa.raw=pa))

   amax  <- max(a,na.rm=T)
   FP    <- c(FP,list(amax=amax))

   if (autoA) {
      zA    <- rev(cumsum(rev(pa)) < epsilon)
      if (all(zA==F)) A <- amax
      else            A <- a[zA][1] - 1
      if (!is.null(bi) && A<max(bi))
         stop("Age of plus class A lower than maximum stated anomaly age bi")
      FP[["A"]] <- A; phi[1,"A"] <- A
   ; #x1 <- pa[zz]; x2 <- pa[length(x1)+1]
   }
   zA    <- a >= A
   pA    <- sum(pa[zA]); names(pA) <- A
   paA   <- c(pa[!zA],pA)

   pa.obs <- paA
   attr(pa.obs,"input") <- c(k,A,B,b0,epsilon,bi)
   FP    <- c(FP,list(pa.obs=pa.obs))

#  Grouping
   pi.obs    <- NULL; temp <- 0
   for (i in 1:length(paA)) {
      temp <- temp + paA[i]
      if (temp >= epsilon) {
         names(temp) <- names(paA[i])
         pi.obs   <- c(pi.obs,temp)
         temp <- 0
      }
   }
   g  <- length(pi.obs);  i <- 1:g
   FP <- c(FP,list(g=g))

   PA <- array(NA,dim=c(length(paA),2),dimnames=list(names(paA),c("age","pa")))
   PA[names(paA),"age"] <- as.numeric(names(paA))
   PA[names(paA),"pa"]  <- paA

   PI <- array(NA,dim=c(g,2),dimnames=list(names(pi.obs),c("age","pi")))
   PI[names(pi.obs),"age"]  <- as.numeric(names(pi.obs))
   PI[names(pi.obs),"pi"]   <- pi.obs
   acut  <- c(k-1,as.numeric(names(pi.obs))); acut[length(acut)] <- amax
   FP    <- c(FP,list(acut=acut))
   FP    <- c(FP,list(pi.obs=pi.obs))

   idxM <- c(T,T,T,T,F,F,T,T,T,T,T) & onoff
   idxD <- c(T,T,T,T,F,T,T,T,T,T,T) & onoff
   idxL <- c(T,T,T,T,T,F,T,T,T,T,T) & onoff
   FP   <- c(FP,list(idxM=idxM,idxD=idxD,idxL=idxL))

   #pset <- switch(MDL,idxM,idxD,idxL); pmon <- pnam[pset]
   pset <- c(T,T,T,T,F,T,T,T,T,T,T) & as.vector(phi>0); # Fix for Dirichlet
   pmon <- pnam[pset]
   if (any(substring(pmon,1,3)=="rho")) {
      pmon <- pmon[!is.element(substring(pmon,1,3),"rho")]
      pmon <- c(pmon,"rho") }
   FP  <<- c(FP,list(pmon=pmon))

   fnam <- paste(type,spp,year,sep=".")
   assign(paste("FP",fnam,sep="."),FP,pos=1)
   setWinVal(list(phi=phi,pset=pset)) #,onoff=onoff))
   remove(list=ls(1)[is.element(ls(1),paste("Fout",1:3,sep=""))],pos=1)
}

Ueval <- function() {
   # Evaluate user's model
   # ---------------------
   getWinVal(scope="L"); unpackList(FP,scope="L");
   suff <- paste(type,spp,year,sep=".")
   mods <- (1:3)[modT]

   N <<- 0
   if (any(mods==1)) {
      Pcur <<-1; Fout1 <<- Fmin(theta,idxM); print(Fout1); assign(paste("Fout1",suff,sep="."),Fout1,pos=1) }
   if (any(mods==2)) {
      Pcur <<-2; Fout2 <<- Fmin(theta,idxD); print(Fout2); assign(paste("Fout2",suff,sep="."),Fout2,pos=1) }
   if (any(mods==3)) {
      Pcur <<-3; Fout3 <<- Fmin(theta,idxL); print(Fout3); assign(paste("Fout3",suff,sep="."),Fout3,pos=1) }
}

Fmin <- function(Pmat,idx) {
   # Minimize UFun, given Pmat (will be superceded by PBS Modelling function in future)
   # -------------------------
   Sfun <- function(S,Pmat,idx) {
      # function of surrogate parameters
      P <- S2P(S,Pmat,idx);
      Uval <- Ufun(P);
      # print(c(P,S,Uval)); # <-- for debugging
      return(Uval); };

   S0 <- P2S(Pmat,idx); nS <- length(S0);

   Fout <- nlm(f=Sfun,p=S0,typsize=rep(0.005,nS), Pmat=Pmat, idx=idx)
   p.out <- Fout$estimate

   Pfinal <- S2P(p.out,Pmat,idx);
   Pfmat <- rbind(Pfinal,Pmat[2:3,]);
   Pout <- list(start=Pmat[1,], end=Pfinal, surrogates=P2S(Pfmat,idx), check=p.out,
      grad=Fout$grad, code=Fout$code, iters=Fout$iterations, fmin=Fout$minimum, AIC=2*Fout$minimum+2*sum(idx) );
   cat("\n\n")
   return(Pout); };

P2S <- function(Pmat,idx) {
   # Convert true parameters to surrogates
   # -------------------------------------
   P0  <- Pmat[1,]; Pmin <- Pmat[2,]; Pmax <- Pmat[3,];
   #idx <- (Pmin < Pmax); # indices that change
   S0 <- (P0[idx]-Pmin[idx]) / (Pmax[idx]-Pmin[idx]);
   S0 <- pmax(S0,0); S0 <- pmin(S0,1);  # enforces the range
   S <- (2/pi) * asin(sqrt(S0));
   names(S) <- dimnames(Pmat)[[2]][idx];
   return(S); }

S2P <- function(S,Pmat,idx) {
   # Convert surrogates to true parameters
   # -------------------------------------
   P0 <- Pmat[1,]; Pmin <- Pmat[2,]; Pmax <- Pmat[3,];
   #idx <- (Pmin < Pmax);
   if (sum(idx) != length(S)) stop("Warning: S & P not consistent/n");
   P1 <- Pmin[idx] + (Pmax[idx]-Pmin[idx])*sin(pi*S/2)^2;
   P <- P0; P[idx] <- P1; return(P); };

Ufun <- function(P) {
   # User's function (CCA in this case)
   # ---------------
   Z<-P[1]; alpha<-P[2]; betak<-P[3]; tau<-P[4]; sigma<-P[5]; n<-P[6]
   rho1<-P[7]; rho2<-P[8]; rho3<-P[9]; rho4<-P[10]; rho5<-P[11];
   N <<- N + 1;
   if (N==1 | N==ceiling(N/10)*10)  cat(round(Z,5),"\n", sep="")

   unpackList(FP,scope="L")
   a      <- as.numeric(names(pa.obs))
   pa     <- ccaMod(P=c(Z,alpha,betak,tau,rho1,rho2,rho3,rho4,rho5))

   if (Pcur==1) {
      out    <- -nspec * sum(pa.obs * log(pa))
      assign("pi1",pa,pos=1)
   }
   if (any(Pcur==c(2,3))) {
      z      <- as.numeric(cut(a,acut))
      pi     <- sapply(split(pa,z),sum)

      if (Pcur==2) {
         g      <- length(pi.obs)
         out <- sum( lgamma(n*pi) - n*pi*log(pi.obs) ) - lgamma(n)
         assign("pi2",pi,pos=1)
      }
      if (Pcur==3) {
         g      <- length(pi.obs)
         ytil <- gm(pi.obs)
         ptil <- gm(pi)
         out  <- (g-1) * log(sigma) + (1/(2*sigma^2)) * sum((log(pi.obs/ytil)-log(pi/ptil))^2)
         assign("pi3",pi,pos=1)
      }
   }
   return(out)
}

Uplot <- function() {
   # User's plot of NLM results
   # --------------------------
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph();

   # Data matrix for plotting
   mods <- (1:3)[modT]; nmods <- length(mods)
   suff <- paste(type,spp,year,sep=".")
   nliy <- c(20,15)

   x <- k:amax; xx <- as.character(x); xa <- k:A; xlim <- c(k,A)+c(-1,1)
   pmat <- array(NA,dim=c(length(x),10,nmods),dimnames=list(x,c("x","y","ya","yi","pa","pi","cya","cyi","cpa","cpi"),mods))
   names(dimnames(pmat)) <- c("age","xy","mod")

   for (i in mods) {
      ii   <- as.character(i)
      Fout <- get(paste("Fout",i,sep=""))
      pars <- Fout$end
      y    <- pa.raw
      ya   <- pa.obs
      yi   <- switch(i, pa.obs, pi.obs, pi.obs)
      a    <- as.numeric(names(yi))
      pa   <- ccaMod(P=pars[c("Z","alpha","betak","tau","rho1","rho2","rho3","rho4","rho5")])
      if (i!=1) {
         z  <- as.numeric(cut(xa,acut))
         z  <- z[!is.na(z)]
         pi <- sapply(split(pa,z),sum,na.rm=T); names(pi) <- a }
      else pi <- pa
      cya <- cumsum(ya); cyi  <- cumsum(yi); cpa  <- cumsum(pa); cpi  <- cumsum(pi)

      pmat[xx,"x",ii]           <- x
      pmat[names(y),  "y",  ii] <- y
      pmat[names(ya), "ya", ii] <- ya
      pmat[names(yi), "yi", ii] <- yi
      pmat[names(pa), "pa", ii] <- pa
      pmat[names(pi), "pi", ii] <- pi
      pmat[names(cya),"cya",ii] <- cya
      pmat[names(cyi),"cyi",ii] <- cyi
      pmat[names(cpa),"cpa",ii] <- cpa
      pmat[names(cpi),"cpi",ii] <- cpi
   }
   assign(paste("pmat",suff,sep="."),pmat,pos=1)

   ymax  <- max(apply(pmat,match("xy",names(dimnames(pmat))),max,na.rm=T)[c("ya","yi")])
   ylim  <- c(0,ymax)
   xpos  <- seq(0,B,5)  ; zx  <- xpos>=k & xpos<=A
   ypos1 <- seq(0,1,.01); zy1 <- is.element(ypos1,seq(0,1,.02))
   ypos2 <- seq(0,1,.05); zy2 <- is.element(ypos2,seq(0,1,.1))

   # Plot the results
   if (wmf) win.metafile(filename=paste(suff,paste(".mods",paste(mods,collapse=""),sep=""),".emf",sep=""),
      width=6.5,height=switch(nmods,5,8,9.5),pointsize=12)
   if (names(dev.cur())=="null device") {
      windows(width=6,height=8); frame(); }
   din <- par()$din; xmarg <- 1*c(0.5,0.5);
   ymarg <- switch(nmods,max(1,par()$din[2]-4),2,1); ymarg <- c(.6,.4)*ymarg;
   ymarg <- max(.8,par()$din[2]-4*nmods); ymarg <- c(.7,.3)*ymarg;

   par(mfrow=c(nmods,2),mai=c(0,0,0,0),omi=c(ymarg[1],xmarg[1],ymarg[2],xmarg[2]), mgp=c(0,.25,0))
   sz1 <- (par()$fin[2]/nliy[1])/par()$csi; sz2 <- (par()$fin[2]/nliy[2])/par()$csi

   for (i in mods) {
      ii   <- as.character(i)
      idx  <- switch(i,idxM,idxD,idxL)
      Fout <- get(paste("Fout",i,sep=""))
      pars <- Fout$end[idx]; npars <- length(pars)

      # PLOT 1 - Bars of pi
      za <- !is.na(pmat[xx,"ya",ii])
      zi <- !is.na(pmat[xx,"yi",ii])
      xa <- pmat[xx,"x",ii][za]
      xi <- pmat[xx,"x",ii][zi]
      ya <- pmat[xx,"ya",ii][za]
      yi <- pmat[xx,"yi",ii][zi]
      pa <- pmat[xx,"pa",ii][za]
      pi <- pmat[xx,"pi",ii][zi]
      plot(0,0,type="n",xlim=xlim,ylim=ylim,xaxt="n",yaxt="n",xlab="",ylab="")
      axis(1,at=xpos,tck=.01,labels=F)
      axis(1,at=xpos[zx],tck=.015,labels=(par()$mfg[1]==par()$mfg[3]),cex.axis=sz1)
      axis(2,at=ypos1,tck=.01,labels=F)
      axis(2,at=ypos1[zy1],tck=.02,labels=T,adj=1,cex.axis=sz1)

      abline(h=epsilon,lty=3)
      lines(xa,pa,col=switch(i,"blue","forestgreen","red"),lwd=2)
      if (seepa) lines(xa,ya,type="h",lwd=2)
      if (seepi) lines(xi+.25,yi,type="h",lwd=2,lty=3,col="gray40")
      zbi <- is.element(xa,bi)
      lines(xa[zbi],ya[zbi],type="h",lwd=2,col="#FF8000")
      mtext(paste(c("pa","pi")[c(seepa,seepi)],collapse="/"),side=2,cex=sz2,line=1.5*nmods^.2)
      addLabel(.95,.95,switch(i,"Multinomial","Dirichlet","Logistic-normal"),cex=sz2,adj=1,
         col=switch(i,"blue","forestgreen","red"))

      # PLOT 2 - Cumulative pi
      cyi <- pmat[xx,"cyi",ii][zi]; cpi <- pmat[xx,"cpi",ii][zi]
      cya <- pmat[xx,"cya",ii][za]; cpa <- pmat[xx,"cpa",ii][za]
      plot (xi,cpi,type="n",xlim=xlim,ylim=c(0,1),xaxt="n",yaxt="n",xlab="",ylab="")
      axis(1,at=xpos,tck=.01,labels=F)
      axis(1,at=xpos[zx],tck=.015,labels=(par()$mfg[1]==par()$mfg[3]),cex.axis=sz1)
      axis(4,at=ypos2,tck=.01,labels=F)
      axis(4,at=ypos2[zy2],tck=.02,labels=T,adj=0,cex.axis=sz1)
      if (seepa) {
         lines(xa,cya,lwd=3,col=1)
         lines(xa,cpa,lwd=2,col=switch(i,"blue","forestgreen","red")) }
      if (!seepa & seepi) {
         lines(xi,cyi,lwd=3,col=1)
         lines(xi,cpi,lwd=2,col=switch(i,"blue","forestgreen","red")) }
      mtext(paste("Cumulative",c("pa","pi")[c(seepa,!seepa&seepi)]),side=4,line=1.5*nmods^.4,cex=sz2)
      pval <- as.character(signif(pars,3)); plab <- names(pars)
      fval <- as.character(signif(c(Fout$fmin,Fout$AIC),3)); flab <- c("fmin","AIC")
      y0p  <- (1/nliy[1]) * (npars+3); y0f  <- (1/nliy[1]) * (3)
      addLabel(.65,y0p,paste(plab,collapse="\n"),adj=c(1,1),col="blue",cex=sz1)
      addLabel(.70,y0p,paste(pval,collapse="\n"),adj=c(0,1),col="blue",cex=sz1)
      addLabel(.65,y0f,paste(flab,collapse="\n"),adj=c(1,1),col="red",cex=sz1)
      addLabel(.70,y0f,paste(fval,collapse="\n"),adj=c(0,1),col="red",cex=sz1)
   }
   mtext("Age",outer=T,side=1,line=2,cex=sz2)
   if(wmf) dev.off()
}

#--WinBUGS-Model----------------------------------------------------------------

checkMon <- function() {
   # Check user's monitor choices
   # ----------------------------
   getWinVal(scope="L"); unpackList(FP,scope="L");
   #Pset <- switch(MDL,idxM,idxD,idxL);
   Pset <- c(T,T,T,T,F,T,T,T,T,T,T) & as.vector(phi>0); # Fix for Dirichlet
   z <- Pset-pset
   if(any(z==-1)) {
      print(paste("Cannot monitor (",paste(pnam[is.element(z,-1)],collapse=","),")",sep=""));
      return(FALSE) }
   else {
      pmon <- pnam[pset]
      if (any(substring(pmon,1,3)=="rho")) {
         pmon <- pmon[!is.element(substring(pmon,1,3),"rho")]
         pmon <- c(pmon,"rho") }
      FP$pmon <<- pmon;
      return(TRUE) }
}

modCompile <- function() {
   # Initialize and compile the WinBUGS model
   # ----------------------------------------
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L");
   suff <- paste(type,spp,year,sep=".")
   ai   <- acut[2:length(acut)]; ai[length(ai)] <- A; y <- pi.obs
   dat  <- list(k=k,B=B,b0=b0,m=m,b=bi,g=g,ai=ai,y=y);
   bugsData(dat,"CCAdat.txt")
   modelCheck("CCAmod.txt");  # check model syntax
   modelData("CCAdat.txt");   # load current data
   modelCompile(nc);          # compile with nc chains
   modelGenInits();           # generate randoms inits
   samplesSet(pmon);          # parameters to monitor
   setWinVal(list(clen=100,cthin=1,ctot=0,s1=1,s2=100,sthin=1,chn2=nc)); par(ask=F);
}

modUpdate <- function() {
   # Update the model and save complete history in global "CCAhist"
   # --------------------------------------------------------------
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L");
   modelUpdate(clen,cthin);
   Bhist <- as.data.frame(samplesHistory("*",beg=0,plot=F) );
   nams  <- names(Bhist); nams <- sub("rho\\.","rho",nams); nams <- sub("\\.\\.",".",nams);
   names(Bhist) <- nams
   CCAhist <<- cbind(X=1:nrow(Bhist),Bhist)
   ctot <- dim(CCAhist)[1];    # total length so far
   setWinVal(list(ctot=ctot,s1=ctot-clen+1,s2=ctot)); par(ask=F);
}

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

modTrace <- function(file=CCAhist,clrs=c("blue","red","green","magenta","navy")) {
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph();
   i1   <- max(s1,1); i2 <- min(s2,ctot);  # ensure valid range
   idx  <- seq(i1,i2,by=sthin);  file <- file[idx,];  nams <- names(file);
   pmon <- pnam[pset]; nr <- length(pmon); chains <- chn1:chn2
   par(mfrow=c(nr,1), mar=c(1,3,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=F)
   for (i in pmon) {
      ii  <- paste(i,chains,sep="."); iii <- (1:length(nams))[is.element(nams,ii)]
      dot <- regexpr("\\.",ii); j <- as.numeric(substring(ii,dot+1))
      plotTrace(file[,c(1,iii)],clrs=clrs[j])
      addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2)
   }
}

plotDens <- function(file,clrs=c("blue","red","green","magenta","navy"),...) {
   nc <- ncol(file)
   dd <- density(unlist(file[,2:nc]),adjust=1.25); xlim <- range(dd$x,na.rm=T); ylim <- range(dd$y,na.rm=T)
   for (i in 2:nc) {
      d <- density(file[,i], adjust=1.25);
      xlim[1] <- min(xlim[1],min(d$x)); xlim[2] <- max(xlim[2],max(d$x));
      ylim[1] <- min(ylim[1],min(d$y)); ylim[2] <- max(ylim[2],max(d$y));
   }
   plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1)
   lines(dd$x,dd$y,col="gray",lwd=2)
   for (i in 2:nc) {
      y <- file[,i]; d <- density(y, adjust=1.25)
      lines(d$x,d$y,col=clrs[i-1])
   }
}

modDens <- function(file=CCAhist,clrs=c("blue","red","green","magenta","navy")) {
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph();
   i1   <- max(s1,1); i2 <- min(s2,ctot);  # ensure valid range
   idx  <- seq(i1,i2,by=sthin);  file <- file[idx,];  nams <- names(file);
   pmon <- pnam[pset]; nr <- length(pmon); chains <- chn1:chn2
   par(mfrow=c(nr,1), mar=c(1,3.5,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=F)
   for (i in pmon) {
      ii  <- paste(i,chains,sep="."); iii <- (1:length(nams))[is.element(nams,ii)]
      dot <- regexpr("\\.",ii); j <- as.numeric(substring(ii,dot+1))
      plotDens(file[,c(1,iii)],clrs=clrs[j])
      addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2)
   }
}

plotACF <- function(file,lags=20,clrs=c("blue","red","green","magenta","navy"),...) {
   nc <- ncol(file); nch <- nc-1;
   acfout <- acf(file[2:nc],lag.max=lags,plot=F); acfacf <<- acfout$acf
   ymin <- min(diag(apply(acfacf,2:3,min)),-.2); ymax <- max(diag(apply(acfacf,2:3,max)));
   xlim <- c(0,lags); ylim <- c(ymin,ymax);
   plot(0,0,xlim=xlim,ylim=ylim,type="n",tck=.03,xlab="",ylab="",las=1)
   abline(h=c(-.2,.2),col="#400080",lty=2);
   for (i in 2:nc) {
      x <- (0:lags)+(i-2)*(.5/nch); y <- acfacf[,i-1,i-1];
      lines(x,y,type="h",lwd=3,col=clrs[i-1])
   }
   abline(h=0,col="gray40",lty=3); box();
}

modACF <- function(file=CCAhist,clrs=c("blue","red","green","magenta","navy")) {
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph();
   i1   <- max(s1,1); i2 <- min(s2,ctot);  # ensure valid range
   idx  <- seq(i1,i2,by=sthin);  file <- file[idx,];  nams <- names(file);
   pmon <- pnam[pset]; nr <- length(pmon); chains <- chn1:chn2
   par(mfrow=c(nr,1), mar=c(1,3,0.5,0.5), oma=c(1,0,0,0), mgp=c(0,.25,0), ask=F)
   for (i in pmon) {
      ii  <- paste(i,chains,sep="."); iii <- (1:length(nams))[is.element(nams,ii)]
      dot <- regexpr("\\.",ii); j <- as.numeric(substring(ii,dot+1))
      plotACF(file[,c(1,iii)],clrs=clrs[j])
      addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2)
   }
}

panel.hist <- function(x, ...) {
   usr <- par("usr"); on.exit(par(usr))
   h <- hist(x, breaks="Sturges", plot=FALSE)
   breaks <- h$breaks; nB <- length(breaks)
   y <- h$counts; y <- y/sum(y)
   par(usr = c(usr[1:2], 0, max(y)*1.5) )
   rect(breaks[-nB], 0, breaks[-1], y, col="#FFD18F", ...)
   box()
}

modPairs <- function() {
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph();
   i1   <- max(s1,1); i2 <- min(s2,ctot); # ensure valid range
   idx  <- seq(i1,i2,by=sthin); file <- CCAhist[idx,];
   nams <- names(file); dot <- regexpr("\\.",nams);
   chains <- chn1:chn2; nchn <- length(chains);
   pmon <- pnam[pset];
   z1   <- is.element(substring(nams,dot+1),chains);
   z2   <- is.element(substring(nams,1,dot-1),pmon);
   file <- file[,z1&z2];
   PMON <- paste(rep(pmon,each=nchn),rep(chains,length(pmon)),sep="."); file <- file[,PMON]
   if (exists(paste("Fout",MDL,sep=""),where=1)) {
      modes <- get(paste("Fout",MDL,sep=""),pos=1)$end[pset];
      print("MODES:"); print(paste(names(modes),signif(modes,3),sep="="));
      modes <- rep(modes,each=nchn); }
   else  modes <- NULL;

   par(ask=F,mgp=c(0,.75,0));
   pairs(file, diag.panel=panel.hist, gap=0, cex.labels=1.2,
      panel=function(x,y,z=modes) {
         n   <- length(x); nn <- n-1;
         xmn <- mean(x,na.rm=T); ymn <- mean(y,na.rm=T);
         points(x,y,pch=16,cex=0.6,col="darkgray");
         abline(h=ymn,v=xmn,col="blue",lty=3);
         points(xmn,ymn,col="cyan",pch=15,cex=1.2);
         points(xmn,ymn,col="blue",pch=0,cex=1.2);
         if (!is.null(modes)) {
            xmd <- z[par()$mfg[2]]; ymd <- z[par()$mfg[1]];
            points(xmd,ymd,col="red",pch=17,cex=1.2);
            points(xmd,ymd,col="black",pch=2,cex=1.2);
         }
      })
}

modSub <- function(chains=samplesGetFirstChain():samplesGetLastChain()) {
   if (!exists("CCAhist")) return(FALSE)
   getWinVal(scope="L"); unpackList(FP,scope="L");
   i1  <- max(s1,1); i2 <- min(s2,ctot); # ensure valid range
   idx <- seq(i1,i2,by=sthin);

   Pfld <- pnam[pset]; nP <- length(Pfld)
   nch  <- length(chains)
   pfld <- paste(rep(Pfld,each=nch),chains,sep=".")
   file <- CCAhist[idx,pfld]

   temp <- NULL
   for (i in chains) {
      junk <- file[,paste(Pfld,i,sep=".")]
      names(junk) <- Pfld
      temp <- rbind(temp,junk)
   }
   CCAsub <<- temp
   return(TRUE)
}

modHist <- function() {
   isOK <- checkMon(); if (!isOK) stop("Reset monitors")
   getWinVal(scope="L"); unpackList(FP,scope="L"); resetGraph(); par(ask=F);
   chains <- chn1:chn2; pmon <- pnam[pset]; nP <- length(pmon); nbin <- 30;

   isOK <- modSub(chains); if (!isOK) stop("Generate Updates")
   file <- CCAsub

   iclr <- c("aquamarine",rep("sandybrown",2),"plum","royalblue","darkseagreen",rep("gold",5)); names(iclr) <- pnam;
   par(mfrow=c(nP,1),mar=c(1,4,.5,.5),oma=c(1,0,0,0))

   for (i in pmon) {
      x    <- file[,i]
      q95  <- quantile(x,c(.025,.975))
      x95  <- c(rep(q95[1],2),NA,rep(q95[2],2))
      xmn  <- mean(x)

      xf   <- hist(x,nclass=nbin,plot=F)
      xx   <- xf$mids
      yy   <- xf$counts/sum(xf$counts)
      xoff <- diff(xf$breaks)[1]/2
      nn   <- length(xx)
      xxx  <- as.vector(rbind(xx-xoff,xx+xoff,xx+xoff,xx-xoff,rep(NA,nn)))
      yyy  <- as.vector(rbind(rep(0,nn),rep(0,nn),yy,yy,rep(NA,nn)))
      xlim <- range(xxx,na.rm=T); xdiff <- diff(xlim); ylim <- c(0,max(yy)); ydiff <- diff(ylim)
      ylim <- ylim + c(0,.08*ydiff)

      plot(0,0,xlim=xlim,ylim=ylim,type="n",mgp=c(3.5,.5,0),xaxt="n",yaxt="n",axes=F,xlab="",ylab="",cex.lab=1.5)
      axis(1,at=xx-xoff,pos=0,mgp=c(0,.5,0),tck=-.01)
      axis(2,at=pretty(ylim),cex.axis=1.2,adj=1,mgp=c(0,.6,0),las=1)
      polygon(xxx,yyy,col=as.vector(iclr[i]))

      y75 <- ylim[2]*.75; y75t <- y75*1.05
      y95<- ylim[2]*0.95;  y95t <- y95*1.05

      ycl <- c(0,y75,NA,0,y75)
      lines(x95,ycl,col="red",lty=5,lwd=2)
      text(q95+.01*xdiff,rep(y75,2),signif(q95,3),adj=c(0,0))
      lines(rep(xmn,2),c(0,y95),col="red",lwd=3)
      text(xmn+.01*xdiff,y95,signif(xmn,3),adj=c(0,0))
      addLabel(.97,.95,i,cex=1.2,col="#400080",adj=c(1,1),font=2)
   }
}

#|||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||

remove(list=ls(1)[is.element(ls(1),c("afile","FP","N","Fout","Fout1","Fout2","Fout3"))],pos=1);
if (!require(BRugs, quietly=TRUE)) stop("The BRugs package is required for this example")
if (!require(PBSmodelling, quietly=TRUE)) stop("The PBSmodelling package is required for this example")
createWin("CCAWin.txt"); Uget();

