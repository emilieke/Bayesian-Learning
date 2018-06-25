# Conjugate updating with Bernoulli trial data.

# Sufficient statistics under different sampling models.
Bernoullisuff <- function(x){
  return(list("heads"=sum(x),"tails"=length(x)-sum(x)))
}
Binomialsuff <- function(n,x){
  return("heads"=sum(x),"tails"=sum(n)-sum(x))
}
Geometricsuff <- function(x){
  return("heads"=length(x),"tails"=sum(x))
}
Negbinsuff <- function(r,x){
  return("heads"=sum(r),"tails"=sum(x))
}

# Prior updating.
betaposterior <- function(heads,tails,a=0,b=0){
  astar <- a+heads
  if (astar==0){stop("Improper posterior")}
  bstar <- b+tails
  if (bstar==0){stop("Improper posterior")}
  return(list("astar"=a+heads,"bstar"=b+tails))
}

# Mixture prior updating
betamixposterior <- function(heads,tails,w,a=0,b=0){
  astar <- a+heads
  bstar <- b+tails
  wstar <- exp(log(w)+lbeta(astar,bstar)-lbeta(a,b))
  return(list("wstar"=wstar,"astar"=astar,"bstar"=bstar))
}

# Predictive distributions
Bernoullipred <- function(x,a,b){
  px <- ifelse(x==1,a/(a+b),b/(a+b))
  return(px)
}

Binomialpred <- function(n,x,a,b){
  return(px=exp(lchoose(n,x)+lbeta(a+x,b+n-x)-lbeta(a,b)))
}

Geometricpred <- function(x,a,b){
  return(px=exp(lbeta(a+1,b+x)-lbeta(a,b)))
}

Negbinpred <- function(r,x,a,b){
  return(px=exp(lgamma(x+r)-lgamma(r)+lbeta(a+r,b+x)-lbeta(a,b)))
}

# Mixture predictive distributions
Bernoullimixpred <- function(x,w,a,b){
  px <- ifelse(x==1,sum(w*a/(a+b)),sum(w*b/(a+b)))
  return(px)
}

Binomialmixpred <- function(n,x,w,a,b){
  px <- sum(w*exp(lchoose(n,x)+lbeta(a+x,b+n-x)-lbeta(a,b)))
  return(px)
}

Geometricmixpred <- function(x,w,a,b){
  px <- sum(w*exp(lbeta(a+1,b+x)-lbeta(a,b)))
  return(px)
}

Negbinmixpred <- function(r,x,w,a,b){
  px <- sum(w*exp(lgamma(x+r)-lgamma(r)+lbeta(a+r,b+x)-lbeta(a,b)))
  return(px)
}

# Example code

# Data generation
x <- rbinom(100,1,0.7)
cc <- Bernoullisuff(x)
heads <- cc$heads
tails <- cc$tails

# Prior settings
a <- 1
b <- 1

# Posterior calculations
cc <- betaposterior(heads,tails,a,b)
astar <- cc$astar
bstar <- cc$bstar

# Plot of posterior (blue), scaled likelihood (red) and prior (green if proper)
thetagrid <- seq(0,1,length.out=1001)
fpost <- dbeta(thetagrid,astar,bstar)
flik <- dbeta(thetagrid,heads+1,tails+1)
fmax <- max(flik,fpost)
plot(thetagrid,fpost,type="l",lwd=3,col="blue",xlim=c(0,1),ylim=c(0,fmax),xlab=expression(theta),ylab="f")
lines(thetagrid,flik,type="l",lwd=3,col="red")
if(a>0&&b>0){
  fpri <- dbeta(thetagrid,a,b)
  lines(thetagrid,fpri,type="l",lwd=3,col="green")
}

# Predictive distribution
npred <- 10
px <- Binomialpred(npred,c(0:npred),astar,bstar)
pxclass <- dbinom(c(0:npred),npred,heads/(heads+tails))
fmax <- max(px,pxclass)
plot(c(0:npred),px,type="h",lwd=3,col="blue",ylim=c(0,fmax),xlab="x",ylab="P")
lines(c(0:npred)+0.1,pxclass,type="h",lwd=3,col="red")
if(a>0&&b>0){
  pxpri <- Binomialpred(npred,c(0:npred),a,b)
  lines(c(0:npred)-0.1,pxpri,type="h",lwd=3,col="green")
} 
