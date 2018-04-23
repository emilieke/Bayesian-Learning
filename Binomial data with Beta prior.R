# ----------------------------------------------------------------------------------------------------
# HOMEWORK 2
# Emilie Engen, 100356077
# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning/Homework 2")

# ----------------------------------------------------------------------------------------------------
# Choose a (possibly big) data set from a certain random variable.
# Binomial data

# Data generation
x <- rbinom(100,1,0.7)
n <- length(x)

Binomialsuff <- function(n,x){
  return(list("heads"=sum(x),"tails"=sum(n)-sum(x)))
}

cc <- Binomialsuff(n,x)
heads <- cc$heads
tails <- cc$tails

# ----------------------------------------------------------------------------------------------------
# Assume a parametric model for this variable with a conjugate prior, e.g.:
# Binomial data with a Beta prior for the success probability.

# Prior settings
a <- 1
b <- 1

# Prior updating.
betaposterior <- function(heads,tails,a=0,b=0){
  astar <- a+heads
  if (astar==0){stop("Improper posterior")}
  bstar <- b+tails
  if (bstar==0){stop("Improper posterior")}
  return(list("astar"=a+heads,"bstar"=b+tails))
}

# Poisson data with a Gamma prior for the mean.
# Exponential data with a Gamma prior for the rate parameter.
# Normal data with a normal-gamma prior for the mean and precision.

# ----------------------------------------------------------------------------------------------------
# Obtain the posterior distribution for the model parameters and the predictive density. 

# Posterior calculations
cc <- betaposterior(heads,tails,a,b)
astar <- cc$astar
bstar <- cc$bstar

# Plot of posterior (blue), scaled likelihood (red) and prior (green if proper)
thetagrid <- seq(0,1,length.out=1001)

# Posterior
fpost <- dbeta(thetagrid,astar,bstar)

# Scaled likelihood
flik <- dbeta(thetagrid,heads+1,tails+1)

fmax <- max(flik,fpost)
plot(thetagrid,fpost,type="l",lwd=3,col="blue",xlim=c(0,1),ylim=c(0,fmax),xlab=expression(theta),ylab="f")
lines(thetagrid,flik,type="l",lwd=3,col="red")

# Prior
if(a>0 && b>0){
  fpri <- dbeta(thetagrid,a,b)
  lines(thetagrid,fpri,type="l",lwd=3,col="green")
}


# Predictive distribution
npred <- 10

# Binomial predictive distribution
Binomialpred <- function(n,x,a,b){
  return(px=exp(lchoose(n,x)+lbeta(a+x,b+n-x)-lbeta(a,b)))
}

px <- Binomialpred(npred,c(0:npred),astar,bstar)
pxclass <- dbinom(c(0:npred),npred,heads/(heads+tails))
fmax <- max(px,pxclass)

# Plot the distribution
plot(c(0:npred),px,type="h",lwd=3,col="blue",ylim=c(0,fmax),xlab="x",ylab="P")
lines(c(0:npred)+0.1,pxclass,type="h",lwd=3,col="red")
if(a>0&&b>0){
  pxpri <- Binomialpred(npred,c(0:npred),a,b)
  lines(c(0:npred)-0.1,pxpri,type="h",lwd=3,col="green")
} 


# ----------------------------------------------------------------------------------------------------
# Calculate various predictive posterior probabilities of quantities of interest.


