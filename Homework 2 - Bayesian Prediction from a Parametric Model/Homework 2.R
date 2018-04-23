# ----------------------------------------------------------------------------------------------------
# HOMEWORK 2
# Emilie Engen, 100356077
# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning/Homework 2")

# Install packages
install.packages("bootstrap")

# Libraries
library(bootstrap)
library(dplyr)
# ----------------------------------------------------------------------------------------------------
# Choose a (possibly big) data set from a certain random variable.

# Clear the workspace
rm(list=ls())

# Load file
atlantic_data <- read.csv('atlantic.csv')

# Creat dataframe
df <- data.frame(atlantic_data)

# Get year from date in df
df$Date <- substr(df$Date,1,4)

# Select distinct events
df <- subset(df, Status == " HU")
df <- distinct(df, ID, .keep_all = TRUE)

# Count annual hurricane occurences
x <- table(df$Date);x

length(x) # total number of years
sum(x) # total number of hurricanes

# ----------------------------------------------------------------------------------------------------
# Assume a parametric model for this variable with a conjugate prior
# Poisson data with a Gamma prior for the mean
require(bootstrap)

x_early = x[1:50]
bs = bootstrap(x_early, theta = mean, nboot = 1000)
qbs = quantile(bs$thetastar, prob = c(0.05, 0.95));qbs

obj = function(x) {
  sum(abs(pgamma(q = qbs, shape = x[1], rate = x[2]) - c(0.05, 0.95)))
}

theta = optim(par = c(2, 1), obj)$par

# Priors
a = theta[1];a
b = theta[2];b

# Sufficient statistics, Poisson
poissuff <- function(x){
  return(list("time"=length(x),"events"=sum(x)))
}

x_late = x[51:length(x)]

# Sufficient statistics
c <- poissuff(x_late);c
t <- c$time;t
n <- c$events;n

# Prior updating
gammaposterior <- function(time,events,a=0,b=0){
  return(list("astar"=a+events,"bstar"=b+time))
}

# Updating
c <- gammaposterior(t,n,a,b);c
astar <- c$astar;astar
bstar <- c$bstar;bstar

# ----------------------------------------------------------------------------------------------------
# Obtain the posterior distribution for the model parameters and the predictive density. 

# Plot the posterior distribution
par(las = 1, mgp = c(2, 0.4, 0), tcl = -0.3)
curve(dgamma(x, shape = astar, rate = bstar), from = 3, to = 7, xlab = expression(lambda), 
      ylab = "Density", col = 4, lwd = 2) # posterior
curve(dgamma(x, shape = n, rate = t), add = TRUE, col = 2, lwd = 2) # likelihood evt. n+1 (en av stedene)
curve(dgamma(x, shape = a, rate = b), add = TRUE, col = 3, lwd = 2) # prior
grid()
legend("topright", c("Prior", "Likelihood", "Posterior"), col = c(3, 2, 4), 
       lwd = c(2, 2, 2), bg = "white", cex=0.8)

# Predictive distribution
poispred <- function(t=1,x,a,b){
  px <- dnbinom(x,a,b/(b+t))
  return(px)
}

# Prediction
px <- poispred(1,c(0:max(x)),astar,bstar);px

# Plot the predictive density for the next year
ymax <- max(px,table(x)/length(x))
plot(table(x)/length(x),lwd=3,col=2,ylim=c(0,ymax),xlab='Number of hurricanes',ylab='Probability')
lines(c(0:max(x))+0.15,px,type='h',lwd=3,col=4)
if (a>0&&b>0){
  lines(c(0:max(x))-0.15,poispred(1,c(0:max(x)),a,b),type='h',col=3,lwd=3)
}
legend("topright", c("Likelihood", "Posterior", "Prior"), col = c(2, 4, 3), 
       lwd = c(2, 2, 2), bg = "white", cex=0.8)

# Plot the predictive density and probability for the next 10 years
Th = 10
m = Th * (n + a)/(t + b)
v = Th * m * (Th + t + b)/(t + b)
nl = 0:100
hh = dnbinom(nl, mu = m, size = v)
plot(nl, hh, type = "h", xlab = "Number of hurricanes", ylab = "Probability", 
     col = "lightgray", lwd = 4)
par(new = TRUE)
p = pnbinom(nl, mu = m, size = v)
plot(nl, p, type = "l", xaxt = "n", yaxt = "n", xlab = "", ylab = "", lwd = 2)
axis(4)
mtext("Probability $h < H$", side = 4, line = 1, las = 0)

# ----------------------------------------------------------------------------------------------------
# Calculate various predictive posterior probabilities of quantities of interest.

plot(c(0, 220), c(0, 1), xlab = "Number of hurricanes, H", ylab="Probability of h > H",lwd = 1,col="white")
grid()
Th = c(30, 20, 10)
cls = rev(c("black", "dark gray", "lightgray"))
ey = c(220, 160, 100)
for (i in 1:3) {
  m = Th[i] * (n + a)/(t + b)
  v = Th[i] * m * (Th + t + b)/(t + b)
  nl = 0:ey[i]
  p = 1 - pnbinom(nl, mu = m, size = v)
  points(nl, p, pch = 16, col = cls[i])
}
legend("topright", c("30 years", "20 years", "10 years"), col = cls, 
       lwd = c(2, 2, 2), bg = "white", cex=0.8)




