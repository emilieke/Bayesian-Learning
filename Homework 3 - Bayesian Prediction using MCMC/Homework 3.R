# ----------------------------------------------------------------------------------------------------
# HOMEWORK 3
# Emilie Engen, 100356077
# ----------------------------------------------------------------------------------------------------
# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning/Homework 3")

# ----------------------------------------------------------------------------------------------------
# Clear the workspace
rm(list=ls())


# Load file
data <- read.csv2("January09.csv")

# Attach data to R search path
attach(data)

# Names of data
names(data)

# Look at the main characteristics of the data
hist(Discharge, col='light blue', breaks=30)
mean(Discharge)
sd(Discharge)
min(Discharge)
max(Discharge)

summary(Discharge)

# Assume that the discharge, Y , follows a Weibull distribution, Y |??, ?? ??? W (??, ??) 
# whose density function is given by: f(y |??,??)=????y?????1exp(?????y??), y >0. 
# Assume e.g. the following prior distribution: ?? ??? Uniform (??min , ??max ) ?? ??? Gamma (a, b)

# Set the prior parameters by assuming non informative priors
k_min=0
k_max=100
a=0.01
b=0.01

# Define the data set
y=Discharge
n=length(y)

# Set the number of burnin iterations and iterations in equilibrium:
burnin=1000
iters=10000
T=burnin+iters

# Initialize a vector for the values of ?? and ?? in the Markov chain:
k=rep(0,T)
theta=rep(0,T)

# Set initial values for the model parameters
k[1]=2
theta[1]=15

# Initialize proportion of accepted values in the MH algorithm:
pac=0

# We are ready to simulate the Markov chain
?rweibull
?rgamma


# Construct an MCMC algorithm to sample from the posterior of (??, ?? | y) 
# and obtain estimations for the posterior means and 95% credible intervals for ?? and ??.

# MCMC chain
for (t in 2:T)
{
  theta[t]=rgamma(1,shape=a+n,rate=b+sum(y^k[t-1]))
  k_c=rnorm(1,k[t-1],sd=1)
  if(k_c<k_min || k_c>k_max){k[t]=k[t-1]}
  else{
    logal=n*log(k_c) + (k_c-1)*sum(log(y))-theta[t]*sum(y^k_c)
    logal=logal-(n*log(k[t-1]) + (k[t-1]-1)*sum(log(y))-theta[t]*sum(y^k[t-1]))
    u=runif(1)
    if (log(u)<logal)
    {
      k[t]=k_c
      if (t>burnin){pac=pac+1}
    }
    else k[t]=k[t-1]
  }
}

# We can calculate the proportion of accepted values:
pac=pac/iters;pac

par(mfrow=c(1,2))
# We can plot the traces of the Markov chain:
ts.plot(k,ylab=expression(kappa),col='deepskyblue4',main='Traces of Markov chain for the shape parameter')
ts.plot(theta,ylab=expression(theta),col='light blue',main='Traces of Markov chain for the scale parameter')

# And obtain posterior samples in equilibrium:
k_post=k[burnin+1:iters];k_post
theta_post=theta[burnin+1:iters];theta_post

par(mfrow=c(1,2))
# We can approximate the posterior distribution of the model parameters:
hist(k_post,col='deepskyblue4', main='Distribution of the posterior shape parameter',xlab='Posterior shape parameter')
hist(theta_post,col='light blue', main='Distribution of the posterior scale parameter',xlab='Posterior scale parameter')

# And obtain 95% credible intervals for k and theta
quantile(k_post,c(0.025,0.975))
quantile(theta_post,c(0.025,0.975))

# We can also approximate the posterior mean and standard deviations of k and theta
mean(k_post)
sd(k_post)
mean(theta_post)
sd(theta_post)

# Obtain an approximate sample from the predictive distribution of the discharge 
# and estimate the probability of a discharge larger than 1m3/sec ?? km2.

# However, we may use the posterior MCMC sample to approximate predictive
# probabilities.  A sample of size (M = 10 000) from the predictive distribution can
# be obtained with: 
M=10000
disc_pred=rweibull(M,shape=k_post,scale=1/theta_post)
disc_pred=rgamma(M,shape=k_post,rate=theta_post)

# Plot the histogram of predicted data
hist(disc_pred,freq=F, main='Distribution of predicted data',xlab='Predicted y',col='light blue')

# We can compare the observed data with the estimated predictive density using:
hist(y,freq=F,main='Observed data and predictive density',ylim=c(0,6),col='deepskyblue4',breaks=22)
lines(density(disc_pred))

# And we can approximate the predictive probability of a discharge larger than 1m3/sec??km2
# using simply the mean of sampled values larger than 1:
mean(disc_pred>1)

