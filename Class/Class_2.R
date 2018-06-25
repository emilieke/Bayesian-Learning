# Working directory
setwd("/Users/Emilie/Dropbox/Skole/UC3M/Bayesian Learning")

# Clear the workspace
rm(list=ls())

# Load file
x <- read.csv('1987.csv')

# Investigate the data set
attach(x)
names(x)
length(ArrDelay)

# Remove NA values for canceled flights
ArrDelay87=na.omit(ArrDelay)
length(ArrDelay87)

# Plot histogram (distribution)
hist(ArrDelay87,1000)
hist(ArrDelay87,1000,xlim=c(-100,500))

# You can see the distribution is right skewed
# We will for this assignment assume gaussian distribution. This is not a good assumption
# You may probably want to consider a mixture of distributions for this data set or consider using a nonparametric 
# model

# Plot the empirical cummulative distribution function
F = ecdf(ArrDelay87)
plot(F)

# This does not look good

# The empirical probability of delay
1-F(0)
# We can see that 62 % of the flights have been delayed

# The probability of the flight being more than 15 minutes late
(1-F(15))/(1-F(0))

# Notice that we assume that the flights are independent, and using a parametric model
# This is real life is not the case (delay depend and other flight delays, on the airline etc.)

# The values for a and b parameters are small, so the weight of the prior is small
# Setting a small value for c you say that the variance is large (large tails)
m=0; c=0.01; a=0.01; b=0.01;

n=length(ArrDelay87)
mean.delay=mean(ArrDelay87)
var.delay=var(ArrDelay87)
m.ast=(c*m+n*mean.delay)/(c+n)
c.ast=c+n
a.ast=a+n
b.ast=b+(n-1)*var.delay+c*n*(m-mean.delay)^2/(c+n)

# m* is close to the mean, because the data set is very large

# The predictive probability that a future new flight is positively delayed
P0=1-pt(-m.ast/sqrt((c.ast+1)*b.ast/(c.ast*a.ast)),a.ast)
# This is close to mean(x.pred>0) because we know that the distribution is student-t

# And conditionally on being delayed, the predictive probability of arriving later than
# 15 minutes
P15_0=(1-pt(15-m.ast/sqrt((c.ast+1)*b.ast/(c.ast*a.ast)),a.ast))/P0
# This result is badely estimated because we are assuming a gaussian distribution (this is an estimation of the tail, 
# therefore this result is badly estimated compared to the probability of delay)


