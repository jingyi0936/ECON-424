# econ424lab3.R			script file for econ 424 Homework 3 calculations
# summer 2018
# 
# first install the mvtnorm package. 

options(digits=4)
library(mvtnorm)

#
# Example: bivariate normal distribution
#

mu.x = 0.01
sig.x = 0.25
mu.y = 0.05
sig.y = 0.15

# simulate from bivariate normal with a given rho 
rho.xy = -0.5
sig.xy = rho.xy*sig.x*sig.y
# set up the Var-CoVar Matrix
Sigma.xy = matrix(c(sig.x^2, sig.xy, sig.xy, sig.y^2), 2, 2, byrow=TRUE)

# use the rmvnorm() function to simulate bivariate normal data
?rmvnorm  # read more about this command
n = 100
set.seed(123)
xy.vals = rmvnorm(n, mean=c(mu.x, mu.y), sigma=Sigma.xy) 
head(xy.vals) # showing sample simulations

# scatterplot
plot(xy.vals[,1], xy.vals[,2], pch=16, cex=1, col="blue", 
     xlab="x", ylab="y")
title("Bivariate normal: rho=?")  # UPDATE IT TO THE VALUE YOU USE
abline(h=mu.y, v=mu.x)
segments(x0=0, y0=min(xy.vals[,2]), x1=0, y1=0, col="red")
segments(x0=min(xy.vals[,1]), y0=0, x1=0, y1=0, col="red")

# compute area under bivariate standard normal distribution
# Finc P( -00 < X < 0 and -00 < Y < 0)
?pmvnorm
pmvnorm(lower=c(-Inf, -Inf), upper=c(0, 0), mean=c(mu.x, mu.y), sigma=Sigma.xy)



# Example for simulating MA(1) process with theta = 0.5
ma1.model.5 = list(ma=0.5)
mu = 0.05
set.seed(123)
ma1.sim.5 = mu + arima.sim(model=ma1.model.5, n=250,
innov=rnorm(n=250, mean=0, sd=0.1))
acf.ma1.model.5 = ARMAacf(ma=0.9, lag.max=10)

par(mfrow=c(3,1)) # the figures are hard to see, change the 3 to 1
ts.plot(ma1.sim.5, main="MA(1) Process: mu=0.05, theta=0.9",
xlab="time",ylab="y(t)")
abline(h=0)
plot(1:10, acf.ma1.model.5[2:11], type="h", col="blue", main="theoretical ACF")
tmp=acf(ma1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the the MA model with theta = other values, e.g 0.8 and -0.8

# Example for simulating AR(1) process with phi = 0.99
ar1.model.5 = list(ar=0.99)
mu = 0.05
set.seed(123)
ar1.sim.5 = mu + arima.sim(model=ar1.model.5, n = 250,
innov=rnorm(n=250, mean=0, sd=0.1))
acf.ar1.model.5 = ARMAacf(ar=0.99, lag.max=10)

par(mfrow=c(1,1))  # the figures are hard to see, change the 3 to 1
ts.plot(ar1.sim.5,main="AR(1) Process: mu=0.05, phi=0.99",
xlab="time",ylab="y(t)")
abline(h=0)
plot(1:10, acf.ar1.model.5[2:11], type="h", col="blue", main="Theoretical ACF")
tmp=acf(ar1.sim.5, lag.max=10, main="Sample ACF")
par(mfrow=c(1,1))

# do the same for the the AR model with phi = other values, e.g 0.8 and -0.8

# Matrix algebra in R
matA = matrix(c(1,4,7,2,4,8,6,1,3), 3, 3, byrow=T)
matA
vecx = matrix(c(1,2,3), 3, 1)
vecx


# Transposing Matrices
t(matA)

# Basic Operations
2*matA      # 2A
matA%*%vecx     # Ax
t(vecx)%*%matA%*%vecx  #  x'Ax

# Computing Portfolio Moments

vecmu = matrix(c(0.01,0.04,0.02), 3, 1)     # vector of expected returns for 3 assets
matSigma = matrix(c(0.1,0.3,0.1,0.3,0.15,-0.2,0.10,-0.20, 0.08), 3, 3, byrow=T)  # Var-CoV Matrix
vecx = matrix(c(0.2,0.5,0.3), 3, 1)     # portfolio weights: 20%, 50%, 30%
vecmu
matSigma
vecx

crossprod(vecmu, vecx)      # Expected Portfolio Return: method 1
t(vecmu)%*%vecx             # Expected Portfolio Return: method 2
crossprod(vecx, matSigma%*%vecx)  # Portfolio variance
t(vecx)%*%matSigma%*%vecx          # Portfolio variance


