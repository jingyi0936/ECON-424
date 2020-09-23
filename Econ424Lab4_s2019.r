# econ424lab4.R	 script file for econ 424 lab4 calculations
#
# author: Eric Zivot
#created: September 17, 2008
# revision history: 
# August, 2019: Revised for summer 2019 (by YC)
# July 31, 2018
#   Revised for summer 2018
# July 7, 2016
#   Revised for summer 2016
# January 19, 2016
#   Revised for Winter 2016
# July 7, 2015
#   Revised for Summer 2015
# January 20, 2015
#   Revised for Winter 2015
# July 8, 2014
#   Revised for summer 2014
# July 6, 2012
#   Revised for summer 2012

# first install the packages from CRAN if you have not done so already.
# In Rstudio, go to the Tools tab and click the "Install Packages...", 
# type the package names then press the install button
# Or use the install.packages() command

# comments:
# Data for the lab are
# monthly continuously compounded returns on Vanguard long term bond index fund
#(VBLTX), Fidelity Magellan stock mutual fund (FMAGX), and Starbucks stock (SBUX)
#
# This lab requires the following packages
# PerformanceAnalytics  return and risk analytics
# zoo			              Zeilie's ordered observations
# tseries               various time series functions
# make sure you install these packages before you load them.

options(digits=4)
library(IntroCompFinR)
library(zoo)
library(xts)
library(tseries)
library(PerformanceAnalytics)
library(corrplot)

#
# Part I. Descriptive Statistics
#

# get monthly adjusted closing price data on VBLTX, FMAGX and NFLX from Yahoo
# using the tseries function get.hist.quote(). Set sample to Jan 2003 through
# Dec 2017. Note: if you are not careful with the start and end dates
# or if you set the retclass to "ts" then results might look weird

# look at help on get.hist.quote
?get.hist.quote

# get the adjusted closing prices from Yahoo!
VBLTX.prices = get.hist.quote(instrument="vbltx", start="2003-01-01",
                              end="2017-12-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
# change class of time index to yearmon which is appropriate for monthly data
# index() and as.yearmon() are functions in the zoo package 
#                             
index(VBLTX.prices) = as.yearmon(index(VBLTX.prices))

class(VBLTX.prices)
colnames(VBLTX.prices)
start(VBLTX.prices)
end(VBLTX.prices)

FMAGX.prices = get.hist.quote(instrument="fmagx", start="2003-01-01",
                              end="2017-12-31", quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
index(FMAGX.prices) = as.yearmon(index(FMAGX.prices))

NFLX.prices = get.hist.quote(instrument="nflx", start="2003-01-01",
                             end="2017-12-31", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(NFLX.prices) = as.yearmon(index(NFLX.prices))

# create merged price data
lab4Prices.z = merge(VBLTX.prices, FMAGX.prices, NFLX.prices)
# rename columns
colnames(lab4Prices.z) = c("VBLTX", "FMAGX", "NFLX")

# calculate cc returns as difference in log prices
lab4Returns.z = diff(log(lab4Prices.z))

#
# See the document "Working with Time Series in R" on the
# class webpage for more details on zoo objects
#    

# look at the return data
start(lab4Returns.z)
end(lab4Returns.z)
colnames(lab4Returns.z) 
head(lab4Returns.z)

# I. Univariate Graphical Analysis
#
# Create time plots of data
#

# 3 panel plot (each y axis has different scale)
# note: here, the generic plot() function invokes the plot method for objects
# of class zoo. See the help on plot.zoo
# 
# panel function for plot.zoo to add horizontal line at zero in each panel
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab4Returns.z,col="blue", lwd=2, main="Monthly cc returns on 3 assets",
     panel=my.panel)

# all on the same graph 
plot(lab4Returns.z, plot.type="single", col=c("black","red","green"), lwd=2,
     main="Monthly cc returns on 3 assets", 
     ylab="Return")
legend(x="bottomleft", cex=0.5, legend=colnames(lab4Returns.z), col=c("black","red","green"), lwd=2)
abline(h=0)

# plot returns using the PerformanceAnalytics function chart.TimeSeries()
# this create a slightly nicer looking plot than plot.zoo()
?chart.TimeSeries
chart.TimeSeries(lab4Returns.z, legend.loc="bottomleft", main="Monthly cc returns on 3 assets") 

# the previous charts are a bit hard to read.
# the PerformanceAnalytics function chart.Bar makes it easier 
# to compare the returns of different assets on the same plot
?charts.Bar
charts.Bar(lab4Returns.z, legend.loc="bottomright", main="Monthly cc returns on 3 assets")


# cumulative return plot
# must use simple returns and not cc returns for this
# use PerformanceAnalytics function chart.CumReturns()
?chart.CumReturns
chart.CumReturns(diff(lab4Prices.z)/lag(lab4Prices.z, k=-1), # notice this part defines the simple return
                 legend.loc="topleft", wealth.index=TRUE,
                 main="Future Value of $1 invested")

#
# Create matrix of return data. some core R functions don't work
# correctly with zoo objects 
#

ret.mat = coredata(lab4Returns.z)
class(ret.mat)
colnames(ret.mat)
head(ret.mat)

#
# Create graphical summaries of each data series
#

# online help on hist, boxplot, density, qqnorm
?hist
?boxplot
?density
?qqnorm

# here are the 4 panel plots for viewing the empirical distribution
# drop=FALSE preserves the column name
fourPanelPlot(ret.mat[, "VBLTX", drop=FALSE])
fourPanelPlot(ret.mat[, "FMAGX", drop=FALSE])
fourPanelPlot(ret.mat[, "NFLX", drop=FALSE])

# If there are missing or distorted components in the plot
# Try the following command to reset your graphics device,
# which will will remove any leftover options or settings from previous plots.
dev.off()


# show boxplot of three series on one plot
boxplot(ret.mat, col="cornflowerblue")

# do the same thing using the PerformanceAnalytics function chart.Boxplot
chart.Boxplot(lab4Returns.z)

#
# II. Univariate Numerical Summary Statistics
# 

summary(ret.mat)

# compute descriptive statistics by column using the base R function apply()
# note: skewness and kurtosis are in the package PerformanceAnalytics
# note: kurtosis returns excess kurtosis

?apply
args(apply)
apply(ret.mat, 2, mean)
apply(ret.mat, 2, var)
apply(ret.mat, 2, sd)
apply(ret.mat, 2, skewness)
apply(ret.mat, 2, kurtosis)

# A nice PerformanceAnalytics function that computes 
# all of the relevant descriptive statistics is table.Stats
?table.Stats
table.Stats(lab4Returns.z)

#
# Annualize monthly estimates
#

# annualized cc mean 
12*apply(ret.mat, 2, mean)

# annualized simple mean
exp(12*apply(ret.mat, 2, mean)) - 1

# annualized sd values
sqrt(12)*apply(ret.mat, 2, sd)


#
# III. Historical VaR
#
w0 = 100000
# matrix of empirical quantiles
q.hat = apply(ret.mat, 2, quantile, probs=c(0.01, 0.05))
q.hat

# compute all VaR values at once - isn't matrix programming great?
VaR.vals = w0*(exp(q.hat) - 1)
VaR.vals

#
# IV. Bivariate Graphical Analysis
# Compute bivariate descriptive statistics
#

# online help on pairs
?pairs
pairs(ret.mat, col="cornflowerblue", pch=16, cex=1.5)

# V. Bivariate Numerical Summary Statistics
# online help on var and cor
?var
?cor

# compute 3 x 3 covariance and correlation matrices
var(ret.mat)
cov(ret.mat)
cor(ret.mat)

# visualize the correlation matrix with corrplot()
cor.mat = cor(ret.mat)
corrplot(cor.mat, method="ellipse")
corrplot.mixed(cor.mat, lower="number", upper="ellipse")

# if using Rstudio, press the Clear All button on the 
# graph pane to reset the graphics window before making the next plots

#
# VI. Time Series Summary Statistics
# Compute time series diagnostics
#

# autocorrelations

# online help on acf
?acf

par(mfrow=c(3,1))
 acf.vbltx = acf(ret.mat[,"VBLTX"], main="VBLTX")
 acf.fmagx = acf(ret.mat[,"FMAGX"], main="FMAGX")
 acf.nflx = acf(ret.mat[,"NFLX"], main="NFLX")
par(mfrow=c(1,1))


#
# Part II: Constant Expected Return Model
# Compute estimates of CER model parameters
#
muhat.vals = apply(ret.mat, 2, mean)
muhat.vals
sigma2hat.vals = apply(ret.mat, 2, var)
sigma2hat.vals
sigmahat.vals = apply(ret.mat, 2, sd)
sigmahat.vals
cov.mat = var(ret.mat)
cov.mat
cor.mat = cor(ret.mat)
cor.mat
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- 
  c("VBLTX,FMAGX","VBLTX,NFLX","FMAGX,NFLX")
covhat.vals
rhohat.vals

cbind(muhat.vals,sigma2hat.vals,sigmahat.vals)
cbind(covhat.vals,rhohat.vals)

#
# Compute standard errors for estimated parameters
#

# compute estimated standard error for mean
nobs = nrow(ret.mat)
nobs
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat

cbind(muhat.vals,se.muhat)

# compute approx 95% confidence intervals
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)

# compute estimated standard errors for variance and sd
se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigma2hat
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

cbind(sigma2hat.vals,se.sigma2hat)
cbind(sigmahat.vals,se.sigmahat)

# compute approx 95% confidence intervals
sigma2.lower = sigma2hat.vals - 2*se.sigma2hat
sigma2.upper = sigma2hat.vals + 2*se.sigma2hat
cbind(sigma2.lower,sigma2.upper)

sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
cbind(sigma.lower,sigma.upper)

# compute estimated standard errors for correlation
se.rhohat = (1-rhohat.vals^2)/sqrt(nobs)
se.rhohat
cbind(rhohat.vals,se.rhohat)

# compute approx 95% confidence intervals
rho.lower = rhohat.vals - 2*se.rhohat
rho.upper = rhohat.vals + 2*se.rhohat
cbind(rho.lower,rho.upper)

## ADD FOR YOURSELF: Compute the 99% confidence intervals ##

YOUR ANSWER

#
# Compute 5% and 1% Value at Risk
#

# define the function to compute Value-at-Risk
# note: default values are selected for 
# the probability level (p) and the initial wealth (w)
# These values can be changed when calling the function.
# Highlight the entire function, 
# right click and select run line or selection
Value.at.Risk = function(x,p=0.05,w=100000) {
  x = as.matrix(x)
  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# 5% and 1% VaR estimates based on W0 = 100000

Value.at.Risk(ret.mat,p=0.05,w=100000)

## ADD FOR YOURSELF: 1% VaR estimates based on W0 = 100000 ##

YOUR ANSWER

