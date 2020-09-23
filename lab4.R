options(digits=4)
library(IntroCompFinR)
library(zoo)
library(xts)
library(tseries)
library(PerformanceAnalytics)
library(corrplot)

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

# look at the return data
start(lab4Returns.z)
end(lab4Returns.z)
colnames(lab4Returns.z) 
head(lab4Returns.z)

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

# cumulative return plot
# must use simple returns and not cc returns for this
# use PerformanceAnalytics function chart.CumReturns()
?chart.CumReturns
chart.CumReturns(diff(lab4Prices.z)/lag(lab4Prices.z, k=-1), # notice this part defines the simple return
                 legend.loc="topleft", wealth.index=TRUE,
                 main="Future Value of $1 invested")

# drop=FALSE preserves the column name
fourPanelPlot(ret.mat[, "VBLTX", drop=FALSE])
fourPanelPlot(ret.mat[, "FMAGX", drop=FALSE])
fourPanelPlot(ret.mat[, "NFLX", drop=FALSE])

# 2
summary(ret.mat)

# compute descriptive statistics by column using the base R function apply()
# note: skewness and kurtosis are in the package PerformanceAnalytics
# note: kurtosis returns excess kurtosis

args(apply)
apply(ret.mat, 2, mean)
apply(ret.mat, 2, var)
apply(ret.mat, 2, sd)
apply(ret.mat, 2, skewness)
apply(ret.mat, 2, kurtosis)

# A nice PerformanceAnalytics function that computes 
# all of the relevant descriptive statistics is table.Stats
table.Stats(lab4Returns.z)

# Annualize monthly estimates

# annualized cc mean 
12*apply(ret.mat, 2, mean)

# annualized simple mean
exp(12*apply(ret.mat, 2, mean)) - 1

# annualized sd values
sqrt(12)*apply(ret.mat, 2, sd)



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

pairs(ret.mat, col="cornflowerblue", pch=16, cex=1.5)

# compute 3 x 3 covariance and correlation matrices
var(ret.mat)
cov(ret.mat)
cor(ret.mat)

# visualize the correlation matrix with corrplot()
cor.mat = cor(ret.mat)
corrplot(cor.mat, method="ellipse")
corrplot.mixed(cor.mat, lower="number", upper="ellipse")

par(mfrow=c(3,1))
acf.vbltx = acf(ret.mat[,"VBLTX"], main="VBLTX")
acf.fmagx = acf(ret.mat[,"FMAGX"], main="FMAGX")
acf.nflx = acf(ret.mat[,"NFLX"], main="NFLX")
par(mfrow=c(1,1))

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

# Compute standard errors for estimated parameters
# compute estimated standard error for mean
nobs = nrow(ret.mat)
nobs
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat

cbind(muhat.vals,se.muhat)

# compute estimated standard error for variance
nobs = nrow(ret.mat)
se.sigma2hat = (((sigmahat.vals)*(sigmahat.vals)))/sqrt(nobs/2)
se.sigma2hat

cbind(sigma2hat.vals,se.sigma2hat)

# compute estimated standard error for standard deviation
nobs = nrow(ret.mat)
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

cbind(sigmahat.vals,se.sigmahat)

# compute standard error for correlation
nobs = nrow(ret.mat)
se.rhohat = (1-(rhohat.vals))/sqrt(nobs)
se.rhohat

cbind(rhohat.vals,se.rhohat)

# compute approx 95% confidence intervals for mu
mu.lower = muhat.vals - 2*se.muhat
mu.upper = muhat.vals + 2*se.muhat
cbind(mu.lower,mu.upper)

# compute approx 95% confidence intervals for sigma and sigma2
sigma2.lower = sigma2hat.vals - 2*se.sigma2hat
sigma2.upper = sigma2hat.vals + 2*se.sigma2hat
cbind(sigma2.lower,sigma2.upper)

sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
cbind(sigma.lower,sigma.upper)

# compute approx 95% confidence intervals for rho
rho.lower = rhohat.vals - 2*se.rhohat
rho.upper = rhohat.vals + 2*se.rhohat
cbind(rho.lower,rho.upper)

# compute approx 99% confidence intervals for mu
mu.lower = muhat.vals - 3*se.muhat
mu.upper = muhat.vals + 3*se.muhat
cbind(mu.lower,mu.upper)

# compute approx 99% confidence intervals for sigma and sigma2
sigma2.lower = sigma2hat.vals - 3*se.sigma2hat
sigma2.upper = sigma2hat.vals + 3*se.sigma2hat
cbind(sigma2.lower,sigma2.upper)

sigma.lower = sigmahat.vals - 3*se.sigmahat
sigma.upper = sigmahat.vals + 3*se.sigmahat
cbind(sigma.lower,sigma.upper)

# compute approx 99% confidence intervals for rho
rho.lower = rhohat.vals - 3*se.rhohat
rho.upper = rhohat.vals + 3*se.rhohat
cbind(rho.lower,rho.upper)

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

Value.at.Risk1 = function(x,p=0.01,w=100000) {
  x = as.matrix(x)
  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# 5% and 1% VaR estimates based on W0 = 100000

Value.at.Risk1(ret.mat,p=0.01,w=100000)