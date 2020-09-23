options(digits=4, width=70)

library(corrplot)
library(IntroCompFinR)
library(PerformanceAnalytics)
library(zoo)

# load data from file (Make sure you change the path to where you downloaded the file)
lab5returns.df = read.csv(file="/Users/cuijy/Desktop/econ424lab5_return_1.csv",
                          stringsAsFactors=FALSE)

# Fix to problem with the yearmon class
dates = seq(as.Date("1992-07-01"), as.Date("2000-10-01"), by="months")
lab5returns.df$Date = dates
# create zoo object
lab5returns.z = zoo(lab5returns.df[,-1], lab5returns.df$Date)
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab5returns.z, lwd=2, col="blue", panel = my.panel)

# compute estimates of CER model and annualize
muhat.annual = apply(lab5returns.z,2,mean)*12   
sigma2.annual = apply(lab5returns.z,2,var)*12
sigma.annual = sqrt(sigma2.annual)
covmat.annual = cov(lab5returns.z)*12 
covhat.annual = cov(lab5returns.z)[1,2]*12   
rhohat.annual = cor(lab5returns.z)[1,2]

mu.s = muhat.annual["rsbux"]
mu.m = muhat.annual["rmsft"]
sig2.s =  sigma2.annual["rsbux"]
sig2.m = sigma2.annual["rmsft"]
sig.s = sigma.annual["rsbux"]
sig.m = sigma.annual["rmsft"]
sig.sm = covhat.annual
rho.sm = rhohat.annual


#
# create portfolios and plot
#
x.s = seq(from=-1, to=2, by=0.1)
x.m = 1 - x.s
mu.p = x.s*mu.s + x.m*mu.m
sig2.p = x.s^2 * sig2.s + x.m^2 * sig2.m + 2*x.s*x.m*sig.sm
sig.p = sqrt(sig2.p)
cbind(x.s, x.m, mu.p, sig.p, sig2.p)
plot(sig.p, mu.p, type="b", pch=16, 
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 14), rep("red", 17)))
text(x=sig.s, y=mu.s, labels="SBUX", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)

# now compute portfolios with assets and T-bills as well as Sharpe slopes

r.f = 0.025
# T-bills + SBUX
x.s = seq(from=0, to=2, by=0.1)
mu.p.s = r.f + x.s*(mu.s - r.f)
mu.p.s
sig2.p.s = x.s*x.s*sig.s*sig.s
sig2.p.s
sig.p.s = x.s*sig.s
sig.p.s
sharpe.s = (mu.s - r.f)/sig.s
sharpe.s

# T-bills + MSFT
x.m = seq(from=0, to=2, by=0.1)
mu.p.m = r.f + x.m*(mu.m - r.f)
mu.p.m
sig2.p.m = x.m*x.m*sig.m*sig.m
sig2.p.m
sig.p.m = x.m*sig.m
sig.p.m
sharpe.m = (mu.m - r.f)/sig.m
sharpe.m

plot(sig.p, mu.p, type="b", pch=16, 
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 14), rep("red", 17)))
text(x=sig.s, y=mu.s, labels="SBUX", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
points(sig.p.s, mu.p.s, type="b", col="blue")
points(sig.p.m, mu.p.m, type="b", col="orange")

plot(sig.p, mu.p, type="b", pch=16, ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)), 
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 14), rep("red", 17)))
text(x=sig.s, y=mu.s, labels="SBUX", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
points(sig.p.s, mu.p.s, type="b", col="blue")
points(sig.p.m, mu.p.m, type="b", col="orange")



# 2 compute global minimum variance portfolio

gmin.port = globalMin.portfolio(muhat.annual, covmat.annual) 
gmin.port
summary(gmin.port, risk.free=0.025)
plot(gmin.port)

pie(gmin.port$weights)

sharpe.gmin = (gmin.port$er - r.f)/gmin.port$sd
sharpe.gmin

plot(sig.p, mu.p, type="b", pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 14), rep("red", 17)))
text(x=sig.s, y=mu.s, labels="SBUX", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=gmin.port$sd, y=gmin.port$er, labels="Global min", pos=4)

sigma2.gim = 0.327^2
sigma2.gim



# compute tangency portfolio

tan.port = tangency.portfolio(muhat.annual, covmat.annual,risk.free=0.025) 
tan.port
summary(tan.port,risk.free=0.025)
plot(tan.port)
pie(tan.port$weights)                              


# T-bills + tangency
x.t = seq(from=0, to=2, by=0.1)
mu.p.t = r.f + x.t*(tan.port$er - r.f)
mu.p.t
sig.p.t = x.t*tan.port$sd
sig.p.t
sig2.p.t = sig.p.t^2
sig2.p.t
sharpe.t = (tan.port$er - r.f)/tan.port$sd
sharpe.t


plot(sig.p, mu.p, type="b", pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]),
     col=c(rep("green", 14), rep("red", 17)))
text(x=sig.s, y=mu.s, labels="SBUX", pos=4)
text(x=sig.m, y=mu.m, labels="MSFT", pos=4)
text(x=tan.port$sd, y=tan.port$er, labels="Tangency", pos=4)
points(sig.p.t, mu.p.t, type="b", col="blue", pch=16)


# part 2 Computing Efficient Portfolios Using Matrix Algebra
# Clear the the environment before part II
rm(list=ls())

options(digits=4, width=70)
library(zoo)
library(corrplot)
library(IntroCompFinR)
# load the data into a zoo object using the zoo function read.csv

lab5.df = read.csv(file="/Users/cuijy/Desktop/econ424lab5_return_2.csv",
                   stringsAsFactors=F)
colnames(lab5.df)

#
# Create zoo object from data and dates in lab5.df
#    

lab5.z = zoo(x=lab5.df[, -1], 
             order.by=as.yearmon(lab5.df[, 1], format="%b-%y"))
start(lab5.z)
end(lab5.z)
colnames(lab5.z)

ret.mat = coredata(lab5.z)

#
# Create timePlots of data
#

# create custom panel function to draw horizontal 
# line at zero in each panel of plot
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(lab5.z, lwd=2, panel=my.panel, col="blue")

# all on the same graph
plot(lab5.z, plot.type = "single", main="lab5 returns",
     col=1:4, lwd=2)
abline(h=0)
legend(x="bottomleft", legend=colnames(lab5.z), col=1:4, lwd=2)


#
# Compute pairwise scatterplots
#
pairs(coredata(lab5.z), col="blue", pch=16)
corrplot(cor(lab5.z), method="ellipse")
# clear the plots if use Rstudio

#
# Compute estimates of CER model parameters
#
muhat.vals = apply(ret.mat, 2, mean)
sigma2hat.vals = apply(ret.mat, 2, var)
sigmahat.vals = apply(ret.mat, 2, sd)
cov.mat = var(ret.mat)
cor.mat = cor(ret.mat)
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <-
  c("Nord,Boeing","SBUX,Boeing","MSFT,Boeing","SBUX,Nord",
    "MSFT,Nord","MSFT,SBUX")

muhat.vals
sigma2hat.vals
sigmahat.vals
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

# compute estimated standard errors for variance and sd
se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigma2hat
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigmahat

cbind(sigma2hat.vals,se.sigma2hat)
cbind(sigmahat.vals,se.sigmahat)

# compute estimated standard errors for correlation
se.rhohat = (1-rhohat.vals^2)/sqrt(nobs)
se.rhohat
cbind(rhohat.vals,se.rhohat)

#
# Export means and covariance matrix to .csv file for import to Excel.
#

write.csv(muhat.vals, file="/Users/cuijy/Desktop/muhatvals.csv")
write.csv(cov.mat, file="/Users/cuijy/Desktop/covmat.csv")

#
# portfolio theory calculations
#

# compute global minimum variance portfolio with short sales
gmin.port = globalMin.portfolio(muhat.vals, cov.mat)
gmin.port
plot(gmin.port, col="blue")


# compute efficient portfolio with target return equal to highest average return
mu.target = max(muhat.vals)
e1.port = efficient.portfolio(muhat.vals, cov.mat, mu.target)
e1.port
plot(e1.port, col="blue")

gmin.port.w = gmin.port[[4]]
e1.port.w = e1.port[[4]]
cov = t(gmin.port.w)%*%cov.mat%*%e1.port.w
cov

# compute efficient portfolio with target return equal to highest average return
# but do not allow short sales
mu.target = max(muhat.vals)
e1.noshorts.port = efficient.portfolio(muhat.vals, cov.mat, mu.target, shorts=FALSE)
e1.noshorts.port
plot(e1.noshorts.port, col="blue")

# compute global minimum variance portfolio without short sales
gmin.noshort.port = globalMin.portfolio(muhat.vals, cov.mat, shorts = FALSE)
gmin.noshort.port
plot(gmin.noshort.port, col="blue")

# compute tangency portfolio with rf = 0.005
tan.port = tangency.portfolio(muhat.vals, cov.mat, risk.free=0.005)
summary(tan.port)
plot(tan.port, col="blue")

# compute tangency portfolio with rf = 0.005
tan.noshort.port = tangency.portfolio(muhat.vals, cov.mat, 
                                      risk.free=0.005, shorts = FALSE)
summary(tan.noshort.port)
plot(tan.noshort.port, col="blue")

# Plot the efficient frontier
plot(e.frontier, plot.assets=T, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er,
       col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red",
       pch=16, cex=2)
text(gmin.port$sd, gmin.port$er,
     labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er,
     labels="TANGENCY", pos=2)
sharpe.tan = (tan.port$er - 0.005)/tan.port$sd
sharpe.tan
abline(a=0.005, b=sharpe.tan, col="green", lwd=2)

# efficient portfolio of T-bills + tangency that has the same SD as sbux
names(tan.port)
x.tan = sigmahat.vals["Starbucks"]/tan.port$sd
x.tan
mu.pe = 0.005 + x.tan*(tan.port$er - 0.005)
mu.pe

# VaR analysis
w0 = 50000
qhat.05 = muhat.vals + sigmahat.vals*qnorm(0.05)
qhat.01 = muhat.vals + sigmahat.vals*qnorm(0.01)
qhatGmin.05 = gmin.port$er + gmin.port$sd*qnorm(0.05)
qhatGmin.01 = gmin.port$er + gmin.port$sd*qnorm(0.01)
VaR.05 = w0*qhat.05
VaR.01 = w0*qhat.01
VaR.05
VaR.01
VaRgmin.05 = w0*qhatGmin.05
VaRgmin.01 = w0*qhatGmin.01
VaRgmin.05
VaRgmin.01