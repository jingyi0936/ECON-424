# X ~ N(0.25, (0.20)^2)
mu.x = 0.25
sigma.x = 0.20

# Pr(X > 0.10) 
1 - pnorm(0.10, mu.x, sigma.x)

# Pr(X < -0.10)
pnorm(-0.10, mu.x, sigma.x)

# Pr(-0.05 < X < 0.15) 
pnorm(0.15, mu.x, sigma.x) - pnorm(-0.05, mu.x, sigma.x)

# q.01, q.05, q.95, q.99
qnorm(c(0.01, 0.05, 0.50, 0.95), mu.x, sigma.x)


# X ~ N(0.15, (0.10)^2) and Y ~ N(0.25, (0.20)^2)
X = seq(-0.4, 0.8, 0.01) 
Y = seq(-0.4, 0.8, 0.01)
X.pdf = dnorm(X, mean = 0.15, sd = 0.10, log = FALSE)
Y.pdf = dnorm(Y, mean = 0.25, sd = 0.20, log = FALSE)

# Plot the pdf curve for Microsoft returns
plot(X, X.pdf, type = "l", col = "blue", lwd = 2,
     xlab = "X, Y", ylab = "pdf") 
segments(0.15, 0, 0.15, max(X.pdf), col = "blue", lwd = 2)

# Add Starbucks ruturns
lines(Y, Y.pdf, type = "l", col = "red", lwd = 2)
segments(0.25, 0, 0.25, max(Y.pdf), col = "red", lwd = 2)


# Assume that R ~ N(0.03, (0.08)^2) and that W0 = $10,000.
mu.R = 0.03
sigma.R = 0.08
W0 = 10000
R.val=seq(-0.4, 0.5, 0.001)
R.pdf=dnorm(R.val, mean = mu.R, sd = sigma.R, log = FALSE)
plot(R.val, R.pdf, type="l", col="blue", lwd=2,
     xlab="Returns", ylab="pdf")

# add shaded region below q_0.05
i = R.val <= qnorm(0.05, mu.R, sigma.R)
polygon(c(-0.4, R.val[i], qnorm(0.05, mu.R, sigma.R)), c(0, R.pdf[i], 0), col="blue")

# add shaded region below q_0.01
j = R.val <= qnorm(0.01, mu.R, sigma.R)
polygon(c(-0.4, R.val[j], qnorm(0.01, mu.R, sigma.R)), c(0, R.pdf[j], 0), col="red")

VaR.05 = W0*qnorm(0.05, mu.R, sigma.R)
abs(VaR.05)
VaR.01 = W0*qnorm(0.01, mu.R, sigma.R)
abs(VaR.01)



# Assume that r ~ N(0.03, (0.08)^2) and that W0 = $10,000.
mu.r = 0.03
sigma.r = 0.08
W0 = 10000

VaR.05 = W0*(exp(qnorm(0.05, mu.r, sigma.r)) - 1)
abs(VaR.05)
VaR.01 = W0*(exp(qnorm(0.01, mu.r, sigma.r)) - 1)
abs(VaR.01)

# 1% and 5% value-at-risk (VaR) over the year 
mu.r.12 = 12*mu.r   # r(12) = 12*r
sigma.r.12 = sqrt(12)*sigma.r

VaR.05.12 = W0*(exp(qnorm(0.05, mu.r.12, sigma.r.12)) - 1)
abs(VaR.05.12)
VaR.01.12 = W0*(exp(qnorm(0.01, mu.r.12, sigma.r.12)) - 1)
abs(VaR.01.12)

# PDF plot for r
r.val=seq(-0.4, 1, 0.001)
r.pdf=dnorm(r.val, mu.r, sigma.r, log = FALSE)
plot(r.val, r.pdf, type="l", col="blue", lwd=2,
     xlab="CC Returns", ylab="pdf")
# add PDF plot for r(12)
r.val.12=seq(-0.4, 1, 0.001)
r.pdf.12=dnorm(r.val.12, mu.r.12, sigma.r.12, log = FALSE)
lines(r.val.12, r.pdf.12, col="red", lwd=2)

# add a legend
legend(x="topright", legend=c("r", "r(12)"), 
       lty=1, lwd=2, col=c("blue","red"))



# First define the vector of prices
PA = c(1629.62, 1699.80)
PM = c(98.84, 98.61)

# simple monthly returns
RA = (PA[2] - PA[1])/PA[1]
RA
RM = (PM[2] - PM[1])/PM[1]
RM

# continuously compounded returns
rA = log(1 + RA)
rA
rM = log(1 + RM)
rM

# part c
DM = 0.10
RM.total = (PM[2] + DM- PM[1])/PM[1]
RM.total
DY = DM/PM[1]
DY

# part d
RA.annual = (1 + RA)^12 - 1
rA.annual = log(1 + RA.annual)
RA.annual
rA.annual

RM.annual = (1 + RM)^12 - 1
rM.annual = log(1 + RM.annual)
RM.annual
rM.annual

# part e
xA = 8000/10000
xM = 1 - xA
xA
xM

# part f
R.p = xA*RA + xM*RM
R.p
r.p = log(1 + R.p)
r.p
