# Link: https://datascienceplus.com/structural-changes-in-global-warming/

# R Packages
suppressPackageStartupMessages(library(strucchange))
suppressPackageStartupMessages(library(fUnitRoots))
suppressPackageStartupMessages(library(astsa))

# Basic Data Exploration
data("globtemp")
globtemp

summary(globtemp)

plot(globtemp)
grid()

tt <- 1:length(globtemp)
fit <- ts(loess(globtemp ~ tt, span = 0.2)$fitted, start = 1880, frequency = 1)
plot(globtemp, type='l')
lines(fit, col = 4)
grid()

plot(density(globtemp), main = "globtemp density plot")

urdftest_lag = floor(12*(length(globtemp)/100)^0.25) # long
urdfTest(globtemp, lags = urdftest_lag, type = c("ct"), doplot = FALSE)

urkpssTest(globtemp, type = c("tau"), lags = c("long"),  doplot = FALSE)

# Structural Changes Analysis
summary(lm(globtemp ~ 1))

globtemp_win <- window(globtemp, end = 2000)
lev_fit <- lm(globtemp_win ~ 1)
summary(lev_fit)

plot(globtemp_win)
lines(ts(fitted(lev_fit), start = 1880, frequency = 1), col = 4)

globtemp_brk <- breakpoints(globtemp_win ~ 1, h = 0.1)
summary(globtemp_brk)

plot(globtemp_brk)

plot(globtemp_win)
lines(fitted(globtemp_brk, breaks = 5), col = 4)
lines(confint(globtemp_brk, breaks = 5))

breakdates(globtemp_brk, breaks = 5)

coef(globtemp_brk, breaks = 5)

# Trend Structural Changes
l <- length(globtemp)
tt <- 1:l
trend_fit <- lm(globtemp ~ tt)
summary(trend_fit)

plot(globtemp)
lines(ts(fitted(trend_fit), start=1880, frequency = 1), col = 4)

globtemp_brk <- breakpoints(globtemp ~ tt, h = 0.1)
summary(globtemp_brk)

plot(globtemp_brk)

plot(globtemp)
lines(fitted(globtemp_brk, breaks = 4), col = 4)
lines(confint(globtemp_brk, breaks = 4))

breakdates(globtemp_brk, breaks = 4)

coef(globtemp_brk, breaks = 4)

