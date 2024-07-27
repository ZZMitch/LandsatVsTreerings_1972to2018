#####DendroLandsatWorkflow#####

##### Set up and Data #####
#####
library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop

### Bring in rwl ###
rwl = read.rwl("Ring Width Chronologies/M06/M06_Acer_1.rwl") # Change for each site
rwl.report(rwl)
plot(rwl, plot.type = "spag")
rwl.ids = read.ids(rwl, stc = c(4, 2, 1))
rwi.stats(rwl, rwl.ids, prewhiten = TRUE)
rwlstats = rwl.stats(rwl)
mean(rwlstats$year) # Age
sd(rwlstats$year)
mean(rwlstats$mean) # Ring widths
sd(rwlstats$mean)

rwl.crn = chron(rwl, prewhiten = TRUE)
yr = time(rwl)

plot(rwl.crn)

### Set smoothing rigidity ###
nyrs = 10  # Can test different numbers and see impact on correlations
# Defaults to length of chronology / 2

### Bring in Landsat time-series ###
lts = read.csv("Ring Width Chronologies/M06/DetrendingLandsat/M06_Landsat_1.csv", 
               row.names = 1) 
# Change for each site
plot(rownames(lts), lts$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts), lts$Avg_CC_Fitted)
# Outliers changed to average of surrounding years

# Just the one we want
lts.avg = lts[c("Avg_CC_Median")]
#####

##### Build RWI Chronologies #####
#####
### Non-detrended RWI Chronologies ###
rwl.sss = sss(rwl, rwl.ids)
cutoff = max(yr[rwl.sss < 0.85])
yr.cutoff = yr[yr > cutoff]

# Noisy
rwl.nodetrend.crn = subset(rwl.crn, yr > cutoff) # standard & residual
plot(rwl.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = nyrs)
# Standard on top, residual on bottom

# Smoothed
rwl.nodetrend.crn$std.smooth = ffcsaps(rwl.nodetrend.crn$xxxstd, nyrs = nyrs) # standard

#start = sum(is.na(rwl.nodetrend.crn$xxxres)) + 1
#rwl.nodetrend.crn$res.smooth[start:nrow(rwl.nodetrend.crn)] = 
#  ffcsaps(na.omit(rwl.nodetrend.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = nyrs)

### Mixed Method (Mean or ModNegExp depending on early growth) ###
rwl.mix = i.detrend(rwl)

rwl.mix.sss = sss(rwl.mix, rwl.ids)
cutoff.mix = max(yr[rwl.mix.sss < 0.85])
yr.cutoff.mix = yr[yr > cutoff.mix]

# Noisy
rwl.mix.crn = subset(chron(rwl.mix, prewhiten = TRUE), yr > cutoff.mix) 
# standard & residual
plot(rwl.mix.crn, ylab = "RWI (Mixed)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.mix.crn$std.smooth = ffcsaps(rwl.mix.crn$xxxstd, nyrs = nyrs) # standard

#start.mix = sum(is.na(rwl.mix.crn$xxxres)) + 1
#rwl.mix.crn$res.smooth[start.mix:nrow(rwl.mix.crn)] = 
#  ffcsaps(na.omit(rwl.mix.crn$xxxres), nyrs = nyrs)

plot(rwl.mix.crn, ylab = "RWI (Mixed)", add.spline = TRUE, nyrs = nyrs)

### Mean ###
rwl.mean = detrend(rwl, method = "Mean")

rwl.mean.sss = sss(rwl.mean, rwl.ids)
cutoff.mean = max(yr[rwl.mean.sss < 0.85])
yr.cutoff.mean = yr[yr > cutoff.mean]

# Noisy
rwl.mean.crn = chron(detrend(rwl[yr > cutoff.mean,], method = "Mean"), prewhiten = TRUE)
# standard & residual
plot(rwl.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.mean.crn$std.smooth = ffcsaps(rwl.mean.crn$xxxstd, nyrs = nyrs) # standard

#start.mean = sum(is.na(rwl.mean.crn$xxxres)) + 1
#rwl.mean.crn$res.smooth[start.mean:nrow(rwl.mean.crn)] = 
#  ffcsaps(na.omit(rwl.mean.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = nyrs)

### ModNegExp ###
rwl.modnegexp = detrend(rwl, method = "ModNegExp")

rwl.modnegexp.sss = sss(rwl.modnegexp, rwl.ids)
cutoff.modnegexp = max(yr[rwl.modnegexp.sss < 0.85])
yr.cutoff.modnegexp = yr[yr > cutoff.modnegexp]

# Noisy
rwl.modnegexp.crn = chron(detrend(rwl[yr > cutoff.modnegexp,], method = "ModNegExp"), 
                          prewhiten = TRUE)
# standard & residual
plot(rwl.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.modnegexp.crn$std.smooth = ffcsaps(rwl.modnegexp.crn$xxxstd, nyrs = nyrs) # standard

#start.modnegexp = sum(is.na(rwl.modnegexp.crn$xxxres)) + 1
#rwl.modnegexp.crn$res.smooth[start.modnegexp:nrow(rwl.modnegexp.crn)] = 
#  ffcsaps(na.omit(rwl.modnegexp.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = nyrs)

### ModHugershoff ###
rwl.modhuger = detrend(rwl, method = "ModHugershoff")

rwl.modhuger.sss = sss(rwl.modhuger, rwl.ids)
cutoff.modhuger = max(yr[rwl.modhuger.sss < 0.85])
yr.cutoff.modhuger = yr[yr > cutoff.modhuger]

# Noisy
rwl.modhuger.crn = chron(detrend(rwl[yr > cutoff.modhuger,], method = "ModHugershoff"), 
                         prewhiten = TRUE)
# standard & residual
plot(rwl.modhuger.crn, ylab = "RWI (ModHugershoff)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.modhuger.crn$std.smooth = ffcsaps(rwl.modhuger.crn$xxxstd, nyrs = nyrs) # standard

#start.modhuger = sum(is.na(rwl.modhuger.crn$xxxres)) + 1
#rwl.modhuger.crn$res.smooth[start.modhuger:nrow(rwl.modhuger.crn)] = 
#  ffcsaps(na.omit(rwl.modhuger.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.modhuger.crn, ylab = "RWI (ModHugershoff)", add.spline = TRUE, nyrs = nyrs)

### Spline ###
rwl.spline = detrend(rwl, method = "Spline")

rwl.spline.sss = sss(rwl.spline, rwl.ids)
cutoff.spline = max(yr[rwl.spline.sss < 0.85])
yr.cutoff.spline = yr[yr > cutoff.spline]

# Noisy
rwl.spline.crn = chron(detrend(rwl[yr > cutoff.spline,], method = "Spline"), 
                       prewhiten = TRUE)
# standard & residual
plot(rwl.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.spline.crn$std.smooth = ffcsaps(rwl.spline.crn$xxxstd, nyrs = nyrs) # standard

#start.spline = sum(is.na(rwl.spline.crn$xxxres)) + 1
#rwl.spline.crn$res.smooth[start.spline:nrow(rwl.spline.crn)] = 
#  ffcsaps(na.omit(rwl.spline.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = nyrs)

### Friedman ### 
rwl.friedman = detrend(rwl, method = "Friedman")

rwl.friedman.sss = sss(rwl.friedman, rwl.ids)
cutoff.friedman = max(yr[rwl.friedman.sss < 0.85])
yr.cutoff.friedman = yr[yr > cutoff.friedman]

# Noisy
rwl.friedman.crn = chron(detrend(rwl[yr > cutoff.friedman,], method = "Friedman"), 
                         prewhiten = TRUE)
# standard & residual
plot(rwl.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
rwl.friedman.crn$std.smooth = ffcsaps(rwl.friedman.crn$xxxstd, nyrs = nyrs) # standard

#start.friedman = sum(is.na(rwl.friedman.crn$xxxres)) + 1
#rwl.friedman.crn$res.smooth[start.friedman:nrow(rwl.friedman.crn)] = 
#  ffcsaps(na.omit(rwl.friedman.crn$xxxres), nyrs = nyrs) # residual

plot(rwl.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = nyrs)
#####

##### Build Landsat Time-series #####
#####
### Non-detrended ###
# Noisy
lts.nodetrend = chron(lts.avg, prewhiten = TRUE)
# standard and residual
plot(lts.nodetrend, ylab = "%CC", add.spline = TRUE, nyrs = nyrs)

# Smoothed
lts.nodetrend$std.smooth = ffcsaps(lts.nodetrend$xxxstd, nyrs = nyrs) # standard

#start.lts = sum(is.na(lts.nodetrend$xxxres)) + 1
#lts.nodetrend$res.smooth[start.lts:nrow(lts.nodetrend)] = 
#  ffcsaps(na.omit(lts.nodetrend$xxxres), nyrs = nyrs) # residual

plot(lts.nodetrend, ylab = "%CC", add.spline = TRUE, nyrs = nyrs)

### Mean ###
# Noisy
#lts.mean = chron(detrend(lts.avg, method = "Mean"), prewhiten = TRUE)
# standard and residual
#plot(lts.mean, ylab = "%CC (Mean)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
#lts.mean$std.smooth = ffcsaps(lts.mean$xxxstd, nyrs = nyrs) # standard

#start.mean.lts = sum(is.na(lts.mean$xxxres)) + 1
#lts.mean$res.smooth[start.mean.lts:nrow(lts.mean)] = 
#  ffcsaps(na.omit(lts.mean$xxxres), nyrs = nyrs) # residual

#plot(lts.mean, ylab = "%CC (Mean)", add.spline = TRUE, nyrs = nyrs)

### Spline ###
# Noisy
lts.spline = chron(detrend(lts.avg, method = "Spline"), prewhiten = TRUE)
# standard and residual
plot(lts.spline, ylab = "%CC (Spline)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
lts.spline$std.smooth = ffcsaps(lts.spline$xxxstd, nyrs = nyrs) # standard

#start.spline.lts = sum(is.na(lts.spline$xxxres)) + 1
#lts.spline$res.smooth[start.spline.lts:nrow(lts.spline)] = 
#  ffcsaps(na.omit(lts.spline$xxxres), nyrs = nyrs) # residual

plot(lts.spline, ylab = "%CC (Spline)", add.spline = TRUE, nyrs = nyrs)

### Friedman ###
# Noisy
lts.friedman = chron(detrend(lts.avg, method = "Friedman"), prewhiten = TRUE)
# standard and residual
plot(lts.friedman, ylab = "%CC (Friedman)", add.spline = TRUE, nyrs = nyrs)

# Smoothed
lts.friedman$std.smooth = ffcsaps(lts.friedman$xxxstd, nyrs = nyrs) # standard

#start.friedman.lts = sum(is.na(lts.friedman$xxxres)) + 1
#lts.friedman$res.smooth[start.friedman.lts:nrow(lts.friedman)] = 
#  ffcsaps(na.omit(lts.friedman$xxxres), nyrs = nyrs) # residual

plot(lts.friedman, ylab = "%CC (Friedman)", add.spline = TRUE, nyrs = nyrs)
#####

##### Test for Autocorrelation #####
#####
### RWI ###
# No Detrend
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.nodetrend.crn$xxxstd))
pacf(na.omit(rwl.nodetrend.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.nodetrend.crn$xxxres))
pacf(na.omit(rwl.nodetrend.crn$xxxres))
# noisy residual data should have no autocorrelation

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.nodetrend.crn$std.smooth))
pacf(na.omit(rwl.nodetrend.crn$std.smooth))
# Adds more autocorrelation back in

# Mix
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.mix.crn$xxxstd))
pacf(na.omit(rwl.mix.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.mix.crn$xxxres))
pacf(na.omit(rwl.mix.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.mix.crn$std.smooth))
pacf(na.omit(rwl.mix.crn$std.smooth))

# Mean
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.mean.crn$xxxstd))
pacf(na.omit(rwl.mean.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.mean.crn$xxxres))
pacf(na.omit(rwl.mean.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.mean.crn$std.smooth))
pacf(na.omit(rwl.mean.crn$std.smooth))

# ModNegExp
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.modnegexp.crn$xxxstd))
pacf(na.omit(rwl.modnegexp.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.modnegexp.crn$xxxres))
pacf(na.omit(rwl.modnegexp.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.modnegexp.crn$std.smooth))
pacf(na.omit(rwl.modnegexp.crn$std.smooth))

# ModHuger
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.modhuger.crn$xxxstd))
pacf(na.omit(rwl.modhuger.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.modhuger.crn$xxxres))
pacf(na.omit(rwl.modhuger.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.modhuger.crn$std.smooth))
pacf(na.omit(rwl.modhuger.crn$std.smooth))

# Spline
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.spline.crn$xxxstd))
pacf(na.omit(rwl.spline.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.spline.crn$xxxres))
pacf(na.omit(rwl.spline.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.spline.crn$std.smooth))
pacf(na.omit(rwl.spline.crn$std.smooth))

# Friedman
par(mfcol = c(1, 2)) # standard
acf(na.omit(rwl.friedman.crn$xxxstd))
pacf(na.omit(rwl.friedman.crn$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(rwl.friedman.crn$xxxres))
pacf(na.omit(rwl.friedman.crn$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(rwl.friedman.crn$std.smooth))
pacf(na.omit(rwl.friedman.crn$std.smooth))

### Landsat Time-series ###
# No Detrend
par(mfcol = c(1, 2)) # standard
acf(na.omit(lts.nodetrend$xxxstd))
pacf(na.omit(lts.nodetrend$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(lts.nodetrend$xxxres))
pacf(na.omit(lts.nodetrend$xxxres))
# noisy residual data should have no autocorrelation

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(lts.nodetrend$std.smooth))
pacf(na.omit(lts.nodetrend$std.smooth))
# Adds more autocorrelation back in

# Mean
#par(mfcol = c(1, 2)) # standard
#acf(na.omit(lts.mean$xxxstd))
#pacf(na.omit(lts.mean$xxxstd))

#par(mfcol = c(1, 2)) # residual
#acf(na.omit(lts.mean$xxxres))
#pacf(na.omit(lts.mean$xxxres))

#par(mfcol = c(1, 2)) # standard smooth
#acf(na.omit(lts.mean$std.smooth))
#pacf(na.omit(lts.mean$std.smooth))

# Spline
par(mfcol = c(1, 2)) # standard
acf(na.omit(lts.spline$xxxstd))
pacf(na.omit(lts.spline$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(lts.spline$xxxres))
pacf(na.omit(lts.spline$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(lts.spline$std.smooth))
pacf(na.omit(lts.spline$std.smooth))

# Friedman
par(mfcol = c(1, 2)) # standard
acf(na.omit(lts.friedman$xxxstd))
pacf(na.omit(lts.friedman$xxxstd))

par(mfcol = c(1, 2)) # residual
acf(na.omit(lts.friedman$xxxres))
pacf(na.omit(lts.friedman$xxxres))

par(mfcol = c(1, 2)) # standard smooth
acf(na.omit(lts.friedman$std.smooth))
pacf(na.omit(lts.friedman$std.smooth))

dev.off()
#####

##### Correlations #####
### Neff function ###
calc.neff <- function(x,y){
  x.ar1 = acf(x,plot=F)
  sig.lvl = qnorm((1 + 0.95)/2)/sqrt(x.ar1$n.used)
  x.ar1 = x.ar1$acf[2,1,1]
  x.ar1 = ifelse(x.ar1 < sig.lvl, 0, x.ar1)
  
  y.ar1 = acf(y,plot=F)
  sig.lvl = qnorm((1 + 0.9)/2)/sqrt(y.ar1$n.used)
  y.ar1 = y.ar1$acf[2,1,1]
  y.ar1 = ifelse(y.ar1 < sig.lvl, 0, y.ar1)
  
  n <- length(x)
  neff <- round(n*(1-x.ar1*y.ar1)/(1+x.ar1*y.ar1)) # originally floor()
  neff
} 
# Finds first order autocorrelation, finds autocorrelation value at edge of significance
# If autocorrelation < edge autocorrelation then no penalty
# Else drop effective sample size based on size of autocorrelation (x and y)

### Adjusted cor.test function ###
my.cor.test <- function (x, y, alternative = c("two.sided", "less", "greater"),
                         method = c("pearson", "kendall", "spearman"), exact = NULL,
                         conf.level = 0.95, n = length(x), ...)
{
  alternative <- match.arg(alternative)
  method <- match.arg(method)
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  if (length(x) != length(y))
    stop("'x' and 'y' must have the same length")
  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  #    n <- length(x), added n as an input to allow for using neff
  PVAL <- NULL
  NVAL <- 0
  conf.int <- FALSE
  if (method == "pearson") {
    if (n < 3)
      stop("not enough finite observations")
    method <- "Pearson's product-moment correlation"
    names(NVAL) <- "correlation"
    r <- cor(x, y)
    df <- n - 2
    ESTIMATE <- c(cor = r)
    PARAMETER <- c(df = df)
    STATISTIC <- c(t = sqrt(df) * r/sqrt(1 - r^2))
    p <- pt(STATISTIC, df)
    if (n > 3) {
      if (!missing(conf.level) && (length(conf.level) !=
                                   1 || !is.finite(conf.level) || conf.level < 0 ||
                                   conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
      conf.int <- TRUE
      z <- atanh(r)
      sigma <- 1/sqrt(n - 3)
      cint <- switch(alternative, less = c(-Inf, z + sigma *
                                             qnorm(conf.level)), greater = c(z - sigma * qnorm(conf.level),
                                                                             Inf), two.sided = z + c(-1, 1) * sigma * qnorm((1 +
                                                                                                                               conf.level)/2))
      cint <- tanh(cint)
      attr(cint, "conf.level") <- conf.level
    }
  }
  else {
    if (n < 2)
      stop("not enough finite observations")
    PARAMETER <- NULL
    TIES <- (min(length(unique(x)), length(unique(y))) <
               n)
    if (method == "kendall") {
      method <- "Kendall's rank correlation tau"
      names(NVAL) <- "tau"
      r <- cor(x, y, method = "kendall")
      ESTIMATE <- c(tau = r)
      if (!is.finite(ESTIMATE)) {
        ESTIMATE[] <- NA
        STATISTIC <- c(T = NA)
        PVAL <- NA
      }
      else {
        if (is.null(exact))
          exact <- (n < 50)
        if (exact && !TIES) {
          q <- round((r + 1) * n * (n - 1)/4)
          pkendall <- function(q, n) {
            .C("pkendall", length(q), p = as.double(q),
               as.integer(n), PACKAGE = "stats")$p
          }
          PVAL <- switch(alternative, two.sided = {
            if (q > n * (n - 1)/4)
              p <- 1 - pkendall(q - 1, n)
            else p <- pkendall(q, n)
            min(2 * p, 1)
          }, greater = 1 - pkendall(q - 1, n), less = pkendall(q,
                                                               n))
          STATISTIC <- c(T = q)
        }
        else {
          xties <- table(x[duplicated(x)]) + 1
          yties <- table(y[duplicated(y)]) + 1
          T0 <- n * (n - 1)/2
          T1 <- sum(xties * (xties - 1))/2
          T2 <- sum(yties * (yties - 1))/2
          S <- r * sqrt((T0 - T1) * (T0 - T2))
          v0 <- n * (n - 1) * (2 * n + 5)
          vt <- sum(xties * (xties - 1) * (2 * xties +
                                             5))
          vu <- sum(yties * (yties - 1) * (2 * yties +
                                             5))
          v1 <- sum(xties * (xties - 1)) * sum(yties *
                                                 (yties - 1))
          v2 <- sum(xties * (xties - 1) * (xties - 2)) *
            sum(yties * (yties - 1) * (yties - 2))
          var_S <- (v0 - vt - vu)/18 + v1/(2 * n * (n -
                                                      1)) + v2/(9 * n * (n - 1) * (n - 2))
          STATISTIC <- c(z = S/sqrt(var_S))
          p <- pnorm(STATISTIC)
          if (exact && TIES)
            warning("Cannot compute exact p-value with ties")
        }
      }
    }
    else {
      method <- "Spearman's rank correlation rho"
      if (is.null(exact))
        exact <- TRUE
      names(NVAL) <- "rho"
      r <- cor(rank(x), rank(y))
      ESTIMATE <- c(rho = r)
      if (!is.finite(ESTIMATE)) {
        ESTIMATE[] <- NA
        STATISTIC <- c(S = NA)
        PVAL <- NA
      }
      else {
        pspearman <- function(q, n, lower.tail = TRUE) {
          if (n <= 1290 && exact)
            .C("prho", as.integer(n), as.double(round(q) +
                                                  lower.tail), p = double(1), integer(1),
               as.logical(lower.tail), PACKAGE = "stats")$p
          else {
            r <- 1 - 6 * q/(n * (n^2 - 1))
            pt(r/sqrt((1 - r^2)/(n - 2)), df = n - 2,
               lower.tail = !lower.tail)
          }
        }
        q <- (n^3 - n) * (1 - r)/6
        STATISTIC <- c(S = q)
        if (TIES && exact) {
          exact <- FALSE
          warning("Cannot compute exact p-values with ties")
        }
        PVAL <- switch(alternative, two.sided = {
          p <- if (q > (n^3 - n)/6)
            pspearman(q, n, lower.tail = FALSE)
          else pspearman(q, n, lower.tail = TRUE)
          min(2 * p, 1)
        }, greater = pspearman(q, n, lower.tail = TRUE),
        less = pspearman(q, n, lower.tail = FALSE))
      }
    }
  }
  if (is.null(PVAL))
    PVAL <- switch(alternative, less = p, greater = 1 - p,
                   two.sided = 2 * min(p, 1 - p))
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
               p.value = as.numeric(PVAL), estimate = ESTIMATE, null.value = NVAL,
               alternative = alternative, method = method, data.name = DNAME)
  if (conf.int)
    RVAL <- c(RVAL, list(conf.int = cint))
  class(RVAL) <- "htest"
  RVAL
}
# Just like cor.test but can replace n with neff

#####
### rwl.nodetrend.crn vs. lts.nodetrend ###
plot(rwl.nodetrend.crn$xxxstd[yr.cutoff > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.nodetrend.crn$xxxstd[yr.cutoff > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.nodetrend.crn$xxxstd[yr.cutoff > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard
# Update n based on previous neff

plot(rwl.nodetrend.crn$xxxres[yr.cutoff > 1971], lts.nodetrend$xxxres)
# No Calc.neff because there is no autocorrelation in residuals
cor.test(rwl.nodetrend.crn$xxxres[yr.cutoff > 1971], lts.nodetrend$xxxres, # residual
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 

# Plot time-series
plot(1972:2018, rwl.nodetrend.crn$xxxstd[yr.cutoff > 1971], type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(1972:2018, rwl.nodetrend.crn$std.smooth[yr.cutoff > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.mix.crn vs. lts.nodetrend ###
plot(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.nodetrend$xxxstd, 
         alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.mix.crn$xxxres[yr.cutoff.mix > 1971], lts.nodetrend$xxxres)
cor.test(rwl.mix.crn$xxxres[yr.cutoff.mix > 1971], lts.nodetrend$xxxres, # residual
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson")

# Plot time-series
plot(1972:2018, rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(1972:2018, rwl.mix.crn$std.smooth[yr.cutoff.mix > 1971], col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.mean.crn vs. lts.nodetrend ###
plot(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.mean.crn$xxxres[yr.cutoff.mean > 1971], lts.nodetrend$xxxres)
cor.test(rwl.mean.crn$xxxres[yr.cutoff.mean > 1971], lts.nodetrend$xxxres, # residual
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson")

# Plot time-series
plot(1972:2018, rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(1972:2018, rwl.mean.crn$std.smooth[yr.cutoff.mean > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.modnegexp.crn vs. lts.nodetrend ###
plot(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.modnegexp.crn$xxxres[yr.cutoff.modnegexp > 1971], lts.nodetrend$xxxres)
cor.test(rwl.modnegexp.crn$xxxres[yr.cutoff.modnegexp > 1971], lts.nodetrend$xxxres, 
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 
# residual

# Plot time-series
plot(1972:2018, rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], type = "l", 
     col = "darkgreen", ylab = "", xlab = "")
lines(1972:2018, rwl.modnegexp.crn$std.smooth[yr.cutoff.modnegexp > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.modhuger.crn vs. lts.nodetrend ###
plot(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.modhuger.crn$xxxres[yr.cutoff.modhuger > 1971], lts.nodetrend$xxxres)
cor.test(rwl.modhuger.crn$xxxres[yr.cutoff.modhuger > 1971], lts.nodetrend$xxxres, 
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 
# residual

# Plot time-series
plot(1972:2018, rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], type = "l", 
     col = "darkgreen", ylab = "", xlab = "")
lines(1972:2018, rwl.modhuger.crn$std.smooth[yr.cutoff.modhuger > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.spline.crn vs. lts.nodetrend ###
plot(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.spline.crn$xxxres[yr.cutoff.spline > 1971], lts.nodetrend$xxxres)
cor.test(rwl.spline.crn$xxxres[yr.cutoff.spline > 1971], lts.nodetrend$xxxres, # residual
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson")

# Plot time-series
plot(1972:2018, rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], type = "l", 
     col = "darkgreen", ylab = "", xlab = "")
lines(1972:2018, rwl.spline.crn$std.smooth[yr.cutoff.spline > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.friedman.crn vs. lts.nodetrend ###
plot(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.nodetrend$xxxstd)
calc.neff(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.nodetrend$xxxstd)
my.cor.test(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.nodetrend$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.friedman.crn$xxxres[yr.cutoff.friedman > 1971], lts.nodetrend$xxxres)
cor.test(rwl.friedman.crn$xxxres[yr.cutoff.friedman > 1971], lts.nodetrend$xxxres, 
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 
# residual

# Plot time-series
plot(1972:2018, rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], type = "l", 
     col = "darkgreen", ylab = "", xlab = "")
lines(1972:2018, rwl.friedman.crn$std.smooth[yr.cutoff.friedman > 1971], 
      col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.nodetrend$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.nodetrend$std.smooth, col = "red3", lwd = 3)

### rwl.mix.crn vs. lts.mean ###
#plot(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.mean$xxxstd)
#calc.neff(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.mean$xxxstd)
#my.cor.test(rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], lts.mean$xxxstd, 
#            alternative = "greater", method = "pearson", n = 36) # standard

#plot(rwl.mix.crn$xxxres[yr.cutoff.mix > 1971], lts.mean$xxxres)
#cor.test(rwl.mix.crn$xxxres[yr.cutoff.mix > 1971], lts.mean$xxxres, # residual
#         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 

# Plot time-series
#plot(1972:2018, rwl.mix.crn$xxxstd[yr.cutoff.mix > 1971], type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(1972:2018, rwl.mix.crn$std.smooth[yr.cutoff.mix > 1971], col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.mean$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.mean$std.smooth, col = "red3", lwd = 3)

### rwl.mean.crn vs. lts.mean ###
#plot(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.mean$xxxstd)
#calc.neff(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.mean$xxxstd)
#my.cor.test(rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], lts.mean$xxxstd, 
#            alternative = "greater", method = "pearson", n = 34) # standard

#plot(rwl.mean.crn$xxxres[yr.cutoff.mean > 1971], lts.mean$xxxres)
#cor.test(rwl.mean.crn$xxxres[yr.cutoff.mean > 1971], lts.mean$xxxres, # residual
#         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 

# Plot time-series
#plot(1972:2018, rwl.mean.crn$xxxstd[yr.cutoff.mean > 1971], type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(1972:2018, rwl.mean.crn$std.smooth[yr.cutoff.mean > 1971], 
#      col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.mean$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.mean$std.smooth, col = "red3", lwd = 3)

### rwl.modnegexp.crn vs. lts.mean ###
#plot(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.mean$xxxstd)
#calc.neff(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.mean$xxxstd)
#my.cor.test(rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], lts.mean$xxxstd, 
#         alternative = "greater", method = "pearson", n = 36) # standard

#plot(rwl.modnegexp.crn$xxxres[yr.cutoff.modnegexp > 1971], lts.mean$xxxres)
#cor.test(rwl.modnegexp.crn$xxxres[yr.cutoff.modnegexp > 1971], lts.mean$xxxres, # residual
#         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 

# Plot time-series
#plot(1972:2018, rwl.modnegexp.crn$xxxstd[yr.cutoff.modnegexp > 1971], type = "l", 
#     col = "darkgreen", ylab = "", xlab = "")
#lines(1972:2018, rwl.modnegexp.crn$std.smooth[yr.cutoff.modnegexp > 1971], 
#      col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.mean$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.mean$std.smooth, col = "red3", lwd = 3)

### rwl.modhuger.crn vs. lts.mean ###
#plot(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.mean$xxxstd)
#calc.neff(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.mean$xxxstd)
#my.cor.test(rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], lts.mean$xxxstd, 
#         alternative = "greater", method = "pearson", n = 36) # standard

#plot(rwl.modhuger.crn$xxxres[yr.cutoff.modhuger > 1971], lts.mean$xxxres)
#cor.test(rwl.modhuger.crn$xxxres[yr.cutoff.modhuger > 1971], lts.mean$xxxres, # residual
#         alternative = "greater", use = "pairwise.complete.obs", method = "pearson")

# Plot time-series
#plot(1972:2018, rwl.modhuger.crn$xxxstd[yr.cutoff.modhuger > 1971], type = "l", 
#     col = "darkgreen", ylab = "", xlab = "")
#lines(1972:2018, rwl.modhuger.crn$std.smooth[yr.cutoff.modhuger > 1971], 
#      col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.mean$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.mean$std.smooth, col = "red3", lwd = 3)

### rwl.spline.crn vs. lts.spline ###
plot(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.spline$xxxstd)
calc.neff(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.spline$xxxstd)
my.cor.test(rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], lts.spline$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.spline.crn$xxxres[yr.cutoff.spline > 1971], lts.spline$xxxres)
cor.test(rwl.spline.crn$xxxres[yr.cutoff.spline > 1971], lts.spline$xxxres, # residual
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 

# Plot time-series
plot(1972:2018, rwl.spline.crn$xxxstd[yr.cutoff.spline > 1971], type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(1972:2018, rwl.spline.crn$std.smooth[yr.cutoff.spline > 1971], col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.spline$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.spline$std.smooth, col = "red3", lwd = 3)

### rwl.friedman.crn vs. lts.friedman ###
plot(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.friedman$xxxstd)
calc.neff(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.friedman$xxxstd)
my.cor.test(rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], lts.friedman$xxxstd, 
            alternative = "greater", method = "pearson", n = 47) # standard

plot(rwl.friedman.crn$xxxres[yr.cutoff.friedman > 1971], lts.friedman$xxxres)
cor.test(rwl.friedman.crn$xxxres[yr.cutoff.friedman > 1971], lts.friedman$xxxres, 
         alternative = "greater", use = "pairwise.complete.obs", method = "pearson") 
# residual

# Plot time-series
plot(1972:2018, rwl.friedman.crn$xxxstd[yr.cutoff.friedman > 1971], type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(1972:2018, rwl.friedman.crn$std.smooth[yr.cutoff.friedman > 1971], col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.friedman$xxxstd, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.friedman$std.smooth, col = "red3", lwd = 3)
#####
