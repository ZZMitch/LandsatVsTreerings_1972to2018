# Overall Correlation and Temporal Correlation #

##### Set up and Data #####
library(dplR)
library(dplyr)
library(gtools)
library(ggplot2)
library(patchwork)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop

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

# Smoothing spline length
nyrs = 10

##### 02Que/Ace/Car #####
# F15Que
rwl.F15Que = read.rwl("Ring Width Chronologies/F15/F15_Quercus_2.rwl")
rwl.report(rwl.F15Que)
rwl.F15Que.ids = read.ids(rwl.F15Que, stc = c(4, 2, 1))
yr.F15Que = time(rwl.F15Que)

rwl.F15Que.mne = detrend(rwl.F15Que, method = "ModNegExp")

rwl.F15Que.mne.sss = sss(rwl.F15Que.mne, rwl.F15Que.ids)
cut.F15Que.mne = max(yr.F15Que[rwl.F15Que.mne.sss < 0.85])
yr.cut.F15Que.mne = yr.F15Que[yr.F15Que > cut.F15Que.mne]

rwl.F15Que.mne.crn = chron(detrend(rwl.F15Que[yr.F15Que > cut.F15Que.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Que.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Ace
rwl.F15Ace = read.rwl("Ring Width Chronologies/F15/F15_Acer_1.rwl")
rwl.report(rwl.F15Ace)
rwl.F15Ace.ids = read.ids(rwl.F15Ace, stc = c(4, 2, 1))
yr.F15Ace = time(rwl.F15Ace)

rwl.F15Ace.mne = detrend(rwl.F15Ace, method = "ModNegExp")

rwl.F15Ace.mne.sss = sss(rwl.F15Ace.mne, rwl.F15Ace.ids)
cut.F15Ace.mne = max(yr.F15Ace[rwl.F15Ace.mne.sss < 0.85])
yr.cut.F15Ace.mne = yr.F15Ace[yr.F15Ace > cut.F15Ace.mne]

rwl.F15Ace.mne.crn = chron(detrend(rwl.F15Ace[yr.F15Ace > cut.F15Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Car
rwl.F15Car = read.rwl("Ring Width Chronologies/F15/F15_Carya_1.rwl")
rwl.report(rwl.F15Car)
rwl.F15Car.ids = read.ids(rwl.F15Car, stc = c(4, 2, 1))
yr.F15Car = time(rwl.F15Car)

rwl.F15Car.mne = detrend(rwl.F15Car, method = "ModNegExp")

rwl.F15Car.mne.sss = sss(rwl.F15Car.mne, rwl.F15Car.ids)
cut.F15Car.mne = max(yr.F15Car[rwl.F15Car.mne.sss < 0.85])
yr.cut.F15Car.mne = yr.F15Car[yr.F15Car > cut.F15Car.mne]

rwl.F15Car.mne.crn = chron(detrend(rwl.F15Car[yr.F15Car > cut.F15Car.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Car.mne.crn, add.spline = TRUE, nyrs = 10)

# F15
lts.F15 = read.csv("Ring Width Chronologies/F15/DetrendingLandsat/F15_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F15), lts.F15$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F15), lts.F15$Avg_CC_Fitted)

### Merge into one time-series with weight based on number of cores through time ###
f15tbl = data.frame(matrix(nrow = nrow(rwl.F15Que.mne.crn), ncol = 8)) # nrow of shortest chronology
colnames(f15tbl) = c("year", "rwi_que", "rwi_ace","rwi_car", "ss_que", "ss_ace", "ss_car", 
                     "rwi_combined")
f15tbl$year = 1909:2018
f15tbl$rwi_que = rwl.F15Que.mne.crn$xxxstd
f15tbl$ss_que = rwl.F15Que.mne.crn$samp.depth
f15tbl$rwi_ace = rwl.F15Ace.mne.crn$xxxstd[8:117]
f15tbl$ss_ace = rwl.F15Ace.mne.crn$samp.depth[8:117]
f15tbl$rwi_car = rwl.F15Car.mne.crn$xxxstd[9:118]
f15tbl$ss_car = rwl.F15Car.mne.crn$samp.depth[9:118]

for(i in 1:nrow(f15tbl)) { 
  f15tbl[i,8] = weighted.mean(as.numeric(f15tbl[i,2:4]), f15tbl[i,5:7])
}

plot(f15tbl$year, f15tbl$rwi_combined, type = "l")

# Calc Neff and Cor.test
F15comb.cor = my.cor.test(f15tbl$rwi_combined[64:110], lts.F15$Avg_CC_Median, alternative = "greater", 
                         method = "pearson", n = calc.neff(f15tbl$rwi_combined[64:110], lts.F15$Avg_CC_Median))
F15comb.cor
#####

##### 03Ace/Que #####
# M06Ace
rwl.M06Ace = read.rwl("Ring Width Chronologies/M06/M06_Acer_1.rwl")
rwl.report(rwl.M06Ace)
rwl.M06Ace.ids = read.ids(rwl.M06Ace, stc = c(4, 2, 1))
yr.M06Ace = time(rwl.M06Ace)

rwl.M06Ace.mne = detrend(rwl.M06Ace, method = "ModNegExp")

rwl.M06Ace.mne.sss = sss(rwl.M06Ace.mne, rwl.M06Ace.ids)
cut.M06Ace.mne = max(yr.M06Ace[rwl.M06Ace.mne.sss < 0.85])
yr.cut.M06Ace.mne = yr.M06Ace[yr.M06Ace > cut.M06Ace.mne]

rwl.M06Ace.mne.crn = chron(detrend(rwl.M06Ace[yr.M06Ace > cut.M06Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.M06Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# M06Que
rwl.M06Que = read.rwl("Ring Width Chronologies/M06/M06_Quercus_1.rwl")
rwl.report(rwl.M06Que)
rwl.M06Que.ids = read.ids(rwl.M06Que, stc = c(4, 2, 1))
yr.M06Que = time(rwl.M06Que)

rwl.M06Que.mne = detrend(rwl.M06Que, method = "ModNegExp")

rwl.M06Que.mne.sss = sss(rwl.M06Que.mne, rwl.M06Que.ids)
cut.M06Que.mne = max(yr.M06Que[rwl.M06Que.mne.sss < 0.85])
yr.cut.M06Que.mne = yr.M06Que[yr.M06Que > cut.M06Que.mne]

rwl.M06Que.mne.crn = chron(detrend(rwl.M06Que[yr.M06Que > cut.M06Que.mne,],
                                   method = "ModNegExp"))
plot(rwl.M06Que.mne.crn, add.spline = TRUE, nyrs = 10)

# M06
lts.M06 = read.csv("Ring Width Chronologies/M06/DetrendingLandsat/M06_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M06), lts.M06$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M06), lts.M06$Avg_CC_Fitted)

### Merge into one time-series with weight based on number of cores through time ###
m06tbl = data.frame(matrix(nrow = nrow(rwl.M06Que.mne.crn), ncol = 6)) # nrow of shortest chronology
colnames(m06tbl) = c("year", "rwi_ace", "rwi_que", "ss_ace", "ss_que", "rwi_combined")
m06tbl$year = 1901:2018
m06tbl$rwi_ace = rwl.M06Ace.mne.crn$xxxstd[3:120]
m06tbl$ss_ace = rwl.M06Ace.mne.crn$samp.depth[3:120]
m06tbl$rwi_que = rwl.M06Que.mne.crn$xxxstd
m06tbl$ss_que = rwl.M06Que.mne.crn$samp.depth

for(i in 1:nrow(m06tbl)) { 
  m06tbl[i,6] = weighted.mean(as.numeric(m06tbl[i,2:3]), m06tbl[i,4:5])
}

plot(m06tbl$year, m06tbl$rwi_combined, type = "l")

# Calc Neff and Cor.test
M06comb.cor = my.cor.test(m06tbl$rwi_combined[72:118], lts.M06$Avg_CC_Median, alternative = "greater", 
                          method = "pearson", n = calc.neff(m06tbl$rwi_combined[72:118], lts.M06$Avg_CC_Median))
M06comb.cor
#####

##### 10Thu/Dec #####
# M26Thu
rwl.M26Thu = read.rwl("Ring Width Chronologies/M26/M26_Thuja_1.rwl")
rwl.report(rwl.M26Thu)
rwl.M26Thu.ids = read.ids(rwl.M26Thu, stc = c(4, 2, 1))
yr.M26Thu = time(rwl.M26Thu)

rwl.M26Thu.mne = detrend(rwl.M26Thu, method = "ModNegExp")

rwl.M26Thu.mne.sss = sss(rwl.M26Thu.mne, rwl.M26Thu.ids)
cut.M26Thu.mne = max(yr.M26Thu[rwl.M26Thu.mne.sss < 0.85])
yr.cut.M26Thu.mne = yr.M26Thu[yr.M26Thu > cut.M26Thu.mne]

rwl.M26Thu.mne.crn = chron(detrend(rwl.M26Thu[yr.M26Thu > cut.M26Thu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M26Thu.mne.crn, add.spline = TRUE, nyrs = 10)

# M26Dec
rwl.M26Dec = read.rwl("Ring Width Chronologies/M26/M26_Deciduous_1.rwl")
rwl.report(rwl.M26Dec)
rwl.M26Dec.ids = read.ids(rwl.M26Dec, stc = c(4, 2, 1))
yr.M26Dec = time(rwl.M26Dec)

rwl.M26Dec.mne = detrend(rwl.M26Dec, method = "ModNegExp")

rwl.M26Dec.mne.sss = sss(rwl.M26Dec.mne, rwl.M26Dec.ids)
cut.M26Dec.mne = max(yr.M26Dec[rwl.M26Dec.mne.sss < 0.85])
yr.cut.M26Dec.mne = yr.M26Dec[yr.M26Dec > cut.M26Dec.mne]

rwl.M26Dec.mne.crn = chron(detrend(rwl.M26Dec[yr.M26Dec > cut.M26Dec.mne,],
                                   method = "ModNegExp"))
plot(rwl.M26Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# M26
lts.M26 = read.csv("Ring Width Chronologies/M26/DetrendingLandsat/M26_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M26), lts.M26$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M26), lts.M26$Avg_CC_Fitted)

### Merge into one time-series with weight based on number of cores through time ###
m26tbl = data.frame(matrix(nrow = nrow(rwl.M26Dec.mne.crn), ncol = 6)) # nrow of shortest chronology
colnames(m26tbl) = c("year", "rwi_thu", "rwi_dec", "ss_thu", "ss_dec", "rwi_combined")
m26tbl$year = 1920:2018
m26tbl$rwi_thu = rwl.M26Thu.mne.crn$xxxstd[17:115]
m26tbl$ss_thu = rwl.M26Thu.mne.crn$samp.depth[17:115]
m26tbl$rwi_dec = rwl.M26Dec.mne.crn$xxxstd
m26tbl$ss_dec = rwl.M26Dec.mne.crn$samp.depth

for(i in 1:nrow(m26tbl)) { 
  m26tbl[i,6] = weighted.mean(as.numeric(m26tbl[i,2:3]), m26tbl[i,4:5])
}

plot(m26tbl$year, m26tbl$rwi_combined, type = "l")

# Calc Neff and Cor.test
m26comb.cor = my.cor.test(m26tbl$rwi_combined[53:99], lts.M26$Avg_CC_Median, alternative = "greater", 
                          method = "pearson", n = calc.neff(m26tbl$rwi_combined[53:99], lts.M26$Avg_CC_Median))
m26comb.cor
#####

##### 15Bet/Ace #####
# F30Bet
rwl.F30Bet = read.rwl("Ring Width Chronologies/F30/F30_Betula_1.rwl")
rwl.report(rwl.F30Bet)
rwl.F30Bet.ids = read.ids(rwl.F30Bet, stc = c(4, 2, 1))
yr.F30Bet = time(rwl.F30Bet)

rwl.F30Bet.mne = detrend(rwl.F30Bet, method = "ModNegExp")

rwl.F30Bet.mne.sss = sss(rwl.F30Bet.mne, rwl.F30Bet.ids)
cut.F30Bet.mne = max(yr.F30Bet[rwl.F30Bet.mne.sss < 0.85])
yr.cut.F30Bet.mne = yr.F30Bet[yr.F30Bet > cut.F30Bet.mne]

rwl.F30Bet.mne.crn = chron(detrend(rwl.F30Bet[yr.F30Bet > cut.F30Bet.mne,],
                                   method = "ModNegExp"))
plot(rwl.F30Bet.mne.crn, add.spline = TRUE, nyrs = 10)

# F30Ace
rwl.F30Ace = read.rwl("Ring Width Chronologies/F30/F30_Acer_5.rwl")
rwl.report(rwl.F30Ace)
rwl.F30Ace.ids = read.ids(rwl.F30Ace, stc = c(4, 2, 1))
yr.F30Ace = time(rwl.F30Ace)

rwl.F30Ace.mne = detrend(rwl.F30Ace, method = "ModNegExp")

rwl.F30Ace.mne.sss = sss(rwl.F30Ace.mne, rwl.F30Ace.ids)
cut.F30Ace.mne = max(yr.F30Ace[rwl.F30Ace.mne.sss < 0.85])
yr.cut.F30Ace.mne = yr.F30Ace[yr.F30Ace > cut.F30Ace.mne]

rwl.F30Ace.mne.crn = chron(detrend(rwl.F30Ace[yr.F30Ace > cut.F30Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F30Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F30
lts.F30 = read.csv("Ring Width Chronologies/F30/DetrendingLandsat/F30_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F30), lts.F30$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F30), lts.F30$Avg_CC_Fitted)

### Merge into one time-series with weight based on number of cores through time ###
f30tbl = data.frame(matrix(nrow = nrow(rwl.F30Bet.mne.crn), ncol = 6)) # nrow of shortest chronology
colnames(f30tbl) = c("year", "rwi_bet", "rwi_ace", "ss_bet", "ss_ace", "rwi_combined")
f30tbl$year = 1942:2018
f30tbl$rwi_bet = rwl.F30Bet.mne.crn$xxxstd
f30tbl$ss_bet = rwl.F30Bet.mne.crn$samp.depth
f30tbl$rwi_ace = rwl.F30Ace.mne.crn$xxxstd[6:82]
f30tbl$ss_ace = rwl.F30Ace.mne.crn$samp.depth[6:82]

for(i in 1:nrow(f30tbl)) { 
  f30tbl[i,6] = weighted.mean(as.numeric(f30tbl[i,2:3]), f30tbl[i,4:5])
}

plot(f30tbl$year, f30tbl$rwi_combined, type = "l")

# Calc Neff and Cor.test
f30comb.cor = my.cor.test(f30tbl$rwi_combined[31:77], lts.F30$Avg_CC_Median, alternative = "greater", 
                          method = "pearson", n = calc.neff(f30tbl$rwi_combined[31:77], lts.F30$Avg_CC_Median))
f30comb.cor
#####