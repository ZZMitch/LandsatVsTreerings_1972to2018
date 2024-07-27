# Correlation between tree rings / Landsat and monthy temperature and precipitation #

library(dplR)
library(dplyr)
library(gtools)
library(ggplot2)
library(patchwork)
library(treeclim)
library(ggtext)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/CompareClimate") 
#Laptop

##### Input Data #####
### Tree rings ###
rwi = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/FINAL TIMESERIES/RWI/combined_rwi_20.csv")
rwi = subset(rwi, Year > 1971) # Only overlap years (1972-2018)

#rwi_01Tsu = as.data.frame(rwi$M13Tsu)
#colnames(rwi_01Tsu) = c("rwi")
#row.names(rwi_01Tsu) = rwi$Year

#rwi_04Tsu = as.data.frame(rwi$M07Tsu)
#colnames(rwi_04Tsu) = c("rwi")
#row.names(rwi_04Tsu) = rwi$Year

### Landsat time-series ###
lts = read.csv("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/FINAL TIMESERIES/LTS/combined_lts.csv")
lts = subset(lts, Year > 1971) # Only overlap years (1972-2018)

#lts_01 = as.data.frame(lts$M13)
#colnames(lts_01) = c("cc")
#row.names(lts_01) = lts$Year

#lts_04 = as.data.frame(lts$M07)
#colnames(lts_04) = c("cc")
#row.names(lts_04) = lts$Year

### Temperature ###
# Monthly mean of daily mean (AHCCD)
temp_mm_P = read.csv("Adjusted Station/Temperature/mm6158731_PearsonAirport.csv")
# Pearson airport
#temp_mm_F = read.csv("Adjusted Station/Temperature/mm6142400_FergusDam.csv")
# Fergus Dam

# Monthly mean of daily max (AHCCD)
temp_mx_P = read.csv("Adjusted Station/Temperature/mx6158731_PearsonAirport.csv")
# Pearson airport

# Monthly mean of daily min (AHCCD)
temp_mn_P = read.csv("Adjusted Station/Temperature/mn6158731_PearsonAirport.csv")

temp_P = temp_mx_P 

### Precipitation ###
# Monthly total precipitaton (AHCCD - except recent couple of years... manually adjusted)
precip_P = read.csv("Adjusted Station/Precipitation/mt6158733_PearsonAirport_fix.csv")
# Pearson airport
#precip_O = read.csv("Adjusted Station/Precipitation/mt6155790_Orangeville.csv")
# Orangeville (Fergus last few years)

#####

##### Treeclim Option #####
#test = dcc(rwi_01Tsu, temp_mm_P[1:13])
#summary(test)
#plot(test)

#test = dcc(lts_01, temp_mm_P[1:13])
#summary(test)
#plot(test)

#test = dcc(rwi_04Tsu, temp_mm_P[1:13])
#summary(test)
#plot(test)

#test = dcc(lts_04, temp_mm_P[1:13])
#summary(test)
#plot(test)
#####

##### Adjust cor.test (used elsewhere in paper) #####
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

##### Create long-form tables to fill #####
# Temperature
temp_cor = as.data.frame(matrix(nrow = 340, ncol = 9))
colnames(temp_cor) = c("site", "temp",
                          "cor_rwi", "pval_rwi", "sig_rwi",
                          "cor_lts", "pval_lts", "sig_lts", "type")
temp_cor[1:17,1] = "01Tsu" # M13
temp_cor[18:34,1] = "02Que" # F15
temp_cor[35:51,1] = "03Ace" # M06
temp_cor[52:68,1] = "04Tsu" # M07
temp_cor[69:85,1] = "05Ace" # F23
temp_cor[86:102,1] = "06Dec" # F21
temp_cor[103:119,1] = "07Thu" # M05
temp_cor[120:136,1] = "08Ace" # F07
temp_cor[137:153,1] = "09Pin" # M27
temp_cor[154:170,1] = "10Thu" # M26
temp_cor[171:187,1] = "11Ace" # F25
temp_cor[188:204,1] = "12Pop" # M01
temp_cor[205:221,1] = "13Pic" # F33
temp_cor[222:238,1] = "14Thu" # M20
temp_cor[239:255,1] = "15Bet" # F30
temp_cor[256:272,1] = "16Thu" # M17
# New 4
temp_cor[273:289,1] = "02Ace" # F15
temp_cor[290:306,1] = "03Que" # M06
temp_cor[307:323,1] = "10Dec" # M26
temp_cor[324:340,1] = "15Ace" # F30

temp_cor[1:17,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                        "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[18:34,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                         "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[35:51,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                         "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[52:68,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                         "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[69:85,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                         "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[86:102,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                          "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[103:119,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[120:136,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[137:153,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[154:170,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[171:187,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[188:204,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[205:221,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[222:238,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[239:255,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[256:272,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[273:289,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[290:306,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[307:323,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor[324:340,2] = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                           "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP")
temp_cor$temp = factor(temp_cor$temp,
                          levels = c("may", "jun", "jul", "aug", "sep", "oct", "nov", "dec",
                                     "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP"),
                          ordered = TRUE)

# Type
temp_cor[1:17,9] = "Con" #"01Tsu" # M13
temp_cor[18:34,9] = "Dec" #02Que" # F15
temp_cor[35:51,9] = "Dec" #"03Ace" # M06
temp_cor[52:68,9] = "Con" #"04Tsu" # M07
temp_cor[69:85,9] = "Dec" #"05Ace" # F23
temp_cor[86:102,9] = "Dec" #"06Dec" # F21
temp_cor[103:119,9] = "Con" #"07Thu" # M05
temp_cor[120:136,9] = "Dec" #"08Ace" # F07
temp_cor[137:153,9] = "Con" #"09Pin" # M27
temp_cor[154:170,9] = "Con" #"10Thu" # M26
temp_cor[171:187,9] = "Dec" #"11Ace" # F25
temp_cor[188:204,9] = "Dec" #"12Pop" # M01
temp_cor[205:221,9] = "Con" #"13Pic" # F33
temp_cor[222:238,9] = "Con" #"14Thu" # M20
temp_cor[239:255,9] = "Dec" #"15Bet" # F30
temp_cor[256:272,9] = "Con" #"16Thu" # M17
# New 4
temp_cor[273:289,9] = "Dec" # F15
temp_cor[290:306,9] = "Dec" # M06
temp_cor[307:323,9] = "Dec" # M26
temp_cor[324:340,9] = "Dec" # F30

# Total Precipitation
precip_cor = temp_cor
#####

##### Temperature: 01Tsu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M13Tsu, temp_P$May[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$May[1:47]))
test
plot(rwi$M13Tsu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M13Tsu))
temp_cor[1,3] = test$estimate
temp_cor[1,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M13Tsu, temp_P$Jun[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Jun[1:47]))
test
plot(rwi$M13Tsu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M13Tsu))
temp_cor[2,3] = test$estimate
temp_cor[2,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M13Tsu, temp_P$Jul[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Jul[1:47]))
test
plot(rwi$M13Tsu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M13Tsu))
temp_cor[3,3] = test$estimate
temp_cor[3,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M13Tsu, temp_P$Aug[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Aug[1:47]))
test
plot(rwi$M13Tsu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M13Tsu))
temp_cor[4,3] = test$estimate
temp_cor[4,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M13Tsu, temp_P$Sep[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Sep[1:47]))
test
plot(rwi$M13Tsu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M13Tsu))
temp_cor[5,3] = test$estimate
temp_cor[5,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M13Tsu, temp_P$Oct[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Oct[1:47]))
test
plot(rwi$M13Tsu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M13Tsu))
temp_cor[6,3] = test$estimate
temp_cor[6,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M13Tsu, temp_P$Nov[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Nov[1:47]))
test
plot(rwi$M13Tsu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M13Tsu))
temp_cor[7,3] = test$estimate
temp_cor[7,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M13Tsu, temp_P$Dec[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Dec[1:47]))
test
plot(rwi$M13Tsu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M13Tsu))
temp_cor[8,3] = test$estimate
temp_cor[8,4] = test$p.value

# Current January
test = my.cor.test(rwi$M13Tsu, temp_P$Jan[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Jan[2:48]))
test
plot(rwi$M13Tsu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M13Tsu))
temp_cor[9,3] = test$estimate
temp_cor[9,4] = test$p.value

# Current February
test = my.cor.test(rwi$M13Tsu, temp_P$Feb[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Feb[2:48]))
test
plot(rwi$M13Tsu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M13Tsu))
temp_cor[10,3] = test$estimate
temp_cor[10,4] = test$p.value

# Current March
test = my.cor.test(rwi$M13Tsu, temp_P$Mar[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Mar[2:48]))
test
plot(rwi$M13Tsu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M13Tsu))
temp_cor[11,3] = test$estimate
temp_cor[11,4] = test$p.value

# Current April
test = my.cor.test(rwi$M13Tsu, temp_P$Apr[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Apr[2:48]))
test
plot(rwi$M13Tsu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M13Tsu))
temp_cor[12,3] = test$estimate
temp_cor[12,4] = test$p.value

# Current May
test = my.cor.test(rwi$M13Tsu, temp_P$May[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$May[2:48]))
test
plot(rwi$M13Tsu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M13Tsu))
temp_cor[13,3] = test$estimate
temp_cor[13,4] = test$p.value

# Current June
test = my.cor.test(rwi$M13Tsu, temp_P$Jun[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Jun[2:48]))
test
plot(rwi$M13Tsu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M13Tsu))
temp_cor[14,3] = test$estimate
temp_cor[14,4] = test$p.value

# Current July
test = my.cor.test(rwi$M13Tsu, temp_P$Jul[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Jul[2:48]))
test
plot(rwi$M13Tsu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M13Tsu))
temp_cor[15,3] = test$estimate
temp_cor[15,4] = test$p.value

# Current August
test = my.cor.test(rwi$M13Tsu, temp_P$Aug[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Aug[2:48]))
test
plot(rwi$M13Tsu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M13Tsu))
temp_cor[16,3] = test$estimate
temp_cor[16,4] = test$p.value

# Current September
test = my.cor.test(rwi$M13Tsu, temp_P$Sep[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Sep[2:48]))
test
plot(rwi$M13Tsu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M13Tsu))
temp_cor[17,3] = test$estimate
temp_cor[17,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M13, temp_P$May[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$May[1:47]))
test
plot(lts$M13, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M13))
temp_cor[1,6] = test$estimate
temp_cor[1,7] = test$p.value

# Previous June
test = my.cor.test(lts$M13, temp_P$Jun[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Jun[1:47]))
test
plot(lts$M13, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M13))
temp_cor[2,6] = test$estimate
temp_cor[2,7] = test$p.value

# Previous July
test = my.cor.test(lts$M13, temp_P$Jul[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Jul[1:47]))
test
plot(lts$M13, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M13))
temp_cor[3,6] = test$estimate
temp_cor[3,7] = test$p.value

# Previous August
test = my.cor.test(lts$M13, temp_P$Aug[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Aug[1:47]))
test
plot(lts$M13, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M13))
temp_cor[4,6] = test$estimate
temp_cor[4,7] = test$p.value

# Previous September
test = my.cor.test(lts$M13, temp_P$Sep[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Sep[1:47]))
test
plot(lts$M13, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M13))
temp_cor[5,6] = test$estimate
temp_cor[5,7] = test$p.value

# Previous October
test = my.cor.test(lts$M13, temp_P$Oct[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Oct[1:47]))
test
plot(lts$M13, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M13))
temp_cor[6,6] = test$estimate
temp_cor[6,7] = test$p.value

# Previous November
test = my.cor.test(lts$M13, temp_P$Nov[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Nov[1:47]))
test
plot(lts$M13, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M13))
temp_cor[7,6] = test$estimate
temp_cor[7,7] = test$p.value

# Previous December
test = my.cor.test(lts$M13, temp_P$Dec[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Dec[1:47]))
test
plot(lts$M13, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M13))
temp_cor[8,6] = test$estimate
temp_cor[8,7] = test$p.value

# Current January
test = my.cor.test(lts$M13, temp_P$Jan[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Jan[2:48]))
test
plot(lts$M13, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M13))
temp_cor[9,6] = test$estimate
temp_cor[9,7] = test$p.value

# Current February
test = my.cor.test(lts$M13, temp_P$Feb[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Feb[2:48]))
test
plot(lts$M13, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M13))
temp_cor[10,6] = test$estimate
temp_cor[10,7] = test$p.value

# Current March
test = my.cor.test(lts$M13, temp_P$Mar[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Mar[2:48]))
test
plot(lts$M13, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M13))
temp_cor[11,6] = test$estimate
temp_cor[11,7] = test$p.value

# Current April
test = my.cor.test(lts$M13, temp_P$Apr[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Apr[2:48]))
test
plot(lts$M13, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M13))
temp_cor[12,6] = test$estimate
temp_cor[12,7] = test$p.value

# Current May
test = my.cor.test(lts$M13, temp_P$May[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$May[2:48]))
test
plot(lts$M13, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M13))
temp_cor[13,6] = test$estimate
temp_cor[13,7] = test$p.value

# Current June
test = my.cor.test(lts$M13, temp_P$Jun[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Jun[2:48]))
test
plot(lts$M13, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M13))
temp_cor[14,6] = test$estimate
temp_cor[14,7] = test$p.value

# Current July
test = my.cor.test(lts$M13, temp_P$Jul[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Jul[2:48]))
test
plot(lts$M13, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M13))
temp_cor[15,6] = test$estimate
temp_cor[15,7] = test$p.value

# Current August
test = my.cor.test(lts$M13, temp_P$Aug[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Aug[2:48]))
test
plot(lts$M13, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M13))
temp_cor[16,6] = test$estimate
temp_cor[16,7] = test$p.value

# Current September
test = my.cor.test(lts$M13, temp_P$Sep[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Sep[2:48]))
test
plot(lts$M13, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M13))
temp_cor[17,6] = test$estimate
temp_cor[17,7] = test$p.value
#####

##### Temperature: Site 02Que #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F15Que, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$May[1:47]))
test
plot(rwi$F15Que, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F15Que))
temp_cor[18,3] = test$estimate
temp_cor[18,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F15Que, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Jun[1:47]))
test
plot(rwi$F15Que, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F15Que))
temp_cor[19,3] = test$estimate
temp_cor[19,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F15Que, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Jul[1:47]))
test
plot(rwi$F15Que, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F15Que))
temp_cor[20,3] = test$estimate
temp_cor[20,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F15Que, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Aug[1:47]))
test
plot(rwi$F15Que, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F15Que))
temp_cor[21,3] = test$estimate
temp_cor[21,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F15Que, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Sep[1:47]))
test
plot(rwi$F15Que, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F15Que))
temp_cor[22,3] = test$estimate
temp_cor[22,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F15Que, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Oct[1:47]))
test
plot(rwi$F15Que, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F15Que))
temp_cor[23,3] = test$estimate
temp_cor[23,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F15Que, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Nov[1:47]))
test
plot(rwi$F15Que, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F15Que))
temp_cor[24,3] = test$estimate
temp_cor[24,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F15Que, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Dec[1:47]))
test
plot(rwi$F15Que, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F15Que))
temp_cor[25,3] = test$estimate
temp_cor[25,4] = test$p.value

# Current January
test = my.cor.test(rwi$F15Que, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Jan[2:48]))
test
plot(rwi$F15Que, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F15Que))
temp_cor[26,3] = test$estimate
temp_cor[26,4] = test$p.value

# Current February
test = my.cor.test(rwi$F15Que, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Feb[2:48]))
test
plot(rwi$F15Que, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F15Que))
temp_cor[27,3] = test$estimate
temp_cor[27,4] = test$p.value

# Current March
test = my.cor.test(rwi$F15Que, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Mar[2:48]))
test
plot(rwi$F15Que, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F15Que))
temp_cor[28,3] = test$estimate
temp_cor[28,4] = test$p.value

# Current April
test = my.cor.test(rwi$F15Que, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Apr[2:48]))
test
plot(rwi$F15Que, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F15Que))
temp_cor[29,3] = test$estimate
temp_cor[29,4] = test$p.value

# Current May
test = my.cor.test(rwi$F15Que, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$May[2:48]))
test
plot(rwi$F15Que, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F15Que))
temp_cor[30,3] = test$estimate
temp_cor[30,4] = test$p.value

# Current June
test = my.cor.test(rwi$F15Que, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Jun[2:48]))
test
plot(rwi$F15Que, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F15Que))
temp_cor[31,3] = test$estimate
temp_cor[31,4] = test$p.value

# Current July
test = my.cor.test(rwi$F15Que, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Jul[2:48]))
test
plot(rwi$F15Que, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F15Que))
temp_cor[32,3] = test$estimate
temp_cor[32,4] = test$p.value

# Current August
test = my.cor.test(rwi$F15Que, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Aug[2:48]))
test
plot(rwi$F15Que, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F15Que))
temp_cor[33,3] = test$estimate
temp_cor[33,4] = test$p.value

# Current September
test = my.cor.test(rwi$F15Que, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Sep[2:48]))
test
plot(rwi$F15Que, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F15Que))
temp_cor[34,3] = test$estimate
temp_cor[34,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F15, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$May[1:47]))
test
plot(lts$F15, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F15))
temp_cor[18,6] = test$estimate
temp_cor[18,7] = test$p.value

# Previous June
test = my.cor.test(lts$F15, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Jun[1:47]))
test
plot(lts$F15, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F15))
temp_cor[19,6] = test$estimate
temp_cor[19,7] = test$p.value

# Previous July
test = my.cor.test(lts$F15, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Jul[1:47]))
test
plot(lts$F15, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F15))
temp_cor[20,6] = test$estimate
temp_cor[20,7] = test$p.value

# Previous August
test = my.cor.test(lts$F15, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Aug[1:47]))
test
plot(lts$F15, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F15))
temp_cor[21,6] = test$estimate
temp_cor[21,7] = test$p.value

# Previous September
test = my.cor.test(lts$F15, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Sep[1:47]))
test
plot(lts$F15, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F15))
temp_cor[22,6] = test$estimate
temp_cor[22,7] = test$p.value

# Previous October
test = my.cor.test(lts$F15, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Oct[1:47]))
test
plot(lts$F15, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F15))
temp_cor[23,6] = test$estimate
temp_cor[23,7] = test$p.value

# Previous November
test = my.cor.test(lts$F15, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Nov[1:47]))
test
plot(lts$F15, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F15))
temp_cor[24,6] = test$estimate
temp_cor[24,7] = test$p.value

# Previous December
test = my.cor.test(lts$F15, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Dec[1:47]))
test
plot(lts$F15, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F15))
temp_cor[25,6] = test$estimate
temp_cor[25,7] = test$p.value

# Current January
test = my.cor.test(lts$F15, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Jan[2:48]))
test
plot(lts$F15, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F15))
temp_cor[26,6] = test$estimate
temp_cor[26,7] = test$p.value

# Current February
test = my.cor.test(lts$F15, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Feb[2:48]))
test
plot(lts$F15, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F15))
temp_cor[27,6] = test$estimate
temp_cor[27,7] = test$p.value

# Current March
test = my.cor.test(lts$F15, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Mar[2:48]))
test
plot(lts$F15, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F15))
temp_cor[28,6] = test$estimate
temp_cor[28,7] = test$p.value

# Current April
test = my.cor.test(lts$F15, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Apr[2:48]))
test
plot(lts$F15, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F15))
temp_cor[29,6] = test$estimate
temp_cor[29,7] = test$p.value

# Current May
test = my.cor.test(lts$F15, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$May[2:48]))
test
plot(lts$F15, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F15))
temp_cor[30,6] = test$estimate
temp_cor[30,7] = test$p.value

# Current June
test = my.cor.test(lts$F15, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Jun[2:48]))
test
plot(lts$F15, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F15))
temp_cor[31,6] = test$estimate
temp_cor[31,7] = test$p.value

# Current July
test = my.cor.test(lts$F15, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Jul[2:48]))
test
plot(lts$F15, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F15))
temp_cor[32,6] = test$estimate
temp_cor[32,7] = test$p.value

# Current August
test = my.cor.test(lts$F15, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Aug[2:48]))
test
plot(lts$F15, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F15))
temp_cor[33,6] = test$estimate
temp_cor[33,7] = test$p.value

# Current September
test = my.cor.test(lts$F15, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, temp_P$Sep[2:48]))
test
plot(lts$F15, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F15))
temp_cor[34,6] = test$estimate
temp_cor[34,7] = test$p.value
#####

##### Temperature: Site 03Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M06Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$May[1:47]))
test
plot(rwi$M06Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M06Ace))
temp_cor[35,3] = test$estimate
temp_cor[35,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M06Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Jun[1:47]))
test
plot(rwi$M06Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M06Ace))
temp_cor[36,3] = test$estimate
temp_cor[36,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M06Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Jul[1:47]))
test
plot(rwi$M06Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M06Ace))
temp_cor[37,3] = test$estimate
temp_cor[37,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M06Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Aug[1:47]))
test
plot(rwi$M06Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M06Ace))
temp_cor[38,3] = test$estimate
temp_cor[38,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M06Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Sep[1:47]))
test
plot(rwi$M06Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M06Ace))
temp_cor[39,3] = test$estimate
temp_cor[39,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M06Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Oct[1:47]))
test
plot(rwi$M06Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M06Ace))
temp_cor[40,3] = test$estimate
temp_cor[40,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M06Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Nov[1:47]))
test
plot(rwi$M06Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M06Ace))
temp_cor[41,3] = test$estimate
temp_cor[41,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M06Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Dec[1:47]))
test
plot(rwi$M06Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M06Ace))
temp_cor[42,3] = test$estimate
temp_cor[42,4] = test$p.value

# Current January
test = my.cor.test(rwi$M06Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Jan[2:48]))
test
plot(rwi$M06Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M06Ace))
temp_cor[43,3] = test$estimate
temp_cor[43,4] = test$p.value

# Current February
test = my.cor.test(rwi$M06Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Feb[2:48]))
test
plot(rwi$M06Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M06Ace))
temp_cor[44,3] = test$estimate
temp_cor[44,4] = test$p.value

# Current March
test = my.cor.test(rwi$M06Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Mar[2:48]))
test
plot(rwi$M06Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M06Ace))
temp_cor[45,3] = test$estimate
temp_cor[45,4] = test$p.value

# Current April
test = my.cor.test(rwi$M06Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Apr[2:48]))
test
plot(rwi$M06Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M06Ace))
temp_cor[46,3] = test$estimate
temp_cor[46,4] = test$p.value

# Current May
test = my.cor.test(rwi$M06Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$May[2:48]))
test
plot(rwi$M06Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M06Ace))
temp_cor[47,3] = test$estimate
temp_cor[47,4] = test$p.value

# Current June
test = my.cor.test(rwi$M06Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Jun[2:48]))
test
plot(rwi$M06Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M06Ace))
temp_cor[48,3] = test$estimate
temp_cor[48,4] = test$p.value

# Current July
test = my.cor.test(rwi$M06Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Jul[2:48]))
test
plot(rwi$M06Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M06Ace))
temp_cor[49,3] = test$estimate
temp_cor[49,4] = test$p.value

# Current August
test = my.cor.test(rwi$M06Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Aug[2:48]))
test
plot(rwi$M06Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M06Ace))
temp_cor[50,3] = test$estimate
temp_cor[50,4] = test$p.value

# Current September
test = my.cor.test(rwi$M06Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Sep[2:48]))
test
plot(rwi$M06Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M06Ace))
temp_cor[51,3] = test$estimate
temp_cor[51,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M06, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$May[1:47]))
test
plot(lts$M06, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M06))
temp_cor[35,6] = test$estimate
temp_cor[35,7] = test$p.value

# Previous June
test = my.cor.test(lts$M06, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Jun[1:47]))
test
plot(lts$M06, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M06))
temp_cor[36,6] = test$estimate
temp_cor[36,7] = test$p.value

# Previous July
test = my.cor.test(lts$M06, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Jul[1:47]))
test
plot(lts$M06, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M06))
temp_cor[37,6] = test$estimate
temp_cor[37,7] = test$p.value

# Previous August
test = my.cor.test(lts$M06, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Aug[1:47]))
test
plot(lts$M06, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M06))
temp_cor[38,6] = test$estimate
temp_cor[38,7] = test$p.value

# Previous September
test = my.cor.test(lts$M06, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Sep[1:47]))
test
plot(lts$M06, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M06))
temp_cor[39,6] = test$estimate
temp_cor[39,7] = test$p.value

# Previous October
test = my.cor.test(lts$M06, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Oct[1:47]))
test
plot(lts$M06, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M06))
temp_cor[40,6] = test$estimate
temp_cor[40,7] = test$p.value

# Previous November
test = my.cor.test(lts$M06, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Nov[1:47]))
test
plot(lts$M06, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M06))
temp_cor[41,6] = test$estimate
temp_cor[41,7] = test$p.value

# Previous December
test = my.cor.test(lts$M06, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Dec[1:47]))
test
plot(lts$M06, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M06))
temp_cor[42,6] = test$estimate
temp_cor[42,7] = test$p.value

# Current January
test = my.cor.test(lts$M06, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Jan[2:48]))
test
plot(lts$M06, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M06))
temp_cor[43,6] = test$estimate
temp_cor[43,7] = test$p.value

# Current February
test = my.cor.test(lts$M06, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Feb[2:48]))
test
plot(lts$M06, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M06))
temp_cor[44,6] = test$estimate
temp_cor[44,7] = test$p.value

# Current March
test = my.cor.test(lts$M06, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Mar[2:48]))
test
plot(lts$M06, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M06))
temp_cor[45,6] = test$estimate
temp_cor[45,7] = test$p.value

# Current April
test = my.cor.test(lts$M06, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Apr[2:48]))
test
plot(lts$M06, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M06))
temp_cor[46,6] = test$estimate
temp_cor[46,7] = test$p.value

# Current May
test = my.cor.test(lts$M06, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$May[2:48]))
test
plot(lts$M06, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M06))
temp_cor[47,6] = test$estimate
temp_cor[47,7] = test$p.value

# Current June
test = my.cor.test(lts$M06, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Jun[2:48]))
test
plot(lts$M06, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M06))
temp_cor[48,6] = test$estimate
temp_cor[48,7] = test$p.value

# Current July
test = my.cor.test(lts$M06, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Jul[2:48]))
test
plot(lts$M06, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M06))
temp_cor[49,6] = test$estimate
temp_cor[49,7] = test$p.value

# Current August
test = my.cor.test(lts$M06, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Aug[2:48]))
test
plot(lts$M06, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M06))
temp_cor[50,6] = test$estimate
temp_cor[50,7] = test$p.value

# Current September
test = my.cor.test(lts$M06, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, temp_P$Sep[2:48]))
test
plot(lts$M06, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M06))
temp_cor[51,6] = test$estimate
temp_cor[51,7] = test$p.value
#####

##### Temperature: Site 04Tsu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M07Tsu, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$May[1:47]))
test
plot(rwi$M07Tsu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M07Tsu))
temp_cor[52,3] = test$estimate
temp_cor[52,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M07Tsu, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Jun[1:47]))
test
plot(rwi$M07Tsu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M07Tsu))
temp_cor[53,3] = test$estimate
temp_cor[53,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M07Tsu, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Jul[1:47]))
test
plot(rwi$M07Tsu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M07Tsu))
temp_cor[54,3] = test$estimate
temp_cor[54,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M07Tsu, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Aug[1:47]))
test
plot(rwi$M07Tsu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M07Tsu))
temp_cor[55,3] = test$estimate
temp_cor[55,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M07Tsu, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Sep[1:47]))
test
plot(rwi$M07Tsu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M07Tsu))
temp_cor[56,3] = test$estimate
temp_cor[56,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M07Tsu, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Oct[1:47]))
test
plot(rwi$M07Tsu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M07Tsu))
temp_cor[57,3] = test$estimate
temp_cor[57,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M07Tsu, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Nov[1:47]))
test
plot(rwi$M07Tsu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M07Tsu))
temp_cor[58,3] = test$estimate
temp_cor[58,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M07Tsu, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Dec[1:47]))
test
plot(rwi$M07Tsu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M07Tsu))
temp_cor[59,3] = test$estimate
temp_cor[59,4] = test$p.value

# Current January
test = my.cor.test(rwi$M07Tsu, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Jan[2:48]))
test
plot(rwi$M07Tsu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M07Tsu))
temp_cor[60,3] = test$estimate
temp_cor[60,4] = test$p.value

# Current February
test = my.cor.test(rwi$M07Tsu, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Feb[2:48]))
test
plot(rwi$M07Tsu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M07Tsu))
temp_cor[61,3] = test$estimate
temp_cor[61,4] = test$p.value

# Current March
test = my.cor.test(rwi$M07Tsu, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Mar[2:48]))
test
plot(rwi$M07Tsu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M07Tsu))
temp_cor[62,3] = test$estimate
temp_cor[62,4] = test$p.value

# Current April
test = my.cor.test(rwi$M07Tsu, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Apr[2:48]))
test
plot(rwi$M07Tsu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M07Tsu))
temp_cor[63,3] = test$estimate
temp_cor[63,4] = test$p.value

# Current May
test = my.cor.test(rwi$M07Tsu, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$May[2:48]))
test
plot(rwi$M07Tsu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M07Tsu))
temp_cor[64,3] = test$estimate
temp_cor[64,4] = test$p.value

# Current June
test = my.cor.test(rwi$M07Tsu, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Jun[2:48]))
test
plot(rwi$M07Tsu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M07Tsu))
temp_cor[65,3] = test$estimate
temp_cor[65,4] = test$p.value

# Current July
test = my.cor.test(rwi$M07Tsu, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Jul[2:48]))
test
plot(rwi$M07Tsu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M07Tsu))
temp_cor[66,3] = test$estimate
temp_cor[66,4] = test$p.value

# Current August
test = my.cor.test(rwi$M07Tsu, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Aug[2:48]))
test
plot(rwi$M07Tsu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M07Tsu))
temp_cor[67,3] = test$estimate
temp_cor[67,4] = test$p.value

# Current September
test = my.cor.test(rwi$M07Tsu, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Sep[2:48]))
test
plot(rwi$M07Tsu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M07Tsu))
temp_cor[68,3] = test$estimate
temp_cor[68,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M07, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$May[1:47]))
test
plot(lts$M07, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M07))
temp_cor[52,6] = test$estimate
temp_cor[52,7] = test$p.value

# Previous June
test = my.cor.test(lts$M07, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Jun[1:47]))
test
plot(lts$M07, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M07))
temp_cor[53,6] = test$estimate
temp_cor[53,7] = test$p.value

# Previous July
test = my.cor.test(lts$M07, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Jul[1:47]))
test
plot(lts$M07, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M07))
temp_cor[54,6] = test$estimate
temp_cor[54,7] = test$p.value

# Previous August
test = my.cor.test(lts$M07, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Aug[1:47]))
test
plot(lts$M07, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M07))
temp_cor[55,6] = test$estimate
temp_cor[55,7] = test$p.value

# Previous September
test = my.cor.test(lts$M07, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Sep[1:47]))
test
plot(lts$M07, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M07))
temp_cor[56,6] = test$estimate
temp_cor[56,7] = test$p.value

# Previous October
test = my.cor.test(lts$M07, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Oct[1:47]))
test
plot(lts$M07, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M07))
temp_cor[57,6] = test$estimate
temp_cor[57,7] = test$p.value

# Previous November
test = my.cor.test(lts$M07, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Nov[1:47]))
test
plot(lts$M07, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M07))
temp_cor[58,6] = test$estimate
temp_cor[58,7] = test$p.value

# Previous December
test = my.cor.test(lts$M07, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Dec[1:47]))
test
plot(lts$M07, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M07))
temp_cor[59,6] = test$estimate
temp_cor[59,7] = test$p.value

# Current January
test = my.cor.test(lts$M07, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Jan[2:48]))
test
plot(lts$M07, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M07))
temp_cor[60,6] = test$estimate
temp_cor[60,7] = test$p.value

# Current February
test = my.cor.test(lts$M07, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Feb[2:48]))
test
plot(lts$M07, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M07))
temp_cor[61,6] = test$estimate
temp_cor[61,7] = test$p.value

# Current March
test = my.cor.test(lts$M07, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Mar[2:48]))
test
plot(lts$M07, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M07))
temp_cor[62,6] = test$estimate
temp_cor[62,7] = test$p.value

# Current April
test = my.cor.test(lts$M07, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Apr[2:48]))
test
plot(lts$M07, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M07))
temp_cor[63,6] = test$estimate
temp_cor[63,7] = test$p.value

# Current May
test = my.cor.test(lts$M07, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$May[2:48]))
test
plot(lts$M07, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M07))
temp_cor[64,6] = test$estimate
temp_cor[64,7] = test$p.value

# Current June
test = my.cor.test(lts$M07, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Jun[2:48]))
test
plot(lts$M07, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M07))
temp_cor[65,6] = test$estimate
temp_cor[65,7] = test$p.value

# Current July
test = my.cor.test(lts$M07, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Jul[2:48]))
test
plot(lts$M07, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M07))
temp_cor[66,6] = test$estimate
temp_cor[66,7] = test$p.value

# Current August
test = my.cor.test(lts$M07, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Aug[2:48]))
test
plot(lts$M07, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M07))
temp_cor[67,6] = test$estimate
temp_cor[67,7] = test$p.value

# Current September
test = my.cor.test(lts$M07, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, temp_P$Sep[2:48]))
test
plot(lts$M07, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M07))
temp_cor[68,6] = test$estimate
temp_cor[68,7] = test$p.value
#####

##### Temperature: Site 05Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F23Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$May[1:47]))
test
plot(rwi$F23Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F23Ace))
temp_cor[69,3] = test$estimate
temp_cor[69,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F23Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Jun[1:47]))
test
plot(rwi$F23Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F23Ace))
temp_cor[70,3] = test$estimate
temp_cor[70,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F23Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Jul[1:47]))
test
plot(rwi$F23Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F23Ace))
temp_cor[71,3] = test$estimate
temp_cor[71,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F23Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Aug[1:47]))
test
plot(rwi$F23Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F23Ace))
temp_cor[72,3] = test$estimate
temp_cor[72,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F23Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Sep[1:47]))
test
plot(rwi$F23Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F23Ace))
temp_cor[73,3] = test$estimate
temp_cor[73,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F23Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Oct[1:47]))
test
plot(rwi$F23Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F23Ace))
temp_cor[74,3] = test$estimate
temp_cor[74,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F23Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Nov[1:47]))
test
plot(rwi$F23Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F23Ace))
temp_cor[75,3] = test$estimate
temp_cor[75,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F23Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Dec[1:47]))
test
plot(rwi$F23Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F23Ace))
temp_cor[76,3] = test$estimate
temp_cor[76,4] = test$p.value

# Current January
test = my.cor.test(rwi$F23Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Jan[2:48]))
test
plot(rwi$F23Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F23Ace))
temp_cor[77,3] = test$estimate
temp_cor[77,4] = test$p.value

# Current February
test = my.cor.test(rwi$F23Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Feb[2:48]))
test
plot(rwi$F23Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F23Ace))
temp_cor[78,3] = test$estimate
temp_cor[78,4] = test$p.value

# Current March
test = my.cor.test(rwi$F23Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Mar[2:48]))
test
plot(rwi$F23Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F23Ace))
temp_cor[79,3] = test$estimate
temp_cor[79,4] = test$p.value

# Current April
test = my.cor.test(rwi$F23Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Apr[2:48]))
test
plot(rwi$F23Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F23Ace))
temp_cor[80,3] = test$estimate
temp_cor[80,4] = test$p.value

# Current May
test = my.cor.test(rwi$F23Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$May[2:48]))
test
plot(rwi$F23Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F23Ace))
temp_cor[81,3] = test$estimate
temp_cor[81,4] = test$p.value

# Current June
test = my.cor.test(rwi$F23Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Jun[2:48]))
test
plot(rwi$F23Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F23Ace))
temp_cor[82,3] = test$estimate
temp_cor[82,4] = test$p.value

# Current July
test = my.cor.test(rwi$F23Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Jul[2:48]))
test
plot(rwi$F23Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F23Ace))
temp_cor[83,3] = test$estimate
temp_cor[83,4] = test$p.value

# Current August
test = my.cor.test(rwi$F23Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Aug[2:48]))
test
plot(rwi$F23Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F23Ace))
temp_cor[84,3] = test$estimate
temp_cor[84,4] = test$p.value

# Current September
test = my.cor.test(rwi$F23Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Sep[2:48]))
test
plot(rwi$F23Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F23Ace))
temp_cor[85,3] = test$estimate
temp_cor[85,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F23, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$May[1:47]))
test
plot(lts$F23, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F23))
temp_cor[69,6] = test$estimate
temp_cor[69,7] = test$p.value

# Previous June
test = my.cor.test(lts$F23, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Jun[1:47]))
test
plot(lts$F23, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F23))
temp_cor[70,6] = test$estimate
temp_cor[70,7] = test$p.value

# Previous July
test = my.cor.test(lts$F23, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Jul[1:47]))
test
plot(lts$F23, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F23))
temp_cor[71,6] = test$estimate
temp_cor[71,7] = test$p.value

# Previous August
test = my.cor.test(lts$F23, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Aug[1:47]))
test
plot(lts$F23, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F23))
temp_cor[72,6] = test$estimate
temp_cor[72,7] = test$p.value

# Previous September
test = my.cor.test(lts$F23, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Sep[1:47]))
test
plot(lts$F23, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F23))
temp_cor[73,6] = test$estimate
temp_cor[73,7] = test$p.value

# Previous October
test = my.cor.test(lts$F23, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Oct[1:47]))
test
plot(lts$F23, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F23))
temp_cor[74,6] = test$estimate
temp_cor[74,7] = test$p.value

# Previous November
test = my.cor.test(lts$F23, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Nov[1:47]))
test
plot(lts$F23, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F23))
temp_cor[75,6] = test$estimate
temp_cor[75,7] = test$p.value

# Previous December
test = my.cor.test(lts$F23, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Dec[1:47]))
test
plot(lts$F23, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F23))
temp_cor[76,6] = test$estimate
temp_cor[76,7] = test$p.value

# Current January
test = my.cor.test(lts$F23, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Jan[2:48]))
test
plot(lts$F23, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F23))
temp_cor[77,6] = test$estimate
temp_cor[77,7] = test$p.value

# Current February
test = my.cor.test(lts$F23, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Feb[2:48]))
test
plot(lts$F23, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F23))
temp_cor[78,6] = test$estimate
temp_cor[78,7] = test$p.value

# Current March
test = my.cor.test(lts$F23, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Mar[2:48]))
test
plot(lts$F23, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F23))
temp_cor[79,6] = test$estimate
temp_cor[79,7] = test$p.value

# Current April
test = my.cor.test(lts$F23, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Apr[2:48]))
test
plot(lts$F23, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F23))
temp_cor[80,6] = test$estimate
temp_cor[80,7] = test$p.value

# Current May
test = my.cor.test(lts$F23, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$May[2:48]))
test
plot(lts$F23, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F23))
temp_cor[81,6] = test$estimate
temp_cor[81,7] = test$p.value

# Current June
test = my.cor.test(lts$F23, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Jun[2:48]))
test
plot(lts$F23, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F23))
temp_cor[82,6] = test$estimate
temp_cor[82,7] = test$p.value

# Current July
test = my.cor.test(lts$F23, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Jul[2:48]))
test
plot(lts$F23, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F23))
temp_cor[83,6] = test$estimate
temp_cor[83,7] = test$p.value

# Current August
test = my.cor.test(lts$F23, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Aug[2:48]))
test
plot(lts$F23, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F23))
temp_cor[84,6] = test$estimate
temp_cor[84,7] = test$p.value

# Current September
test = my.cor.test(lts$F23, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, temp_P$Sep[2:48]))
test
plot(lts$F23, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F23))
temp_cor[85,6] = test$estimate
temp_cor[85,7] = test$p.value
#####

##### Temperature: Site 06Dec #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F21Dec, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$May[1:47]))
test
plot(rwi$F21Dec, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F21Dec))
temp_cor[86,3] = test$estimate
temp_cor[86,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F21Dec, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Jun[1:47]))
test
plot(rwi$F21Dec, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F21Dec))
temp_cor[87,3] = test$estimate
temp_cor[87,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F21Dec, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Jul[1:47]))
test
plot(rwi$F21Dec, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F21Dec))
temp_cor[88,3] = test$estimate
temp_cor[88,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F21Dec, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Aug[1:47]))
test
plot(rwi$F21Dec, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F21Dec))
temp_cor[89,3] = test$estimate
temp_cor[89,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F21Dec, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Sep[1:47]))
test
plot(rwi$F21Dec, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F21Dec))
temp_cor[90,3] = test$estimate
temp_cor[90,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F21Dec, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Oct[1:47]))
test
plot(rwi$F21Dec, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F21Dec))
temp_cor[91,3] = test$estimate
temp_cor[91,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F21Dec, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Nov[1:47]))
test
plot(rwi$F21Dec, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F21Dec))
temp_cor[92,3] = test$estimate
temp_cor[92,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F21Dec, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Dec[1:47]))
test
plot(rwi$F21Dec, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F21Dec))
temp_cor[93,3] = test$estimate
temp_cor[93,4] = test$p.value

# Current January
test = my.cor.test(rwi$F21Dec, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Jan[2:48]))
test
plot(rwi$F21Dec, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F21Dec))
temp_cor[94,3] = test$estimate
temp_cor[94,4] = test$p.value

# Current February
test = my.cor.test(rwi$F21Dec, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Feb[2:48]))
test
plot(rwi$F21Dec, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F21Dec))
temp_cor[95,3] = test$estimate
temp_cor[95,4] = test$p.value

# Current March
test = my.cor.test(rwi$F21Dec, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Mar[2:48]))
test
plot(rwi$F21Dec, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F21Dec))
temp_cor[96,3] = test$estimate
temp_cor[96,4] = test$p.value

# Current April
test = my.cor.test(rwi$F21Dec, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Apr[2:48]))
test
plot(rwi$F21Dec, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F21Dec))
temp_cor[97,3] = test$estimate
temp_cor[97,4] = test$p.value

# Current May
test = my.cor.test(rwi$F21Dec, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$May[2:48]))
test
plot(rwi$F21Dec, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F21Dec))
temp_cor[98,3] = test$estimate
temp_cor[98,4] = test$p.value

# Current June
test = my.cor.test(rwi$F21Dec, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Jun[2:48]))
test
plot(rwi$F21Dec, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F21Dec))
temp_cor[99,3] = test$estimate
temp_cor[99,4] = test$p.value

# Current July
test = my.cor.test(rwi$F21Dec, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Jul[2:48]))
test
plot(rwi$F21Dec, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F21Dec))
temp_cor[100,3] = test$estimate
temp_cor[100,4] = test$p.value

# Current August
test = my.cor.test(rwi$F21Dec, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Aug[2:48]))
test
plot(rwi$F21Dec, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F21Dec))
temp_cor[101,3] = test$estimate
temp_cor[101,4] = test$p.value

# Current September
test = my.cor.test(rwi$F21Dec, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Sep[2:48]))
test
plot(rwi$F21Dec, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F21Dec))
temp_cor[102,3] = test$estimate
temp_cor[102,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F21, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$May[1:47]))
test
plot(lts$F21, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F21))
temp_cor[86,6] = test$estimate
temp_cor[86,7] = test$p.value

# Previous June
test = my.cor.test(lts$F21, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Jun[1:47]))
test
plot(lts$F21, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F21))
temp_cor[87,6] = test$estimate
temp_cor[87,7] = test$p.value

# Previous July
test = my.cor.test(lts$F21, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Jul[1:47]))
test
plot(lts$F21, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F21))
temp_cor[88,6] = test$estimate
temp_cor[88,7] = test$p.value

# Previous August
test = my.cor.test(lts$F21, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Aug[1:47]))
test
plot(lts$F21, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F21))
temp_cor[89,6] = test$estimate
temp_cor[89,7] = test$p.value

# Previous September
test = my.cor.test(lts$F21, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Sep[1:47]))
test
plot(lts$F21, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F21))
temp_cor[90,6] = test$estimate
temp_cor[90,7] = test$p.value

# Previous October
test = my.cor.test(lts$F21, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Oct[1:47]))
test
plot(lts$F21, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F21))
temp_cor[91,6] = test$estimate
temp_cor[91,7] = test$p.value

# Previous November
test = my.cor.test(lts$F21, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Nov[1:47]))
test
plot(lts$F21, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F21))
temp_cor[92,6] = test$estimate
temp_cor[92,7] = test$p.value

# Previous December
test = my.cor.test(lts$F21, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Dec[1:47]))
test
plot(lts$F21, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F21))
temp_cor[93,6] = test$estimate
temp_cor[93,7] = test$p.value

# Current January
test = my.cor.test(lts$F21, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Jan[2:48]))
test
plot(lts$F21, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F21))
temp_cor[94,6] = test$estimate
temp_cor[94,7] = test$p.value

# Current February
test = my.cor.test(lts$F21, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Feb[2:48]))
test
plot(lts$F21, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F21))
temp_cor[95,6] = test$estimate
temp_cor[95,7] = test$p.value

# Current March
test = my.cor.test(lts$F21, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Mar[2:48]))
test
plot(lts$F21, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F21))
temp_cor[96,6] = test$estimate
temp_cor[96,7] = test$p.value

# Current April
test = my.cor.test(lts$F21, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Apr[2:48]))
test
plot(lts$F21, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F21))
temp_cor[97,6] = test$estimate
temp_cor[97,7] = test$p.value

# Current May
test = my.cor.test(lts$F21, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$May[2:48]))
test
plot(lts$F21, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F21))
temp_cor[98,6] = test$estimate
temp_cor[98,7] = test$p.value

# Current June
test = my.cor.test(lts$F21, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Jun[2:48]))
test
plot(lts$F21, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F21))
temp_cor[99,6] = test$estimate
temp_cor[99,7] = test$p.value

# Current July
test = my.cor.test(lts$F21, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Jul[2:48]))
test
plot(lts$F21, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F21))
temp_cor[100,6] = test$estimate
temp_cor[100,7] = test$p.value

# Current August
test = my.cor.test(lts$F21, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Aug[2:48]))
test
plot(lts$F21, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F21))
temp_cor[101,6] = test$estimate
temp_cor[101,7] = test$p.value

# Current September
test = my.cor.test(lts$F21, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, temp_P$Sep[2:48]))
test
plot(lts$F21, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F21))
temp_cor[102,6] = test$estimate
temp_cor[102,7] = test$p.value
#####

##### Temperature: Site 07Thu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M05Thu, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$May[1:47]))
test
plot(rwi$M05Thu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M05Thu))
temp_cor[103,3] = test$estimate
temp_cor[103,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M05Thu, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Jun[1:47]))
test
plot(rwi$M05Thu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M05Thu))
temp_cor[104,3] = test$estimate
temp_cor[104,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M05Thu, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Jul[1:47]))
test
plot(rwi$M05Thu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M05Thu))
temp_cor[105,3] = test$estimate
temp_cor[105,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M05Thu, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Aug[1:47]))
test
plot(rwi$M05Thu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M05Thu))
temp_cor[106,3] = test$estimate
temp_cor[106,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M05Thu, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Sep[1:47]))
test
plot(rwi$M05Thu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M05Thu))
temp_cor[107,3] = test$estimate
temp_cor[107,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M05Thu, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Oct[1:47]))
test
plot(rwi$M05Thu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M05Thu))
temp_cor[108,3] = test$estimate
temp_cor[108,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M05Thu, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Nov[1:47]))
test
plot(rwi$M05Thu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M05Thu))
temp_cor[109,3] = test$estimate
temp_cor[109,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M05Thu, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Dec[1:47]))
test
plot(rwi$M05Thu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M05Thu))
temp_cor[110,3] = test$estimate
temp_cor[110,4] = test$p.value

# Current January
test = my.cor.test(rwi$M05Thu, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Jan[2:48]))
test
plot(rwi$M05Thu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M05Thu))
temp_cor[111,3] = test$estimate
temp_cor[111,4] = test$p.value

# Current February
test = my.cor.test(rwi$M05Thu, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Feb[2:48]))
test
plot(rwi$M05Thu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M05Thu))
temp_cor[112,3] = test$estimate
temp_cor[112,4] = test$p.value

# Current March
test = my.cor.test(rwi$M05Thu, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Mar[2:48]))
test
plot(rwi$M05Thu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M05Thu))
temp_cor[113,3] = test$estimate
temp_cor[113,4] = test$p.value

# Current April
test = my.cor.test(rwi$M05Thu, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Apr[2:48]))
test
plot(rwi$M05Thu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M05Thu))
temp_cor[114,3] = test$estimate
temp_cor[114,4] = test$p.value

# Current May
test = my.cor.test(rwi$M05Thu, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$May[2:48]))
test
plot(rwi$M05Thu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M05Thu))
temp_cor[115,3] = test$estimate
temp_cor[115,4] = test$p.value

# Current June
test = my.cor.test(rwi$M05Thu, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Jun[2:48]))
test
plot(rwi$M05Thu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M05Thu))
temp_cor[116,3] = test$estimate
temp_cor[116,4] = test$p.value

# Current July
test = my.cor.test(rwi$M05Thu, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Jul[2:48]))
test
plot(rwi$M05Thu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M05Thu))
temp_cor[117,3] = test$estimate
temp_cor[117,4] = test$p.value

# Current August
test = my.cor.test(rwi$M05Thu, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Aug[2:48]))
test
plot(rwi$M05Thu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M05Thu))
temp_cor[118,3] = test$estimate
temp_cor[118,4] = test$p.value

# Current September
test = my.cor.test(rwi$M05Thu, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Sep[2:48]))
test
plot(rwi$M05Thu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M05Thu))
temp_cor[119,3] = test$estimate
temp_cor[119,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M05, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$May[1:47]))
test
plot(lts$M05, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M05))
temp_cor[103,6] = test$estimate
temp_cor[103,7] = test$p.value

# Previous June
test = my.cor.test(lts$M05, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Jun[1:47]))
test
plot(lts$M05, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M05))
temp_cor[104,6] = test$estimate
temp_cor[104,7] = test$p.value

# Previous July
test = my.cor.test(lts$M05, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Jul[1:47]))
test
plot(lts$M05, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M05))
temp_cor[105,6] = test$estimate
temp_cor[105,7] = test$p.value

# Previous August
test = my.cor.test(lts$M05, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Aug[1:47]))
test
plot(lts$M05, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M05))
temp_cor[106,6] = test$estimate
temp_cor[106,7] = test$p.value

# Previous September
test = my.cor.test(lts$M05, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Sep[1:47]))
test
plot(lts$M05, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M05))
temp_cor[107,6] = test$estimate
temp_cor[107,7] = test$p.value

# Previous October
test = my.cor.test(lts$M05, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Oct[1:47]))
test
plot(lts$M05, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M05))
temp_cor[108,6] = test$estimate
temp_cor[108,7] = test$p.value

# Previous November
test = my.cor.test(lts$M05, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Nov[1:47]))
test
plot(lts$M05, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M05))
temp_cor[109,6] = test$estimate
temp_cor[109,7] = test$p.value

# Previous December
test = my.cor.test(lts$M05, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Dec[1:47]))
test
plot(lts$M05, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M05))
temp_cor[110,6] = test$estimate
temp_cor[110,7] = test$p.value

# Current January
test = my.cor.test(lts$M05, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Jan[2:48]))
test
plot(lts$M05, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M05))
temp_cor[111,6] = test$estimate
temp_cor[111,7] = test$p.value

# Current February
test = my.cor.test(lts$M05, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Feb[2:48]))
test
plot(lts$M05, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M05))
temp_cor[112,6] = test$estimate
temp_cor[112,7] = test$p.value

# Current March
test = my.cor.test(lts$M05, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Mar[2:48]))
test
plot(lts$M05, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M05))
temp_cor[113,6] = test$estimate
temp_cor[113,7] = test$p.value

# Current April
test = my.cor.test(lts$M05, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Apr[2:48]))
test
plot(lts$M05, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M05))
temp_cor[114,6] = test$estimate
temp_cor[114,7] = test$p.value

# Current May
test = my.cor.test(lts$M05, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$May[2:48]))
test
plot(lts$M05, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M05))
temp_cor[115,6] = test$estimate
temp_cor[115,7] = test$p.value

# Current June
test = my.cor.test(lts$M05, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Jun[2:48]))
test
plot(lts$M05, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M05))
temp_cor[116,6] = test$estimate
temp_cor[116,7] = test$p.value

# Current July
test = my.cor.test(lts$M05, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Jul[2:48]))
test
plot(lts$M05, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M05))
temp_cor[117,6] = test$estimate
temp_cor[117,7] = test$p.value

# Current August
test = my.cor.test(lts$M05, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Aug[2:48]))
test
plot(lts$M05, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M05))
temp_cor[118,6] = test$estimate
temp_cor[118,7] = test$p.value

# Current September
test = my.cor.test(lts$M05, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, temp_P$Sep[2:48]))
test
plot(lts$M05, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M05))
temp_cor[119,6] = test$estimate
temp_cor[119,7] = test$p.value
#####

##### Temperature: Site 08Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F07Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$May[1:47]))
test
plot(rwi$F07Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F07Ace))
temp_cor[120,3] = test$estimate
temp_cor[120,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F07Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Jun[1:47]))
test
plot(rwi$F07Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F07Ace))
temp_cor[121,3] = test$estimate
temp_cor[121,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F07Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Jul[1:47]))
test
plot(rwi$F07Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F07Ace))
temp_cor[122,3] = test$estimate
temp_cor[122,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F07Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Aug[1:47]))
test
plot(rwi$F07Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F07Ace))
temp_cor[123,3] = test$estimate
temp_cor[123,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F07Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Sep[1:47]))
test
plot(rwi$F07Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F07Ace))
temp_cor[124,3] = test$estimate
temp_cor[124,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F07Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Oct[1:47]))
test
plot(rwi$F07Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F07Ace))
temp_cor[125,3] = test$estimate
temp_cor[125,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F07Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Nov[1:47]))
test
plot(rwi$F07Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F07Ace))
temp_cor[126,3] = test$estimate
temp_cor[126,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F07Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Dec[1:47]))
test
plot(rwi$F07Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F07Ace))
temp_cor[127,3] = test$estimate
temp_cor[127,4] = test$p.value

# Current January
test = my.cor.test(rwi$F07Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Jan[2:48]))
test
plot(rwi$F07Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F07Ace))
temp_cor[128,3] = test$estimate
temp_cor[128,4] = test$p.value

# Current February
test = my.cor.test(rwi$F07Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Feb[2:48]))
test
plot(rwi$F07Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F07Ace))
temp_cor[129,3] = test$estimate
temp_cor[129,4] = test$p.value

# Current March
test = my.cor.test(rwi$F07Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Mar[2:48]))
test
plot(rwi$F07Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F07Ace))
temp_cor[130,3] = test$estimate
temp_cor[130,4] = test$p.value

# Current April
test = my.cor.test(rwi$F07Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Apr[2:48]))
test
plot(rwi$F07Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F07Ace))
temp_cor[131,3] = test$estimate
temp_cor[131,4] = test$p.value

# Current May
test = my.cor.test(rwi$F07Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$May[2:48]))
test
plot(rwi$F07Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F07Ace))
temp_cor[132,3] = test$estimate
temp_cor[132,4] = test$p.value

# Current June
test = my.cor.test(rwi$F07Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Jun[2:48]))
test
plot(rwi$F07Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F07Ace))
temp_cor[133,3] = test$estimate
temp_cor[133,4] = test$p.value

# Current July
test = my.cor.test(rwi$F07Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Jul[2:48]))
test
plot(rwi$F07Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F07Ace))
temp_cor[134,3] = test$estimate
temp_cor[134,4] = test$p.value

# Current August
test = my.cor.test(rwi$F07Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Aug[2:48]))
test
plot(rwi$F07Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F07Ace))
temp_cor[135,3] = test$estimate
temp_cor[135,4] = test$p.value

# Current September
test = my.cor.test(rwi$F07Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Sep[2:48]))
test
plot(rwi$F07Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F07Ace))
temp_cor[136,3] = test$estimate
temp_cor[136,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F07, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$May[1:47]))
test
plot(lts$F07, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F07))
temp_cor[120,6] = test$estimate
temp_cor[120,7] = test$p.value

# Previous June
test = my.cor.test(lts$F07, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Jun[1:47]))
test
plot(lts$F07, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F07))
temp_cor[121,6] = test$estimate
temp_cor[121,7] = test$p.value

# Previous July
test = my.cor.test(lts$F07, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Jul[1:47]))
test
plot(lts$F07, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F07))
temp_cor[122,6] = test$estimate
temp_cor[122,7] = test$p.value

# Previous August
test = my.cor.test(lts$F07, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Aug[1:47]))
test
plot(lts$F07, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F07))
temp_cor[123,6] = test$estimate
temp_cor[123,7] = test$p.value

# Previous September
test = my.cor.test(lts$F07, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Sep[1:47]))
test
plot(lts$F07, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F07))
temp_cor[124,6] = test$estimate
temp_cor[124,7] = test$p.value

# Previous October
test = my.cor.test(lts$F07, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Oct[1:47]))
test
plot(lts$F07, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F07))
temp_cor[125,6] = test$estimate
temp_cor[125,7] = test$p.value

# Previous November
test = my.cor.test(lts$F07, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Nov[1:47]))
test
plot(lts$F07, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F07))
temp_cor[126,6] = test$estimate
temp_cor[126,7] = test$p.value

# Previous December
test = my.cor.test(lts$F07, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Dec[1:47]))
test
plot(lts$F07, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F07))
temp_cor[127,6] = test$estimate
temp_cor[127,7] = test$p.value

# Current January
test = my.cor.test(lts$F07, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Jan[2:48]))
test
plot(lts$F07, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F07))
temp_cor[128,6] = test$estimate
temp_cor[128,7] = test$p.value

# Current February
test = my.cor.test(lts$F07, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Feb[2:48]))
test
plot(lts$F07, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F07))
temp_cor[129,6] = test$estimate
temp_cor[129,7] = test$p.value

# Current March
test = my.cor.test(lts$F07, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Mar[2:48]))
test
plot(lts$F07, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F07))
temp_cor[130,6] = test$estimate
temp_cor[130,7] = test$p.value

# Current April
test = my.cor.test(lts$F07, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Apr[2:48]))
test
plot(lts$F07, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F07))
temp_cor[131,6] = test$estimate
temp_cor[131,7] = test$p.value

# Current May
test = my.cor.test(lts$F07, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$May[2:48]))
test
plot(lts$F07, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F07))
temp_cor[132,6] = test$estimate
temp_cor[132,7] = test$p.value

# Current June
test = my.cor.test(lts$F07, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Jun[2:48]))
test
plot(lts$F07, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F07))
temp_cor[133,6] = test$estimate
temp_cor[133,7] = test$p.value

# Current July
test = my.cor.test(lts$F07, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Jul[2:48]))
test
plot(lts$F07, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F07))
temp_cor[134,6] = test$estimate
temp_cor[134,7] = test$p.value

# Current August
test = my.cor.test(lts$F07, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Aug[2:48]))
test
plot(lts$F07, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F07))
temp_cor[135,6] = test$estimate
temp_cor[135,7] = test$p.value

# Current September
test = my.cor.test(lts$F07, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, temp_P$Sep[2:48]))
test
plot(lts$F07, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F07))
temp_cor[136,6] = test$estimate
temp_cor[136,7] = test$p.value
#####

##### Temperature: Site 09Pin #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M27Pin[10:47], temp_P$May[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$May[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$May[10:47])
abline(lm(temp_P$May[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[137,3] = test$estimate
temp_cor[137,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Jun[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Jun[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Jun[10:47])
abline(lm(temp_P$Jun[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[138,3] = test$estimate
temp_cor[138,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Jul[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Jul[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Jul[10:47])
abline(lm(temp_P$Jul[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[139,3] = test$estimate
temp_cor[139,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Aug[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Aug[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Aug[10:47])
abline(lm(temp_P$Aug[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[140,3] = test$estimate
temp_cor[140,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Sep[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Sep[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Sep[10:47])
abline(lm(temp_P$Sep[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[141,3] = test$estimate
temp_cor[141,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Oct[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Oct[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Oct[10:47])
abline(lm(temp_P$Oct[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[142,3] = test$estimate
temp_cor[142,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Nov[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Nov[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Nov[10:47])
abline(lm(temp_P$Nov[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[143,3] = test$estimate
temp_cor[143,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Dec[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Dec[10:47]))
test
plot(rwi$M27Pin[10:47], temp_P$Dec[10:47])
abline(lm(temp_P$Dec[10:47] ~ rwi$M27Pin[10:47]))
temp_cor[144,3] = test$estimate
temp_cor[144,4] = test$p.value

# Current January
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Jan[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Jan[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Jan[11:48])
abline(lm(temp_P$Jan[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[145,3] = test$estimate
temp_cor[145,4] = test$p.value

# Current February
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Feb[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Feb[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Feb[11:48])
abline(lm(temp_P$Feb[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[146,3] = test$estimate
temp_cor[146,4] = test$p.value

# Current March
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Mar[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Mar[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Mar[11:48])
abline(lm(temp_P$Mar[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[147,3] = test$estimate
temp_cor[147,4] = test$p.value

# Current April
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Apr[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Apr[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Apr[11:48])
abline(lm(temp_P$Apr[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[148,3] = test$estimate
temp_cor[148,4] = test$p.value

# Current May
test = my.cor.test(rwi$M27Pin[10:47], temp_P$May[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$May[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$May[11:48])
abline(lm(temp_P$May[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[149,3] = test$estimate
temp_cor[149,4] = test$p.value

# Current June
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Jun[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Jun[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Jun[11:48])
abline(lm(temp_P$Jun[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[150,3] = test$estimate
temp_cor[150,4] = test$p.value

# Current July
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Jul[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Jul[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Jul[11:48])
abline(lm(temp_P$Jul[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[151,3] = test$estimate
temp_cor[151,4] = test$p.value

# Current August
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Aug[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Aug[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Aug[11:48])
abline(lm(temp_P$Aug[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[152,3] = test$estimate
temp_cor[152,4] = test$p.value

# Current September
test = my.cor.test(rwi$M27Pin[10:47], temp_P$Sep[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Sep[11:48]))
test
plot(rwi$M27Pin[10:47], temp_P$Sep[11:48])
abline(lm(temp_P$Sep[11:48] ~ rwi$M27Pin[10:47]))
temp_cor[153,3] = test$estimate
temp_cor[153,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M27[10:47], temp_P$May[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$May[10:47]))
test
plot(lts$M27[10:47], temp_P$May[10:47])
abline(lm(temp_P$May[10:47] ~ lts$M27[10:47]))
temp_cor[137,6] = test$estimate
temp_cor[137,7] = test$p.value

# Previous June
test = my.cor.test(lts$M27[10:47], temp_P$Jun[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Jun[10:47]))
test
plot(lts$M27[10:47], temp_P$Jun[10:47])
abline(lm(temp_P$Jun[10:47] ~ lts$M27[10:47]))
temp_cor[138,6] = test$estimate
temp_cor[138,7] = test$p.value

# Previous July
test = my.cor.test(lts$M27[10:47], temp_P$Jul[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Jul[10:47]))
test
plot(lts$M27[10:47], temp_P$Jul[10:47])
abline(lm(temp_P$Jul[10:47] ~ lts$M27[10:47]))
temp_cor[139,6] = test$estimate
temp_cor[139,7] = test$p.value

# Previous August
test = my.cor.test(lts$M27[10:47], temp_P$Aug[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Aug[10:47]))
test
plot(lts$M27[10:47], temp_P$Aug[10:47])
abline(lm(temp_P$Aug[10:47] ~ lts$M27[10:47]))
temp_cor[140,6] = test$estimate
temp_cor[140,7] = test$p.value

# Previous September
test = my.cor.test(lts$M27[10:47], temp_P$Sep[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Sep[10:47]))
test
plot(lts$M27[10:47], temp_P$Sep[10:47])
abline(lm(temp_P$Sep[10:47] ~ lts$M27[10:47]))
temp_cor[141,6] = test$estimate
temp_cor[141,7] = test$p.value

# Previous October
test = my.cor.test(lts$M27[10:47], temp_P$Oct[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Oct[10:47]))
test
plot(lts$M27[10:47], temp_P$Oct[10:47])
abline(lm(temp_P$Oct[10:47] ~ lts$M27[10:47]))
temp_cor[142,6] = test$estimate
temp_cor[142,7] = test$p.value

# Previous November
test = my.cor.test(lts$M27[10:47], temp_P$Nov[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Nov[10:47]))
test
plot(lts$M27[10:47], temp_P$Nov[10:47])
abline(lm(temp_P$Nov[10:47] ~ lts$M27[10:47]))
temp_cor[143,6] = test$estimate
temp_cor[143,7] = test$p.value

# Previous December
test = my.cor.test(lts$M27[10:47], temp_P$Dec[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Dec[10:47]))
test
plot(lts$M27[10:47], temp_P$Dec[10:47])
abline(lm(temp_P$Dec[10:47] ~ lts$M27[10:47]))
temp_cor[144,6] = test$estimate
temp_cor[144,7] = test$p.value

# Current January
test = my.cor.test(lts$M27[10:47], temp_P$Jan[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Jan[11:48]))
test
plot(lts$M27[10:47], temp_P$Jan[11:48])
abline(lm(temp_P$Jan[11:48] ~ lts$M27[10:47]))
temp_cor[145,6] = test$estimate
temp_cor[145,7] = test$p.value

# Current February
test = my.cor.test(lts$M27[10:47], temp_P$Feb[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Feb[11:48]))
test
plot(lts$M27[10:47], temp_P$Feb[11:48])
abline(lm(temp_P$Feb[11:48] ~ lts$M27[10:47]))
temp_cor[146,6] = test$estimate
temp_cor[146,7] = test$p.value

# Current March
test = my.cor.test(lts$M27[10:47], temp_P$Mar[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Mar[11:48]))
test
plot(lts$M27[10:47], temp_P$Mar[11:48])
abline(lm(temp_P$Mar[11:48] ~ lts$M27[10:47]))
temp_cor[147,6] = test$estimate
temp_cor[147,7] = test$p.value

# Current April
test = my.cor.test(lts$M27[10:47], temp_P$Apr[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Apr[11:48]))
test
plot(lts$M27[10:47], temp_P$Apr[11:48])
abline(lm(temp_P$Apr[11:48] ~ lts$M27[10:47]))
temp_cor[148,6] = test$estimate
temp_cor[148,7] = test$p.value

# Current May
test = my.cor.test(lts$M27[10:47], temp_P$May[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$May[11:48]))
test
plot(lts$M27[10:47], temp_P$May[11:48])
abline(lm(temp_P$May[11:48] ~ lts$M27[10:47]))
temp_cor[149,6] = test$estimate
temp_cor[149,7] = test$p.value

# Current June
test = my.cor.test(lts$M27[10:47], temp_P$Jun[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Jun[11:48]))
test
plot(lts$M27[10:47], temp_P$Jun[11:48])
abline(lm(temp_P$Jun[11:48] ~ lts$M27[10:47]))
temp_cor[150,6] = test$estimate
temp_cor[150,7] = test$p.value

# Current July
test = my.cor.test(lts$M27[10:47], temp_P$Jul[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Jul[11:48]))
test
plot(lts$M27[10:47], temp_P$Jul[11:48])
abline(lm(temp_P$Jul[11:48] ~ lts$M27[10:47]))
temp_cor[151,6] = test$estimate
temp_cor[151,7] = test$p.value

# Current August
test = my.cor.test(lts$M27[10:47], temp_P$Aug[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Aug[11:48]))
test
plot(lts$M27[10:47], temp_P$Aug[11:48])
abline(lm(temp_P$Aug[11:48] ~ lts$M27[10:47]))
temp_cor[152,6] = test$estimate
temp_cor[152,7] = test$p.value

# Current September
test = my.cor.test(lts$M27[10:47], temp_P$Sep[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Sep[11:48]))
test
plot(lts$M27[10:47], temp_P$Sep[11:48])
abline(lm(temp_P$Sep[11:48] ~ lts$M27[10:47]))
temp_cor[153,6] = test$estimate
temp_cor[153,7] = test$p.value
#####

##### Temperature: Site 10Thu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M26Thu, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$May[1:47]))
test
plot(rwi$M26Thu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M26Thu))
temp_cor[154,3] = test$estimate
temp_cor[154,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M26Thu, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Jun[1:47]))
test
plot(rwi$M26Thu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M26Thu))
temp_cor[155,3] = test$estimate
temp_cor[155,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M26Thu, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Jul[1:47]))
test
plot(rwi$M26Thu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M26Thu))
temp_cor[156,3] = test$estimate
temp_cor[156,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M26Thu, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Aug[1:47]))
test
plot(rwi$M26Thu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M26Thu))
temp_cor[157,3] = test$estimate
temp_cor[157,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M26Thu, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Sep[1:47]))
test
plot(rwi$M26Thu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M26Thu))
temp_cor[158,3] = test$estimate
temp_cor[158,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M26Thu, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Oct[1:47]))
test
plot(rwi$M26Thu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M26Thu))
temp_cor[159,3] = test$estimate
temp_cor[159,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M26Thu, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Nov[1:47]))
test
plot(rwi$M26Thu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M26Thu))
temp_cor[160,3] = test$estimate
temp_cor[160,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M26Thu, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Dec[1:47]))
test
plot(rwi$M26Thu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M26Thu))
temp_cor[161,3] = test$estimate
temp_cor[161,4] = test$p.value

# Current January
test = my.cor.test(rwi$M26Thu, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Jan[2:48]))
test
plot(rwi$M26Thu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M26Thu))
temp_cor[162,3] = test$estimate
temp_cor[162,4] = test$p.value

# Current February
test = my.cor.test(rwi$M26Thu, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Feb[2:48]))
test
plot(rwi$M26Thu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M26Thu))
temp_cor[163,3] = test$estimate
temp_cor[163,4] = test$p.value

# Current March
test = my.cor.test(rwi$M26Thu, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Mar[2:48]))
test
plot(rwi$M26Thu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M26Thu))
temp_cor[164,3] = test$estimate
temp_cor[164,4] = test$p.value

# Current April
test = my.cor.test(rwi$M26Thu, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Apr[2:48]))
test
plot(rwi$M26Thu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M26Thu))
temp_cor[165,3] = test$estimate
temp_cor[165,4] = test$p.value

# Current May
test = my.cor.test(rwi$M26Thu, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$May[2:48]))
test
plot(rwi$M26Thu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M26Thu))
temp_cor[166,3] = test$estimate
temp_cor[166,4] = test$p.value

# Current June
test = my.cor.test(rwi$M26Thu, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Jun[2:48]))
test
plot(rwi$M26Thu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M26Thu))
temp_cor[167,3] = test$estimate
temp_cor[167,4] = test$p.value

# Current July
test = my.cor.test(rwi$M26Thu, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Jul[2:48]))
test
plot(rwi$M26Thu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M26Thu))
temp_cor[168,3] = test$estimate
temp_cor[168,4] = test$p.value

# Current August
test = my.cor.test(rwi$M26Thu, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Aug[2:48]))
test
plot(rwi$M26Thu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M26Thu))
temp_cor[169,3] = test$estimate
temp_cor[169,4] = test$p.value

# Current September
test = my.cor.test(rwi$M26Thu, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Sep[2:48]))
test
plot(rwi$M26Thu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M26Thu))
temp_cor[170,3] = test$estimate
temp_cor[170,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M26, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$May[1:47]))
test
plot(lts$M26, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M26))
temp_cor[154,6] = test$estimate
temp_cor[154,7] = test$p.value

# Previous June
test = my.cor.test(lts$M26, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Jun[1:47]))
test
plot(lts$M26, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M26))
temp_cor[155,6] = test$estimate
temp_cor[155,7] = test$p.value

# Previous July
test = my.cor.test(lts$M26, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Jul[1:47]))
test
plot(lts$M26, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M26))
temp_cor[156,6] = test$estimate
temp_cor[156,7] = test$p.value

# Previous August
test = my.cor.test(lts$M26, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Aug[1:47]))
test
plot(lts$M26, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M26))
temp_cor[157,6] = test$estimate
temp_cor[157,7] = test$p.value

# Previous September
test = my.cor.test(lts$M26, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Sep[1:47]))
test
plot(lts$M26, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M26))
temp_cor[158,6] = test$estimate
temp_cor[158,7] = test$p.value

# Previous October
test = my.cor.test(lts$M26, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Oct[1:47]))
test
plot(lts$M26, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M26))
temp_cor[159,6] = test$estimate
temp_cor[159,7] = test$p.value

# Previous November
test = my.cor.test(lts$M26, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Nov[1:47]))
test
plot(lts$M26, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M26))
temp_cor[160,6] = test$estimate
temp_cor[160,7] = test$p.value

# Previous December
test = my.cor.test(lts$M26, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Dec[1:47]))
test
plot(lts$M26, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M26))
temp_cor[161,6] = test$estimate
temp_cor[161,7] = test$p.value

# Current January
test = my.cor.test(lts$M26, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Jan[2:48]))
test
plot(lts$M26, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M26))
temp_cor[162,6] = test$estimate
temp_cor[162,7] = test$p.value

# Current February
test = my.cor.test(lts$M26, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Feb[2:48]))
test
plot(lts$M26, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M26))
temp_cor[163,6] = test$estimate
temp_cor[163,7] = test$p.value

# Current March
test = my.cor.test(lts$M26, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Mar[2:48]))
test
plot(lts$M26, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M26))
temp_cor[164,6] = test$estimate
temp_cor[164,7] = test$p.value

# Current April
test = my.cor.test(lts$M26, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Apr[2:48]))
test
plot(lts$M26, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M26))
temp_cor[165,6] = test$estimate
temp_cor[165,7] = test$p.value

# Current May
test = my.cor.test(lts$M26, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$May[2:48]))
test
plot(lts$M26, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M26))
temp_cor[166,6] = test$estimate
temp_cor[166,7] = test$p.value

# Current June
test = my.cor.test(lts$M26, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Jun[2:48]))
test
plot(lts$M26, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M26))
temp_cor[167,6] = test$estimate
temp_cor[167,7] = test$p.value

# Current July
test = my.cor.test(lts$M26, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Jul[2:48]))
test
plot(lts$M26, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M26))
temp_cor[168,6] = test$estimate
temp_cor[168,7] = test$p.value

# Current August
test = my.cor.test(lts$M26, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Aug[2:48]))
test
plot(lts$M26, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M26))
temp_cor[169,6] = test$estimate
temp_cor[169,7] = test$p.value

# Current September
test = my.cor.test(lts$M26, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, temp_P$Sep[2:48]))
test
plot(lts$M26, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M26))
temp_cor[170,6] = test$estimate
temp_cor[170,7] = test$p.value
#####

##### Temperature: Site 11Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F25Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$May[1:47]))
test
plot(rwi$F25Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F25Ace))
temp_cor[171,3] = test$estimate
temp_cor[171,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F25Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Jun[1:47]))
test
plot(rwi$F25Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F25Ace))
temp_cor[172,3] = test$estimate
temp_cor[172,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F25Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Jul[1:47]))
test
plot(rwi$F25Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F25Ace))
temp_cor[173,3] = test$estimate
temp_cor[173,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F25Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Aug[1:47]))
test
plot(rwi$F25Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F25Ace))
temp_cor[174,3] = test$estimate
temp_cor[174,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F25Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Sep[1:47]))
test
plot(rwi$F25Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F25Ace))
temp_cor[175,3] = test$estimate
temp_cor[175,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F25Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Oct[1:47]))
test
plot(rwi$F25Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F25Ace))
temp_cor[176,3] = test$estimate
temp_cor[176,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F25Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Nov[1:47]))
test
plot(rwi$F25Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F25Ace))
temp_cor[177,3] = test$estimate
temp_cor[177,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F25Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Dec[1:47]))
test
plot(rwi$F25Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F25Ace))
temp_cor[178,3] = test$estimate
temp_cor[178,4] = test$p.value

# Current January
test = my.cor.test(rwi$F25Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Jan[2:48]))
test
plot(rwi$F25Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F25Ace))
temp_cor[179,3] = test$estimate
temp_cor[179,4] = test$p.value

# Current February
test = my.cor.test(rwi$F25Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Feb[2:48]))
test
plot(rwi$F25Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F25Ace))
temp_cor[180,3] = test$estimate
temp_cor[180,4] = test$p.value

# Current March
test = my.cor.test(rwi$F25Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Mar[2:48]))
test
plot(rwi$F25Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F25Ace))
temp_cor[181,3] = test$estimate
temp_cor[181,4] = test$p.value

# Current April
test = my.cor.test(rwi$F25Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Apr[2:48]))
test
plot(rwi$F25Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F25Ace))
temp_cor[182,3] = test$estimate
temp_cor[182,4] = test$p.value

# Current May
test = my.cor.test(rwi$F25Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$May[2:48]))
test
plot(rwi$F25Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F25Ace))
temp_cor[183,3] = test$estimate
temp_cor[183,4] = test$p.value

# Current June
test = my.cor.test(rwi$F25Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Jun[2:48]))
test
plot(rwi$F25Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F25Ace))
temp_cor[184,3] = test$estimate
temp_cor[184,4] = test$p.value

# Current July
test = my.cor.test(rwi$F25Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Jul[2:48]))
test
plot(rwi$F25Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F25Ace))
temp_cor[185,3] = test$estimate
temp_cor[185,4] = test$p.value

# Current August
test = my.cor.test(rwi$F25Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Aug[2:48]))
test
plot(rwi$F25Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F25Ace))
temp_cor[186,3] = test$estimate
temp_cor[186,4] = test$p.value

# Current September
test = my.cor.test(rwi$F25Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Sep[2:48]))
test
plot(rwi$F25Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F25Ace))
temp_cor[187,3] = test$estimate
temp_cor[187,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F25, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$May[1:47]))
test
plot(lts$F25, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F25))
temp_cor[171,6] = test$estimate
temp_cor[171,7] = test$p.value

# Previous June
test = my.cor.test(lts$F25, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Jun[1:47]))
test
plot(lts$F25, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F25))
temp_cor[172,6] = test$estimate
temp_cor[172,7] = test$p.value

# Previous July
test = my.cor.test(lts$F25, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Jul[1:47]))
test
plot(lts$F25, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F25))
temp_cor[173,6] = test$estimate
temp_cor[173,7] = test$p.value

# Previous August
test = my.cor.test(lts$F25, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Aug[1:47]))
test
plot(lts$F25, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F25))
temp_cor[174,6] = test$estimate
temp_cor[174,7] = test$p.value

# Previous September
test = my.cor.test(lts$F25, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Sep[1:47]))
test
plot(lts$F25, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F25))
temp_cor[175,6] = test$estimate
temp_cor[175,7] = test$p.value

# Previous October
test = my.cor.test(lts$F25, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Oct[1:47]))
test
plot(lts$F25, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F25))
temp_cor[176,6] = test$estimate
temp_cor[176,7] = test$p.value

# Previous November
test = my.cor.test(lts$F25, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Nov[1:47]))
test
plot(lts$F25, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F25))
temp_cor[177,6] = test$estimate
temp_cor[177,7] = test$p.value

# Previous December
test = my.cor.test(lts$F25, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Dec[1:47]))
test
plot(lts$F25, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F25))
temp_cor[178,6] = test$estimate
temp_cor[178,7] = test$p.value

# Current January
test = my.cor.test(lts$F25, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Jan[2:48]))
test
plot(lts$F25, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F25))
temp_cor[179,6] = test$estimate
temp_cor[179,7] = test$p.value

# Current February
test = my.cor.test(lts$F25, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Feb[2:48]))
test
plot(lts$F25, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F25))
temp_cor[180,6] = test$estimate
temp_cor[180,7] = test$p.value

# Current March
test = my.cor.test(lts$F25, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Mar[2:48]))
test
plot(lts$F25, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F25))
temp_cor[181,6] = test$estimate
temp_cor[181,7] = test$p.value

# Current April
test = my.cor.test(lts$F25, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Apr[2:48]))
test
plot(lts$F25, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F25))
temp_cor[182,6] = test$estimate
temp_cor[182,7] = test$p.value

# Current May
test = my.cor.test(lts$F25, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$May[2:48]))
test
plot(lts$F25, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F25))
temp_cor[183,6] = test$estimate
temp_cor[183,7] = test$p.value

# Current June
test = my.cor.test(lts$F25, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Jun[2:48]))
test
plot(lts$F25, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F25))
temp_cor[184,6] = test$estimate
temp_cor[184,7] = test$p.value

# Current July
test = my.cor.test(lts$F25, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Jul[2:48]))
test
plot(lts$F25, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F25))
temp_cor[185,6] = test$estimate
temp_cor[185,7] = test$p.value

# Current August
test = my.cor.test(lts$F25, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Aug[2:48]))
test
plot(lts$F25, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F25))
temp_cor[186,6] = test$estimate
temp_cor[186,7] = test$p.value

# Current September
test = my.cor.test(lts$F25, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, temp_P$Sep[2:48]))
test
plot(lts$F25, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F25))
temp_cor[187,6] = test$estimate
temp_cor[187,7] = test$p.value
#####

##### Temperature: Site 12Pop #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M01Pop, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$May[1:47]))
test
plot(rwi$M01Pop, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M01Pop))
temp_cor[188,3] = test$estimate
temp_cor[188,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M01Pop, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Jun[1:47]))
test
plot(rwi$M01Pop, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M01Pop))
temp_cor[189,3] = test$estimate
temp_cor[189,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M01Pop, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Jul[1:47]))
test
plot(rwi$M01Pop, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M01Pop))
temp_cor[190,3] = test$estimate
temp_cor[190,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M01Pop, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Aug[1:47]))
test
plot(rwi$M01Pop, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M01Pop))
temp_cor[191,3] = test$estimate
temp_cor[191,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M01Pop, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Sep[1:47]))
test
plot(rwi$M01Pop, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M01Pop))
temp_cor[192,3] = test$estimate
temp_cor[192,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M01Pop, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Oct[1:47]))
test
plot(rwi$M01Pop, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M01Pop))
temp_cor[193,3] = test$estimate
temp_cor[193,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M01Pop, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Nov[1:47]))
test
plot(rwi$M01Pop, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M01Pop))
temp_cor[194,3] = test$estimate
temp_cor[194,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M01Pop, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Dec[1:47]))
test
plot(rwi$M01Pop, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M01Pop))
temp_cor[195,3] = test$estimate
temp_cor[195,4] = test$p.value

# Current January
test = my.cor.test(rwi$M01Pop, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Jan[2:48]))
test
plot(rwi$M01Pop, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M01Pop))
temp_cor[196,3] = test$estimate
temp_cor[196,4] = test$p.value

# Current February
test = my.cor.test(rwi$M01Pop, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Feb[2:48]))
test
plot(rwi$M01Pop, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M01Pop))
temp_cor[197,3] = test$estimate
temp_cor[197,4] = test$p.value

# Current March
test = my.cor.test(rwi$M01Pop, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Mar[2:48]))
test
plot(rwi$M01Pop, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M01Pop))
temp_cor[198,3] = test$estimate
temp_cor[198,4] = test$p.value

# Current April
test = my.cor.test(rwi$M01Pop, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Apr[2:48]))
test
plot(rwi$M01Pop, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M01Pop))
temp_cor[199,3] = test$estimate
temp_cor[199,4] = test$p.value

# Current May
test = my.cor.test(rwi$M01Pop, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$May[2:48]))
test
plot(rwi$M01Pop, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M01Pop))
temp_cor[200,3] = test$estimate
temp_cor[200,4] = test$p.value

# Current June
test = my.cor.test(rwi$M01Pop, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Jun[2:48]))
test
plot(rwi$M01Pop, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M01Pop))
temp_cor[201,3] = test$estimate
temp_cor[201,4] = test$p.value

# Current July
test = my.cor.test(rwi$M01Pop, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Jul[2:48]))
test
plot(rwi$M01Pop, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M01Pop))
temp_cor[202,3] = test$estimate
temp_cor[202,4] = test$p.value

# Current August
test = my.cor.test(rwi$M01Pop, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Aug[2:48]))
test
plot(rwi$M01Pop, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M01Pop))
temp_cor[203,3] = test$estimate
temp_cor[203,4] = test$p.value

# Current September
test = my.cor.test(rwi$M01Pop, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Sep[2:48]))
test
plot(rwi$M01Pop, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M01Pop))
temp_cor[204,3] = test$estimate
temp_cor[204,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M01, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$May[1:47]))
test
plot(lts$M01, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M01))
temp_cor[188,6] = test$estimate
temp_cor[188,7] = test$p.value

# Previous June
test = my.cor.test(lts$M01, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Jun[1:47]))
test
plot(lts$M01, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M01))
temp_cor[189,6] = test$estimate
temp_cor[189,7] = test$p.value

# Previous July
test = my.cor.test(lts$M01, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Jul[1:47]))
test
plot(lts$M01, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M01))
temp_cor[190,6] = test$estimate
temp_cor[190,7] = test$p.value

# Previous August
test = my.cor.test(lts$M01, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Aug[1:47]))
test
plot(lts$M01, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M01))
temp_cor[191,6] = test$estimate
temp_cor[191,7] = test$p.value

# Previous September
test = my.cor.test(lts$M01, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Sep[1:47]))
test
plot(lts$M01, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M01))
temp_cor[192,6] = test$estimate
temp_cor[192,7] = test$p.value

# Previous October
test = my.cor.test(lts$M01, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Oct[1:47]))
test
plot(lts$M01, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M01))
temp_cor[193,6] = test$estimate
temp_cor[193,7] = test$p.value

# Previous November
test = my.cor.test(lts$M01, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Nov[1:47]))
test
plot(lts$M01, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M01))
temp_cor[194,6] = test$estimate
temp_cor[194,7] = test$p.value

# Previous December
test = my.cor.test(lts$M01, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Dec[1:47]))
test
plot(lts$M01, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M01))
temp_cor[195,6] = test$estimate
temp_cor[195,7] = test$p.value

# Current January
test = my.cor.test(lts$M01, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Jan[2:48]))
test
plot(lts$M01, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M01))
temp_cor[196,6] = test$estimate
temp_cor[196,7] = test$p.value

# Current February
test = my.cor.test(lts$M01, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Feb[2:48]))
test
plot(lts$M01, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M01))
temp_cor[197,6] = test$estimate
temp_cor[197,7] = test$p.value

# Current March
test = my.cor.test(lts$M01, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Mar[2:48]))
test
plot(lts$M01, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M01))
temp_cor[198,6] = test$estimate
temp_cor[198,7] = test$p.value

# Current April
test = my.cor.test(lts$M01, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Apr[2:48]))
test
plot(lts$M01, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M01))
temp_cor[199,6] = test$estimate
temp_cor[199,7] = test$p.value

# Current May
test = my.cor.test(lts$M01, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$May[2:48]))
test
plot(lts$M01, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M01))
temp_cor[200,6] = test$estimate
temp_cor[200,7] = test$p.value

# Current June
test = my.cor.test(lts$M01, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Jun[2:48]))
test
plot(lts$M01, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M01))
temp_cor[201,6] = test$estimate
temp_cor[201,7] = test$p.value

# Current July
test = my.cor.test(lts$M01, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Jul[2:48]))
test
plot(lts$M01, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M01))
temp_cor[202,6] = test$estimate
temp_cor[202,7] = test$p.value

# Current August
test = my.cor.test(lts$M01, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Aug[2:48]))
test
plot(lts$M01, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M01))
temp_cor[203,6] = test$estimate
temp_cor[203,7] = test$p.value

# Current September
test = my.cor.test(lts$M01, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, temp_P$Sep[2:48]))
test
plot(lts$M01, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M01))
temp_cor[204,6] = test$estimate
temp_cor[204,7] = test$p.value
#####

##### Temperature: Site 13Pic #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F33Pic, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$May[1:47]))
test
plot(rwi$F33Pic, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F33Pic))
temp_cor[205,3] = test$estimate
temp_cor[205,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F33Pic, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Jun[1:47]))
test
plot(rwi$F33Pic, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F33Pic))
temp_cor[206,3] = test$estimate
temp_cor[206,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F33Pic, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Jul[1:47]))
test
plot(rwi$F33Pic, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F33Pic))
temp_cor[207,3] = test$estimate
temp_cor[207,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F33Pic, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Aug[1:47]))
test
plot(rwi$F33Pic, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F33Pic))
temp_cor[208,3] = test$estimate
temp_cor[208,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F33Pic, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Sep[1:47]))
test
plot(rwi$F33Pic, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F33Pic))
temp_cor[209,3] = test$estimate
temp_cor[209,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F33Pic, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Oct[1:47]))
test
plot(rwi$F33Pic, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F33Pic))
temp_cor[210,3] = test$estimate
temp_cor[210,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F33Pic, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Nov[1:47]))
test
plot(rwi$F33Pic, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F33Pic))
temp_cor[211,3] = test$estimate
temp_cor[211,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F33Pic, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Dec[1:47]))
test
plot(rwi$F33Pic, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F33Pic))
temp_cor[212,3] = test$estimate
temp_cor[212,4] = test$p.value

# Current January
test = my.cor.test(rwi$F33Pic, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Jan[2:48]))
test
plot(rwi$F33Pic, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F33Pic))
temp_cor[213,3] = test$estimate
temp_cor[213,4] = test$p.value

# Current February
test = my.cor.test(rwi$F33Pic, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Feb[2:48]))
test
plot(rwi$F33Pic, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F33Pic))
temp_cor[214,3] = test$estimate
temp_cor[214,4] = test$p.value

# Current March
test = my.cor.test(rwi$F33Pic, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Mar[2:48]))
test
plot(rwi$F33Pic, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F33Pic))
temp_cor[215,3] = test$estimate
temp_cor[215,4] = test$p.value

# Current April
test = my.cor.test(rwi$F33Pic, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Apr[2:48]))
test
plot(rwi$F33Pic, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F33Pic))
temp_cor[216,3] = test$estimate
temp_cor[216,4] = test$p.value

# Current May
test = my.cor.test(rwi$F33Pic, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$May[2:48]))
test
plot(rwi$F33Pic, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F33Pic))
temp_cor[217,3] = test$estimate
temp_cor[217,4] = test$p.value

# Current June
test = my.cor.test(rwi$F33Pic, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Jun[2:48]))
test
plot(rwi$F33Pic, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F33Pic))
temp_cor[218,3] = test$estimate
temp_cor[218,4] = test$p.value

# Current July
test = my.cor.test(rwi$F33Pic, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Jul[2:48]))
test
plot(rwi$F33Pic, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F33Pic))
temp_cor[219,3] = test$estimate
temp_cor[219,4] = test$p.value

# Current August
test = my.cor.test(rwi$F33Pic, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Aug[2:48]))
test
plot(rwi$F33Pic, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F33Pic))
temp_cor[220,3] = test$estimate
temp_cor[220,4] = test$p.value

# Current September
test = my.cor.test(rwi$F33Pic, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Sep[2:48]))
test
plot(rwi$F33Pic, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F33Pic))
temp_cor[221,3] = test$estimate
temp_cor[221,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F33, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$May[1:47]))
test
plot(lts$F33, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F33))
temp_cor[205,6] = test$estimate
temp_cor[205,7] = test$p.value

# Previous June
test = my.cor.test(lts$F33, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Jun[1:47]))
test
plot(lts$F33, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F33))
temp_cor[206,6] = test$estimate
temp_cor[206,7] = test$p.value

# Previous July
test = my.cor.test(lts$F33, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Jul[1:47]))
test
plot(lts$F33, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F33))
temp_cor[207,6] = test$estimate
temp_cor[207,7] = test$p.value

# Previous August
test = my.cor.test(lts$F33, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Aug[1:47]))
test
plot(lts$F33, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F33))
temp_cor[208,6] = test$estimate
temp_cor[208,7] = test$p.value

# Previous September
test = my.cor.test(lts$F33, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Sep[1:47]))
test
plot(lts$F33, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F33))
temp_cor[209,6] = test$estimate
temp_cor[209,7] = test$p.value

# Previous October
test = my.cor.test(lts$F33, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Oct[1:47]))
test
plot(lts$F33, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F33))
temp_cor[210,6] = test$estimate
temp_cor[210,7] = test$p.value

# Previous November
test = my.cor.test(lts$F33, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Nov[1:47]))
test
plot(lts$F33, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F33))
temp_cor[211,6] = test$estimate
temp_cor[211,7] = test$p.value

# Previous December
test = my.cor.test(lts$F33, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Dec[1:47]))
test
plot(lts$F33, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F33))
temp_cor[212,6] = test$estimate
temp_cor[212,7] = test$p.value

# Current January
test = my.cor.test(lts$F33, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Jan[2:48]))
test
plot(lts$F33, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F33))
temp_cor[213,6] = test$estimate
temp_cor[213,7] = test$p.value

# Current February
test = my.cor.test(lts$F33, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Feb[2:48]))
test
plot(lts$F33, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F33))
temp_cor[214,6] = test$estimate
temp_cor[214,7] = test$p.value

# Current March
test = my.cor.test(lts$F33, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Mar[2:48]))
test
plot(lts$F33, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F33))
temp_cor[215,6] = test$estimate
temp_cor[215,7] = test$p.value

# Current April
test = my.cor.test(lts$F33, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Apr[2:48]))
test
plot(lts$F33, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F33))
temp_cor[216,6] = test$estimate
temp_cor[216,7] = test$p.value

# Current May
test = my.cor.test(lts$F33, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$May[2:48]))
test
plot(lts$F33, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F33))
temp_cor[217,6] = test$estimate
temp_cor[217,7] = test$p.value

# Current June
test = my.cor.test(lts$F33, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Jun[2:48]))
test
plot(lts$F33, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F33))
temp_cor[218,6] = test$estimate
temp_cor[218,7] = test$p.value

# Current July
test = my.cor.test(lts$F33, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Jul[2:48]))
test
plot(lts$F33, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F33))
temp_cor[219,6] = test$estimate
temp_cor[219,7] = test$p.value

# Current August
test = my.cor.test(lts$F33, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Aug[2:48]))
test
plot(lts$F33, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F33))
temp_cor[220,6] = test$estimate
temp_cor[220,7] = test$p.value

# Current September
test = my.cor.test(lts$F33, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, temp_P$Sep[2:48]))
test
plot(lts$F33, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F33))
temp_cor[221,6] = test$estimate
temp_cor[221,7] = test$p.value
#####

##### Temperature: Site 14Thu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M20Thu, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$May[1:47]))
test
plot(rwi$M20Thu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M20Thu))
temp_cor[222,3] = test$estimate
temp_cor[222,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M20Thu, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Jun[1:47]))
test
plot(rwi$M20Thu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M20Thu))
temp_cor[223,3] = test$estimate
temp_cor[223,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M20Thu, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Jul[1:47]))
test
plot(rwi$M20Thu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M20Thu))
temp_cor[224,3] = test$estimate
temp_cor[224,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M20Thu, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Aug[1:47]))
test
plot(rwi$M20Thu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M20Thu))
temp_cor[225,3] = test$estimate
temp_cor[225,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M20Thu, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Sep[1:47]))
test
plot(rwi$M20Thu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M20Thu))
temp_cor[226,3] = test$estimate
temp_cor[226,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M20Thu, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Oct[1:47]))
test
plot(rwi$M20Thu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M20Thu))
temp_cor[227,3] = test$estimate
temp_cor[227,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M20Thu, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Nov[1:47]))
test
plot(rwi$M20Thu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M20Thu))
temp_cor[228,3] = test$estimate
temp_cor[228,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M20Thu, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Dec[1:47]))
test
plot(rwi$M20Thu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M20Thu))
temp_cor[229,3] = test$estimate
temp_cor[229,4] = test$p.value

# Current January
test = my.cor.test(rwi$M20Thu, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Jan[2:48]))
test
plot(rwi$M20Thu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M20Thu))
temp_cor[230,3] = test$estimate
temp_cor[230,4] = test$p.value

# Current February
test = my.cor.test(rwi$M20Thu, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Feb[2:48]))
test
plot(rwi$M20Thu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M20Thu))
temp_cor[231,3] = test$estimate
temp_cor[231,4] = test$p.value

# Current March
test = my.cor.test(rwi$M20Thu, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Mar[2:48]))
test
plot(rwi$M20Thu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M20Thu))
temp_cor[232,3] = test$estimate
temp_cor[232,4] = test$p.value

# Current April
test = my.cor.test(rwi$M20Thu, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Apr[2:48]))
test
plot(rwi$M20Thu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M20Thu))
temp_cor[233,3] = test$estimate
temp_cor[233,4] = test$p.value

# Current May
test = my.cor.test(rwi$M20Thu, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$May[2:48]))
test
plot(rwi$M20Thu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M20Thu))
temp_cor[234,3] = test$estimate
temp_cor[234,4] = test$p.value

# Current June
test = my.cor.test(rwi$M20Thu, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Jun[2:48]))
test
plot(rwi$M20Thu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M20Thu))
temp_cor[235,3] = test$estimate
temp_cor[235,4] = test$p.value

# Current July
test = my.cor.test(rwi$M20Thu, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Jul[2:48]))
test
plot(rwi$M20Thu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M20Thu))
temp_cor[236,3] = test$estimate
temp_cor[236,4] = test$p.value

# Current August
test = my.cor.test(rwi$M20Thu, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Aug[2:48]))
test
plot(rwi$M20Thu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M20Thu))
temp_cor[237,3] = test$estimate
temp_cor[237,4] = test$p.value

# Current September
test = my.cor.test(rwi$M20Thu, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Sep[2:48]))
test
plot(rwi$M20Thu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M20Thu))
temp_cor[238,3] = test$estimate
temp_cor[238,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M20, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$May[1:47]))
test
plot(lts$M20, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M20))
temp_cor[222,6] = test$estimate
temp_cor[222,7] = test$p.value

# Previous June
test = my.cor.test(lts$M20, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Jun[1:47]))
test
plot(lts$M20, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M20))
temp_cor[223,6] = test$estimate
temp_cor[223,7] = test$p.value

# Previous July
test = my.cor.test(lts$M20, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Jul[1:47]))
test
plot(lts$M20, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M20))
temp_cor[224,6] = test$estimate
temp_cor[224,7] = test$p.value

# Previous August
test = my.cor.test(lts$M20, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Aug[1:47]))
test
plot(lts$M20, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M20))
temp_cor[225,6] = test$estimate
temp_cor[225,7] = test$p.value

# Previous September
test = my.cor.test(lts$M20, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Sep[1:47]))
test
plot(lts$M20, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M20))
temp_cor[226,6] = test$estimate
temp_cor[226,7] = test$p.value

# Previous October
test = my.cor.test(lts$M20, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Oct[1:47]))
test
plot(lts$M20, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M20))
temp_cor[227,6] = test$estimate
temp_cor[227,7] = test$p.value

# Previous November
test = my.cor.test(lts$M20, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Nov[1:47]))
test
plot(lts$M20, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M20))
temp_cor[228,6] = test$estimate
temp_cor[228,7] = test$p.value

# Previous December
test = my.cor.test(lts$M20, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Dec[1:47]))
test
plot(lts$M20, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M20))
temp_cor[229,6] = test$estimate
temp_cor[229,7] = test$p.value

# Current January
test = my.cor.test(lts$M20, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Jan[2:48]))
test
plot(lts$M20, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M20))
temp_cor[230,6] = test$estimate
temp_cor[230,7] = test$p.value

# Current February
test = my.cor.test(lts$M20, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Feb[2:48]))
test
plot(lts$M20, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M20))
temp_cor[231,6] = test$estimate
temp_cor[231,7] = test$p.value

# Current March
test = my.cor.test(lts$M20, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Mar[2:48]))
test
plot(lts$M20, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M20))
temp_cor[232,6] = test$estimate
temp_cor[232,7] = test$p.value

# Current April
test = my.cor.test(lts$M20, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Apr[2:48]))
test
plot(lts$M20, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M20))
temp_cor[233,6] = test$estimate
temp_cor[233,7] = test$p.value

# Current May
test = my.cor.test(lts$M20, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$May[2:48]))
test
plot(lts$M20, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M20))
temp_cor[234,6] = test$estimate
temp_cor[234,7] = test$p.value

# Current June
test = my.cor.test(lts$M20, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Jun[2:48]))
test
plot(lts$M20, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M20))
temp_cor[235,6] = test$estimate
temp_cor[235,7] = test$p.value

# Current July
test = my.cor.test(lts$M20, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Jul[2:48]))
test
plot(lts$M20, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M20))
temp_cor[236,6] = test$estimate
temp_cor[236,7] = test$p.value

# Current August
test = my.cor.test(lts$M20, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Aug[2:48]))
test
plot(lts$M20, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M20))
temp_cor[237,6] = test$estimate
temp_cor[237,7] = test$p.value

# Current September
test = my.cor.test(lts$M20, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, temp_P$Sep[2:48]))
test
plot(lts$M20, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M20))
temp_cor[238,6] = test$estimate
temp_cor[238,7] = test$p.value
#####

##### Temperature: Site 15Bet #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F30Bet, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$May[1:47]))
test
plot(rwi$F30Bet, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F30Bet))
temp_cor[239,3] = test$estimate
temp_cor[239,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F30Bet, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Jun[1:47]))
test
plot(rwi$F30Bet, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F30Bet))
temp_cor[240,3] = test$estimate
temp_cor[240,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F30Bet, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Jul[1:47]))
test
plot(rwi$F30Bet, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F30Bet))
temp_cor[241,3] = test$estimate
temp_cor[241,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F30Bet, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Aug[1:47]))
test
plot(rwi$F30Bet, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F30Bet))
temp_cor[242,3] = test$estimate
temp_cor[242,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F30Bet, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Sep[1:47]))
test
plot(rwi$F30Bet, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F30Bet))
temp_cor[243,3] = test$estimate
temp_cor[243,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F30Bet, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Oct[1:47]))
test
plot(rwi$F30Bet, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F30Bet))
temp_cor[244,3] = test$estimate
temp_cor[244,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F30Bet, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Nov[1:47]))
test
plot(rwi$F30Bet, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F30Bet))
temp_cor[245,3] = test$estimate
temp_cor[245,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F30Bet, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Dec[1:47]))
test
plot(rwi$F30Bet, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F30Bet))
temp_cor[246,3] = test$estimate
temp_cor[246,4] = test$p.value

# Current January
test = my.cor.test(rwi$F30Bet, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Jan[2:48]))
test
plot(rwi$F30Bet, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F30Bet))
temp_cor[247,3] = test$estimate
temp_cor[247,4] = test$p.value

# Current February
test = my.cor.test(rwi$F30Bet, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Feb[2:48]))
test
plot(rwi$F30Bet, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F30Bet))
temp_cor[248,3] = test$estimate
temp_cor[248,4] = test$p.value

# Current March
test = my.cor.test(rwi$F30Bet, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Mar[2:48]))
test
plot(rwi$F30Bet, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F30Bet))
temp_cor[249,3] = test$estimate
temp_cor[249,4] = test$p.value

# Current April
test = my.cor.test(rwi$F30Bet, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Apr[2:48]))
test
plot(rwi$F30Bet, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F30Bet))
temp_cor[250,3] = test$estimate
temp_cor[250,4] = test$p.value

# Current May
test = my.cor.test(rwi$F30Bet, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$May[2:48]))
test
plot(rwi$F30Bet, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F30Bet))
temp_cor[251,3] = test$estimate
temp_cor[251,4] = test$p.value

# Current June
test = my.cor.test(rwi$F30Bet, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Jun[2:48]))
test
plot(rwi$F30Bet, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F30Bet))
temp_cor[252,3] = test$estimate
temp_cor[252,4] = test$p.value

# Current July
test = my.cor.test(rwi$F30Bet, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Jul[2:48]))
test
plot(rwi$F30Bet, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F30Bet))
temp_cor[253,3] = test$estimate
temp_cor[253,4] = test$p.value

# Current August
test = my.cor.test(rwi$F30Bet, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Aug[2:48]))
test
plot(rwi$F30Bet, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F30Bet))
temp_cor[254,3] = test$estimate
temp_cor[254,4] = test$p.value

# Current September
test = my.cor.test(rwi$F30Bet, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Sep[2:48]))
test
plot(rwi$F30Bet, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F30Bet))
temp_cor[255,3] = test$estimate
temp_cor[255,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F30, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$May[1:47]))
test
plot(lts$F30, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$F30))
temp_cor[239,6] = test$estimate
temp_cor[239,7] = test$p.value

# Previous June
test = my.cor.test(lts$F30, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Jun[1:47]))
test
plot(lts$F30, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$F30))
temp_cor[240,6] = test$estimate
temp_cor[240,7] = test$p.value

# Previous July
test = my.cor.test(lts$F30, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Jul[1:47]))
test
plot(lts$F30, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$F30))
temp_cor[241,6] = test$estimate
temp_cor[241,7] = test$p.value

# Previous August
test = my.cor.test(lts$F30, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Aug[1:47]))
test
plot(lts$F30, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$F30))
temp_cor[242,6] = test$estimate
temp_cor[242,7] = test$p.value

# Previous September
test = my.cor.test(lts$F30, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Sep[1:47]))
test
plot(lts$F30, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$F30))
temp_cor[243,6] = test$estimate
temp_cor[243,7] = test$p.value

# Previous October
test = my.cor.test(lts$F30, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Oct[1:47]))
test
plot(lts$F30, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$F30))
temp_cor[244,6] = test$estimate
temp_cor[244,7] = test$p.value

# Previous November
test = my.cor.test(lts$F30, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Nov[1:47]))
test
plot(lts$F30, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$F30))
temp_cor[245,6] = test$estimate
temp_cor[245,7] = test$p.value

# Previous December
test = my.cor.test(lts$F30, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Dec[1:47]))
test
plot(lts$F30, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$F30))
temp_cor[246,6] = test$estimate
temp_cor[246,7] = test$p.value

# Current January
test = my.cor.test(lts$F30, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Jan[2:48]))
test
plot(lts$F30, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$F30))
temp_cor[247,6] = test$estimate
temp_cor[247,7] = test$p.value

# Current February
test = my.cor.test(lts$F30, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Feb[2:48]))
test
plot(lts$F30, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$F30))
temp_cor[248,6] = test$estimate
temp_cor[248,7] = test$p.value

# Current March
test = my.cor.test(lts$F30, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Mar[2:48]))
test
plot(lts$F30, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$F30))
temp_cor[249,6] = test$estimate
temp_cor[249,7] = test$p.value

# Current April
test = my.cor.test(lts$F30, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Apr[2:48]))
test
plot(lts$F30, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$F30))
temp_cor[250,6] = test$estimate
temp_cor[250,7] = test$p.value

# Current May
test = my.cor.test(lts$F30, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$May[2:48]))
test
plot(lts$F30, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$F30))
temp_cor[251,6] = test$estimate
temp_cor[251,7] = test$p.value

# Current June
test = my.cor.test(lts$F30, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Jun[2:48]))
test
plot(lts$F30, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$F30))
temp_cor[252,6] = test$estimate
temp_cor[252,7] = test$p.value

# Current July
test = my.cor.test(lts$F30, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Jul[2:48]))
test
plot(lts$F30, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$F30))
temp_cor[253,6] = test$estimate
temp_cor[253,7] = test$p.value

# Current August
test = my.cor.test(lts$F30, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Aug[2:48]))
test
plot(lts$F30, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$F30))
temp_cor[254,6] = test$estimate
temp_cor[254,7] = test$p.value

# Current September
test = my.cor.test(lts$F30, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, temp_P$Sep[2:48]))
test
plot(lts$F30, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$F30))
temp_cor[255,6] = test$estimate
temp_cor[255,7] = test$p.value
#####

##### Temperature: Site 16Thu #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M17Thu, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$May[1:47]))
test
plot(rwi$M17Thu, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M17Thu))
temp_cor[256,3] = test$estimate
temp_cor[256,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M17Thu, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Jun[1:47]))
test
plot(rwi$M17Thu, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M17Thu))
temp_cor[257,3] = test$estimate
temp_cor[257,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M17Thu, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Jul[1:47]))
test
plot(rwi$M17Thu, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M17Thu))
temp_cor[258,3] = test$estimate
temp_cor[258,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M17Thu, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Aug[1:47]))
test
plot(rwi$M17Thu, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M17Thu))
temp_cor[259,3] = test$estimate
temp_cor[259,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M17Thu, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Sep[1:47]))
test
plot(rwi$M17Thu, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M17Thu))
temp_cor[260,3] = test$estimate
temp_cor[260,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M17Thu, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Oct[1:47]))
test
plot(rwi$M17Thu, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M17Thu))
temp_cor[261,3] = test$estimate
temp_cor[261,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M17Thu, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Nov[1:47]))
test
plot(rwi$M17Thu, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M17Thu))
temp_cor[262,3] = test$estimate
temp_cor[262,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M17Thu, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Dec[1:47]))
test
plot(rwi$M17Thu, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M17Thu))
temp_cor[263,3] = test$estimate
temp_cor[263,4] = test$p.value

# Current January
test = my.cor.test(rwi$M17Thu, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Jan[2:48]))
test
plot(rwi$M17Thu, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M17Thu))
temp_cor[264,3] = test$estimate
temp_cor[264,4] = test$p.value

# Current February
test = my.cor.test(rwi$M17Thu, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Feb[2:48]))
test
plot(rwi$M17Thu, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M17Thu))
temp_cor[265,3] = test$estimate
temp_cor[265,4] = test$p.value

# Current March
test = my.cor.test(rwi$M17Thu, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Mar[2:48]))
test
plot(rwi$M17Thu, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M17Thu))
temp_cor[266,3] = test$estimate
temp_cor[266,4] = test$p.value

# Current April
test = my.cor.test(rwi$M17Thu, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Apr[2:48]))
test
plot(rwi$M17Thu, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M17Thu))
temp_cor[267,3] = test$estimate
temp_cor[267,4] = test$p.value

# Current May
test = my.cor.test(rwi$M17Thu, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$May[2:48]))
test
plot(rwi$M17Thu, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M17Thu))
temp_cor[268,3] = test$estimate
temp_cor[268,4] = test$p.value

# Current June
test = my.cor.test(rwi$M17Thu, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Jun[2:48]))
test
plot(rwi$M17Thu, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M17Thu))
temp_cor[269,3] = test$estimate
temp_cor[269,4] = test$p.value

# Current July
test = my.cor.test(rwi$M17Thu, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Jul[2:48]))
test
plot(rwi$M17Thu, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M17Thu))
temp_cor[270,3] = test$estimate
temp_cor[270,4] = test$p.value

# Current August
test = my.cor.test(rwi$M17Thu, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Aug[2:48]))
test
plot(rwi$M17Thu, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M17Thu))
temp_cor[271,3] = test$estimate
temp_cor[271,4] = test$p.value

# Current September
test = my.cor.test(rwi$M17Thu, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Sep[2:48]))
test
plot(rwi$M17Thu, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M17Thu))
temp_cor[272,3] = test$estimate
temp_cor[272,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M17, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$May[1:47]))
test
plot(lts$M17, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ lts$M17))
temp_cor[256,6] = test$estimate
temp_cor[256,7] = test$p.value

# Previous June
test = my.cor.test(lts$M17, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Jun[1:47]))
test
plot(lts$M17, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ lts$M17))
temp_cor[257,6] = test$estimate
temp_cor[257,7] = test$p.value

# Previous July
test = my.cor.test(lts$M17, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Jul[1:47]))
test
plot(lts$M17, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ lts$M17))
temp_cor[258,6] = test$estimate
temp_cor[258,7] = test$p.value

# Previous August
test = my.cor.test(lts$M17, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Aug[1:47]))
test
plot(lts$M17, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ lts$M17))
temp_cor[259,6] = test$estimate
temp_cor[259,7] = test$p.value

# Previous September
test = my.cor.test(lts$M17, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Sep[1:47]))
test
plot(lts$M17, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ lts$M17))
temp_cor[260,6] = test$estimate
temp_cor[260,7] = test$p.value

# Previous October
test = my.cor.test(lts$M17, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Oct[1:47]))
test
plot(lts$M17, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ lts$M17))
temp_cor[261,6] = test$estimate
temp_cor[261,7] = test$p.value

# Previous November
test = my.cor.test(lts$M17, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Nov[1:47]))
test
plot(lts$M17, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ lts$M17))
temp_cor[262,6] = test$estimate
temp_cor[262,7] = test$p.value

# Previous December
test = my.cor.test(lts$M17, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Dec[1:47]))
test
plot(lts$M17, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ lts$M17))
temp_cor[263,6] = test$estimate
temp_cor[263,7] = test$p.value

# Current January
test = my.cor.test(lts$M17, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Jan[2:48]))
test
plot(lts$M17, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ lts$M17))
temp_cor[264,6] = test$estimate
temp_cor[264,7] = test$p.value

# Current February
test = my.cor.test(lts$M17, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Feb[2:48]))
test
plot(lts$M17, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ lts$M17))
temp_cor[265,6] = test$estimate
temp_cor[265,7] = test$p.value

# Current March
test = my.cor.test(lts$M17, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Mar[2:48]))
test
plot(lts$M17, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ lts$M17))
temp_cor[266,6] = test$estimate
temp_cor[266,7] = test$p.value

# Current April
test = my.cor.test(lts$M17, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Apr[2:48]))
test
plot(lts$M17, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ lts$M17))
temp_cor[267,6] = test$estimate
temp_cor[267,7] = test$p.value

# Current May
test = my.cor.test(lts$M17, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$May[2:48]))
test
plot(lts$M17, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ lts$M17))
temp_cor[268,6] = test$estimate
temp_cor[268,7] = test$p.value

# Current June
test = my.cor.test(lts$M17, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Jun[2:48]))
test
plot(lts$M17, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ lts$M17))
temp_cor[269,6] = test$estimate
temp_cor[269,7] = test$p.value

# Current July
test = my.cor.test(lts$M17, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Jul[2:48]))
test
plot(lts$M17, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ lts$M17))
temp_cor[270,6] = test$estimate
temp_cor[270,7] = test$p.value

# Current August
test = my.cor.test(lts$M17, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Aug[2:48]))
test
plot(lts$M17, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ lts$M17))
temp_cor[271,6] = test$estimate
temp_cor[271,7] = test$p.value

# Current September
test = my.cor.test(lts$M17, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, temp_P$Sep[2:48]))
test
plot(lts$M17, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ lts$M17))
temp_cor[272,6] = test$estimate
temp_cor[272,7] = test$p.value
#####

##### Temperature: Site 02Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F15Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$May[1:47]))
test
plot(rwi$F15Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F15Ace))
temp_cor[273,3] = test$estimate
temp_cor[273,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F15Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Jun[1:47]))
test
plot(rwi$F15Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F15Ace))
temp_cor[274,3] = test$estimate
temp_cor[274,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F15Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Jul[1:47]))
test
plot(rwi$F15Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F15Ace))
temp_cor[275,3] = test$estimate
temp_cor[275,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F15Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Aug[1:47]))
test
plot(rwi$F15Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F15Ace))
temp_cor[276,3] = test$estimate
temp_cor[276,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F15Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Sep[1:47]))
test
plot(rwi$F15Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F15Ace))
temp_cor[277,3] = test$estimate
temp_cor[277,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F15Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Oct[1:47]))
test
plot(rwi$F15Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F15Ace))
temp_cor[278,3] = test$estimate
temp_cor[278,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F15Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Nov[1:47]))
test
plot(rwi$F15Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F15Ace))
temp_cor[279,3] = test$estimate
temp_cor[279,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F15Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Dec[1:47]))
test
plot(rwi$F15Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F15Ace))
temp_cor[280,3] = test$estimate
temp_cor[280,4] = test$p.value

# Current January
test = my.cor.test(rwi$F15Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Jan[2:48]))
test
plot(rwi$F15Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F15Ace))
temp_cor[281,3] = test$estimate
temp_cor[281,4] = test$p.value

# Current February
test = my.cor.test(rwi$F15Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Feb[2:48]))
test
plot(rwi$F15Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F15Ace))
temp_cor[282,3] = test$estimate
temp_cor[282,4] = test$p.value

# Current March
test = my.cor.test(rwi$F15Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Mar[2:48]))
test
plot(rwi$F15Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F15Ace))
temp_cor[283,3] = test$estimate
temp_cor[283,4] = test$p.value

# Current April
test = my.cor.test(rwi$F15Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Apr[2:48]))
test
plot(rwi$F15Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F15Ace))
temp_cor[284,3] = test$estimate
temp_cor[284,4] = test$p.value

# Current May
test = my.cor.test(rwi$F15Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$May[2:48]))
test
plot(rwi$F15Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F15Ace))
temp_cor[285,3] = test$estimate
temp_cor[285,4] = test$p.value

# Current June
test = my.cor.test(rwi$F15Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Jun[2:48]))
test
plot(rwi$F15Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F15Ace))
temp_cor[286,3] = test$estimate
temp_cor[286,4] = test$p.value

# Current July
test = my.cor.test(rwi$F15Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Jul[2:48]))
test
plot(rwi$F15Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F15Ace))
temp_cor[287,3] = test$estimate
temp_cor[287,4] = test$p.value

# Current August
test = my.cor.test(rwi$F15Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Aug[2:48]))
test
plot(rwi$F15Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F15Ace))
temp_cor[288,3] = test$estimate
temp_cor[288,4] = test$p.value

# Current September
test = my.cor.test(rwi$F15Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Sep[2:48]))
test
plot(rwi$F15Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F15Ace))
temp_cor[289,3] = test$estimate
temp_cor[289,4] = test$p.value
#####

##### Temperature: Site 03Que #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M06Que, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$May[1:47]))
test
plot(rwi$M06Que, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M06Que))
temp_cor[290,3] = test$estimate
temp_cor[290,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M06Que, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Jun[1:47]))
test
plot(rwi$M06Que, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M06Que))
temp_cor[291,3] = test$estimate
temp_cor[291,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M06Que, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Jul[1:47]))
test
plot(rwi$M06Que, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M06Que))
temp_cor[292,3] = test$estimate
temp_cor[292,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M06Que, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Aug[1:47]))
test
plot(rwi$M06Que, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M06Que))
temp_cor[293,3] = test$estimate
temp_cor[293,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M06Que, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Sep[1:47]))
test
plot(rwi$M06Que, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M06Que))
temp_cor[294,3] = test$estimate
temp_cor[294,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M06Que, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Oct[1:47]))
test
plot(rwi$M06Que, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M06Que))
temp_cor[295,3] = test$estimate
temp_cor[295,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M06Que, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Nov[1:47]))
test
plot(rwi$M06Que, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M06Que))
temp_cor[296,3] = test$estimate
temp_cor[296,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M06Que, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Dec[1:47]))
test
plot(rwi$M06Que, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M06Que))
temp_cor[297,3] = test$estimate
temp_cor[297,4] = test$p.value

# Current January
test = my.cor.test(rwi$M06Que, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Jan[2:48]))
test
plot(rwi$M06Que, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M06Que))
temp_cor[298,3] = test$estimate
temp_cor[298,4] = test$p.value

# Current February
test = my.cor.test(rwi$M06Que, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Feb[2:48]))
test
plot(rwi$M06Que, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M06Que))
temp_cor[299,3] = test$estimate
temp_cor[299,4] = test$p.value

# Current March
test = my.cor.test(rwi$M06Que, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Mar[2:48]))
test
plot(rwi$M06Que, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M06Que))
temp_cor[300,3] = test$estimate
temp_cor[300,4] = test$p.value

# Current April
test = my.cor.test(rwi$M06Que, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Apr[2:48]))
test
plot(rwi$M06Que, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M06Que))
temp_cor[301,3] = test$estimate
temp_cor[301,4] = test$p.value

# Current May
test = my.cor.test(rwi$M06Que, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$May[2:48]))
test
plot(rwi$M06Que, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M06Que))
temp_cor[302,3] = test$estimate
temp_cor[302,4] = test$p.value

# Current June
test = my.cor.test(rwi$M06Que, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Jun[2:48]))
test
plot(rwi$M06Que, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M06Que))
temp_cor[303,3] = test$estimate
temp_cor[303,4] = test$p.value

# Current July
test = my.cor.test(rwi$M06Que, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Jul[2:48]))
test
plot(rwi$M06Que, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M06Que))
temp_cor[304,3] = test$estimate
temp_cor[304,4] = test$p.value

# Current August
test = my.cor.test(rwi$M06Que, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Aug[2:48]))
test
plot(rwi$M06Que, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M06Que))
temp_cor[305,3] = test$estimate
temp_cor[305,4] = test$p.value

# Current September
test = my.cor.test(rwi$M06Que, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Sep[2:48]))
test
plot(rwi$M06Que, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M06Que))
temp_cor[306,3] = test$estimate
temp_cor[306,4] = test$p.value
#####

##### Temperature: Site 10Dec #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M26Dec, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$May[1:47]))
test
plot(rwi$M26Dec, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$M26Dec))
temp_cor[307,3] = test$estimate
temp_cor[307,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M26Dec, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Jun[1:47]))
test
plot(rwi$M26Dec, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$M26Dec))
temp_cor[308,3] = test$estimate
temp_cor[308,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M26Dec, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Jul[1:47]))
test
plot(rwi$M26Dec, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$M26Dec))
temp_cor[309,3] = test$estimate
temp_cor[309,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M26Dec, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Aug[1:47]))
test
plot(rwi$M26Dec, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$M26Dec))
temp_cor[310,3] = test$estimate
temp_cor[310,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M26Dec, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Sep[1:47]))
test
plot(rwi$M26Dec, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$M26Dec))
temp_cor[311,3] = test$estimate
temp_cor[311,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M26Dec, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Oct[1:47]))
test
plot(rwi$M26Dec, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$M26Dec))
temp_cor[312,3] = test$estimate
temp_cor[312,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M26Dec, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Nov[1:47]))
test
plot(rwi$M26Dec, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$M26Dec))
temp_cor[313,3] = test$estimate
temp_cor[313,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M26Dec, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Dec[1:47]))
test
plot(rwi$M26Dec, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$M26Dec))
temp_cor[314,3] = test$estimate
temp_cor[314,4] = test$p.value

# Current January
test = my.cor.test(rwi$M26Dec, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Jan[2:48]))
test
plot(rwi$M26Dec, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$M26Dec))
temp_cor[315,3] = test$estimate
temp_cor[315,4] = test$p.value

# Current February
test = my.cor.test(rwi$M26Dec, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Feb[2:48]))
test
plot(rwi$M26Dec, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$M26Dec))
temp_cor[316,3] = test$estimate
temp_cor[316,4] = test$p.value

# Current March
test = my.cor.test(rwi$M26Dec, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Mar[2:48]))
test
plot(rwi$M26Dec, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$M26Dec))
temp_cor[317,3] = test$estimate
temp_cor[317,4] = test$p.value

# Current April
test = my.cor.test(rwi$M26Dec, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Apr[2:48]))
test
plot(rwi$M26Dec, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$M26Dec))
temp_cor[318,3] = test$estimate
temp_cor[318,4] = test$p.value

# Current May
test = my.cor.test(rwi$M26Dec, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$May[2:48]))
test
plot(rwi$M26Dec, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$M26Dec))
temp_cor[319,3] = test$estimate
temp_cor[319,4] = test$p.value

# Current June
test = my.cor.test(rwi$M26Dec, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Jun[2:48]))
test
plot(rwi$M26Dec, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$M26Dec))
temp_cor[320,3] = test$estimate
temp_cor[320,4] = test$p.value

# Current July
test = my.cor.test(rwi$M26Dec, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Jul[2:48]))
test
plot(rwi$M26Dec, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$M26Dec))
temp_cor[321,3] = test$estimate
temp_cor[321,4] = test$p.value

# Current August
test = my.cor.test(rwi$M26Dec, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Aug[2:48]))
test
plot(rwi$M26Dec, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$M26Dec))
temp_cor[322,3] = test$estimate
temp_cor[322,4] = test$p.value

# Current September
test = my.cor.test(rwi$M26Dec, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Sep[2:48]))
test
plot(rwi$M26Dec, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$M26Dec))
temp_cor[323,3] = test$estimate
temp_cor[323,4] = test$p.value
#####

##### Temperature: Site 15Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F30Ace, temp_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$May[1:47]))
test
plot(rwi$F30Ace, temp_P$May[1:47])
abline(lm(temp_P$May[1:47] ~ rwi$F30Ace))
temp_cor[324,3] = test$estimate
temp_cor[324,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F30Ace, temp_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Jun[1:47]))
test
plot(rwi$F30Ace, temp_P$Jun[1:47])
abline(lm(temp_P$Jun[1:47] ~ rwi$F30Ace))
temp_cor[325,3] = test$estimate
temp_cor[325,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F30Ace, temp_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Jul[1:47]))
test
plot(rwi$F30Ace, temp_P$Jul[1:47])
abline(lm(temp_P$Jul[1:47] ~ rwi$F30Ace))
temp_cor[326,3] = test$estimate
temp_cor[326,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F30Ace, temp_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Aug[1:47]))
test
plot(rwi$F30Ace, temp_P$Aug[1:47])
abline(lm(temp_P$Aug[1:47] ~ rwi$F30Ace))
temp_cor[327,3] = test$estimate
temp_cor[327,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F30Ace, temp_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Sep[1:47]))
test
plot(rwi$F30Ace, temp_P$Sep[1:47])
abline(lm(temp_P$Sep[1:47] ~ rwi$F30Ace))
temp_cor[328,3] = test$estimate
temp_cor[328,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F30Ace, temp_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Oct[1:47]))
test
plot(rwi$F30Ace, temp_P$Oct[1:47])
abline(lm(temp_P$Oct[1:47] ~ rwi$F30Ace))
temp_cor[329,3] = test$estimate
temp_cor[329,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F30Ace, temp_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Nov[1:47]))
test
plot(rwi$F30Ace, temp_P$Nov[1:47])
abline(lm(temp_P$Nov[1:47] ~ rwi$F30Ace))
temp_cor[330,3] = test$estimate
temp_cor[330,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F30Ace, temp_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Dec[1:47]))
test
plot(rwi$F30Ace, temp_P$Dec[1:47])
abline(lm(temp_P$Dec[1:47] ~ rwi$F30Ace))
temp_cor[331,3] = test$estimate
temp_cor[331,4] = test$p.value

# Current January
test = my.cor.test(rwi$F30Ace, temp_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Jan[2:48]))
test
plot(rwi$F30Ace, temp_P$Jan[2:48])
abline(lm(temp_P$Jan[2:48] ~ rwi$F30Ace))
temp_cor[332,3] = test$estimate
temp_cor[332,4] = test$p.value

# Current February
test = my.cor.test(rwi$F30Ace, temp_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Feb[2:48]))
test
plot(rwi$F30Ace, temp_P$Feb[2:48])
abline(lm(temp_P$Feb[2:48] ~ rwi$F30Ace))
temp_cor[333,3] = test$estimate
temp_cor[333,4] = test$p.value

# Current March
test = my.cor.test(rwi$F30Ace, temp_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Mar[2:48]))
test
plot(rwi$F30Ace, temp_P$Mar[2:48])
abline(lm(temp_P$Mar[2:48] ~ rwi$F30Ace))
temp_cor[334,3] = test$estimate
temp_cor[334,4] = test$p.value

# Current April
test = my.cor.test(rwi$F30Ace, temp_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Apr[2:48]))
test
plot(rwi$F30Ace, temp_P$Apr[2:48])
abline(lm(temp_P$Apr[2:48] ~ rwi$F30Ace))
temp_cor[335,3] = test$estimate
temp_cor[335,4] = test$p.value

# Current May
test = my.cor.test(rwi$F30Ace, temp_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$May[2:48]))
test
plot(rwi$F30Ace, temp_P$May[2:48])
abline(lm(temp_P$May[2:48] ~ rwi$F30Ace))
temp_cor[336,3] = test$estimate
temp_cor[336,4] = test$p.value

# Current June
test = my.cor.test(rwi$F30Ace, temp_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Jun[2:48]))
test
plot(rwi$F30Ace, temp_P$Jun[2:48])
abline(lm(temp_P$Jun[2:48] ~ rwi$F30Ace))
temp_cor[337,3] = test$estimate
temp_cor[337,4] = test$p.value

# Current July
test = my.cor.test(rwi$F30Ace, temp_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Jul[2:48]))
test
plot(rwi$F30Ace, temp_P$Jul[2:48])
abline(lm(temp_P$Jul[2:48] ~ rwi$F30Ace))
temp_cor[338,3] = test$estimate
temp_cor[338,4] = test$p.value

# Current August
test = my.cor.test(rwi$F30Ace, temp_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Aug[2:48]))
test
plot(rwi$F30Ace, temp_P$Aug[2:48])
abline(lm(temp_P$Aug[2:48] ~ rwi$F30Ace))
temp_cor[339,3] = test$estimate
temp_cor[339,4] = test$p.value

# Current September
test = my.cor.test(rwi$F30Ace, temp_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Sep[2:48]))
test
plot(rwi$F30Ace, temp_P$Sep[2:48])
abline(lm(temp_P$Sep[2:48] ~ rwi$F30Ace))
temp_cor[340,3] = test$estimate
temp_cor[340,4] = test$p.value
#####

# Significance
temp_cor$sig_rwi <- ifelse(temp_cor$pval_rwi < 0.055, "Yes", "No")
temp_cor$sig_lts <- ifelse(temp_cor$pval_lts < 0.055, "Yes", "No")

#write.csv(temp_cor, "temp_cor_mn_20.csv")

##### Mean Temperature Figure #####
t_cor01.p = ggplot(data = subset(temp_cor, site == "01Tsu")) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("01Tsu (0.64**)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
t_cor01.p

t_cor02.p = ggplot(data = subset(temp_cor, site == "02Que")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(temp_cor, site == "02Ace"),
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(temp_cor, site == "02Ace"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(temp_cor, site == "02Ace"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >**02Que (0.28*)**</span> / 
                <span style= 'color:darkgreen;' >02Ace (-0.13)</span>") +
  #ggtitle("02Que (0.28*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
t_cor02.p

t_cor03.p = ggplot(data = subset(temp_cor, site == "03Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(temp_cor, site == "03Que"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(temp_cor, site == "03Que"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(temp_cor, site == "03Que"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >**03Ace (0.24*)**</span> / 
                <span style= 'color:darkgreen;' >03Que (0.02)</span>") + 
  #ggtitle("03Ace (0.24*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
t_cor03.p

t_cor04.p = ggplot(data = subset(temp_cor, site == "04Tsu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("04Tsu (0.16)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
t_cor04.p

t_cor05.p = ggplot(data = subset(temp_cor, site == "05Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("05Ace (-0.21)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
t_cor05.p

t_cor06.p = ggplot(data = subset(temp_cor, site == "06Dec")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("06Dec (0.37*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
t_cor06.p

t_cor07.p = ggplot(data = subset(temp_cor, site == "07Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("07Thu (0.37*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
t_cor07.p

t_cor08.p = ggplot(data = subset(temp_cor, site == "08Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("08Ace (-0.12)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
t_cor08.p

t_cor09.p = ggplot(data = subset(temp_cor, site == "09Pin")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("09Pin (0.20)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
t_cor09.p

t_cor10.p = ggplot(data = subset(temp_cor, site == "10Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(temp_cor, site == "10Dec"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(data = subset(temp_cor, site == "10Dec"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(data = subset(temp_cor, site == "10Dec"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:red3;' >**10Thu (0.55*)**</span> / 
                <span style= 'color:blue;' >10Dec (-0.17)</span>") + 
  #ggtitle("10Thu (0.55*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
t_cor10.p

t_cor11.p = ggplot(data = subset(temp_cor, site == "11Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("11Ace (-0.22)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
t_cor11.p

t_cor12.p = ggplot(data = subset(temp_cor, site == "12Pop")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("12Pop (0.57**)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
t_cor12.p

t_cor13.p = ggplot(data = subset(temp_cor, site == "13Pic")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("13Pic (0.31*)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = "Correlation with Monthly Mean Temperature", expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 12, hjust = -0.55, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
t_cor13.p

t_cor14.p = ggplot(data = subset(temp_cor, site == "14Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("14Thu (0.10)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
t_cor14.p

t_cor15.p = ggplot(data = subset(temp_cor, site == "15Bet")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(temp_cor, site == "15Ace"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(temp_cor, site == "15Ace"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(temp_cor, site == "15Ace"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >15Bet (-0.10)</span> / 
                <span style= 'color:darkgreen;' >15Ace (-0.11)</span>") + 
  #ggtitle("15Bet (-0.10)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                             "aug" = "a", "sep" = "s", "oct" = "o",
                                             "nov" = "n", "dec" = "d", "JAN" = "J",
                                             "FEB" = "F", "MAR" = "M", "APR" = "A",
                                             "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                             "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
t_cor15.p

t_cor16.p = ggplot(data = subset(temp_cor, site == "16Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("16Thu (0.43*)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
t_cor16.p
#####
tiff("temp_cor1_20.tiff", units = "in", width = 6.5, height = 6.5, res = 300)
(t_cor01.p | t_cor02.p | t_cor03.p | t_cor04.p) /
  (t_cor05.p | t_cor06.p | t_cor07.p | t_cor08.p) / 
  (t_cor09.p | t_cor10.p | t_cor11.p | t_cor12.p) /
  (t_cor13.p | t_cor14.p | t_cor15.p | t_cor16.p)
dev.off()

##### Mean Temperature Summary Figure #####
temp_cor_rwi_sum.p = ggplot(data = temp_cor) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_point(aes(x = temp, y = cor_rwi, col = type), alpha = 0.5) + # Background
  geom_point(aes(x = temp, y = cor_rwi, col = type, fill = sig_rwi), shape = 21) +
  stat_summary(aes(x = temp, y = cor_rwi, group = type, col = type), 
               geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_rwi, group = 1), geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_rwi, group = type, col = type), 
  #             geom = "point", fun = "mean", shape = 4, size = 4) +
  scale_y_continuous(name = "Correlation with Monthly Mean Temperature", 
                     expand = c(0,0), limits = c(-0.72,0.72), #0.76 for mn
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_color_manual(values = c("red3", "blue")) +
  scale_fill_manual(values = c("white", "#00000000")) + # NA no longer works for some reason
  ggtitle("A. RWI") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
temp_cor_rwi_sum.p

temp_cor_lts_sum.p = ggplot(data = temp_cor) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_point(aes(x = temp, y = cor_lts, col = type), alpha = 0.5) + # Background
  geom_point(aes(x = temp, y = cor_lts, col = type, fill = sig_lts), shape = 21) +
  stat_summary(aes(x = temp, y = cor_lts, group = type, col = type), 
               geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_lts, group = 1), geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_lts, group = type, col = type), 
  #             geom = "point", fun = "mean", shape = 4, size = 4) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), #0.76 for mn
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_color_manual(values = c("red3", "blue")) +
  scale_fill_manual(values = c("white", "#00000000")) +
  ggtitle("B. CC (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
temp_cor_lts_sum.p
#####
#tiff("temp_cor_sum1.tiff", units = "in", width = 6.5, height = 4, res = 300)
#temp_cor_rwi_sum.p | temp_cor_lts_sum.p
#dev.off()

##### Total Precipitation: Site 01 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M13Tsu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$May[1:47]))
test
plot(rwi$M13Tsu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M13Tsu))
precip_cor[1,3] = test$estimate
precip_cor[1,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M13Tsu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Jun[1:47]))
test
plot(rwi$M13Tsu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M13Tsu))
precip_cor[2,3] = test$estimate
precip_cor[2,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M13Tsu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Jul[1:47]))
test
plot(rwi$M13Tsu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M13Tsu))
precip_cor[3,3] = test$estimate
precip_cor[3,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M13Tsu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Aug[1:47]))
test
plot(rwi$M13Tsu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M13Tsu))
precip_cor[4,3] = test$estimate
precip_cor[4,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M13Tsu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Sep[1:47]))
test
plot(rwi$M13Tsu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M13Tsu))
precip_cor[5,3] = test$estimate
precip_cor[5,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M13Tsu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Oct[1:47]))
test
plot(rwi$M13Tsu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M13Tsu))
precip_cor[6,3] = test$estimate
precip_cor[6,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M13Tsu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Nov[1:47]))
test
plot(rwi$M13Tsu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M13Tsu))
precip_cor[7,3] = test$estimate
precip_cor[7,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M13Tsu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Dec[1:47]))
test
plot(rwi$M13Tsu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M13Tsu))
precip_cor[8,3] = test$estimate
precip_cor[8,4] = test$p.value

# Current January
test = my.cor.test(rwi$M13Tsu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Jan[2:48]))
test
plot(rwi$M13Tsu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M13Tsu))
precip_cor[9,3] = test$estimate
precip_cor[9,4] = test$p.value

# Current February
test = my.cor.test(rwi$M13Tsu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Feb[2:48]))
test
plot(rwi$M13Tsu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M13Tsu))
precip_cor[10,3] = test$estimate
precip_cor[10,4] = test$p.value

# Current March
test = my.cor.test(rwi$M13Tsu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Mar[2:48]))
test
plot(rwi$M13Tsu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M13Tsu))
precip_cor[11,3] = test$estimate
precip_cor[11,4] = test$p.value

# Current April
test = my.cor.test(rwi$M13Tsu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Apr[2:48]))
test
plot(rwi$M13Tsu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M13Tsu))
precip_cor[12,3] = test$estimate
precip_cor[12,4] = test$p.value

# Current May
test = my.cor.test(rwi$M13Tsu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$May[2:48]))
test
plot(rwi$M13Tsu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M13Tsu))
precip_cor[13,3] = test$estimate
precip_cor[13,4] = test$p.value

# Current June
test = my.cor.test(rwi$M13Tsu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Jun[2:48]))
test
plot(rwi$M13Tsu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M13Tsu))
precip_cor[14,3] = test$estimate
precip_cor[14,4] = test$p.value

# Current July
test = my.cor.test(rwi$M13Tsu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Jul[2:48]))
test
plot(rwi$M13Tsu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M13Tsu))
precip_cor[15,3] = test$estimate
precip_cor[15,4] = test$p.value

# Current August
test = my.cor.test(rwi$M13Tsu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Aug[2:48]))
test
plot(rwi$M13Tsu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M13Tsu))
precip_cor[16,3] = test$estimate
precip_cor[16,4] = test$p.value

# Current September
test = my.cor.test(rwi$M13Tsu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Sep[2:48]))
test
plot(rwi$M13Tsu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M13Tsu))
precip_cor[17,3] = test$estimate
precip_cor[17,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M13, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$May[1:47]))
test
plot(lts$M13, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M13))
precip_cor[1,6] = test$estimate
precip_cor[1,7] = test$p.value

# Previous June
test = my.cor.test(lts$M13, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Jun[1:47]))
test
plot(lts$M13, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M13))
precip_cor[2,6] = test$estimate
precip_cor[2,7] = test$p.value

# Previous July
test = my.cor.test(lts$M13, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Jul[1:47]))
test
plot(lts$M13, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M13))
precip_cor[3,6] = test$estimate
precip_cor[3,7] = test$p.value

# Previous August
test = my.cor.test(lts$M13, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Aug[1:47]))
test
plot(lts$M13, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M13))
precip_cor[4,6] = test$estimate
precip_cor[4,7] = test$p.value

# Previous September
test = my.cor.test(lts$M13, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Sep[1:47]))
test
plot(lts$M13, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M13))
precip_cor[5,6] = test$estimate
precip_cor[5,7] = test$p.value

# Previous October
test = my.cor.test(lts$M13, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Oct[1:47]))
test
plot(lts$M13, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M13))
precip_cor[6,6] = test$estimate
precip_cor[6,7] = test$p.value

# Previous November
test = my.cor.test(lts$M13, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Nov[1:47]))
test
plot(lts$M13, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M13))
precip_cor[7,6] = test$estimate
precip_cor[7,7] = test$p.value

# Previous December
test = my.cor.test(lts$M13, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Dec[1:47]))
test
plot(lts$M13, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M13))
precip_cor[8,6] = test$estimate
precip_cor[8,7] = test$p.value

# Current January
test = my.cor.test(lts$M13, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Jan[2:48]))
test
plot(lts$M13, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M13))
precip_cor[9,6] = test$estimate
precip_cor[9,7] = test$p.value

# Current February
test = my.cor.test(lts$M13, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Feb[2:48]))
test
plot(lts$M13, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M13))
precip_cor[10,6] = test$estimate
precip_cor[10,7] = test$p.value

# Current March
test = my.cor.test(lts$M13, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Mar[2:48]))
test
plot(lts$M13, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M13))
precip_cor[11,6] = test$estimate
precip_cor[11,7] = test$p.value

# Current April
test = my.cor.test(lts$M13, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Apr[2:48]))
test
plot(lts$M13, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M13))
precip_cor[12,6] = test$estimate
precip_cor[12,7] = test$p.value

# Current May
test = my.cor.test(lts$M13, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$May[2:48]))
test
plot(lts$M13, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M13))
precip_cor[13,6] = test$estimate
precip_cor[13,7] = test$p.value

# Current June
test = my.cor.test(lts$M13, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Jun[2:48]))
test
plot(lts$M13, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M13))
precip_cor[14,6] = test$estimate
precip_cor[14,7] = test$p.value

# Current July
test = my.cor.test(lts$M13, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Jul[2:48]))
test
plot(lts$M13, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M13))
precip_cor[15,6] = test$estimate
precip_cor[15,7] = test$p.value

# Current August
test = my.cor.test(lts$M13, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Aug[2:48]))
test
plot(lts$M13, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M13))
precip_cor[16,6] = test$estimate
precip_cor[16,7] = test$p.value

# Current September
test = my.cor.test(lts$M13, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Sep[2:48]))
test
plot(lts$M13, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M13))
precip_cor[17,6] = test$estimate
precip_cor[17,7] = test$p.value
#####

##### Total Precipitation: Site 02 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F15Que, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$May[1:47]))
test
plot(rwi$F15Que, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F15Que))
precip_cor[18,3] = test$estimate
precip_cor[18,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F15Que, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Jun[1:47]))
test
plot(rwi$F15Que, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F15Que))
precip_cor[19,3] = test$estimate
precip_cor[19,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F15Que, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Jul[1:47]))
test
plot(rwi$F15Que, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F15Que))
precip_cor[20,3] = test$estimate
precip_cor[20,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F15Que, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Aug[1:47]))
test
plot(rwi$F15Que, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F15Que))
precip_cor[21,3] = test$estimate
precip_cor[21,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F15Que, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Sep[1:47]))
test
plot(rwi$F15Que, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F15Que))
precip_cor[22,3] = test$estimate
precip_cor[22,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F15Que, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Oct[1:47]))
test
plot(rwi$F15Que, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F15Que))
precip_cor[23,3] = test$estimate
precip_cor[23,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F15Que, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Nov[1:47]))
test
plot(rwi$F15Que, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F15Que))
precip_cor[24,3] = test$estimate
precip_cor[24,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F15Que, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Dec[1:47]))
test
plot(rwi$F15Que, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F15Que))
precip_cor[25,3] = test$estimate
precip_cor[25,4] = test$p.value

# Current January
test = my.cor.test(rwi$F15Que, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Jan[2:48]))
test
plot(rwi$F15Que, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F15Que))
precip_cor[26,3] = test$estimate
precip_cor[26,4] = test$p.value

# Current February
test = my.cor.test(rwi$F15Que, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Feb[2:48]))
test
plot(rwi$F15Que, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F15Que))
precip_cor[27,3] = test$estimate
precip_cor[27,4] = test$p.value

# Current March
test = my.cor.test(rwi$F15Que, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Mar[2:48]))
test
plot(rwi$F15Que, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F15Que))
precip_cor[28,3] = test$estimate
precip_cor[28,4] = test$p.value

# Current April
test = my.cor.test(rwi$F15Que, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Apr[2:48]))
test
plot(rwi$F15Que, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F15Que))
precip_cor[29,3] = test$estimate
precip_cor[29,4] = test$p.value

# Current May
test = my.cor.test(rwi$F15Que, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$May[2:48]))
test
plot(rwi$F15Que, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F15Que))
precip_cor[30,3] = test$estimate
precip_cor[30,4] = test$p.value

# Current June
test = my.cor.test(rwi$F15Que, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Jun[2:48]))
test
plot(rwi$F15Que, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F15Que))
precip_cor[31,3] = test$estimate
precip_cor[31,4] = test$p.value

# Current July
test = my.cor.test(rwi$F15Que, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Jul[2:48]))
test
plot(rwi$F15Que, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F15Que))
precip_cor[32,3] = test$estimate
precip_cor[32,4] = test$p.value

# Current August
test = my.cor.test(rwi$F15Que, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Aug[2:48]))
test
plot(rwi$F15Que, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F15Que))
precip_cor[33,3] = test$estimate
precip_cor[33,4] = test$p.value

# Current September
test = my.cor.test(rwi$F15Que, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Sep[2:48]))
test
plot(rwi$F15Que, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F15Que))
precip_cor[34,3] = test$estimate
precip_cor[34,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F15, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$May[1:47]))
test
plot(lts$F15, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F15))
precip_cor[18,6] = test$estimate
precip_cor[18,7] = test$p.value

# Previous June
test = my.cor.test(lts$F15, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Jun[1:47]))
test
plot(lts$F15, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F15))
precip_cor[19,6] = test$estimate
precip_cor[19,7] = test$p.value

# Previous July
test = my.cor.test(lts$F15, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Jul[1:47]))
test
plot(lts$F15, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F15))
precip_cor[20,6] = test$estimate
precip_cor[20,7] = test$p.value

# Previous August
test = my.cor.test(lts$F15, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Aug[1:47]))
test
plot(lts$F15, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F15))
precip_cor[21,6] = test$estimate
precip_cor[21,7] = test$p.value

# Previous September
test = my.cor.test(lts$F15, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Sep[1:47]))
test
plot(lts$F15, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F15))
precip_cor[22,6] = test$estimate
precip_cor[22,7] = test$p.value

# Previous October
test = my.cor.test(lts$F15, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Oct[1:47]))
test
plot(lts$F15, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F15))
precip_cor[23,6] = test$estimate
precip_cor[23,7] = test$p.value

# Previous November
test = my.cor.test(lts$F15, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Nov[1:47]))
test
plot(lts$F15, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F15))
precip_cor[24,6] = test$estimate
precip_cor[24,7] = test$p.value

# Previous December
test = my.cor.test(lts$F15, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Dec[1:47]))
test
plot(lts$F15, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F15))
precip_cor[25,6] = test$estimate
precip_cor[25,7] = test$p.value

# Current January
test = my.cor.test(lts$F15, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Jan[2:48]))
test
plot(lts$F15, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F15))
precip_cor[26,6] = test$estimate
precip_cor[26,7] = test$p.value

# Current February
test = my.cor.test(lts$F15, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Feb[2:48]))
test
plot(lts$F15, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F15))
precip_cor[27,6] = test$estimate
precip_cor[27,7] = test$p.value

# Current March
test = my.cor.test(lts$F15, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Mar[2:48]))
test
plot(lts$F15, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F15))
precip_cor[28,6] = test$estimate
precip_cor[28,7] = test$p.value

# Current April
test = my.cor.test(lts$F15, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Apr[2:48]))
test
plot(lts$F15, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F15))
precip_cor[29,6] = test$estimate
precip_cor[29,7] = test$p.value

# Current May
test = my.cor.test(lts$F15, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$May[2:48]))
test
plot(lts$F15, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F15))
precip_cor[30,6] = test$estimate
precip_cor[30,7] = test$p.value

# Current June
test = my.cor.test(lts$F15, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Jun[2:48]))
test
plot(lts$F15, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F15))
precip_cor[31,6] = test$estimate
precip_cor[31,7] = test$p.value

# Current July
test = my.cor.test(lts$F15, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Jul[2:48]))
test
plot(lts$F15, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F15))
precip_cor[32,6] = test$estimate
precip_cor[32,7] = test$p.value

# Current August
test = my.cor.test(lts$F15, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Aug[2:48]))
test
plot(lts$F15, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F15))
precip_cor[33,6] = test$estimate
precip_cor[33,7] = test$p.value

# Current September
test = my.cor.test(lts$F15, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F15, precip_P$Sep[2:48]))
test
plot(lts$F15, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F15))
precip_cor[34,6] = test$estimate
precip_cor[34,7] = test$p.value
#####

##### Total Precipitatione: Site 03 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M06Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$May[1:47]))
test
plot(rwi$M06Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M06Ace))
precip_cor[35,3] = test$estimate
precip_cor[35,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M06Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Jun[1:47]))
test
plot(rwi$M06Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M06Ace))
precip_cor[36,3] = test$estimate
precip_cor[36,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M06Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Jul[1:47]))
test
plot(rwi$M06Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M06Ace))
precip_cor[37,3] = test$estimate
precip_cor[37,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M06Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Aug[1:47]))
test
plot(rwi$M06Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M06Ace))
precip_cor[38,3] = test$estimate
precip_cor[38,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M06Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Sep[1:47]))
test
plot(rwi$M06Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M06Ace))
precip_cor[39,3] = test$estimate
precip_cor[39,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M06Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Oct[1:47]))
test
plot(rwi$M06Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M06Ace))
precip_cor[40,3] = test$estimate
precip_cor[40,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M06Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Nov[1:47]))
test
plot(rwi$M06Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M06Ace))
precip_cor[41,3] = test$estimate
precip_cor[41,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M06Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Dec[1:47]))
test
plot(rwi$M06Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M06Ace))
precip_cor[42,3] = test$estimate
precip_cor[42,4] = test$p.value

# Current January
test = my.cor.test(rwi$M06Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Jan[2:48]))
test
plot(rwi$M06Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M06Ace))
precip_cor[43,3] = test$estimate
precip_cor[43,4] = test$p.value

# Current February
test = my.cor.test(rwi$M06Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Feb[2:48]))
test
plot(rwi$M06Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M06Ace))
precip_cor[44,3] = test$estimate
precip_cor[44,4] = test$p.value

# Current March
test = my.cor.test(rwi$M06Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Mar[2:48]))
test
plot(rwi$M06Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M06Ace))
precip_cor[45,3] = test$estimate
precip_cor[45,4] = test$p.value

# Current April
test = my.cor.test(rwi$M06Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Apr[2:48]))
test
plot(rwi$M06Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M06Ace))
precip_cor[46,3] = test$estimate
precip_cor[46,4] = test$p.value

# Current May
test = my.cor.test(rwi$M06Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$May[2:48]))
test
plot(rwi$M06Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M06Ace))
precip_cor[47,3] = test$estimate
precip_cor[47,4] = test$p.value

# Current June
test = my.cor.test(rwi$M06Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Jun[2:48]))
test
plot(rwi$M06Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M06Ace))
precip_cor[48,3] = test$estimate
precip_cor[48,4] = test$p.value

# Current July
test = my.cor.test(rwi$M06Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Jul[2:48]))
test
plot(rwi$M06Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M06Ace))
precip_cor[49,3] = test$estimate
precip_cor[49,4] = test$p.value

# Current August
test = my.cor.test(rwi$M06Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Aug[2:48]))
test
plot(rwi$M06Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M06Ace))
precip_cor[50,3] = test$estimate
precip_cor[50,4] = test$p.value

# Current September
test = my.cor.test(rwi$M06Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Sep[2:48]))
test
plot(rwi$M06Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M06Ace))
precip_cor[51,3] = test$estimate
precip_cor[51,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M06, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$May[1:47]))
test
plot(lts$M06, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M06))
precip_cor[35,6] = test$estimate
precip_cor[35,7] = test$p.value

# Previous June
test = my.cor.test(lts$M06, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Jun[1:47]))
test
plot(lts$M06, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M06))
precip_cor[36,6] = test$estimate
precip_cor[36,7] = test$p.value

# Previous July
test = my.cor.test(lts$M06, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Jul[1:47]))
test
plot(lts$M06, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M06))
precip_cor[37,6] = test$estimate
precip_cor[37,7] = test$p.value

# Previous August
test = my.cor.test(lts$M06, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Aug[1:47]))
test
plot(lts$M06, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M06))
precip_cor[38,6] = test$estimate
precip_cor[38,7] = test$p.value

# Previous September
test = my.cor.test(lts$M06, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Sep[1:47]))
test
plot(lts$M06, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M06))
precip_cor[39,6] = test$estimate
precip_cor[39,7] = test$p.value

# Previous October
test = my.cor.test(lts$M06, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Oct[1:47]))
test
plot(lts$M06, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M06))
precip_cor[40,6] = test$estimate
precip_cor[40,7] = test$p.value

# Previous November
test = my.cor.test(lts$M06, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Nov[1:47]))
test
plot(lts$M06, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M06))
precip_cor[41,6] = test$estimate
precip_cor[41,7] = test$p.value

# Previous December
test = my.cor.test(lts$M06, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Dec[1:47]))
test
plot(lts$M06, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M06))
precip_cor[42,6] = test$estimate
precip_cor[42,7] = test$p.value

# Current January
test = my.cor.test(lts$M06, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Jan[2:48]))
test
plot(lts$M06, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M06))
precip_cor[43,6] = test$estimate
precip_cor[43,7] = test$p.value

# Current February
test = my.cor.test(lts$M06, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Feb[2:48]))
test
plot(lts$M06, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M06))
precip_cor[44,6] = test$estimate
precip_cor[44,7] = test$p.value

# Current March
test = my.cor.test(lts$M06, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Mar[2:48]))
test
plot(lts$M06, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M06))
precip_cor[45,6] = test$estimate
precip_cor[45,7] = test$p.value

# Current April
test = my.cor.test(lts$M06, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Apr[2:48]))
test
plot(lts$M06, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M06))
precip_cor[46,6] = test$estimate
precip_cor[46,7] = test$p.value

# Current May
test = my.cor.test(lts$M06, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$May[2:48]))
test
plot(lts$M06, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M06))
precip_cor[47,6] = test$estimate
precip_cor[47,7] = test$p.value

# Current June
test = my.cor.test(lts$M06, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Jun[2:48]))
test
plot(lts$M06, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M06))
precip_cor[48,6] = test$estimate
precip_cor[48,7] = test$p.value

# Current July
test = my.cor.test(lts$M06, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Jul[2:48]))
test
plot(lts$M06, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M06))
precip_cor[49,6] = test$estimate
precip_cor[49,7] = test$p.value

# Current August
test = my.cor.test(lts$M06, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Aug[2:48]))
test
plot(lts$M06, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M06))
precip_cor[50,6] = test$estimate
precip_cor[50,7] = test$p.value

# Current September
test = my.cor.test(lts$M06, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M06, precip_P$Sep[2:48]))
test
plot(lts$M06, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M06))
precip_cor[51,6] = test$estimate
precip_cor[51,7] = test$p.value
#####

##### Total Precipitation: Site 04 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M07Tsu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$May[1:47]))
test
plot(rwi$M07Tsu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M07Tsu))
precip_cor[52,3] = test$estimate
precip_cor[52,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M07Tsu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Jun[1:47]))
test
plot(rwi$M07Tsu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M07Tsu))
precip_cor[53,3] = test$estimate
precip_cor[53,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M07Tsu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Jul[1:47]))
test
plot(rwi$M07Tsu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M07Tsu))
precip_cor[54,3] = test$estimate
precip_cor[54,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M07Tsu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Aug[1:47]))
test
plot(rwi$M07Tsu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M07Tsu))
precip_cor[55,3] = test$estimate
precip_cor[55,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M07Tsu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Sep[1:47]))
test
plot(rwi$M07Tsu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M07Tsu))
precip_cor[56,3] = test$estimate
precip_cor[56,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M07Tsu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Oct[1:47]))
test
plot(rwi$M07Tsu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M07Tsu))
precip_cor[57,3] = test$estimate
precip_cor[57,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M07Tsu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Nov[1:47]))
test
plot(rwi$M07Tsu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M07Tsu))
precip_cor[58,3] = test$estimate
precip_cor[58,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M07Tsu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Dec[1:47]))
test
plot(rwi$M07Tsu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M07Tsu))
precip_cor[59,3] = test$estimate
precip_cor[59,4] = test$p.value

# Current January
test = my.cor.test(rwi$M07Tsu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Jan[2:48]))
test
plot(rwi$M07Tsu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M07Tsu))
precip_cor[60,3] = test$estimate
precip_cor[60,4] = test$p.value

# Current February
test = my.cor.test(rwi$M07Tsu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Feb[2:48]))
test
plot(rwi$M07Tsu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M07Tsu))
precip_cor[61,3] = test$estimate
precip_cor[61,4] = test$p.value

# Current March
test = my.cor.test(rwi$M07Tsu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Mar[2:48]))
test
plot(rwi$M07Tsu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M07Tsu))
precip_cor[62,3] = test$estimate
precip_cor[62,4] = test$p.value

# Current April
test = my.cor.test(rwi$M07Tsu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Apr[2:48]))
test
plot(rwi$M07Tsu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M07Tsu))
precip_cor[63,3] = test$estimate
precip_cor[63,4] = test$p.value

# Current May
test = my.cor.test(rwi$M07Tsu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$May[2:48]))
test
plot(rwi$M07Tsu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M07Tsu))
precip_cor[64,3] = test$estimate
precip_cor[64,4] = test$p.value

# Current June
test = my.cor.test(rwi$M07Tsu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Jun[2:48]))
test
plot(rwi$M07Tsu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M07Tsu))
precip_cor[65,3] = test$estimate
precip_cor[65,4] = test$p.value

# Current July
test = my.cor.test(rwi$M07Tsu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Jul[2:48]))
test
plot(rwi$M07Tsu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M07Tsu))
precip_cor[66,3] = test$estimate
precip_cor[66,4] = test$p.value

# Current August
test = my.cor.test(rwi$M07Tsu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Aug[2:48]))
test
plot(rwi$M07Tsu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M07Tsu))
precip_cor[67,3] = test$estimate
precip_cor[67,4] = test$p.value

# Current September
test = my.cor.test(rwi$M07Tsu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Sep[2:48]))
test
plot(rwi$M07Tsu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M07Tsu))
precip_cor[68,3] = test$estimate
precip_cor[68,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M07, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$May[1:47]))
test
plot(lts$M07, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M07))
precip_cor[52,6] = test$estimate
precip_cor[52,7] = test$p.value

# Previous June
test = my.cor.test(lts$M07, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Jun[1:47]))
test
plot(lts$M07, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M07))
precip_cor[53,6] = test$estimate
precip_cor[53,7] = test$p.value

# Previous July
test = my.cor.test(lts$M07, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Jul[1:47]))
test
plot(lts$M07, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M07))
precip_cor[54,6] = test$estimate
precip_cor[54,7] = test$p.value

# Previous August
test = my.cor.test(lts$M07, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Aug[1:47]))
test
plot(lts$M07, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M07))
precip_cor[55,6] = test$estimate
precip_cor[55,7] = test$p.value

# Previous September
test = my.cor.test(lts$M07, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Sep[1:47]))
test
plot(lts$M07, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M07))
precip_cor[56,6] = test$estimate
precip_cor[56,7] = test$p.value

# Previous October
test = my.cor.test(lts$M07, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Oct[1:47]))
test
plot(lts$M07, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M07))
precip_cor[57,6] = test$estimate
precip_cor[57,7] = test$p.value

# Previous November
test = my.cor.test(lts$M07, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Nov[1:47]))
test
plot(lts$M07, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M07))
precip_cor[58,6] = test$estimate
precip_cor[58,7] = test$p.value

# Previous December
test = my.cor.test(lts$M07, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Dec[1:47]))
test
plot(lts$M07, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M07))
precip_cor[59,6] = test$estimate
precip_cor[59,7] = test$p.value

# Current January
test = my.cor.test(lts$M07, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Jan[2:48]))
test
plot(lts$M07, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M07))
precip_cor[60,6] = test$estimate
precip_cor[60,7] = test$p.value

# Current February
test = my.cor.test(lts$M07, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Feb[2:48]))
test
plot(lts$M07, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M07))
precip_cor[61,6] = test$estimate
precip_cor[61,7] = test$p.value

# Current March
test = my.cor.test(lts$M07, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Mar[2:48]))
test
plot(lts$M07, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M07))
precip_cor[62,6] = test$estimate
precip_cor[62,7] = test$p.value

# Current April
test = my.cor.test(lts$M07, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Apr[2:48]))
test
plot(lts$M07, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M07))
precip_cor[63,6] = test$estimate
precip_cor[63,7] = test$p.value

# Current May
test = my.cor.test(lts$M07, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$May[2:48]))
test
plot(lts$M07, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M07))
precip_cor[64,6] = test$estimate
precip_cor[64,7] = test$p.value

# Current June
test = my.cor.test(lts$M07, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Jun[2:48]))
test
plot(lts$M07, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M07))
precip_cor[65,6] = test$estimate
precip_cor[65,7] = test$p.value

# Current July
test = my.cor.test(lts$M07, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Jul[2:48]))
test
plot(lts$M07, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M07))
precip_cor[66,6] = test$estimate
precip_cor[66,7] = test$p.value

# Current August
test = my.cor.test(lts$M07, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Aug[2:48]))
test
plot(lts$M07, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M07))
precip_cor[67,6] = test$estimate
precip_cor[67,7] = test$p.value

# Current September
test = my.cor.test(lts$M07, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M07, precip_P$Sep[2:48]))
test
plot(lts$M07, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M07))
precip_cor[68,6] = test$estimate
precip_cor[68,7] = test$p.value
#####

##### Total Precipitation: Site 05 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F23Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$May[1:47]))
test
plot(rwi$F23Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F23Ace))
precip_cor[69,3] = test$estimate
precip_cor[69,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F23Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Jun[1:47]))
test
plot(rwi$F23Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F23Ace))
precip_cor[70,3] = test$estimate
precip_cor[70,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F23Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Jul[1:47]))
test
plot(rwi$F23Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F23Ace))
precip_cor[71,3] = test$estimate
precip_cor[71,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F23Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Aug[1:47]))
test
plot(rwi$F23Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F23Ace))
precip_cor[72,3] = test$estimate
precip_cor[72,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F23Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Sep[1:47]))
test
plot(rwi$F23Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F23Ace))
precip_cor[73,3] = test$estimate
precip_cor[73,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F23Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Oct[1:47]))
test
plot(rwi$F23Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F23Ace))
precip_cor[74,3] = test$estimate
precip_cor[74,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F23Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Nov[1:47]))
test
plot(rwi$F23Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F23Ace))
precip_cor[75,3] = test$estimate
precip_cor[75,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F23Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Dec[1:47]))
test
plot(rwi$F23Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F23Ace))
precip_cor[76,3] = test$estimate
precip_cor[76,4] = test$p.value

# Current January
test = my.cor.test(rwi$F23Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Jan[2:48]))
test
plot(rwi$F23Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F23Ace))
precip_cor[77,3] = test$estimate
precip_cor[77,4] = test$p.value

# Current February
test = my.cor.test(rwi$F23Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Feb[2:48]))
test
plot(rwi$F23Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F23Ace))
precip_cor[78,3] = test$estimate
precip_cor[78,4] = test$p.value

# Current March
test = my.cor.test(rwi$F23Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Mar[2:48]))
test
plot(rwi$F23Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F23Ace))
precip_cor[79,3] = test$estimate
precip_cor[79,4] = test$p.value

# Current April
test = my.cor.test(rwi$F23Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Apr[2:48]))
test
plot(rwi$F23Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F23Ace))
precip_cor[80,3] = test$estimate
precip_cor[80,4] = test$p.value

# Current May
test = my.cor.test(rwi$F23Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$May[2:48]))
test
plot(rwi$F23Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F23Ace))
precip_cor[81,3] = test$estimate
precip_cor[81,4] = test$p.value

# Current June
test = my.cor.test(rwi$F23Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Jun[2:48]))
test
plot(rwi$F23Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F23Ace))
precip_cor[82,3] = test$estimate
precip_cor[82,4] = test$p.value

# Current July
test = my.cor.test(rwi$F23Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Jul[2:48]))
test
plot(rwi$F23Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F23Ace))
precip_cor[83,3] = test$estimate
precip_cor[83,4] = test$p.value

# Current August
test = my.cor.test(rwi$F23Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Aug[2:48]))
test
plot(rwi$F23Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F23Ace))
precip_cor[84,3] = test$estimate
precip_cor[84,4] = test$p.value

# Current September
test = my.cor.test(rwi$F23Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Sep[2:48]))
test
plot(rwi$F23Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F23Ace))
precip_cor[85,3] = test$estimate
precip_cor[85,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F23, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$May[1:47]))
test
plot(lts$F23, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F23))
precip_cor[69,6] = test$estimate
precip_cor[69,7] = test$p.value

# Previous June
test = my.cor.test(lts$F23, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Jun[1:47]))
test
plot(lts$F23, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F23))
precip_cor[70,6] = test$estimate
precip_cor[70,7] = test$p.value

# Previous July
test = my.cor.test(lts$F23, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Jul[1:47]))
test
plot(lts$F23, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F23))
precip_cor[71,6] = test$estimate
precip_cor[71,7] = test$p.value

# Previous August
test = my.cor.test(lts$F23, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Aug[1:47]))
test
plot(lts$F23, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F23))
precip_cor[72,6] = test$estimate
precip_cor[72,7] = test$p.value

# Previous September
test = my.cor.test(lts$F23, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Sep[1:47]))
test
plot(lts$F23, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F23))
precip_cor[73,6] = test$estimate
precip_cor[73,7] = test$p.value

# Previous October
test = my.cor.test(lts$F23, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Oct[1:47]))
test
plot(lts$F23, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F23))
precip_cor[74,6] = test$estimate
precip_cor[74,7] = test$p.value

# Previous November
test = my.cor.test(lts$F23, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Nov[1:47]))
test
plot(lts$F23, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F23))
precip_cor[75,6] = test$estimate
precip_cor[75,7] = test$p.value

# Previous December
test = my.cor.test(lts$F23, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Dec[1:47]))
test
plot(lts$F23, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F23))
precip_cor[76,6] = test$estimate
precip_cor[76,7] = test$p.value

# Current January
test = my.cor.test(lts$F23, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Jan[2:48]))
test
plot(lts$F23, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F23))
precip_cor[77,6] = test$estimate
precip_cor[77,7] = test$p.value

# Current February
test = my.cor.test(lts$F23, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Feb[2:48]))
test
plot(lts$F23, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F23))
precip_cor[78,6] = test$estimate
precip_cor[78,7] = test$p.value

# Current March
test = my.cor.test(lts$F23, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Mar[2:48]))
test
plot(lts$F23, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F23))
precip_cor[79,6] = test$estimate
precip_cor[79,7] = test$p.value

# Current April
test = my.cor.test(lts$F23, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Apr[2:48]))
test
plot(lts$F23, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F23))
precip_cor[80,6] = test$estimate
precip_cor[80,7] = test$p.value

# Current May
test = my.cor.test(lts$F23, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$May[2:48]))
test
plot(lts$F23, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F23))
precip_cor[81,6] = test$estimate
precip_cor[81,7] = test$p.value

# Current June
test = my.cor.test(lts$F23, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Jun[2:48]))
test
plot(lts$F23, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F23))
precip_cor[82,6] = test$estimate
precip_cor[82,7] = test$p.value

# Current July
test = my.cor.test(lts$F23, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Jul[2:48]))
test
plot(lts$F23, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F23))
precip_cor[83,6] = test$estimate
precip_cor[83,7] = test$p.value

# Current August
test = my.cor.test(lts$F23, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Aug[2:48]))
test
plot(lts$F23, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F23))
precip_cor[84,6] = test$estimate
precip_cor[84,7] = test$p.value

# Current September
test = my.cor.test(lts$F23, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F23, precip_P$Sep[2:48]))
test
plot(lts$F23, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F23))
precip_cor[85,6] = test$estimate
precip_cor[85,7] = test$p.value
#####

##### Total Precipitation: Site 06 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F21Dec, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$May[1:47]))
test
plot(rwi$F21Dec, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F21Dec))
precip_cor[86,3] = test$estimate
precip_cor[86,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F21Dec, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Jun[1:47]))
test
plot(rwi$F21Dec, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F21Dec))
precip_cor[87,3] = test$estimate
precip_cor[87,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F21Dec, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Jul[1:47]))
test
plot(rwi$F21Dec, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F21Dec))
precip_cor[88,3] = test$estimate
precip_cor[88,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F21Dec, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Aug[1:47]))
test
plot(rwi$F21Dec, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F21Dec))
precip_cor[89,3] = test$estimate
precip_cor[89,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F21Dec, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Sep[1:47]))
test
plot(rwi$F21Dec, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F21Dec))
precip_cor[90,3] = test$estimate
precip_cor[90,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F21Dec, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Oct[1:47]))
test
plot(rwi$F21Dec, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F21Dec))
precip_cor[91,3] = test$estimate
precip_cor[91,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F21Dec, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Nov[1:47]))
test
plot(rwi$F21Dec, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F21Dec))
precip_cor[92,3] = test$estimate
precip_cor[92,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F21Dec, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Dec[1:47]))
test
plot(rwi$F21Dec, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F21Dec))
precip_cor[93,3] = test$estimate
precip_cor[93,4] = test$p.value

# Current January
test = my.cor.test(rwi$F21Dec, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Jan[2:48]))
test
plot(rwi$F21Dec, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F21Dec))
precip_cor[94,3] = test$estimate
precip_cor[94,4] = test$p.value

# Current February
test = my.cor.test(rwi$F21Dec, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Feb[2:48]))
test
plot(rwi$F21Dec, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F21Dec))
precip_cor[95,3] = test$estimate
precip_cor[95,4] = test$p.value

# Current March
test = my.cor.test(rwi$F21Dec, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Mar[2:48]))
test
plot(rwi$F21Dec, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F21Dec))
precip_cor[96,3] = test$estimate
precip_cor[96,4] = test$p.value

# Current April
test = my.cor.test(rwi$F21Dec, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Apr[2:48]))
test
plot(rwi$F21Dec, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F21Dec))
precip_cor[97,3] = test$estimate
precip_cor[97,4] = test$p.value

# Current May
test = my.cor.test(rwi$F21Dec, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$May[2:48]))
test
plot(rwi$F21Dec, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F21Dec))
precip_cor[98,3] = test$estimate
precip_cor[98,4] = test$p.value

# Current June
test = my.cor.test(rwi$F21Dec, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Jun[2:48]))
test
plot(rwi$F21Dec, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F21Dec))
precip_cor[99,3] = test$estimate
precip_cor[99,4] = test$p.value

# Current July
test = my.cor.test(rwi$F21Dec, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Jul[2:48]))
test
plot(rwi$F21Dec, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F21Dec))
precip_cor[100,3] = test$estimate
precip_cor[100,4] = test$p.value

# Current August
test = my.cor.test(rwi$F21Dec, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Aug[2:48]))
test
plot(rwi$F21Dec, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F21Dec))
precip_cor[101,3] = test$estimate
precip_cor[101,4] = test$p.value

# Current September
test = my.cor.test(rwi$F21Dec, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Sep[2:48]))
test
plot(rwi$F21Dec, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F21Dec))
precip_cor[102,3] = test$estimate
precip_cor[102,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F21, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$May[1:47]))
test
plot(lts$F21, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F21))
precip_cor[86,6] = test$estimate
precip_cor[86,7] = test$p.value

# Previous June
test = my.cor.test(lts$F21, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Jun[1:47]))
test
plot(lts$F21, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F21))
precip_cor[87,6] = test$estimate
precip_cor[87,7] = test$p.value

# Previous July
test = my.cor.test(lts$F21, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Jul[1:47]))
test
plot(lts$F21, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F21))
precip_cor[88,6] = test$estimate
precip_cor[88,7] = test$p.value

# Previous August
test = my.cor.test(lts$F21, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Aug[1:47]))
test
plot(lts$F21, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F21))
precip_cor[89,6] = test$estimate
precip_cor[89,7] = test$p.value

# Previous September
test = my.cor.test(lts$F21, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Sep[1:47]))
test
plot(lts$F21, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F21))
precip_cor[90,6] = test$estimate
precip_cor[90,7] = test$p.value

# Previous October
test = my.cor.test(lts$F21, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Oct[1:47]))
test
plot(lts$F21, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F21))
precip_cor[91,6] = test$estimate
precip_cor[91,7] = test$p.value

# Previous November
test = my.cor.test(lts$F21, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Nov[1:47]))
test
plot(lts$F21, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F21))
precip_cor[92,6] = test$estimate
precip_cor[92,7] = test$p.value

# Previous December
test = my.cor.test(lts$F21, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Dec[1:47]))
test
plot(lts$F21, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F21))
precip_cor[93,6] = test$estimate
precip_cor[93,7] = test$p.value

# Current January
test = my.cor.test(lts$F21, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Jan[2:48]))
test
plot(lts$F21, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F21))
precip_cor[94,6] = test$estimate
precip_cor[94,7] = test$p.value

# Current February
test = my.cor.test(lts$F21, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Feb[2:48]))
test
plot(lts$F21, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F21))
precip_cor[95,6] = test$estimate
precip_cor[95,7] = test$p.value

# Current March
test = my.cor.test(lts$F21, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Mar[2:48]))
test
plot(lts$F21, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F21))
precip_cor[96,6] = test$estimate
precip_cor[96,7] = test$p.value

# Current April
test = my.cor.test(lts$F21, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Apr[2:48]))
test
plot(lts$F21, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F21))
precip_cor[97,6] = test$estimate
precip_cor[97,7] = test$p.value

# Current May
test = my.cor.test(lts$F21, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$May[2:48]))
test
plot(lts$F21, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F21))
precip_cor[98,6] = test$estimate
precip_cor[98,7] = test$p.value

# Current June
test = my.cor.test(lts$F21, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Jun[2:48]))
test
plot(lts$F21, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F21))
precip_cor[99,6] = test$estimate
precip_cor[99,7] = test$p.value

# Current July
test = my.cor.test(lts$F21, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Jul[2:48]))
test
plot(lts$F21, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F21))
precip_cor[100,6] = test$estimate
precip_cor[100,7] = test$p.value

# Current August
test = my.cor.test(lts$F21, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Aug[2:48]))
test
plot(lts$F21, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F21))
precip_cor[101,6] = test$estimate
precip_cor[101,7] = test$p.value

# Current September
test = my.cor.test(lts$F21, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F21, precip_P$Sep[2:48]))
test
plot(lts$F21, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F21))
precip_cor[102,6] = test$estimate
precip_cor[102,7] = test$p.value
#####

##### Total Precipitation: Site 07 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M05Thu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$May[1:47]))
test
plot(rwi$M05Thu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M05Thu))
precip_cor[103,3] = test$estimate
precip_cor[103,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M05Thu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Jun[1:47]))
test
plot(rwi$M05Thu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M05Thu))
precip_cor[104,3] = test$estimate
precip_cor[104,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M05Thu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Jul[1:47]))
test
plot(rwi$M05Thu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M05Thu))
precip_cor[105,3] = test$estimate
precip_cor[105,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M05Thu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Aug[1:47]))
test
plot(rwi$M05Thu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M05Thu))
precip_cor[106,3] = test$estimate
precip_cor[106,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M05Thu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Sep[1:47]))
test
plot(rwi$M05Thu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M05Thu))
precip_cor[107,3] = test$estimate
precip_cor[107,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M05Thu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Oct[1:47]))
test
plot(rwi$M05Thu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M05Thu))
precip_cor[108,3] = test$estimate
precip_cor[108,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M05Thu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Nov[1:47]))
test
plot(rwi$M05Thu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M05Thu))
precip_cor[109,3] = test$estimate
precip_cor[109,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M05Thu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Dec[1:47]))
test
plot(rwi$M05Thu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M05Thu))
precip_cor[110,3] = test$estimate
precip_cor[110,4] = test$p.value

# Current January
test = my.cor.test(rwi$M05Thu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Jan[2:48]))
test
plot(rwi$M05Thu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M05Thu))
precip_cor[111,3] = test$estimate
precip_cor[111,4] = test$p.value

# Current February
test = my.cor.test(rwi$M05Thu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Feb[2:48]))
test
plot(rwi$M05Thu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M05Thu))
precip_cor[112,3] = test$estimate
precip_cor[112,4] = test$p.value

# Current March
test = my.cor.test(rwi$M05Thu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Mar[2:48]))
test
plot(rwi$M05Thu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M05Thu))
precip_cor[113,3] = test$estimate
precip_cor[113,4] = test$p.value

# Current April
test = my.cor.test(rwi$M05Thu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Apr[2:48]))
test
plot(rwi$M05Thu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M05Thu))
precip_cor[114,3] = test$estimate
precip_cor[114,4] = test$p.value

# Current May
test = my.cor.test(rwi$M05Thu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$May[2:48]))
test
plot(rwi$M05Thu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M05Thu))
precip_cor[115,3] = test$estimate
precip_cor[115,4] = test$p.value

# Current June
test = my.cor.test(rwi$M05Thu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Jun[2:48]))
test
plot(rwi$M05Thu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M05Thu))
precip_cor[116,3] = test$estimate
precip_cor[116,4] = test$p.value

# Current July
test = my.cor.test(rwi$M05Thu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Jul[2:48]))
test
plot(rwi$M05Thu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M05Thu))
precip_cor[117,3] = test$estimate
precip_cor[117,4] = test$p.value

# Current August
test = my.cor.test(rwi$M05Thu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Aug[2:48]))
test
plot(rwi$M05Thu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M05Thu))
precip_cor[118,3] = test$estimate
precip_cor[118,4] = test$p.value

# Current September
test = my.cor.test(rwi$M05Thu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Sep[2:48]))
test
plot(rwi$M05Thu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M05Thu))
precip_cor[119,3] = test$estimate
precip_cor[119,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M05, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$May[1:47]))
test
plot(lts$M05, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M05))
precip_cor[103,6] = test$estimate
precip_cor[103,7] = test$p.value

# Previous June
test = my.cor.test(lts$M05, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Jun[1:47]))
test
plot(lts$M05, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M05))
precip_cor[104,6] = test$estimate
precip_cor[104,7] = test$p.value

# Previous July
test = my.cor.test(lts$M05, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Jul[1:47]))
test
plot(lts$M05, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M05))
precip_cor[105,6] = test$estimate
precip_cor[105,7] = test$p.value

# Previous August
test = my.cor.test(lts$M05, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Aug[1:47]))
test
plot(lts$M05, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M05))
precip_cor[106,6] = test$estimate
precip_cor[106,7] = test$p.value

# Previous September
test = my.cor.test(lts$M05, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Sep[1:47]))
test
plot(lts$M05, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M05))
precip_cor[107,6] = test$estimate
precip_cor[107,7] = test$p.value

# Previous October
test = my.cor.test(lts$M05, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Oct[1:47]))
test
plot(lts$M05, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M05))
precip_cor[108,6] = test$estimate
precip_cor[108,7] = test$p.value

# Previous November
test = my.cor.test(lts$M05, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Nov[1:47]))
test
plot(lts$M05, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M05))
precip_cor[109,6] = test$estimate
precip_cor[109,7] = test$p.value

# Previous December
test = my.cor.test(lts$M05, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Dec[1:47]))
test
plot(lts$M05, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M05))
precip_cor[110,6] = test$estimate
precip_cor[110,7] = test$p.value

# Current January
test = my.cor.test(lts$M05, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Jan[2:48]))
test
plot(lts$M05, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M05))
precip_cor[111,6] = test$estimate
precip_cor[111,7] = test$p.value

# Current February
test = my.cor.test(lts$M05, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Feb[2:48]))
test
plot(lts$M05, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M05))
precip_cor[112,6] = test$estimate
precip_cor[112,7] = test$p.value

# Current March
test = my.cor.test(lts$M05, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Mar[2:48]))
test
plot(lts$M05, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M05))
precip_cor[113,6] = test$estimate
precip_cor[113,7] = test$p.value

# Current April
test = my.cor.test(lts$M05, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Apr[2:48]))
test
plot(lts$M05, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M05))
precip_cor[114,6] = test$estimate
precip_cor[114,7] = test$p.value

# Current May
test = my.cor.test(lts$M05, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$May[2:48]))
test
plot(lts$M05, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M05))
precip_cor[115,6] = test$estimate
precip_cor[115,7] = test$p.value

# Current June
test = my.cor.test(lts$M05, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Jun[2:48]))
test
plot(lts$M05, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M05))
precip_cor[116,6] = test$estimate
precip_cor[116,7] = test$p.value

# Current July
test = my.cor.test(lts$M05, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Jul[2:48]))
test
plot(lts$M05, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M05))
precip_cor[117,6] = test$estimate
precip_cor[117,7] = test$p.value

# Current August
test = my.cor.test(lts$M05, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Aug[2:48]))
test
plot(lts$M05, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M05))
precip_cor[118,6] = test$estimate
precip_cor[118,7] = test$p.value

# Current September
test = my.cor.test(lts$M05, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M05, precip_P$Sep[2:48]))
test
plot(lts$M05, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M05))
precip_cor[119,6] = test$estimate
precip_cor[119,7] = test$p.value
#####

##### Total Precipitation: Site 08 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F07Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$May[1:47]))
test
plot(rwi$F07Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F07Ace))
precip_cor[120,3] = test$estimate
precip_cor[120,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F07Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Jun[1:47]))
test
plot(rwi$F07Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F07Ace))
precip_cor[121,3] = test$estimate
precip_cor[121,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F07Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Jul[1:47]))
test
plot(rwi$F07Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F07Ace))
precip_cor[122,3] = test$estimate
precip_cor[122,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F07Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Aug[1:47]))
test
plot(rwi$F07Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F07Ace))
precip_cor[123,3] = test$estimate
precip_cor[123,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F07Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Sep[1:47]))
test
plot(rwi$F07Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F07Ace))
precip_cor[124,3] = test$estimate
precip_cor[124,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F07Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Oct[1:47]))
test
plot(rwi$F07Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F07Ace))
precip_cor[125,3] = test$estimate
precip_cor[125,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F07Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Nov[1:47]))
test
plot(rwi$F07Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F07Ace))
precip_cor[126,3] = test$estimate
precip_cor[126,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F07Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Dec[1:47]))
test
plot(rwi$F07Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F07Ace))
precip_cor[127,3] = test$estimate
precip_cor[127,4] = test$p.value

# Current January
test = my.cor.test(rwi$F07Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Jan[2:48]))
test
plot(rwi$F07Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F07Ace))
precip_cor[128,3] = test$estimate
precip_cor[128,4] = test$p.value

# Current February
test = my.cor.test(rwi$F07Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Feb[2:48]))
test
plot(rwi$F07Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F07Ace))
precip_cor[129,3] = test$estimate
precip_cor[129,4] = test$p.value

# Current March
test = my.cor.test(rwi$F07Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Mar[2:48]))
test
plot(rwi$F07Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F07Ace))
precip_cor[130,3] = test$estimate
precip_cor[130,4] = test$p.value

# Current April
test = my.cor.test(rwi$F07Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Apr[2:48]))
test
plot(rwi$F07Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F07Ace))
precip_cor[131,3] = test$estimate
precip_cor[131,4] = test$p.value

# Current May
test = my.cor.test(rwi$F07Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$May[2:48]))
test
plot(rwi$F07Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F07Ace))
precip_cor[132,3] = test$estimate
precip_cor[132,4] = test$p.value

# Current June
test = my.cor.test(rwi$F07Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Jun[2:48]))
test
plot(rwi$F07Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F07Ace))
precip_cor[133,3] = test$estimate
precip_cor[133,4] = test$p.value

# Current July
test = my.cor.test(rwi$F07Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Jul[2:48]))
test
plot(rwi$F07Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F07Ace))
precip_cor[134,3] = test$estimate
precip_cor[134,4] = test$p.value

# Current August
test = my.cor.test(rwi$F07Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Aug[2:48]))
test
plot(rwi$F07Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F07Ace))
precip_cor[135,3] = test$estimate
precip_cor[135,4] = test$p.value

# Current September
test = my.cor.test(rwi$F07Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Sep[2:48]))
test
plot(rwi$F07Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F07Ace))
precip_cor[136,3] = test$estimate
precip_cor[136,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F07, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$May[1:47]))
test
plot(lts$F07, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F07))
precip_cor[120,6] = test$estimate
precip_cor[120,7] = test$p.value

# Previous June
test = my.cor.test(lts$F07, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Jun[1:47]))
test
plot(lts$F07, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F07))
precip_cor[121,6] = test$estimate
precip_cor[121,7] = test$p.value

# Previous July
test = my.cor.test(lts$F07, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Jul[1:47]))
test
plot(lts$F07, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F07))
precip_cor[122,6] = test$estimate
precip_cor[122,7] = test$p.value

# Previous August
test = my.cor.test(lts$F07, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Aug[1:47]))
test
plot(lts$F07, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F07))
precip_cor[123,6] = test$estimate
precip_cor[123,7] = test$p.value

# Previous September
test = my.cor.test(lts$F07, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Sep[1:47]))
test
plot(lts$F07, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F07))
precip_cor[124,6] = test$estimate
precip_cor[124,7] = test$p.value

# Previous October
test = my.cor.test(lts$F07, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Oct[1:47]))
test
plot(lts$F07, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F07))
precip_cor[125,6] = test$estimate
precip_cor[125,7] = test$p.value

# Previous November
test = my.cor.test(lts$F07, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Nov[1:47]))
test
plot(lts$F07, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F07))
precip_cor[126,6] = test$estimate
precip_cor[126,7] = test$p.value

# Previous December
test = my.cor.test(lts$F07, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Dec[1:47]))
test
plot(lts$F07, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F07))
precip_cor[127,6] = test$estimate
precip_cor[127,7] = test$p.value

# Current January
test = my.cor.test(lts$F07, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Jan[2:48]))
test
plot(lts$F07, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F07))
precip_cor[128,6] = test$estimate
precip_cor[128,7] = test$p.value

# Current February
test = my.cor.test(lts$F07, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Feb[2:48]))
test
plot(lts$F07, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F07))
precip_cor[129,6] = test$estimate
precip_cor[129,7] = test$p.value

# Current March
test = my.cor.test(lts$F07, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Mar[2:48]))
test
plot(lts$F07, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F07))
precip_cor[130,6] = test$estimate
precip_cor[130,7] = test$p.value

# Current April
test = my.cor.test(lts$F07, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Apr[2:48]))
test
plot(lts$F07, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F07))
precip_cor[131,6] = test$estimate
precip_cor[131,7] = test$p.value

# Current May
test = my.cor.test(lts$F07, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$May[2:48]))
test
plot(lts$F07, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F07))
precip_cor[132,6] = test$estimate
precip_cor[132,7] = test$p.value

# Current June
test = my.cor.test(lts$F07, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Jun[2:48]))
test
plot(lts$F07, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F07))
precip_cor[133,6] = test$estimate
precip_cor[133,7] = test$p.value

# Current July
test = my.cor.test(lts$F07, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Jul[2:48]))
test
plot(lts$F07, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F07))
precip_cor[134,6] = test$estimate
precip_cor[134,7] = test$p.value

# Current August
test = my.cor.test(lts$F07, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Aug[2:48]))
test
plot(lts$F07, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F07))
precip_cor[135,6] = test$estimate
precip_cor[135,7] = test$p.value

# Current September
test = my.cor.test(lts$F07, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F07, precip_P$Sep[2:48]))
test
plot(lts$F07, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F07))
precip_cor[136,6] = test$estimate
precip_cor[136,7] = test$p.value
#####

##### Total Precipitation: Site 09 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M27Pin[10:47], precip_P$May[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$May[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$May[10:47])
abline(lm(precip_P$May[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[137,3] = test$estimate
precip_cor[137,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Jun[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Jun[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Jun[10:47])
abline(lm(precip_P$Jun[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[138,3] = test$estimate
precip_cor[138,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Jul[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Jul[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Jul[10:47])
abline(lm(precip_P$Jul[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[139,3] = test$estimate
precip_cor[139,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Aug[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Aug[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Aug[10:47])
abline(lm(precip_P$Aug[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[140,3] = test$estimate
precip_cor[140,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Sep[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Sep[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Sep[10:47])
abline(lm(precip_P$Sep[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[141,3] = test$estimate
precip_cor[141,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Oct[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Oct[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Oct[10:47])
abline(lm(precip_P$Oct[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[142,3] = test$estimate
precip_cor[142,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Nov[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Nov[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Nov[10:47])
abline(lm(precip_P$Nov[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[143,3] = test$estimate
precip_cor[143,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Dec[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Dec[10:47]))
test
plot(rwi$M27Pin[10:47], precip_P$Dec[10:47])
abline(lm(precip_P$Dec[10:47] ~ rwi$M27Pin[10:47]))
precip_cor[144,3] = test$estimate
precip_cor[144,4] = test$p.value

# Current January
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Jan[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Jan[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Jan[11:48])
abline(lm(precip_P$Jan[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[145,3] = test$estimate
precip_cor[145,4] = test$p.value

# Current February
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Feb[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Feb[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Feb[11:48])
abline(lm(precip_P$Feb[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[146,3] = test$estimate
precip_cor[146,4] = test$p.value

# Current March
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Mar[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Mar[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Mar[11:48])
abline(lm(precip_P$Mar[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[147,3] = test$estimate
precip_cor[147,4] = test$p.value

# Current April
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Apr[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Apr[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Apr[11:48])
abline(lm(precip_P$Apr[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[148,3] = test$estimate
precip_cor[148,4] = test$p.value

# Current May
test = my.cor.test(rwi$M27Pin[10:47], precip_P$May[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$May[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$May[11:48])
abline(lm(precip_P$May[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[149,3] = test$estimate
precip_cor[149,4] = test$p.value

# Current June
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Jun[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Jun[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Jun[11:48])
abline(lm(precip_P$Jun[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[150,3] = test$estimate
precip_cor[150,4] = test$p.value

# Current July
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Jul[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Jul[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Jul[11:48])
abline(lm(precip_P$Jul[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[151,3] = test$estimate
precip_cor[151,4] = test$p.value

# Current August
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Aug[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Aug[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Aug[11:48])
abline(lm(precip_P$Aug[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[152,3] = test$estimate
precip_cor[152,4] = test$p.value

# Current September
test = my.cor.test(rwi$M27Pin[10:47], precip_P$Sep[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Sep[11:48]))
test
plot(rwi$M27Pin[10:47], precip_P$Sep[11:48])
abline(lm(precip_P$Sep[11:48] ~ rwi$M27Pin[10:47]))
precip_cor[153,3] = test$estimate
precip_cor[153,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M27[10:47], precip_P$May[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$May[10:47]))
test
plot(lts$M27[10:47], precip_P$May[10:47])
abline(lm(precip_P$May[10:47] ~ lts$M27[10:47]))
precip_cor[137,6] = test$estimate
precip_cor[137,7] = test$p.value

# Previous June
test = my.cor.test(lts$M27[10:47], precip_P$Jun[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Jun[10:47]))
test
plot(lts$M27[10:47], precip_P$Jun[10:47])
abline(lm(precip_P$Jun[10:47] ~ lts$M27[10:47]))
precip_cor[138,6] = test$estimate
precip_cor[138,7] = test$p.value

# Previous July
test = my.cor.test(lts$M27[10:47], precip_P$Jul[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Jul[10:47]))
test
plot(lts$M27[10:47], precip_P$Jul[10:47])
abline(lm(precip_P$Jul[10:47] ~ lts$M27[10:47]))
precip_cor[139,6] = test$estimate
precip_cor[139,7] = test$p.value

# Previous August
test = my.cor.test(lts$M27[10:47], precip_P$Aug[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Aug[10:47]))
test
plot(lts$M27[10:47], precip_P$Aug[10:47])
abline(lm(precip_P$Aug[10:47] ~ lts$M27[10:47]))
precip_cor[140,6] = test$estimate
precip_cor[140,7] = test$p.value

# Previous September
test = my.cor.test(lts$M27[10:47], precip_P$Sep[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Sep[10:47]))
test
plot(lts$M27[10:47], precip_P$Sep[10:47])
abline(lm(precip_P$Sep[10:47] ~ lts$M27[10:47]))
precip_cor[141,6] = test$estimate
precip_cor[141,7] = test$p.value

# Previous October
test = my.cor.test(lts$M27[10:47], precip_P$Oct[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Oct[10:47]))
test
plot(lts$M27[10:47], precip_P$Oct[10:47])
abline(lm(precip_P$Oct[10:47] ~ lts$M27[10:47]))
precip_cor[142,6] = test$estimate
precip_cor[142,7] = test$p.value

# Previous November
test = my.cor.test(lts$M27[10:47], precip_P$Nov[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Nov[10:47]))
test
plot(lts$M27[10:47], precip_P$Nov[10:47])
abline(lm(precip_P$Nov[10:47] ~ lts$M27[10:47]))
precip_cor[143,6] = test$estimate
precip_cor[143,7] = test$p.value

# Previous December
test = my.cor.test(lts$M27[10:47], precip_P$Dec[10:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Dec[10:47]))
test
plot(lts$M27[10:47], precip_P$Dec[10:47])
abline(lm(precip_P$Dec[10:47] ~ lts$M27[10:47]))
precip_cor[144,6] = test$estimate
precip_cor[144,7] = test$p.value

# Current January
test = my.cor.test(lts$M27[10:47], precip_P$Jan[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Jan[11:48]))
test
plot(lts$M27[10:47], precip_P$Jan[11:48])
abline(lm(precip_P$Jan[11:48] ~ lts$M27[10:47]))
precip_cor[145,6] = test$estimate
precip_cor[145,7] = test$p.value

# Current February
test = my.cor.test(lts$M27[10:47], precip_P$Feb[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Feb[11:48]))
test
plot(lts$M27[10:47], precip_P$Feb[11:48])
abline(lm(precip_P$Feb[11:48] ~ lts$M27[10:47]))
precip_cor[146,6] = test$estimate
precip_cor[146,7] = test$p.value

# Current March
test = my.cor.test(lts$M27[10:47], precip_P$Mar[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Mar[11:48]))
test
plot(lts$M27[10:47], precip_P$Mar[11:48])
abline(lm(precip_P$Mar[11:48] ~ lts$M27[10:47]))
precip_cor[147,6] = test$estimate
precip_cor[147,7] = test$p.value

# Current April
test = my.cor.test(lts$M27[10:47], precip_P$Apr[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Apr[11:48]))
test
plot(lts$M27[10:47], precip_P$Apr[11:48])
abline(lm(precip_P$Apr[11:48] ~ lts$M27[10:47]))
precip_cor[148,6] = test$estimate
precip_cor[148,7] = test$p.value

# Current May
test = my.cor.test(lts$M27[10:47], precip_P$May[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$May[11:48]))
test
plot(lts$M27[10:47], precip_P$May[11:48])
abline(lm(precip_P$May[11:48] ~ lts$M27[10:47]))
precip_cor[149,6] = test$estimate
precip_cor[149,7] = test$p.value

# Current June
test = my.cor.test(lts$M27[10:47], precip_P$Jun[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Jun[11:48]))
test
plot(lts$M27[10:47], precip_P$Jun[11:48])
abline(lm(precip_P$Jun[11:48] ~ lts$M27[10:47]))
precip_cor[150,6] = test$estimate
precip_cor[150,7] = test$p.value

# Current July
test = my.cor.test(lts$M27[10:47], precip_P$Jul[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Jul[11:48]))
test
plot(lts$M27[10:47], precip_P$Jul[11:48])
abline(lm(precip_P$Jul[11:48] ~ lts$M27[10:47]))
precip_cor[151,6] = test$estimate
precip_cor[151,7] = test$p.value

# Current August
test = my.cor.test(lts$M27[10:47], precip_P$Aug[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Aug[11:48]))
test
plot(lts$M27[10:47], precip_P$Aug[11:48])
abline(lm(precip_P$Aug[11:48] ~ lts$M27[10:47]))
precip_cor[152,6] = test$estimate
precip_cor[152,7] = test$p.value

# Current September
test = my.cor.test(lts$M27[10:47], precip_P$Sep[11:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Sep[11:48]))
test
plot(lts$M27[10:47], precip_P$Sep[11:48])
abline(lm(precip_P$Sep[11:48] ~ lts$M27[10:47]))
precip_cor[153,6] = test$estimate
precip_cor[153,7] = test$p.value
#####

##### Total Precipitation: Site 10 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M26Thu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$May[1:47]))
test
plot(rwi$M26Thu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M26Thu))
precip_cor[154,3] = test$estimate
precip_cor[154,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M26Thu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Jun[1:47]))
test
plot(rwi$M26Thu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M26Thu))
precip_cor[155,3] = test$estimate
precip_cor[155,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M26Thu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Jul[1:47]))
test
plot(rwi$M26Thu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M26Thu))
precip_cor[156,3] = test$estimate
precip_cor[156,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M26Thu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Aug[1:47]))
test
plot(rwi$M26Thu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M26Thu))
precip_cor[157,3] = test$estimate
precip_cor[157,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M26Thu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Sep[1:47]))
test
plot(rwi$M26Thu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M26Thu))
precip_cor[158,3] = test$estimate
precip_cor[158,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M26Thu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Oct[1:47]))
test
plot(rwi$M26Thu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M26Thu))
precip_cor[159,3] = test$estimate
precip_cor[159,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M26Thu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Nov[1:47]))
test
plot(rwi$M26Thu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M26Thu))
precip_cor[160,3] = test$estimate
precip_cor[160,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M26Thu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Dec[1:47]))
test
plot(rwi$M26Thu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M26Thu))
precip_cor[161,3] = test$estimate
precip_cor[161,4] = test$p.value

# Current January
test = my.cor.test(rwi$M26Thu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Jan[2:48]))
test
plot(rwi$M26Thu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M26Thu))
precip_cor[162,3] = test$estimate
precip_cor[162,4] = test$p.value

# Current February
test = my.cor.test(rwi$M26Thu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Feb[2:48]))
test
plot(rwi$M26Thu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M26Thu))
precip_cor[163,3] = test$estimate
precip_cor[163,4] = test$p.value

# Current March
test = my.cor.test(rwi$M26Thu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Mar[2:48]))
test
plot(rwi$M26Thu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M26Thu))
precip_cor[164,3] = test$estimate
precip_cor[164,4] = test$p.value

# Current April
test = my.cor.test(rwi$M26Thu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Apr[2:48]))
test
plot(rwi$M26Thu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M26Thu))
precip_cor[165,3] = test$estimate
precip_cor[165,4] = test$p.value

# Current May
test = my.cor.test(rwi$M26Thu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$May[2:48]))
test
plot(rwi$M26Thu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M26Thu))
precip_cor[166,3] = test$estimate
precip_cor[166,4] = test$p.value

# Current June
test = my.cor.test(rwi$M26Thu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Jun[2:48]))
test
plot(rwi$M26Thu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M26Thu))
precip_cor[167,3] = test$estimate
precip_cor[167,4] = test$p.value

# Current July
test = my.cor.test(rwi$M26Thu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Jul[2:48]))
test
plot(rwi$M26Thu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M26Thu))
precip_cor[168,3] = test$estimate
precip_cor[168,4] = test$p.value

# Current August
test = my.cor.test(rwi$M26Thu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Aug[2:48]))
test
plot(rwi$M26Thu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M26Thu))
precip_cor[169,3] = test$estimate
precip_cor[169,4] = test$p.value

# Current September
test = my.cor.test(rwi$M26Thu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Sep[2:48]))
test
plot(rwi$M26Thu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M26Thu))
precip_cor[170,3] = test$estimate
precip_cor[170,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M26, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$May[1:47]))
test
plot(lts$M26, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M26))
precip_cor[154,6] = test$estimate
precip_cor[154,7] = test$p.value

# Previous June
test = my.cor.test(lts$M26, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Jun[1:47]))
test
plot(lts$M26, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M26))
precip_cor[155,6] = test$estimate
precip_cor[155,7] = test$p.value

# Previous July
test = my.cor.test(lts$M26, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Jul[1:47]))
test
plot(lts$M26, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M26))
precip_cor[156,6] = test$estimate
precip_cor[156,7] = test$p.value

# Previous August
test = my.cor.test(lts$M26, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Aug[1:47]))
test
plot(lts$M26, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M26))
precip_cor[157,6] = test$estimate
precip_cor[157,7] = test$p.value

# Previous September
test = my.cor.test(lts$M26, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Sep[1:47]))
test
plot(lts$M26, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M26))
precip_cor[158,6] = test$estimate
precip_cor[158,7] = test$p.value

# Previous October
test = my.cor.test(lts$M26, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Oct[1:47]))
test
plot(lts$M26, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M26))
precip_cor[159,6] = test$estimate
precip_cor[159,7] = test$p.value

# Previous November
test = my.cor.test(lts$M26, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Nov[1:47]))
test
plot(lts$M26, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M26))
precip_cor[160,6] = test$estimate
precip_cor[160,7] = test$p.value

# Previous December
test = my.cor.test(lts$M26, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Dec[1:47]))
test
plot(lts$M26, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M26))
precip_cor[161,6] = test$estimate
precip_cor[161,7] = test$p.value

# Current January
test = my.cor.test(lts$M26, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Jan[2:48]))
test
plot(lts$M26, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M26))
precip_cor[162,6] = test$estimate
precip_cor[162,7] = test$p.value

# Current February
test = my.cor.test(lts$M26, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Feb[2:48]))
test
plot(lts$M26, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M26))
precip_cor[163,6] = test$estimate
precip_cor[163,7] = test$p.value

# Current March
test = my.cor.test(lts$M26, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Mar[2:48]))
test
plot(lts$M26, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M26))
precip_cor[164,6] = test$estimate
precip_cor[164,7] = test$p.value

# Current April
test = my.cor.test(lts$M26, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Apr[2:48]))
test
plot(lts$M26, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M26))
precip_cor[165,6] = test$estimate
precip_cor[165,7] = test$p.value

# Current May
test = my.cor.test(lts$M26, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$May[2:48]))
test
plot(lts$M26, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M26))
precip_cor[166,6] = test$estimate
precip_cor[166,7] = test$p.value

# Current June
test = my.cor.test(lts$M26, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Jun[2:48]))
test
plot(lts$M26, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M26))
precip_cor[167,6] = test$estimate
precip_cor[167,7] = test$p.value

# Current July
test = my.cor.test(lts$M26, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Jul[2:48]))
test
plot(lts$M26, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M26))
precip_cor[168,6] = test$estimate
precip_cor[168,7] = test$p.value

# Current August
test = my.cor.test(lts$M26, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Aug[2:48]))
test
plot(lts$M26, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M26))
precip_cor[169,6] = test$estimate
precip_cor[169,7] = test$p.value

# Current September
test = my.cor.test(lts$M26, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M26, precip_P$Sep[2:48]))
test
plot(lts$M26, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M26))
precip_cor[170,6] = test$estimate
precip_cor[170,7] = test$p.value
#####

##### Total Precipitation: Site 11 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F25Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$May[1:47]))
test
plot(rwi$F25Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F25Ace))
precip_cor[171,3] = test$estimate
precip_cor[171,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F25Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Jun[1:47]))
test
plot(rwi$F25Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F25Ace))
precip_cor[172,3] = test$estimate
precip_cor[172,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F25Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Jul[1:47]))
test
plot(rwi$F25Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F25Ace))
precip_cor[173,3] = test$estimate
precip_cor[173,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F25Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Aug[1:47]))
test
plot(rwi$F25Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F25Ace))
precip_cor[174,3] = test$estimate
precip_cor[174,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F25Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Sep[1:47]))
test
plot(rwi$F25Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F25Ace))
precip_cor[175,3] = test$estimate
precip_cor[175,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F25Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Oct[1:47]))
test
plot(rwi$F25Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F25Ace))
precip_cor[176,3] = test$estimate
precip_cor[176,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F25Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Nov[1:47]))
test
plot(rwi$F25Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F25Ace))
precip_cor[177,3] = test$estimate
precip_cor[177,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F25Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Dec[1:47]))
test
plot(rwi$F25Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F25Ace))
precip_cor[178,3] = test$estimate
precip_cor[178,4] = test$p.value

# Current January
test = my.cor.test(rwi$F25Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Jan[2:48]))
test
plot(rwi$F25Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F25Ace))
precip_cor[179,3] = test$estimate
precip_cor[179,4] = test$p.value

# Current February
test = my.cor.test(rwi$F25Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Feb[2:48]))
test
plot(rwi$F25Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F25Ace))
precip_cor[180,3] = test$estimate
precip_cor[180,4] = test$p.value

# Current March
test = my.cor.test(rwi$F25Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Mar[2:48]))
test
plot(rwi$F25Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F25Ace))
precip_cor[181,3] = test$estimate
precip_cor[181,4] = test$p.value

# Current April
test = my.cor.test(rwi$F25Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Apr[2:48]))
test
plot(rwi$F25Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F25Ace))
precip_cor[182,3] = test$estimate
precip_cor[182,4] = test$p.value

# Current May
test = my.cor.test(rwi$F25Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$May[2:48]))
test
plot(rwi$F25Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F25Ace))
precip_cor[183,3] = test$estimate
precip_cor[183,4] = test$p.value

# Current June
test = my.cor.test(rwi$F25Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Jun[2:48]))
test
plot(rwi$F25Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F25Ace))
precip_cor[184,3] = test$estimate
precip_cor[184,4] = test$p.value

# Current July
test = my.cor.test(rwi$F25Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Jul[2:48]))
test
plot(rwi$F25Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F25Ace))
precip_cor[185,3] = test$estimate
precip_cor[185,4] = test$p.value

# Current August
test = my.cor.test(rwi$F25Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Aug[2:48]))
test
plot(rwi$F25Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F25Ace))
precip_cor[186,3] = test$estimate
precip_cor[186,4] = test$p.value

# Current September
test = my.cor.test(rwi$F25Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Sep[2:48]))
test
plot(rwi$F25Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F25Ace))
precip_cor[187,3] = test$estimate
precip_cor[187,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F25, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$May[1:47]))
test
plot(lts$F25, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F25))
precip_cor[171,6] = test$estimate
precip_cor[171,7] = test$p.value

# Previous June
test = my.cor.test(lts$F25, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Jun[1:47]))
test
plot(lts$F25, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F25))
precip_cor[172,6] = test$estimate
precip_cor[172,7] = test$p.value

# Previous July
test = my.cor.test(lts$F25, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Jul[1:47]))
test
plot(lts$F25, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F25))
precip_cor[173,6] = test$estimate
precip_cor[173,7] = test$p.value

# Previous August
test = my.cor.test(lts$F25, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Aug[1:47]))
test
plot(lts$F25, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F25))
precip_cor[174,6] = test$estimate
precip_cor[174,7] = test$p.value

# Previous September
test = my.cor.test(lts$F25, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Sep[1:47]))
test
plot(lts$F25, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F25))
precip_cor[175,6] = test$estimate
precip_cor[175,7] = test$p.value

# Previous October
test = my.cor.test(lts$F25, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Oct[1:47]))
test
plot(lts$F25, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F25))
precip_cor[176,6] = test$estimate
precip_cor[176,7] = test$p.value

# Previous November
test = my.cor.test(lts$F25, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Nov[1:47]))
test
plot(lts$F25, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F25))
precip_cor[177,6] = test$estimate
precip_cor[177,7] = test$p.value

# Previous December
test = my.cor.test(lts$F25, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Dec[1:47]))
test
plot(lts$F25, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F25))
precip_cor[178,6] = test$estimate
precip_cor[178,7] = test$p.value

# Current January
test = my.cor.test(lts$F25, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Jan[2:48]))
test
plot(lts$F25, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F25))
precip_cor[179,6] = test$estimate
precip_cor[179,7] = test$p.value

# Current February
test = my.cor.test(lts$F25, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Feb[2:48]))
test
plot(lts$F25, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F25))
precip_cor[180,6] = test$estimate
precip_cor[180,7] = test$p.value

# Current March
test = my.cor.test(lts$F25, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Mar[2:48]))
test
plot(lts$F25, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F25))
precip_cor[181,6] = test$estimate
precip_cor[181,7] = test$p.value

# Current April
test = my.cor.test(lts$F25, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Apr[2:48]))
test
plot(lts$F25, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F25))
precip_cor[182,6] = test$estimate
precip_cor[182,7] = test$p.value

# Current May
test = my.cor.test(lts$F25, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$May[2:48]))
test
plot(lts$F25, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F25))
precip_cor[183,6] = test$estimate
precip_cor[183,7] = test$p.value

# Current June
test = my.cor.test(lts$F25, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Jun[2:48]))
test
plot(lts$F25, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F25))
precip_cor[184,6] = test$estimate
precip_cor[184,7] = test$p.value

# Current July
test = my.cor.test(lts$F25, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Jul[2:48]))
test
plot(lts$F25, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F25))
precip_cor[185,6] = test$estimate
precip_cor[185,7] = test$p.value

# Current August
test = my.cor.test(lts$F25, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Aug[2:48]))
test
plot(lts$F25, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F25))
precip_cor[186,6] = test$estimate
precip_cor[186,7] = test$p.value

# Current September
test = my.cor.test(lts$F25, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F25, precip_P$Sep[2:48]))
test
plot(lts$F25, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F25))
precip_cor[187,6] = test$estimate
precip_cor[187,7] = test$p.value
#####

##### Total Precipitation: Site 12 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M01Pop, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$May[1:47]))
test
plot(rwi$M01Pop, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M01Pop))
precip_cor[188,3] = test$estimate
precip_cor[188,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M01Pop, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Jun[1:47]))
test
plot(rwi$M01Pop, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M01Pop))
precip_cor[189,3] = test$estimate
precip_cor[189,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M01Pop, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Jul[1:47]))
test
plot(rwi$M01Pop, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M01Pop))
precip_cor[190,3] = test$estimate
precip_cor[190,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M01Pop, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Aug[1:47]))
test
plot(rwi$M01Pop, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M01Pop))
precip_cor[191,3] = test$estimate
precip_cor[191,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M01Pop, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Sep[1:47]))
test
plot(rwi$M01Pop, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M01Pop))
precip_cor[192,3] = test$estimate
precip_cor[192,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M01Pop, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Oct[1:47]))
test
plot(rwi$M01Pop, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M01Pop))
precip_cor[193,3] = test$estimate
precip_cor[193,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M01Pop, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Nov[1:47]))
test
plot(rwi$M01Pop, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M01Pop))
precip_cor[194,3] = test$estimate
precip_cor[194,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M01Pop, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Dec[1:47]))
test
plot(rwi$M01Pop, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M01Pop))
precip_cor[195,3] = test$estimate
precip_cor[195,4] = test$p.value

# Current January
test = my.cor.test(rwi$M01Pop, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Jan[2:48]))
test
plot(rwi$M01Pop, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M01Pop))
precip_cor[196,3] = test$estimate
precip_cor[196,4] = test$p.value

# Current February
test = my.cor.test(rwi$M01Pop, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Feb[2:48]))
test
plot(rwi$M01Pop, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M01Pop))
precip_cor[197,3] = test$estimate
precip_cor[197,4] = test$p.value

# Current March
test = my.cor.test(rwi$M01Pop, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Mar[2:48]))
test
plot(rwi$M01Pop, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M01Pop))
precip_cor[198,3] = test$estimate
precip_cor[198,4] = test$p.value

# Current April
test = my.cor.test(rwi$M01Pop, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Apr[2:48]))
test
plot(rwi$M01Pop, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M01Pop))
precip_cor[199,3] = test$estimate
precip_cor[199,4] = test$p.value

# Current May
test = my.cor.test(rwi$M01Pop, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$May[2:48]))
test
plot(rwi$M01Pop, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M01Pop))
precip_cor[200,3] = test$estimate
precip_cor[200,4] = test$p.value

# Current June
test = my.cor.test(rwi$M01Pop, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Jun[2:48]))
test
plot(rwi$M01Pop, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M01Pop))
precip_cor[201,3] = test$estimate
precip_cor[201,4] = test$p.value

# Current July
test = my.cor.test(rwi$M01Pop, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Jul[2:48]))
test
plot(rwi$M01Pop, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M01Pop))
precip_cor[202,3] = test$estimate
precip_cor[202,4] = test$p.value

# Current August
test = my.cor.test(rwi$M01Pop, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Aug[2:48]))
test
plot(rwi$M01Pop, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M01Pop))
precip_cor[203,3] = test$estimate
precip_cor[203,4] = test$p.value

# Current September
test = my.cor.test(rwi$M01Pop, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Sep[2:48]))
test
plot(rwi$M01Pop, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M01Pop))
precip_cor[204,3] = test$estimate
precip_cor[204,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M01, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$May[1:47]))
test
plot(lts$M01, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M01))
precip_cor[188,6] = test$estimate
precip_cor[188,7] = test$p.value

# Previous June
test = my.cor.test(lts$M01, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Jun[1:47]))
test
plot(lts$M01, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M01))
precip_cor[189,6] = test$estimate
precip_cor[189,7] = test$p.value

# Previous July
test = my.cor.test(lts$M01, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Jul[1:47]))
test
plot(lts$M01, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M01))
precip_cor[190,6] = test$estimate
precip_cor[190,7] = test$p.value

# Previous August
test = my.cor.test(lts$M01, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Aug[1:47]))
test
plot(lts$M01, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M01))
precip_cor[191,6] = test$estimate
precip_cor[191,7] = test$p.value

# Previous September
test = my.cor.test(lts$M01, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Sep[1:47]))
test
plot(lts$M01, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M01))
precip_cor[192,6] = test$estimate
precip_cor[192,7] = test$p.value

# Previous October
test = my.cor.test(lts$M01, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Oct[1:47]))
test
plot(lts$M01, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M01))
precip_cor[193,6] = test$estimate
precip_cor[193,7] = test$p.value

# Previous November
test = my.cor.test(lts$M01, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Nov[1:47]))
test
plot(lts$M01, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M01))
precip_cor[194,6] = test$estimate
precip_cor[194,7] = test$p.value

# Previous December
test = my.cor.test(lts$M01, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Dec[1:47]))
test
plot(lts$M01, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M01))
precip_cor[195,6] = test$estimate
precip_cor[195,7] = test$p.value

# Current January
test = my.cor.test(lts$M01, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Jan[2:48]))
test
plot(lts$M01, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M01))
precip_cor[196,6] = test$estimate
precip_cor[196,7] = test$p.value

# Current February
test = my.cor.test(lts$M01, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Feb[2:48]))
test
plot(lts$M01, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M01))
precip_cor[197,6] = test$estimate
precip_cor[197,7] = test$p.value

# Current March
test = my.cor.test(lts$M01, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Mar[2:48]))
test
plot(lts$M01, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M01))
precip_cor[198,6] = test$estimate
precip_cor[198,7] = test$p.value

# Current April
test = my.cor.test(lts$M01, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Apr[2:48]))
test
plot(lts$M01, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M01))
precip_cor[199,6] = test$estimate
precip_cor[199,7] = test$p.value

# Current May
test = my.cor.test(lts$M01, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$May[2:48]))
test
plot(lts$M01, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M01))
precip_cor[200,6] = test$estimate
precip_cor[200,7] = test$p.value

# Current June
test = my.cor.test(lts$M01, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Jun[2:48]))
test
plot(lts$M01, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M01))
precip_cor[201,6] = test$estimate
precip_cor[201,7] = test$p.value

# Current July
test = my.cor.test(lts$M01, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Jul[2:48]))
test
plot(lts$M01, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M01))
precip_cor[202,6] = test$estimate
precip_cor[202,7] = test$p.value

# Current August
test = my.cor.test(lts$M01, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Aug[2:48]))
test
plot(lts$M01, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M01))
precip_cor[203,6] = test$estimate
precip_cor[203,7] = test$p.value

# Current September
test = my.cor.test(lts$M01, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M01, precip_P$Sep[2:48]))
test
plot(lts$M01, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M01))
precip_cor[204,6] = test$estimate
precip_cor[204,7] = test$p.value
#####

##### Total Precipitation: Site 13 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F33Pic, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$May[1:47]))
test
plot(rwi$F33Pic, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F33Pic))
precip_cor[205,3] = test$estimate
precip_cor[205,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F33Pic, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Jun[1:47]))
test
plot(rwi$F33Pic, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F33Pic))
precip_cor[206,3] = test$estimate
precip_cor[206,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F33Pic, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Jul[1:47]))
test
plot(rwi$F33Pic, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F33Pic))
precip_cor[207,3] = test$estimate
precip_cor[207,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F33Pic, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Aug[1:47]))
test
plot(rwi$F33Pic, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F33Pic))
precip_cor[208,3] = test$estimate
precip_cor[208,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F33Pic, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Sep[1:47]))
test
plot(rwi$F33Pic, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F33Pic))
precip_cor[209,3] = test$estimate
precip_cor[209,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F33Pic, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Oct[1:47]))
test
plot(rwi$F33Pic, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F33Pic))
precip_cor[210,3] = test$estimate
precip_cor[210,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F33Pic, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Nov[1:47]))
test
plot(rwi$F33Pic, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F33Pic))
precip_cor[211,3] = test$estimate
precip_cor[211,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F33Pic, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Dec[1:47]))
test
plot(rwi$F33Pic, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F33Pic))
precip_cor[212,3] = test$estimate
precip_cor[212,4] = test$p.value

# Current January
test = my.cor.test(rwi$F33Pic, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Jan[2:48]))
test
plot(rwi$F33Pic, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F33Pic))
precip_cor[213,3] = test$estimate
precip_cor[213,4] = test$p.value

# Current February
test = my.cor.test(rwi$F33Pic, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Feb[2:48]))
test
plot(rwi$F33Pic, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F33Pic))
precip_cor[214,3] = test$estimate
precip_cor[214,4] = test$p.value

# Current March
test = my.cor.test(rwi$F33Pic, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Mar[2:48]))
test
plot(rwi$F33Pic, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F33Pic))
precip_cor[215,3] = test$estimate
precip_cor[215,4] = test$p.value

# Current April
test = my.cor.test(rwi$F33Pic, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Apr[2:48]))
test
plot(rwi$F33Pic, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F33Pic))
precip_cor[216,3] = test$estimate
precip_cor[216,4] = test$p.value

# Current May
test = my.cor.test(rwi$F33Pic, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$May[2:48]))
test
plot(rwi$F33Pic, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F33Pic))
precip_cor[217,3] = test$estimate
precip_cor[217,4] = test$p.value

# Current June
test = my.cor.test(rwi$F33Pic, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Jun[2:48]))
test
plot(rwi$F33Pic, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F33Pic))
precip_cor[218,3] = test$estimate
precip_cor[218,4] = test$p.value

# Current July
test = my.cor.test(rwi$F33Pic, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Jul[2:48]))
test
plot(rwi$F33Pic, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F33Pic))
precip_cor[219,3] = test$estimate
precip_cor[219,4] = test$p.value

# Current August
test = my.cor.test(rwi$F33Pic, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Aug[2:48]))
test
plot(rwi$F33Pic, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F33Pic))
precip_cor[220,3] = test$estimate
precip_cor[220,4] = test$p.value

# Current September
test = my.cor.test(rwi$F33Pic, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Sep[2:48]))
test
plot(rwi$F33Pic, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F33Pic))
precip_cor[221,3] = test$estimate
precip_cor[221,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F33, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$May[1:47]))
test
plot(lts$F33, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F33))
precip_cor[205,6] = test$estimate
precip_cor[205,7] = test$p.value

# Previous June
test = my.cor.test(lts$F33, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Jun[1:47]))
test
plot(lts$F33, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F33))
precip_cor[206,6] = test$estimate
precip_cor[206,7] = test$p.value

# Previous July
test = my.cor.test(lts$F33, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Jul[1:47]))
test
plot(lts$F33, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F33))
precip_cor[207,6] = test$estimate
precip_cor[207,7] = test$p.value

# Previous August
test = my.cor.test(lts$F33, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Aug[1:47]))
test
plot(lts$F33, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F33))
precip_cor[208,6] = test$estimate
precip_cor[208,7] = test$p.value

# Previous September
test = my.cor.test(lts$F33, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Sep[1:47]))
test
plot(lts$F33, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F33))
precip_cor[209,6] = test$estimate
precip_cor[209,7] = test$p.value

# Previous October
test = my.cor.test(lts$F33, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Oct[1:47]))
test
plot(lts$F33, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F33))
precip_cor[210,6] = test$estimate
precip_cor[210,7] = test$p.value

# Previous November
test = my.cor.test(lts$F33, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Nov[1:47]))
test
plot(lts$F33, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F33))
precip_cor[211,6] = test$estimate
precip_cor[211,7] = test$p.value

# Previous December
test = my.cor.test(lts$F33, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Dec[1:47]))
test
plot(lts$F33, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F33))
precip_cor[212,6] = test$estimate
precip_cor[212,7] = test$p.value

# Current January
test = my.cor.test(lts$F33, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Jan[2:48]))
test
plot(lts$F33, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F33))
precip_cor[213,6] = test$estimate
precip_cor[213,7] = test$p.value

# Current February
test = my.cor.test(lts$F33, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Feb[2:48]))
test
plot(lts$F33, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F33))
precip_cor[214,6] = test$estimate
precip_cor[214,7] = test$p.value

# Current March
test = my.cor.test(lts$F33, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Mar[2:48]))
test
plot(lts$F33, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F33))
precip_cor[215,6] = test$estimate
precip_cor[215,7] = test$p.value

# Current April
test = my.cor.test(lts$F33, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Apr[2:48]))
test
plot(lts$F33, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F33))
precip_cor[216,6] = test$estimate
precip_cor[216,7] = test$p.value

# Current May
test = my.cor.test(lts$F33, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$May[2:48]))
test
plot(lts$F33, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F33))
precip_cor[217,6] = test$estimate
precip_cor[217,7] = test$p.value

# Current June
test = my.cor.test(lts$F33, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Jun[2:48]))
test
plot(lts$F33, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F33))
precip_cor[218,6] = test$estimate
precip_cor[218,7] = test$p.value

# Current July
test = my.cor.test(lts$F33, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Jul[2:48]))
test
plot(lts$F33, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F33))
precip_cor[219,6] = test$estimate
precip_cor[219,7] = test$p.value

# Current August
test = my.cor.test(lts$F33, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Aug[2:48]))
test
plot(lts$F33, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F33))
precip_cor[220,6] = test$estimate
precip_cor[220,7] = test$p.value

# Current September
test = my.cor.test(lts$F33, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F33, precip_P$Sep[2:48]))
test
plot(lts$F33, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F33))
precip_cor[221,6] = test$estimate
precip_cor[221,7] = test$p.value
#####

##### Total Precipitation: Site 14 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M20Thu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$May[1:47]))
test
plot(rwi$M20Thu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M20Thu))
precip_cor[222,3] = test$estimate
precip_cor[222,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M20Thu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Jun[1:47]))
test
plot(rwi$M20Thu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M20Thu))
precip_cor[223,3] = test$estimate
precip_cor[223,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M20Thu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Jul[1:47]))
test
plot(rwi$M20Thu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M20Thu))
precip_cor[224,3] = test$estimate
precip_cor[224,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M20Thu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Aug[1:47]))
test
plot(rwi$M20Thu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M20Thu))
precip_cor[225,3] = test$estimate
precip_cor[225,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M20Thu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Sep[1:47]))
test
plot(rwi$M20Thu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M20Thu))
precip_cor[226,3] = test$estimate
precip_cor[226,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M20Thu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Oct[1:47]))
test
plot(rwi$M20Thu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M20Thu))
precip_cor[227,3] = test$estimate
precip_cor[227,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M20Thu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Nov[1:47]))
test
plot(rwi$M20Thu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M20Thu))
precip_cor[228,3] = test$estimate
precip_cor[228,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M20Thu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Dec[1:47]))
test
plot(rwi$M20Thu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M20Thu))
precip_cor[229,3] = test$estimate
precip_cor[229,4] = test$p.value

# Current January
test = my.cor.test(rwi$M20Thu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Jan[2:48]))
test
plot(rwi$M20Thu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M20Thu))
precip_cor[230,3] = test$estimate
precip_cor[230,4] = test$p.value

# Current February
test = my.cor.test(rwi$M20Thu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Feb[2:48]))
test
plot(rwi$M20Thu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M20Thu))
precip_cor[231,3] = test$estimate
precip_cor[231,4] = test$p.value

# Current March
test = my.cor.test(rwi$M20Thu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Mar[2:48]))
test
plot(rwi$M20Thu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M20Thu))
precip_cor[232,3] = test$estimate
precip_cor[232,4] = test$p.value

# Current April
test = my.cor.test(rwi$M20Thu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Apr[2:48]))
test
plot(rwi$M20Thu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M20Thu))
precip_cor[233,3] = test$estimate
precip_cor[233,4] = test$p.value

# Current May
test = my.cor.test(rwi$M20Thu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$May[2:48]))
test
plot(rwi$M20Thu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M20Thu))
precip_cor[234,3] = test$estimate
precip_cor[234,4] = test$p.value

# Current June
test = my.cor.test(rwi$M20Thu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Jun[2:48]))
test
plot(rwi$M20Thu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M20Thu))
precip_cor[235,3] = test$estimate
precip_cor[235,4] = test$p.value

# Current July
test = my.cor.test(rwi$M20Thu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Jul[2:48]))
test
plot(rwi$M20Thu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M20Thu))
precip_cor[236,3] = test$estimate
precip_cor[236,4] = test$p.value

# Current August
test = my.cor.test(rwi$M20Thu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Aug[2:48]))
test
plot(rwi$M20Thu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M20Thu))
precip_cor[237,3] = test$estimate
precip_cor[237,4] = test$p.value

# Current September
test = my.cor.test(rwi$M20Thu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Sep[2:48]))
test
plot(rwi$M20Thu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M20Thu))
precip_cor[238,3] = test$estimate
precip_cor[238,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M20, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$May[1:47]))
test
plot(lts$M20, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M20))
precip_cor[222,6] = test$estimate
precip_cor[222,7] = test$p.value

# Previous June
test = my.cor.test(lts$M20, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Jun[1:47]))
test
plot(lts$M20, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M20))
precip_cor[223,6] = test$estimate
precip_cor[223,7] = test$p.value

# Previous July
test = my.cor.test(lts$M20, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Jul[1:47]))
test
plot(lts$M20, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M20))
precip_cor[224,6] = test$estimate
precip_cor[224,7] = test$p.value

# Previous August
test = my.cor.test(lts$M20, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Aug[1:47]))
test
plot(lts$M20, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M20))
precip_cor[225,6] = test$estimate
precip_cor[225,7] = test$p.value

# Previous September
test = my.cor.test(lts$M20, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Sep[1:47]))
test
plot(lts$M20, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M20))
precip_cor[226,6] = test$estimate
precip_cor[226,7] = test$p.value

# Previous October
test = my.cor.test(lts$M20, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Oct[1:47]))
test
plot(lts$M20, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M20))
precip_cor[227,6] = test$estimate
precip_cor[227,7] = test$p.value

# Previous November
test = my.cor.test(lts$M20, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Nov[1:47]))
test
plot(lts$M20, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M20))
precip_cor[228,6] = test$estimate
precip_cor[228,7] = test$p.value

# Previous December
test = my.cor.test(lts$M20, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Dec[1:47]))
test
plot(lts$M20, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M20))
precip_cor[229,6] = test$estimate
precip_cor[229,7] = test$p.value

# Current January
test = my.cor.test(lts$M20, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Jan[2:48]))
test
plot(lts$M20, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M20))
precip_cor[230,6] = test$estimate
precip_cor[230,7] = test$p.value

# Current February
test = my.cor.test(lts$M20, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Feb[2:48]))
test
plot(lts$M20, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M20))
precip_cor[231,6] = test$estimate
precip_cor[231,7] = test$p.value

# Current March
test = my.cor.test(lts$M20, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Mar[2:48]))
test
plot(lts$M20, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M20))
precip_cor[232,6] = test$estimate
precip_cor[232,7] = test$p.value

# Current April
test = my.cor.test(lts$M20, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Apr[2:48]))
test
plot(lts$M20, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M20))
precip_cor[233,6] = test$estimate
precip_cor[233,7] = test$p.value

# Current May
test = my.cor.test(lts$M20, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$May[2:48]))
test
plot(lts$M20, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M20))
precip_cor[234,6] = test$estimate
precip_cor[234,7] = test$p.value

# Current June
test = my.cor.test(lts$M20, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Jun[2:48]))
test
plot(lts$M20, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M20))
precip_cor[235,6] = test$estimate
precip_cor[235,7] = test$p.value

# Current July
test = my.cor.test(lts$M20, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Jul[2:48]))
test
plot(lts$M20, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M20))
precip_cor[236,6] = test$estimate
precip_cor[236,7] = test$p.value

# Current August
test = my.cor.test(lts$M20, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Aug[2:48]))
test
plot(lts$M20, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M20))
precip_cor[237,6] = test$estimate
precip_cor[237,7] = test$p.value

# Current September
test = my.cor.test(lts$M20, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M20, precip_P$Sep[2:48]))
test
plot(lts$M20, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M20))
precip_cor[238,6] = test$estimate
precip_cor[238,7] = test$p.value
#####

##### Total Precipitation: Site 15 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F30Bet, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$May[1:47]))
test
plot(rwi$F30Bet, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F30Bet))
precip_cor[239,3] = test$estimate
precip_cor[239,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F30Bet, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Jun[1:47]))
test
plot(rwi$F30Bet, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F30Bet))
precip_cor[240,3] = test$estimate
precip_cor[240,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F30Bet, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Jul[1:47]))
test
plot(rwi$F30Bet, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F30Bet))
precip_cor[241,3] = test$estimate
precip_cor[241,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F30Bet, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Aug[1:47]))
test
plot(rwi$F30Bet, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F30Bet))
precip_cor[242,3] = test$estimate
precip_cor[242,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F30Bet, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Sep[1:47]))
test
plot(rwi$F30Bet, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F30Bet))
precip_cor[243,3] = test$estimate
precip_cor[243,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F30Bet, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Oct[1:47]))
test
plot(rwi$F30Bet, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F30Bet))
precip_cor[244,3] = test$estimate
precip_cor[244,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F30Bet, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Nov[1:47]))
test
plot(rwi$F30Bet, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F30Bet))
precip_cor[245,3] = test$estimate
precip_cor[245,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F30Bet, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Dec[1:47]))
test
plot(rwi$F30Bet, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F30Bet))
precip_cor[246,3] = test$estimate
precip_cor[246,4] = test$p.value

# Current January
test = my.cor.test(rwi$F30Bet, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Jan[2:48]))
test
plot(rwi$F30Bet, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F30Bet))
precip_cor[247,3] = test$estimate
precip_cor[247,4] = test$p.value

# Current February
test = my.cor.test(rwi$F30Bet, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Feb[2:48]))
test
plot(rwi$F30Bet, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F30Bet))
precip_cor[248,3] = test$estimate
precip_cor[248,4] = test$p.value

# Current March
test = my.cor.test(rwi$F30Bet, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Mar[2:48]))
test
plot(rwi$F30Bet, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F30Bet))
precip_cor[249,3] = test$estimate
precip_cor[249,4] = test$p.value

# Current April
test = my.cor.test(rwi$F30Bet, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Apr[2:48]))
test
plot(rwi$F30Bet, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F30Bet))
precip_cor[250,3] = test$estimate
precip_cor[250,4] = test$p.value

# Current May
test = my.cor.test(rwi$F30Bet, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$May[2:48]))
test
plot(rwi$F30Bet, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F30Bet))
precip_cor[251,3] = test$estimate
precip_cor[251,4] = test$p.value

# Current June
test = my.cor.test(rwi$F30Bet, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Jun[2:48]))
test
plot(rwi$F30Bet, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F30Bet))
precip_cor[252,3] = test$estimate
precip_cor[252,4] = test$p.value

# Current July
test = my.cor.test(rwi$F30Bet, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Jul[2:48]))
test
plot(rwi$F30Bet, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F30Bet))
precip_cor[253,3] = test$estimate
precip_cor[253,4] = test$p.value

# Current August
test = my.cor.test(rwi$F30Bet, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Aug[2:48]))
test
plot(rwi$F30Bet, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F30Bet))
precip_cor[254,3] = test$estimate
precip_cor[254,4] = test$p.value

# Current September
test = my.cor.test(rwi$F30Bet, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Sep[2:48]))
test
plot(rwi$F30Bet, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F30Bet))
precip_cor[255,3] = test$estimate
precip_cor[255,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$F30, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$May[1:47]))
test
plot(lts$F30, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$F30))
precip_cor[239,6] = test$estimate
precip_cor[239,7] = test$p.value

# Previous June
test = my.cor.test(lts$F30, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Jun[1:47]))
test
plot(lts$F30, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$F30))
precip_cor[240,6] = test$estimate
precip_cor[240,7] = test$p.value

# Previous July
test = my.cor.test(lts$F30, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Jul[1:47]))
test
plot(lts$F30, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$F30))
precip_cor[241,6] = test$estimate
precip_cor[241,7] = test$p.value

# Previous August
test = my.cor.test(lts$F30, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Aug[1:47]))
test
plot(lts$F30, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$F30))
precip_cor[242,6] = test$estimate
precip_cor[242,7] = test$p.value

# Previous September
test = my.cor.test(lts$F30, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Sep[1:47]))
test
plot(lts$F30, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$F30))
precip_cor[243,6] = test$estimate
precip_cor[243,7] = test$p.value

# Previous October
test = my.cor.test(lts$F30, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Oct[1:47]))
test
plot(lts$F30, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$F30))
precip_cor[244,6] = test$estimate
precip_cor[244,7] = test$p.value

# Previous November
test = my.cor.test(lts$F30, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Nov[1:47]))
test
plot(lts$F30, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$F30))
precip_cor[245,6] = test$estimate
precip_cor[245,7] = test$p.value

# Previous December
test = my.cor.test(lts$F30, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Dec[1:47]))
test
plot(lts$F30, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$F30))
precip_cor[246,6] = test$estimate
precip_cor[246,7] = test$p.value

# Current January
test = my.cor.test(lts$F30, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Jan[2:48]))
test
plot(lts$F30, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$F30))
precip_cor[247,6] = test$estimate
precip_cor[247,7] = test$p.value

# Current February
test = my.cor.test(lts$F30, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Feb[2:48]))
test
plot(lts$F30, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$F30))
precip_cor[248,6] = test$estimate
precip_cor[248,7] = test$p.value

# Current March
test = my.cor.test(lts$F30, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Mar[2:48]))
test
plot(lts$F30, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$F30))
precip_cor[249,6] = test$estimate
precip_cor[249,7] = test$p.value

# Current April
test = my.cor.test(lts$F30, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Apr[2:48]))
test
plot(lts$F30, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$F30))
precip_cor[250,6] = test$estimate
precip_cor[250,7] = test$p.value

# Current May
test = my.cor.test(lts$F30, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$May[2:48]))
test
plot(lts$F30, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$F30))
precip_cor[251,6] = test$estimate
precip_cor[251,7] = test$p.value

# Current June
test = my.cor.test(lts$F30, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Jun[2:48]))
test
plot(lts$F30, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$F30))
precip_cor[252,6] = test$estimate
precip_cor[252,7] = test$p.value

# Current July
test = my.cor.test(lts$F30, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Jul[2:48]))
test
plot(lts$F30, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$F30))
precip_cor[253,6] = test$estimate
precip_cor[253,7] = test$p.value

# Current August
test = my.cor.test(lts$F30, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Aug[2:48]))
test
plot(lts$F30, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$F30))
precip_cor[254,6] = test$estimate
precip_cor[254,7] = test$p.value

# Current September
test = my.cor.test(lts$F30, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$F30, precip_P$Sep[2:48]))
test
plot(lts$F30, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$F30))
precip_cor[255,6] = test$estimate
precip_cor[255,7] = test$p.value
#####

##### Total Precipitation: Site 16 #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M17Thu, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$May[1:47]))
test
plot(rwi$M17Thu, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M17Thu))
precip_cor[256,3] = test$estimate
precip_cor[256,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M17Thu, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Jun[1:47]))
test
plot(rwi$M17Thu, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M17Thu))
precip_cor[257,3] = test$estimate
precip_cor[257,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M17Thu, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Jul[1:47]))
test
plot(rwi$M17Thu, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M17Thu))
precip_cor[258,3] = test$estimate
precip_cor[258,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M17Thu, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Aug[1:47]))
test
plot(rwi$M17Thu, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M17Thu))
precip_cor[259,3] = test$estimate
precip_cor[259,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M17Thu, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Sep[1:47]))
test
plot(rwi$M17Thu, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M17Thu))
precip_cor[260,3] = test$estimate
precip_cor[260,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M17Thu, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Oct[1:47]))
test
plot(rwi$M17Thu, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M17Thu))
precip_cor[261,3] = test$estimate
precip_cor[261,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M17Thu, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Nov[1:47]))
test
plot(rwi$M17Thu, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M17Thu))
precip_cor[262,3] = test$estimate
precip_cor[262,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M17Thu, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Dec[1:47]))
test
plot(rwi$M17Thu, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M17Thu))
precip_cor[263,3] = test$estimate
precip_cor[263,4] = test$p.value

# Current January
test = my.cor.test(rwi$M17Thu, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Jan[2:48]))
test
plot(rwi$M17Thu, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M17Thu))
precip_cor[264,3] = test$estimate
precip_cor[264,4] = test$p.value

# Current February
test = my.cor.test(rwi$M17Thu, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Feb[2:48]))
test
plot(rwi$M17Thu, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M17Thu))
precip_cor[265,3] = test$estimate
precip_cor[265,4] = test$p.value

# Current March
test = my.cor.test(rwi$M17Thu, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Mar[2:48]))
test
plot(rwi$M17Thu, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M17Thu))
precip_cor[266,3] = test$estimate
precip_cor[266,4] = test$p.value

# Current April
test = my.cor.test(rwi$M17Thu, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Apr[2:48]))
test
plot(rwi$M17Thu, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M17Thu))
precip_cor[267,3] = test$estimate
precip_cor[267,4] = test$p.value

# Current May
test = my.cor.test(rwi$M17Thu, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$May[2:48]))
test
plot(rwi$M17Thu, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M17Thu))
precip_cor[268,3] = test$estimate
precip_cor[268,4] = test$p.value

# Current June
test = my.cor.test(rwi$M17Thu, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Jun[2:48]))
test
plot(rwi$M17Thu, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M17Thu))
precip_cor[269,3] = test$estimate
precip_cor[269,4] = test$p.value

# Current July
test = my.cor.test(rwi$M17Thu, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Jul[2:48]))
test
plot(rwi$M17Thu, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M17Thu))
precip_cor[270,3] = test$estimate
precip_cor[270,4] = test$p.value

# Current August
test = my.cor.test(rwi$M17Thu, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Aug[2:48]))
test
plot(rwi$M17Thu, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M17Thu))
precip_cor[271,3] = test$estimate
precip_cor[271,4] = test$p.value

# Current September
test = my.cor.test(rwi$M17Thu, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Sep[2:48]))
test
plot(rwi$M17Thu, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M17Thu))
precip_cor[272,3] = test$estimate
precip_cor[272,4] = test$p.value

### LTS ###
# Previous May
test = my.cor.test(lts$M17, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$May[1:47]))
test
plot(lts$M17, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ lts$M17))
precip_cor[256,6] = test$estimate
precip_cor[256,7] = test$p.value

# Previous June
test = my.cor.test(lts$M17, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Jun[1:47]))
test
plot(lts$M17, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ lts$M17))
precip_cor[257,6] = test$estimate
precip_cor[257,7] = test$p.value

# Previous July
test = my.cor.test(lts$M17, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Jul[1:47]))
test
plot(lts$M17, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ lts$M17))
precip_cor[258,6] = test$estimate
precip_cor[258,7] = test$p.value

# Previous August
test = my.cor.test(lts$M17, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Aug[1:47]))
test
plot(lts$M17, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ lts$M17))
precip_cor[259,6] = test$estimate
precip_cor[259,7] = test$p.value

# Previous September
test = my.cor.test(lts$M17, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Sep[1:47]))
test
plot(lts$M17, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ lts$M17))
precip_cor[260,6] = test$estimate
precip_cor[260,7] = test$p.value

# Previous October
test = my.cor.test(lts$M17, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Oct[1:47]))
test
plot(lts$M17, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ lts$M17))
precip_cor[261,6] = test$estimate
precip_cor[261,7] = test$p.value

# Previous November
test = my.cor.test(lts$M17, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Nov[1:47]))
test
plot(lts$M17, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ lts$M17))
precip_cor[262,6] = test$estimate
precip_cor[262,7] = test$p.value

# Previous December
test = my.cor.test(lts$M17, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Dec[1:47]))
test
plot(lts$M17, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ lts$M17))
precip_cor[263,6] = test$estimate
precip_cor[263,7] = test$p.value

# Current January
test = my.cor.test(lts$M17, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Jan[2:48]))
test
plot(lts$M17, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ lts$M17))
precip_cor[264,6] = test$estimate
precip_cor[264,7] = test$p.value

# Current February
test = my.cor.test(lts$M17, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Feb[2:48]))
test
plot(lts$M17, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ lts$M17))
precip_cor[265,6] = test$estimate
precip_cor[265,7] = test$p.value

# Current March
test = my.cor.test(lts$M17, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Mar[2:48]))
test
plot(lts$M17, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ lts$M17))
precip_cor[266,6] = test$estimate
precip_cor[266,7] = test$p.value

# Current April
test = my.cor.test(lts$M17, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Apr[2:48]))
test
plot(lts$M17, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ lts$M17))
precip_cor[267,6] = test$estimate
precip_cor[267,7] = test$p.value

# Current May
test = my.cor.test(lts$M17, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$May[2:48]))
test
plot(lts$M17, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ lts$M17))
precip_cor[268,6] = test$estimate
precip_cor[268,7] = test$p.value

# Current June
test = my.cor.test(lts$M17, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Jun[2:48]))
test
plot(lts$M17, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ lts$M17))
precip_cor[269,6] = test$estimate
precip_cor[269,7] = test$p.value

# Current July
test = my.cor.test(lts$M17, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Jul[2:48]))
test
plot(lts$M17, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ lts$M17))
precip_cor[270,6] = test$estimate
precip_cor[270,7] = test$p.value

# Current August
test = my.cor.test(lts$M17, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Aug[2:48]))
test
plot(lts$M17, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ lts$M17))
precip_cor[271,6] = test$estimate
precip_cor[271,7] = test$p.value

# Current September
test = my.cor.test(lts$M17, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M17, precip_P$Sep[2:48]))
test
plot(lts$M17, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ lts$M17))
precip_cor[272,6] = test$estimate
precip_cor[272,7] = test$p.value
#####

##### Mean Precipitation: Site 02Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F15Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$May[1:47]))
test
plot(rwi$F15Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F15Ace))
precip_cor[273,3] = test$estimate
precip_cor[273,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F15Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Jun[1:47]))
test
plot(rwi$F15Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F15Ace))
precip_cor[274,3] = test$estimate
precip_cor[274,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F15Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Jul[1:47]))
test
plot(rwi$F15Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F15Ace))
precip_cor[275,3] = test$estimate
precip_cor[275,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F15Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Aug[1:47]))
test
plot(rwi$F15Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F15Ace))
precip_cor[276,3] = test$estimate
precip_cor[276,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F15Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Sep[1:47]))
test
plot(rwi$F15Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F15Ace))
precip_cor[277,3] = test$estimate
precip_cor[277,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F15Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Oct[1:47]))
test
plot(rwi$F15Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F15Ace))
precip_cor[278,3] = test$estimate
precip_cor[278,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F15Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Nov[1:47]))
test
plot(rwi$F15Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F15Ace))
precip_cor[279,3] = test$estimate
precip_cor[279,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F15Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Dec[1:47]))
test
plot(rwi$F15Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F15Ace))
precip_cor[280,3] = test$estimate
precip_cor[280,4] = test$p.value

# Current January
test = my.cor.test(rwi$F15Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Jan[2:48]))
test
plot(rwi$F15Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F15Ace))
precip_cor[281,3] = test$estimate
precip_cor[281,4] = test$p.value

# Current February
test = my.cor.test(rwi$F15Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Feb[2:48]))
test
plot(rwi$F15Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F15Ace))
precip_cor[282,3] = test$estimate
precip_cor[282,4] = test$p.value

# Current March
test = my.cor.test(rwi$F15Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Mar[2:48]))
test
plot(rwi$F15Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F15Ace))
precip_cor[283,3] = test$estimate
precip_cor[283,4] = test$p.value

# Current April
test = my.cor.test(rwi$F15Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Apr[2:48]))
test
plot(rwi$F15Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F15Ace))
precip_cor[284,3] = test$estimate
precip_cor[284,4] = test$p.value

# Current May
test = my.cor.test(rwi$F15Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$May[2:48]))
test
plot(rwi$F15Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F15Ace))
precip_cor[285,3] = test$estimate
precip_cor[285,4] = test$p.value

# Current June
test = my.cor.test(rwi$F15Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Jun[2:48]))
test
plot(rwi$F15Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F15Ace))
precip_cor[286,3] = test$estimate
precip_cor[286,4] = test$p.value

# Current July
test = my.cor.test(rwi$F15Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Jul[2:48]))
test
plot(rwi$F15Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F15Ace))
precip_cor[287,3] = test$estimate
precip_cor[287,4] = test$p.value

# Current August
test = my.cor.test(rwi$F15Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Aug[2:48]))
test
plot(rwi$F15Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F15Ace))
precip_cor[288,3] = test$estimate
precip_cor[288,4] = test$p.value

# Current September
test = my.cor.test(rwi$F15Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Sep[2:48]))
test
plot(rwi$F15Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F15Ace))
precip_cor[289,3] = test$estimate
precip_cor[289,4] = test$p.value
#####

##### Mean Precipitation: Site 03Que #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M06Que, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$May[1:47]))
test
plot(rwi$M06Que, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M06Que))
precip_cor[290,3] = test$estimate
precip_cor[290,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M06Que, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Jun[1:47]))
test
plot(rwi$M06Que, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M06Que))
precip_cor[291,3] = test$estimate
precip_cor[291,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M06Que, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Jul[1:47]))
test
plot(rwi$M06Que, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M06Que))
precip_cor[292,3] = test$estimate
precip_cor[292,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M06Que, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Aug[1:47]))
test
plot(rwi$M06Que, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M06Que))
precip_cor[293,3] = test$estimate
precip_cor[293,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M06Que, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Sep[1:47]))
test
plot(rwi$M06Que, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M06Que))
precip_cor[294,3] = test$estimate
precip_cor[294,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M06Que, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Oct[1:47]))
test
plot(rwi$M06Que, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M06Que))
precip_cor[295,3] = test$estimate
precip_cor[295,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M06Que, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Nov[1:47]))
test
plot(rwi$M06Que, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M06Que))
precip_cor[296,3] = test$estimate
precip_cor[296,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M06Que, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Dec[1:47]))
test
plot(rwi$M06Que, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M06Que))
precip_cor[297,3] = test$estimate
precip_cor[297,4] = test$p.value

# Current January
test = my.cor.test(rwi$M06Que, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Jan[2:48]))
test
plot(rwi$M06Que, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M06Que))
precip_cor[298,3] = test$estimate
precip_cor[298,4] = test$p.value

# Current February
test = my.cor.test(rwi$M06Que, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Feb[2:48]))
test
plot(rwi$M06Que, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M06Que))
precip_cor[299,3] = test$estimate
precip_cor[299,4] = test$p.value

# Current March
test = my.cor.test(rwi$M06Que, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Mar[2:48]))
test
plot(rwi$M06Que, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M06Que))
precip_cor[300,3] = test$estimate
precip_cor[300,4] = test$p.value

# Current April
test = my.cor.test(rwi$M06Que, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Apr[2:48]))
test
plot(rwi$M06Que, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M06Que))
precip_cor[301,3] = test$estimate
precip_cor[301,4] = test$p.value

# Current May
test = my.cor.test(rwi$M06Que, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$May[2:48]))
test
plot(rwi$M06Que, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M06Que))
precip_cor[302,3] = test$estimate
precip_cor[302,4] = test$p.value

# Current June
test = my.cor.test(rwi$M06Que, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Jun[2:48]))
test
plot(rwi$M06Que, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M06Que))
precip_cor[303,3] = test$estimate
precip_cor[303,4] = test$p.value

# Current July
test = my.cor.test(rwi$M06Que, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Jul[2:48]))
test
plot(rwi$M06Que, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M06Que))
precip_cor[304,3] = test$estimate
precip_cor[304,4] = test$p.value

# Current August
test = my.cor.test(rwi$M06Que, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Aug[2:48]))
test
plot(rwi$M06Que, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M06Que))
precip_cor[305,3] = test$estimate
precip_cor[305,4] = test$p.value

# Current September
test = my.cor.test(rwi$M06Que, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Sep[2:48]))
test
plot(rwi$M06Que, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M06Que))
precip_cor[306,3] = test$estimate
precip_cor[306,4] = test$p.value
#####

##### Mean Precipitation: Site 10Dec #####
### RWI ###
# Previous May
test = my.cor.test(rwi$M26Dec, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$May[1:47]))
test
plot(rwi$M26Dec, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$M26Dec))
precip_cor[307,3] = test$estimate
precip_cor[307,4] = test$p.value

# Previous June
test = my.cor.test(rwi$M26Dec, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Jun[1:47]))
test
plot(rwi$M26Dec, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$M26Dec))
precip_cor[308,3] = test$estimate
precip_cor[308,4] = test$p.value

# Previous July
test = my.cor.test(rwi$M26Dec, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Jul[1:47]))
test
plot(rwi$M26Dec, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$M26Dec))
precip_cor[309,3] = test$estimate
precip_cor[309,4] = test$p.value

# Previous August
test = my.cor.test(rwi$M26Dec, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Aug[1:47]))
test
plot(rwi$M26Dec, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$M26Dec))
precip_cor[310,3] = test$estimate
precip_cor[310,4] = test$p.value

# Previous September
test = my.cor.test(rwi$M26Dec, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Sep[1:47]))
test
plot(rwi$M26Dec, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$M26Dec))
precip_cor[311,3] = test$estimate
precip_cor[311,4] = test$p.value

# Previous October
test = my.cor.test(rwi$M26Dec, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Oct[1:47]))
test
plot(rwi$M26Dec, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$M26Dec))
precip_cor[312,3] = test$estimate
precip_cor[312,4] = test$p.value

# Previous November
test = my.cor.test(rwi$M26Dec, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Nov[1:47]))
test
plot(rwi$M26Dec, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$M26Dec))
precip_cor[313,3] = test$estimate
precip_cor[313,4] = test$p.value

# Previous December
test = my.cor.test(rwi$M26Dec, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Dec[1:47]))
test
plot(rwi$M26Dec, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$M26Dec))
precip_cor[314,3] = test$estimate
precip_cor[314,4] = test$p.value

# Current January
test = my.cor.test(rwi$M26Dec, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Jan[2:48]))
test
plot(rwi$M26Dec, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$M26Dec))
precip_cor[315,3] = test$estimate
precip_cor[315,4] = test$p.value

# Current February
test = my.cor.test(rwi$M26Dec, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Feb[2:48]))
test
plot(rwi$M26Dec, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$M26Dec))
precip_cor[316,3] = test$estimate
precip_cor[316,4] = test$p.value

# Current March
test = my.cor.test(rwi$M26Dec, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Mar[2:48]))
test
plot(rwi$M26Dec, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$M26Dec))
precip_cor[317,3] = test$estimate
precip_cor[317,4] = test$p.value

# Current April
test = my.cor.test(rwi$M26Dec, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Apr[2:48]))
test
plot(rwi$M26Dec, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$M26Dec))
precip_cor[318,3] = test$estimate
precip_cor[318,4] = test$p.value

# Current May
test = my.cor.test(rwi$M26Dec, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$May[2:48]))
test
plot(rwi$M26Dec, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$M26Dec))
precip_cor[319,3] = test$estimate
precip_cor[319,4] = test$p.value

# Current June
test = my.cor.test(rwi$M26Dec, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Jun[2:48]))
test
plot(rwi$M26Dec, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$M26Dec))
precip_cor[320,3] = test$estimate
precip_cor[320,4] = test$p.value

# Current July
test = my.cor.test(rwi$M26Dec, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Jul[2:48]))
test
plot(rwi$M26Dec, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$M26Dec))
precip_cor[321,3] = test$estimate
precip_cor[321,4] = test$p.value

# Current August
test = my.cor.test(rwi$M26Dec, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Aug[2:48]))
test
plot(rwi$M26Dec, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$M26Dec))
precip_cor[322,3] = test$estimate
precip_cor[322,4] = test$p.value

# Current September
test = my.cor.test(rwi$M26Dec, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Sep[2:48]))
test
plot(rwi$M26Dec, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$M26Dec))
precip_cor[323,3] = test$estimate
precip_cor[323,4] = test$p.value
#####

##### Mean Precipitation: Site 15Ace #####
### RWI ###
# Previous May
test = my.cor.test(rwi$F30Ace, precip_P$May[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$May[1:47]))
test
plot(rwi$F30Ace, precip_P$May[1:47])
abline(lm(precip_P$May[1:47] ~ rwi$F30Ace))
precip_cor[324,3] = test$estimate
precip_cor[324,4] = test$p.value

# Previous June
test = my.cor.test(rwi$F30Ace, precip_P$Jun[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Jun[1:47]))
test
plot(rwi$F30Ace, precip_P$Jun[1:47])
abline(lm(precip_P$Jun[1:47] ~ rwi$F30Ace))
precip_cor[325,3] = test$estimate
precip_cor[325,4] = test$p.value

# Previous July
test = my.cor.test(rwi$F30Ace, precip_P$Jul[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Jul[1:47]))
test
plot(rwi$F30Ace, precip_P$Jul[1:47])
abline(lm(precip_P$Jul[1:47] ~ rwi$F30Ace))
precip_cor[326,3] = test$estimate
precip_cor[326,4] = test$p.value

# Previous August
test = my.cor.test(rwi$F30Ace, precip_P$Aug[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Aug[1:47]))
test
plot(rwi$F30Ace, precip_P$Aug[1:47])
abline(lm(precip_P$Aug[1:47] ~ rwi$F30Ace))
precip_cor[327,3] = test$estimate
precip_cor[327,4] = test$p.value

# Previous September
test = my.cor.test(rwi$F30Ace, precip_P$Sep[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Sep[1:47]))
test
plot(rwi$F30Ace, precip_P$Sep[1:47])
abline(lm(precip_P$Sep[1:47] ~ rwi$F30Ace))
precip_cor[328,3] = test$estimate
precip_cor[328,4] = test$p.value

# Previous October
test = my.cor.test(rwi$F30Ace, precip_P$Oct[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Oct[1:47]))
test
plot(rwi$F30Ace, precip_P$Oct[1:47])
abline(lm(precip_P$Oct[1:47] ~ rwi$F30Ace))
precip_cor[329,3] = test$estimate
precip_cor[329,4] = test$p.value

# Previous November
test = my.cor.test(rwi$F30Ace, precip_P$Nov[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Nov[1:47]))
test
plot(rwi$F30Ace, precip_P$Nov[1:47])
abline(lm(precip_P$Nov[1:47] ~ rwi$F30Ace))
precip_cor[330,3] = test$estimate
precip_cor[330,4] = test$p.value

# Previous December
test = my.cor.test(rwi$F30Ace, precip_P$Dec[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Dec[1:47]))
test
plot(rwi$F30Ace, precip_P$Dec[1:47])
abline(lm(precip_P$Dec[1:47] ~ rwi$F30Ace))
precip_cor[331,3] = test$estimate
precip_cor[331,4] = test$p.value

# Current January
test = my.cor.test(rwi$F30Ace, precip_P$Jan[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Jan[2:48]))
test
plot(rwi$F30Ace, precip_P$Jan[2:48])
abline(lm(precip_P$Jan[2:48] ~ rwi$F30Ace))
precip_cor[332,3] = test$estimate
precip_cor[332,4] = test$p.value

# Current February
test = my.cor.test(rwi$F30Ace, precip_P$Feb[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Feb[2:48]))
test
plot(rwi$F30Ace, precip_P$Feb[2:48])
abline(lm(precip_P$Feb[2:48] ~ rwi$F30Ace))
precip_cor[333,3] = test$estimate
precip_cor[333,4] = test$p.value

# Current March
test = my.cor.test(rwi$F30Ace, precip_P$Mar[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Mar[2:48]))
test
plot(rwi$F30Ace, precip_P$Mar[2:48])
abline(lm(precip_P$Mar[2:48] ~ rwi$F30Ace))
precip_cor[334,3] = test$estimate
precip_cor[334,4] = test$p.value

# Current April
test = my.cor.test(rwi$F30Ace, precip_P$Apr[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Apr[2:48]))
test
plot(rwi$F30Ace, precip_P$Apr[2:48])
abline(lm(precip_P$Apr[2:48] ~ rwi$F30Ace))
precip_cor[335,3] = test$estimate
precip_cor[335,4] = test$p.value

# Current May
test = my.cor.test(rwi$F30Ace, precip_P$May[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$May[2:48]))
test
plot(rwi$F30Ace, precip_P$May[2:48])
abline(lm(precip_P$May[2:48] ~ rwi$F30Ace))
precip_cor[336,3] = test$estimate
precip_cor[336,4] = test$p.value

# Current June
test = my.cor.test(rwi$F30Ace, precip_P$Jun[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Jun[2:48]))
test
plot(rwi$F30Ace, precip_P$Jun[2:48])
abline(lm(precip_P$Jun[2:48] ~ rwi$F30Ace))
precip_cor[337,3] = test$estimate
precip_cor[337,4] = test$p.value

# Current July
test = my.cor.test(rwi$F30Ace, precip_P$Jul[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Jul[2:48]))
test
plot(rwi$F30Ace, precip_P$Jul[2:48])
abline(lm(precip_P$Jul[2:48] ~ rwi$F30Ace))
precip_cor[338,3] = test$estimate
precip_cor[338,4] = test$p.value

# Current August
test = my.cor.test(rwi$F30Ace, precip_P$Aug[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Aug[2:48]))
test
plot(rwi$F30Ace, precip_P$Aug[2:48])
abline(lm(precip_P$Aug[2:48] ~ rwi$F30Ace))
precip_cor[339,3] = test$estimate
precip_cor[339,4] = test$p.value

# Current September
test = my.cor.test(rwi$F30Ace, precip_P$Sep[2:48], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Sep[2:48]))
test
plot(rwi$F30Ace, precip_P$Sep[2:48])
abline(lm(precip_P$Sep[2:48] ~ rwi$F30Ace))
precip_cor[340,3] = test$estimate
precip_cor[340,4] = test$p.value
#####

# Significance
precip_cor$sig_rwi <- ifelse(precip_cor$pval_rwi < 0.055, "Yes", "No")
precip_cor$sig_lts <- ifelse(precip_cor$pval_lts < 0.055, "Yes", "No")

#write.csv(precip_cor, "precip_cor_20.csv")

##### Total Precipitation Figure #####
p_cor01.p = ggplot(data = subset(precip_cor, site == "01Tsu")) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("01Tsu (0.64**)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
p_cor01.p

p_cor02.p = ggplot(data = subset(precip_cor, site == "02Que")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(precip_cor, site == "02Ace"),
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(precip_cor, site == "02Ace"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(precip_cor, site == "02Ace"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >**02Que (0.28*)**</span> / 
                <span style= 'color:darkgreen;' >02Ace (-0.13)</span>") +
  #ggtitle("02Que (0.28*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
p_cor02.p

p_cor03.p = ggplot(data = subset(precip_cor, site == "03Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(precip_cor, site == "03Que"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(precip_cor, site == "03Que"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(precip_cor, site == "03Que"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >**03Ace (0.24*)**</span> / 
                <span style= 'color:darkgreen;' >03Que (0.02)</span>") + 
  #ggtitle("03Ace (0.24*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
p_cor03.p

p_cor04.p = ggplot(data = subset(precip_cor, site == "04Tsu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("04Tsu (0.16)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
p_cor04.p

p_cor05.p = ggplot(data = subset(precip_cor, site == "05Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("05Ace (-0.21)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
p_cor05.p

p_cor06.p = ggplot(data = subset(precip_cor, site == "06Dec")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("06Dec (0.37*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
p_cor06.p

p_cor07.p = ggplot(data = subset(precip_cor, site == "07Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("07Thu (0.37*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
p_cor07.p

p_cor08.p = ggplot(data = subset(precip_cor, site == "08Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("08Ace (-0.12)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
p_cor08.p

p_cor09.p = ggplot(data = subset(precip_cor, site == "09Pin")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("09Pin (0.20)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
p_cor09.p

p_cor10.p = ggplot(data = subset(precip_cor, site == "10Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(precip_cor, site == "10Dec"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(data = subset(precip_cor, site == "10Dec"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(data = subset(precip_cor, site == "10Dec"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:red3;' >**10Thu (0.55*)**</span> / 
                <span style= 'color:blue;' >10Dec (-0.17)</span>") + 
  #ggtitle("10Thu (0.55*)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
p_cor10.p

p_cor11.p = ggplot(data = subset(precip_cor, site == "11Ace")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("11Ace (-0.22)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"),
        legend.position = "none")
p_cor11.p

p_cor12.p = ggplot(data = subset(precip_cor, site == "12Pop")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("12Pop (0.57**)") +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"),
        legend.position = "none")
p_cor12.p

p_cor13.p = ggplot(data = subset(precip_cor, site == "13Pic")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("13Pic (0.31*)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = "Correlation with Monthly Total Precipitation", expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_text(size = 12, hjust = -0.55, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
p_cor13.p

p_cor14.p = ggplot(data = subset(precip_cor, site == "14Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("14Thu (0.10)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"),
        legend.position = "none")
p_cor14.p

p_cor15.p = ggplot(data = subset(precip_cor, site == "15Bet")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(data = subset(precip_cor, site == "15Ace"), 
            aes(x = temp, y = cor_rwi, group = 1), col = "darkgreen") +
  geom_point(data = subset(precip_cor, site == "15Ace"), aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "darkgreen", col = "darkgreen") + # Background
  geom_point(data = subset(precip_cor, site == "15Ace"), 
             aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "darkgreen") +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "blue") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "blue", col = "blue") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "blue") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  labs(title = "<span style= 'color:blue;' >15Bet (-0.10)</span> / 
                <span style= 'color:darkgreen;' >15Ace (-0.11)</span>") + 
  #ggtitle("15Bet (-0.10)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1),
        #plot.title = element_text(size = 10, vjust = -1),
        legend.position = "none")
p_cor15.p

p_cor16.p = ggplot(data = subset(precip_cor, site == "16Thu")) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_line(aes(x = temp, y = cor_rwi, group = 1), col = "red3") +
  geom_point(aes(x = temp, y = cor_rwi, group = 1), 
             shape = 21, size = 1.5, fill = "red3", col = "red3") + # Background
  geom_point(aes(x = temp, y = cor_rwi, group = 1, fill = sig_rwi), 
             shape = 21, size = 1.5, col = "red3") +
  geom_line(aes(x = temp, y = cor_lts, group = 1), col = "black") +
  geom_point(aes(x = temp, y = cor_lts, group = 1), 
             shape = 21, size = 1.5, fill = "black", col = "black") + # Background
  geom_point(aes(x = temp, y = cor_lts, group = 1, fill = sig_lts), 
             shape = 21, size = 1.5, col = "black") +
  scale_fill_manual(values = c("white", NA)) +
  ggtitle("16Thu (0.43*)") +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 8),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"),
        legend.position = "none")
p_cor16.p
#####
tiff("precip_cor1_20.tiff", units = "in", width = 6.5, height = 6.5, res = 300)
(p_cor01.p | p_cor02.p | p_cor03.p | p_cor04.p) /
  (p_cor05.p | p_cor06.p | p_cor07.p | p_cor08.p) / 
  (p_cor09.p | p_cor10.p | p_cor11.p | p_cor12.p) /
  (p_cor13.p | p_cor14.p | p_cor15.p | p_cor16.p)
dev.off()

##### Total Precipitation Summary Figure #####
precip_cor_rwi_sum.p = ggplot(data = precip_cor) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_point(aes(x = temp, y = cor_rwi, col = type), alpha = 0.5) + # Background
  geom_point(aes(x = temp, y = cor_rwi, col = type, fill = sig_rwi), shape = 21) +
  stat_summary(aes(x = temp, y = cor_rwi, group = type, col = type), 
               geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_rwi, group = 1), geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_rwi, group = type, col = type), 
  #             geom = "point", fun = "mean", shape = 4, size = 4) +
  scale_y_continuous(name = "Correlation with Monthly Total Precipitation", 
                     expand = c(0,0), limits = c(-0.72,0.72), #0.76 for mn 
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_color_manual(values = c("red3", "blue")) +
  scale_fill_manual(values = c("white", "#00000000")) +
  ggtitle("C. RWI") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
precip_cor_rwi_sum.p

precip_cor_lts_sum.p = ggplot(data = precip_cor) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_vline(xintercept = 1.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 4.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 7.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 10.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 13.5, col = "gray48", lty = 3) +
  geom_vline(xintercept = 16.5, col = "gray48", lty = 3) +
  geom_point(aes(x = temp, y = cor_lts, col = type), alpha = 0.5) + # Background
  geom_point(aes(x = temp, y = cor_lts, col = type, fill = sig_lts), shape = 21) +
  stat_summary(aes(x = temp, y = cor_lts, group = type, col = type), 
               geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_lts, group = 1), geom = "line", fun = "mean") +
  #stat_summary(aes(x = temp, y = cor_lts, group = type, col = type), 
  #             geom = "point", fun = "mean", shape = 4, size = 4) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.72,0.72), #0.76 for mn
                     breaks = seq(-0.6,0.6,length.out = 7)) +
  scale_x_discrete(name = NULL, labels = c("may" = "m", "jun" = "j", "jul" = "j",
                                           "aug" = "a", "sep" = "s", "oct" = "o",
                                           "nov" = "n", "dec" = "d", "JAN" = "J",
                                           "FEB" = "F", "MAR" = "M", "APR" = "A",
                                           "MAY" = "M", "JUN" = "J", "JUL" = "J",
                                           "AUG" = "A", "SEP" = "S")) +
  scale_color_manual(values = c("red3", "blue")) +
  scale_fill_manual(values = c("white", "#00000000")) +
  ggtitle("D. CC (%)") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"),
        legend.position = "none")
precip_cor_lts_sum.p
#####
#tiff("precip_cor_sum1.tiff", units = "in", width = 6.5, height = 4, res = 300)
#precip_cor_rwi_sum.p | precip_cor_lts_sum.p
#dev.off()

temp.p = temp_cor_rwi_sum.p | temp_cor_lts_sum.p
precip.p = precip_cor_rwi_sum.p | precip_cor_lts_sum.p

tiff("climate_cor_sum2_20_tempmm.tiff", units = "in", width = 6.5, height = 8, res = 300)
temp.p / precip.p
dev.off()

# Seasonal Precip Test

# RWI
my.cor.test(rwi$M13Tsu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, precip_P$Summer[2:48]))
my.cor.test(rwi$F15Que, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F15Que, precip_P$Summer[2:48]))
my.cor.test(rwi$F15Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F15Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$M06Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M06Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$M06Que, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M06Que, precip_P$Summer[2:48]))
my.cor.test(rwi$M07Tsu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M07Tsu, precip_P$Summer[2:48]))
my.cor.test(rwi$F23Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F23Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$F21Dec, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F21Dec, precip_P$Summer[2:48]))
my.cor.test(rwi$M05Thu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M05Thu, precip_P$Summer[2:48]))
my.cor.test(rwi$F07Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F07Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$M27Pin[10:47], precip_P$Summer[11:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M27Pin[10:47], precip_P$Summer[11:48]))
my.cor.test(rwi$M26Thu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M26Thu, precip_P$Summer[2:48]))
my.cor.test(rwi$M26Dec, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M26Dec, precip_P$Summer[2:48]))
my.cor.test(rwi$F25Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F25Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$M01Pop, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M01Pop, precip_P$Summer[2:48]))
my.cor.test(rwi$F33Pic, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F33Pic, precip_P$Summer[2:48]))
my.cor.test(rwi$M20Thu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M20Thu, precip_P$Summer[2:48]))
my.cor.test(rwi$F30Bet, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F30Bet, precip_P$Summer[2:48]))
my.cor.test(rwi$F30Ace, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F30Ace, precip_P$Summer[2:48]))
my.cor.test(rwi$M17Thu, precip_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M17Thu, precip_P$Summer[2:48]))
# LTS
my.cor.test(lts$M13, precip_P$Summer[1:47], alternative = "two.sided", 
                   method = "pearson", n = calc.neff(lts$M13, precip_P$Summer[1:47]))
my.cor.test(lts$F15, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F15, precip_P$Summer[1:47]))
my.cor.test(lts$M06, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M06, precip_P$Summer[1:47]))
my.cor.test(lts$M07, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M07, precip_P$Summer[1:47]))
my.cor.test(lts$F23, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F23, precip_P$Summer[1:47]))
my.cor.test(lts$F21, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F21, precip_P$Summer[1:47]))
my.cor.test(lts$M05, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M05, precip_P$Summer[1:47]))
my.cor.test(lts$F07, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F07, precip_P$Summer[1:47]))
my.cor.test(lts$M27[10:47], precip_P$Summer[10:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M27[10:47], precip_P$Summer[10:47]))
my.cor.test(lts$M26, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M26, precip_P$Summer[1:47]))
my.cor.test(lts$F25, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F25, precip_P$Summer[1:47]))
my.cor.test(lts$M01, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M01, precip_P$Summer[1:47]))
my.cor.test(lts$F33, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F33, precip_P$Summer[1:47]))
my.cor.test(lts$M20, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M20, precip_P$Summer[1:47]))
my.cor.test(lts$F30, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F30, precip_P$Summer[1:47]))
my.cor.test(lts$M17, precip_P$Summer[1:47], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M17, precip_P$Summer[1:47]))

# Seasonal Temp Test

# RWI
my.cor.test(rwi$M13Tsu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M13Tsu, temp_P$Summer[2:48]))
my.cor.test(rwi$F15Que, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F15Que, temp_P$Summer[2:48]))
my.cor.test(rwi$F15Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F15Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$M06Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M06Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$M06Que, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M06Que, temp_P$Summer[2:48]))
my.cor.test(rwi$M07Tsu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M07Tsu, temp_P$Summer[2:48]))
my.cor.test(rwi$F23Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F23Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$F21Dec, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F21Dec, temp_P$Summer[2:48]))
my.cor.test(rwi$M05Thu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M05Thu, temp_P$Summer[2:48]))
my.cor.test(rwi$F07Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F07Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$M27Pin[10:47], temp_P$Summer[11:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M27Pin[10:47], temp_P$Summer[11:48]))
my.cor.test(rwi$M26Thu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M26Thu, temp_P$Summer[2:48]))
my.cor.test(rwi$M26Dec, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M26Dec, temp_P$Summer[2:48]))
my.cor.test(rwi$F25Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F25Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$M01Pop, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M01Pop, temp_P$Summer[2:48]))
my.cor.test(rwi$F33Pic, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F33Pic, temp_P$Summer[2:48]))
my.cor.test(rwi$M20Thu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M20Thu, temp_P$Summer[2:48]))
my.cor.test(rwi$F30Bet, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F30Bet, temp_P$Summer[2:48]))
my.cor.test(rwi$F30Ace, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$F30Ace, temp_P$Summer[2:48]))
my.cor.test(rwi$M17Thu, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(rwi$M17Thu, temp_P$Summer[2:48]))
# LTS
my.cor.test(lts$M13, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M13, temp_P$Summer[2:48]))
my.cor.test(lts$F15, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F15, temp_P$Summer[2:48]))
my.cor.test(lts$M06, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M06, temp_P$Summer[2:48]))
my.cor.test(lts$M07, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M07, temp_P$Summer[2:48]))
my.cor.test(lts$F23, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F23, temp_P$Summer[2:48]))
my.cor.test(lts$F21, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F21, temp_P$Summer[2:48]))
my.cor.test(lts$M05, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M05, temp_P$Summer[2:48]))
my.cor.test(lts$F07, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F07, temp_P$Summer[2:48]))
my.cor.test(lts$M27[10:47], temp_P$Summer[11:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M27[10:47], temp_P$Summer[11:48]))
my.cor.test(lts$M26, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M26, temp_P$Summer[2:48]))
my.cor.test(lts$F25, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F25, temp_P$Summer[2:48]))
my.cor.test(lts$M01, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M01, temp_P$Summer[2:48]))
my.cor.test(lts$F33, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F33, temp_P$Summer[2:48]))
my.cor.test(lts$M20, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M20, temp_P$Summer[2:48]))
my.cor.test(lts$F30, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$F30, temp_P$Summer[2:48]))
my.cor.test(lts$M17, temp_P$Summer[2:48], alternative = "two.sided", 
            method = "pearson", n = calc.neff(lts$M17, temp_P$Summer[2:48]))

subset(temp_cor$cor_rwi, temp_cor$type == "Con" & temp_cor$temp == "APR")
subset(temp_cor$sig_rwi, temp_cor$type == "Con" & temp_cor$temp == "APR")
mean(subset(temp_cor$cor_rwi, temp_cor$type == "Con" & temp_cor$temp == "APR"))
sd(subset(temp_cor$cor_rwi, temp_cor$type == "Con" & temp_cor$temp == "APR"))

subset(temp_cor$cor_rwi, temp_cor$type == "Dec" & temp_cor$temp == "AUG")
subset(temp_cor$sig_rwi, temp_cor$type == "Dec" & temp_cor$temp == "AUG")
mean(subset(temp_cor$cor_rwi, temp_cor$type == "Dec" & temp_cor$temp == "AUG"))
sd(subset(temp_cor$cor_rwi, temp_cor$type == "Dec" & temp_cor$temp == "AUG"))

subset(temp_cor$cor_lts, temp_cor$type == "Con" & temp_cor$temp == "JUN")
subset(temp_cor$sig_lts, temp_cor$type == "Con" & temp_cor$temp == "JUN")
mean(subset(temp_cor$cor_lts, temp_cor$type == "Con" & temp_cor$temp == "JUN"))
sd(subset(temp_cor$cor_lts, temp_cor$type == "Con" & temp_cor$temp == "JUN"))

subset(temp_cor$cor_lts, temp_cor$type == "Dec" & temp_cor$temp == "JUN")
subset(temp_cor$sig_lts, temp_cor$type == "Dec" & temp_cor$temp == "JUN")
mean(subset(temp_cor$cor_lts, temp_cor$type == "Dec" & temp_cor$temp == "JUN"), na.rm = TRUE)
sd(subset(temp_cor$cor_lts, temp_cor$type == "Dec" & temp_cor$temp == "JUN"), na.rm = TRUE)

subset(temp_cor$sig_rwi, temp_cor$type == "Con" & temp_cor$cor_rwi > 0 & temp_cor$sig_rwi == "Yes")
subset(temp_cor$sig_rwi, temp_cor$type == "Dec" & temp_cor$cor_rwi > 0 & temp_cor$sig_rwi == "Yes")

subset(temp_cor$sig_rwi, temp_cor$type == "Con" & temp_cor$cor_rwi < 0 & temp_cor$sig_rwi == "Yes")
subset(temp_cor$sig_rwi, temp_cor$type == "Dec" & temp_cor$cor_rwi < 0 & temp_cor$sig_rwi == "Yes")

subset(temp_cor$sig_lts, temp_cor$type == "Con" & temp_cor$cor_lts > 0 & temp_cor$sig_lts == "Yes")
subset(temp_cor$sig_lts, temp_cor$type == "Dec" & temp_cor$cor_lts > 0 & temp_cor$sig_lts == "Yes")

subset(precip_cor$cor_rwi, precip_cor$temp == "jul")
subset(precip_cor$sig_rwi, precip_cor$temp == "jul")
mean(subset(precip_cor$cor_rwi, precip_cor$temp == "jul"))
sd(subset(precip_cor$cor_rwi, precip_cor$temp == "jul"))
