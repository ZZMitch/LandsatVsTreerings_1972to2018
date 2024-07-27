# this calculates neff between x and y
calc.neff <- function(x,y){
  x.ar1 = acf(x,plot=F)
  #sig.lvl = qnorm((1 + 0.9)/2)/sqrt(x.ar1$n.used) #  Changes sig level.
  sig.lvl = qnorm((1 + 0.95)/2)/sqrt(x.ar1$n.used)
  x.ar1 = x.ar1$acf[2,1,1]
  x.ar1 = ifelse(x.ar1 < sig.lvl, 0, x.ar1)

  y.ar1 = acf(y,plot=F)
  sig.lvl = qnorm((1 + 0.9)/2)/sqrt(y.ar1$n.used)
  y.ar1 = y.ar1$acf[2,1,1]
  y.ar1 = ifelse(y.ar1 < sig.lvl, 0, y.ar1)

  n <- length(x)
  neff <- floor(n*(1-x.ar1*y.ar1)/(1+x.ar1*y.ar1))
  neff
}
# on a df does each column
calc.neff2 <- function(dat){
  dat.acf <- rep(NA,ncol(dat))
  for(i in 1:ncol(dat)){
    tmp = acf(dat[,i],plot=F)
    sig.lvl = qnorm((1 + 0.9)/2)/sqrt(tmp$n.used)
    tmp2 = tmp$acf[2,1,1]
    dat.acf[i] = ifelse(tmp2 < sig.lvl, 0, tmp2)
  }
  dat.acf2 <- data.frame(x = dat.acf[1], y = dat.acf[-1])
  n <- nrow(dat)
  neff <- floor(n*(1-dat.acf2$x*dat.acf2$y)/(1+dat.acf2$x*dat.acf2$y))
  neff
}
## as an aside here is how to get the cor.test code
#methods("cor.test")
#stats:::cor.test.default
#
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
