library(dplR)

# Working with Ring-Width Data
data(ca533)
nrow(ca533)
ncol(ca533)
colnames(ca533)
head(time(ca533), n = 10)
class(ca533)
plot(ca533, plot.type = "spag")

# Descriptive Statistics
rwl.report(ca533)
ca533.stats = summary(ca533)
head(ca533.stats, n = 5)

boxplot(ca533.stats$ar1, ylab = expression(phi[1]), col = "lightblue")
stripchart(ca533.stats$ar1, vertical = TRUE, method = "jitter", jitter = 0.02,
           add = TRUE, pch = 20, col = "darkblue", cex = 1.25)
ar1Quant = quantile(ca533.stats$ar1, probs = c(0.25, 0.5, 0.75))
abline(h = ar1Quant, lty = "dashed", col = "grey")
mtext(text = names(ar1Quant), side = 4, at = ar1Quant, las = 2)

# Detrending
ca533.rwi = detrend(rwl = ca533, method = "ModNegExp")
nrow(ca533.rwi)
ncol(ca533.rwi)
colMeans(ca533.rwi, na.rm = TRUE)

CAM011.rwi = detrend.series(y = ca533[, "CAM011"], verbose = TRUE)

# Descriptive Statistics for Detrended Data
ca533.ids = read.ids(ca533, stc = c(3, 2, 1))
rwi.stats(ca533.rwi, ca533.ids, prewhiten = TRUE)

ca533.rho = interseries.cor(ca533.rwi, prewhiten = TRUE, method = "spearman")
ca533.rho[1:5, ]
mean(ca533.rho[,1])
sd(ca533.rho[,1])

# Building a Mean Value Chronology
ca533.crn = chron(ca533.rwi, prefix = "CAM") # Sample depth = # of cores
dim(ca533.rwi)
dim(ca533.crn)

plot(ca533.crn, add.spline = TRUE, nyrs = 100)

# Conclusion
ca533.sss = sss(ca533.rwi, ca533.ids)
yr = time(ca533)
par(mar = c(2, 2, 2, 2), mgp = c(1.1, 0.1, 0), tcl = 0.25, xaxs = "i")
plot(yr, ca533.crn[, 1], type = "n", xlab = "Year", ylab = "RWI", axes = FALSE)
cutoff = max(yr[ca533.sss < 0.85])
xx = c(500, 500, cutoff, cutoff)
yy = c(-1, 3, 3, -1)
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yr, ca533.crn[, 1], col = "grey50")
lines(yr, ffcsaps(ca533.crn[, 1], nyrs = 32), col = "red", lwd = 2)
axis(1)
axis(2) 
axis(3)
par(new = TRUE)
## Add SSS
plot(yr, ca533.sss, type = "l", xlab = "", ylab = "",
     axes = FALSE, col = "blue")
abline(h=0.85,col="blue",lty="dashed")
axis(4, at = pretty(ca533.sss))
mtext("SSS", side = 4, line = 1.1, lwd=1.5)
box()