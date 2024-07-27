library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20/DetrendingLandsat") 
#Laptop

M20.rs = read.csv("M20_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(M20.rs), M20.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(M20.rs), M20.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(M20.rs), M20.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(M20.rs), M20.rs$Avg_CC_Fitted)
#Check for outliers

M20.rs$Pt5_CC_Median[row.names(M20.rs) == 1977] = 70
M20.rs$Avg_CC_Median[row.names(M20.rs) == 1977] = 70
M20.rs$Pt5_CC_Median[row.names(M20.rs) == 1988] = 70
M20.rs$Avg_CC_Median[row.names(M20.rs) == 1988] = 70

#####Noisy Values#####
#####
###Detrending###
M20.rs.pt5.detrend = detrend.series(M20.rs$Pt5_CC_Median)
M20.rs.avg.detrend = detrend.series(M20.rs$Avg_CC_Median)

row.names(M20.rs.pt5.detrend) = row.names(M20.rs)
row.names(M20.rs.avg.detrend) = row.names(M20.rs)

M20.rs.pt5.detrend$NoDetrend = M20.rs$Pt5_CC_Median
M20.rs.avg.detrend$NoDetrend = M20.rs$Avg_CC_Median
#####
write.csv(M20.rs.pt5.detrend, "M20_LandsatPt5_Chronologies.csv")
write.csv(M20.rs.avg.detrend, "M20_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
M20.rs.pt5.spline.spline = ffcsaps(M20.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.spline.spline)

M20.rs.avg.spline.spline = ffcsaps(M20.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.spline.spline)

#ModNegExp
M20.rs.pt5.modnegexp.spline = ffcsaps(M20.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.modnegexp.spline)

M20.rs.avg.modnegexp.spline = ffcsaps(M20.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.modnegexp.spline)

#Mean
M20.rs.pt5.mean.spline = ffcsaps(M20.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.mean.spline)

M20.rs.avg.mean.spline = ffcsaps(M20.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.mean.spline)

#Ar
M20.rs.pt5.ar.spline = ffcsaps(M20.rs.pt5.detrend$Ar[row.names(M20.rs) >= 1974],
                               nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1974:2018), M20.rs.pt5.ar.spline)

M20.rs.avg.ar.spline = ffcsaps(M20.rs.avg.detrend$Ar[row.names(M20.rs) >= 1974],
                               nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1974:2018), M20.rs.avg.ar.spline)

#Friedman
M20.rs.pt5.friedman.spline = ffcsaps(M20.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.friedman.spline)

M20.rs.avg.friedman.spline = ffcsaps(M20.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.friedman.spline)

#ModHugershoff
M20.rs.pt5.modhuger.spline = ffcsaps(M20.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.modhuger.spline)

M20.rs.avg.modhuger.spline = ffcsaps(M20.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.modhuger.spline)

#NoDetrend
M20.rs.pt5.nodetrend.spline = ffcsaps(M20.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(M20.rs), M20.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M20.rs), M20.rs.pt5.nodetrend.spline)

M20.rs.avg.nodetrend.spline = ffcsaps(M20.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(M20.rs), M20.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M20.rs), M20.rs.avg.nodetrend.spline)

#Merge
M20.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M20.rs))),
                                      row.names = rownames(M20.rs))
colnames(M20.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M20.rs.pt5.merged.spline$Spline = M20.rs.pt5.spline.spline
M20.rs.pt5.merged.spline$ModNegExp = M20.rs.pt5.modnegexp.spline
M20.rs.pt5.merged.spline$Mean = M20.rs.pt5.mean.spline
M20.rs.pt5.merged.spline$Ar[3:47] = M20.rs.pt5.ar.spline
M20.rs.pt5.merged.spline$Friedman = M20.rs.pt5.friedman.spline
M20.rs.pt5.merged.spline$ModHugershoff = M20.rs.pt5.modhuger.spline
M20.rs.pt5.merged.spline$NoDetrend = M20.rs.pt5.nodetrend.spline

M20.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M20.rs))),
                                      row.names = rownames(M20.rs))
colnames(M20.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M20.rs.avg.merged.spline$Spline = M20.rs.avg.spline.spline
M20.rs.avg.merged.spline$ModNegExp = M20.rs.avg.modnegexp.spline
M20.rs.avg.merged.spline$Mean = M20.rs.avg.mean.spline
M20.rs.avg.merged.spline$Ar[3:47] = M20.rs.avg.ar.spline
M20.rs.avg.merged.spline$Friedman = M20.rs.avg.friedman.spline
M20.rs.avg.merged.spline$ModHugershoff = M20.rs.avg.modhuger.spline
M20.rs.avg.merged.spline$NoDetrend = M20.rs.avg.nodetrend.spline

#####
write.csv(M20.rs.pt5.merged.spline, "M20_LandsatPt5_Chronologies_Spline.csv")
write.csv(M20.rs.avg.merged.spline, "M20_LandsatAvg_Chronologies_Spline.csv")
