library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13/DetrendingLandsat") 
#Laptop

M13.rs = read.csv("M13_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(M13.rs), M13.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(M13.rs), M13.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(M13.rs), M13.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(M13.rs), M13.rs$Avg_CC_Fitted)
#Check for outliers

M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1972] = 72.5
M13.rs$Avg_CC_Median[row.names(M13.rs) == 1972] = 72.5
M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1980] = 75
M13.rs$Avg_CC_Median[row.names(M13.rs) == 1980] = 77.5
M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1983] = 77.5
M13.rs$Avg_CC_Median[row.names(M13.rs) == 1983] = 75
M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1997] = 85
M13.rs$Avg_CC_Median[row.names(M13.rs) == 1997] = 85

#M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1983] = 70
#M13.rs$Avg_CC_Median[row.names(M13.rs) == 1983] = 70
#M13.rs$Pt5_CC_Median[row.names(M13.rs) == 1997] = 85
#M13.rs$Avg_CC_Median[row.names(M13.rs) == 1997] = 85

#####Noisy Values#####
#####
###Detrending###
M13.rs.pt5.detrend = detrend.series(M13.rs$Pt5_CC_Median)
M13.rs.avg.detrend = detrend.series(M13.rs$Avg_CC_Median)

row.names(M13.rs.pt5.detrend) = row.names(M13.rs)
row.names(M13.rs.avg.detrend) = row.names(M13.rs)

M13.rs.pt5.detrend$NoDetrend = M13.rs$Pt5_CC_Median
M13.rs.avg.detrend$NoDetrend = M13.rs$Avg_CC_Median
#####
write.csv(M13.rs.pt5.detrend, "M13_LandsatPt5_Chronologies.csv")
write.csv(M13.rs.avg.detrend, "M13_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
M13.rs.pt5.spline.spline = ffcsaps(M13.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.spline.spline)

M13.rs.avg.spline.spline = ffcsaps(M13.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.spline.spline)

#ModNegExp
M13.rs.pt5.modnegexp.spline = ffcsaps(M13.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.modnegexp.spline)

M13.rs.avg.modnegexp.spline = ffcsaps(M13.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.modnegexp.spline)

#Mean
M13.rs.pt5.mean.spline = ffcsaps(M13.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.mean.spline)

M13.rs.avg.mean.spline = ffcsaps(M13.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.mean.spline)

#Ar
M13.rs.pt5.ar.spline = ffcsaps(M13.rs.pt5.detrend$Ar[row.names(M13.rs) >= 1975],
                               nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1975:2018), M13.rs.pt5.ar.spline)

M13.rs.avg.ar.spline = ffcsaps(M13.rs.avg.detrend$Ar[row.names(M13.rs) >= 1975],
                               nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1975:2018), M13.rs.avg.ar.spline)

#Friedman
M13.rs.pt5.friedman.spline = ffcsaps(M13.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.friedman.spline)

M13.rs.avg.friedman.spline = ffcsaps(M13.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.friedman.spline)

#ModHugershoff
M13.rs.pt5.modhuger.spline = ffcsaps(M13.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.modhuger.spline)

M13.rs.avg.modhuger.spline = ffcsaps(M13.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.modhuger.spline)

#NoDetrend
M13.rs.pt5.nodetrend.spline = ffcsaps(M13.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(M13.rs), M13.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M13.rs), M13.rs.pt5.nodetrend.spline)

M13.rs.avg.nodetrend.spline = ffcsaps(M13.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(M13.rs), M13.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M13.rs), M13.rs.avg.nodetrend.spline)

#Merge
M13.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M13.rs))),
                                      row.names = rownames(M13.rs))
colnames(M13.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M13.rs.pt5.merged.spline$Spline = M13.rs.pt5.spline.spline
M13.rs.pt5.merged.spline$ModNegExp = M13.rs.pt5.modnegexp.spline
M13.rs.pt5.merged.spline$Mean = M13.rs.pt5.mean.spline
M13.rs.pt5.merged.spline$Ar[4:47] = M13.rs.pt5.ar.spline
M13.rs.pt5.merged.spline$Friedman = M13.rs.pt5.friedman.spline
M13.rs.pt5.merged.spline$ModHugershoff = M13.rs.pt5.modhuger.spline
M13.rs.pt5.merged.spline$NoDetrend = M13.rs.pt5.nodetrend.spline

M13.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M13.rs))),
                                      row.names = rownames(M13.rs))
colnames(M13.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M13.rs.avg.merged.spline$Spline = M13.rs.avg.spline.spline
M13.rs.avg.merged.spline$ModNegExp = M13.rs.avg.modnegexp.spline
M13.rs.avg.merged.spline$Mean = M13.rs.avg.mean.spline
M13.rs.avg.merged.spline$Ar[4:47] = M13.rs.avg.ar.spline
M13.rs.avg.merged.spline$Friedman = M13.rs.avg.friedman.spline
M13.rs.avg.merged.spline$ModHugershoff = M13.rs.avg.modhuger.spline
M13.rs.avg.merged.spline$NoDetrend = M13.rs.avg.nodetrend.spline

#####
write.csv(M13.rs.pt5.merged.spline, "M13_LandsatPt5_Chronologies_Spline.csv")
write.csv(M13.rs.avg.merged.spline, "M13_LandsatAvg_Chronologies_Spline.csv")
