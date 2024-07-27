library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06/DetrendingLandsat") 
#Laptop

M06.rs = read.csv("M06_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(M06.rs), M06.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(M06.rs), M06.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(M06.rs), M06.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(M06.rs), M06.rs$Avg_CC_Fitted)
#Check for outliers

M06.rs$Pt5_CC_Median[row.names(M06.rs) == 1975] = 90
M06.rs$Avg_CC_Median[row.names(M06.rs) == 1975] = 90
M06.rs$Pt5_CC_Median[row.names(M06.rs) == 1980] = 75
M06.rs$Avg_CC_Median[row.names(M06.rs) == 1980] = 75
M06.rs$Pt5_CC_Median[row.names(M06.rs) == 2017] = 85
M06.rs$Avg_CC_Median[row.names(M06.rs) == 2017] = 85

#####Noisy Values#####
#####
###Detrending###
M06.rs.pt5.detrend = detrend.series(M06.rs$Pt5_CC_Median)
M06.rs.avg.detrend = detrend.series(M06.rs$Avg_CC_Median)

row.names(M06.rs.pt5.detrend) = row.names(M06.rs)
row.names(M06.rs.avg.detrend) = row.names(M06.rs)

M06.rs.pt5.detrend$NoDetrend = M06.rs$Pt5_CC_Median
M06.rs.avg.detrend$NoDetrend = M06.rs$Avg_CC_Median
#####
write.csv(M06.rs.pt5.detrend, "M06_LandsatPt5_Chronologies.csv")
write.csv(M06.rs.avg.detrend, "M06_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
M06.rs.pt5.spline.spline = ffcsaps(M06.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.spline.spline)

M06.rs.avg.spline.spline = ffcsaps(M06.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.spline.spline)

#ModNegExp
M06.rs.pt5.modnegexp.spline = ffcsaps(M06.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.modnegexp.spline)

M06.rs.avg.modnegexp.spline = ffcsaps(M06.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.modnegexp.spline)

#Mean
M06.rs.pt5.mean.spline = ffcsaps(M06.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.mean.spline)

M06.rs.avg.mean.spline = ffcsaps(M06.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.mean.spline)

#Ar
M06.rs.pt5.ar.spline = ffcsaps(M06.rs.pt5.detrend$Ar[row.names(M06.rs) >= 1973],
                               nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1973:2018), M06.rs.pt5.ar.spline)

M06.rs.avg.ar.spline = ffcsaps(M06.rs.avg.detrend$Ar[row.names(M06.rs) >= 1975],
                               nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1975:2018), M06.rs.avg.ar.spline)

#Friedman
M06.rs.pt5.friedman.spline = ffcsaps(M06.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.friedman.spline)

M06.rs.avg.friedman.spline = ffcsaps(M06.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.friedman.spline)

#ModHugershoff
M06.rs.pt5.modhuger.spline = ffcsaps(M06.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.modhuger.spline)

M06.rs.avg.modhuger.spline = ffcsaps(M06.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.modhuger.spline)

#NoDetrend
M06.rs.pt5.nodetrend.spline = ffcsaps(M06.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(M06.rs), M06.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M06.rs), M06.rs.pt5.nodetrend.spline)

M06.rs.avg.nodetrend.spline = ffcsaps(M06.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(M06.rs), M06.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M06.rs), M06.rs.avg.nodetrend.spline)

#Merge
M06.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M06.rs))),
                                      row.names = rownames(M06.rs))
colnames(M06.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M06.rs.pt5.merged.spline$Spline = M06.rs.pt5.spline.spline
M06.rs.pt5.merged.spline$ModNegExp = M06.rs.pt5.modnegexp.spline
M06.rs.pt5.merged.spline$Mean = M06.rs.pt5.mean.spline
M06.rs.pt5.merged.spline$Ar[2:47] = M06.rs.pt5.ar.spline
M06.rs.pt5.merged.spline$Friedman = M06.rs.pt5.friedman.spline
M06.rs.pt5.merged.spline$ModHugershoff = M06.rs.pt5.modhuger.spline
M06.rs.pt5.merged.spline$NoDetrend = M06.rs.pt5.nodetrend.spline

M06.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M06.rs))),
                                      row.names = rownames(M06.rs))
colnames(M06.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M06.rs.avg.merged.spline$Spline = M06.rs.avg.spline.spline
M06.rs.avg.merged.spline$ModNegExp = M06.rs.avg.modnegexp.spline
M06.rs.avg.merged.spline$Mean = M06.rs.avg.mean.spline
M06.rs.avg.merged.spline$Ar[4:47] = M06.rs.avg.ar.spline
M06.rs.avg.merged.spline$Friedman = M06.rs.avg.friedman.spline
M06.rs.avg.merged.spline$ModHugershoff = M06.rs.avg.modhuger.spline
M06.rs.avg.merged.spline$NoDetrend = M06.rs.avg.nodetrend.spline

#####
write.csv(M06.rs.pt5.merged.spline, "M06_LandsatPt5_Chronologies_Spline.csv")
write.csv(M06.rs.avg.merged.spline, "M06_LandsatAvg_Chronologies_Spline.csv")
