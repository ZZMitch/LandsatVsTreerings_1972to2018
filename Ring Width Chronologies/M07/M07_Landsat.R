library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07/DetrendingLandsat") 
#Laptop

M07.rs = read.csv("M07_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(M07.rs), M07.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(M07.rs), M07.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(M07.rs), M07.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(M07.rs), M07.rs$Avg_CC_Fitted)
#Check for outliers

M07.rs$Pt5_CC_Median[row.names(M07.rs) == 1987] = 70
M07.rs$Avg_CC_Median[row.names(M07.rs) == 1987] = 70

#####Noisy Values#####
#####
###Detrending###
M07.rs.pt5.detrend = detrend.series(M07.rs$Pt5_CC_Median)
M07.rs.avg.detrend = detrend.series(M07.rs$Avg_CC_Median)

row.names(M07.rs.pt5.detrend) = row.names(M07.rs)
row.names(M07.rs.avg.detrend) = row.names(M07.rs)

M07.rs.pt5.detrend$NoDetrend = M07.rs$Pt5_CC_Median
M07.rs.avg.detrend$NoDetrend = M07.rs$Avg_CC_Median
#####
write.csv(M07.rs.pt5.detrend, "M07_LandsatPt5_Chronologies.csv")
write.csv(M07.rs.avg.detrend, "M07_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
M07.rs.pt5.spline.spline = ffcsaps(M07.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.spline.spline)

M07.rs.avg.spline.spline = ffcsaps(M07.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.spline.spline)

#ModNegExp
M07.rs.pt5.modnegexp.spline = ffcsaps(M07.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.modnegexp.spline)

M07.rs.avg.modnegexp.spline = ffcsaps(M07.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.modnegexp.spline)

#Mean
M07.rs.pt5.mean.spline = ffcsaps(M07.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.mean.spline)

M07.rs.avg.mean.spline = ffcsaps(M07.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.mean.spline)

#Ar
M07.rs.pt5.ar.spline = ffcsaps(M07.rs.pt5.detrend$Ar[row.names(M07.rs) >= 1976],
                               nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1976:2018), M07.rs.pt5.ar.spline)

M07.rs.avg.ar.spline = ffcsaps(M07.rs.avg.detrend$Ar[row.names(M07.rs) >= 1976],
                               nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1976:2018), M07.rs.avg.ar.spline)

#Friedman
M07.rs.pt5.friedman.spline = ffcsaps(M07.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.friedman.spline)

M07.rs.avg.friedman.spline = ffcsaps(M07.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.friedman.spline)

#ModHugershoff
M07.rs.pt5.modhuger.spline = ffcsaps(M07.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.modhuger.spline)

M07.rs.avg.modhuger.spline = ffcsaps(M07.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.modhuger.spline)

#NoDetrend
M07.rs.pt5.nodetrend.spline = ffcsaps(M07.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(M07.rs), M07.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M07.rs), M07.rs.pt5.nodetrend.spline)

M07.rs.avg.nodetrend.spline = ffcsaps(M07.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(M07.rs), M07.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(M07.rs), M07.rs.avg.nodetrend.spline)

#Merge
M07.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M07.rs))),
                                      row.names = rownames(M07.rs))
colnames(M07.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M07.rs.pt5.merged.spline$Spline = M07.rs.pt5.spline.spline
M07.rs.pt5.merged.spline$ModNegExp = M07.rs.pt5.modnegexp.spline
M07.rs.pt5.merged.spline$Mean = M07.rs.pt5.mean.spline
M07.rs.pt5.merged.spline$Ar[5:47] = M07.rs.pt5.ar.spline
M07.rs.pt5.merged.spline$Friedman = M07.rs.pt5.friedman.spline
M07.rs.pt5.merged.spline$ModHugershoff = M07.rs.pt5.modhuger.spline
M07.rs.pt5.merged.spline$NoDetrend = M07.rs.pt5.nodetrend.spline

M07.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(M07.rs))),
                                      row.names = rownames(M07.rs))
colnames(M07.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

M07.rs.avg.merged.spline$Spline = M07.rs.avg.spline.spline
M07.rs.avg.merged.spline$ModNegExp = M07.rs.avg.modnegexp.spline
M07.rs.avg.merged.spline$Mean = M07.rs.avg.mean.spline
M07.rs.avg.merged.spline$Ar[5:47] = M07.rs.avg.ar.spline
M07.rs.avg.merged.spline$Friedman = M07.rs.avg.friedman.spline
M07.rs.avg.merged.spline$ModHugershoff = M07.rs.avg.modhuger.spline
M07.rs.avg.merged.spline$NoDetrend = M07.rs.avg.nodetrend.spline

#####
write.csv(M07.rs.pt5.merged.spline, "M07_LandsatPt5_Chronologies_Spline.csv")
write.csv(M07.rs.avg.merged.spline, "M07_LandsatAvg_Chronologies_Spline.csv")
