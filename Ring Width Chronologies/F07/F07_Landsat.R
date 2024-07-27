library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07/DetrendingLandsat") 
#Laptop

F07.rs = read.csv("F07_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(F07.rs), F07.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(F07.rs), F07.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(F07.rs), F07.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(F07.rs), F07.rs$Avg_CC_Fitted)
#Check for outliers

F07.rs$Pt5_CC_Median[row.names(F07.rs) == 2000] = 94
F07.rs$Avg_CC_Median[row.names(F07.rs) == 2000] = 93
F07.rs$Pt5_CC_Median[row.names(F07.rs) == 2003] = 94
F07.rs$Avg_CC_Median[row.names(F07.rs) == 2003] = 94
F07.rs$Pt5_CC_Median[row.names(F07.rs) == 2014] = 88
F07.rs$Avg_CC_Median[row.names(F07.rs) == 2014] = 88

# Original
#F07.rs$Pt5_CC_Median[row.names(F07.rs) == 2014] = 85
#F07.rs$Avg_CC_Median[row.names(F07.rs) == 2014] = 85

#####Noisy Values#####
#####
###Detrending###
F07.rs.pt5.detrend = detrend.series(F07.rs$Pt5_CC_Median)
F07.rs.avg.detrend = detrend.series(F07.rs$Avg_CC_Median)

row.names(F07.rs.pt5.detrend) = row.names(F07.rs)
row.names(F07.rs.avg.detrend) = row.names(F07.rs)

F07.rs.pt5.detrend$NoDetrend = F07.rs$Pt5_CC_Median
F07.rs.avg.detrend$NoDetrend = F07.rs$Avg_CC_Median
#####
write.csv(F07.rs.pt5.detrend, "F07_LandsatPt5_Chronologies.csv")
write.csv(F07.rs.avg.detrend, "F07_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
F07.rs.pt5.spline.spline = ffcsaps(F07.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.spline.spline)

F07.rs.avg.spline.spline = ffcsaps(F07.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.spline.spline)

#ModNegExp
F07.rs.pt5.modnegexp.spline = ffcsaps(F07.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.modnegexp.spline)

F07.rs.avg.modnegexp.spline = ffcsaps(F07.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.modnegexp.spline)

#Mean
F07.rs.pt5.mean.spline = ffcsaps(F07.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.mean.spline)

F07.rs.avg.mean.spline = ffcsaps(F07.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.mean.spline)

#Ar
F07.rs.pt5.ar.spline = ffcsaps(F07.rs.pt5.detrend$Ar[row.names(F07.rs) >= 1973],
                               nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1973:2018), F07.rs.pt5.ar.spline)

F07.rs.avg.ar.spline = ffcsaps(F07.rs.avg.detrend$Ar[row.names(F07.rs) >= 1973],
                               nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1973:2018), F07.rs.avg.ar.spline)

#Friedman
F07.rs.pt5.friedman.spline = ffcsaps(F07.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.friedman.spline)

F07.rs.avg.friedman.spline = ffcsaps(F07.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.friedman.spline)

#ModHugershoff
F07.rs.pt5.modhuger.spline = ffcsaps(F07.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.modhuger.spline)

F07.rs.avg.modhuger.spline = ffcsaps(F07.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.modhuger.spline)

#NoDetrend
F07.rs.pt5.nodetrend.spline = ffcsaps(F07.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(F07.rs), F07.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F07.rs), F07.rs.pt5.nodetrend.spline)

F07.rs.avg.nodetrend.spline = ffcsaps(F07.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(F07.rs), F07.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F07.rs), F07.rs.avg.nodetrend.spline)

#Merge
F07.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F07.rs))),
                                      row.names = rownames(F07.rs))
colnames(F07.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F07.rs.pt5.merged.spline$Spline = F07.rs.pt5.spline.spline
F07.rs.pt5.merged.spline$ModNegExp = F07.rs.pt5.modnegexp.spline
F07.rs.pt5.merged.spline$Mean = F07.rs.pt5.mean.spline
F07.rs.pt5.merged.spline$Ar[2:47] = F07.rs.pt5.ar.spline
F07.rs.pt5.merged.spline$Friedman = F07.rs.pt5.friedman.spline
F07.rs.pt5.merged.spline$ModHugershoff = F07.rs.pt5.modhuger.spline
F07.rs.pt5.merged.spline$NoDetrend = F07.rs.pt5.nodetrend.spline

F07.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F07.rs))),
                                      row.names = rownames(F07.rs))
colnames(F07.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F07.rs.avg.merged.spline$Spline = F07.rs.avg.spline.spline
F07.rs.avg.merged.spline$ModNegExp = F07.rs.avg.modnegexp.spline
F07.rs.avg.merged.spline$Mean = F07.rs.avg.mean.spline
F07.rs.avg.merged.spline$Ar[2:47] = F07.rs.avg.ar.spline
F07.rs.avg.merged.spline$Friedman = F07.rs.avg.friedman.spline
F07.rs.avg.merged.spline$ModHugershoff = F07.rs.avg.modhuger.spline
F07.rs.avg.merged.spline$NoDetrend = F07.rs.avg.nodetrend.spline

#####
write.csv(F07.rs.pt5.merged.spline, "F07_LandsatPt5_Chronologies_Spline.csv")
write.csv(F07.rs.avg.merged.spline, "F07_LandsatAvg_Chronologies_Spline.csv")
