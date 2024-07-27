library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23/DetrendingLandsat") 
#Laptop

F23.rs = read.csv("F23_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(F23.rs), F23.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(F23.rs), F23.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(F23.rs), F23.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(F23.rs), F23.rs$Avg_CC_Fitted)
#Check for outliers

F23.rs$Pt5_CC_Median[row.names(F23.rs) == 1979] = 70
F23.rs$Avg_CC_Median[row.names(F23.rs) == 1979] = 70
F23.rs$Pt5_CC_Median[row.names(F23.rs) == 1998] = 84
F23.rs$Avg_CC_Median[row.names(F23.rs) == 1998] = 84
F23.rs$Pt5_CC_Median[row.names(F23.rs) == 2003] = 85
F23.rs$Avg_CC_Median[row.names(F23.rs) == 2003] = 85
F23.rs$Pt5_CC_Median[row.names(F23.rs) == 2014] = 83
F23.rs$Avg_CC_Median[row.names(F23.rs) == 2014] = 82

#####Noisy Values#####
#####
###Detrending###
F23.rs.pt5.detrend = detrend.series(F23.rs$Pt5_CC_Median)
F23.rs.avg.detrend = detrend.series(F23.rs$Avg_CC_Median)

row.names(F23.rs.pt5.detrend) = row.names(F23.rs)
row.names(F23.rs.avg.detrend) = row.names(F23.rs)

F23.rs.pt5.detrend$NoDetrend = F23.rs$Pt5_CC_Median
F23.rs.avg.detrend$NoDetrend = F23.rs$Avg_CC_Median
#####
write.csv(F23.rs.pt5.detrend, "F23_LandsatPt5_Chronologies.csv")
write.csv(F23.rs.avg.detrend, "F23_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
F23.rs.pt5.spline.spline = ffcsaps(F23.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.spline.spline)

F23.rs.avg.spline.spline = ffcsaps(F23.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.spline.spline)

#ModNegExp
F23.rs.pt5.modnegexp.spline = ffcsaps(F23.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.modnegexp.spline)

F23.rs.avg.modnegexp.spline = ffcsaps(F23.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.modnegexp.spline)

#Mean
F23.rs.pt5.mean.spline = ffcsaps(F23.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.mean.spline)

F23.rs.avg.mean.spline = ffcsaps(F23.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.mean.spline)

#Ar
F23.rs.pt5.ar.spline = ffcsaps(F23.rs.pt5.detrend$Ar[row.names(F23.rs) >= 1973],
                               nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1973:2018), F23.rs.pt5.ar.spline)

F23.rs.avg.ar.spline = ffcsaps(F23.rs.avg.detrend$Ar[row.names(F23.rs) >= 1973],
                               nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1973:2018), F23.rs.avg.ar.spline)

#Friedman
F23.rs.pt5.friedman.spline = ffcsaps(F23.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.friedman.spline)

F23.rs.avg.friedman.spline = ffcsaps(F23.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.friedman.spline)

#ModHugershoff
F23.rs.pt5.modhuger.spline = ffcsaps(F23.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.modhuger.spline)

F23.rs.avg.modhuger.spline = ffcsaps(F23.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.modhuger.spline)

#NoDetrend
F23.rs.pt5.nodetrend.spline = ffcsaps(F23.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(F23.rs), F23.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F23.rs), F23.rs.pt5.nodetrend.spline)

F23.rs.avg.nodetrend.spline = ffcsaps(F23.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(F23.rs), F23.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F23.rs), F23.rs.avg.nodetrend.spline)

#Merge
F23.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F23.rs))),
                                      row.names = rownames(F23.rs))
colnames(F23.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F23.rs.pt5.merged.spline$Spline = F23.rs.pt5.spline.spline
F23.rs.pt5.merged.spline$ModNegExp = F23.rs.pt5.modnegexp.spline
F23.rs.pt5.merged.spline$Mean = F23.rs.pt5.mean.spline
F23.rs.pt5.merged.spline$Ar[2:47] = F23.rs.pt5.ar.spline
F23.rs.pt5.merged.spline$Friedman = F23.rs.pt5.friedman.spline
F23.rs.pt5.merged.spline$ModHugershoff = F23.rs.pt5.modhuger.spline
F23.rs.pt5.merged.spline$NoDetrend = F23.rs.pt5.nodetrend.spline

F23.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F23.rs))),
                                      row.names = rownames(F23.rs))
colnames(F23.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F23.rs.avg.merged.spline$Spline = F23.rs.avg.spline.spline
F23.rs.avg.merged.spline$ModNegExp = F23.rs.avg.modnegexp.spline
F23.rs.avg.merged.spline$Mean = F23.rs.avg.mean.spline
F23.rs.avg.merged.spline$Ar[2:47] = F23.rs.avg.ar.spline
F23.rs.avg.merged.spline$Friedman = F23.rs.avg.friedman.spline
F23.rs.avg.merged.spline$ModHugershoff = F23.rs.avg.modhuger.spline
F23.rs.avg.merged.spline$NoDetrend = F23.rs.avg.nodetrend.spline

#####
write.csv(F23.rs.pt5.merged.spline, "F23_LandsatPt5_Chronologies_Spline.csv")
write.csv(F23.rs.avg.merged.spline, "F23_LandsatAvg_Chronologies_Spline.csv")
