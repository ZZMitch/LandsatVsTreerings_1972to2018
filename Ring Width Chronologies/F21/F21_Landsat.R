library(dplR)

#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21/DetrendingLandsat") 
#Laptop

F21.rs = read.csv("F21_Landsat.csv", row.names = 1)

#Pt5 Data
plot(rownames(F21.rs), F21.rs$Pt5_CC_Median, ylab = "Landsat-derived %CC ", 
     xlab = "")
lines(rownames(F21.rs), F21.rs$Pt5_CC_Fitted)
#Check for outliers

#3x3 Window Average Data
plot(rownames(F21.rs), F21.rs$Avg_CC_Median, ylab = "Landsat-derived %CC",
     xlab = "")
lines(rownames(F21.rs), F21.rs$Avg_CC_Fitted)
#Check for outliers

F21.rs$Pt5_CC_Median[row.names(F21.rs) == 1988] = 70
F21.rs$Avg_CC_Median[row.names(F21.rs) == 1988] = 70

#####Noisy Values#####
#####
###Detrending###
F21.rs.pt5.detrend = detrend.series(F21.rs$Pt5_CC_Median)
F21.rs.avg.detrend = detrend.series(F21.rs$Avg_CC_Median)

row.names(F21.rs.pt5.detrend) = row.names(F21.rs)
row.names(F21.rs.avg.detrend) = row.names(F21.rs)

F21.rs.pt5.detrend$NoDetrend = F21.rs$Pt5_CC_Median
F21.rs.avg.detrend$NoDetrend = F21.rs$Avg_CC_Median
#####
write.csv(F21.rs.pt5.detrend, "F21_LandsatPt5_Chronologies.csv")
write.csv(F21.rs.avg.detrend, "F21_LandsatAvg_Chronologies.csv")

#####Smoothed Values#####
#####
#Spline
F21.rs.pt5.spline.spline = ffcsaps(F21.rs.pt5.detrend$Spline, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.spline.spline)

F21.rs.avg.spline.spline = ffcsaps(F21.rs.avg.detrend$Spline, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$Spline, 
     ylab = "Landsat-derived %CC (Spline)", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.spline.spline)

#ModNegExp
F21.rs.pt5.modnegexp.spline = ffcsaps(F21.rs.pt5.detrend$ModNegExp, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.modnegexp.spline)

F21.rs.avg.modnegexp.spline = ffcsaps(F21.rs.avg.detrend$ModNegExp, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$ModNegExp, 
     ylab = "Landsat-derived %CC (ModNegExp)", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.modnegexp.spline)

#Mean
F21.rs.pt5.mean.spline = ffcsaps(F21.rs.pt5.detrend$Mean, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.mean.spline)

F21.rs.avg.mean.spline = ffcsaps(F21.rs.avg.detrend$Mean, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$Mean, 
     ylab = "Landsat-derived %CC (Mean)", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.mean.spline)

#Ar
F21.rs.pt5.ar.spline = ffcsaps(F21.rs.pt5.detrend$Ar[row.names(F21.rs) >= 1974],
                               nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1974:2018), F21.rs.pt5.ar.spline)

F21.rs.avg.ar.spline = ffcsaps(F21.rs.avg.detrend$Ar[row.names(F21.rs) >= 1974],
                               nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$Ar, 
     ylab = "Landsat-derived %CC (Ar)", xlab = "")
lines(c(1974:2018), F21.rs.avg.ar.spline)

#Friedman
F21.rs.pt5.friedman.spline = ffcsaps(F21.rs.pt5.detrend$Friedman, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.friedman.spline)

F21.rs.avg.friedman.spline = ffcsaps(F21.rs.avg.detrend$Friedman, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$Friedman, 
     ylab = "Landsat-derived %CC (Friedman)", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.friedman.spline)

#ModHugershoff
F21.rs.pt5.modhuger.spline = ffcsaps(F21.rs.pt5.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.modhuger.spline)

F21.rs.avg.modhuger.spline = ffcsaps(F21.rs.avg.detrend$ModHugershoff, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$ModHugershoff, 
     ylab = "Landsat-derived %CC (ModHugershoff)", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.modhuger.spline)

#NoDetrend
F21.rs.pt5.nodetrend.spline = ffcsaps(F21.rs.pt5.detrend$NoDetrend, nyrs = 10)
plot(rownames(F21.rs), F21.rs.pt5.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F21.rs), F21.rs.pt5.nodetrend.spline)

F21.rs.avg.nodetrend.spline = ffcsaps(F21.rs.avg.detrend$NoDetrend, nyrs = 10)
plot(rownames(F21.rs), F21.rs.avg.detrend$NoDetrend, 
     ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(F21.rs), F21.rs.avg.nodetrend.spline)

#Merge
F21.rs.pt5.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F21.rs))),
                                      row.names = rownames(F21.rs))
colnames(F21.rs.pt5.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F21.rs.pt5.merged.spline$Spline = F21.rs.pt5.spline.spline
F21.rs.pt5.merged.spline$ModNegExp = F21.rs.pt5.modnegexp.spline
F21.rs.pt5.merged.spline$Mean = F21.rs.pt5.mean.spline
F21.rs.pt5.merged.spline$Ar[3:47] = F21.rs.pt5.ar.spline
F21.rs.pt5.merged.spline$Friedman = F21.rs.pt5.friedman.spline
F21.rs.pt5.merged.spline$ModHugershoff = F21.rs.pt5.modhuger.spline
F21.rs.pt5.merged.spline$NoDetrend = F21.rs.pt5.nodetrend.spline

F21.rs.avg.merged.spline = data.frame(matrix(ncol = 7, nrow = (nrow(F21.rs))),
                                      row.names = rownames(F21.rs))
colnames(F21.rs.avg.merged.spline) = c("Spline", "ModNegExp", "Mean", "Ar", 
                                       "Friedman", "ModHugershoff", "NoDetrend")

F21.rs.avg.merged.spline$Spline = F21.rs.avg.spline.spline
F21.rs.avg.merged.spline$ModNegExp = F21.rs.avg.modnegexp.spline
F21.rs.avg.merged.spline$Mean = F21.rs.avg.mean.spline
F21.rs.avg.merged.spline$Ar[3:47] = F21.rs.avg.ar.spline
F21.rs.avg.merged.spline$Friedman = F21.rs.avg.friedman.spline
F21.rs.avg.merged.spline$ModHugershoff = F21.rs.avg.modhuger.spline
F21.rs.avg.merged.spline$NoDetrend = F21.rs.avg.nodetrend.spline

#####
write.csv(F21.rs.pt5.merged.spline, "F21_LandsatPt5_Chronologies_Spline.csv")
write.csv(F21.rs.avg.merged.spline, "F21_LandsatAvg_Chronologies_Spline.csv")
