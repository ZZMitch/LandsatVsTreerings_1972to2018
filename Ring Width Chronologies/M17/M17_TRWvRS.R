#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M17/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M17/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M17.trw = read.csv("M17_Chronologies.csv", row.names = 1)
M17.trw.spline = read.csv("M17_Chronologies_Spline.csv", row.names = 1)

#RS#
M17.rs.pt5 = read.csv("M17_LandsatPt5_Chronologies.csv", row.names = 1)
M17.rs.pt5.spline = read.csv("M17_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M17.rs.avg = read.csv("M17_LandsatAvg_Chronologies.csv", row.names = 1)
M17.rs.avg.spline = read.csv("M17_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M17.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M17.trw vs. M17.rs.pt5
M17.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M17.rs.pt5)),
                                 row.names = rownames(M17.rs.pt5))
colnames(M17.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M17.noisy.pt5.merge[,c(1:7)] = M17.trw[yrs >= 1972, c(1:7)]
M17.noisy.pt5.merge[,c(8:14)] = M17.rs.pt5

round(cor(M17.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M17.trw vs. M17.rs.avg
M17.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M17.rs.avg)),
                                 row.names = rownames(M17.rs.avg))
colnames(M17.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M17.noisy.avg.merge[,c(1:7)] = M17.trw[yrs >= 1972, c(1:7)]
M17.noisy.avg.merge[,c(8:14)] = M17.rs.avg

round(cor(M17.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M17.trw.spline vs. M17.rs.pt5.spline
M17.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M17.rs.pt5)),
                                 row.names = rownames(M17.rs.pt5))
colnames(M17.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M17.spline.pt5.merge[,c(1:7)] = M17.trw.spline[yrs >= 1972, c(1:7)]
M17.spline.pt5.merge[,c(8:14)] = M17.rs.pt5.spline

round(cor(M17.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M17.trw.spline vs. M17.rs.avg.spline
M17.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M17.rs.avg)),
                                 row.names = rownames(M17.rs.avg))
colnames(M17.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M17.spline.avg.merge[,c(1:7)] = M17.trw.spline[yrs >= 1972, c(1:7)]
M17.spline.avg.merge[,c(8:14)] = M17.rs.avg.spline

round(cor(M17.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M17.rs.avg), M17.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(M17.rs.avg), M17.rs.avg.spline$Mean)
# Further comparisons and 1-year lags in Excel
strt.yr = 1938 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M17.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M17.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M17.bestcor$TRW.smoothed = M17.trw.spline$ModHugershoff[yrs >= strt.yr]
M17.bestcor$RS.smoothed[rs.strt:length.trw] = M17.rs.avg.spline$NoDetrend
M17.bestcor$TRW.diff[2:length.trw] = diff(M17.bestcor$TRW.smoothed)
M17.bestcor$RS.diff[2:length.trw] = diff(M17.bestcor$RS.smoothed)

for (i in 2:nrow(M17.bestcor)){
  if (M17.bestcor$TRW.diff[i] > 0) {
    M17.bestcor$TRW.class[i] = 1
  } else {
    M17.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M17.bestcor)){
  if (M17.bestcor$RS.diff[i] > 0) {
    M17.bestcor$RS.class[i] = 1
  } else {
    M17.bestcor$RS.class[i] = 0
  }
}

sum(M17.bestcor$TRW.class, na.rm = TRUE)
sum(M17.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M17.bestcor)){
  if (M17.bestcor$TRW.class[i] == M17.bestcor$RS.class[i]){
    M17.bestcor$TRW.RS.agree[i] = 1
  } else {
    M17.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M17.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)