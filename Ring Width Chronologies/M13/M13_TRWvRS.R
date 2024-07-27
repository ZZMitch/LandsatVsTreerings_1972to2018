#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M13.trw = read.csv("M13_Chronologies.csv", row.names = 1)
M13.trw.spline = read.csv("M13_Chronologies_Spline.csv", row.names = 1)

#RS#
M13.rs.pt5 = read.csv("M13_LandsatPt5_Chronologies.csv", row.names = 1)
M13.rs.pt5.spline = read.csv("M13_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M13.rs.avg = read.csv("M13_LandsatAvg_Chronologies.csv", row.names = 1)
M13.rs.avg.spline = read.csv("M13_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M13.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M13.trw vs. M13.rs.pt5
M13.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M13.rs.pt5)),
                                 row.names = rownames(M13.rs.pt5))
colnames(M13.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M13.noisy.pt5.merge[,c(1:7)] = M13.trw[yrs >= 1972, c(1:7)]
M13.noisy.pt5.merge[,c(8:14)] = M13.rs.pt5

round(cor(M13.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M13.trw vs. M13.rs.avg
M13.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M13.rs.avg)),
                                 row.names = rownames(M13.rs.avg))
colnames(M13.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M13.noisy.avg.merge[,c(1:7)] = M13.trw[yrs >= 1972, c(1:7)]
M13.noisy.avg.merge[,c(8:14)] = M13.rs.avg

round(cor(M13.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M13.trw.spline vs. M13.rs.pt5.spline
M13.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M13.rs.pt5)),
                                 row.names = rownames(M13.rs.pt5))
colnames(M13.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M13.spline.pt5.merge[,c(1:7)] = M13.trw.spline[yrs >= 1972, c(1:7)]
M13.spline.pt5.merge[,c(8:14)] = M13.rs.pt5.spline

round(cor(M13.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M13.trw.spline vs. M13.rs.avg.spline
M13.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M13.rs.avg)),
                                 row.names = rownames(M13.rs.avg))
colnames(M13.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M13.spline.avg.merge[,c(1:7)] = M13.trw.spline[yrs >= 1972, c(1:7)]
M13.spline.avg.merge[,c(8:14)] = M13.rs.avg.spline

round(cor(M13.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M13.rs.avg), M13.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(M13.rs.avg), M13.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1902 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M13.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M13.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M13.bestcor$TRW.smoothed = M13.trw.spline$ModNegExp[yrs >= strt.yr]
M13.bestcor$RS.smoothed[rs.strt:length.trw] = M13.rs.avg.spline$NoDetrend
M13.bestcor$TRW.diff[2:length.trw] = diff(M13.bestcor$TRW.smoothed)
M13.bestcor$RS.diff[2:length.trw] = diff(M13.bestcor$RS.smoothed)

for (i in 2:nrow(M13.bestcor)){
  if (M13.bestcor$TRW.diff[i] > 0) {
    M13.bestcor$TRW.class[i] = 1
  } else {
    M13.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M13.bestcor)){
  if (M13.bestcor$RS.diff[i] > 0) {
    M13.bestcor$RS.class[i] = 1
  } else {
    M13.bestcor$RS.class[i] = 0
  }
}

sum(M13.bestcor$TRW.class, na.rm = TRUE)
sum(M13.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M13.bestcor)){
  if (M13.bestcor$TRW.class[i] == M13.bestcor$RS.class[i]){
    M13.bestcor$TRW.RS.agree[i] = 1
  } else {
    M13.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M13.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)