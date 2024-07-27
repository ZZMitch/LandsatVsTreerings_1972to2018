#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M01.trw = read.csv("M01_Chronologies.csv", row.names = 1)
M01.trw.spline = read.csv("M01_Chronologies_Spline.csv", row.names = 1)

#RS#
M01.rs.pt5 = read.csv("M01_LandsatPt5_Chronologies.csv", row.names = 1)
M01.rs.pt5.spline = read.csv("M01_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M01.rs.avg = read.csv("M01_LandsatAvg_Chronologies.csv", row.names = 1)
M01.rs.avg.spline = read.csv("M01_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M01.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M01.trw vs. M01.rs.pt5
M01.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M01.rs.pt5)),
                                 row.names = rownames(M01.rs.pt5))
colnames(M01.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M01.noisy.pt5.merge[,c(1:7)] = M01.trw[yrs >= 1972, c(1:7)]
M01.noisy.pt5.merge[,c(8:14)] = M01.rs.pt5

round(cor(M01.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M01.trw vs. M01.rs.avg
M01.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M01.rs.avg)),
                                 row.names = rownames(M01.rs.avg))
colnames(M01.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M01.noisy.avg.merge[,c(1:7)] = M01.trw[yrs >= 1972, c(1:7)]
M01.noisy.avg.merge[,c(8:14)] = M01.rs.avg

round(cor(M01.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M01.trw.spline vs. M01.rs.pt5.spline
M01.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M01.rs.pt5)),
                                 row.names = rownames(M01.rs.pt5))
colnames(M01.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M01.spline.pt5.merge[,c(1:7)] = M01.trw.spline[yrs >= 1972, c(1:7)]
M01.spline.pt5.merge[,c(8:14)] = M01.rs.pt5.spline

round(cor(M01.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M01.trw.spline vs. M01.rs.avg.spline
M01.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M01.rs.avg)),
                                 row.names = rownames(M01.rs.avg))
colnames(M01.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M01.spline.avg.merge[,c(1:7)] = M01.trw.spline[yrs >= 1972, c(1:7)]
M01.spline.avg.merge[,c(8:14)] = M01.rs.avg.spline

round(cor(M01.spline.avg.merge, use = "pairwise.complete.obs"), 2)

### Best correlation ###
plot(row.names(M01.rs.avg), M01.trw.spline$Ar[yrs >= 1972])
lines(row.names(M01.rs.avg), M01.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1954 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M01.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M01.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M01.bestcor$TRW.smoothed = M01.trw.spline$Ar[yrs >= strt.yr]
M01.bestcor$RS.smoothed[rs.strt:length.trw] = M01.rs.avg.spline$NoDetrend
M01.bestcor$TRW.diff[2:length.trw] = diff(M01.bestcor$TRW.smoothed)
M01.bestcor$RS.diff[2:length.trw] = diff(M01.bestcor$RS.smoothed)

for (i in 2:nrow(M01.bestcor)){
  if (M01.bestcor$TRW.diff[i] > 0) {
    M01.bestcor$TRW.class[i] = 1
  } else {
    M01.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M01.bestcor)){
  if (M01.bestcor$RS.diff[i] > 0) {
    M01.bestcor$RS.class[i] = 1
  } else {
    M01.bestcor$RS.class[i] = 0
  }
}

sum(M01.bestcor$TRW.class, na.rm = TRUE)
sum(M01.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M01.bestcor)){
  if (M01.bestcor$TRW.class[i] == M01.bestcor$RS.class[i]){
    M01.bestcor$TRW.RS.agree[i] = 1
  } else {
    M01.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M01.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(M01.rs.avg), M01.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(M01.rs.avg), M01.rs.avg.spline$ModHugershoff)
# Further comparisons and 1-year lags in Excel
strt.yr = 1951 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M01.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M01.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M01.bestcor$TRW.smoothed = M01.trw.spline$ModHugershoff[yrs >= strt.yr]
M01.bestcor$RS.smoothed[rs.strt:length.trw] = M01.rs.pt5.spline$ModHugershoff
M01.bestcor$TRW.diff[2:length.trw] = diff(M01.bestcor$TRW.smoothed)
M01.bestcor$RS.diff[2:length.trw] = diff(M01.bestcor$RS.smoothed)

for (i in 2:nrow(M01.bestcor)){
  if (M01.bestcor$TRW.diff[i] > 0) {
    M01.bestcor$TRW.class[i] = 1
  } else {
    M01.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M01.bestcor)){
  if (M01.bestcor$RS.diff[i] > 0) {
    M01.bestcor$RS.class[i] = 1
  } else {
    M01.bestcor$RS.class[i] = 0
  }
}

sum(M01.bestcor$TRW.class, na.rm = TRUE)
sum(M01.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M01.bestcor)){
  if (M01.bestcor$TRW.class[i] == M01.bestcor$RS.class[i]){
    M01.bestcor$TRW.RS.agree[i] = 1
  } else {
    M01.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M01.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

