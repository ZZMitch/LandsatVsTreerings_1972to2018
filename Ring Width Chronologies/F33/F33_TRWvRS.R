#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F33/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F33/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F33.trw = read.csv("F33_Chronologies.csv", row.names = 1)
F33.trw.spline = read.csv("F33_Chronologies_Spline.csv", row.names = 1)

#RS#
F33.rs.pt5 = read.csv("F33_LandsatPt5_Chronologies.csv", row.names = 1)
F33.rs.pt5.spline = read.csv("F33_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F33.rs.avg = read.csv("F33_LandsatAvg_Chronologies.csv", row.names = 1)
F33.rs.avg.spline = read.csv("F33_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F33.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F33.trw vs. F33.rs.pt5
F33.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F33.rs.pt5)),
                                 row.names = rownames(F33.rs.pt5))
colnames(F33.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F33.noisy.pt5.merge[,c(1:7)] = F33.trw[yrs >= 1972, c(1:7)]
F33.noisy.pt5.merge[,c(8:14)] = F33.rs.pt5

round(cor(F33.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F33.trw vs. F33.rs.avg
F33.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F33.rs.avg)),
                                 row.names = rownames(F33.rs.avg))
colnames(F33.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F33.noisy.avg.merge[,c(1:7)] = F33.trw[yrs >= 1972, c(1:7)]
F33.noisy.avg.merge[,c(8:14)] = F33.rs.avg

round(cor(F33.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F33.trw.spline vs. F33.rs.pt5.spline
F33.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F33.rs.pt5)),
                                 row.names = rownames(F33.rs.pt5))
colnames(F33.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F33.spline.pt5.merge[,c(1:7)] = F33.trw.spline[yrs >= 1972, c(1:7)]
F33.spline.pt5.merge[,c(8:14)] = F33.rs.pt5.spline

round(cor(F33.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F33.trw.spline vs. F33.rs.avg.spline
F33.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F33.rs.avg)),
                                 row.names = rownames(F33.rs.avg))
colnames(F33.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F33.spline.avg.merge[,c(1:7)] = F33.trw.spline[yrs >= 1972, c(1:7)]
F33.spline.avg.merge[,c(8:14)] = F33.rs.avg.spline

round(cor(F33.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F33.rs.avg), F33.trw.spline$Spline[yrs >= 1972])
lines(row.names(F33.rs.avg), F33.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1958 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F33.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F33.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F33.bestcor$TRW.smoothed = F33.trw.spline$Spline[yrs >= strt.yr]
F33.bestcor$RS.smoothed[rs.strt:length.trw] = F33.rs.avg.spline$NoDetrend
F33.bestcor$TRW.diff[2:length.trw] = diff(F33.bestcor$TRW.smoothed)
F33.bestcor$RS.diff[2:length.trw] = diff(F33.bestcor$RS.smoothed)

for (i in 2:nrow(F33.bestcor)){
  if (F33.bestcor$TRW.diff[i] > 0) {
    F33.bestcor$TRW.class[i] = 1
  } else {
    F33.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F33.bestcor)){
  if (F33.bestcor$RS.diff[i] > 0) {
    F33.bestcor$RS.class[i] = 1
  } else {
    F33.bestcor$RS.class[i] = 0
  }
}

sum(F33.bestcor$TRW.class, na.rm = TRUE)
sum(F33.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F33.bestcor)){
  if (F33.bestcor$TRW.class[i] == F33.bestcor$RS.class[i]){
    F33.bestcor$TRW.RS.agree[i] = 1
  } else {
    F33.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F33.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation (Matching methods) ###
plot(row.names(F33.rs.avg), F33.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(F33.rs.avg), F33.rs.avg.spline$ModHugershoff)
# Further comparisons and 1-year lags in Excel
strt.yr = 1959 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F33.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F33.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F33.bestcor$TRW.smoothed = F33.trw.spline$ModHugershoff[yrs >= strt.yr]
F33.bestcor$RS.smoothed[rs.strt:length.trw] = F33.rs.avg.spline$ModHugershoff
F33.bestcor$TRW.diff[2:length.trw] = diff(F33.bestcor$TRW.smoothed)
F33.bestcor$RS.diff[2:length.trw] = diff(F33.bestcor$RS.smoothed)

for (i in 2:nrow(F33.bestcor)){
  if (F33.bestcor$TRW.diff[i] > 0) {
    F33.bestcor$TRW.class[i] = 1
  } else {
    F33.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F33.bestcor)){
  if (F33.bestcor$RS.diff[i] > 0) {
    F33.bestcor$RS.class[i] = 1
  } else {
    F33.bestcor$RS.class[i] = 0
  }
}

sum(F33.bestcor$TRW.class, na.rm = TRUE)
sum(F33.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F33.bestcor)){
  if (F33.bestcor$TRW.class[i] == F33.bestcor$RS.class[i]){
    F33.bestcor$TRW.RS.agree[i] = 1
  } else {
    F33.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F33.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)