#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M27.trw = read.csv("M27_Chronologies.csv", row.names = 1)
M27.trw.spline = read.csv("M27_Chronologies_Spline.csv", row.names = 1)

#RS#
M27.rs.pt5 = read.csv("M27_LandsatPt5_Chronologies.csv", row.names = 1)
M27.rs.pt5.spline = read.csv("M27_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M27.rs.avg = read.csv("M27_LandsatAvg_Chronologies.csv", row.names = 1)
M27.rs.avg.spline = read.csv("M27_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M27.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M27.trw vs. M27.rs.pt5
M27.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M27.rs.pt5)),
                                 row.names = rownames(M27.rs.pt5))
colnames(M27.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M27.noisy.pt5.merge[c(8:47),c(1:7)] = M27.trw[,c(1:7)]
M27.noisy.pt5.merge[,c(8:14)] = M27.rs.pt5

round(cor(M27.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M27.trw vs. M27.rs.avg
M27.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M27.rs.avg)),
                                 row.names = rownames(M27.rs.avg))
colnames(M27.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M27.noisy.avg.merge[c(8:47),c(1:7)] = M27.trw[,c(1:7)]
M27.noisy.avg.merge[,c(8:14)] = M27.rs.avg

round(cor(M27.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M27.trw.spline vs. M27.rs.pt5.spline
M27.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M27.rs.pt5)),
                                 row.names = rownames(M27.rs.pt5))
colnames(M27.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M27.spline.pt5.merge[c(8:47),c(1:7)] = M27.trw.spline[,c(1:7)]
M27.spline.pt5.merge[,c(8:14)] = M27.rs.pt5.spline

round(cor(M27.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M27.trw.spline vs. M27.rs.avg.spline
M27.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M27.rs.avg)),
                                 row.names = rownames(M27.rs.avg))
colnames(M27.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M27.spline.avg.merge[c(8:47),c(1:7)] = M27.trw.spline[,c(1:7)]
M27.spline.avg.merge[,c(8:14)] = M27.rs.avg.spline

round(cor(M27.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M27.trw), M27.trw.spline$Spline)
lines(row.names(M27.rs.avg), M27.rs.avg.spline$Spline)
# Further comparisons and 1-year lags in Excel
strt.yr = 1972 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M27.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M27.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M27.bestcor$TRW.smoothed[8:47] = M27.trw.spline$Spline
M27.bestcor$RS.smoothed[rs.strt:length.trw] = M27.rs.avg.spline$Spline
M27.bestcor$TRW.diff[2:length.trw] = diff(M27.bestcor$TRW.smoothed)
M27.bestcor$RS.diff[2:length.trw] = diff(M27.bestcor$RS.smoothed)

for (i in 11:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.diff[i] > 0) {
    M27.bestcor$TRW.class[i] = 1
  } else {
    M27.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M27.bestcor)){
  if (M27.bestcor$RS.diff[i] > 0) {
    M27.bestcor$RS.class[i] = 1
  } else {
    M27.bestcor$RS.class[i] = 0
  }
}

sum(M27.bestcor$TRW.class, na.rm = TRUE)
sum(M27.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 11:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.class[i] == M27.bestcor$RS.class[i]){
    M27.bestcor$TRW.RS.agree[i] = 1
  } else {
    M27.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M27.bestcor$TRW.RS.agree, na.rm = TRUE) / 37, 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(M27.trw), M27.trw.spline$Spline)
lines(row.names(M27.rs.avg), M27.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1972 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M27.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M27.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M27.bestcor$TRW.smoothed[8:47] = M27.trw.spline$Spline
M27.bestcor$RS.smoothed[rs.strt:length.trw] = M27.rs.avg.spline$NoDetrend
M27.bestcor$TRW.diff[2:length.trw] = diff(M27.bestcor$TRW.smoothed)
M27.bestcor$RS.diff[2:length.trw] = diff(M27.bestcor$RS.smoothed)

for (i in 11:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.diff[i] > 0) {
    M27.bestcor$TRW.class[i] = 1
  } else {
    M27.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M27.bestcor)){
  if (M27.bestcor$RS.diff[i] > 0) {
    M27.bestcor$RS.class[i] = 1
  } else {
    M27.bestcor$RS.class[i] = 0
  }
}

sum(M27.bestcor$TRW.class, na.rm = TRUE)
sum(M27.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 11:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.class[i] == M27.bestcor$RS.class[i]){
    M27.bestcor$TRW.RS.agree[i] = 1
  } else {
    M27.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M27.bestcor$TRW.RS.agree, na.rm = TRUE) / 37, 2)

### Best correlation - Matching Methods ###
plot(row.names(M27.rs.avg), M27.trw.spline$Friedman[yrs >= 1972])
lines(row.names(M27.rs.avg), M27.rs.pt5.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1934 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M27.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M27.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M27.bestcor$TRW.smoothed = M27.trw.spline$Friedman[yrs >= strt.yr]
M27.bestcor$RS.smoothed[rs.strt:length.trw] = M27.rs.pt5.spline$Friedman
M27.bestcor$TRW.diff[2:length.trw] = diff(M27.bestcor$TRW.smoothed)
M27.bestcor$RS.diff[2:length.trw] = diff(M27.bestcor$RS.smoothed)

for (i in 2:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.diff[i] > 0) {
    M27.bestcor$TRW.class[i] = 1
  } else {
    M27.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M27.bestcor)){
  if (M27.bestcor$RS.diff[i] > 0) {
    M27.bestcor$RS.class[i] = 1
  } else {
    M27.bestcor$RS.class[i] = 0
  }
}

sum(M27.bestcor$TRW.class, na.rm = TRUE)
sum(M27.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M27.bestcor)){
  if (M27.bestcor$TRW.class[i] == M27.bestcor$RS.class[i]){
    M27.bestcor$TRW.RS.agree[i] = 1
  } else {
    M27.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M27.bestcor$TRW.RS.agree, na.rm = TRUE) / 37, 2)
