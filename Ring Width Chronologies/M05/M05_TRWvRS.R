#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M05/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M05/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M05.trw = read.csv("M05_Chronologies.csv", row.names = 1)
M05.trw.spline = read.csv("M05_Chronologies_Spline.csv", row.names = 1)

#RS#
M05.rs.pt5 = read.csv("M05_LandsatPt5_Chronologies.csv", row.names = 1)
M05.rs.pt5.spline = read.csv("M05_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M05.rs.avg = read.csv("M05_LandsatAvg_Chronologies.csv", row.names = 1)
M05.rs.avg.spline = read.csv("M05_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M05.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M05.trw vs. M05.rs.pt5
M05.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M05.rs.pt5)),
                                 row.names = rownames(M05.rs.pt5))
colnames(M05.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M05.noisy.pt5.merge[,c(1:7)] = M05.trw[yrs >= 1972, c(1:7)]
M05.noisy.pt5.merge[,c(8:14)] = M05.rs.pt5

round(cor(M05.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M05.trw vs. M05.rs.avg
M05.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M05.rs.avg)),
                                 row.names = rownames(M05.rs.avg))
colnames(M05.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M05.noisy.avg.merge[,c(1:7)] = M05.trw[yrs >= 1972, c(1:7)]
M05.noisy.avg.merge[,c(8:14)] = M05.rs.avg

round(cor(M05.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M05.trw.spline vs. M05.rs.pt5.spline
M05.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M05.rs.pt5)),
                                 row.names = rownames(M05.rs.pt5))
colnames(M05.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M05.spline.pt5.merge[,c(1:7)] = M05.trw.spline[yrs >= 1972, c(1:7)]
M05.spline.pt5.merge[,c(8:14)] = M05.rs.pt5.spline

round(cor(M05.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M05.trw.spline vs. M05.rs.avg.spline
M05.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M05.rs.avg)),
                                 row.names = rownames(M05.rs.avg))
colnames(M05.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M05.spline.avg.merge[,c(1:7)] = M05.trw.spline[yrs >= 1972, c(1:7)]
M05.spline.avg.merge[,c(8:14)] = M05.rs.avg.spline

round(cor(M05.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M05.rs.avg), M05.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(M05.rs.avg), M05.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1931 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M05.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M05.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M05.bestcor$TRW.smoothed = M05.trw.spline$ModHugershoff[yrs >= strt.yr]
M05.bestcor$RS.smoothed[rs.strt:length.trw] = M05.rs.avg.spline$Ar
M05.bestcor$TRW.diff[2:length.trw] = diff(M05.bestcor$TRW.smoothed)
M05.bestcor$RS.diff[2:length.trw] = diff(M05.bestcor$RS.smoothed)

for (i in 2:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.diff[i] > 0) {
    M05.bestcor$TRW.class[i] = 1
  } else {
    M05.bestcor$TRW.class[i] = 0
  }
}

for (i in 46:nrow(M05.bestcor)){
  if (M05.bestcor$RS.diff[i] > 0) {
    M05.bestcor$RS.class[i] = 1
  } else {
    M05.bestcor$RS.class[i] = 0
  }
}

sum(M05.bestcor$TRW.class, na.rm = TRUE)
sum(M05.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 46:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.class[i] == M05.bestcor$RS.class[i]){
    M05.bestcor$TRW.RS.agree[i] = 1
  } else {
    M05.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M05.bestcor$TRW.RS.agree, na.rm = TRUE) / 43, 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(M05.rs.avg), M05.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(M05.rs.avg), M05.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1932 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M05.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M05.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M05.bestcor$TRW.smoothed = M05.trw.spline$ModNegExp[yrs >= strt.yr]
M05.bestcor$RS.smoothed[rs.strt:length.trw] = M05.rs.avg.spline$NoDetrend
M05.bestcor$TRW.diff[2:length.trw] = diff(M05.bestcor$TRW.smoothed)
M05.bestcor$RS.diff[2:length.trw] = diff(M05.bestcor$RS.smoothed)

for (i in 2:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.diff[i] > 0) {
    M05.bestcor$TRW.class[i] = 1
  } else {
    M05.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M05.bestcor)){
  if (M05.bestcor$RS.diff[i] > 0) {
    M05.bestcor$RS.class[i] = 1
  } else {
    M05.bestcor$RS.class[i] = 0
  }
}

sum(M05.bestcor$TRW.class, na.rm = TRUE)
sum(M05.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.class[i] == M05.bestcor$RS.class[i]){
    M05.bestcor$TRW.RS.agree[i] = 1
  } else {
    M05.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M05.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation (Matching method) ###
plot(row.names(M05.rs.avg), M05.trw.spline$Ar[yrs >= 1972])
lines(row.names(M05.rs.avg), M05.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1924 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M05.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M05.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M05.bestcor$TRW.smoothed = M05.trw.spline$Ar[yrs >= strt.yr]
M05.bestcor$RS.smoothed[rs.strt:length.trw] = M05.rs.avg.spline$Ar
M05.bestcor$TRW.diff[2:length.trw] = diff(M05.bestcor$TRW.smoothed)
M05.bestcor$RS.diff[2:length.trw] = diff(M05.bestcor$RS.smoothed)

for (i in 2:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.diff[i] > 0) {
    M05.bestcor$TRW.class[i] = 1
  } else {
    M05.bestcor$TRW.class[i] = 0
  }
}

for (i in 53:nrow(M05.bestcor)){
  if (M05.bestcor$RS.diff[i] > 0) {
    M05.bestcor$RS.class[i] = 1
  } else {
    M05.bestcor$RS.class[i] = 0
  }
}

sum(M05.bestcor$TRW.class, na.rm = TRUE)
sum(M05.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 53:nrow(M05.bestcor)){
  if (M05.bestcor$TRW.class[i] == M05.bestcor$RS.class[i]){
    M05.bestcor$TRW.RS.agree[i] = 1
  } else {
    M05.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M05.bestcor$TRW.RS.agree, na.rm = TRUE) / 43, 2)
