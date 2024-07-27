#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M20.trw = read.csv("M20_Chronologies.csv", row.names = 1)
M20.trw.spline = read.csv("M20_Chronologies_Spline.csv", row.names = 1)
M20.thuja.trw = read.csv("M20_Thuja_Chronologies.csv", row.names = 1)
M20.thuja.trw.spline = read.csv("M20_Thuja_Chronologies_Spline.csv", row.names = 1)

#RS#
M20.rs.pt5 = read.csv("M20_LandsatPt5_Chronologies.csv", row.names = 1)
M20.rs.pt5.spline = read.csv("M20_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M20.rs.avg = read.csv("M20_LandsatAvg_Chronologies.csv", row.names = 1)
M20.rs.avg.spline = read.csv("M20_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M20.trw)
yrs.thuja = row.names(M20.thuja.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M20.trw vs. M20.rs.pt5
M20.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.pt5)),
                                 row.names = rownames(M20.rs.pt5))
colnames(M20.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M20.noisy.pt5.merge[,c(1:7)] = M20.trw[yrs >= 1972, c(1:7)]
M20.noisy.pt5.merge[,c(8:14)] = M20.rs.pt5

round(cor(M20.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M20.trw vs. M20.rs.avg
M20.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.avg)),
                                 row.names = rownames(M20.rs.avg))
colnames(M20.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M20.noisy.avg.merge[,c(1:7)] = M20.trw[yrs >= 1972, c(1:7)]
M20.noisy.avg.merge[,c(8:14)] = M20.rs.avg

round(cor(M20.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M20.trw.spline vs. M20.rs.pt5.spline
M20.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.pt5)),
                                 row.names = rownames(M20.rs.pt5))
colnames(M20.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M20.spline.pt5.merge[,c(1:7)] = M20.trw.spline[yrs >= 1972, c(1:7)]
M20.spline.pt5.merge[,c(8:14)] = M20.rs.pt5.spline

round(cor(M20.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M20.trw.spline vs. M20.rs.avg.spline
M20.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.avg)),
                                 row.names = rownames(M20.rs.avg))
colnames(M20.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M20.spline.avg.merge[,c(1:7)] = M20.trw.spline[yrs >= 1972, c(1:7)]
M20.spline.avg.merge[,c(8:14)] = M20.rs.avg.spline

round(cor(M20.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (Thuja)###
# M20.thuja.trw vs. M20.rs.pt5
M20.thuja.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.pt5)),
                                 row.names = rownames(M20.rs.pt5))
colnames(M20.thuja.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M20.thuja.noisy.pt5.merge[,c(1:7)] = M20.thuja.trw[yrs >= 1972, c(1:7)]
M20.thuja.noisy.pt5.merge[,c(8:14)] = M20.rs.pt5

round(cor(M20.thuja.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M20.thuja.trw vs. M20.rs.avg
M20.thuja.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.avg)),
                                 row.names = rownames(M20.rs.avg))
colnames(M20.thuja.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M20.thuja.noisy.avg.merge[,c(1:7)] = M20.thuja.trw[yrs >= 1972, c(1:7)]
M20.thuja.noisy.avg.merge[,c(8:14)] = M20.rs.avg

round(cor(M20.thuja.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Thuja)###
# M20.thuja.trw.spline vs. M20.rs.pt5.spline
M20.thuja.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.pt5)),
                                  row.names = rownames(M20.rs.pt5))
colnames(M20.thuja.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M20.thuja.spline.pt5.merge[,c(1:7)] = M20.thuja.trw.spline[yrs >= 1972, c(1:7)]
M20.thuja.spline.pt5.merge[,c(8:14)] = M20.rs.pt5.spline

round(cor(M20.thuja.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M20.thuja.trw.spline vs. M20.rs.avg.spline
M20.thuja.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M20.rs.avg)),
                                        row.names = rownames(M20.rs.avg))
colnames(M20.thuja.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
M20.thuja.spline.avg.merge[,c(1:7)] = M20.thuja.trw.spline[yrs >= 1972, c(1:7)]
M20.thuja.spline.avg.merge[,c(8:14)] = M20.rs.avg.spline

round(cor(M20.thuja.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M20.rs.avg), M20.trw.spline$Spline[yrs >= 1972])
lines(row.names(M20.rs.avg), M20.rs.pt5.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1936 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M20.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M20.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M20.bestcor$TRW.smoothed = M20.trw.spline$Spline[yrs >= strt.yr]
M20.bestcor$RS.smoothed[rs.strt:length.trw] = M20.rs.pt5.spline$Friedman
M20.bestcor$TRW.diff[2:length.trw] = diff(M20.bestcor$TRW.smoothed)
M20.bestcor$RS.diff[2:length.trw] = diff(M20.bestcor$RS.smoothed)

for (i in 2:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.diff[i] > 0) {
    M20.bestcor$TRW.class[i] = 1
  } else {
    M20.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$RS.diff[i] > 0) {
    M20.bestcor$RS.class[i] = 1
  } else {
    M20.bestcor$RS.class[i] = 0
  }
}

sum(M20.bestcor$TRW.class, na.rm = TRUE)
sum(M20.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.class[i] == M20.bestcor$RS.class[i]){
    M20.bestcor$TRW.RS.agree[i] = 1
  } else {
    M20.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M20.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(M20.rs.avg), M20.thuja.trw.spline$Friedman[yrs >= 1972])
lines(row.names(M20.rs.avg), M20.rs.pt5.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1934 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M20.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M20.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M20.bestcor$TRW.smoothed = M20.thuja.trw.spline$Friedman[yrs >= strt.yr]
M20.bestcor$RS.smoothed[rs.strt:length.trw] = M20.rs.pt5.spline$NoDetrend
M20.bestcor$TRW.diff[2:length.trw] = diff(M20.bestcor$TRW.smoothed)
M20.bestcor$RS.diff[2:length.trw] = diff(M20.bestcor$RS.smoothed)

for (i in 2:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.diff[i] > 0) {
    M20.bestcor$TRW.class[i] = 1
  } else {
    M20.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$RS.diff[i] > 0) {
    M20.bestcor$RS.class[i] = 1
  } else {
    M20.bestcor$RS.class[i] = 0
  }
}

sum(M20.bestcor$TRW.class, na.rm = TRUE)
sum(M20.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.class[i] == M20.bestcor$RS.class[i]){
    M20.bestcor$TRW.RS.agree[i] = 1
  } else {
    M20.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M20.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(M20.rs.avg), M20.thuja.trw.spline$Friedman[yrs >= 1972])
lines(row.names(M20.rs.avg), M20.rs.pt5.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1934 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M20.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M20.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M20.bestcor$TRW.smoothed = M20.trw.spline$Friedman[yrs >= strt.yr]
M20.bestcor$RS.smoothed[rs.strt:length.trw] = M20.rs.pt5.spline$Friedman
M20.bestcor$TRW.diff[2:length.trw] = diff(M20.bestcor$TRW.smoothed)
M20.bestcor$RS.diff[2:length.trw] = diff(M20.bestcor$RS.smoothed)

for (i in 2:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.diff[i] > 0) {
    M20.bestcor$TRW.class[i] = 1
  } else {
    M20.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$RS.diff[i] > 0) {
    M20.bestcor$RS.class[i] = 1
  } else {
    M20.bestcor$RS.class[i] = 0
  }
}

sum(M20.bestcor$TRW.class, na.rm = TRUE)
sum(M20.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M20.bestcor)){
  if (M20.bestcor$TRW.class[i] == M20.bestcor$RS.class[i]){
    M20.bestcor$TRW.RS.agree[i] = 1
  } else {
    M20.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M20.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

