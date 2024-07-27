#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M07.trw = read.csv("M07_Chronologies.csv", row.names = 1)
M07.trw.spline = read.csv("M07_Chronologies_Spline.csv", row.names = 1)
M07.tsuga.trw = read.csv("M07_Tsuga_Chronologies.csv", row.names = 1)
M07.tsuga.trw.spline = read.csv("M07_Tsuga_Chronologies_Spline.csv", row.names = 1)

#RS#
M07.rs.pt5 = read.csv("M07_LandsatPt5_Chronologies.csv", row.names = 1)
M07.rs.pt5.spline = read.csv("M07_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M07.rs.avg = read.csv("M07_LandsatAvg_Chronologies.csv", row.names = 1)
M07.rs.avg.spline = read.csv("M07_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(M07.trw)
yrs.tsuga = row.names(M07.tsuga.trw) #same

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# M07.trw vs. M07.rs.pt5
M07.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.pt5)),
                                 row.names = rownames(M07.rs.pt5))
colnames(M07.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M07.noisy.pt5.merge[,c(1:7)] = M07.trw[yrs >= 1972, c(1:7)]
M07.noisy.pt5.merge[,c(8:14)] = M07.rs.pt5

round(cor(M07.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M07.trw vs. M07.rs.avg
M07.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.avg)),
                                 row.names = rownames(M07.rs.avg))
colnames(M07.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M07.noisy.avg.merge[,c(1:7)] = M07.trw[yrs >= 1972, c(1:7)]
M07.noisy.avg.merge[,c(8:14)] = M07.rs.avg

round(cor(M07.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M07.trw.spline vs. M07.rs.pt5.spline
M07.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.pt5)),
                                 row.names = rownames(M07.rs.pt5))
colnames(M07.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M07.spline.pt5.merge[,c(1:7)] = M07.trw.spline[yrs >= 1972, c(1:7)]
M07.spline.pt5.merge[,c(8:14)] = M07.rs.pt5.spline

round(cor(M07.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M07.trw.spline vs. M07.rs.avg.spline
M07.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.avg)),
                                 row.names = rownames(M07.rs.avg))
colnames(M07.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M07.spline.avg.merge[,c(1:7)] = M07.trw.spline[yrs >= 1972, c(1:7)]
M07.spline.avg.merge[,c(8:14)] = M07.rs.avg.spline

round(cor(M07.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (tsuga)###
# M07.tsuga.trw vs. M07.rs.pt5
M07.tsuga.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.pt5)),
                                 row.names = rownames(M07.rs.pt5))
colnames(M07.tsuga.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M07.tsuga.noisy.pt5.merge[,c(1:7)] = M07.tsuga.trw[yrs >= 1972, c(1:7)]
M07.tsuga.noisy.pt5.merge[,c(8:14)] = M07.rs.pt5

round(cor(M07.tsuga.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M07.tsuga.trw vs. M07.rs.avg
M07.tsuga.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.avg)),
                                 row.names = rownames(M07.rs.avg))
colnames(M07.tsuga.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M07.tsuga.noisy.avg.merge[,c(1:7)] = M07.tsuga.trw[yrs >= 1972, c(1:7)]
M07.tsuga.noisy.avg.merge[,c(8:14)] = M07.rs.avg

round(cor(M07.tsuga.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (tsuga)###
# M07.tsuga.trw.spline vs. M07.rs.pt5.spline
M07.tsuga.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.pt5)),
                                  row.names = rownames(M07.rs.pt5))
colnames(M07.tsuga.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M07.tsuga.spline.pt5.merge[,c(1:7)] = M07.tsuga.trw.spline[yrs >= 1972, c(1:7)]
M07.tsuga.spline.pt5.merge[,c(8:14)] = M07.rs.pt5.spline

round(cor(M07.tsuga.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M07.tsuga.trw.spline vs. M07.rs.avg.spline
M07.tsuga.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M07.rs.avg)),
                                        row.names = rownames(M07.rs.avg))
colnames(M07.tsuga.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
M07.tsuga.spline.avg.merge[,c(1:7)] = M07.tsuga.trw.spline[yrs >= 1972, c(1:7)]
M07.tsuga.spline.avg.merge[,c(8:14)] = M07.rs.avg.spline

round(cor(M07.tsuga.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M07.rs.avg), M07.tsuga.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(M07.rs.avg), M07.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1894 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M07.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M07.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M07.bestcor$TRW.smoothed = M07.tsuga.trw.spline$ModNegExp[yrs >= strt.yr]
M07.bestcor$RS.smoothed[rs.strt:length.trw] = M07.rs.avg.spline$Ar
M07.bestcor$TRW.diff[2:length.trw] = diff(M07.bestcor$TRW.smoothed)
M07.bestcor$RS.diff[2:length.trw] = diff(M07.bestcor$RS.smoothed)

for (i in 2:nrow(M07.bestcor)){
  if (M07.bestcor$TRW.diff[i] > 0) {
    M07.bestcor$TRW.class[i] = 1
  } else {
    M07.bestcor$TRW.class[i] = 0
  }
}

for (i in 84:nrow(M07.bestcor)){
  if (M07.bestcor$RS.diff[i] > 0) {
    M07.bestcor$RS.class[i] = 1
  } else {
    M07.bestcor$RS.class[i] = 0
  }
}

sum(M07.bestcor$TRW.class, na.rm = TRUE)
sum(M07.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 84:nrow(M07.bestcor)){
  if (M07.bestcor$TRW.class[i] == M07.bestcor$RS.class[i]){
    M07.bestcor$TRW.RS.agree[i] = 1
  } else {
    M07.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M07.bestcor$TRW.RS.agree, na.rm = TRUE) / 42, 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(M07.rs.avg), M07.tsuga.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(M07.rs.avg), M07.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1894 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M07.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M07.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M07.bestcor$TRW.smoothed = M07.tsuga.trw.spline$ModNegExp[yrs >= strt.yr]
M07.bestcor$RS.smoothed[rs.strt:length.trw] = M07.rs.avg.spline$NoDetrend
M07.bestcor$TRW.diff[2:length.trw] = diff(M07.bestcor$TRW.smoothed)
M07.bestcor$RS.diff[2:length.trw] = diff(M07.bestcor$RS.smoothed)

for (i in 2:nrow(M07.bestcor)){
  if (M07.bestcor$TRW.diff[i] > 0) {
    M07.bestcor$TRW.class[i] = 1
  } else {
    M07.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M07.bestcor)){
  if (M07.bestcor$RS.diff[i] > 0) {
    M07.bestcor$RS.class[i] = 1
  } else {
    M07.bestcor$RS.class[i] = 0
  }
}

sum(M07.bestcor$TRW.class, na.rm = TRUE)
sum(M07.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M07.bestcor)){
  if (M07.bestcor$TRW.class[i] == M07.bestcor$RS.class[i]){
    M07.bestcor$TRW.RS.agree[i] = 1
  } else {
    M07.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M07.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

