#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F25/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F25/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F25.trw = read.csv("F25_Chronologies.csv", row.names = 1)
F25.trw.spline = read.csv("F25_Chronologies_Spline.csv", row.names = 1)

#RS#
F25.rs.pt5 = read.csv("F25_LandsatPt5_Chronologies.csv", row.names = 1)
F25.rs.pt5.spline = read.csv("F25_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F25.rs.avg = read.csv("F25_LandsatAvg_Chronologies.csv", row.names = 1)
F25.rs.avg.spline = read.csv("F25_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F25.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F25.trw vs. F25.rs.pt5
F25.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F25.rs.pt5)),
                                 row.names = rownames(F25.rs.pt5))
colnames(F25.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F25.noisy.pt5.merge[,c(1:7)] = F25.trw[yrs >= 1972, c(1:7)]
F25.noisy.pt5.merge[,c(8:14)] = F25.rs.pt5

round(cor(F25.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F25.trw vs. F25.rs.avg
F25.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F25.rs.avg)),
                                 row.names = rownames(F25.rs.avg))
colnames(F25.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F25.noisy.avg.merge[,c(1:7)] = F25.trw[yrs >= 1972, c(1:7)]
F25.noisy.avg.merge[,c(8:14)] = F25.rs.avg

round(cor(F25.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F25.trw.spline vs. F25.rs.pt5.spline
F25.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F25.rs.pt5)),
                                 row.names = rownames(F25.rs.pt5))
colnames(F25.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F25.spline.pt5.merge[,c(1:7)] = F25.trw.spline[yrs >= 1972, c(1:7)]
F25.spline.pt5.merge[,c(8:14)] = F25.rs.pt5.spline

round(cor(F25.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F25.trw.spline vs. F25.rs.avg.spline
F25.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F25.rs.avg)),
                                 row.names = rownames(F25.rs.avg))
colnames(F25.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F25.spline.avg.merge[,c(1:7)] = F25.trw.spline[yrs >= 1972, c(1:7)]
F25.spline.avg.merge[,c(8:14)] = F25.rs.avg.spline

round(cor(F25.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F25.rs.avg), F25.trw.spline$Friedman[yrs >= 1972])
lines(row.names(F25.rs.avg), F25.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1931 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F25.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F25.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F25.bestcor$TRW.smoothed = F25.trw.spline$ModHugershoff[yrs >= strt.yr]
F25.bestcor$RS.smoothed[rs.strt:length.trw] = F25.rs.avg.spline$Ar
F25.bestcor$TRW.diff[2:length.trw] = diff(F25.bestcor$TRW.smoothed)
F25.bestcor$RS.diff[2:length.trw] = diff(F25.bestcor$RS.smoothed)

for (i in 2:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.diff[i] > 0) {
    F25.bestcor$TRW.class[i] = 1
  } else {
    F25.bestcor$TRW.class[i] = 0
  }
}

for (i in 46:nrow(F25.bestcor)){
  if (F25.bestcor$RS.diff[i] > 0) {
    F25.bestcor$RS.class[i] = 1
  } else {
    F25.bestcor$RS.class[i] = 0
  }
}

sum(F25.bestcor$TRW.class, na.rm = TRUE)
sum(F25.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 46:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.class[i] == F25.bestcor$RS.class[i]){
    F25.bestcor$TRW.RS.agree[i] = 1
  } else {
    F25.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F25.bestcor$TRW.RS.agree, na.rm = TRUE) / 43, 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(F25.rs.avg), F25.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(F25.rs.avg), F25.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1932 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F25.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F25.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F25.bestcor$TRW.smoothed = F25.trw.spline$ModNegExp[yrs >= strt.yr]
F25.bestcor$RS.smoothed[rs.strt:length.trw] = F25.rs.avg.spline$NoDetrend
F25.bestcor$TRW.diff[2:length.trw] = diff(F25.bestcor$TRW.smoothed)
F25.bestcor$RS.diff[2:length.trw] = diff(F25.bestcor$RS.smoothed)

for (i in 2:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.diff[i] > 0) {
    F25.bestcor$TRW.class[i] = 1
  } else {
    F25.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F25.bestcor)){
  if (F25.bestcor$RS.diff[i] > 0) {
    F25.bestcor$RS.class[i] = 1
  } else {
    F25.bestcor$RS.class[i] = 0
  }
}

sum(F25.bestcor$TRW.class, na.rm = TRUE)
sum(F25.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.class[i] == F25.bestcor$RS.class[i]){
    F25.bestcor$TRW.RS.agree[i] = 1
  } else {
    F25.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F25.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation (Matching method) ###
plot(row.names(F25.rs.avg), F25.trw.spline$Friedman[yrs >= 1972])
lines(row.names(F25.rs.avg), F25.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1935 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F25.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F25.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F25.bestcor$TRW.smoothed = F25.trw.spline$Friedman[yrs >= strt.yr]
F25.bestcor$RS.smoothed[rs.strt:length.trw] = F25.rs.avg.spline$Ar
F25.bestcor$TRW.diff[2:length.trw] = diff(F25.bestcor$TRW.smoothed)
F25.bestcor$RS.diff[2:length.trw] = diff(F25.bestcor$RS.smoothed)

for (i in 2:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.diff[i] > 0) {
    F25.bestcor$TRW.class[i] = 1
  } else {
    F25.bestcor$TRW.class[i] = 0
  }
}

for (i in 41:nrow(F25.bestcor)){
  if (F25.bestcor$RS.diff[i] > 0) {
    F25.bestcor$RS.class[i] = 1
  } else {
    F25.bestcor$RS.class[i] = 0
  }
}

sum(F25.bestcor$TRW.class, na.rm = TRUE)
sum(F25.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 41:nrow(F25.bestcor)){
  if (F25.bestcor$TRW.class[i] == F25.bestcor$RS.class[i]){
    F25.bestcor$TRW.RS.agree[i] = 1
  } else {
    F25.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F25.bestcor$TRW.RS.agree, na.rm = TRUE) / 44, 2)
