#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F23.trw = read.csv("F23_Chronologies.csv", row.names = 1)
F23.trw.spline = read.csv("F23_Chronologies_Spline.csv", row.names = 1)

#RS#
F23.rs.pt5 = read.csv("F23_LandsatPt5_Chronologies.csv", row.names = 1)
F23.rs.pt5.spline = read.csv("F23_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F23.rs.avg = read.csv("F23_LandsatAvg_Chronologies.csv", row.names = 1)
F23.rs.avg.spline = read.csv("F23_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F23.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F23.trw vs. F23.rs.pt5
F23.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F23.rs.pt5)),
                                 row.names = rownames(F23.rs.pt5))
colnames(F23.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F23.noisy.pt5.merge[,c(1:7)] = F23.trw[yrs >= 1972, c(1:7)]
F23.noisy.pt5.merge[,c(8:14)] = F23.rs.pt5

round(cor(F23.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F23.trw vs. F23.rs.avg
F23.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F23.rs.avg)),
                                 row.names = rownames(F23.rs.avg))
colnames(F23.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F23.noisy.avg.merge[,c(1:7)] = F23.trw[yrs >= 1972, c(1:7)]
F23.noisy.avg.merge[,c(8:14)] = F23.rs.avg

round(cor(F23.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F23.trw.spline vs. F23.rs.pt5.spline
F23.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F23.rs.pt5)),
                                 row.names = rownames(F23.rs.pt5))
colnames(F23.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F23.spline.pt5.merge[,c(1:7)] = F23.trw.spline[yrs >= 1972, c(1:7)]
F23.spline.pt5.merge[,c(8:14)] = F23.rs.pt5.spline

round(cor(F23.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F23.trw.spline vs. F23.rs.avg.spline
F23.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F23.rs.avg)),
                                 row.names = rownames(F23.rs.avg))
colnames(F23.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F23.spline.avg.merge[,c(1:7)] = F23.trw.spline[yrs >= 1972, c(1:7)]
F23.spline.avg.merge[,c(8:14)] = F23.rs.avg.spline

round(cor(F23.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F23.rs.avg), F23.trw.spline$Ar[yrs >= 1972])
lines(row.names(F23.rs.avg), F23.rs.pt5.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1896 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F23.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F23.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F23.bestcor$TRW.smoothed = F23.trw.spline$Ar[yrs >= strt.yr]
F23.bestcor$RS.smoothed[rs.strt:length.trw] = F23.rs.pt5.spline$Friedman
F23.bestcor$TRW.diff[2:length.trw] = diff(F23.bestcor$TRW.smoothed)
F23.bestcor$RS.diff[2:length.trw] = diff(F23.bestcor$RS.smoothed)

for (i in 2:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.diff[i] > 0) {
    F23.bestcor$TRW.class[i] = 1
  } else {
    F23.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$RS.diff[i] > 0) {
    F23.bestcor$RS.class[i] = 1
  } else {
    F23.bestcor$RS.class[i] = 0
  }
}

sum(F23.bestcor$TRW.class, na.rm = TRUE)
sum(F23.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.class[i] == F23.bestcor$RS.class[i]){
    F23.bestcor$TRW.RS.agree[i] = 1
  } else {
    F23.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F23.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(F23.rs.avg), F23.trw.spline$Ar[yrs >= 1972])
lines(row.names(F23.rs.avg), F23.rs.avg.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1896 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F23.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F23.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F23.bestcor$TRW.smoothed = F23.trw.spline$Ar[yrs >= strt.yr]
F23.bestcor$RS.smoothed[rs.strt:length.trw] = F23.rs.avg.spline$NoDetrend
F23.bestcor$TRW.diff[2:length.trw] = diff(F23.bestcor$TRW.smoothed)
F23.bestcor$RS.diff[2:length.trw] = diff(F23.bestcor$RS.smoothed)

for (i in 2:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.diff[i] > 0) {
    F23.bestcor$TRW.class[i] = 1
  } else {
    F23.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$RS.diff[i] > 0) {
    F23.bestcor$RS.class[i] = 1
  } else {
    F23.bestcor$RS.class[i] = 0
  }
}

sum(F23.bestcor$TRW.class, na.rm = TRUE)
sum(F23.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.class[i] == F23.bestcor$RS.class[i]){
    F23.bestcor$TRW.RS.agree[i] = 1
  } else {
    F23.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F23.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(F23.rs.avg), F23.trw.spline$Friedman[yrs >= 1972])
lines(row.names(F23.rs.avg), F23.rs.pt5.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1891 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F23.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F23.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F23.bestcor$TRW.smoothed = F23.trw.spline$Friedman[yrs >= strt.yr]
F23.bestcor$RS.smoothed[rs.strt:length.trw] = F23.rs.pt5.spline$Friedman
F23.bestcor$TRW.diff[2:length.trw] = diff(F23.bestcor$TRW.smoothed)
F23.bestcor$RS.diff[2:length.trw] = diff(F23.bestcor$RS.smoothed)

for (i in 2:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.diff[i] > 0) {
    F23.bestcor$TRW.class[i] = 1
  } else {
    F23.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$RS.diff[i] > 0) {
    F23.bestcor$RS.class[i] = 1
  } else {
    F23.bestcor$RS.class[i] = 0
  }
}

sum(F23.bestcor$TRW.class, na.rm = TRUE)
sum(F23.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F23.bestcor)){
  if (F23.bestcor$TRW.class[i] == F23.bestcor$RS.class[i]){
    F23.bestcor$TRW.RS.agree[i] = 1
  } else {
    F23.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F23.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

