#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F30/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F30/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F30.acer.trw = read.csv("F30_Acer_Chronologies.csv", row.names = 1)
F30.acer.trw.spline = read.csv("F30_Acer_Chronologies_Spline.csv", row.names = 1)
F30.betula.trw = read.csv("F30_Betula_Chronologies.csv", row.names = 1)
F30.betula.trw.spline = read.csv("F30_Betula_Chronologies_Spline.csv", row.names = 1)

#RS#
F30.rs.pt5 = read.csv("F30_LandsatPt5_Chronologies.csv", row.names = 1)
F30.rs.pt5.spline = read.csv("F30_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F30.rs.avg = read.csv("F30_LandsatAvg_Chronologies.csv", row.names = 1)
F30.rs.avg.spline = read.csv("F30_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs.acer = row.names(F30.acer.trw)
yrs.betula = row.names(F30.betula.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS (Acer)###
# F30.acer.trw vs. F30.rs.pt5
F30.acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.pt5)),
                                 row.names = rownames(F30.rs.pt5))
colnames(F30.acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F30.acer.noisy.pt5.merge[,c(1:7)] = F30.acer.trw[yrs.acer >= 1972, c(1:7)]
F30.acer.noisy.pt5.merge[,c(8:14)] = F30.rs.pt5

round(cor(F30.acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F30.trw vs. F30.rs.avg
F30.acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.avg)),
                                 row.names = rownames(F30.rs.avg))
colnames(F30.acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F30.acer.noisy.avg.merge[,c(1:7)] = F30.acer.trw[yrs.acer >= 1972, c(1:7)]
F30.acer.noisy.avg.merge[,c(8:14)] = F30.rs.avg

round(cor(F30.acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F30.trw.spline vs. F30.rs.pt5.spline
F30.acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.pt5)),
                                 row.names = rownames(F30.rs.pt5))
colnames(F30.acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F30.acer.spline.pt5.merge[,c(1:7)] = F30.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
F30.acer.spline.pt5.merge[,c(8:14)] = F30.rs.pt5.spline

round(cor(F30.acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F30.trw.spline vs. F30.rs.avg.spline
F30.acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.avg)),
                                 row.names = rownames(F30.rs.avg))
colnames(F30.acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F30.acer.spline.avg.merge[,c(1:7)] = F30.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
F30.acer.spline.avg.merge[,c(8:14)] = F30.rs.avg.spline

round(cor(F30.acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (betula)###
# F30.betula.trw vs. F30.rs.pt5
F30.betula.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.pt5)),
                                 row.names = rownames(F30.rs.pt5))
colnames(F30.betula.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F30.betula.noisy.pt5.merge[,c(1:7)] = F30.betula.trw[yrs.betula >= 1972, c(1:7)]
F30.betula.noisy.pt5.merge[,c(8:14)] = F30.rs.pt5

round(cor(F30.betula.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F30.betula.trw vs. F30.rs.avg
F30.betula.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.avg)),
                                 row.names = rownames(F30.rs.avg))
colnames(F30.betula.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F30.betula.noisy.avg.merge[,c(1:7)] = F30.betula.trw[yrs.betula >= 1972, c(1:7)]
F30.betula.noisy.avg.merge[,c(8:14)] = F30.rs.avg

round(cor(F30.betula.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (betula)###
# F30.betula.trw.spline vs. F30.rs.pt5.spline
F30.betula.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.pt5)),
                                  row.names = rownames(F30.rs.pt5))
colnames(F30.betula.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F30.betula.spline.pt5.merge[,c(1:7)] = F30.betula.trw.spline[yrs.betula >= 1972, c(1:7)]
F30.betula.spline.pt5.merge[,c(8:14)] = F30.rs.pt5.spline

round(cor(F30.betula.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F30.betula.trw.spline vs. F30.rs.avg.spline
F30.betula.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F30.rs.avg)),
                                        row.names = rownames(F30.rs.avg))
colnames(F30.betula.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F30.betula.spline.avg.merge[,c(1:7)] = F30.betula.trw.spline[yrs.betula >= 1972, c(1:7)]
F30.betula.spline.avg.merge[,c(8:14)] = F30.rs.avg.spline

round(cor(F30.betula.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F30.rs.avg), F30.acer.trw.spline$Ar[yrs.acer >= 1972])
lines(row.names(F30.rs.avg), F30.rs.pt5.spline$Spline)
# Further comparisons and 1-year lags in Excel
strt.yr = 1939 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F30.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F30.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F30.bestcor$TRW.smoothed = F30.acer.trw.spline$Ar[yrs.acer >= strt.yr]
F30.bestcor$RS.smoothed[rs.strt:length.trw] = F30.rs.pt5.spline$Spline
F30.bestcor$TRW.diff[2:length.trw] = diff(F30.bestcor$TRW.smoothed)
F30.bestcor$RS.diff[2:length.trw] = diff(F30.bestcor$RS.smoothed)

for (i in 2:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.diff[i] > 0) {
    F30.bestcor$TRW.class[i] = 1
  } else {
    F30.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$RS.diff[i] > 0) {
    F30.bestcor$RS.class[i] = 1
  } else {
    F30.bestcor$RS.class[i] = 0
  }
}

sum(F30.bestcor$TRW.class, na.rm = TRUE)
sum(F30.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.class[i] == F30.bestcor$RS.class[i]){
    F30.bestcor$TRW.RS.agree[i] = 1
  } else {
    F30.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F30.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(F30.rs.avg), F30.acer.trw.spline$ModHugershoff[yrs.acer >= 1972])
lines(row.names(F30.rs.avg), F30.rs.pt5.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1937 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F30.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F30.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F30.bestcor$TRW.smoothed = F30.acer.trw.spline$ModHugershoff[yrs.acer >= strt.yr]
F30.bestcor$RS.smoothed[rs.strt:length.trw] = F30.rs.avg.spline$ModHugershoff
F30.bestcor$TRW.diff[2:length.trw] = diff(F30.bestcor$TRW.smoothed)
F30.bestcor$RS.diff[2:length.trw] = diff(F30.bestcor$RS.smoothed)

for (i in 2:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.diff[i] > 0) {
    F30.bestcor$TRW.class[i] = 1
  } else {
    F30.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$RS.diff[i] > 0) {
    F30.bestcor$RS.class[i] = 1
  } else {
    F30.bestcor$RS.class[i] = 0
  }
}

sum(F30.bestcor$TRW.class, na.rm = TRUE)
sum(F30.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.class[i] == F30.bestcor$RS.class[i]){
    F30.bestcor$TRW.RS.agree[i] = 1
  } else {
    F30.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F30.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(F30.rs.avg), F30.acer.trw.spline$Friedman[yrs.acer >= 1972])
lines(row.names(F30.rs.avg), F30.rs.avg.spline$Friedman)
# Further comparisons and 1-year lags in Excel
strt.yr = 1940 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F30.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F30.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F30.bestcor$TRW.smoothed = F30.acer.trw.spline$Friedman[yrs.acer >= strt.yr]
F30.bestcor$RS.smoothed[rs.strt:length.trw] = F30.rs.avg.spline$Friedman
F30.bestcor$TRW.diff[2:length.trw] = diff(F30.bestcor$TRW.smoothed)
F30.bestcor$RS.diff[2:length.trw] = diff(F30.bestcor$RS.smoothed)

for (i in 2:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.diff[i] > 0) {
    F30.bestcor$TRW.class[i] = 1
  } else {
    F30.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$RS.diff[i] > 0) {
    F30.bestcor$RS.class[i] = 1
  } else {
    F30.bestcor$RS.class[i] = 0
  }
}

sum(F30.bestcor$TRW.class, na.rm = TRUE)
sum(F30.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F30.bestcor)){
  if (F30.bestcor$TRW.class[i] == F30.bestcor$RS.class[i]){
    F30.bestcor$TRW.RS.agree[i] = 1
  } else {
    F30.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F30.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

