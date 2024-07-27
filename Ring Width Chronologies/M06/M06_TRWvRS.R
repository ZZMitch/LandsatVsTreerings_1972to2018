#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M06.acer.trw = read.csv("M06_Acer_Chronologies.csv", row.names = 1)
M06.acer.trw.spline = read.csv("M06_Acer_Chronologies_Spline.csv", row.names = 1)
M06.quercus.trw = read.csv("M06_Quercus_Chronologies.csv", row.names = 1)
M06.quercus.trw.spline = read.csv("M06_Quercus_Chronologies_Spline.csv", row.names = 1)

#RS#
M06.rs.pt5 = read.csv("M06_LandsatPt5_Chronologies.csv", row.names = 1)
M06.rs.pt5.spline = read.csv("M06_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M06.rs.avg = read.csv("M06_LandsatAvg_Chronologies.csv", row.names = 1)
M06.rs.avg.spline = read.csv("M06_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs.acer = row.names(M06.acer.trw)
yrs.quercus = row.names(M06.quercus.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS (Acer)###
# M06.acer.trw vs. M06.rs.pt5
M06.acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.pt5)),
                                 row.names = rownames(M06.rs.pt5))
colnames(M06.acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M06.acer.noisy.pt5.merge[,c(1:7)] = M06.acer.trw[yrs.acer >= 1972, c(1:7)]
M06.acer.noisy.pt5.merge[,c(8:14)] = M06.rs.pt5

round(cor(M06.acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M06.trw vs. M06.rs.avg
M06.acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.avg)),
                                 row.names = rownames(M06.rs.avg))
colnames(M06.acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M06.acer.noisy.avg.merge[,c(1:7)] = M06.acer.trw[yrs.acer >= 1972, c(1:7)]
M06.acer.noisy.avg.merge[,c(8:14)] = M06.rs.avg

round(cor(M06.acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M06.trw.spline vs. M06.rs.pt5.spline
M06.acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.pt5)),
                                 row.names = rownames(M06.rs.pt5))
colnames(M06.acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M06.acer.spline.pt5.merge[,c(1:7)] = M06.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
M06.acer.spline.pt5.merge[,c(8:14)] = M06.rs.pt5.spline

round(cor(M06.acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M06.trw.spline vs. M06.rs.avg.spline
M06.acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.avg)),
                                 row.names = rownames(M06.rs.avg))
colnames(M06.acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M06.acer.spline.avg.merge[,c(1:7)] = M06.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
M06.acer.spline.avg.merge[,c(8:14)] = M06.rs.avg.spline

round(cor(M06.acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (Quercus)###
# M06.quercus.trw vs. M06.rs.pt5
M06.quercus.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.pt5)),
                                 row.names = rownames(M06.rs.pt5))
colnames(M06.quercus.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M06.quercus.noisy.pt5.merge[,c(1:7)] = M06.quercus.trw[yrs.quercus >= 1972, c(1:7)]
M06.quercus.noisy.pt5.merge[,c(8:14)] = M06.rs.pt5

round(cor(M06.quercus.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M06.quercus.trw vs. M06.rs.avg
M06.quercus.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.avg)),
                                 row.names = rownames(M06.rs.avg))
colnames(M06.quercus.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M06.quercus.noisy.avg.merge[,c(1:7)] = M06.quercus.trw[yrs.quercus >= 1972, c(1:7)]
M06.quercus.noisy.avg.merge[,c(8:14)] = M06.rs.avg

round(cor(M06.quercus.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Quercus)###
# M06.quercus.trw.spline vs. M06.rs.pt5.spline
M06.quercus.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.pt5)),
                                  row.names = rownames(M06.rs.pt5))
colnames(M06.quercus.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M06.quercus.spline.pt5.merge[,c(1:7)] = M06.quercus.trw.spline[yrs.quercus >= 1972, c(1:7)]
M06.quercus.spline.pt5.merge[,c(8:14)] = M06.rs.pt5.spline

round(cor(M06.quercus.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M06.quercus.trw.spline vs. M06.rs.avg.spline
M06.quercus.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M06.rs.avg)),
                                        row.names = rownames(M06.rs.avg))
colnames(M06.quercus.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
M06.quercus.spline.avg.merge[,c(1:7)] = M06.quercus.trw.spline[yrs.quercus >= 1972, c(1:7)]
M06.quercus.spline.avg.merge[,c(8:14)] = M06.rs.avg.spline

round(cor(M06.quercus.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M06.rs.avg), M06.acer.trw.spline$Spline[yrs.acer >= 1972])
lines(row.names(M06.rs.avg), M06.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1898 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M06.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M06.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M06.bestcor$TRW.smoothed = M06.acer.trw.spline$Spline[yrs.acer >= strt.yr]
M06.bestcor$RS.smoothed[rs.strt:length.trw] = M06.rs.avg.spline$Ar
M06.bestcor$TRW.diff[2:length.trw] = diff(M06.bestcor$TRW.smoothed)
M06.bestcor$RS.diff[2:length.trw] = diff(M06.bestcor$RS.smoothed)

for (i in 2:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.diff[i] > 0) {
    M06.bestcor$TRW.class[i] = 1
  } else {
    M06.bestcor$TRW.class[i] = 0
  }
}

for (i in 79:nrow(M06.bestcor)){
  if (M06.bestcor$RS.diff[i] > 0) {
    M06.bestcor$RS.class[i] = 1
  } else {
    M06.bestcor$RS.class[i] = 0
  }
}

sum(M06.bestcor$TRW.class, na.rm = TRUE)
sum(M06.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 79:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.class[i] == M06.bestcor$RS.class[i]){
    M06.bestcor$TRW.RS.agree[i] = 1
  } else {
    M06.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M06.bestcor$TRW.RS.agree, na.rm = TRUE) / 43, 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(M06.rs.avg), M06.acer.trw.spline$Spline[yrs.acer >= 1972])
lines(row.names(M06.rs.avg), M06.rs.avg.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1898 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M06.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M06.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M06.bestcor$TRW.smoothed = M06.acer.trw.spline$Spline[yrs.acer >= strt.yr]
M06.bestcor$RS.smoothed[rs.strt:length.trw] = M06.rs.avg.spline$NoDetrend
M06.bestcor$TRW.diff[2:length.trw] = diff(M06.bestcor$TRW.smoothed)
M06.bestcor$RS.diff[2:length.trw] = diff(M06.bestcor$RS.smoothed)

for (i in 2:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.diff[i] > 0) {
    M06.bestcor$TRW.class[i] = 1
  } else {
    M06.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M06.bestcor)){
  if (M06.bestcor$RS.diff[i] > 0) {
    M06.bestcor$RS.class[i] = 1
  } else {
    M06.bestcor$RS.class[i] = 0
  }
}

sum(M06.bestcor$TRW.class, na.rm = TRUE)
sum(M06.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.class[i] == M06.bestcor$RS.class[i]){
    M06.bestcor$TRW.RS.agree[i] = 1
  } else {
    M06.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M06.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(M06.rs.avg), M06.quercus.trw.spline$ModHugershoff[yrs.quercus >= 1972])
lines(row.names(M06.rs.avg), M06.rs.pt5.spline$ModHugershoff)
# Further comparisons and 1-year lags in Excel
strt.yr = 1901 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M06.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M06.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M06.bestcor$TRW.smoothed = M06.quercus.trw.spline$ModHugershoff[yrs.quercus >= strt.yr]
M06.bestcor$RS.smoothed[rs.strt:length.trw] = M06.rs.pt5.spline$ModHugershoff
M06.bestcor$TRW.diff[2:length.trw] = diff(M06.bestcor$TRW.smoothed)
M06.bestcor$RS.diff[2:length.trw] = diff(M06.bestcor$RS.smoothed)

for (i in 2:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.diff[i] > 0) {
    M06.bestcor$TRW.class[i] = 1
  } else {
    M06.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M06.bestcor)){
  if (M06.bestcor$RS.diff[i] > 0) {
    M06.bestcor$RS.class[i] = 1
  } else {
    M06.bestcor$RS.class[i] = 0
  }
}

sum(M06.bestcor$TRW.class, na.rm = TRUE)
sum(M06.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M06.bestcor)){
  if (M06.bestcor$TRW.class[i] == M06.bestcor$RS.class[i]){
    M06.bestcor$TRW.RS.agree[i] = 1
  } else {
    M06.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M06.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

