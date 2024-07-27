#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F07.trw = read.csv("F07_Chronologies.csv", row.names = 1)
F07.trw.spline = read.csv("F07_Chronologies_Spline.csv", row.names = 1)
F07.Acer.trw = read.csv("F07_Acer_Chronologies.csv", row.names = 1)
F07.Acer.trw.spline = read.csv("F07_Acer_Chronologies_Spline.csv", row.names = 1)

#RS#
F07.rs.pt5 = read.csv("F07_LandsatPt5_Chronologies.csv", row.names = 1)
F07.rs.pt5.spline = read.csv("F07_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F07.rs.avg = read.csv("F07_LandsatAvg_Chronologies.csv", row.names = 1)
F07.rs.avg.spline = read.csv("F07_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F07.trw)
yrs.acer = row.names(F07.Acer.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F07.trw vs. F07.rs.pt5
F07.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.pt5)),
                                 row.names = rownames(F07.rs.pt5))
colnames(F07.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F07.noisy.pt5.merge[,c(1:7)] = F07.trw[yrs >= 1972, c(1:7)]
F07.noisy.pt5.merge[,c(8:14)] = F07.rs.pt5

round(cor(F07.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F07.trw vs. F07.rs.avg
F07.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.avg)),
                                 row.names = rownames(F07.rs.avg))
colnames(F07.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F07.noisy.avg.merge[,c(1:7)] = F07.trw[yrs >= 1972, c(1:7)]
F07.noisy.avg.merge[,c(8:14)] = F07.rs.avg

round(cor(F07.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F07.trw.spline vs. F07.rs.pt5.spline
F07.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.pt5)),
                                 row.names = rownames(F07.rs.pt5))
colnames(F07.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F07.spline.pt5.merge[,c(1:7)] = F07.trw.spline[yrs >= 1972, c(1:7)]
F07.spline.pt5.merge[,c(8:14)] = F07.rs.pt5.spline

round(cor(F07.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F07.trw.spline vs. F07.rs.avg.spline
F07.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.avg)),
                                 row.names = rownames(F07.rs.avg))
colnames(F07.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F07.spline.avg.merge[,c(1:7)] = F07.trw.spline[yrs >= 1972, c(1:7)]
F07.spline.avg.merge[,c(8:14)] = F07.rs.avg.spline

round(cor(F07.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (Acer)###
# F07.Acer.trw vs. F07.rs.pt5
F07.Acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.pt5)),
                                 row.names = rownames(F07.rs.pt5))
colnames(F07.Acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F07.Acer.noisy.pt5.merge[,c(1:7)] = F07.Acer.trw[yrs >= 1972, c(1:7)]
F07.Acer.noisy.pt5.merge[,c(8:14)] = F07.rs.pt5

round(cor(F07.Acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F07.Acer.trw vs. F07.rs.avg
F07.Acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.avg)),
                                 row.names = rownames(F07.rs.avg))
colnames(F07.Acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F07.Acer.noisy.avg.merge[,c(1:7)] = F07.Acer.trw[yrs >= 1972, c(1:7)]
F07.Acer.noisy.avg.merge[,c(8:14)] = F07.rs.avg

round(cor(F07.Acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Acer)###
# F07.Acer.trw.spline vs. F07.rs.pt5.spline
F07.Acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.pt5)),
                                  row.names = rownames(F07.rs.pt5))
colnames(F07.Acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F07.Acer.spline.pt5.merge[,c(1:7)] = F07.Acer.trw.spline[yrs >= 1972, c(1:7)]
F07.Acer.spline.pt5.merge[,c(8:14)] = F07.rs.pt5.spline

round(cor(F07.Acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F07.Acer.trw.spline vs. F07.rs.avg.spline
F07.Acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F07.rs.avg)),
                                        row.names = rownames(F07.rs.avg))
colnames(F07.Acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F07.Acer.spline.avg.merge[,c(1:7)] = F07.Acer.trw.spline[yrs >= 1972, c(1:7)]
F07.Acer.spline.avg.merge[,c(8:14)] = F07.rs.avg.spline

round(cor(F07.Acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F07.rs.avg), F07.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(F07.rs.avg), F07.rs.pt5.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1942 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F07.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F07.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F07.bestcor$TRW.smoothed = F07.trw.spline$ModHugershoff[yrs >= strt.yr]
F07.bestcor$RS.smoothed[rs.strt:length.trw] = F07.rs.pt5.spline$Ar
F07.bestcor$TRW.diff[2:length.trw] = diff(F07.bestcor$TRW.smoothed)
F07.bestcor$RS.diff[2:length.trw] = diff(F07.bestcor$RS.smoothed)

for (i in 2:nrow(F07.bestcor)){
  if (F07.bestcor$TRW.diff[i] > 0) {
    F07.bestcor$TRW.class[i] = 1
  } else {
    F07.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt + 1:nrow(F07.bestcor)){
  if (F07.bestcor$RS.diff[i] > 0) {
    F07.bestcor$RS.class[i] = 1
  } else {
    F07.bestcor$RS.class[i] = 0
  }
}

sum(F07.bestcor$TRW.class, na.rm = TRUE)
sum(F07.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt + 1:nrow(F07.bestcor)){
  if (F07.bestcor$TRW.class[i] == F07.bestcor$RS.class[i]){
    F07.bestcor$TRW.RS.agree[i] = 1
  } else {
    F07.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F07.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 2), 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(F07.rs.avg), F07.trw.spline$ModHugershoff[yrs >= 1972])
lines(row.names(F07.rs.avg), F07.rs.pt5.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1942 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F07.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F07.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F07.bestcor$TRW.smoothed = F07.trw.spline$ModHugershoff[yrs >= strt.yr]
F07.bestcor$RS.smoothed[rs.strt:length.trw] = F07.rs.pt5.spline$NoDetrend
F07.bestcor$TRW.diff[2:length.trw] = diff(F07.bestcor$TRW.smoothed)
F07.bestcor$RS.diff[2:length.trw] = diff(F07.bestcor$RS.smoothed)

for (i in 2:nrow(F07.bestcor)){
  if (F07.bestcor$TRW.diff[i] > 0) {
    F07.bestcor$TRW.class[i] = 1
  } else {
    F07.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F07.bestcor)){
  if (F07.bestcor$RS.diff[i] > 0) {
    F07.bestcor$RS.class[i] = 1
  } else {
    F07.bestcor$RS.class[i] = 0
  }
}

sum(F07.bestcor$TRW.class, na.rm = TRUE)
sum(F07.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F07.bestcor)){
  if (F07.bestcor$TRW.class[i] == F07.bestcor$RS.class[i]){
    F07.bestcor$TRW.RS.agree[i] = 1
  } else {
    F07.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F07.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

