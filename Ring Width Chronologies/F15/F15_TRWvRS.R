#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F15/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F15/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F15.trw = read.csv("F15_Chronologies.csv", row.names = 1)
F15.trw.spline = read.csv("F15_Chronologies_Spline.csv", row.names = 1)
F15.Acer.trw = read.csv("F15_Acer_Chronologies.csv", row.names = 1)
F15.Acer.trw.spline = read.csv("F15_Acer_Chronologies_Spline.csv", row.names = 1)
F15.Carya.trw = read.csv("F15_Carya_Chronologies.csv", row.names = 1)
F15.Carya.trw.spline = read.csv("F15_Carya_Chronologies_Spline.csv", row.names = 1)
F15.Quercus.trw = read.csv("F15_Quercus_Chronologies.csv", row.names = 1)
F15.Quercus.trw.spline = read.csv("F15_Quercus_Chronologies_Spline.csv", row.names = 1)

#RS#
F15.rs.pt5 = read.csv("F15_LandsatPt5_Chronologies.csv", row.names = 1)
F15.rs.pt5.spline = read.csv("F15_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F15.rs.avg = read.csv("F15_LandsatAvg_Chronologies.csv", row.names = 1)
F15.rs.avg.spline = read.csv("F15_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F15.trw)
yrs.acer = row.names(F15.Acer.trw)
yrs.carya = row.names(F15.Carya.trw)
yrs.quercus = row.names(F15.Quercus.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F15.trw vs. F15.rs.pt5
F15.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                 row.names = rownames(F15.rs.pt5))
colnames(F15.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F15.noisy.pt5.merge[,c(1:7)] = F15.trw[yrs >= 1972, c(1:7)]
F15.noisy.pt5.merge[,c(8:14)] = F15.rs.pt5

round(cor(F15.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.trw vs. F15.rs.avg
F15.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                 row.names = rownames(F15.rs.avg))
colnames(F15.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F15.noisy.avg.merge[,c(1:7)] = F15.trw[yrs >= 1972, c(1:7)]
F15.noisy.avg.merge[,c(8:14)] = F15.rs.avg

round(cor(F15.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F15.trw.spline vs. F15.rs.pt5.spline
F15.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                 row.names = rownames(F15.rs.pt5))
colnames(F15.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F15.spline.pt5.merge[,c(1:7)] = F15.trw.spline[yrs >= 1972, c(1:7)]
F15.spline.pt5.merge[,c(8:14)] = F15.rs.pt5.spline

round(cor(F15.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.trw.spline vs. F15.rs.avg.spline
F15.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                 row.names = rownames(F15.rs.avg))
colnames(F15.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F15.spline.avg.merge[,c(1:7)] = F15.trw.spline[yrs >= 1972, c(1:7)]
F15.spline.avg.merge[,c(8:14)] = F15.rs.avg.spline

round(cor(F15.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (Acer)###
# F15.Acer.trw vs. F15.rs.pt5
F15.Acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                 row.names = rownames(F15.rs.pt5))
colnames(F15.Acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F15.Acer.noisy.pt5.merge[,c(1:7)] = F15.Acer.trw[yrs.acer >= 1972, c(1:7)]
F15.Acer.noisy.pt5.merge[,c(8:14)] = F15.rs.pt5

round(cor(F15.Acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Acer.trw vs. F15.rs.avg
F15.Acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                 row.names = rownames(F15.rs.avg))
colnames(F15.Acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F15.Acer.noisy.avg.merge[,c(1:7)] = F15.Acer.trw[yrs.acer >= 1972, c(1:7)]
F15.Acer.noisy.avg.merge[,c(8:14)] = F15.rs.avg

round(cor(F15.Acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Acer)###
# F15.Acer.trw.spline vs. F15.rs.pt5.spline
F15.Acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                  row.names = rownames(F15.rs.pt5))
colnames(F15.Acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F15.Acer.spline.pt5.merge[,c(1:7)] = F15.Acer.trw.spline[yrs.acer >= 1972, c(1:7)]
F15.Acer.spline.pt5.merge[,c(8:14)] = F15.rs.pt5.spline

round(cor(F15.Acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Acer.trw.spline vs. F15.rs.avg.spline
F15.Acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                        row.names = rownames(F15.rs.avg))
colnames(F15.Acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F15.Acer.spline.avg.merge[,c(1:7)] = F15.Acer.trw.spline[yrs.acer >= 1972, c(1:7)]
F15.Acer.spline.avg.merge[,c(8:14)] = F15.rs.avg.spline

round(cor(F15.Acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

###Noisy TRW vs. Noisy RS (Carya)###
# F15.Carya.trw vs. F15.rs.pt5
F15.Carya.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                      row.names = rownames(F15.rs.pt5))
colnames(F15.Carya.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                       "Mean.trw", "Ar.trw", "Friedman.trw",
                                       "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                       "Mean.rs", "Ar.rs", "Friedman.rs", 
                                       "ModHuger.rs", "NoDetrend.rs")
F15.Carya.noisy.pt5.merge[,c(1:7)] = F15.Carya.trw[yrs.carya >= 1972, c(1:7)]
F15.Carya.noisy.pt5.merge[,c(8:14)] = F15.rs.pt5

round(cor(F15.Carya.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Carya.trw vs. F15.rs.avg
F15.Carya.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                      row.names = rownames(F15.rs.avg))
colnames(F15.Carya.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                       "Mean.trw", "Ar.trw", "Friedman.trw",
                                       "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                       "Mean.rs", "Ar.rs", "Friedman.rs", 
                                       "ModHuger.rs", "NoDetrend.rs")
F15.Carya.noisy.avg.merge[,c(1:7)] = F15.Carya.trw[yrs.carya >= 1972, c(1:7)]
F15.Carya.noisy.avg.merge[,c(8:14)] = F15.rs.avg

round(cor(F15.Carya.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Carya)###
# F15.Carya.trw.spline vs. F15.rs.pt5.spline
F15.Carya.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                       row.names = rownames(F15.rs.pt5))
colnames(F15.Carya.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
F15.Carya.spline.pt5.merge[,c(1:7)] = F15.Carya.trw.spline[yrs.carya >= 1972, c(1:7)]
F15.Carya.spline.pt5.merge[,c(8:14)] = F15.rs.pt5.spline

round(cor(F15.Carya.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Carya.trw.spline vs. F15.rs.avg.spline
F15.Carya.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                       row.names = rownames(F15.rs.avg))
colnames(F15.Carya.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
F15.Carya.spline.avg.merge[,c(1:7)] = F15.Carya.trw.spline[yrs.carya >= 1972, c(1:7)]
F15.Carya.spline.avg.merge[,c(8:14)] = F15.rs.avg.spline

round(cor(F15.Carya.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####


###Noisy TRW vs. Noisy RS (Quercus)###
# F15.Quercus.trw vs. F15.rs.pt5
F15.Quercus.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                       row.names = rownames(F15.rs.pt5))
colnames(F15.Quercus.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
F15.Quercus.noisy.pt5.merge[,c(1:7)] = F15.Quercus.trw[yrs.quercus >= 1972, c(1:7)]
F15.Quercus.noisy.pt5.merge[,c(8:14)] = F15.rs.pt5

round(cor(F15.Quercus.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Quercus.trw vs. F15.rs.avg
F15.Quercus.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                       row.names = rownames(F15.rs.avg))
colnames(F15.Quercus.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
F15.Quercus.noisy.avg.merge[,c(1:7)] = F15.Quercus.trw[yrs.quercus >= 1972, c(1:7)]
F15.Quercus.noisy.avg.merge[,c(8:14)] = F15.rs.avg

round(cor(F15.Quercus.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Quercus)###
# F15.Quercus.trw.spline vs. F15.rs.pt5.spline
F15.Quercus.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.pt5)),
                                        row.names = rownames(F15.rs.pt5))
colnames(F15.Quercus.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F15.Quercus.spline.pt5.merge[,c(1:7)] = F15.Quercus.trw.spline[yrs.quercus >= 1972, c(1:7)]
F15.Quercus.spline.pt5.merge[,c(8:14)] = F15.rs.pt5.spline

round(cor(F15.Quercus.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F15.Quercus.trw.spline vs. F15.rs.avg.spline
F15.Quercus.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F15.rs.avg)),
                                        row.names = rownames(F15.rs.avg))
colnames(F15.Quercus.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F15.Quercus.spline.avg.merge[,c(1:7)] = F15.Quercus.trw.spline[yrs.quercus >= 1972, c(1:7)]
F15.Quercus.spline.avg.merge[,c(8:14)] = F15.rs.avg.spline

round(cor(F15.Quercus.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F15.rs.avg), F15.Quercus.trw.spline$NoDetrend[yrs.quercus >= 1972])
lines(row.names(F15.rs.avg), F15.rs.avg.spline$NoDetrend)
# Further comparisons and 1-year lags in Excel
strt.yr = 1909 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F15.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F15.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F15.bestcor$TRW.smoothed = F15.Quercus.trw.spline$NoDetrend[yrs.quercus >= strt.yr]
F15.bestcor$RS.smoothed[rs.strt:length.trw] = F15.rs.avg.spline$NoDetrend
F15.bestcor$TRW.diff[2:length.trw] = diff(F15.bestcor$TRW.smoothed)
F15.bestcor$RS.diff[2:length.trw] = diff(F15.bestcor$RS.smoothed)

for (i in 2:nrow(F15.bestcor)){
  if (F15.bestcor$TRW.diff[i] > 0) {
    F15.bestcor$TRW.class[i] = 1
  } else {
    F15.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F15.bestcor)){
  if (F15.bestcor$RS.diff[i] > 0) {
    F15.bestcor$RS.class[i] = 1
  } else {
    F15.bestcor$RS.class[i] = 0
  }
}

sum(F15.bestcor$TRW.class, na.rm = TRUE)
sum(F15.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F15.bestcor)){
  if (F15.bestcor$TRW.class[i] == F15.bestcor$RS.class[i]){
    F15.bestcor$TRW.RS.agree[i] = 1
  } else {
    F15.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F15.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation (Matching) ###
plot(row.names(F15.rs.avg), F15.Quercus.trw.spline$Mean[yrs.quercus >= 1972])
lines(row.names(F15.rs.avg), F15.rs.avg.spline$Mean)
# Further comparisons and 1-year lags in Excel
strt.yr = 1909 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F15.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F15.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F15.bestcor$TRW.smoothed = F15.Quercus.trw.spline$Mean[yrs.quercus >= strt.yr]
F15.bestcor$RS.smoothed[rs.strt:length.trw] = F15.rs.avg.spline$Mean
F15.bestcor$TRW.diff[2:length.trw] = diff(F15.bestcor$TRW.smoothed)
F15.bestcor$RS.diff[2:length.trw] = diff(F15.bestcor$RS.smoothed)

for (i in 2:nrow(F15.bestcor)){
  if (F15.bestcor$TRW.diff[i] > 0) {
    F15.bestcor$TRW.class[i] = 1
  } else {
    F15.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F15.bestcor)){
  if (F15.bestcor$RS.diff[i] > 0) {
    F15.bestcor$RS.class[i] = 1
  } else {
    F15.bestcor$RS.class[i] = 0
  }
}

sum(F15.bestcor$TRW.class, na.rm = TRUE)
sum(F15.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F15.bestcor)){
  if (F15.bestcor$TRW.class[i] == F15.bestcor$RS.class[i]){
    F15.bestcor$TRW.RS.agree[i] = 1
  } else {
    F15.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F15.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

