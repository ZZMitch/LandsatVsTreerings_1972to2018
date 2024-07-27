#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
F21.trw = read.csv("F21_Chronologies.csv", row.names = 1)
F21.trw.spline = read.csv("F21_Chronologies_Spline.csv", row.names = 1)
F21.Acer.trw = read.csv("F21_Acer_Chronologies.csv", row.names = 1)
F21.Acer.trw.spline = read.csv("F21_Acer_Chronologies_Spline.csv", row.names = 1)

#RS#
F21.rs.pt5 = read.csv("F21_LandsatPt5_Chronologies.csv", row.names = 1)
F21.rs.pt5.spline = read.csv("F21_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
F21.rs.avg = read.csv("F21_LandsatAvg_Chronologies.csv", row.names = 1)
F21.rs.avg.spline = read.csv("F21_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs = row.names(F21.trw)
yrs.acer = row.names(F21.Acer.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS###
# F21.trw vs. F21.rs.pt5
F21.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.pt5)),
                                 row.names = rownames(F21.rs.pt5))
colnames(F21.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F21.noisy.pt5.merge[,c(1:7)] = F21.trw[yrs >= 1972, c(1:7)]
F21.noisy.pt5.merge[,c(8:14)] = F21.rs.pt5

round(cor(F21.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F21.trw vs. F21.rs.avg
F21.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.avg)),
                                 row.names = rownames(F21.rs.avg))
colnames(F21.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F21.noisy.avg.merge[,c(1:7)] = F21.trw[yrs >= 1972, c(1:7)]
F21.noisy.avg.merge[,c(8:14)] = F21.rs.avg

round(cor(F21.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# F21.trw.spline vs. F21.rs.pt5.spline
F21.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.pt5)),
                                 row.names = rownames(F21.rs.pt5))
colnames(F21.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F21.spline.pt5.merge[,c(1:7)] = F21.trw.spline[yrs >= 1972, c(1:7)]
F21.spline.pt5.merge[,c(8:14)] = F21.rs.pt5.spline

round(cor(F21.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F21.trw.spline vs. F21.rs.avg.spline
F21.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.avg)),
                                 row.names = rownames(F21.rs.avg))
colnames(F21.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F21.spline.avg.merge[,c(1:7)] = F21.trw.spline[yrs >= 1972, c(1:7)]
F21.spline.avg.merge[,c(8:14)] = F21.rs.avg.spline

round(cor(F21.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (Acer)###
# F21.Acer.trw vs. F21.rs.pt5
F21.Acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.pt5)),
                                 row.names = rownames(F21.rs.pt5))
colnames(F21.Acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F21.Acer.noisy.pt5.merge[,c(1:7)] = F21.Acer.trw[yrs >= 1972, c(1:7)]
F21.Acer.noisy.pt5.merge[,c(8:14)] = F21.rs.pt5

round(cor(F21.Acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# F21.Acer.trw vs. F21.rs.avg
F21.Acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.avg)),
                                 row.names = rownames(F21.rs.avg))
colnames(F21.Acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
F21.Acer.noisy.avg.merge[,c(1:7)] = F21.Acer.trw[yrs >= 1972, c(1:7)]
F21.Acer.noisy.avg.merge[,c(8:14)] = F21.rs.avg

round(cor(F21.Acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (Acer)###
# F21.Acer.trw.spline vs. F21.rs.pt5.spline
F21.Acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.pt5)),
                                  row.names = rownames(F21.rs.pt5))
colnames(F21.Acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
F21.Acer.spline.pt5.merge[,c(1:7)] = F21.Acer.trw.spline[yrs >= 1972, c(1:7)]
F21.Acer.spline.pt5.merge[,c(8:14)] = F21.rs.pt5.spline

round(cor(F21.Acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# F21.Acer.trw.spline vs. F21.rs.avg.spline
F21.Acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(F21.rs.avg)),
                                        row.names = rownames(F21.rs.avg))
colnames(F21.Acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
F21.Acer.spline.avg.merge[,c(1:7)] = F21.Acer.trw.spline[yrs >= 1972, c(1:7)]
F21.Acer.spline.avg.merge[,c(8:14)] = F21.rs.avg.spline

round(cor(F21.Acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(F21.rs.avg), F21.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(F21.rs.avg), F21.rs.avg.spline$Ar)
# Further comparisons and 1-year lags in Excel
strt.yr = 1916 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F21.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F21.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F21.bestcor$TRW.smoothed = F21.trw.spline$ModNegExp[yrs >= strt.yr]
F21.bestcor$RS.smoothed[rs.strt:length.trw] = F21.rs.avg.spline$Ar
F21.bestcor$TRW.diff[2:length.trw] = diff(F21.bestcor$TRW.smoothed)
F21.bestcor$RS.diff[2:length.trw] = diff(F21.bestcor$RS.smoothed)

for (i in 2:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.diff[i] > 0) {
    F21.bestcor$TRW.class[i] = 1
  } else {
    F21.bestcor$TRW.class[i] = 0
  }
}

for (i in 60:nrow(F21.bestcor)){
  if (F21.bestcor$RS.diff[i] > 0) {
    F21.bestcor$RS.class[i] = 1
  } else {
    F21.bestcor$RS.class[i] = 0
  }
}

sum(F21.bestcor$TRW.class, na.rm = TRUE)
sum(F21.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in 60:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.class[i] == F21.bestcor$RS.class[i]){
    F21.bestcor$TRW.RS.agree[i] = 1
  } else {
    F21.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F21.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 3), 2)

### Best correlation vs. NoDetrend.rs ###
plot(row.names(F21.rs.avg), F21.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(F21.rs.avg), F21.rs.avg.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1916 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F21.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F21.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F21.bestcor$TRW.smoothed = F21.trw.spline$ModNegExp[yrs >= strt.yr]
F21.bestcor$RS.smoothed[rs.strt:length.trw] = F21.rs.avg.spline$NoDetrend
F21.bestcor$TRW.diff[2:length.trw] = diff(F21.bestcor$TRW.smoothed)
F21.bestcor$RS.diff[2:length.trw] = diff(F21.bestcor$RS.smoothed)

for (i in 2:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.diff[i] > 0) {
    F21.bestcor$TRW.class[i] = 1
  } else {
    F21.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F21.bestcor)){
  if (F21.bestcor$RS.diff[i] > 0) {
    F21.bestcor$RS.class[i] = 1
  } else {
    F21.bestcor$RS.class[i] = 0
  }
}

sum(F21.bestcor$TRW.class, na.rm = TRUE)
sum(F21.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.class[i] == F21.bestcor$RS.class[i]){
    F21.bestcor$TRW.RS.agree[i] = 1
  } else {
    F21.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F21.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

### Best correlation - Matching Methods ###
plot(row.names(F21.rs.avg), F21.trw.spline$ModNegExp[yrs >= 1972])
lines(row.names(F21.rs.avg), F21.rs.pt5.spline$ModNegExp)
# Further comparisons and 1-year lags in Excel
strt.yr = 1916 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
F21.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(F21.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

F21.bestcor$TRW.smoothed = F21.trw.spline$ModNegExp[yrs >= strt.yr]
F21.bestcor$RS.smoothed[rs.strt:length.trw] = F21.rs.avg.spline$ModNegExp
F21.bestcor$TRW.diff[2:length.trw] = diff(F21.bestcor$TRW.smoothed)
F21.bestcor$RS.diff[2:length.trw] = diff(F21.bestcor$RS.smoothed)

for (i in 2:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.diff[i] > 0) {
    F21.bestcor$TRW.class[i] = 1
  } else {
    F21.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(F21.bestcor)){
  if (F21.bestcor$RS.diff[i] > 0) {
    F21.bestcor$RS.class[i] = 1
  } else {
    F21.bestcor$RS.class[i] = 0
  }
}

sum(F21.bestcor$TRW.class, na.rm = TRUE)
sum(F21.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(F21.bestcor)){
  if (F21.bestcor$TRW.class[i] == F21.bestcor$RS.class[i]){
    F21.bestcor$TRW.RS.agree[i] = 1
  } else {
    F21.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(F21.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)

