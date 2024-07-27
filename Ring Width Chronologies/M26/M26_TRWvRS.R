#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26/DetrendingLandsat") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26/DetrendingLandsat") 
#Laptop

#####Import#####
#####
#TRW#
M26.acer.trw = read.csv("M26_Acer_Chronologies.csv", row.names = 1)
M26.acer.trw.spline = read.csv("M26_Acer_Chronologies_Spline.csv", row.names = 1)
M26.deciduous.trw = read.csv("M26_Deciduous_Chronologies.csv", row.names = 1)
M26.deciduous.trw.spline = read.csv("M26_Deciduous_Chronologies_Spline.csv", row.names = 1)
M26.thuja.trw = read.csv("M26_Thuja_Chronologies.csv", row.names = 1)
M26.thuja.trw.spline = read.csv("M26_Thuja_Chronologies_Spline.csv", row.names = 1)

#RS#
M26.rs.pt5 = read.csv("M26_LandsatPt5_Chronologies.csv", row.names = 1)
M26.rs.pt5.spline = read.csv("M26_LandsatPt5_Chronologies_Spline.csv", row.names = 1)
M26.rs.avg = read.csv("M26_LandsatAvg_Chronologies.csv", row.names = 1)
M26.rs.avg.spline = read.csv("M26_LandsatAvg_Chronologies_Spline.csv", row.names = 1)
#####

yrs.acer = row.names(M26.acer.trw)
yrs.deciduous = row.names(M26.deciduous.trw)
yrs.thuja = row.names(M26.thuja.trw)

#####Correlations#####
#####
###Noisy TRW vs. Noisy RS (Acer)###
# M26.acer.trw vs. M26.rs.pt5
M26.acer.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                 row.names = rownames(M26.rs.pt5))
colnames(M26.acer.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M26.acer.noisy.pt5.merge[,c(1:7)] = M26.acer.trw[yrs.acer >= 1972, c(1:7)]
M26.acer.noisy.pt5.merge[,c(8:14)] = M26.rs.pt5

round(cor(M26.acer.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.trw vs. M26.rs.avg
M26.acer.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                 row.names = rownames(M26.rs.avg))
colnames(M26.acer.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M26.acer.noisy.avg.merge[,c(1:7)] = M26.acer.trw[yrs.acer >= 1972, c(1:7)]
M26.acer.noisy.avg.merge[,c(8:14)] = M26.rs.avg

round(cor(M26.acer.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M26.trw.spline vs. M26.rs.pt5.spline
M26.acer.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                 row.names = rownames(M26.rs.pt5))
colnames(M26.acer.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M26.acer.spline.pt5.merge[,c(1:7)] = M26.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
M26.acer.spline.pt5.merge[,c(8:14)] = M26.rs.pt5.spline

round(cor(M26.acer.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.trw.spline vs. M26.rs.avg.spline
M26.acer.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                 row.names = rownames(M26.rs.avg))
colnames(M26.acer.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M26.acer.spline.avg.merge[,c(1:7)] = M26.acer.trw.spline[yrs.acer >= 1972, c(1:7)]
M26.acer.spline.avg.merge[,c(8:14)] = M26.rs.avg.spline

round(cor(M26.acer.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (deciduous)###
# M26.deciduous.trw vs. M26.rs.pt5
M26.deciduous.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                 row.names = rownames(M26.rs.pt5))
colnames(M26.deciduous.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M26.deciduous.noisy.pt5.merge[,c(1:7)] = M26.deciduous.trw[yrs.deciduous >= 1972, c(1:7)]
M26.deciduous.noisy.pt5.merge[,c(8:14)] = M26.rs.pt5

round(cor(M26.deciduous.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.deciduous.trw vs. M26.rs.avg
M26.deciduous.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                 row.names = rownames(M26.rs.avg))
colnames(M26.deciduous.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                  "Mean.trw", "Ar.trw", "Friedman.trw",
                                  "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                  "Mean.rs", "Ar.rs", "Friedman.rs", 
                                  "ModHuger.rs", "NoDetrend.rs")
M26.deciduous.noisy.avg.merge[,c(1:7)] = M26.deciduous.trw[yrs.deciduous >= 1972, c(1:7)]
M26.deciduous.noisy.avg.merge[,c(8:14)] = M26.rs.avg

round(cor(M26.deciduous.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS (deciduous)###
# M26.deciduous.trw.spline vs. M26.rs.pt5.spline
M26.deciduous.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                  row.names = rownames(M26.rs.pt5))
colnames(M26.deciduous.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                   "Mean.trw", "Ar.trw", "Friedman.trw",
                                   "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                   "Mean.rs", "Ar.rs", "Friedman.rs", 
                                   "ModHuger.rs", "NoDetrend.rs")
M26.deciduous.spline.pt5.merge[,c(1:7)] = M26.deciduous.trw.spline[yrs.deciduous >= 1972, c(1:7)]
M26.deciduous.spline.pt5.merge[,c(8:14)] = M26.rs.pt5.spline

round(cor(M26.deciduous.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.deciduous.trw.spline vs. M26.rs.avg.spline
M26.deciduous.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                        row.names = rownames(M26.rs.avg))
colnames(M26.deciduous.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                         "Mean.trw", "Ar.trw", "Friedman.trw",
                                         "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                         "Mean.rs", "Ar.rs", "Friedman.rs", 
                                         "ModHuger.rs", "NoDetrend.rs")
M26.deciduous.spline.avg.merge[,c(1:7)] = M26.deciduous.trw.spline[yrs.deciduous >= 1972, c(1:7)]
M26.deciduous.spline.avg.merge[,c(8:14)] = M26.rs.avg.spline

round(cor(M26.deciduous.spline.avg.merge, use = "pairwise.complete.obs"), 2)

###Noisy TRW vs. Noisy RS (thuja)###
# M26.thuja.trw vs. M26.rs.pt5
M26.thuja.noisy.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                      row.names = rownames(M26.rs.pt5))
colnames(M26.thuja.noisy.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                       "Mean.trw", "Ar.trw", "Friedman.trw",
                                       "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                       "Mean.rs", "Ar.rs", "Friedman.rs", 
                                       "ModHuger.rs", "NoDetrend.rs")
M26.thuja.noisy.pt5.merge[,c(1:7)] = M26.thuja.trw[yrs.thuja >= 1972, c(1:7)]
M26.thuja.noisy.pt5.merge[,c(8:14)] = M26.rs.pt5

round(cor(M26.thuja.noisy.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.trw vs. M26.rs.avg
M26.thuja.noisy.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                      row.names = rownames(M26.rs.avg))
colnames(M26.thuja.noisy.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                       "Mean.trw", "Ar.trw", "Friedman.trw",
                                       "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                       "Mean.rs", "Ar.rs", "Friedman.rs", 
                                       "ModHuger.rs", "NoDetrend.rs")
M26.thuja.noisy.avg.merge[,c(1:7)] = M26.thuja.trw[yrs.thuja >= 1972, c(1:7)]
M26.thuja.noisy.avg.merge[,c(8:14)] = M26.rs.avg

round(cor(M26.thuja.noisy.avg.merge, use = "pairwise.complete.obs"), 2)

###Spline TRW vs. Spline RS###
# M26.trw.spline vs. M26.rs.pt5.spline
M26.thuja.spline.pt5.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.pt5)),
                                       row.names = rownames(M26.rs.pt5))
colnames(M26.thuja.spline.pt5.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
M26.thuja.spline.pt5.merge[,c(1:7)] = M26.thuja.trw.spline[yrs.thuja >= 1972, c(1:7)]
M26.thuja.spline.pt5.merge[,c(8:14)] = M26.rs.pt5.spline

round(cor(M26.thuja.spline.pt5.merge, use = "pairwise.complete.obs"), 2)

# M26.trw.spline vs. M26.rs.avg.spline
M26.thuja.spline.avg.merge = data.frame(matrix(ncol = 14, nrow = nrow(M26.rs.avg)),
                                       row.names = rownames(M26.rs.avg))
colnames(M26.thuja.spline.avg.merge) = c("NoDetrend.trw", "Spline.trw", "ModNegExp.trw",
                                        "Mean.trw", "Ar.trw", "Friedman.trw",
                                        "ModHuger.trw", "Spline.rs", "ModNegExp.rs", 
                                        "Mean.rs", "Ar.rs", "Friedman.rs", 
                                        "ModHuger.rs", "NoDetrend.rs")
M26.thuja.spline.avg.merge[,c(1:7)] = M26.thuja.trw.spline[yrs.thuja >= 1972, c(1:7)]
M26.thuja.spline.avg.merge[,c(8:14)] = M26.rs.avg.spline

round(cor(M26.thuja.spline.avg.merge, use = "pairwise.complete.obs"), 2)
#####

### Best correlation ###
plot(row.names(M26.rs.avg), M26.thuja.trw.spline$ModHugershoff[yrs.thuja >= 1972])
lines(row.names(M26.rs.avg), M26.rs.avg.spline$NoDetrend) #will not show up
# Further comparisons and 1-year lags in Excel
strt.yr = 1904 #Update for each site
end.yr = 2018 #Update for each site
length.trw = end.yr - strt.yr + 1 
length.rs = 47
rs.strt = length.trw - length.rs + 1
rs.diff.strt = length.trw - length.rs + 2

# Classify into increasing and decreasing...
M26.bestcor = data.frame(matrix(ncol = 7, nrow = length.trw),
                         row.names = c(strt.yr:end.yr))
colnames(M26.bestcor) = c("TRW.smoothed", "RS.smoothed", "TRW.diff", "RS.diff",
                          "TRW.class", "RS.class", "TRW.RS.agree")

M26.bestcor$TRW.smoothed = M26.thuja.trw.spline$ModHugershoff[yrs.thuja >= strt.yr]
M26.bestcor$RS.smoothed[rs.strt:length.trw] = M26.rs.avg.spline$NoDetrend
M26.bestcor$TRW.diff[2:length.trw] = diff(M26.bestcor$TRW.smoothed)
M26.bestcor$RS.diff[2:length.trw] = diff(M26.bestcor$RS.smoothed)

for (i in 2:nrow(M26.bestcor)){
  if (M26.bestcor$TRW.diff[i] > 0) {
    M26.bestcor$TRW.class[i] = 1
  } else {
    M26.bestcor$TRW.class[i] = 0
  }
}

for (i in rs.diff.strt:nrow(M26.bestcor)){
  if (M26.bestcor$RS.diff[i] > 0) {
    M26.bestcor$RS.class[i] = 1
  } else {
    M26.bestcor$RS.class[i] = 0
  }
}

sum(M26.bestcor$TRW.class, na.rm = TRUE)
sum(M26.bestcor$RS.class, na.rm = TRUE)

# Agreement
for (i in rs.diff.strt:nrow(M26.bestcor)){
  if (M26.bestcor$TRW.class[i] == M26.bestcor$RS.class[i]){
    M26.bestcor$TRW.RS.agree[i] = 1
  } else {
    M26.bestcor$TRW.RS.agree[i] = 0
  }
}

round(sum(M26.bestcor$TRW.RS.agree, na.rm = TRUE) / (length.rs - 1), 2)