###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13") 
#Laptop

###RWLs###
M13 = read.rwl('M13_Tsuga_2.rwl')
plot(M13, plot.type = "spag") #How to plot by age?
M13.ids = read.ids(M13, stc = c(4, 2, 1))
rwi.stats(M13, M13.ids, prewhiten = TRUE)
M13.crn = chron(M13, prefix = "M13") # Mean chronology
yr = time(M13)

M13.sss = sss(M13, M13.ids)
cutoff = max(yr[M13.sss < 0.85])

#Check for outliers
plot(M13.crn)

#####Noisy Values#####
#####
#No Detrending#
M13.nodetrend.crn = subset(M13.crn, yr > cutoff)
plot(M13.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(M13)

#Spline#
M13.spline = detrend(M13, method = "Spline")
rwi.stats(M13.spline, M13.ids, prewhiten = TRUE)

M13.spline.sss = sss(M13.spline, M13.ids)
cutoff.spline = max(yr[M13.spline.sss < 0.85])

M13.spline.crn = chron(detrend(M13[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M13")
plot(M13.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M13.modnegexp = detrend(M13, method = "ModNegExp")
rwi.stats(M13.modnegexp, M13.ids, prewhiten = TRUE)

M13.modnegexp.sss = sss(M13.modnegexp, M13.ids)
cutoff.modnegexp = max(yr[M13.modnegexp.sss < 0.85])

M13.modnegexp.crn = chron(detrend(M13[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M13")
plot(M13.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M13.mean = detrend(M13, method = "Mean")
rwi.stats(M13.mean, M13.ids, prewhiten = TRUE)

M13.mean.sss = sss(M13.mean, M13.ids)
cutoff.mean = max(yr[M13.mean.sss < 0.85])

M13.mean.crn = chron(detrend(M13[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M13")
plot(M13.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M13.ar = detrend(M13, method = "Ar")
rwi.stats(M13.ar, M13.ids, prewhiten = TRUE)

M13.ar.sss = sss(M13.ar, M13.ids)
cutoff.ar = max(yr[M13.ar.sss < 0.85])

M13.ar.crn = chron(detrend(M13[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M13")
M13.ar.crn = M13.ar.crn[-1,] # First row is NaN
plot(M13.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M13.friedman = detrend(M13, method = "Friedman")
rwi.stats(M13.friedman, M13.ids, prewhiten = TRUE)

M13.friedman.sss = sss(M13.friedman, M13.ids)
cutoff.friedman = max(yr[M13.friedman.sss < 0.85])

M13.friedman.crn = chron(detrend(M13[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M13")
plot(M13.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M13.modhuger = detrend(M13, method = "ModHugershoff")
rwi.stats(M13.modhuger, M13.ids, prewhiten = TRUE)

M13.modhuger.sss = sss(M13.modhuger, M13.ids)
cutoff.modhuger = max(yr[M13.modhuger.sss < 0.85])

M13.modhuger.crn = chron(detrend(M13[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M13")
plot(M13.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M13_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M13))), 
                        row.names = rownames(M13))
colnames(M13_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M13_Merged$NoDetrend[yr > cutoff] = M13.nodetrend.crn$M13std
M13_Merged$Spline[yr > cutoff.spline] = M13.spline.crn$M13std
M13_Merged$ModNegExp[yr > cutoff.modnegexp] = M13.modnegexp.crn$M13std
M13_Merged$Mean[yr > cutoff.mean] = M13.mean.crn$M13std
M13_Merged$Ar[yr > cutoff.ar + 1] = M13.ar.crn$M13std #First row is NaN
M13_Merged$Friedman[yr > cutoff.friedman] = M13.friedman.crn$M13std
M13_Merged$ModHugershoff[yr > cutoff.modhuger] = M13.modhuger.crn$M13std

M13_Merged$SampleDepth = M13.crn$samp.depth
#####
write.csv(M13_Merged, "M13_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M13.nodetrend.crn.spline = ffcsaps(M13.nodetrend.crn$M13std, nyrs = 10)
plot(M13.nodetrend.crn.spline)

#Spline#
M13.spline.crn.spline = ffcsaps(M13.spline.crn$M13std, nyrs = 10)
plot(M13.spline.crn.spline)

#ModNegExp#
M13.modnegexp.crn.spline = ffcsaps(M13.modnegexp.crn$M13std, nyrs = 10)
plot(M13.modnegexp.crn.spline)

#Mean#
M13.mean.crn.spline = ffcsaps(M13.mean.crn$M13std, nyrs = 10)
plot(M13.mean.crn.spline)

#Ar#
M13.ar.crn.spline = ffcsaps(M13.ar.crn$M13std, nyrs = 10)
plot(M13.ar.crn.spline)

#Friedman#
M13.friedman.crn.spline = ffcsaps(M13.friedman.crn$M13std, nyrs = 10)
plot(M13.friedman.crn.spline)

#ModHugershoff#
M13.modhuger.crn.spline = ffcsaps(M13.modhuger.crn$M13std, nyrs = 10)
plot(M13.modhuger.crn.spline)

###Merge into single dataset###
M13_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M13))), 
                        row.names = rownames(M13))
colnames(M13_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M13_Merged_Spline$NoDetrend[yr > cutoff] = M13.nodetrend.crn.spline
M13_Merged_Spline$Spline[yr > cutoff.spline] = M13.spline.crn.spline
M13_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M13.modnegexp.crn.spline
M13_Merged_Spline$Mean[yr > cutoff.mean] = M13.mean.crn.spline
M13_Merged_Spline$Ar[yr > cutoff.ar + 1] = M13.ar.crn.spline #First row is NaN
M13_Merged_Spline$Friedman[yr > cutoff.friedman] = M13.friedman.crn.spline
M13_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M13.modhuger.crn.spline

M13_Merged_Spline$SampleDepth = M13.crn$samp.depth
#####
write.csv(M13_Merged_Spline, "M13_Chronologies_Spline.csv")
