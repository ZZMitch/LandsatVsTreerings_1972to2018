###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01") 
#Laptop

###RWLs###
M01 = read.rwl('M01_11.rwl')
plot(M01, plot.type = "spag") #How to plot by age?
M01.ids = read.ids(M01, stc = c(4, 2, 1))
rwi.stats(M01, M01.ids, prewhiten = TRUE)
M01.crn = chron(M01, prefix = "M01") # Mean chronology
yr = time(M01)

M01.sss = sss(M01, M01.ids)
cutoff = max(yr[M01.sss < 0.85])

#Check for outliers
plot(M01.crn)

#####Noisy Values#####
#####
#No Detrending#
M01.nodetrend.crn = subset(M01.crn, yr > cutoff)
plot(M01.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(M01)

#Spline#
M01.spline = detrend(M01, method = "Spline")
rwi.stats(M01.spline, M01.ids, prewhiten = TRUE)

M01.spline.sss = sss(M01.spline, M01.ids)
cutoff.spline = max(yr[M01.spline.sss < 0.85])

M01.spline.crn = chron(detrend(M01[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M01")
plot(M01.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M01.modnegexp = detrend(M01, method = "ModNegExp")
rwi.stats(M01.modnegexp, M01.ids, prewhiten = TRUE)

M01.modnegexp.sss = sss(M01.modnegexp, M01.ids)
cutoff.modnegexp = max(yr[M01.modnegexp.sss < 0.85])

M01.modnegexp.crn = chron(detrend(M01[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M01")
plot(M01.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M01.mean = detrend(M01, method = "Mean")
rwi.stats(M01.mean, M01.ids, prewhiten = TRUE)

M01.mean.sss = sss(M01.mean, M01.ids)
cutoff.mean = max(yr[M01.mean.sss < 0.85])

M01.mean.crn = chron(detrend(M01[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M01")
plot(M01.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M01.ar = detrend(M01, method = "Ar")
rwi.stats(M01.ar, M01.ids, prewhiten = TRUE)

M01.ar.sss = sss(M01.ar, M01.ids)
cutoff.ar = max(yr[M01.ar.sss < 0.85])

M01.ar.crn = chron(detrend(M01[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M01")
M01.ar.crn = M01.ar.crn[-1,] # First row is NaN
plot(M01.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M01.friedman = detrend(M01, method = "Friedman")
rwi.stats(M01.friedman, M01.ids, prewhiten = TRUE)

M01.friedman.sss = sss(M01.friedman, M01.ids)
cutoff.friedman = max(yr[M01.friedman.sss < 0.85])

M01.friedman.crn = chron(detrend(M01[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M01")
plot(M01.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M01.modhuger = detrend(M01, method = "ModHugershoff")
rwi.stats(M01.modhuger, M01.ids, prewhiten = TRUE)

M01.modhuger.sss = sss(M01.modhuger, M01.ids)
cutoff.modhuger = max(yr[M01.modhuger.sss < 0.85])

M01.modhuger.crn = chron(detrend(M01[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M01")
plot(M01.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M01_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M01))), 
                        row.names = rownames(M01))
colnames(M01_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M01_Merged$NoDetrend[yr > cutoff] = M01.nodetrend.crn$M01std
M01_Merged$Spline[yr > cutoff.spline] = M01.spline.crn$M01std
M01_Merged$ModNegExp[yr > cutoff.modnegexp] = M01.modnegexp.crn$M01std
M01_Merged$Mean[yr > cutoff.mean] = M01.mean.crn$M01std
M01_Merged$Ar[yr > cutoff.ar + 1] = M01.ar.crn$M01std #First row is NaN
M01_Merged$Friedman[yr > cutoff.friedman] = M01.friedman.crn$M01std
M01_Merged$ModHugershoff[yr > cutoff.modhuger] = M01.modhuger.crn$M01std

M01_Merged$SampleDepth = M01.crn$samp.depth
#####
write.csv(M01_Merged, "M01_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M01.nodetrend.crn.spline = ffcsaps(M01.nodetrend.crn$M01std, nyrs = 10)
plot(M01.nodetrend.crn.spline)

#Spline#
M01.spline.crn.spline = ffcsaps(M01.spline.crn$M01std, nyrs = 10)
plot(M01.spline.crn.spline)

#ModNegExp#
M01.modnegexp.crn.spline = ffcsaps(M01.modnegexp.crn$M01std, nyrs = 10)
plot(M01.modnegexp.crn.spline)

#Mean#
M01.mean.crn.spline = ffcsaps(M01.mean.crn$M01std, nyrs = 10)
plot(M01.mean.crn.spline)

#Ar#
M01.ar.crn.spline = ffcsaps(M01.ar.crn$M01std, nyrs = 10)
plot(M01.ar.crn.spline)

#Friedman#
M01.friedman.crn.spline = ffcsaps(M01.friedman.crn$M01std, nyrs = 10)
plot(M01.friedman.crn.spline)

#ModHugershoff#
M01.modhuger.crn.spline = ffcsaps(M01.modhuger.crn$M01std, nyrs = 10)
plot(M01.modhuger.crn.spline)

###Merge into single dataset###
M01_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M01))), 
                        row.names = rownames(M01))
colnames(M01_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M01_Merged_Spline$NoDetrend[yr > cutoff] = M01.nodetrend.crn.spline
M01_Merged_Spline$Spline[yr > cutoff.spline] = M01.spline.crn.spline
M01_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M01.modnegexp.crn.spline
M01_Merged_Spline$Mean[yr > cutoff.mean] = M01.mean.crn.spline
M01_Merged_Spline$Ar[yr > cutoff.ar + 1] = M01.ar.crn.spline #First row is NaN
M01_Merged_Spline$Friedman[yr > cutoff.friedman] = M01.friedman.crn.spline
M01_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M01.modhuger.crn.spline

M01_Merged_Spline$SampleDepth = M01.crn$samp.depth
#####
write.csv(M01_Merged_Spline, "M01_Chronologies_Spline.csv")
