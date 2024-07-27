###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") 
#Laptop

###RWLs###
M27 = read.rwl('M27_6.rwl')
plot(M27, plot.type = "spag") #How to plot by age?
M27.ids = read.ids(M27, stc = c(4, 2, 1))
rwi.stats(M27, M27.ids, prewhiten = TRUE)
M27.crn = chron(M27, prefix = "M27") # Mean chronology
yr = time(M27)

M27.sss = sss(M27, M27.ids)
cutoff = max(yr[M27.sss < 0.85])

#Check for outliers
plot(M27.crn)

#####Noisy Values#####
#####
#No Detrending#
M27.nodetrend.crn = subset(M27.crn, yr > cutoff)
plot(M27.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(M27)

#Spline#
M27.spline = detrend(M27, method = "Spline")
rwi.stats(M27.spline, M27.ids, prewhiten = TRUE)

M27.spline.sss = sss(M27.spline, M27.ids)
cutoff.spline = max(yr[M27.spline.sss < 0.85])

M27.spline.crn = chron(detrend(M27[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M27")
plot(M27.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M27.modnegexp = detrend(M27, method = "ModNegExp")
rwi.stats(M27.modnegexp, M27.ids, prewhiten = TRUE)

M27.modnegexp.sss = sss(M27.modnegexp, M27.ids)
cutoff.modnegexp = max(yr[M27.modnegexp.sss < 0.85])

M27.modnegexp.crn = chron(detrend(M27[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M27")
plot(M27.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M27.mean = detrend(M27, method = "Mean")
rwi.stats(M27.mean, M27.ids, prewhiten = TRUE)

M27.mean.sss = sss(M27.mean, M27.ids)
cutoff.mean = max(yr[M27.mean.sss < 0.85])

M27.mean.crn = chron(detrend(M27[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M27")
plot(M27.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M27.ar = detrend(M27, method = "Ar")
rwi.stats(M27.ar, M27.ids, prewhiten = TRUE)

M27.ar.sss = sss(M27.ar, M27.ids)
cutoff.ar = max(yr[M27.ar.sss < 0.85])

M27.ar.crn = chron(detrend(M27[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M27")
M27.ar.crn = M27.ar.crn[-1,] # First row is NaN
plot(M27.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M27.friedman = detrend(M27, method = "Friedman")
rwi.stats(M27.friedman, M27.ids, prewhiten = TRUE)

M27.friedman.sss = sss(M27.friedman, M27.ids)
cutoff.friedman = max(yr[M27.friedman.sss < 0.85])

M27.friedman.crn = chron(detrend(M27[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M27")
plot(M27.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M27.modhuger = detrend(M27, method = "ModHugershoff")
rwi.stats(M27.modhuger, M27.ids, prewhiten = TRUE)

M27.modhuger.sss = sss(M27.modhuger, M27.ids)
cutoff.modhuger = max(yr[M27.modhuger.sss < 0.85])

M27.modhuger.crn = chron(detrend(M27[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M27")
plot(M27.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M27_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M27))), 
                        row.names = rownames(M27))
colnames(M27_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M27_Merged$NoDetrend[yr > cutoff] = M27.nodetrend.crn$M27std
M27_Merged$Spline[yr > cutoff.spline] = M27.spline.crn$M27std
M27_Merged$ModNegExp[yr > cutoff.modnegexp] = M27.modnegexp.crn$M27std
M27_Merged$Mean[yr > cutoff.mean] = M27.mean.crn$M27std
M27_Merged$Ar[yr > cutoff.ar + 1] = M27.ar.crn$M27std #First row is NaN
M27_Merged$Friedman[yr > cutoff.friedman] = M27.friedman.crn$M27std
M27_Merged$ModHugershoff[yr > cutoff.modhuger] = M27.modhuger.crn$M27std

M27_Merged$SampleDepth = M27.crn$samp.depth
#####
write.csv(M27_Merged, "M27_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M27.nodetrend.crn.spline = ffcsaps(M27.nodetrend.crn$M27std, nyrs = 10)
plot(M27.nodetrend.crn.spline)

#Spline#
M27.spline.crn.spline = ffcsaps(M27.spline.crn$M27std, nyrs = 10)
plot(M27.spline.crn.spline)

#ModNegExp#
M27.modnegexp.crn.spline = ffcsaps(M27.modnegexp.crn$M27std, nyrs = 10)
plot(M27.modnegexp.crn.spline)

#Mean#
M27.mean.crn.spline = ffcsaps(M27.mean.crn$M27std, nyrs = 10)
plot(M27.mean.crn.spline)

#Ar#
M27.ar.crn.spline = ffcsaps(M27.ar.crn$M27std, nyrs = 10)
plot(M27.ar.crn.spline)

#Friedman#
M27.friedman.crn.spline = ffcsaps(M27.friedman.crn$M27std, nyrs = 10)
plot(M27.friedman.crn.spline)

#ModHugershoff#
M27.modhuger.crn.spline = ffcsaps(M27.modhuger.crn$M27std, nyrs = 10)
plot(M27.modhuger.crn.spline)

###Merge into single dataset###
M27_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M27))), 
                        row.names = rownames(M27))
colnames(M27_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M27_Merged_Spline$NoDetrend[yr > cutoff] = M27.nodetrend.crn.spline
M27_Merged_Spline$Spline[yr > cutoff.spline] = M27.spline.crn.spline
M27_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M27.modnegexp.crn.spline
M27_Merged_Spline$Mean[yr > cutoff.mean] = M27.mean.crn.spline
M27_Merged_Spline$Ar[yr > cutoff.ar + 1] = M27.ar.crn.spline #First row is NaN
M27_Merged_Spline$Friedman[yr > cutoff.friedman] = M27.friedman.crn.spline
M27_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M27.modhuger.crn.spline

M27_Merged_Spline$SampleDepth = M27.crn$samp.depth
#####
write.csv(M27_Merged_Spline, "M27_Chronologies_Spline.csv")
