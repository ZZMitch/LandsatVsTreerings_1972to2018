###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06") 
#Laptop

###RWLs###
#M06 = read.rwl('M06_Acer_1.rwl')
M06 = read.rwl('M06_Quercus_1.rwl')
plot(M06, plot.type = "spag") #How to plot by age?
M06.ids = read.ids(M06, stc = c(4, 2, 1))
rwi.stats(M06, M06.ids, prewhiten = TRUE)
M06.crn = chron(M06, prefix = "M06") # Mean chronology
yr = time(M06)

M06.sss = sss(M06, M06.ids)
cutoff = max(yr[M06.sss < 0.85])

#Check for outliers
plot(M06.crn)

#####Noisy Values#####
#####
#No Detrending#
M06.nodetrend.crn = subset(M06.crn, yr > cutoff)
plot(M06.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(M06)

#Spline#
M06.spline = detrend(M06, method = "Spline")
rwi.stats(M06.spline, M06.ids, prewhiten = TRUE)

M06.spline.sss = sss(M06.spline, M06.ids)
cutoff.spline = max(yr[M06.spline.sss < 0.85])

M06.spline.crn = chron(detrend(M06[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M06")
plot(M06.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M06.modnegexp = detrend(M06, method = "ModNegExp")
rwi.stats(M06.modnegexp, M06.ids, prewhiten = TRUE)

M06.modnegexp.sss = sss(M06.modnegexp, M06.ids)
cutoff.modnegexp = max(yr[M06.modnegexp.sss < 0.85])

M06.modnegexp.crn = chron(detrend(M06[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M06")
plot(M06.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M06.mean = detrend(M06, method = "Mean")
rwi.stats(M06.mean, M06.ids, prewhiten = TRUE)

M06.mean.sss = sss(M06.mean, M06.ids)
cutoff.mean = max(yr[M06.mean.sss < 0.85])

M06.mean.crn = chron(detrend(M06[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M06")
plot(M06.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M06.ar = detrend(M06, method = "Ar")
rwi.stats(M06.ar, M06.ids, prewhiten = TRUE)

M06.ar.sss = sss(M06.ar, M06.ids)
cutoff.ar = max(yr[M06.ar.sss < 0.85])

M06.ar.crn = chron(detrend(M06[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M06")
M06.ar.crn = M06.ar.crn[-1,] # First row is NaN
plot(M06.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M06.friedman = detrend(M06, method = "Friedman")
rwi.stats(M06.friedman, M06.ids, prewhiten = TRUE)

M06.friedman.sss = sss(M06.friedman, M06.ids)
cutoff.friedman = max(yr[M06.friedman.sss < 0.85])

M06.friedman.crn = chron(detrend(M06[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M06")
plot(M06.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M06.modhuger = detrend(M06, method = "ModHugershoff")
rwi.stats(M06.modhuger, M06.ids, prewhiten = TRUE)

M06.modhuger.sss = sss(M06.modhuger, M06.ids)
cutoff.modhuger = max(yr[M06.modhuger.sss < 0.85])

M06.modhuger.crn = chron(detrend(M06[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M06")
plot(M06.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M06_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M06))), 
                        row.names = rownames(M06))
colnames(M06_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M06_Merged$NoDetrend[yr > cutoff] = M06.nodetrend.crn$M06std
M06_Merged$Spline[yr > cutoff.spline] = M06.spline.crn$M06std
M06_Merged$ModNegExp[yr > cutoff.modnegexp] = M06.modnegexp.crn$M06std
M06_Merged$Mean[yr > cutoff.mean] = M06.mean.crn$M06std
M06_Merged$Ar[yr > cutoff.ar + 1] = M06.ar.crn$M06std #First row is NaN
M06_Merged$Friedman[yr > cutoff.friedman] = M06.friedman.crn$M06std
M06_Merged$ModHugershoff[yr > cutoff.modhuger] = M06.modhuger.crn$M06std

M06_Merged$SampleDepth = M06.crn$samp.depth
#####
write.csv(M06_Merged, "M06_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M06.nodetrend.crn.spline = ffcsaps(M06.nodetrend.crn$M06std, nyrs = 10)
plot(M06.nodetrend.crn.spline)

#Spline#
M06.spline.crn.spline = ffcsaps(M06.spline.crn$M06std, nyrs = 10)
plot(M06.spline.crn.spline)

#ModNegExp#
M06.modnegexp.crn.spline = ffcsaps(M06.modnegexp.crn$M06std, nyrs = 10)
plot(M06.modnegexp.crn.spline)

#Mean#
M06.mean.crn.spline = ffcsaps(M06.mean.crn$M06std, nyrs = 10)
plot(M06.mean.crn.spline)

#Ar#
M06.ar.crn.spline = ffcsaps(M06.ar.crn$M06std, nyrs = 10)
plot(M06.ar.crn.spline)

#Friedman#
M06.friedman.crn.spline = ffcsaps(M06.friedman.crn$M06std, nyrs = 10)
plot(M06.friedman.crn.spline)

#ModHugershoff#
M06.modhuger.crn.spline = ffcsaps(M06.modhuger.crn$M06std, nyrs = 10)
plot(M06.modhuger.crn.spline)

###Merge into single dataset###
M06_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M06))), 
                        row.names = rownames(M06))
colnames(M06_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M06_Merged_Spline$NoDetrend[yr > cutoff] = M06.nodetrend.crn.spline
M06_Merged_Spline$Spline[yr > cutoff.spline] = M06.spline.crn.spline
M06_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M06.modnegexp.crn.spline
M06_Merged_Spline$Mean[yr > cutoff.mean] = M06.mean.crn.spline
M06_Merged_Spline$Ar[yr > cutoff.ar + 1] = M06.ar.crn.spline #First row is NaN
M06_Merged_Spline$Friedman[yr > cutoff.friedman] = M06.friedman.crn.spline
M06_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M06.modhuger.crn.spline

M06_Merged_Spline$SampleDepth = M06.crn$samp.depth
#####
write.csv(M06_Merged_Spline, "M06_Chronologies_Spline.csv")
