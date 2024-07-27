###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M17") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M17") 
#Laptop

###RWLs###
M17 = read.rwl('M17_1.rwl')
plot(M17, plot.type = "spag") #How to plot by age?
M17.ids = read.ids(M17, stc = c(4, 2, 1))
rwi.stats(M17, M17.ids, prewhiten = TRUE)
M17.crn = chron(M17, prefix = "M17") # Mean chronology
yr = time(M17)

M17.sss = sss(M17, M17.ids)
cutoff = max(yr[M17.sss < 0.85])

#Check for outliers
plot(M17.crn)

#####Noisy Values#####
#####
#No Detrending#
M17.nodetrend.crn = subset(M17.crn, yr > cutoff)
plot(M17.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(M17)

#Spline#
M17.spline = detrend(M17, method = "Spline")
rwi.stats(M17.spline, M17.ids, prewhiten = TRUE)

M17.spline.sss = sss(M17.spline, M17.ids)
cutoff.spline = max(yr[M17.spline.sss < 0.85])

M17.spline.crn = chron(detrend(M17[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M17")
plot(M17.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M17.modnegexp = detrend(M17, method = "ModNegExp")
rwi.stats(M17.modnegexp, M17.ids, prewhiten = TRUE)

M17.modnegexp.sss = sss(M17.modnegexp, M17.ids)
cutoff.modnegexp = max(yr[M17.modnegexp.sss < 0.85])

M17.modnegexp.crn = chron(detrend(M17[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M17")
plot(M17.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M17.mean = detrend(M17, method = "Mean")
rwi.stats(M17.mean, M17.ids, prewhiten = TRUE)

M17.mean.sss = sss(M17.mean, M17.ids)
cutoff.mean = max(yr[M17.mean.sss < 0.85])

M17.mean.crn = chron(detrend(M17[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M17")
plot(M17.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M17.ar = detrend(M17, method = "Ar")
rwi.stats(M17.ar, M17.ids, prewhiten = TRUE)

M17.ar.sss = sss(M17.ar, M17.ids)
cutoff.ar = max(yr[M17.ar.sss < 0.85])

M17.ar.crn = chron(detrend(M17[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M17")
M17.ar.crn = M17.ar.crn[-1,] # First row is NaN
plot(M17.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M17.friedman = detrend(M17, method = "Friedman")
rwi.stats(M17.friedman, M17.ids, prewhiten = TRUE)

M17.friedman.sss = sss(M17.friedman, M17.ids)
cutoff.friedman = max(yr[M17.friedman.sss < 0.85])

M17.friedman.crn = chron(detrend(M17[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M17")
plot(M17.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M17.modhuger = detrend(M17, method = "ModHugershoff")
rwi.stats(M17.modhuger, M17.ids, prewhiten = TRUE)

M17.modhuger.sss = sss(M17.modhuger, M17.ids)
cutoff.modhuger = max(yr[M17.modhuger.sss < 0.85])

M17.modhuger.crn = chron(detrend(M17[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M17")
plot(M17.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M17_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M17))), 
                        row.names = rownames(M17))
colnames(M17_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M17_Merged$NoDetrend[yr > cutoff] = M17.nodetrend.crn$M17std
M17_Merged$Spline[yr > cutoff.spline] = M17.spline.crn$M17std
M17_Merged$ModNegExp[yr > cutoff.modnegexp] = M17.modnegexp.crn$M17std
M17_Merged$Mean[yr > cutoff.mean] = M17.mean.crn$M17std
M17_Merged$Ar[yr > cutoff.ar + 1] = M17.ar.crn$M17std #First row is NaN
M17_Merged$Friedman[yr > cutoff.friedman] = M17.friedman.crn$M17std
M17_Merged$ModHugershoff[yr > cutoff.modhuger] = M17.modhuger.crn$M17std

M17_Merged$SampleDepth = M17.crn$samp.depth
#####
write.csv(M17_Merged, "M17_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M17.nodetrend.crn.spline = ffcsaps(M17.nodetrend.crn$M17std, nyrs = 10)
plot(M17.nodetrend.crn.spline)

#Spline#
M17.spline.crn.spline = ffcsaps(M17.spline.crn$M17std, nyrs = 10)
plot(M17.spline.crn.spline)

#ModNegExp#
M17.modnegexp.crn.spline = ffcsaps(M17.modnegexp.crn$M17std, nyrs = 10)
plot(M17.modnegexp.crn.spline)

#Mean#
M17.mean.crn.spline = ffcsaps(M17.mean.crn$M17std, nyrs = 10)
plot(M17.mean.crn.spline)

#Ar#
M17.ar.crn.spline = ffcsaps(M17.ar.crn$M17std, nyrs = 10)
plot(M17.ar.crn.spline)

#Friedman#
M17.friedman.crn.spline = ffcsaps(M17.friedman.crn$M17std, nyrs = 10)
plot(M17.friedman.crn.spline)

#ModHugershoff#
M17.modhuger.crn.spline = ffcsaps(M17.modhuger.crn$M17std, nyrs = 10)
plot(M17.modhuger.crn.spline)

###Merge into single dataset###
M17_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M17))), 
                        row.names = rownames(M17))
colnames(M17_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M17_Merged_Spline$NoDetrend[yr > cutoff] = M17.nodetrend.crn.spline
M17_Merged_Spline$Spline[yr > cutoff.spline] = M17.spline.crn.spline
M17_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M17.modnegexp.crn.spline
M17_Merged_Spline$Mean[yr > cutoff.mean] = M17.mean.crn.spline
M17_Merged_Spline$Ar[yr > cutoff.ar + 1] = M17.ar.crn.spline #First row is NaN
M17_Merged_Spline$Friedman[yr > cutoff.friedman] = M17.friedman.crn.spline
M17_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M17.modhuger.crn.spline

M17_Merged_Spline$SampleDepth = M17.crn$samp.depth
#####
write.csv(M17_Merged_Spline, "M17_Chronologies_Spline.csv")
