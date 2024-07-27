###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F30") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F30") 
#Laptop

###RWLs###
#F30 = read.rwl('F30_Acer_5.rwl')
F30 = read.rwl('F30_Betula_1.rwl')
plot(F30, plot.type = "spag") #How to plot by age?
F30.ids = read.ids(F30, stc = c(4, 2, 1))
rwi.stats(F30, F30.ids, prewhiten = TRUE)
F30.crn = chron(F30, prefix = "F30") # Mean chronology
yr = time(F30)

F30.sss = sss(F30, F30.ids)
cutoff = max(yr[F30.sss < 0.85])

#Check for outliers
plot(F30.crn)

#####Noisy Values#####
#####
#No Detrending#
F30.nodetrend.crn = subset(F30.crn, yr > cutoff)
plot(F30.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(F30)

#Spline#
F30.spline = detrend(F30, method = "Spline")
rwi.stats(F30.spline, F30.ids, prewhiten = TRUE)

F30.spline.sss = sss(F30.spline, F30.ids)
cutoff.spline = max(yr[F30.spline.sss < 0.85])

F30.spline.crn = chron(detrend(F30[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F30")
plot(F30.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F30.modnegexp = detrend(F30, method = "ModNegExp")
rwi.stats(F30.modnegexp, F30.ids, prewhiten = TRUE)

F30.modnegexp.sss = sss(F30.modnegexp, F30.ids)
cutoff.modnegexp = max(yr[F30.modnegexp.sss < 0.85])

F30.modnegexp.crn = chron(detrend(F30[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F30")
plot(F30.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F30.mean = detrend(F30, method = "Mean")
rwi.stats(F30.mean, F30.ids, prewhiten = TRUE)

F30.mean.sss = sss(F30.mean, F30.ids)
cutoff.mean = max(yr[F30.mean.sss < 0.85])

F30.mean.crn = chron(detrend(F30[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F30")
plot(F30.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F30.ar = detrend(F30, method = "Ar")
rwi.stats(F30.ar, F30.ids, prewhiten = TRUE)

F30.ar.sss = sss(F30.ar, F30.ids)
cutoff.ar = max(yr[F30.ar.sss < 0.85])

F30.ar.crn = chron(detrend(F30[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F30")
F30.ar.crn = F30.ar.crn[-1,] # First row is NaN
plot(F30.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F30.friedman = detrend(F30, method = "Friedman")
rwi.stats(F30.friedman, F30.ids, prewhiten = TRUE)

F30.friedman.sss = sss(F30.friedman, F30.ids)
cutoff.friedman = max(yr[F30.friedman.sss < 0.85])

F30.friedman.crn = chron(detrend(F30[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F30")
plot(F30.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F30.modhuger = detrend(F30, method = "ModHugershoff")
rwi.stats(F30.modhuger, F30.ids, prewhiten = TRUE)

F30.modhuger.sss = sss(F30.modhuger, F30.ids)
cutoff.modhuger = max(yr[F30.modhuger.sss < 0.85])

F30.modhuger.crn = chron(detrend(F30[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F30")
plot(F30.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F30_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F30))), 
                        row.names = rownames(F30))
colnames(F30_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F30_Merged$NoDetrend[yr > cutoff] = F30.nodetrend.crn$F30std
F30_Merged$Spline[yr > cutoff.spline] = F30.spline.crn$F30std
F30_Merged$ModNegExp[yr > cutoff.modnegexp] = F30.modnegexp.crn$F30std
F30_Merged$Mean[yr > cutoff.mean] = F30.mean.crn$F30std
F30_Merged$Ar[yr > cutoff.ar + 1] = F30.ar.crn$F30std #First row is NaN
F30_Merged$Friedman[yr > cutoff.friedman] = F30.friedman.crn$F30std
F30_Merged$ModHugershoff[yr > cutoff.modhuger] = F30.modhuger.crn$F30std

F30_Merged$SampleDepth = F30.crn$samp.depth
#####
write.csv(F30_Merged, "F30_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F30.nodetrend.crn.spline = ffcsaps(F30.nodetrend.crn$F30std, nyrs = 10)
plot(F30.nodetrend.crn.spline)

#Spline#
F30.spline.crn.spline = ffcsaps(F30.spline.crn$F30std, nyrs = 10)
plot(F30.spline.crn.spline)

#ModNegExp#
F30.modnegexp.crn.spline = ffcsaps(F30.modnegexp.crn$F30std, nyrs = 10)
plot(F30.modnegexp.crn.spline)

#Mean#
F30.mean.crn.spline = ffcsaps(F30.mean.crn$F30std, nyrs = 10)
plot(F30.mean.crn.spline)

#Ar#
F30.ar.crn.spline = ffcsaps(F30.ar.crn$F30std, nyrs = 10)
plot(F30.ar.crn.spline)

#Friedman#
F30.friedman.crn.spline = ffcsaps(F30.friedman.crn$F30std, nyrs = 10)
plot(F30.friedman.crn.spline)

#ModHugershoff#
F30.modhuger.crn.spline = ffcsaps(F30.modhuger.crn$F30std, nyrs = 10)
plot(F30.modhuger.crn.spline)

###Merge into single dataset###
F30_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F30))), 
                        row.names = rownames(F30))
colnames(F30_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F30_Merged_Spline$NoDetrend[yr > cutoff] = F30.nodetrend.crn.spline
F30_Merged_Spline$Spline[yr > cutoff.spline] = F30.spline.crn.spline
F30_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F30.modnegexp.crn.spline
F30_Merged_Spline$Mean[yr > cutoff.mean] = F30.mean.crn.spline
F30_Merged_Spline$Ar[yr > cutoff.ar + 1] = F30.ar.crn.spline #First row is NaN
F30_Merged_Spline$Friedman[yr > cutoff.friedman] = F30.friedman.crn.spline
F30_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F30.modhuger.crn.spline

F30_Merged_Spline$SampleDepth = F30.crn$samp.depth
#####
write.csv(F30_Merged_Spline, "F30_Chronologies_Spline.csv")
