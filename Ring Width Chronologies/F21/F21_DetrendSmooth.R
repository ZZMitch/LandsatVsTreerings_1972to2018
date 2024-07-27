###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21") 
#Laptop

###RWLs###
#F21 = read.rwl('F21_11.rwl')
plot(F21, plot.type = "spag") #How to plot by age?
F21 = read.rwl('F21_Acer_7.rwl')
F21.ids = read.ids(F21, stc = c(4, 2, 1))
rwi.stats(F21, F21.ids, prewhiten = TRUE)
F21.crn = chron(F21, prefix = "F21") # Mean chronology
yr = time(F21)

F21.sss = sss(F21, F21.ids)
cutoff = max(yr[F21.sss < 0.85])

#Check for outliers
plot(F21.crn)

#####Noisy Values#####
#####
#No Detrending#
F21.nodetrend.crn = subset(F21.crn, yr > cutoff)
plot(F21.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(F21)

#Spline#
F21.spline = detrend(F21, method = "Spline")
rwi.stats(F21.spline, F21.ids, prewhiten = TRUE)

F21.spline.sss = sss(F21.spline, F21.ids)
cutoff.spline = max(yr[F21.spline.sss < 0.85])

F21.spline.crn = chron(detrend(F21[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F21")
plot(F21.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F21.modnegexp = detrend(F21, method = "ModNegExp")
rwi.stats(F21.modnegexp, F21.ids, prewhiten = TRUE)

F21.modnegexp.sss = sss(F21.modnegexp, F21.ids)
cutoff.modnegexp = max(yr[F21.modnegexp.sss < 0.85])

F21.modnegexp.crn = chron(detrend(F21[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F21")
plot(F21.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F21.mean = detrend(F21, method = "Mean")
rwi.stats(F21.mean, F21.ids, prewhiten = TRUE)

F21.mean.sss = sss(F21.mean, F21.ids)
cutoff.mean = max(yr[F21.mean.sss < 0.85])

F21.mean.crn = chron(detrend(F21[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F21")
plot(F21.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F21.ar = detrend(F21, method = "Ar")
rwi.stats(F21.ar, F21.ids, prewhiten = TRUE)

F21.ar.sss = sss(F21.ar, F21.ids)
cutoff.ar = max(yr[F21.ar.sss < 0.85])

F21.ar.crn = chron(detrend(F21[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F21")
F21.ar.crn = F21.ar.crn[-1,] # First row is NaN
plot(F21.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F21.friedman = detrend(F21, method = "Friedman")
rwi.stats(F21.friedman, F21.ids, prewhiten = TRUE)

F21.friedman.sss = sss(F21.friedman, F21.ids)
cutoff.friedman = max(yr[F21.friedman.sss < 0.85])

F21.friedman.crn = chron(detrend(F21[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F21")
plot(F21.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F21.modhuger = detrend(F21, method = "ModHugershoff")
rwi.stats(F21.modhuger, F21.ids, prewhiten = TRUE)

F21.modhuger.sss = sss(F21.modhuger, F21.ids)
cutoff.modhuger = max(yr[F21.modhuger.sss < 0.85])

F21.modhuger.crn = chron(detrend(F21[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F21")
plot(F21.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F21_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F21))), 
                        row.names = rownames(F21))
colnames(F21_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F21_Merged$NoDetrend[yr > cutoff] = F21.nodetrend.crn$F21std
F21_Merged$Spline[yr > cutoff.spline] = F21.spline.crn$F21std
F21_Merged$ModNegExp[yr > cutoff.modnegexp] = F21.modnegexp.crn$F21std
F21_Merged$Mean[yr > cutoff.mean] = F21.mean.crn$F21std
F21_Merged$Ar[yr > cutoff.ar + 1] = F21.ar.crn$F21std #First row is NaN
F21_Merged$Friedman[yr > cutoff.friedman] = F21.friedman.crn$F21std
F21_Merged$ModHugershoff[yr > cutoff.modhuger] = F21.modhuger.crn$F21std

F21_Merged$SampleDepth = F21.crn$samp.depth
#####
write.csv(F21_Merged, "F21_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F21.nodetrend.crn.spline = ffcsaps(F21.nodetrend.crn$F21std, nyrs = 10)
plot(F21.nodetrend.crn.spline)

#Spline#
F21.spline.crn.spline = ffcsaps(F21.spline.crn$F21std, nyrs = 10)
plot(F21.spline.crn.spline)

#ModNegExp#
F21.modnegexp.crn.spline = ffcsaps(F21.modnegexp.crn$F21std, nyrs = 10)
plot(F21.modnegexp.crn.spline)

#Mean#
F21.mean.crn.spline = ffcsaps(F21.mean.crn$F21std, nyrs = 10)
plot(F21.mean.crn.spline)

#Ar#
F21.ar.crn.spline = ffcsaps(F21.ar.crn$F21std, nyrs = 10)
plot(F21.ar.crn.spline)

#Friedman#
F21.friedman.crn.spline = ffcsaps(F21.friedman.crn$F21std, nyrs = 10)
plot(F21.friedman.crn.spline)

#ModHugershoff#
F21.modhuger.crn.spline = ffcsaps(F21.modhuger.crn$F21std, nyrs = 10)
plot(F21.modhuger.crn.spline)

###Merge into single dataset###
F21_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F21))), 
                        row.names = rownames(F21))
colnames(F21_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F21_Merged_Spline$NoDetrend[yr > cutoff] = F21.nodetrend.crn.spline
F21_Merged_Spline$Spline[yr > cutoff.spline] = F21.spline.crn.spline
F21_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F21.modnegexp.crn.spline
F21_Merged_Spline$Mean[yr > cutoff.mean] = F21.mean.crn.spline
F21_Merged_Spline$Ar[yr > cutoff.ar + 1] = F21.ar.crn.spline #First row is NaN
F21_Merged_Spline$Friedman[yr > cutoff.friedman] = F21.friedman.crn.spline
F21_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F21.modhuger.crn.spline

F21_Merged_Spline$SampleDepth = F21.crn$samp.depth
#####
write.csv(F21_Merged_Spline, "F21_Chronologies_Spline.csv")
