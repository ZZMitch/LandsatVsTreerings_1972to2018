###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07") 
#Laptop

###RWLs###
#F07 = read.rwl('F07_12.rwl')
F07 = read.rwl('F07_Acer_12.rwl')
plot(F07, plot.type = "spag") #How to plot by age?
F07.ids = read.ids(F07, stc = c(4, 2, 1))
rwi.stats(F07, F07.ids, prewhiten = TRUE)
F07.crn = chron(F07, prefix = "F07") # Mean chronology
yr = time(F07)

F07.sss = sss(F07, F07.ids)
cutoff = max(yr[F07.sss < 0.85])

#Check for outliers
plot(F07.crn)

#####Noisy Values#####
#####
#No Detrending#
F07.nodetrend.crn = subset(F07.crn, yr > cutoff)
plot(F07.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(F07)
#test.crn = chron(test, prefix = "F07")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)
#plot(test.crn$F07std[8:86], F07.mean.crn$F07std)

#Spline#
F07.spline = detrend(F07, method = "Spline")
rwi.stats(F07.spline, F07.ids, prewhiten = TRUE)

F07.spline.sss = sss(F07.spline, F07.ids)
cutoff.spline = max(yr[F07.spline.sss < 0.85])

F07.spline.crn = chron(detrend(F07[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F07")
plot(F07.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F07.modnegexp = detrend(F07, method = "ModNegExp")
rwi.stats(F07.modnegexp, F07.ids, prewhiten = TRUE)

F07.modnegexp.sss = sss(F07.modnegexp, F07.ids)
cutoff.modnegexp = max(yr[F07.modnegexp.sss < 0.85])

F07.modnegexp.crn = chron(detrend(F07[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F07")
plot(F07.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F07.mean = detrend(F07, method = "Mean")
rwi.stats(F07.mean, F07.ids, prewhiten = TRUE)

F07.mean.sss = sss(F07.mean, F07.ids)
cutoff.mean = max(yr[F07.mean.sss < 0.85])

F07.mean.crn = chron(detrend(F07[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F07")
plot(F07.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F07.ar = detrend(F07, method = "Ar")
rwi.stats(F07.ar, F07.ids, prewhiten = TRUE)

F07.ar.sss = sss(F07.ar, F07.ids)
cutoff.ar = max(yr[F07.ar.sss < 0.85])

F07.ar.crn = chron(detrend(F07[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F07")
F07.ar.crn = F07.ar.crn[-1,] # First row is NaN
plot(F07.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F07.friedman = detrend(F07, method = "Friedman")
rwi.stats(F07.friedman, F07.ids, prewhiten = TRUE)

F07.friedman.sss = sss(F07.friedman, F07.ids)
cutoff.friedman = max(yr[F07.friedman.sss < 0.85])

F07.friedman.crn = chron(detrend(F07[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F07")
plot(F07.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F07.modhuger = detrend(F07, method = "ModHugershoff")
rwi.stats(F07.modhuger, F07.ids, prewhiten = TRUE)

F07.modhuger.sss = sss(F07.modhuger, F07.ids)
cutoff.modhuger = max(yr[F07.modhuger.sss < 0.85])

F07.modhuger.crn = chron(detrend(F07[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F07")
plot(F07.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F07_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F07))), 
                        row.names = rownames(F07))
colnames(F07_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F07_Merged$NoDetrend[yr > cutoff] = F07.nodetrend.crn$F07std
F07_Merged$Spline[yr > cutoff.spline] = F07.spline.crn$F07std
F07_Merged$ModNegExp[yr > cutoff.modnegexp] = F07.modnegexp.crn$F07std
F07_Merged$Mean[yr > cutoff.mean] = F07.mean.crn$F07std
F07_Merged$Ar[yr > cutoff.ar + 1] = F07.ar.crn$F07std #First row is NaN
F07_Merged$Friedman[yr > cutoff.friedman] = F07.friedman.crn$F07std
F07_Merged$ModHugershoff[yr > cutoff.modhuger] = F07.modhuger.crn$F07std

F07_Merged$SampleDepth = F07.crn$samp.depth
#####
write.csv(F07_Merged, "F07_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F07.nodetrend.crn.spline = ffcsaps(F07.nodetrend.crn$F07std, nyrs = 10)
plot(F07.nodetrend.crn.spline)

#Spline#
F07.spline.crn.spline = ffcsaps(F07.spline.crn$F07std, nyrs = 10)
plot(F07.spline.crn.spline)

#ModNegExp#
F07.modnegexp.crn.spline = ffcsaps(F07.modnegexp.crn$F07std, nyrs = 10)
plot(F07.modnegexp.crn.spline)

#Mean#
F07.mean.crn.spline = ffcsaps(F07.mean.crn$F07std, nyrs = 10)
plot(F07.mean.crn.spline)

#Ar#
F07.ar.crn.spline = ffcsaps(F07.ar.crn$F07std, nyrs = 10)
plot(F07.ar.crn.spline)

#Friedman#
F07.friedman.crn.spline = ffcsaps(F07.friedman.crn$F07std, nyrs = 10)
plot(F07.friedman.crn.spline)

#ModHugershoff#
F07.modhuger.crn.spline = ffcsaps(F07.modhuger.crn$F07std, nyrs = 10)
plot(F07.modhuger.crn.spline)

###Merge into single dataset###
F07_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F07))), 
                        row.names = rownames(F07))
colnames(F07_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F07_Merged_Spline$NoDetrend[yr > cutoff] = F07.nodetrend.crn.spline
F07_Merged_Spline$Spline[yr > cutoff.spline] = F07.spline.crn.spline
F07_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F07.modnegexp.crn.spline
F07_Merged_Spline$Mean[yr > cutoff.mean] = F07.mean.crn.spline
F07_Merged_Spline$Ar[yr > cutoff.ar + 1] = F07.ar.crn.spline #First row is NaN
F07_Merged_Spline$Friedman[yr > cutoff.friedman] = F07.friedman.crn.spline
F07_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F07.modhuger.crn.spline

F07_Merged_Spline$SampleDepth = F07.crn$samp.depth
#####
write.csv(F07_Merged_Spline, "F07_Chronologies_Spline.csv")
