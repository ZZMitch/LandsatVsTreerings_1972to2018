###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F25") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F25") 
#Laptop

###RWLs###
F25 = read.rwl('F25_6.rwl')
plot(F25, plot.type = "spag") #How to plot by age?
F25.ids = read.ids(F25, stc = c(4, 2, 1))
rwi.stats(F25, F25.ids, prewhiten = TRUE)
F25.crn = chron(F25, prefix = "F25") # Mean chronology
yr = time(F25)

F25.sss = sss(F25, F25.ids)
cutoff = max(yr[F25.sss < 0.85])

#Check for outliers
plot(F25.crn)

#####Noisy Values#####
#####
#No Detrending#
F25.nodetrend.crn = subset(F25.crn, yr > cutoff)
plot(F25.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(F25)
#test.crn = chron(test, prefix = "F25")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)
#plot(test.crn$F25std[8:86], F25.mean.crn$F25std)

#Spline#
F25.spline = detrend(F25, method = "Spline")
rwi.stats(F25.spline, F25.ids, prewhiten = TRUE)

F25.spline.sss = sss(F25.spline, F25.ids)
cutoff.spline = max(yr[F25.spline.sss < 0.85])

F25.spline.crn = chron(detrend(F25[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F25")
plot(F25.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F25.modnegexp = detrend(F25, method = "ModNegExp")
rwi.stats(F25.modnegexp, F25.ids, prewhiten = TRUE)

F25.modnegexp.sss = sss(F25.modnegexp, F25.ids)
cutoff.modnegexp = max(yr[F25.modnegexp.sss < 0.85])

F25.modnegexp.crn = chron(detrend(F25[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F25")
plot(F25.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F25.mean = detrend(F25, method = "Mean")
rwi.stats(F25.mean, F25.ids, prewhiten = TRUE)

F25.mean.sss = sss(F25.mean, F25.ids)
cutoff.mean = max(yr[F25.mean.sss < 0.85])

F25.mean.crn = chron(detrend(F25[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F25")
plot(F25.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F25.ar = detrend(F25, method = "Ar")
rwi.stats(F25.ar, F25.ids, prewhiten = TRUE)

F25.ar.sss = sss(F25.ar, F25.ids)
cutoff.ar = max(yr[F25.ar.sss < 0.85])

F25.ar.crn = chron(detrend(F25[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F25")
F25.ar.crn = F25.ar.crn[-1,] # First row is NaN
plot(F25.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F25.friedman = detrend(F25, method = "Friedman")
rwi.stats(F25.friedman, F25.ids, prewhiten = TRUE)

F25.friedman.sss = sss(F25.friedman, F25.ids)
cutoff.friedman = max(yr[F25.friedman.sss < 0.85])

F25.friedman.crn = chron(detrend(F25[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F25")
plot(F25.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F25.modhuger = detrend(F25, method = "ModHugershoff")
rwi.stats(F25.modhuger, F25.ids, prewhiten = TRUE)

F25.modhuger.sss = sss(F25.modhuger, F25.ids)
cutoff.modhuger = max(yr[F25.modhuger.sss < 0.85])

F25.modhuger.crn = chron(detrend(F25[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F25")
plot(F25.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F25_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F25))), 
                        row.names = rownames(F25))
colnames(F25_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F25_Merged$NoDetrend[yr > cutoff] = F25.nodetrend.crn$F25std
F25_Merged$Spline[yr > cutoff.spline] = F25.spline.crn$F25std
F25_Merged$ModNegExp[yr > cutoff.modnegexp] = F25.modnegexp.crn$F25std
F25_Merged$Mean[yr > cutoff.mean] = F25.mean.crn$F25std
F25_Merged$Ar[yr > cutoff.ar + 1] = F25.ar.crn$F25std #First row is NaN
F25_Merged$Friedman[yr > cutoff.friedman] = F25.friedman.crn$F25std
F25_Merged$ModHugershoff[yr > cutoff.modhuger] = F25.modhuger.crn$F25std

F25_Merged$SampleDepth = F25.crn$samp.depth
#####
write.csv(F25_Merged, "F25_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F25.nodetrend.crn.spline = ffcsaps(F25.nodetrend.crn$F25std, nyrs = 10)
plot(F25.nodetrend.crn.spline)

#Spline#
F25.spline.crn.spline = ffcsaps(F25.spline.crn$F25std, nyrs = 10)
plot(F25.spline.crn.spline)

#ModNegExp#
F25.modnegexp.crn.spline = ffcsaps(F25.modnegexp.crn$F25std, nyrs = 10)
plot(F25.modnegexp.crn.spline)

#Mean#
F25.mean.crn.spline = ffcsaps(F25.mean.crn$F25std, nyrs = 10)
plot(F25.mean.crn.spline)

#Ar#
F25.ar.crn.spline = ffcsaps(F25.ar.crn$F25std, nyrs = 10)
plot(F25.ar.crn.spline)

#Friedman#
F25.friedman.crn.spline = ffcsaps(F25.friedman.crn$F25std, nyrs = 10)
plot(F25.friedman.crn.spline)

#ModHugershoff#
F25.modhuger.crn.spline = ffcsaps(F25.modhuger.crn$F25std, nyrs = 10)
plot(F25.modhuger.crn.spline)

###Merge into single dataset###
F25_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F25))), 
                        row.names = rownames(F25))
colnames(F25_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F25_Merged_Spline$NoDetrend[yr > cutoff] = F25.nodetrend.crn.spline
F25_Merged_Spline$Spline[yr > cutoff.spline] = F25.spline.crn.spline
F25_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F25.modnegexp.crn.spline
F25_Merged_Spline$Mean[yr > cutoff.mean] = F25.mean.crn.spline
F25_Merged_Spline$Ar[yr > cutoff.ar + 1] = F25.ar.crn.spline #First row is NaN
F25_Merged_Spline$Friedman[yr > cutoff.friedman] = F25.friedman.crn.spline
F25_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F25.modhuger.crn.spline

F25_Merged_Spline$SampleDepth = F25.crn$samp.depth
#####
write.csv(F25_Merged_Spline, "F25_Chronologies_Spline.csv")
