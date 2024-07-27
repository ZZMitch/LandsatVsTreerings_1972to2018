###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F15") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F15") 
#Laptop

###RWLs###
#F15 = read.rwl('F15_4.rwl')
#F15 = read.rwl('F15_Acer_1.rwl')
#F15 = read.rwl('F15_Carya_1.rwl')
F15 = read.rwl('F15_Quercus_2.rwl')
plot(F15, plot.type = "spag") #How to plot by age?
F15.ids = read.ids(F15, stc = c(4, 2, 1))
rwi.stats(F15, F15.ids, prewhiten = TRUE)
F15.crn = chron(F15, prefix = "F15") # Mean chronology
yr = time(F15)

F15.sss = sss(F15, F15.ids)
cutoff = max(yr[F15.sss < 0.85])

#Check for outliers
plot(F15.crn)

#####Noisy Values#####
#####
#No Detrending#
F15.nodetrend.crn = subset(F15.crn, yr > cutoff)
plot(F15.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(F15)
#test.crn = chron(test, prefix = "F15")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)
#plot(test.crn$F15std[8:86], F15.mean.crn$F15std)

#Spline#
F15.spline = detrend(F15, method = "Spline")
rwi.stats(F15.spline, F15.ids, prewhiten = TRUE)

F15.spline.sss = sss(F15.spline, F15.ids)
cutoff.spline = max(yr[F15.spline.sss < 0.85])

F15.spline.crn = chron(detrend(F15[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F15")
plot(F15.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F15.modnegexp = detrend(F15, method = "ModNegExp")
rwi.stats(F15.modnegexp, F15.ids, prewhiten = TRUE)

F15.modnegexp.sss = sss(F15.modnegexp, F15.ids)
cutoff.modnegexp = max(yr[F15.modnegexp.sss < 0.85])

F15.modnegexp.crn = chron(detrend(F15[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F15")
plot(F15.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F15.mean = detrend(F15, method = "Mean")
rwi.stats(F15.mean, F15.ids, prewhiten = TRUE)

F15.mean.sss = sss(F15.mean, F15.ids)
cutoff.mean = max(yr[F15.mean.sss < 0.85])

F15.mean.crn = chron(detrend(F15[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F15")
plot(F15.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F15.ar = detrend(F15, method = "Ar")
rwi.stats(F15.ar, F15.ids, prewhiten = TRUE)

F15.ar.sss = sss(F15.ar, F15.ids)
cutoff.ar = max(yr[F15.ar.sss < 0.85])

F15.ar.crn = chron(detrend(F15[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F15")
F15.ar.crn = F15.ar.crn[-1,] # First row is NaN
plot(F15.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F15.friedman = detrend(F15, method = "Friedman")
rwi.stats(F15.friedman, F15.ids, prewhiten = TRUE)

F15.friedman.sss = sss(F15.friedman, F15.ids)
cutoff.friedman = max(yr[F15.friedman.sss < 0.85])

F15.friedman.crn = chron(detrend(F15[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F15")
plot(F15.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F15.modhuger = detrend(F15, method = "ModHugershoff")
rwi.stats(F15.modhuger, F15.ids, prewhiten = TRUE)

F15.modhuger.sss = sss(F15.modhuger, F15.ids)
cutoff.modhuger = max(yr[F15.modhuger.sss < 0.85])

F15.modhuger.crn = chron(detrend(F15[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F15")
plot(F15.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F15_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F15))), 
                        row.names = rownames(F15))
colnames(F15_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F15_Merged$NoDetrend[yr > cutoff] = F15.nodetrend.crn$F15std
F15_Merged$Spline[yr > cutoff.spline] = F15.spline.crn$F15std
F15_Merged$ModNegExp[yr > cutoff.modnegexp] = F15.modnegexp.crn$F15std
F15_Merged$Mean[yr > cutoff.mean] = F15.mean.crn$F15std
F15_Merged$Ar[yr > cutoff.ar + 1] = F15.ar.crn$F15std #First row is NaN
F15_Merged$Friedman[yr > cutoff.friedman] = F15.friedman.crn$F15std
F15_Merged$ModHugershoff[yr > cutoff.modhuger] = F15.modhuger.crn$F15std

F15_Merged$SampleDepth = F15.crn$samp.depth
#####
write.csv(F15_Merged, "F15_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F15.nodetrend.crn.spline = ffcsaps(F15.nodetrend.crn$F15std, nyrs = 10)
plot(F15.nodetrend.crn.spline)

#Spline#
F15.spline.crn.spline = ffcsaps(F15.spline.crn$F15std, nyrs = 10)
plot(F15.spline.crn.spline)

#ModNegExp#
F15.modnegexp.crn.spline = ffcsaps(F15.modnegexp.crn$F15std, nyrs = 10)
plot(F15.modnegexp.crn.spline)

#Mean#
F15.mean.crn.spline = ffcsaps(F15.mean.crn$F15std, nyrs = 10)
plot(F15.mean.crn.spline)

#Ar#
F15.ar.crn.spline = ffcsaps(F15.ar.crn$F15std, nyrs = 10)
plot(F15.ar.crn.spline)

#Friedman#
F15.friedman.crn.spline = ffcsaps(F15.friedman.crn$F15std, nyrs = 10)
plot(F15.friedman.crn.spline)

#ModHugershoff#
F15.modhuger.crn.spline = ffcsaps(F15.modhuger.crn$F15std, nyrs = 10)
plot(F15.modhuger.crn.spline)

###Merge into single dataset###
F15_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F15))), 
                        row.names = rownames(F15))
colnames(F15_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F15_Merged_Spline$NoDetrend[yr > cutoff] = F15.nodetrend.crn.spline
F15_Merged_Spline$Spline[yr > cutoff.spline] = F15.spline.crn.spline
F15_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F15.modnegexp.crn.spline
F15_Merged_Spline$Mean[yr > cutoff.mean] = F15.mean.crn.spline
F15_Merged_Spline$Ar[yr > cutoff.ar + 1] = F15.ar.crn.spline #First row is NaN
F15_Merged_Spline$Friedman[yr > cutoff.friedman] = F15.friedman.crn.spline
F15_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F15.modhuger.crn.spline

F15_Merged_Spline$SampleDepth = F15.crn$samp.depth
#####
write.csv(F15_Merged_Spline, "F15_Chronologies_Spline.csv")
