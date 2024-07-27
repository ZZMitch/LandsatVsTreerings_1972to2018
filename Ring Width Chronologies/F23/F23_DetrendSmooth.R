###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23") 
#Laptop

###RWLs###
F23 = read.rwl('F23_7.rwl')
plot(F23, plot.type = "spag") #How to plot by age?
#F23 = read.rwl('F23_Thuja_3.rwl')
F23.ids = read.ids(F23, stc = c(4, 2, 1))
rwi.stats(F23, F23.ids, prewhiten = TRUE)
F23.crn = chron(F23, prefix = "F23") # Mean chronology
yr = time(F23)

F23.sss = sss(F23, F23.ids)
cutoff = max(yr[F23.sss < 0.85])

#Check for outliers
plot(F23.crn)

#####Noisy Values#####
#####
#No Detrending#
F23.nodetrend.crn = subset(F23.crn, yr > cutoff)
plot(F23.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(F23)
#test.crn = chron(test, prefix = "F23")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#Spline#
F23.spline = detrend(F23, method = "Spline")
rwi.stats(F23.spline, F23.ids, prewhiten = TRUE)

F23.spline.sss = sss(F23.spline, F23.ids)
cutoff.spline = max(yr[F23.spline.sss < 0.85])

F23.spline.crn = chron(detrend(F23[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F23")
plot(F23.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F23.modnegexp = detrend(F23, method = "ModNegExp")
rwi.stats(F23.modnegexp, F23.ids, prewhiten = TRUE)

F23.modnegexp.sss = sss(F23.modnegexp, F23.ids)
cutoff.modnegexp = max(yr[F23.modnegexp.sss < 0.85])

F23.modnegexp.crn = chron(detrend(F23[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F23")
plot(F23.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F23.mean = detrend(F23, method = "Mean")
rwi.stats(F23.mean, F23.ids, prewhiten = TRUE)

F23.mean.sss = sss(F23.mean, F23.ids)
cutoff.mean = max(yr[F23.mean.sss < 0.85])

F23.mean.crn = chron(detrend(F23[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F23")
plot(F23.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F23.ar = detrend(F23, method = "Ar")
rwi.stats(F23.ar, F23.ids, prewhiten = TRUE)

F23.ar.sss = sss(F23.ar, F23.ids)
cutoff.ar = max(yr[F23.ar.sss < 0.85])

F23.ar.crn = chron(detrend(F23[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F23")
F23.ar.crn = F23.ar.crn[-1,] # First row is NaN
plot(F23.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F23.friedman = detrend(F23, method = "Friedman")
rwi.stats(F23.friedman, F23.ids, prewhiten = TRUE)

F23.friedman.sss = sss(F23.friedman, F23.ids)
cutoff.friedman = max(yr[F23.friedman.sss < 0.85])

F23.friedman.crn = chron(detrend(F23[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F23")
plot(F23.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F23.modhuger = detrend(F23, method = "ModHugershoff")
rwi.stats(F23.modhuger, F23.ids, prewhiten = TRUE)

F23.modhuger.sss = sss(F23.modhuger, F23.ids)
cutoff.modhuger = max(yr[F23.modhuger.sss < 0.85])

F23.modhuger.crn = chron(detrend(F23[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F23")
plot(F23.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F23_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F23))), 
                        row.names = rownames(F23))
colnames(F23_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F23_Merged$NoDetrend[yr > cutoff] = F23.nodetrend.crn$F23std
F23_Merged$Spline[yr > cutoff.spline] = F23.spline.crn$F23std
F23_Merged$ModNegExp[yr > cutoff.modnegexp] = F23.modnegexp.crn$F23std
F23_Merged$Mean[yr > cutoff.mean] = F23.mean.crn$F23std
F23_Merged$Ar[yr > cutoff.ar + 1] = F23.ar.crn$F23std #First row is NaN
F23_Merged$Friedman[yr > cutoff.friedman] = F23.friedman.crn$F23std
F23_Merged$ModHugershoff[yr > cutoff.modhuger] = F23.modhuger.crn$F23std

F23_Merged$SampleDepth = F23.crn$samp.depth
#####
write.csv(F23_Merged, "F23_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F23.nodetrend.crn.spline = ffcsaps(F23.nodetrend.crn$F23std, nyrs = 10)
plot(F23.nodetrend.crn.spline)

#Spline#
F23.spline.crn.spline = ffcsaps(F23.spline.crn$F23std, nyrs = 10)
plot(F23.spline.crn.spline)

#ModNegExp#
F23.modnegexp.crn.spline = ffcsaps(F23.modnegexp.crn$F23std, nyrs = 10)
plot(F23.modnegexp.crn.spline)

#Mean#
F23.mean.crn.spline = ffcsaps(F23.mean.crn$F23std, nyrs = 10)
plot(F23.mean.crn.spline)

#Ar#
F23.ar.crn.spline = ffcsaps(F23.ar.crn$F23std, nyrs = 10)
plot(F23.ar.crn.spline)

#Friedman#
F23.friedman.crn.spline = ffcsaps(F23.friedman.crn$F23std, nyrs = 10)
plot(F23.friedman.crn.spline)

#ModHugershoff#
F23.modhuger.crn.spline = ffcsaps(F23.modhuger.crn$F23std, nyrs = 10)
plot(F23.modhuger.crn.spline)

###Merge into single dataset###
F23_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F23))), 
                        row.names = rownames(F23))
colnames(F23_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F23_Merged_Spline$NoDetrend[yr > cutoff] = F23.nodetrend.crn.spline
F23_Merged_Spline$Spline[yr > cutoff.spline] = F23.spline.crn.spline
F23_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F23.modnegexp.crn.spline
F23_Merged_Spline$Mean[yr > cutoff.mean] = F23.mean.crn.spline
F23_Merged_Spline$Ar[yr > cutoff.ar + 1] = F23.ar.crn.spline #First row is NaN
F23_Merged_Spline$Friedman[yr > cutoff.friedman] = F23.friedman.crn.spline
F23_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F23.modhuger.crn.spline

F23_Merged_Spline$SampleDepth = F23.crn$samp.depth
#####
write.csv(F23_Merged_Spline, "F23_Chronologies_Spline.csv")
