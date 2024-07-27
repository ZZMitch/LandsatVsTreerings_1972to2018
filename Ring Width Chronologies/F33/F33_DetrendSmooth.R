###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F33") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F33") 
#Laptop

###RWLs###
F33 = read.rwl('F33_3.rwl')
plot(F33, plot.type = "spag") #How to plot by age?
F33.ids = read.ids(F33, stc = c(4, 2, 1))
rwi.stats(F33, F33.ids, prewhiten = TRUE)
F33.crn = chron(F33, prefix = "F33") # Mean chronology
yr = time(F33)

F33.sss = sss(F33, F33.ids)
cutoff = max(yr[F33.sss < 0.85])

#Check for outliers
plot(F33.crn)

#####Noisy Values#####
#####
#No Detrending#
F33.nodetrend.crn = subset(F33.crn, yr > cutoff)
plot(F33.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(F33)

#Spline#
F33.spline = detrend(F33, method = "Spline")
rwi.stats(F33.spline, F33.ids, prewhiten = TRUE)

F33.spline.sss = sss(F33.spline, F33.ids)
cutoff.spline = max(yr[F33.spline.sss < 0.85])

F33.spline.crn = chron(detrend(F33[yr > cutoff.spline,],
                               method = "Spline"), prefix = "F33")
plot(F33.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
F33.modnegexp = detrend(F33, method = "ModNegExp")
rwi.stats(F33.modnegexp, F33.ids, prewhiten = TRUE)

F33.modnegexp.sss = sss(F33.modnegexp, F33.ids)
cutoff.modnegexp = max(yr[F33.modnegexp.sss < 0.85])

F33.modnegexp.crn = chron(detrend(F33[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "F33")
plot(F33.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
F33.mean = detrend(F33, method = "Mean")
rwi.stats(F33.mean, F33.ids, prewhiten = TRUE)

F33.mean.sss = sss(F33.mean, F33.ids)
cutoff.mean = max(yr[F33.mean.sss < 0.85])

F33.mean.crn = chron(detrend(F33[yr > cutoff.mean,],
                             method = "Mean"), prefix = "F33")
plot(F33.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
F33.ar = detrend(F33, method = "Ar")
rwi.stats(F33.ar, F33.ids, prewhiten = TRUE)

F33.ar.sss = sss(F33.ar, F33.ids)
cutoff.ar = max(yr[F33.ar.sss < 0.85])

F33.ar.crn = chron(detrend(F33[yr > cutoff.ar,],
                           method = "Ar"), prefix = "F33")
F33.ar.crn = F33.ar.crn[-1,] # First row is NaN
plot(F33.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
F33.friedman = detrend(F33, method = "Friedman")
rwi.stats(F33.friedman, F33.ids, prewhiten = TRUE)

F33.friedman.sss = sss(F33.friedman, F33.ids)
cutoff.friedman = max(yr[F33.friedman.sss < 0.85])

F33.friedman.crn = chron(detrend(F33[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "F33")
plot(F33.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
F33.modhuger = detrend(F33, method = "ModHugershoff")
rwi.stats(F33.modhuger, F33.ids, prewhiten = TRUE)

F33.modhuger.sss = sss(F33.modhuger, F33.ids)
cutoff.modhuger = max(yr[F33.modhuger.sss < 0.85])

F33.modhuger.crn = chron(detrend(F33[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "F33")
plot(F33.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
F33_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(F33))), 
                        row.names = rownames(F33))
colnames(F33_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

F33_Merged$NoDetrend[yr > cutoff] = F33.nodetrend.crn$F33std
F33_Merged$Spline[yr > cutoff.spline] = F33.spline.crn$F33std
F33_Merged$ModNegExp[yr > cutoff.modnegexp] = F33.modnegexp.crn$F33std
F33_Merged$Mean[yr > cutoff.mean] = F33.mean.crn$F33std
F33_Merged$Ar[yr > cutoff.ar + 1] = F33.ar.crn$F33std #First row is NaN
F33_Merged$Friedman[yr > cutoff.friedman] = F33.friedman.crn$F33std
F33_Merged$ModHugershoff[yr > cutoff.modhuger] = F33.modhuger.crn$F33std

F33_Merged$SampleDepth = F33.crn$samp.depth
#####
write.csv(F33_Merged, "F33_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
F33.nodetrend.crn.spline = ffcsaps(F33.nodetrend.crn$F33std, nyrs = 10)
plot(F33.nodetrend.crn.spline)

#Spline#
F33.spline.crn.spline = ffcsaps(F33.spline.crn$F33std, nyrs = 10)
plot(F33.spline.crn.spline)

#ModNegExp#
F33.modnegexp.crn.spline = ffcsaps(F33.modnegexp.crn$F33std, nyrs = 10)
plot(F33.modnegexp.crn.spline)

#Mean#
F33.mean.crn.spline = ffcsaps(F33.mean.crn$F33std, nyrs = 10)
plot(F33.mean.crn.spline)

#Ar#
F33.ar.crn.spline = ffcsaps(F33.ar.crn$F33std, nyrs = 10)
plot(F33.ar.crn.spline)

#Friedman#
F33.friedman.crn.spline = ffcsaps(F33.friedman.crn$F33std, nyrs = 10)
plot(F33.friedman.crn.spline)

#ModHugershoff#
F33.modhuger.crn.spline = ffcsaps(F33.modhuger.crn$F33std, nyrs = 10)
plot(F33.modhuger.crn.spline)

###Merge into single dataset###
F33_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(F33))), 
                        row.names = rownames(F33))
colnames(F33_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

F33_Merged_Spline$NoDetrend[yr > cutoff] = F33.nodetrend.crn.spline
F33_Merged_Spline$Spline[yr > cutoff.spline] = F33.spline.crn.spline
F33_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = F33.modnegexp.crn.spline
F33_Merged_Spline$Mean[yr > cutoff.mean] = F33.mean.crn.spline
F33_Merged_Spline$Ar[yr > cutoff.ar + 1] = F33.ar.crn.spline #First row is NaN
F33_Merged_Spline$Friedman[yr > cutoff.friedman] = F33.friedman.crn.spline
F33_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = F33.modhuger.crn.spline

F33_Merged_Spline$SampleDepth = F33.crn$samp.depth
#####
write.csv(F33_Merged_Spline, "F33_Chronologies_Spline.csv")
