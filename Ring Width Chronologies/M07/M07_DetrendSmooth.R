###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07") 
#Laptop

###RWLs###
#M07 = read.rwl('M07_1.rwl')
M07 = read.rwl('M07_Tsuga_2.rwl')
plot(M07, plot.type = "spag") #How to plot by age?
M07.ids = read.ids(M07, stc = c(4, 2, 1))
rwi.stats(M07, M07.ids, prewhiten = TRUE)
M07.crn = chron(M07, prefix = "M07") # Mean chronology
yr = time(M07)

M07.sss = sss(M07, M07.ids)
cutoff = max(yr[M07.sss < 0.85])

#Check for outliers
plot(M07.crn)

#####Noisy Values#####
#####
#No Detrending#
M07.nodetrend.crn = subset(M07.crn, yr > cutoff)
plot(M07.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(M07)
#test.crn = chron(test, prefix = "M07")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)
#plot(test.crn$M07std[8:86], M07.mean.crn$M07std)

#Spline#
M07.spline = detrend(M07, method = "Spline")
rwi.stats(M07.spline, M07.ids, prewhiten = TRUE)

M07.spline.sss = sss(M07.spline, M07.ids)
cutoff.spline = max(yr[M07.spline.sss < 0.85])

M07.spline.crn = chron(detrend(M07[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M07")
plot(M07.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M07.modnegexp = detrend(M07, method = "ModNegExp")
rwi.stats(M07.modnegexp, M07.ids, prewhiten = TRUE)

M07.modnegexp.sss = sss(M07.modnegexp, M07.ids)
cutoff.modnegexp = max(yr[M07.modnegexp.sss < 0.85])

M07.modnegexp.crn = chron(detrend(M07[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M07")
plot(M07.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M07.mean = detrend(M07, method = "Mean")
rwi.stats(M07.mean, M07.ids, prewhiten = TRUE)

M07.mean.sss = sss(M07.mean, M07.ids)
cutoff.mean = max(yr[M07.mean.sss < 0.85])

M07.mean.crn = chron(detrend(M07[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M07")
plot(M07.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M07.ar = detrend(M07, method = "Ar")
rwi.stats(M07.ar, M07.ids, prewhiten = TRUE)

M07.ar.sss = sss(M07.ar, M07.ids)
cutoff.ar = max(yr[M07.ar.sss < 0.85])

M07.ar.crn = chron(detrend(M07[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M07")
M07.ar.crn = M07.ar.crn[-1,] # First row is NaN
plot(M07.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M07.friedman = detrend(M07, method = "Friedman")
rwi.stats(M07.friedman, M07.ids, prewhiten = TRUE)

M07.friedman.sss = sss(M07.friedman, M07.ids)
cutoff.friedman = max(yr[M07.friedman.sss < 0.85])

M07.friedman.crn = chron(detrend(M07[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M07")
plot(M07.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M07.modhuger = detrend(M07, method = "ModHugershoff")
rwi.stats(M07.modhuger, M07.ids, prewhiten = TRUE)

M07.modhuger.sss = sss(M07.modhuger, M07.ids)
cutoff.modhuger = max(yr[M07.modhuger.sss < 0.85])

M07.modhuger.crn = chron(detrend(M07[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M07")
plot(M07.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M07_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M07))), 
                        row.names = rownames(M07))
colnames(M07_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M07_Merged$NoDetrend[yr > cutoff] = M07.nodetrend.crn$M07std
M07_Merged$Spline[yr > cutoff.spline] = M07.spline.crn$M07std
M07_Merged$ModNegExp[yr > cutoff.modnegexp] = M07.modnegexp.crn$M07std
M07_Merged$Mean[yr > cutoff.mean] = M07.mean.crn$M07std
M07_Merged$Ar[yr > cutoff.ar + 1] = M07.ar.crn$M07std #First row is NaN
M07_Merged$Friedman[yr > cutoff.friedman] = M07.friedman.crn$M07std
M07_Merged$ModHugershoff[yr > cutoff.modhuger] = M07.modhuger.crn$M07std

M07_Merged$SampleDepth = M07.crn$samp.depth
#####
write.csv(M07_Merged, "M07_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M07.nodetrend.crn.spline = ffcsaps(M07.nodetrend.crn$M07std, nyrs = 10)
plot(M07.nodetrend.crn.spline)

#Spline#
M07.spline.crn.spline = ffcsaps(M07.spline.crn$M07std, nyrs = 10)
plot(M07.spline.crn.spline)

#ModNegExp#
M07.modnegexp.crn.spline = ffcsaps(M07.modnegexp.crn$M07std, nyrs = 10)
plot(M07.modnegexp.crn.spline)

#Mean#
M07.mean.crn.spline = ffcsaps(M07.mean.crn$M07std, nyrs = 10)
plot(M07.mean.crn.spline)

#Ar#
M07.ar.crn.spline = ffcsaps(M07.ar.crn$M07std, nyrs = 10)
plot(M07.ar.crn.spline)

#Friedman#
M07.friedman.crn.spline = ffcsaps(M07.friedman.crn$M07std, nyrs = 10)
plot(M07.friedman.crn.spline)

#ModHugershoff#
M07.modhuger.crn.spline = ffcsaps(M07.modhuger.crn$M07std, nyrs = 10)
plot(M07.modhuger.crn.spline)

###Merge into single dataset###
M07_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M07))), 
                        row.names = rownames(M07))
colnames(M07_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M07_Merged_Spline$NoDetrend[yr > cutoff] = M07.nodetrend.crn.spline
M07_Merged_Spline$Spline[yr > cutoff.spline] = M07.spline.crn.spline
M07_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M07.modnegexp.crn.spline
M07_Merged_Spline$Mean[yr > cutoff.mean] = M07.mean.crn.spline
M07_Merged_Spline$Ar[yr > cutoff.ar + 1] = M07.ar.crn.spline #First row is NaN
M07_Merged_Spline$Friedman[yr > cutoff.friedman] = M07.friedman.crn.spline
M07_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M07.modhuger.crn.spline

M07_Merged_Spline$SampleDepth = M07.crn$samp.depth
#####
write.csv(M07_Merged_Spline, "M07_Chronologies_Spline.csv")
