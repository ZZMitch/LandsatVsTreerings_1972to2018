###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M05") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M05") 
#Laptop

###RWLs###
M05 = read.rwl('M05_6.rwl')
plot(M05, plot.type = "spag") #How to plot by age?
M05.ids = read.ids(M05, stc = c(4, 2, 1))
rwi.stats(M05, M05.ids, prewhiten = TRUE)
M05.crn = chron(M05, prefix = "M05") # Mean chronology
yr = time(M05)

M05.sss = sss(M05, M05.ids)
cutoff = max(yr[M05.sss < 0.85])

#Check for outliers
plot(M05.crn)

#####Noisy Values#####
#####
#No Detrending#
M05.nodetrend.crn = subset(M05.crn, yr > cutoff)
plot(M05.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(M05)
#test.crn = chron(test, prefix = "M05")
#plot(test.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)
#plot(test.crn$M05std[8:86], M05.mean.crn$M05std)

#Spline#
M05.spline = detrend(M05, method = "Spline")
rwi.stats(M05.spline, M05.ids, prewhiten = TRUE)

M05.spline.sss = sss(M05.spline, M05.ids)
cutoff.spline = max(yr[M05.spline.sss < 0.85])

M05.spline.crn = chron(detrend(M05[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M05")
plot(M05.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M05.modnegexp = detrend(M05, method = "ModNegExp")
rwi.stats(M05.modnegexp, M05.ids, prewhiten = TRUE)

M05.modnegexp.sss = sss(M05.modnegexp, M05.ids)
cutoff.modnegexp = max(yr[M05.modnegexp.sss < 0.85])

M05.modnegexp.crn = chron(detrend(M05[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M05")
plot(M05.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M05.mean = detrend(M05, method = "Mean")
rwi.stats(M05.mean, M05.ids, prewhiten = TRUE)

M05.mean.sss = sss(M05.mean, M05.ids)
cutoff.mean = max(yr[M05.mean.sss < 0.85])

M05.mean.crn = chron(detrend(M05[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M05")
plot(M05.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M05.ar = detrend(M05, method = "Ar")
rwi.stats(M05.ar, M05.ids, prewhiten = TRUE)

M05.ar.sss = sss(M05.ar, M05.ids)
cutoff.ar = max(yr[M05.ar.sss < 0.85])

M05.ar.crn = chron(detrend(M05[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M05")
M05.ar.crn = M05.ar.crn[-1,] # First row is NaN
plot(M05.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M05.friedman = detrend(M05, method = "Friedman")
rwi.stats(M05.friedman, M05.ids, prewhiten = TRUE)

M05.friedman.sss = sss(M05.friedman, M05.ids)
cutoff.friedman = max(yr[M05.friedman.sss < 0.85])

M05.friedman.crn = chron(detrend(M05[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M05")
plot(M05.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M05.modhuger = detrend(M05, method = "ModHugershoff")
rwi.stats(M05.modhuger, M05.ids, prewhiten = TRUE)

M05.modhuger.sss = sss(M05.modhuger, M05.ids)
cutoff.modhuger = max(yr[M05.modhuger.sss < 0.85])

M05.modhuger.crn = chron(detrend(M05[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M05")
plot(M05.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M05_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M05))), 
                        row.names = rownames(M05))
colnames(M05_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M05_Merged$NoDetrend[yr > cutoff] = M05.nodetrend.crn$M05std
M05_Merged$Spline[yr > cutoff.spline] = M05.spline.crn$M05std
M05_Merged$ModNegExp[yr > cutoff.modnegexp] = M05.modnegexp.crn$M05std
M05_Merged$Mean[yr > cutoff.mean] = M05.mean.crn$M05std
M05_Merged$Ar[yr > cutoff.ar + 1] = M05.ar.crn$M05std #First row is NaN
M05_Merged$Friedman[yr > cutoff.friedman] = M05.friedman.crn$M05std
M05_Merged$ModHugershoff[yr > cutoff.modhuger] = M05.modhuger.crn$M05std

M05_Merged$SampleDepth = M05.crn$samp.depth
#####
write.csv(M05_Merged, "M05_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M05.nodetrend.crn.spline = ffcsaps(M05.nodetrend.crn$M05std, nyrs = 10)
plot(M05.nodetrend.crn.spline)

#Spline#
M05.spline.crn.spline = ffcsaps(M05.spline.crn$M05std, nyrs = 10)
plot(M05.spline.crn.spline)

#ModNegExp#
M05.modnegexp.crn.spline = ffcsaps(M05.modnegexp.crn$M05std, nyrs = 10)
plot(M05.modnegexp.crn.spline)

#Mean#
M05.mean.crn.spline = ffcsaps(M05.mean.crn$M05std, nyrs = 10)
plot(M05.mean.crn.spline)

#Ar#
M05.ar.crn.spline = ffcsaps(M05.ar.crn$M05std, nyrs = 10)
plot(M05.ar.crn.spline)

#Friedman#
M05.friedman.crn.spline = ffcsaps(M05.friedman.crn$M05std, nyrs = 10)
plot(M05.friedman.crn.spline)

#ModHugershoff#
M05.modhuger.crn.spline = ffcsaps(M05.modhuger.crn$M05std, nyrs = 10)
plot(M05.modhuger.crn.spline)

###Merge into single dataset###
M05_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M05))), 
                        row.names = rownames(M05))
colnames(M05_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M05_Merged_Spline$NoDetrend[yr > cutoff] = M05.nodetrend.crn.spline
M05_Merged_Spline$Spline[yr > cutoff.spline] = M05.spline.crn.spline
M05_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M05.modnegexp.crn.spline
M05_Merged_Spline$Mean[yr > cutoff.mean] = M05.mean.crn.spline
M05_Merged_Spline$Ar[yr > cutoff.ar + 1] = M05.ar.crn.spline #First row is NaN
M05_Merged_Spline$Friedman[yr > cutoff.friedman] = M05.friedman.crn.spline
M05_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M05.modhuger.crn.spline

M05_Merged_Spline$SampleDepth = M05.crn$samp.depth
#####
write.csv(M05_Merged_Spline, "M05_Chronologies_Spline.csv")
