###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26") 
#Laptop

###RWLs###
#M26 = read.rwl('M26_Acer_1.rwl')
#M26 = read.rwl('M26_Deciduous_1.rwl')
M26 = read.rwl('M26_Thuja_1.rwl')
plot(M26, plot.type = "spag") #How to plot by age?
M26.ids = read.ids(M26, stc = c(4, 2, 1))
rwi.stats(M26, M26.ids, prewhiten = TRUE)
M26.crn = chron(M26, prefix = "M26") # Mean chronology
yr = time(M26)

M26.sss = sss(M26, M26.ids)
cutoff = max(yr[M26.sss < 0.85])

#Check for outliers
plot(M26.crn)

#####Noisy Values#####
#####
#No Detrending#
M26.nodetrend.crn = subset(M26.crn, yr > cutoff)
plot(M26.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
test = i.detrend(M26)

#Spline#
M26.spline = detrend(M26, method = "Spline")
rwi.stats(M26.spline, M26.ids, prewhiten = TRUE)

M26.spline.sss = sss(M26.spline, M26.ids)
cutoff.spline = max(yr[M26.spline.sss < 0.85])

M26.spline.crn = chron(detrend(M26[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M26")
plot(M26.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M26.modnegexp = detrend(M26, method = "ModNegExp")
rwi.stats(M26.modnegexp, M26.ids, prewhiten = TRUE)

M26.modnegexp.sss = sss(M26.modnegexp, M26.ids)
cutoff.modnegexp = max(yr[M26.modnegexp.sss < 0.85])

M26.modnegexp.crn = chron(detrend(M26[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M26")
plot(M26.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M26.mean = detrend(M26, method = "Mean")
rwi.stats(M26.mean, M26.ids, prewhiten = TRUE)

M26.mean.sss = sss(M26.mean, M26.ids)
cutoff.mean = max(yr[M26.mean.sss < 0.85])

M26.mean.crn = chron(detrend(M26[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M26")
plot(M26.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M26.ar = detrend(M26, method = "Ar")
rwi.stats(M26.ar, M26.ids, prewhiten = TRUE)

M26.ar.sss = sss(M26.ar, M26.ids)
cutoff.ar = max(yr[M26.ar.sss < 0.85])

M26.ar.crn = chron(detrend(M26[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M26")
M26.ar.crn = M26.ar.crn[-1,] # First rows NaN
plot(M26.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M26.friedman = detrend(M26, method = "Friedman")
rwi.stats(M26.friedman, M26.ids, prewhiten = TRUE)

M26.friedman.sss = sss(M26.friedman, M26.ids)
cutoff.friedman = max(yr[M26.friedman.sss < 0.85])

M26.friedman.crn = chron(detrend(M26[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M26")
plot(M26.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M26.modhuger = detrend(M26, method = "ModHugershoff")
rwi.stats(M26.modhuger, M26.ids, prewhiten = TRUE)

M26.modhuger.sss = sss(M26.modhuger, M26.ids)
cutoff.modhuger = max(yr[M26.modhuger.sss < 0.85])

M26.modhuger.crn = chron(detrend(M26[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M26")
plot(M26.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M26_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M26))), 
                        row.names = rownames(M26))
colnames(M26_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M26_Merged$NoDetrend[yr > cutoff] = M26.nodetrend.crn$M26std
M26_Merged$Spline[yr > cutoff.spline] = M26.spline.crn$M26std
M26_Merged$ModNegExp[yr > cutoff.modnegexp] = M26.modnegexp.crn$M26std
M26_Merged$Mean[yr > cutoff.mean] = M26.mean.crn$M26std
M26_Merged$Ar[yr > cutoff.ar + 3] = M26.ar.crn$M26std #First row is NaN
M26_Merged$Friedman[yr > cutoff.friedman] = M26.friedman.crn$M26std
M26_Merged$ModHugershoff[yr > cutoff.modhuger] = M26.modhuger.crn$M26std

M26_Merged$SampleDepth = M26.crn$samp.depth
#####
write.csv(M26_Merged, "M26_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M26.nodetrend.crn.spline = ffcsaps(M26.nodetrend.crn$M26std, nyrs = 10)
plot(M26.nodetrend.crn.spline)

#Spline#
M26.spline.crn.spline = ffcsaps(M26.spline.crn$M26std, nyrs = 10)
plot(M26.spline.crn.spline)

#ModNegExp#
M26.modnegexp.crn.spline = ffcsaps(M26.modnegexp.crn$M26std, nyrs = 10)
plot(M26.modnegexp.crn.spline)

#Mean#
M26.mean.crn.spline = ffcsaps(M26.mean.crn$M26std, nyrs = 10)
plot(M26.mean.crn.spline)

#Ar#
M26.ar.crn.spline = ffcsaps(M26.ar.crn$M26std, nyrs = 10)
plot(M26.ar.crn.spline)

#Friedman#
M26.friedman.crn.spline = ffcsaps(M26.friedman.crn$M26std, nyrs = 10)
plot(M26.friedman.crn.spline)

#ModHugershoff#
M26.modhuger.crn.spline = ffcsaps(M26.modhuger.crn$M26std, nyrs = 10)
plot(M26.modhuger.crn.spline)

###Merge into single dataset###
M26_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M26))), 
                        row.names = rownames(M26))
colnames(M26_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M26_Merged_Spline$NoDetrend[yr > cutoff] = M26.nodetrend.crn.spline
M26_Merged_Spline$Spline[yr > cutoff.spline] = M26.spline.crn.spline
M26_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M26.modnegexp.crn.spline
M26_Merged_Spline$Mean[yr > cutoff.mean] = M26.mean.crn.spline
M26_Merged_Spline$Ar[yr > cutoff.ar + 3] = M26.ar.crn.spline #First row is NaN
M26_Merged_Spline$Friedman[yr > cutoff.friedman] = M26.friedman.crn.spline
M26_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M26.modhuger.crn.spline

M26_Merged_Spline$SampleDepth = M26.crn$samp.depth
#####
write.csv(M26_Merged_Spline, "M26_Chronologies_Spline.csv")
