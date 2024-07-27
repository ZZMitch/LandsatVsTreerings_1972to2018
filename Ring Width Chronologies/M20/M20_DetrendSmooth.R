###Dendro Data###
library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20") 
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20") 
#Laptop

###RWLs###
M20 = read.rwl('M20_Acer_1.rwl')
#M20 = read.rwl('M20_Thuja_3.rwl')
plot(M20, plot.type = "spag") #How to plot by age?
M20.ids = read.ids(M20, stc = c(4, 2, 1))
rwi.stats(M20, M20.ids, prewhiten = TRUE)
M20.crn = chron(M20, prefix = "M20") # Mean chronology
yr = time(M20)

M20.sss = sss(M20, M20.ids)
cutoff = max(yr[M20.sss < 0.85])

#Check for outliers
plot(M20.crn)

#####Noisy Values#####
#####
#No Detrending#
M20.nodetrend.crn = subset(M20.crn, yr > cutoff)
plot(M20.nodetrend.crn, ylab = "TRW (mm)", add.spline = TRUE, nyrs = 10)

###Detrending###
#By Series test
#test = i.detrend(M20)

#Spline#
M20.spline = detrend(M20, method = "Spline")
rwi.stats(M20.spline, M20.ids, prewhiten = TRUE)

M20.spline.sss = sss(M20.spline, M20.ids)
cutoff.spline = max(yr[M20.spline.sss < 0.85])

M20.spline.crn = chron(detrend(M20[yr > cutoff.spline,],
                               method = "Spline"), prefix = "M20")
plot(M20.spline.crn, ylab = "RWI (Spline)", add.spline = TRUE, nyrs = 10)

#ModNegExp#
M20.modnegexp = detrend(M20, method = "ModNegExp")
rwi.stats(M20.modnegexp, M20.ids, prewhiten = TRUE)

M20.modnegexp.sss = sss(M20.modnegexp, M20.ids)
cutoff.modnegexp = max(yr[M20.modnegexp.sss < 0.85])

M20.modnegexp.crn = chron(detrend(M20[yr > cutoff.modnegexp,],
                                  method = "ModNegExp"), prefix = "M20")
plot(M20.modnegexp.crn, ylab = "RWI (ModNegExp)", add.spline = TRUE, nyrs = 10)

#Mean#
M20.mean = detrend(M20, method = "Mean")
rwi.stats(M20.mean, M20.ids, prewhiten = TRUE)

M20.mean.sss = sss(M20.mean, M20.ids)
cutoff.mean = max(yr[M20.mean.sss < 0.85])

M20.mean.crn = chron(detrend(M20[yr > cutoff.mean,],
                             method = "Mean"), prefix = "M20")
plot(M20.mean.crn, ylab = "RWI (Mean)", add.spline = TRUE, nyrs = 10)

#Ar#
M20.ar = detrend(M20, method = "Ar")
rwi.stats(M20.ar, M20.ids, prewhiten = TRUE)

M20.ar.sss = sss(M20.ar, M20.ids)
cutoff.ar = max(yr[M20.ar.sss < 0.85])

M20.ar.crn = chron(detrend(M20[yr > cutoff.ar,],
                           method = "Ar"), prefix = "M20")
M20.ar.crn = M20.ar.crn[-1,] # First row is NaN
plot(M20.ar.crn, ylab = "RWI (Ar)", add.spline = TRUE, nyrs = 10)

#Friedman#
M20.friedman = detrend(M20, method = "Friedman")
rwi.stats(M20.friedman, M20.ids, prewhiten = TRUE)

M20.friedman.sss = sss(M20.friedman, M20.ids)
cutoff.friedman = max(yr[M20.friedman.sss < 0.85])

M20.friedman.crn = chron(detrend(M20[yr > cutoff.friedman,],
                                 method = "Friedman"), prefix = "M20")
plot(M20.friedman.crn, ylab = "RWI (Friedman)", add.spline = TRUE, nyrs = 10)

#ModHugershoff#
M20.modhuger = detrend(M20, method = "ModHugershoff")
rwi.stats(M20.modhuger, M20.ids, prewhiten = TRUE)

M20.modhuger.sss = sss(M20.modhuger, M20.ids)
cutoff.modhuger = max(yr[M20.modhuger.sss < 0.85])

M20.modhuger.crn = chron(detrend(M20[yr > cutoff.modhuger,],
                                 method = "ModHugershoff"), prefix = "M20")
plot(M20.modhuger.crn, ylab = "RWI (ModHugershoff)", 
     add.spline = TRUE, nyrs = 10)

###Merge into single dataset###
M20_Merged = data.frame(matrix(ncol = 8, nrow = (nrow(M20))), 
                        row.names = rownames(M20))
colnames(M20_Merged) = c("NoDetrend", "Spline", "ModNegExp", "Mean", "Ar",
             "Friedman", "ModHugershoff", "SampleDepth")

M20_Merged$NoDetrend[yr > cutoff] = M20.nodetrend.crn$M20std
M20_Merged$Spline[yr > cutoff.spline] = M20.spline.crn$M20std
M20_Merged$ModNegExp[yr > cutoff.modnegexp] = M20.modnegexp.crn$M20std
M20_Merged$Mean[yr > cutoff.mean] = M20.mean.crn$M20std
M20_Merged$Ar[yr > cutoff.ar + 1] = M20.ar.crn$M20std #First row is NaN
M20_Merged$Friedman[yr > cutoff.friedman] = M20.friedman.crn$M20std
M20_Merged$ModHugershoff[yr > cutoff.modhuger] = M20.modhuger.crn$M20std

M20_Merged$SampleDepth = M20.crn$samp.depth
#####
write.csv(M20_Merged, "M20_Chronologies.csv")

#####Smoothed Values#####
#####
#No Detrending#
M20.nodetrend.crn.spline = ffcsaps(M20.nodetrend.crn$M20std, nyrs = 10)
plot(M20.nodetrend.crn.spline)

#Spline#
M20.spline.crn.spline = ffcsaps(M20.spline.crn$M20std, nyrs = 10)
plot(M20.spline.crn.spline)

#ModNegExp#
M20.modnegexp.crn.spline = ffcsaps(M20.modnegexp.crn$M20std, nyrs = 10)
plot(M20.modnegexp.crn.spline)

#Mean#
M20.mean.crn.spline = ffcsaps(M20.mean.crn$M20std, nyrs = 10)
plot(M20.mean.crn.spline)

#Ar#
M20.ar.crn.spline = ffcsaps(M20.ar.crn$M20std, nyrs = 10)
plot(M20.ar.crn.spline)

#Friedman#
M20.friedman.crn.spline = ffcsaps(M20.friedman.crn$M20std, nyrs = 10)
plot(M20.friedman.crn.spline)

#ModHugershoff#
M20.modhuger.crn.spline = ffcsaps(M20.modhuger.crn$M20std, nyrs = 10)
plot(M20.modhuger.crn.spline)

###Merge into single dataset###
M20_Merged_Spline = data.frame(matrix(ncol = 8, nrow = (nrow(M20))), 
                        row.names = rownames(M20))
colnames(M20_Merged_Spline) = c("NoDetrend", "Spline", "ModNegExp", "Mean", 
                                "Ar", "Friedman", "ModHugershoff", 
                                "SampleDepth")

M20_Merged_Spline$NoDetrend[yr > cutoff] = M20.nodetrend.crn.spline
M20_Merged_Spline$Spline[yr > cutoff.spline] = M20.spline.crn.spline
M20_Merged_Spline$ModNegExp[yr > cutoff.modnegexp] = M20.modnegexp.crn.spline
M20_Merged_Spline$Mean[yr > cutoff.mean] = M20.mean.crn.spline
M20_Merged_Spline$Ar[yr > cutoff.ar + 1] = M20.ar.crn.spline #First row is NaN
M20_Merged_Spline$Friedman[yr > cutoff.friedman] = M20.friedman.crn.spline
M20_Merged_Spline$ModHugershoff[yr > cutoff.modhuger] = M20.modhuger.crn.spline

M20_Merged_Spline$SampleDepth = M20.crn$samp.depth
#####
write.csv(M20_Merged_Spline, "M20_Chronologies_Spline.csv")
