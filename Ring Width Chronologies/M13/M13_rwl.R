library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M13") #Update for each site
#Laptop

M13 <- read.rwl('M13.rwl') 
M13_ByStem <- read.rwl('M13_ByStem.rwl')
#No Revs

M13_Deciduous <- read.rwl('M13_Deciduous.rwl')
M13_ByStem_Deciduous <- read.rwl('M13_ByStem_Deciduous.rwl')

M13_Tsuga <- read.rwl('M13_Tsuga_1.rwl')
M13_ByStem_Tsuga <- read.rwl('M13_ByStem_Tsuga_1.rwl')

###Descriptive information###
dim(M13) #Length of chronology, number of included series
colnames(M13) #Series IDs
head(rownames(M13)) #First few years
class(M13) #Should be rwl and data.frame
rwl.report(M13)
plot(M13, plot.type = "spag")

dim(M13_ByStem) #Length of chronology, number of included series
colnames(M13_ByStem) #Series IDs
head(rownames(M13_ByStem)) #First few years
class(M13_ByStem) #Should be rwl and data.frame
rwl.report(M13_ByStem)
plot(M13_ByStem, plot.type = "spag")

dim(M13_ByStem_Deciduous) #Length of chronology, number of included series
colnames(M13_ByStem_Deciduous) #Series IDs
head(rownames(M13_ByStem_Deciduous)) #First few years
class(M13_ByStem_Deciduous) #Should be rwl and data.frame
rwl.report(M13_ByStem_Deciduous)
plot(M13_ByStem_Deciduous, plot.type = "spag")

dim(M13_ByStem_Tsuga) #Length of chronology, number of included series
colnames(M13_ByStem_Tsuga) #Series IDs
head(rownames(M13_ByStem_Tsuga)) #First few years
class(M13_ByStem_Tsuga) #Should be rwl and data.frame
rwl.report(M13_ByStem_Tsuga)
plot(M13_ByStem_Tsuga, plot.type = "spag")
rwlstats = rwl.stats(M13_ByStem_Tsuga)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


###Stand level crossdating###
corr.rwl.seg(M13, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M13_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M13_Deciduous, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M13_ByStem_Deciduous, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M13_Tsuga, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M13_ByStem_Tsuga, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Check individual issues###
#M13-2
corr.series.seg(M13_ByStem, series = "M13-2", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-2a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-2b", seg.length = 20, bin.floor = 5)
#Best (>0.4) before 1940
ccf.series.rwl(M13_ByStem, series = "M13-2", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-2a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-2b", seg.length = 20, bin.floor = 5)
#Not clear

#M13-6
corr.series.seg(M13_ByStem, series = "M13-6", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-6a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-6b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-6", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-6a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-6b", seg.length = 20, bin.floor = 5)
#Not clear

#M13-7
corr.series.seg(M13_ByStem, series = "M13-7", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-7a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-7b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-7", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-7a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-7b", seg.length = 20, bin.floor = 5)

#M13-8
corr.series.seg(M13_ByStem, series = "M13-8", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-8a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-8b", seg.length = 20, bin.floor = 5)
#Already pretty good...
ccf.series.rwl(M13_ByStem, series = "M13-8", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-8a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-8b", seg.length = 20, bin.floor = 5)

#M13-9
corr.series.seg(M13_ByStem, series = "M13-9", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-9b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-9b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-10
corr.series.seg(M13_ByStem, series = "M13-10", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-10b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-10b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-11
corr.series.seg(M13_ByStem, series = "M13-11", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-11a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-11b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-11", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-11a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-11b", seg.length = 20, bin.floor = 5)
#Already pretty good...

#M13-12
corr.series.seg(M13_ByStem, series = "M13-12", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-12a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-12b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-12", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-12a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-12b", seg.length = 20, bin.floor = 5)
#Not clear

#M13-13
corr.series.seg(M13_ByStem, series = "M13-13", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-13a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-13b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-13b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-20
corr.series.seg(M13_ByStem, series = "M13-20", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-20a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-20b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-20", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-20a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-20b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-21
corr.series.seg(M13_ByStem, series = "M13-21", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-21a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-21b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-21", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-21a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-21b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-23
corr.series.seg(M13_ByStem, series = "M13-23", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-23a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-23b", seg.length = 20, bin.floor = 5)
# Worst before 1975 (gets a bit better at beginning around 1920)
ccf.series.rwl(M13_ByStem, series = "M13-23", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-23a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-23b", seg.length = 20, bin.floor = 5)
# -3 year lag before ~1975, but does not hold true before 1944 or so
xskel.plot(M13_ByStem, series = "M13-23", win.start = 1950, win.end = 2010)
xskel.plot(M13, series = "M13-23a", win.start = 1950, win.end = 2010)
xskel.plot(M13, series = "M13-23b", win.start = 1950, win.end = 2010)
# Likely need to add a year between 1984 and 1988? Check early/mid-1980s closely. 

#M13-24
corr.series.seg(M13_ByStem, series = "M13-24", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-24a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-24b", seg.length = 20, bin.floor = 5)
#All bad, best most recently (0.2)
ccf.series.rwl(M13_ByStem, series = "M13-24", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-24a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-24b", seg.length = 20, bin.floor = 5)
#Not clear

#M13-26
corr.series.seg(M13_ByStem, series = "M13-26", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-26a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-26b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-26", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-26a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-26b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-27
corr.series.seg(M13_ByStem, series = "M13-27", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-27a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-27b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-27", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-27a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-27b", seg.length = 20, bin.floor = 5)
#Not clear

#M13-28
corr.series.seg(M13_ByStem, series = "M13-28", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13, series = "M13-28a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13, series = "M13-28b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem, series = "M13-28", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-28a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13, series = "M13-28b", seg.length = 20, bin.floor = 5)
#Noisy

#M13-18
corr.series.seg(M13_ByStem_Tsuga, series = "M13-18", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13_Tsuga, series = "M13-18a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13_Tsuga, series = "M13-18b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem_Tsuga, series = "M13-18", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13_Tsuga, series = "M13-18a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13_Tsuga, series = "M13-18b", seg.length = 20, bin.floor = 5)

#M13-18
corr.series.seg(M13_ByStem_Tsuga, series = "M13-19", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M13_Tsuga, series = "M13-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(M13_Tsuga, series = "M13-19b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M13_ByStem_Tsuga, series = "M13-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13_Tsuga, series = "M13-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M13_Tsuga, series = "M13-19b", seg.length = 20, bin.floor = 5)