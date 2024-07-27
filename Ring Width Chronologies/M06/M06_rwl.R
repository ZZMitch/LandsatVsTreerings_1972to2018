library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M06") #Update for each site
#Laptop

#M06 <- read.rwl('M06.rwl') 
#M06_ByStem <- read.rwl('M06_ByStem.rwl')
#No Revs

M06_Quercus <- read.rwl('M06_Quercus.rwl')
M06_ByStem_Quercus <- read.rwl('M06_ByStem_Quercus.rwl')
#No Revs

M06_Acer <- read.rwl('M06_Acer.rwl')
M06_ByStem_Acer <- read.rwl('M06_ByStem_Acer.rwl')
#No Revs

###Descriptive information###
#dim(M06_ByStem) #Length of chronology, number of included series
#colnames(M06_ByStem) #Series IDs
#head(rownames(M06_ByStem)) #First few years
#class(M06_ByStem) #Should be rwl and data.frame
#rwl.report(M06_ByStem)
#plot(M06_ByStem, plot.type = "spag")

dim(M06_ByStem_Quercus) #Length of chronology, number of included series
colnames(M06_ByStem_Quercus) #Series IDs
head(rownames(M06_ByStem_Quercus)) #First few years
class(M06_ByStem_Quercus) #Should be rwl and data.frame
rwl.report(M06_ByStem_Quercus)
plot(M06_ByStem_Quercus, plot.type = "spag")
rwlstats = rwl.stats(M06_ByStem_Quercus)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(M06_ByStem_Acer) #Length of chronology, number of included series
colnames(M06_ByStem_Acer) #Series IDs
head(rownames(M06_ByStem_Acer)) #First few years
class(M06_ByStem_Acer) #Should be rwl and data.frame
rwl.report(M06_ByStem_Acer)
plot(M06_ByStem_Acer, plot.type = "spag")
rwlstats = rwl.stats(M06_ByStem_Acer)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

###Stand level crossdating###
#corr.rwl.seg(M06, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
#             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#corr.rwl.seg(M06_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
#             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M06_Quercus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M06_ByStem_Quercus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M06_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M06_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(M06_Acer, verbose = TRUE)
#Above 0.85 = good?

corr.rwl.seg(Test_EPS, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

dim(Test_EPS) #Length of chronology, number of included series
colnames(Test_EPS) #Series IDs
head(rownames(Test_EPS)) #First few years
class(Test_EPS) #Should be rwl and data.frame
rwl.report(Test_EPS)
plot(Test_EPS, plot.type = "spag")

###Check individual issues###
#M06-3
corr.series.seg(M06_ByStem_Quercus, series = "M06-3", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Quercus, series = "M06-3a", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Quercus, series = "M06-3b", seg.length = 20, bin.floor = 5)
#
ccf.series.rwl(M06_ByStem_Quercus, series = "M06-3", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Quercus, series = "M06-3a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Quercus, series = "M06-3b", seg.length = 20, bin.floor = 5)
#Check 3b before 1944

#M06-23
corr.series.seg(M06_ByStem_Quercus, series = "M06-23", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Quercus, series = "M06-23a", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Quercus, series = "M06-23b", seg.length = 20, bin.floor = 5)
#
ccf.series.rwl(M06_ByStem_Quercus, series = "M06-23", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Quercus, series = "M06-23a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Quercus, series = "M06-23b", seg.length = 20, bin.floor = 5)

#M06-4
corr.series.seg(M06_ByStem_Acer, series = "M06-4", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-4a", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-4b", seg.length = 20, bin.floor = 5)
#
ccf.series.rwl(M06_ByStem_Acer, series = "M06-4", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-4a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-4b", seg.length = 20, bin.floor = 5)
# Good enough

#M06_17
corr.series.seg(M06_ByStem_Acer, series = "M06-17", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-17a", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-17b", seg.length = 20, bin.floor = 5)
#
ccf.series.rwl(M06_ByStem_Acer, series = "M06-17", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-17a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-17b", seg.length = 20, bin.floor = 5)
# Not clear before 1940

#M06-26
corr.series.seg(M06_ByStem_Acer, series = "M06-26", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-26a", seg.length = 20, bin.floor = 5)
corr.series.seg(M06_Acer, series = "M06-26b", seg.length = 20, bin.floor = 5)
# Bad before 1915
ccf.series.rwl(M06_ByStem_Acer, series = "M06-26", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-26a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M06_Acer, series = "M06-26b", seg.length = 20, bin.floor = 5)
# Not clear beefore 1920
