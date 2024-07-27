library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F23") 
#Update for each site

F23 = read.rwl('F23_6.rwl')
F23_ByStem = read.rwl('F23_ByStem_6.rwl')

# Descriptive Information #
dim(F23_ByStem) #Length of chronology, number of included series
colnames(F23_ByStem) #Series IDs
head(rownames(F23_ByStem)) #First few years
class(F23_ByStem) #Should be rwl and data.frame
rwl.report(F23_ByStem)
plot(F23_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F23_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

# Stand Level Cross Dating #
corr.rwl.seg(F23, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F23_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master
#Bottom line in each series adheres to bottom timeline and top adheres to top
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(F23_ByStem, verbose = TRUE)
#Above 0.85 = good?

corr.rwl.seg(Test_EPS, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

dim(Test_EPS) #Length of chronology, number of included series
colnames(Test_EPS) #Series IDs
head(rownames(Test_EPS)) #First few years
class(Test_EPS) #Should be rwl and data.frame
rwl.report(Test_EPS)
plot(Test_EPS, plot.type = "spag")

###Check individual issues###
#F23-10
corr.series.seg(F23_ByStem, series = "F23-10", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-10b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-10b", seg.length = 20, bin.floor = 5)

#F23-11
corr.series.seg(F23_ByStem, series = "F23-11", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-11a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-11b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-11", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-11a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-11b", seg.length = 20, bin.floor = 5)

#F23-19
corr.series.seg(F23_ByStem, series = "F23-19", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-19b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-19b", seg.length = 20, bin.floor = 5)

#F23-21
corr.series.seg(F23_ByStem, series = "F23-21", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-21a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-21b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-21", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-21a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-21b", seg.length = 20, bin.floor = 5)

#F23-1
corr.series.seg(F23_ByStem, series = "F23-1", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-1a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-1b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-1", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-1a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-1b", seg.length = 20, bin.floor = 5)

#F23-22
corr.series.seg(F23_ByStem, series = "F23-22", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-22a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-22b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-22", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-22a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-22b", seg.length = 20, bin.floor = 5)

#F23-14
corr.series.seg(F23_ByStem, series = "F23-14", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-14a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-14b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-14", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-14a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-14b", seg.length = 20, bin.floor = 5)

#F23-8
corr.series.seg(F23_ByStem, series = "F23-8", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-8a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-8b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-8", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-8a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-8b", seg.length = 20, bin.floor = 5)

#F23-5
corr.series.seg(F23_ByStem, series = "F23-5", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-5a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-5b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-5", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-5a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-5b", seg.length = 20, bin.floor = 5)

#F23-17
corr.series.seg(F23_ByStem, series = "F23-17b", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-17b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-17b", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-17b", seg.length = 20, bin.floor = 5)

#F23-5
corr.series.seg(F23_ByStem, series = "F23-25", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-25a", seg.length = 20, bin.floor = 5)
corr.series.seg(F23, series = "F23-25b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F23_ByStem, series = "F23-25", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-25a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F23, series = "F23-25b", seg.length = 20, bin.floor = 5)
