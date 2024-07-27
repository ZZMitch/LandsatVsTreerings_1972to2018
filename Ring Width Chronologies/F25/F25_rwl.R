library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F25") 
#Update for each site

F25 = read.rwl('F25_5.rwl')
F25_ByStem = read.rwl('F25_ByStem_5.rwl')

# Descriptive Information #
dim(F25_ByStem) #Length of chronology, number of included series
colnames(F25_ByStem) #Series IDs
head(rownames(F25_ByStem)) #First few years
class(F25_ByStem) #Should be rwl and data.frame
rwl.report(F25_ByStem)
plot(F25_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F25_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

# Stand Level Cross Dating #
corr.rwl.seg(F25, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F25_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
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
Test_EPS <- strip.rwl(F25_ByStem, verbose = TRUE)
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
# None removed

###Check individual issues###
#F25-10
corr.series.seg(F25_ByStem, series = "F25-10", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-10b", seg.length = 20, bin.floor = 5)
# Bad before 1980
ccf.series.rwl(F25_ByStem, series = "F25-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-10b", seg.length = 20, bin.floor = 5)
# Check before 1970 - removed core b

#F25-21
corr.series.seg(F25_ByStem, series = "F25-21", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-21a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-21b", seg.length = 20, bin.floor = 5)
# Bad before 1965
ccf.series.rwl(F25_ByStem, series = "F25-21", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-21a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-21b", seg.length = 20, bin.floor = 5)
# No clear pattern before 1970 (maybe +1 lag?)

#F25-3
corr.series.seg(F25_ByStem, series = "F25-3", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-3a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-3b", seg.length = 20, bin.floor = 5)
# Bad before 1960/1965
ccf.series.rwl(F25_ByStem, series = "F25-3", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-3a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-3b", seg.length = 20, bin.floor = 5)
# Before 1960 (+1 lag?)

#F25-9
corr.series.seg(F25_ByStem, series = "F25-9", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-9b", seg.length = 20, bin.floor = 5)
# Bad after 1995/2000
ccf.series.rwl(F25_ByStem, series = "F25-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-9b", seg.length = 20, bin.floor = 5)
# Afer 1995 (-1, 4 lag?)

#F25-16
corr.series.seg(F25_ByStem, series = "F25-16", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-16a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-16b", seg.length = 20, bin.floor = 5)
# Bad after 1995
ccf.series.rwl(F25_ByStem, series = "F25-16", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-16a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-16b", seg.length = 20, bin.floor = 5)
# Afer 1995 (1, -2 lag?)

#F25-30
corr.series.seg(F25_ByStem, series = "F25-30", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-30a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-30b", seg.length = 20, bin.floor = 5)
# Core B just bad
ccf.series.rwl(F25_ByStem, series = "F25-30", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-30a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-30b", seg.length = 20, bin.floor = 5)
# Core B: clear -1 lag 1980 - 1999

#F25-26
corr.series.seg(F25_ByStem, series = "F25-26", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-26a", seg.length = 20, bin.floor = 5)
corr.series.seg(F25, series = "F25-26b", seg.length = 20, bin.floor = 5)
# Unclear
ccf.series.rwl(F25_ByStem, series = "F25-26", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-26a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F25, series = "F25-26b", seg.length = 20, bin.floor = 5)
# Unclear