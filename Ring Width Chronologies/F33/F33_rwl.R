library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F33") 
#Update for each site

F33 = read.rwl('F33_2.rwl')
F33_ByStem = read.rwl('F33_ByStem_2.rwl')

# Descriptive Information #
dim(F33_ByStem) #Length of chronology, number of included series
colnames(F33_ByStem) #Series IDs
head(rownames(F33_ByStem)) #First few years
class(F33_ByStem) #Should be rwl and data.frame
rwl.report(F33_ByStem)
plot(F33_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F33_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

# Stand Level Cross Dating #
corr.rwl.seg(F33, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F33_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
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
Test_EPS <- strip.rwl(F33_ByStem, verbose = TRUE)
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
#F33-10
corr.series.seg(F33_ByStem, series = "F33-10", seg.length = 20, bin.floor = 5)
corr.series.seg(F33, series = "F33-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(F33, series = "F33-10b", seg.length = 20, bin.floor = 5)
# Bad before 1975b(core B)
ccf.series.rwl(F33_ByStem, series = "F33-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F33, series = "F33-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F33, series = "F33-10b", seg.length = 20, bin.floor = 5)
# 2 negative lag? 

#F33-28
corr.series.seg(F33_ByStem, series = "F33-28", seg.length = 20, bin.floor = 5)
corr.series.seg(F33, series = "F33-28a", seg.length = 20, bin.floor = 5)
corr.series.seg(F33, series = "F33-28b", seg.length = 20, bin.floor = 5)
# Bad before 2000 (Core A)
ccf.series.rwl(F33_ByStem, series = "F33-28", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F33, series = "F33-28a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F33, series = "F33-28b", seg.length = 20, bin.floor = 5)
# -1 lag?

