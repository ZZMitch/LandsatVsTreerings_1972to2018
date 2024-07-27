library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M05") 
#Update for each site

M05 = read.rwl('M05_5.rwl')
M05_ByStem = read.rwl('M05_ByStem_5.rwl')

# Descriptive Information #
dim(M05_ByStem) #Length of chronology, number of included series
colnames(M05_ByStem) #Series IDs
head(rownames(M05_ByStem)) #First few years
class(M05_ByStem) #Should be rwl and data.frame
rwl.report(M05_ByStem)
plot(M05_ByStem, plot.type = "spag")
rwlstats = rwl.stats(M05_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

# Stand Level Cross Dating #
corr.rwl.seg(M05, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(M05_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
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
Test_EPS <- strip.rwl(M05_ByStem, verbose = TRUE)
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
# M05-11 removed

###Check individual issues###
#M05-11
corr.series.seg(M05_ByStem, series = "M05-11", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-11a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-11b", seg.length = 20, bin.floor = 5)
# Bad before 1955 (Core A), 
ccf.series.rwl(M05_ByStem, series = "M05-11", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-11a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-11b", seg.length = 20, bin.floor = 5)
# 1 lag before 1974? Core A/B
# Removed sketchy early rings in Core A (wavy)

#M05-18
corr.series.seg(M05_ByStem, series = "M05-18", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-18a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-18b", seg.length = 20, bin.floor = 5)
# Core A: 1985-2005, before 1945; Core B: After 1955, before 1930
ccf.series.rwl(M05_ByStem, series = "M05-18", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-18a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-18b", seg.length = 20, bin.floor = 5)
# Sort of bad throughout
# Removed sketchy early rings in Core A (wavy)

#M05-8
corr.series.seg(M05_ByStem, series = "M05-8", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-8a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-8b", seg.length = 20, bin.floor = 5)
# Core A: before 1965, before 1945
ccf.series.rwl(M05_ByStem, series = "M05-8", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-8a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-8b", seg.length = 20, bin.floor = 5)
# Core A: check before 1969

#M05-30
corr.series.seg(M05_ByStem, series = "M05-30", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-30a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-30b", seg.length = 20, bin.floor = 5)
# Core A: after 2000, Core B: before 1980
ccf.series.rwl(M05_ByStem, series = "M05-30", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-30a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-30b", seg.length = 20, bin.floor = 5)
# 3 lag?

#M05-12
corr.series.seg(M05_ByStem, series = "M05-12", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-12a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-12b", seg.length = 20, bin.floor = 5)
# Core B: before 1975
ccf.series.rwl(M05_ByStem, series = "M05-12", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-12a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-12b", seg.length = 20, bin.floor = 5)
# Before 1965: 1 lag?

#M05-9
corr.series.seg(M05_ByStem, series = "M05-9", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-9b", seg.length = 20, bin.floor = 5)
# Bad in middle years (1970-1990)
ccf.series.rwl(M05_ByStem, series = "M05-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-9b", seg.length = 20, bin.floor = 5)

#M05-13
corr.series.seg(M05_ByStem, series = "M05-13", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-13a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-13b", seg.length = 20, bin.floor = 5)
# Core A: before 1965
ccf.series.rwl(M05_ByStem, series = "M05-13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-13b", seg.length = 20, bin.floor = 5)
# Core A: 1950-1969

#M05-14
corr.series.seg(M05_ByStem, series = "M05-14", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-14a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-14b", seg.length = 20, bin.floor = 5)
# Core A: before 1965, Core B: before 2000
ccf.series.rwl(M05_ByStem, series = "M05-14", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-14a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-14b", seg.length = 20, bin.floor = 5)
# Core B: before 1974

#M05-7
corr.series.seg(M05_ByStem, series = "M05-7", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-7a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-7b", seg.length = 20, bin.floor = 5)
# Core A: 1960-1980, B: 1950-1975
ccf.series.rwl(M05_ByStem, series = "M05-7", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-7a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-7b", seg.length = 20, bin.floor = 5)
# Core A: b1945-1984, B: 1955-1984

#M05-3
corr.series.seg(M05_ByStem, series = "M05-3", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-3a", seg.length = 20, bin.floor = 5)
corr.series.seg(M05, series = "M05-3b", seg.length = 20, bin.floor = 5)
# Core A: before 1975, B: before 1950
ccf.series.rwl(M05_ByStem, series = "M05-3", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-3a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-3b", seg.length = 20, bin.floor = 5)

#M05-24
corr.series.seg(M05_ByStem, series = "M05-24", seg.length = 10, bin.floor = 5)
corr.series.seg(M05, series = "M05-24a", seg.length = 10, bin.floor = 5)
corr.series.seg(M05, series = "M05-24b", seg.length = 10, bin.floor = 5)
# Core A: all bad, B: bad before 2005
ccf.series.rwl(M05_ByStem, series = "M05-24", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-24a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-24b", seg.length = 10, bin.floor = 5)
# Core A: b1945-1984, B: 1955-1984

#M05-19
corr.series.seg(M05_ByStem, series = "M05-19", seg.length = 10, bin.floor = 5)
corr.series.seg(M05, series = "M05-19a", seg.length = 10, bin.floor = 5)
corr.series.seg(M05, series = "M05-19b", seg.length = 10, bin.floor = 5)
# Core A: bad before 2000, B: after 2005, before 2000
ccf.series.rwl(M05_ByStem, series = "M05-19", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-19a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M05, series = "M05-19b", seg.length = 10, bin.floor = 5)
# B: 1 lag? 
