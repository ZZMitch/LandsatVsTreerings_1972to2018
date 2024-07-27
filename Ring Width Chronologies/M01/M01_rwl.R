library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M01") #Update for each site
#Laptop

M01 <- read.rwl('M01_10.rwl') 
M01_ByStem <- read.rwl('M01_ByStem_10.rwl')
#Rev 1: Removed 15, 2b, 20a, 19b, 7b; Fixed 16b

###Descriptive information###
dim(M01_ByStem) #Length of chronology, number of included series
colnames(M01_ByStem) #Series IDs
head(rownames(M01_ByStem)) #First few years
class(M01_ByStem) #Should be rwl and data.frame
rwl.report(M01_ByStem)
plot(M01_ByStem, plot.type = "spag")
rwlstats = rwl.stats(M01_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


###Stand level crossdating###
corr.rwl.seg(M01, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M01_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(M01_ByStem, verbose = TRUE)
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
#M01-2
corr.series.seg(M01_ByStem, series = "M01-2b", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-2b", seg.length = 20, bin.floor = 5)
#Not Sig after 2000
ccf.series.rwl(M01_ByStem, series = "M01-2b", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-2b", seg.length = 20, bin.floor = 5)
#Not clear 

#M01-20
corr.series.seg(M01_ByStem, series = "M01-20", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-20a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-20b", seg.length = 20, bin.floor = 5)
#Not Sig after 1990 (esp. Core A)
ccf.series.rwl(M01_ByStem, series = "M01-20", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-20a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-20b", seg.length = 20, bin.floor = 5)
#-2/-1 lag 1985-2014?

#M01-15
corr.series.seg(M01_ByStem, series = "M01-15", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-15a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-15b", seg.length = 20, bin.floor = 5)
#Not Sig after 2005 
ccf.series.rwl(M01_ByStem, series = "M01-15", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-15a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-15b", seg.length = 10, bin.floor = 5)
#-2/-1 lag 1985-2014?

#M01-10
corr.series.seg(M01_ByStem, series = "M01-10b", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-10b", seg.length = 20, bin.floor = 5)
#Not Sig after 1990
ccf.series.rwl(M01_ByStem, series = "M01-10b", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-10b", seg.length = 20, bin.floor = 5)
#1 lag after 1990?

#M01-16
corr.series.seg(M01_ByStem, series = "M01-16", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-16a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-16b", seg.length = 20, bin.floor = 5)
#Sig 1980-2005
ccf.series.rwl(M01_ByStem, series = "M01-16", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-16a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-16b", seg.length = 20, bin.floor = 5)
#Core B: -1 lag 1955-1974?

#M01-11
corr.series.seg(M01_ByStem, series = "M01-11", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-11a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-11b", seg.length = 20, bin.floor = 5)
#Sig after 1980
ccf.series.rwl(M01_ByStem, series = "M01-11", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-11a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-11b", seg.length = 20, bin.floor = 5)
#

#M01-4
corr.series.seg(M01_ByStem, series = "M01-4b", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-4b", seg.length = 20, bin.floor = 5)
#Sig after 1995
ccf.series.rwl(M01_ByStem, series = "M01-4b", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-4b", seg.length = 20, bin.floor = 5)
#

#M01-12
corr.series.seg(M01_ByStem, series = "M01-12", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-12a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-12b", seg.length = 20, bin.floor = 5)
#Sig after 1995
ccf.series.rwl(M01_ByStem, series = "M01-12", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-12a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-12b", seg.length = 20, bin.floor = 5)
#No clear most recently

#M01-19
corr.series.seg(M01_ByStem, series = "M01-19", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-19b", seg.length = 20, bin.floor = 5)
#Sig after 1990
ccf.series.rwl(M01_ByStem, series = "M01-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-19b", seg.length = 20, bin.floor = 5)
#No clear most recently

#M01-1
corr.series.seg(M01_ByStem, series = "M01-1", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-1a", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-1b", seg.length = 20, bin.floor = 5)
#
ccf.series.rwl(M01_ByStem, series = "M01-1", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-1a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-1b", seg.length = 20, bin.floor = 5)
#

#M01-7
corr.series.seg(M01_ByStem, series = "M01-7", seg.length = 20, bin.floor = 5)
corr.series.seg(M01, series = "M01-7a", seg.length = 10, bin.floor = 5)
corr.series.seg(M01, series = "M01-7b", seg.length = 20, bin.floor = 5)
#A not Sig after 2005
ccf.series.rwl(M01_ByStem, series = "M01-7", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-7a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M01, series = "M01-7b", seg.length = 20, bin.floor = 5)
#