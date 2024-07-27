library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") #Update for each site
#Laptop

M27 <- read.rwl('M27_5.rwl') 
M27_ByStem <- read.rwl('M27_ByStem_5.rwl')
#Rev 1: Fixed 9,1; Removed 20a,29b,27b

###Descriptive information###
dim(M27) #Length of chronology, number of included series
colnames(M27) #Series IDs
head(rownames(M27)) #First few years
class(M27) #Should be rwl and data.frame
rwl.report(M27)
plot(M27, plot.type = "spag")

dim(M27_ByStem) #Length of chronology, number of included series
colnames(M27_ByStem) #Series IDs
head(rownames(M27_ByStem)) #First few years
class(M27_ByStem) #Should be rwl and data.frame
rwl.report(M27_ByStem)
plot(M27_ByStem, plot.type = "spag")
rwlstats = rwl.stats(M27_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

###Stand level crossdating###
corr.rwl.seg(M27, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M27_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Check individual issues###
#M27-9
corr.series.seg(M27_ByStem, series = "M27_9", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M27, series = "M27_9a", seg.length = 20, bin.floor = 5)
corr.series.seg(M27, series = "M27_9b", seg.length = 20, bin.floor = 5)
# Low all around, best before 2000 (0.3)
ccf.series.rwl(M27_ByStem, series = "M27_9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_9b", seg.length = 20, bin.floor = 5)
# -2 Year lag 1995-2014, mostly correct before 2004
xskel.plot(M27_ByStem, series = "M27_9", win.start = 1980, win.end = 2018)
xskel.plot(M27, series = "M27_9a", win.start = 1980, win.end = 2018)
xskel.plot(M27, series = "M27_9b", win.start = 1980, win.end = 2018)

#M27-1
corr.series.seg(M27_ByStem, series = "M27_1", seg.length = 10, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M27, series = "M27_1a", seg.length = 10, bin.floor = 5)
corr.series.seg(M27, series = "M27_1b", seg.length = 10, bin.floor = 5)
#Bad before 2005
ccf.series.rwl(M27_ByStem, series = "M27_1", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_1a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_1b", seg.length = 10, bin.floor = 5)
#-1 year lag 1995-2009

#M27-20
corr.series.seg(M27, series = "M27_20a", seg.length = 10, bin.floor = 5)
#Bad before 1995
ccf.series.rwl(M27, series = "M27_20a", seg.length = 10, bin.floor = 5)
#+1 year lag 1985-1994

#M27-29
corr.series.seg(M27, series = "M27_29b", seg.length = 10, bin.floor = 5)
#Worst between 1995-2005
ccf.series.rwl(M27, series = "M27_29b", seg.length = 10, bin.floor = 5)
#No correlation before 2004

#M27-27
#M27-1
corr.series.seg(M27_ByStem, series = "M27_27", seg.length = 10, bin.floor = 5) #NULLs testing series by default
corr.series.seg(M27, series = "M27_27a", seg.length = 10, bin.floor = 5)
corr.series.seg(M27, series = "M27_27b", seg.length = 10, bin.floor = 5)
#Worst between 1990-2005
ccf.series.rwl(M27_ByStem, series = "M27_27", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_27a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(M27, series = "M27_27b", seg.length = 10, bin.floor = 5)
#Lack of correlation between 1990-2004