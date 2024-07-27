library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M20") #Update for each site
#Laptop

M20 <- read.rwl('M20_2.rwl') 
M20_ByStem <- read.rwl('M20_ByStem_2.rwl')
#Rev 0:

M20_Thuja <- read.rwl('M20_Thuja_2.rwl') 
M20_ByStem_Thuja <- read.rwl('M20_ByStem_Thuja_2.rwl')

###Descriptive information###
dim(M20) #Length of chronology, number of included series
colnames(M20) #Series IDs
head(rownames(M20)) #First few years
class(M20) #Should be rwl and data.frame
rwl.report(M20)
plot(M20, plot.type = "spag")

dim(M20_ByStem) #Length of chronology, number of included series
colnames(M20_ByStem) #Series IDs
head(rownames(M20_ByStem)) #First few years
class(M20_ByStem) #Should be rwl and data.frame
rwl.report(M20_ByStem)
plot(M20_ByStem, plot.type = "spag")
rwlstats = rwl.stats(M20_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)
rwi.stats(M20_ByStem)

dim(M20_Thuja) #Length of chronology, number of included series
colnames(M20_Thuja) #Series IDs
head(rownames(M20_Thuja)) #First few years
class(M20_Thuja) #Should be rwl and data.frame
rwl.report(M20_Thuja)
plot(M20_Thuja, plot.type = "spag")

dim(M20_ByStem_Thuja) #Length of chronology, number of included series
colnames(M20_ByStem_Thuja) #Series IDs
head(rownames(M20_ByStem_Thuja)) #First few years
class(M20_ByStem_Thuja) #Should be rwl and data.frame
rwl.report(M20_ByStem_Thuja)
plot(M20_ByStem_Thuja, plot.type = "spag")
rwlstats = rwl.stats(M20_ByStem_Thuja)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

###Stand level crossdating###
corr.rwl.seg(M20, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M20_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M20_Thuja, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M20_ByStem_Thuja, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Check individual issues###
#M20-30
corr.series.seg(M20_Thuja, series = "M20-30a", seg.length = 20, bin.floor = 5)
#Worse before 1990
ccf.series.rwl(M20_Thuja, series = "M20-30a", seg.length = 20, bin.floor = 5)
# No clear lag
xskel.plot(M20_Thuja, series = "M20-30a", win.start = 1940, win.end = 2018)

#M20-24-2 (0)
corr.series.seg(M20_Thuja, series = "M20-0b", seg.length = 20, bin.floor = 5)
#Worse before 1990
ccf.series.rwl(M20_Thuja, series = "M20-0b", seg.length = 20, bin.floor = 5)
#No clear lag
xskel.plot(M20_Thuja, series = "M20-0b", win.start = 1940, win.end = 2018)

#M20-14
corr.series.seg(M20_ByStem_Thuja, series = "M20-14", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-14a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-14b", seg.length = 20, bin.floor = 5)
#Worst after 1990, pre 1955
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-14", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-14a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-14b", seg.length = 20, bin.floor = 5)
# 4 year lag in first?? Probably not.

#M20-10
corr.series.seg(M20_ByStem_Thuja, series = "M20-10", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-10b", seg.length = 20, bin.floor = 5)
#Worst after 1975-1985 (a but after 2000)
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-10b", seg.length = 20, bin.floor = 5)
# 4 year lag in first?? Probably not.

#M20-26
corr.series.seg(M20_ByStem_Thuja, series = "M20-26", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-26a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-26b", seg.length = 20, bin.floor = 5)
#Worst before 1980, two cores not related...
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-26", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-26a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-26b", seg.length = 20, bin.floor = 5)
# Not clear

#M20-15
corr.series.seg(M20_ByStem_Thuja, series = "M20-15", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-15a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-15b", seg.length = 20, bin.floor = 5)
#Worst after 1985... 15b: bad before 2005
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-15", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-15a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-15b", seg.length = 20, bin.floor = 5)
# 15b: check 1980-1999 (-3)

#M20-23
corr.series.seg(M20_Thuja, series = "M20-23a", seg.length = 20, bin.floor = 5)
#Worse after 1985
ccf.series.rwl(M20_Thuja, series = "M20-23a", seg.length = 20, bin.floor = 5)
# No clear lag
xskel.plot(M20_Thuja, series = "M20-23a", win.start = 1940, win.end = 2018)

#M20-13
corr.series.seg(M20_ByStem_Thuja, series = "M20-13", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-13a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-13b", seg.length = 20, bin.floor = 5)
#Worst after 1990
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-13b", seg.length = 20, bin.floor = 5)
# Not clear

#M20-19
corr.series.seg(M20_ByStem_Thuja, series = "M20-19", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20_Thuja, series = "M20-19b", seg.length = 20, bin.floor = 5)
#Worst before 1975
ccf.series.rwl(M20_ByStem_Thuja, series = "M20-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20_Thuja, series = "M20-19b", seg.length = 20, bin.floor = 5)
# Not clear

#M20-4
corr.series.seg(M20_ByStem, series = "M20-4", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-4a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-4b", seg.length = 20, bin.floor = 5)
#Worst after 1995
ccf.series.rwl(M20_ByStem, series = "M20-4", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-4a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-4b", seg.length = 20, bin.floor = 5)
# Not clear

#M20-5
corr.series.seg(M20_ByStem, series = "M20-5", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-5a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-5b", seg.length = 20, bin.floor = 5)
#Worst after 1985
ccf.series.rwl(M20_ByStem, series = "M20-5", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-5a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-5b", seg.length = 20, bin.floor = 5)
# Not clear

#M20-6
corr.series.seg(M20_ByStem, series = "M20-6", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-6a", seg.length = 20, bin.floor = 5)
corr.series.seg(M20, series = "M20-6b", seg.length = 20, bin.floor = 5)
#Worst after 1990
ccf.series.rwl(M20_ByStem, series = "M20-6", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-6a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M20, series = "M20-6b", seg.length = 20, bin.floor = 5)
# -1 lag?