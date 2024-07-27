library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M07") #Update for each site
#Laptop

M07 <- read.rwl('M07.rwl') 
M07_ByStem <- read.rwl('M07_ByStem.rwl')
#No Revs

M07_Tsuga <- read.rwl('M07_Tsuga_1.rwl')
M07_ByStem_Tsuga <- read.rwl('M07_ByStem_Tsuga_1.rwl')
#Rev 1: Removed 29A; Fixed 3b

M07_Pinus <- read.rwl('M07_Pinus_1.rwl')
M07_ByStem_Pinus <- read.rwl('M07_ByStem_Pinus_1.rwl')
#Rev 1: Removed 16b

M07_Acer <- read.rwl('M07_Acer.rwl')
M07_ByStem_Acer <- read.rwl('M07_ByStem_Acer.rwl')
#Rev 1: Removed 11b

###Descriptive information###
dim(M07_ByStem) #Length of chronology, number of included series
colnames(M07_ByStem) #Series IDs
head(rownames(M07_ByStem)) #First few years
class(M07_ByStem) #Should be rwl and data.frame
rwl.report(M07_ByStem)
plot(M07_ByStem, plot.type = "spag")
rwlstats = rwl.stats(M07_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(M07_ByStem_Tsuga) #Length of chronology, number of included series
colnames(M07_ByStem_Tsuga) #Series IDs
head(rownames(M07_ByStem_Tsuga)) #First few years
class(M07_ByStem_Tsuga) #Should be rwl and data.frame
rwl.report(M07_ByStem_Tsuga)
plot(M07_ByStem_Tsuga, plot.type = "spag")
rwlstats = rwl.stats(M07_ByStem_Tsuga)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


dim(M07_ByStem_Pinus) #Length of chronology, number of included series
colnames(M07_ByStem_Pinus) #Series IDs
head(rownames(M07_ByStem_Pinus)) #First few years
class(M07_ByStem_Pinus) #Should be rwl and data.frame
rwl.report(M07_ByStem_Pinus)
plot(M07_ByStem_Pinus, plot.type = "spag")

dim(M07_ByStem_Acer) #Length of chronology, number of included series
colnames(M07_ByStem_Acer) #Series IDs
head(rownames(M07_ByStem_Acer)) #First few years
class(M07_ByStem_Acer) #Should be rwl and data.frame
rwl.report(M07_ByStem_Acer)
plot(M07_ByStem_Acer, plot.type = "spag")

###Stand level crossdating###
corr.rwl.seg(M07, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M07_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M07_Tsuga, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M07_ByStem_Tsuga, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M07_Pinus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M07_ByStem_Pinus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M07_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M07_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)


#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(M07, verbose = TRUE)
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
#M07-29
corr.series.seg(M07_ByStem_Tsuga, series = "M07-29", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-29a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-29b", seg.length = 20, bin.floor = 5)
#Not Sig 1950-1965 (esp. A not sig)
ccf.series.rwl(M07_ByStem_Tsuga, series = "M07-29", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-29a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-29b", seg.length = 20, bin.floor = 5)
#Issues mostly with A

#M07-3
corr.series.seg(M07_ByStem_Tsuga, series = "M07-3", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-3a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-3b", seg.length = 20, bin.floor = 5)
#Not Sig around 1940-1950 (esp. Core B)
ccf.series.rwl(M07_ByStem_Tsuga, series = "M07-3", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-3a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-3b", seg.length = 20, bin.floor = 5)
#B: No link 1940-1959

#M07-9
corr.series.seg(M07_ByStem, series = "M07-9", seg.length = 20, bin.floor = 5)
corr.series.seg(M07, series = "M07-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07, series = "M07-9b", seg.length = 20, bin.floor = 5)
#Not Sig after 1975
ccf.series.rwl(M07_ByStem, series = "M07-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07, series = "M07-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07, series = "M07-9b", seg.length = 20, bin.floor = 5)
#B: 4 yeag lag?

#M07-20a
corr.series.seg(M07_ByStem_Tsuga, series = "M07-20a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-20a", seg.length = 20, bin.floor = 5)
#Not Sig 1970-1990
ccf.series.rwl(M07_ByStem_Tsuga, series = "M07-20a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-20a", seg.length = 20, bin.floor = 5)
#-2 year lag 1960-1979?

#M07-17
corr.series.seg(M07_ByStem_Tsuga, series = "M07-17", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-17a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-17b", seg.length = 20, bin.floor = 5)
#Not Sig after 1975
ccf.series.rwl(M07_ByStem_Tsuga, series = "M07-17", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-17a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-17b", seg.length = 20, bin.floor = 5)
#B: unclear 1970-1999

#M07-8
corr.series.seg(M07_ByStem_Tsuga, series = "M07-8", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-8a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Tsuga, series = "M07-8b", seg.length = 20, bin.floor = 5)
#Not Sig after 1980
ccf.series.rwl(M07_ByStem_Tsuga, series = "M07-8", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-8a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Tsuga, series = "M07-8b", seg.length = 20, bin.floor = 5)
#Unclear after 1970

#M07-16
corr.series.seg(M07_ByStem_Pinus, series = "M07-16", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Pinus, series = "M07-16a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Pinus, series = "M07-16b", seg.length = 20, bin.floor = 5)
#Not Sig around 1930 and 1960
ccf.series.rwl(M07_ByStem_Pinus, series = "M07-16", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Pinus, series = "M07-16a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Pinus, series = "M07-16b", seg.length = 20, bin.floor = 5)
#No sig connection before 1970, esp B. 

#M07-22
corr.series.seg(M07_ByStem_Acer, series = "M07-22", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Acer, series = "M07-22a", seg.length = 20, bin.floor = 5)
corr.series.seg(M07_Acer, series = "M07-22b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(M07_ByStem_Acer, series = "M07-22", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Acer, series = "M07-22a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M07_Acer, series = "M07-22b", seg.length = 20, bin.floor = 5)
 