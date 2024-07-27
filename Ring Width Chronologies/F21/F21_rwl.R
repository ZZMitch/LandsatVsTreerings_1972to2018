library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F21") #Update for each site
#Laptop

F21 <- read.rwl('F21_10.rwl') 
F21_ByStem <- read.rwl('F21_ByStem_10.rwl')
#Rev 5: Fixed 13,19,12,20,10 ; Removed 16,2

F21_Acer <- read.rwl('F21_Acer_6.rwl') 
F21_ByStem_Acer <- read.rwl('F21_ByStem_Acer_6.rwl')

#F21_Tilia <- read.rwl('F21_Tilia.rwl')
#F21_ByStem_Tilia <- read.rwl('F21_ByStem_Tilia.rwl')

###Descriptive information###
dim(F21_ByStem) #Length of chronology, number of included series
colnames(F21_ByStem) #Series IDs
head(rownames(F21_ByStem)) #First few years
class(F21_ByStem) #Should be rwl and data.frame
rwl.report(F21_ByStem)
plot(F21_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F21_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(F21_ByStem_Acer) #Length of chronology, number of included series
colnames(F21_ByStem_Acer) #Series IDs
head(rownames(F21_ByStem_Acer)) #First few years
class(F21_ByStem_Acer) #Should be rwl and data.frame
rwl.report(F21_ByStem_Acer)
plot(F21_ByStem_Acer, plot.type = "spag")
rwlstats = rwl.stats(F21_ByStem_Acer)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

#dim(F21_ByStem_Tilia) #Length of chronology, number of included series
#colnames(F21_ByStem_Tilia) #Series IDs
#head(rownames(F21_ByStem_Tilia)) #First few years
#class(F21_ByStem_Tilia) #Should be rwl and data.frame
#rwl.report(F21_ByStem_Tilia)
#plot(F21_ByStem_Tilia, plot.type = "spag")

###Stand level crossdating###
corr.rwl.seg(F21, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(F21_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(F21_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(F21_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

#corr.rwl.seg(F21_Tilia, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             #method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#corr.rwl.seg(F21_ByStem_Tilia, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             #method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Check individual issues###
#F21-13
corr.series.seg(F21_ByStem, series = "F21-13", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-13a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-13b", seg.length = 20, bin.floor = 5)
#Only reasonable between 1980-1995
ccf.series.rwl(F21_ByStem, series = "F21-13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-13b", seg.length = 20, bin.floor = 5)
# -4 year lag? Core A esp.

#F21-16
corr.series.seg(F21_ByStem, series = "F21-16", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-16a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-16b", seg.length = 20, bin.floor = 5)
#Not sig before 1990
ccf.series.rwl(F21_ByStem, series = "F21-16", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-16a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-16b", seg.length = 20, bin.floor = 5)
#2 year lag 1970-1999, up to 5 1960-1979?

#F21-2
corr.series.seg(F21_ByStem, series = "F21-2", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-2a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-2b", seg.length = 20, bin.floor = 5)
#Not sig 1995
ccf.series.rwl(F21_ByStem, series = "F21-2", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-2a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-2b", seg.length = 20, bin.floor = 5)
#2 year lag 1970-1999, up to 5 1960-1979?

#F21-19
corr.series.seg(F21_ByStem, series = "F21-19", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-19b", seg.length = 20, bin.floor = 5)
#Not sig before 1975
ccf.series.rwl(F21_ByStem, series = "F21-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-19b", seg.length = 20, bin.floor = 5)
#2 year lag 1970-1999, up to 5 1960-1979?

#F21-12
corr.series.seg(F21_ByStem, series = "F21-12", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-12a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-12b", seg.length = 20, bin.floor = 5)
#Not sig 1930-1975
ccf.series.rwl(F21_ByStem, series = "F21-12", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-12a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-12b", seg.length = 20, bin.floor = 5)
#2 year lag 1920-1969, 3-4 year lag 1910-1929

#F21-15
corr.series.seg(F21_ByStem, series = "F21-15", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-15a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-15b", seg.length = 20, bin.floor = 5)
#Not sig 1925-1935, 1965-1975, post-1995
ccf.series.rwl(F21_ByStem, series = "F21-15", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-15a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-15b", seg.length = 20, bin.floor = 5)
#Not clear

#F21-20
corr.series.seg(F21_ByStem_Acer, series = "F21-20", seg.length = 10, bin.floor = 5)
corr.series.seg(F21_Acer, series = "F21-20a", seg.length = 10, bin.floor = 5)
corr.series.seg(F21_Acer, series = "F21-20b", seg.length = 10, bin.floor = 5)
#Not sig before 2000
ccf.series.rwl(F21_ByStem_Acer, series = "F21-20", seg.length = 10, bin.floor = 5)
ccf.series.rwl(F21_Acer, series = "F21-20a", seg.length = 10, bin.floor = 5)
ccf.series.rwl(F21_Acer, series = "F21-20b", seg.length = 10, bin.floor = 5)
#Not clear

#F21-10
corr.series.seg(F21_ByStem, series = "F21-10", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-10b", seg.length = 20, bin.floor = 5)
#A: not sig before 1990, B similar but closer to sig from 1950-1970
ccf.series.rwl(F21_ByStem, series = "F21-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-10b", seg.length = 20, bin.floor = 5)
#3 year lag? A esp.

#F21-9
corr.series.seg(F21_ByStem, series = "F21-9", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(F21, series = "F21-9b", seg.length = 20, bin.floor = 5)
#Up and down
ccf.series.rwl(F21_ByStem, series = "F21-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F21, series = "F21-9b", seg.length = 20, bin.floor = 5)
#Unclear

###Chronology stripping by EPS###
#Looking for bad series
Test_EPS <- strip.rwl(F21_ByStem_Tilia)
#Above 0.85 = good?

corr.rwl.seg(Test_EPS, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)