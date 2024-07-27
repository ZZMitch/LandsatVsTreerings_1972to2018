library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F07") #Update for each site
#Laptop

F07 <- read.rwl('F07_11.rwl') 
F07_ByStem <- read.rwl('F07_ByStem_11.rwl')
#Rev8: Fixed 8,9,13,25,19,20,21,22,15,5,13

F07_Acer <- read.rwl('F07_11_Acer.rwl')
F07_Acer_ByStem <-read.rwl('F07_ByStem_11_Acer.rwl')

###Descriptive information###
dim(F07) #Length of chronology, number of included series
colnames(F07) #Series IDs
head(rownames(F07)) #First few years
class(F07) #Should be rwl and data.frame
rwl.report(F07)
plot(F07, plot.type = "spag")

dim(F07_ByStem) #Length of chronology, number of included series
colnames(F07_ByStem) #Series IDs
head(rownames(F07_ByStem)) #First few years
class(F07_ByStem) #Should be rwl and data.frame
rwl.report(F07_ByStem)
plot(F07_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F07_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(F07_Acer_ByStem) #Length of chronology, number of included series
colnames(F07_Acer_ByStem) #Series IDs
head(rownames(F07_Acer_ByStem)) #First few years
class(F07_Acer_ByStem) #Should be rwl and data.frame
rwl.report(F07_Acer_ByStem)
plot(F07_Acer_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F07_Acer_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

###Stand level crossdating###
corr.rwl.seg(F07, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(F07_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(F07_ByStem, verbose = FALSE)
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
#F07-5:Tree lvl r = 0.8 , Stand lvl r = 0.34 (0.36, 0.29)
corr.series.seg(F07_ByStem, series = "F07-5", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-5a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-5b", seg.length = 20, bin.floor = 5)
# Worst between 1965 - 1985
ccf.series.rwl(F07_ByStem, series = "F07-5", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-5a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-5b", seg.length = 20, bin.floor = 5)
# -1 year lag most likely 1970-1989
xskel.plot(F07_ByStem, series = "F07-5", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-5a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-5b", win.start = 1950, win.end = 2010)
# Likely need to add a year between 1984 and 1988? Check early/mid-1980s closely. 

#F07-8: Tree lvl r = 0.74, Stand lvl r = 0.28 (0.37, 0.11)
corr.series.seg(F07_ByStem, series = "F07-8", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-8a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-8b", seg.length = 20, bin.floor = 5)
#A: Bad 2000-2015, B is bad through the whole thing
ccf.series.rwl(F07_ByStem, series = "F07-8", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-8a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-8b", seg.length = 20, bin.floor = 5)
#A: Worse post 1985 (+2 year lag?), B: Bad all around
xskel.plot(F07_ByStem, series = "F07-8", win.start = 1980, win.end = 2010)
xskel.plot(F07, series = "F07-8a", win.start = 1980, win.end = 2010)
xskel.plot(F07, series = "F07-8b", win.start = 1980, win.end = 2010)

#F07-9: Tree lvl r = 0.61, Stand lvl r = 0.03 (-0.01, -0.04)
corr.series.seg(F07_ByStem, series = "F07-9", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-9b", seg.length = 20, bin.floor = 5)
#A: Bad all around
ccf.series.rwl(F07_ByStem, series = "F07-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-9b", seg.length = 20, bin.floor = 5)
#A: -2/-3 years correlate consistently through all timesteps
xskel.plot(F07_ByStem, series = "F07-9", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-9a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-9b", win.start = 1950, win.end = 2010)

#F07-13: Tree lvl r = 0.65, Stand lvl r = -0.01 (0, 0.04)
corr.series.seg(F07_ByStem, series = "F07-13", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-13a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-13b", seg.length = 20, bin.floor = 5)
#A: Bad all around, worse since 1995
ccf.series.rwl(F07_ByStem, series = "F07-13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-13b", seg.length = 20, bin.floor = 5)
#A: Strongest lag at -4 (some -5, -2)
xskel.plot(F07_ByStem, series = "F07-13", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-13a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-13b", win.start = 1950, win.end = 2010)

#F07-25: Tree lvl r = 0.8, Stand lvl r = -0.01 (0, 0.11)
corr.series.seg(F07_ByStem, series = "F07-25", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-25a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-25b", seg.length = 20, bin.floor = 5)
#Bad before 2000
ccf.series.rwl(F07_ByStem, series = "F07-25", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-25a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-25b", seg.length = 20, bin.floor = 5)
#Likely 1 year lag before 1999
xskel.plot(F07_ByStem, series = "F07-25", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-25a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-25b", win.start = 1950, win.end = 2010)

#F07-19: Tree lvl r = 0.85, Stand lvl r = 0.11 (0.04, 0.04)
corr.series.seg(F07_ByStem, series = "F07-19", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-19a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-19b", seg.length = 20, bin.floor = 5)
#Low throughout, highest before 1975 (0.2), lowest between 1975 - 1995 (-0.4)
ccf.series.rwl(F07_ByStem, series = "F07-19", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-19a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-19b", seg.length = 20, bin.floor = 5)
#-1 year lag 1955 - 2014, not between 1945 - 1955 it looks like
xskel.plot(F07_ByStem, series = "F07-19", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-19a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-19b", win.start = 1950, win.end = 2010)
#Add ring after 1988 (marker year off by one)

#F07-20: Tree lvl r = 0.83, Stand lvl r = 0.17 (0.17, 0.14)
corr.series.seg(F07_ByStem, series = "F07-20", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-20a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-20b", seg.length = 20, bin.floor = 5)
#Low throughout, highest around 1955 (0.4), lowest from 1965-1985 (-0.1)
ccf.series.rwl(F07_ByStem, series = "F07-20", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-20a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-20b", seg.length = 20, bin.floor = 5)
# -1 year lag 1955 - 2014, no lag at earliest part of series
xskel.plot(F07_ByStem, series = "F07-20", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-20a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-20b", win.start = 1950, win.end = 2010)
#Looks like marker year lines up in 1988? 

#F07-21: Tree lvl r = 0.90, Stand lvl r = 0.21 (0.19, 0.16)
corr.series.seg(F07_ByStem, series = "F07-21", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-21a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-21b", seg.length = 20, bin.floor = 5)
#Correlated before 1965, lower after (-0.4)
ccf.series.rwl(F07_ByStem, series = "F07-21", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-21a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-21b", seg.length = 20, bin.floor = 5)
#-3 lag throughout full core (even earliest dates): 1945-2014
xskel.plot(F07_ByStem, series = "F07-21", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-21a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-21b", win.start = 1950, win.end = 2010)
#Looks like main marker year (1988) is 1991 in cores (need to add 3 years?)

#F07-22: Tree lvl r = 0.89, Stand lvl r = 0.28 (0.34, 0.32)
corr.series.seg(F07_ByStem, series = "F07-22", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-22a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-22b", seg.length = 20, bin.floor = 5)
#Corelates well after 1990, 0 before
ccf.series.rwl(F07_ByStem, series = "F07-22", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-22a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-22b", seg.length = 20, bin.floor = 5)
#No lag 1985-2014, 1 from 1975-1994 (sometime between 1980-1999), 2 from 1955-1974
xskel.plot(F07_ByStem, series = "F07-22", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-22a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-22b", win.start = 1950, win.end = 2010)
#1988 marker is correct. Need to remove one ring from series before then. 1974?

#F07-15: Tree lvl r = 0.7, Stand lvl r = 0.29 (0.16, 0.17)
corr.series.seg(F07_ByStem, series = "F07-15", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-15a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-15b", seg.length = 20, bin.floor = 5)
#Good post 1980 (above 0.4)
ccf.series.rwl(F07_ByStem, series = "F07-15", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-15a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-15b", seg.length = 20, bin.floor = 5)
#Strong -1 lag 1955 - 1974, a few lags 1965-1984 (check 1965-1969?), cor a bit lower recently as well
xskel.plot(F07_ByStem, series = "F07-15", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-15a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-15b", win.start = 1950, win.end = 2010)
# Looks pretty good post 1988 marke, off by one at 1954 marker (need to add year?)
#Look around 1960s-1970s

#F07-10: Tree lvl r = 0.83, Stand lvl r = 0.6 (0.68, 0.63)
corr.series.seg(F07_ByStem, series = "F07-10", seg.length = 20, bin.floor = 5) #NULLs testing series by default
corr.series.seg(F07, series = "F07-10a", seg.length = 20, bin.floor = 5)
corr.series.seg(F07, series = "F07-10b", seg.length = 20, bin.floor = 5)
#Lower after 1995 (0.2)
ccf.series.rwl(F07_ByStem, series = "F07-10", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-10a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F07, series = "F07-10b", seg.length = 20, bin.floor = 5)
#No correlation at any lag in most recent segments
xskel.plot(F07_ByStem, series = "F07-10", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-10a", win.start = 1950, win.end = 2010)
xskel.plot(F07, series = "F07-10b", win.start = 1950, win.end = 2010)
