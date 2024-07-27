library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F30") 
#Update for each site

F30_Acer = read.rwl('F30_Acer_4.rwl')
F30_ByStem_Acer = read.rwl('F30_ByStem_Acer_4.rwl')

F30_Betula = read.rwl('F30_Betula.rwl')
F30_ByStem_Betula = read.rwl('F30_ByStem_Betula.rwl')

#F30_Con = read.rwl('F30_Coniferous.rwl')
#F30_ByStem_Con = read.rwl('F30_ByStem_Coniferous.rwl')

# Descriptive Information #
dim(F30_ByStem_Acer) #Length of chronology, number of included series
colnames(F30_ByStem_Acer) #Series IDs
head(rownames(F30_ByStem_Acer)) #First few years
class(F30_ByStem_Acer) #Should be rwl and data.frame
rwl.report(F30_ByStem_Acer)
plot(F30_ByStem_Acer, plot.type = "spag")
rwlstats = rwl.stats(F30_ByStem_Acer)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


dim(F30_ByStem_Betula) #Length of chronology, number of included series
colnames(F30_ByStem_Betula) #Series IDs
head(rownames(F30_ByStem_Betula)) #First few years
class(F30_ByStem_Betula) #Should be rwl and data.frame
rwl.report(F30_ByStem_Betula)
plot(F30_ByStem_Betula, plot.type = "spag")
rwlstats = rwl.stats(F30_ByStem_Betula)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


# Stand Level Cross Dating #
corr.rwl.seg(F30_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F30_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

corr.rwl.seg(F30_Betula, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F30_ByStem_Betula, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
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
Test_EPS <- strip.rwl(F30_ByStem_Betula, verbose = TRUE)
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
#F30-9
corr.series.seg(F30_ByStem_Acer, series = "F30-9", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-9a", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-9b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F30_ByStem_Acer, series = "F30-9", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-9a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-9b", seg.length = 20, bin.floor = 5)

#F30-7
corr.series.seg(F30_ByStem_Acer, series = "F30-7", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-7a", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-7b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F30_ByStem_Acer, series = "F30-7", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-7a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-7b", seg.length = 20, bin.floor = 5)

#F30-7
corr.series.seg(F30_ByStem_Acer, series = "F30-11", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-11a", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Acer, series = "F30-11b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F30_ByStem_Acer, series = "F30-11", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-11a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Acer, series = "F30-11b", seg.length = 20, bin.floor = 5)

#F30-30
corr.series.seg(F30_ByStem_Betula, series = "F30-30", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Betula, series = "F30-30a", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Betula, series = "F30-30b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F30_ByStem_Betula, series = "F30-30", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Betula, series = "F30-30a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Betula, series = "F30-30b", seg.length = 20, bin.floor = 5)

#F30-15
corr.series.seg(F30_ByStem_Betula, series = "F30-15", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Betula, series = "F30-15a", seg.length = 20, bin.floor = 5)
corr.series.seg(F30_Betula, series = "F30-15b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F30_ByStem_Betula, series = "F30-15", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Betula, series = "F30-15a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F30_Betula, series = "F30-15b", seg.length = 20, bin.floor = 5)
