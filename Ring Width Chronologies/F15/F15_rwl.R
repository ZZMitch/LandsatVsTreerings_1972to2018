library(dplR)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/F15") 
#Update for each site

F15_Quercus = read.rwl('F15_Quercus_1.rwl')
F15_ByStem_Quercus = read.rwl('F15_ByStem_Quercus_1.rwl')

F15_Carya = read.rwl('F15_Carya.rwl')
F15_ByStem_Carya = read.rwl('F15_ByStem_Carya.rwl')

F15_Acer = read.rwl('F15_Acer_1.rwl')
F15_ByStem_Acer = read.rwl('F15_ByStem_Acer_1.rwl')

F15 = read.rwl('F15_4.rwl')
F15_ByStem = read.rwl('F15_ByStem_4.rwl')

# Descriptive Information #
dim(F15_ByStem_Quercus) #Length of chronology, number of included series
colnames(F15_ByStem_Quercus) #Series IDs
head(rownames(F15_ByStem_Quercus)) #First few years
class(F15_ByStem_Quercus) #Should be rwl and data.frame
rwl.report(F15_ByStem_Quercus)
plot(F15_ByStem_Quercus, plot.type = "spag")
rwlstats = rwl.stats(F15_ByStem_Quercus)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(F15_ByStem_Carya) #Length of chronology, number of included series
colnames(F15_ByStem_Carya) #Series IDs
head(rownames(F15_ByStem_Carya)) #First few years
class(F15_ByStem_Carya) #Should be rwl and data.frame
rwl.report(F15_ByStem_Carya)
plot(F15_ByStem_Carya, plot.type = "spag")
rwlstats = rwl.stats(F15_ByStem_Carya)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(F15_ByStem_Acer) #Length of chronology, number of included series
colnames(F15_ByStem_Acer) #Series IDs
head(rownames(F15_ByStem_Acer)) #First few years
class(F15_ByStem_Acer) #Should be rwl and data.frame
rwl.report(F15_ByStem_Acer)
plot(F15_ByStem_Acer, plot.type = "spag")
rwlstats = rwl.stats(F15_ByStem_Acer)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


dim(F15_ByStem) #Length of chronology, number of included series
colnames(F15_ByStem) #Series IDs
head(rownames(F15_ByStem)) #First few years
class(F15_ByStem) #Should be rwl and data.frame
rwl.report(F15_ByStem)
plot(F15_ByStem, plot.type = "spag")
rwlstats = rwl.stats(F15_ByStem)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)


# Stand Level Cross Dating #
corr.rwl.seg(F15_Quercus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F15_ByStem_Quercus, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

corr.rwl.seg(F15_Carya, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F15_ByStem_Carya, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

corr.rwl.seg(F15_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F15_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL)

corr.rwl.seg(F15, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
             pcrit = 0.05, biweight = TRUE, method = "spearman", 
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, 
             master = NULL) 

corr.rwl.seg(F15_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, 
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
Test_EPS <- strip.rwl(F15_ByStem, verbose = TRUE)
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
#F15-5
corr.series.seg(F15_ByStem_Quercus, series = "F15_5", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_5a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_5b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Quercus, series = "F15_5", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_5a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_5b", seg.length = 20, bin.floor = 5)

#F15-17
corr.series.seg(F15_ByStem_Quercus, series = "F15_17", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_17a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_17b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Quercus, series = "F15_17", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_17a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_17b", seg.length = 20, bin.floor = 5)

#F15-1
corr.series.seg(F15_ByStem_Quercus, series = "F15_1", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_1a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Quercus, series = "F15_1b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Quercus, series = "F15_1", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_1a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Quercus, series = "F15_1b", seg.length = 20, bin.floor = 5)

#F15-23
corr.series.seg(F15_ByStem_Carya, series = "F15_23", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_23a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_23b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Carya, series = "F15_23", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_23a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_23b", seg.length = 20, bin.floor = 5)

#F15-28
corr.series.seg(F15_ByStem_Carya, series = "F15_28", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_28a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_28b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Carya, series = "F15_28", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_28a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_28b", seg.length = 20, bin.floor = 5)

#F15-30
corr.series.seg(F15_ByStem_Carya, series = "F15_30", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_30a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Carya, series = "F15_30b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Carya, series = "F15_30", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_30a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Carya, series = "F15_30b", seg.length = 20, bin.floor = 5)

#F15-18
corr.series.seg(F15_ByStem_Acer, series = "F15_18", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_18a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_18b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Acer, series = "F15_18", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_18a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_18b", seg.length = 20, bin.floor = 5)

#F15-13
corr.series.seg(F15_ByStem_Acer, series = "F15_13", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_13a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_13b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Acer, series = "F15_13", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_13a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_13b", seg.length = 20, bin.floor = 5)

#F15-12
corr.series.seg(F15_ByStem_Acer, series = "F15_12", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_12a", seg.length = 20, bin.floor = 5)
corr.series.seg(F15_Acer, series = "F15_12b", seg.length = 20, bin.floor = 5)

ccf.series.rwl(F15_ByStem_Acer, series = "F15_12", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_12a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(F15_Acer, series = "F15_12b", seg.length = 20, bin.floor = 5)