library(dplR)
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26") #Update for each site
#Work Computer and Home Desktop

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M26") #Update for each site
#Laptop

#M26 <- read.rwl('M26.rwl') 
#M26_ByStem <- read.rwl('M26_ByStem.rwl')
#No Revs

M26_Thuja <- read.rwl('M26_Thuja.rwl')
M26_ByStem_Thuja <- read.rwl('M26_ByStem_Thuja.rwl')
#No Revs

M26_Deciduous <- read.rwl('M26_Deciduous_1.rwl')
M26_ByStem_Deciduous <- read.rwl('M26_ByStem_Deciduous_1.rwl')
#No Rev

M26_Acer <- read.rwl('M26_Acer_1.rwl')
M26_ByStem_Acer <- read.rwl('M26_ByStem_Acer_1.rwl')
#No Rev

###Descriptive information###
#dim(M26_ByStem) #Length of chronology, number of included series
#colnames(M26_ByStem) #Series IDs
#head(rownames(M26_ByStem)) #First few years
#class(M26_ByStem) #Should be rwl and data.frame
#rwl.report(M26_ByStem)
#plot(M26_ByStem, plot.type = "spag")

dim(M26_ByStem_Thuja) #Length of chronology, number of included series
colnames(M26_ByStem_Thuja) #Series IDs
head(rownames(M26_ByStem_Thuja)) #First few years
class(M26_ByStem_Thuja) #Should be rwl and data.frame
rwl.report(M26_ByStem_Thuja)
plot(M26_ByStem_Thuja, plot.type = "spag")
rwlstats = rwl.stats(M26_ByStem_Thuja)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(M26_ByStem_Deciduous) #Length of chronology, number of included series
colnames(M26_ByStem_Deciduous) #Series IDs
head(rownames(M26_ByStem_Deciduous)) #First few years
class(M26_ByStem_Deciduous) #Should be rwl and data.frame
rwl.report(M26_ByStem_Deciduous)
plot(M26_ByStem_Deciduous, plot.type = "spag")
rwlstats = rwl.stats(M26_ByStem_Deciduous)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)

dim(M26_ByStem_Acer) #Length of chronology, number of included series
colnames(M26_ByStem_Acer) #Series IDs
head(rownames(M26_ByStem_Acer)) #First few years
class(M26_ByStem_Acer) #Should be rwl and data.frame
rwl.report(M26_ByStem_Acer)
plot(M26_ByStem_Acer, plot.type = "spag")
rwlstats = rwl.stats(M26_ByStem_Acer)
mean(rwlstats$year)
sd(rwlstats$year)
mean(rwlstats$mean)
sd(rwlstats$mean)



###Stand level crossdating###
#corr.rwl.seg(M26, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
#             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

#corr.rwl.seg(M26_ByStem, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
#             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M26_Thuja, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M26_ByStem_Thuja, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M26_Deciduous, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M26_ByStem_Deciduous, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)

corr.rwl.seg(M26_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL) 

corr.rwl.seg(M26_ByStem_Acer, seg.length = 20, bin.floor = 5, prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = "spearman", make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE, master = NULL)


#Correlation of each series with rest of rwl: see Bunn, 2010 (Fig. 1)
#Each segment of each series is shown and colored by correlation with master chronology
#Bottom line in each series adheres to bottom timeline and top adheres to top timeline
#Blue = correlates well to master (p < or = 0.05)
#Green = do not overlap the time period (no correlations calculated)
#Red = does not correlate well to master (p > 0.05)

###Chronology stripping by EPS###
#Looking for bad series that can be removed
Test_EPS <- strip.rwl(M26_Deciduous, verbose = TRUE)
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
#M26-29
corr.series.seg(M26_ByStem_Thuja, series = "M26-29", seg.length = 20, bin.floor = 5)
corr.series.seg(M26_Thuja, series = "M26-29a", seg.length = 20, bin.floor = 5)
corr.series.seg(M26_Thuja, series = "M26-29b", seg.length = 20, bin.floor = 5)
#Not Sig 1950-1965 (esp. A not sig)
ccf.series.rwl(M26_ByStem_Thuja, series = "M26-29", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M26_Thuja, series = "M26-29a", seg.length = 20, bin.floor = 5)
ccf.series.rwl(M26_Thuja, series = "M26-29b", seg.length = 20, bin.floor = 5)
#Issues mostly with A