###Dendro Data###
library(dplR)
setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") 
#Update for each site
#Work Computer and Home Desktop

#setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology/Ring Width Chronologies/M27") 
#Update for each site
#Laptop

###ByStem###
ByStem <- read.rwl('M27_ByStem_5.rwl') #Update for each site
#rwl.report(F07_ByStem)
#plot(F07_ByStem, plot.type = "spag")
# CONFIRMED: Same as F07_ByStem raw ring widths
# CONFIRMED: values in rwl files are the average of exact ring width lengths measured (nothing done to them yet!)

#No Detrending
ByStem_NoDetrend_Mean <- chron(ByStem)
# CONFIRMED: values = to mean of raw ring widths
# Biweight does robust mean, defaults to on
# Prewhiten adds res column, appears to not change mean values
plot(ByStem_NoDetrend_Mean)

#Detrending#
#F07_ByStem_Detrend <- detrend(F07_ByStem, make.plot = TRUE)
# All available methods, creates plots for each raw series if make.plot = TRUE
# Spline, ModNegExp, Mean, Ar, Friedman, ModHugershoff
ByStem_Spline <- detrend(ByStem, method = c("Spline"))
ByStem_Spline_Mean <- chron(ByStem_Spline)
plot(ByStem_Spline_Mean)

ByStem_ModNegExp <- detrend(ByStem, method = c("ModNegExp"))
ByStem_ModNegExp_Mean <- chron(ByStem_ModNegExp)
plot(ByStem_ModNegExp_Mean)

ByStem_Mean <- detrend(ByStem, method = c("Mean"))
ByStem_Mean_Mean <- chron(ByStem_Mean)
plot(ByStem_Mean_Mean)

ByStem_Ar <- detrend(ByStem, method = c("Ar"))
ByStem_Ar_Mean <- chron(ByStem_Ar)
plot(ByStem_Ar_Mean)

ByStem_Friedman <- detrend(ByStem, method = c("Friedman"))
ByStem_Friedman_Mean <- chron(ByStem_Friedman)
plot(ByStem_Friedman_Mean)

ByStem_ModHugershoff <- detrend(ByStem, method = c("ModHugershoff"))
ByStem_ModHugershoff_Mean <- chron(ByStem_ModHugershoff)
plot(ByStem_ModHugershoff_Mean)

###ByCore### #Trevor recommends? Not sure if will be much different...
#F07 <- read.rwl('F07_11.rwl')
#F07_Thuja <- read.rwl('F07_11_Thuja.rwl')
#F07_NoDetrend_Mean <- chron(F07, prefix = "F07") #Testing Tree vs. Core level
#plot(F07_NoDetrend_Mean) #Testing Tree vs. Core level

###Merge into single dataset###
ByStem_Merged <- data.frame(row.names = rownames(ByStem))

ByStem_Merged$NoDetrend <- ByStem_NoDetrend_Mean$xxxstd

ByStem_Merged$Spline <- ByStem_Spline_Mean$xxxstd
ByStem_Merged$ModNegExp <- ByStem_ModNegExp_Mean$xxxstd
ByStem_Merged$Mean <- ByStem_Mean_Mean$xxxstd
ByStem_Merged$Ar <- ByStem_Ar_Mean$xxxstd
ByStem_Merged$Friedman <- ByStem_Friedman_Mean$xxxstd
ByStem_Merged$ModHugershoff <- ByStem_ModHugershoff_Mean$xxxstd

ByStem_Merged$SampleDepth <- ByStem_Spline_Mean$samp.depth

#Lag years by -1
#TRW is mostly composed of earlywood cells which are highly reliant on carbohydrate reserves from previous year
#ByStem_Merged_MinusOne <- ByStem_Merged
#rownames(ByStem_Merged_MinusOne)
#rownames(ByStem_Merged_MinusOne) = c("1932","1933","1934","1935","1936","1937","1938","1939","1940","1941",
#                                     "1942","1943","1944","1945","1946","1947","1948","1949","1950","1951",
#                                     "1952","1953","1954","1955","1956","1957","1958","1959","1960","1961",
#                                     "1962","1963","1964","1965","1966","1967","1968","1969","1970","1971",
#                                     "1972","1973","1974","1975","1976","1977","1978","1979","1980","1981",
#                                     "1982","1983","1984","1985","1986","1987","1988","1989","1990","1991",
#                                     "1992","1993","1994","1995","1996","1997","1998","1999","2000","2001",
#                                     "2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
#                                     "2012","2013","2014","2015","2016","2017")
#Need to update above for each Site (if required, some timeseries correlaitons check for lag)

###Landsat Data###
Landsat <- read.csv('M27_Landsat.csv', row.names = 1) #Update for each site
#plot(rownames(Landsat),Landsat$Pt5_CC_Median)
#Landsat_Pt5_CC_Median_Detrend <- detrend.series(Landsat$Pt5_CC_Median)
Landsat_Avg_CC_Median_Detrend <- detrend.series(Landsat$Avg_CC_Median)
#Landsat_Pt5_CC_Fitted_Detrend <- detrend.series(Landsat$Pt5_CC_Fitted) 
#Fitted data are too simple for detrending... need to instead run dendro data through LandTrendr somehow

#Merge into single dataset#
Landsat_Merged <- data.frame(row.names = rownames(Landsat))

#Landsat_Merged$Pt5_Median <- Landsat$Pt5_CC_Median
Landsat_Merged$Avg_Median <- Landsat$Avg_CC_Median
#Landsat_Merged$Pt5_Fitted <- Landsat$Pt5_CC_Fitted
Landsat_Merged$Avg_Fitted <- Landsat$Avg_CC_Fitted

#Landsat_Merged$Pt5_Spline <- Landsat_Pt5_CC_Median_Detrend$Spline
Landsat_Merged$Avg_Spline <- Landsat_Avg_CC_Median_Detrend$Spline
#Landsat_Merged$Pt5_ModNegExp <- Landsat_Pt5_CC_Median_Detrend$ModNegExp
Landsat_Merged$Avg_ModNegExp <- Landsat_Avg_CC_Median_Detrend$ModNegExp
#Landsat_Merged$Pt5_Mean <- Landsat_Pt5_CC_Median_Detrend$Mean
Landsat_Merged$Avg_Mean <- Landsat_Avg_CC_Median_Detrend$Mean
#Landsat_Merged$Pt5_Ar <- Landsat_Pt5_CC_Median_Detrend$Ar
Landsat_Merged$Avg_Ar <- Landsat_Avg_CC_Median_Detrend$Ar
#Landsat_Merged$Pt5_Friedman <- Landsat_Pt5_CC_Median_Detrend$Friedman
Landsat_Merged$Avg_Friedman <- Landsat_Avg_CC_Median_Detrend$Friedman
#Landsat_Merged$Pt5_ModHugershoff <- Landsat_Pt5_CC_Median_Detrend$ModHugershoff
Landsat_Merged$Avg_ModHugershoff <- Landsat_Avg_CC_Median_Detrend$ModHugershoff

###Export###
write.csv(ByStem_Merged,"M27_ByStem_Chronologies.csv") #Update for each site
write.csv(Landsat_Merged, "M27_Landsat_Chronologies.csv") #Update for each site
