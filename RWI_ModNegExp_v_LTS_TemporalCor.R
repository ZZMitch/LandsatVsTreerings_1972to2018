# Overall Correlation and Temporal Correlation #

##### Set up and Data #####
library(dplR)
library(dplyr)
library(gtools)
library(ggplot2)
library(patchwork)
library(ggtext)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop

##### Bring in RWLs and apply ModNegExp detrending #####
#####
# F07Dec
#rwl.F07Dec = read.rwl("Ring Width Chronologies/F07/F07_12.rwl")
#rwl.report(rwl.F07Dec)
#rwl.F07Dec.ids = read.ids(rwl.F07Dec, stc = c(4, 2, 1))
#yr.F07Dec = time(rwl.F07Dec)

#rwl.F07Dec.mne = detrend(rwl.F07Dec, method = "ModNegExp")

#rwl.F07Dec.mne.sss = sss(rwl.F07Dec.mne, rwl.F07Dec.ids)
#cut.F07Dec.mne = max(yr.F07Dec[rwl.F07Dec.mne.sss < 0.85])
#yr.cut.F07Dec.mne = yr.F07Dec[yr.F07Dec > cut.F07Dec.mne]

#rwl.F07Dec.mne.crn = chron(detrend(rwl.F07Dec[yr.F07Dec > cut.F07Dec.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.F07Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# F07Ace (Site 08)
rwl.F07Ace = read.rwl("Ring Width Chronologies/F07/F07_Acer_12.rwl")
rwl.report(rwl.F07Ace)
rwl.F07Ace.ids = read.ids(rwl.F07Ace, stc = c(4, 2, 1))
yr.F07Ace = time(rwl.F07Ace)

rwl.F07Ace.mne = detrend(rwl.F07Ace, method = "ModNegExp")

rwl.F07Ace.mne.sss = sss(rwl.F07Ace.mne, rwl.F07Ace.ids)
cut.F07Ace.mne = max(yr.F07Ace[rwl.F07Ace.mne.sss < 0.85])
yr.cut.F07Ace.mne = yr.F07Ace[yr.F07Ace > cut.F07Ace.mne]

rwl.F07Ace.mne.crn = chron(detrend(rwl.F07Ace[yr.F07Ace > cut.F07Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F07Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Dec
#rwl.F15Dec = read.rwl("Ring Width Chronologies/F15/F15_4.rwl")
#rwl.report(rwl.F15Dec)
#rwl.F15Dec.ids = read.ids(rwl.F15Dec, stc = c(4, 2, 1))
#yr.F15Dec = time(rwl.F15Dec)

#rwl.F15Dec.mne = detrend(rwl.F15Dec, method = "ModNegExp")

#rwl.F15Dec.mne.sss = sss(rwl.F15Dec.mne, rwl.F15Dec.ids)
#cut.F15Dec.mne = max(yr.F15Dec[rwl.F15Dec.mne.sss < 0.85])
#yr.cut.F15Dec.mne = yr.F15Dec[yr.F15Dec > cut.F15Dec.mne]

#rwl.F15Dec.mne.crn = chron(detrend(rwl.F15Dec[yr.F15Dec > cut.F15Dec.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.F15Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Ace (Site 02)
rwl.F15Ace = read.rwl("Ring Width Chronologies/F15/F15_Acer_1.rwl")
rwl.report(rwl.F15Ace)
rwl.F15Ace.ids = read.ids(rwl.F15Ace, stc = c(4, 2, 1))
yr.F15Ace = time(rwl.F15Ace)

rwl.F15Ace.mne = detrend(rwl.F15Ace, method = "ModNegExp")

rwl.F15Ace.mne.sss = sss(rwl.F15Ace.mne, rwl.F15Ace.ids)
cut.F15Ace.mne = max(yr.F15Ace[rwl.F15Ace.mne.sss < 0.85])
yr.cut.F15Ace.mne = yr.F15Ace[yr.F15Ace > cut.F15Ace.mne]

rwl.F15Ace.mne.crn = chron(detrend(rwl.F15Ace[yr.F15Ace > cut.F15Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Car 
#rwl.F15Car = read.rwl("Ring Width Chronologies/F15/F15_Carya_1.rwl")
#rwl.report(rwl.F15Car)
#rwl.F15Car.ids = read.ids(rwl.F15Car, stc = c(4, 2, 1))
#yr.F15Car = time(rwl.F15Car)

#rwl.F15Car.mne = detrend(rwl.F15Car, method = "ModNegExp")

#rwl.F15Car.mne.sss = sss(rwl.F15Car.mne, rwl.F15Car.ids)
#cut.F15Car.mne = max(yr.F15Car[rwl.F15Car.mne.sss < 0.85])
#yr.cut.F15Car.mne = yr.F15Car[yr.F15Car > cut.F15Car.mne]

#rwl.F15Car.mne.crn = chron(detrend(rwl.F15Car[yr.F15Car > cut.F15Car.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.F15Car.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Que (Site 02)
rwl.F15Que = read.rwl("Ring Width Chronologies/F15/F15_Quercus_2.rwl")
rwl.report(rwl.F15Que)
rwl.F15Que.ids = read.ids(rwl.F15Que, stc = c(4, 2, 1))
yr.F15Que = time(rwl.F15Que)

rwl.F15Que.mne = detrend(rwl.F15Que, method = "ModNegExp")

rwl.F15Que.mne.sss = sss(rwl.F15Que.mne, rwl.F15Que.ids)
cut.F15Que.mne = max(yr.F15Que[rwl.F15Que.mne.sss < 0.85])
yr.cut.F15Que.mne = yr.F15Que[yr.F15Que > cut.F15Que.mne]

rwl.F15Que.mne.crn = chron(detrend(rwl.F15Que[yr.F15Que > cut.F15Que.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Que.mne.crn, add.spline = TRUE, nyrs = 10)

# F21Dec (Site 06)
rwl.F21Dec = read.rwl("Ring Width Chronologies/F21/F21_11.rwl")
rwl.report(rwl.F21Dec)
rwl.F21Dec.ids = read.ids(rwl.F21Dec, stc = c(4, 2, 1))
yr.F21Dec = time(rwl.F21Dec)

rwl.F21Dec.mne = detrend(rwl.F21Dec, method = "ModNegExp")

rwl.F21Dec.mne.sss = sss(rwl.F21Dec.mne, rwl.F21Dec.ids)
cut.F21Dec.mne = max(yr.F21Dec[rwl.F21Dec.mne.sss < 0.85])
yr.cut.F21Dec.mne = yr.F21Dec[yr.F21Dec > cut.F21Dec.mne]

rwl.F21Dec.mne.crn = chron(detrend(rwl.F21Dec[yr.F21Dec > cut.F21Dec.mne,],
                                   method = "ModNegExp"))
plot(rwl.F21Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# F21Ace
#rwl.F21Ace = read.rwl("Ring Width Chronologies/F21/F21_Acer_7.rwl")
#rwl.report(rwl.F21Ace)
#rwl.F21Ace.ids = read.ids(rwl.F21Ace, stc = c(4, 2, 1))
#yr.F21Ace = time(rwl.F21Ace)

#rwl.F21Ace.mne = detrend(rwl.F21Ace, method = "ModNegExp")

#rwl.F21Ace.mne.sss = sss(rwl.F21Ace.mne, rwl.F21Ace.ids)
#cut.F21Ace.mne = max(yr.F21Ace[rwl.F21Ace.mne.sss < 0.85])
#yr.cut.F21Ace.mne = yr.F21Ace[yr.F21Ace > cut.F21Ace.mne]

#rwl.F21Ace.mne.crn = chron(detrend(rwl.F21Ace[yr.F21Ace > cut.F21Ace.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.F21Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F23Ace (Site 05)
rwl.F23Ace = read.rwl("Ring Width Chronologies/F23/F23_7.rwl")
rwl.report(rwl.F23Ace)
rwl.F23Ace.ids = read.ids(rwl.F23Ace, stc = c(4, 2, 1))
yr.F23Ace = time(rwl.F23Ace)

rwl.F23Ace.mne = detrend(rwl.F23Ace, method = "ModNegExp")

rwl.F23Ace.mne.sss = sss(rwl.F23Ace.mne, rwl.F23Ace.ids)
cut.F23Ace.mne = max(yr.F23Ace[rwl.F23Ace.mne.sss < 0.85])
yr.cut.F23Ace.mne = yr.F23Ace[yr.F23Ace > cut.F23Ace.mne]

rwl.F23Ace.mne.crn = chron(detrend(rwl.F23Ace[yr.F23Ace > cut.F23Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F23Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F25Ace (Site 11)
rwl.F25Ace = read.rwl("Ring Width Chronologies/F25/F25_6.rwl")
rwl.report(rwl.F25Ace)
rwl.F25Ace.ids = read.ids(rwl.F25Ace, stc = c(4, 2, 1))
yr.F25Ace = time(rwl.F25Ace)

rwl.F25Ace.mne = detrend(rwl.F25Ace, method = "ModNegExp")

rwl.F25Ace.mne.sss = sss(rwl.F25Ace.mne, rwl.F25Ace.ids)
cut.F25Ace.mne = max(yr.F25Ace[rwl.F25Ace.mne.sss < 0.85])
yr.cut.F25Ace.mne = yr.F25Ace[yr.F25Ace > cut.F25Ace.mne]

rwl.F25Ace.mne.crn = chron(detrend(rwl.F25Ace[yr.F25Ace > cut.F25Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F25Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F30Ace (Site 15)
rwl.F30Ace = read.rwl("Ring Width Chronologies/F30/F30_Acer_5.rwl")
rwl.report(rwl.F30Ace)
rwl.F30Ace.ids = read.ids(rwl.F30Ace, stc = c(4, 2, 1))
yr.F30Ace = time(rwl.F30Ace)

rwl.F30Ace.mne = detrend(rwl.F30Ace, method = "ModNegExp")

rwl.F30Ace.mne.sss = sss(rwl.F30Ace.mne, rwl.F30Ace.ids)
cut.F30Ace.mne = max(yr.F30Ace[rwl.F30Ace.mne.sss < 0.85])
yr.cut.F30Ace.mne = yr.F30Ace[yr.F30Ace > cut.F30Ace.mne]

rwl.F30Ace.mne.crn = chron(detrend(rwl.F30Ace[yr.F30Ace > cut.F30Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F30Ace.mne.crn, add.spline = TRUE, nyrs = 10)
 
# F30Bet (Site 15)
rwl.F30Bet = read.rwl("Ring Width Chronologies/F30/F30_Betula_1.rwl")
rwl.report(rwl.F30Bet)
rwl.F30Bet.ids = read.ids(rwl.F30Bet, stc = c(4, 2, 1))
yr.F30Bet = time(rwl.F30Bet)

rwl.F30Bet.mne = detrend(rwl.F30Bet, method = "ModNegExp")

rwl.F30Bet.mne.sss = sss(rwl.F30Bet.mne, rwl.F30Bet.ids)
cut.F30Bet.mne = max(yr.F30Bet[rwl.F30Bet.mne.sss < 0.85])
yr.cut.F30Bet.mne = yr.F30Bet[yr.F30Bet > cut.F30Bet.mne]

rwl.F30Bet.mne.crn = chron(detrend(rwl.F30Bet[yr.F30Bet > cut.F30Bet.mne,],
                                   method = "ModNegExp"))
plot(rwl.F30Bet.mne.crn, add.spline = TRUE, nyrs = 10)

# F33Pic (Site 13)
rwl.F33Pic = read.rwl("Ring Width Chronologies/F33/F33_3.rwl")
rwl.report(rwl.F33Pic)
rwl.F33Pic.ids = read.ids(rwl.F33Pic, stc = c(4, 2, 1))
yr.F33Pic = time(rwl.F33Pic)

rwl.F33Pic.mne = detrend(rwl.F33Pic, method = "ModNegExp")

rwl.F33Pic.mne.sss = sss(rwl.F33Pic.mne, rwl.F33Pic.ids)
cut.F33Pic.mne = max(yr.F33Pic[rwl.F33Pic.mne.sss < 0.85])
yr.cut.F33Pic.mne = yr.F33Pic[yr.F33Pic > cut.F33Pic.mne]

rwl.F33Pic.mne.crn = chron(detrend(rwl.F33Pic[yr.F33Pic > cut.F33Pic.mne,],
                                   method = "ModNegExp"))
plot(rwl.F33Pic.mne.crn, add.spline = TRUE, nyrs = 10)

# M01Pop (Site 12)
rwl.M01Pop = read.rwl("Ring Width Chronologies/M01/M01_11.rwl")
rwl.report(rwl.M01Pop)
rwl.M01Pop.ids = read.ids(rwl.M01Pop, stc = c(4, 2, 1))
yr.M01Pop = time(rwl.M01Pop)

rwl.M01Pop.mne = detrend(rwl.M01Pop, method = "ModNegExp")

rwl.M01Pop.mne.sss = sss(rwl.M01Pop.mne, rwl.M01Pop.ids)
cut.M01Pop.mne = max(yr.M01Pop[rwl.M01Pop.mne.sss < 0.85])
yr.cut.M01Pop.mne = yr.M01Pop[yr.M01Pop > cut.M01Pop.mne]

rwl.M01Pop.mne.crn = chron(detrend(rwl.M01Pop[yr.M01Pop > cut.M01Pop.mne,],
                                   method = "ModNegExp"))
plot(rwl.M01Pop.mne.crn, add.spline = TRUE, nyrs = 10)

# M05Thu (Site 07)
rwl.M05Thu = read.rwl("Ring Width Chronologies/M05/M05_6.rwl")
rwl.report(rwl.M05Thu)
rwl.M05Thu.ids = read.ids(rwl.M05Thu, stc = c(4, 2, 1))
yr.M05Thu = time(rwl.M05Thu)

rwl.M05Thu.mne = detrend(rwl.M05Thu, method = "ModNegExp")

rwl.M05Thu.mne.sss = sss(rwl.M05Thu.mne, rwl.M05Thu.ids)
cut.M05Thu.mne = max(yr.M05Thu[rwl.M05Thu.mne.sss < 0.85])
yr.cut.M05Thu.mne = yr.M05Thu[yr.M05Thu > cut.M05Thu.mne]

rwl.M05Thu.mne.crn = chron(detrend(rwl.M05Thu[yr.M05Thu > cut.M05Thu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M05Thu.mne.crn, add.spline = TRUE, nyrs = 10)

# M06Ace (Site 03)
rwl.M06Ace = read.rwl("Ring Width Chronologies/M06/M06_Acer_1.rwl")
rwl.report(rwl.M06Ace)
rwl.M06Ace.ids = read.ids(rwl.M06Ace, stc = c(4, 2, 1))
yr.M06Ace = time(rwl.M06Ace)

rwl.M06Ace.mne = detrend(rwl.M06Ace, method = "ModNegExp")

rwl.M06Ace.mne.sss = sss(rwl.M06Ace.mne, rwl.M06Ace.ids)
cut.M06Ace.mne = max(yr.M06Ace[rwl.M06Ace.mne.sss < 0.85])
yr.cut.M06Ace.mne = yr.M06Ace[yr.M06Ace > cut.M06Ace.mne]

rwl.M06Ace.mne.crn = chron(detrend(rwl.M06Ace[yr.M06Ace > cut.M06Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.M06Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# M06Que (Site 03)
rwl.M06Que = read.rwl("Ring Width Chronologies/M06/M06_Quercus_1.rwl")
rwl.report(rwl.M06Que)
rwl.M06Que.ids = read.ids(rwl.M06Que, stc = c(4, 2, 1))
yr.M06Que = time(rwl.M06Que)

rwl.M06Que.mne = detrend(rwl.M06Que, method = "ModNegExp")

rwl.M06Que.mne.sss = sss(rwl.M06Que.mne, rwl.M06Que.ids)
cut.M06Que.mne = max(yr.M06Que[rwl.M06Que.mne.sss < 0.85])
yr.cut.M06Que.mne = yr.M06Que[yr.M06Que > cut.M06Que.mne]

rwl.M06Que.mne.crn = chron(detrend(rwl.M06Que[yr.M06Que > cut.M06Que.mne,],
                                   method = "ModNegExp"))
plot(rwl.M06Que.mne.crn, add.spline = TRUE, nyrs = 10)

# M07Tsu (Site 04)
rwl.M07Tsu = read.rwl("Ring Width Chronologies/M07/M07_Tsuga_2.rwl")
rwl.report(rwl.M07Tsu)
rwl.M07Tsu.ids = read.ids(rwl.M07Tsu, stc = c(4, 2, 1))
yr.M07Tsu = time(rwl.M07Tsu)

rwl.M07Tsu.mne = detrend(rwl.M07Tsu, method = "ModNegExp")

rwl.M07Tsu.mne.sss = sss(rwl.M07Tsu.mne, rwl.M07Tsu.ids)
cut.M07Tsu.mne = max(yr.M07Tsu[rwl.M07Tsu.mne.sss < 0.85])
yr.cut.M07Tsu.mne = yr.M07Tsu[yr.M07Tsu > cut.M07Tsu.mne]

rwl.M07Tsu.mne.crn = chron(detrend(rwl.M07Tsu[yr.M07Tsu > cut.M07Tsu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M07Tsu.mne.crn, add.spline = TRUE, nyrs = 10)

# M13Tsu (Site 01)
rwl.M13Tsu = read.rwl("Ring Width Chronologies/M13/M13_Tsuga_2.rwl")
rwl.report(rwl.M13Tsu)
rwl.M13Tsu.ids = read.ids(rwl.M13Tsu, stc = c(4, 2, 1))
yr.M13Tsu = time(rwl.M13Tsu)

rwl.M13Tsu.mne = detrend(rwl.M13Tsu, method = "ModNegExp")

rwl.M13Tsu.mne.sss = sss(rwl.M13Tsu.mne, rwl.M13Tsu.ids)
cut.M13Tsu.mne = max(yr.M13Tsu[rwl.M13Tsu.mne.sss < 0.85])
yr.cut.M13Tsu.mne = yr.M13Tsu[yr.M13Tsu > cut.M13Tsu.mne]

rwl.M13Tsu.mne.crn = chron(detrend(rwl.M13Tsu[yr.M13Tsu > cut.M13Tsu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M13Tsu.mne.crn, add.spline = TRUE, nyrs = 10)

# M17Thu (Site 16)
rwl.M17Thu = read.rwl("Ring Width Chronologies/M17/M17_1.rwl")
rwl.report(rwl.M17Thu)
rwl.M17Thu.ids = read.ids(rwl.M17Thu, stc = c(4, 2, 1))
yr.M17Thu = time(rwl.M17Thu)

rwl.M17Thu.mne = detrend(rwl.M17Thu, method = "ModNegExp")

rwl.M17Thu.mne.sss = sss(rwl.M17Thu.mne, rwl.M17Thu.ids)
cut.M17Thu.mne = max(yr.M17Thu[rwl.M17Thu.mne.sss < 0.85])
yr.cut.M17Thu.mne = yr.M17Thu[yr.M17Thu > cut.M17Thu.mne]

rwl.M17Thu.mne.crn = chron(detrend(rwl.M17Thu[yr.M17Thu > cut.M17Thu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M17Thu.mne.crn, add.spline = TRUE, nyrs = 10)

# M20Con
#rwl.M20Con = read.rwl("Ring Width Chronologies/M20/M20_3.rwl")
#rwl.report(rwl.M20Con)
#rwl.M20Con.ids = read.ids(rwl.M20Con, stc = c(4, 2, 1))
#yr.M20Con = time(rwl.M20Con)

#rwl.M20Con.mne = detrend(rwl.M20Con, method = "ModNegExp")

#rwl.M20Con.mne.sss = sss(rwl.M20Con.mne, rwl.M20Con.ids)
#cut.M20Con.mne = max(yr.M20Con[rwl.M20Con.mne.sss < 0.85])
#yr.cut.M20Con.mne = yr.M20Con[yr.M20Con > cut.M20Con.mne]

#rwl.M20Con.mne.crn = chron(detrend(rwl.M20Con[yr.M20Con > cut.M20Con.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.M20Con.mne.crn, add.spline = TRUE, nyrs = 10)

# M20Thu (Site 14)
rwl.M20Thu = read.rwl("Ring Width Chronologies/M20/M20_Thuja_3.rwl")
rwl.report(rwl.M20Thu)
rwl.M20Thu.ids = read.ids(rwl.M20Thu, stc = c(4, 2, 1))
yr.M20Thu = time(rwl.M20Thu)

rwl.M20Thu.mne = detrend(rwl.M20Thu, method = "ModNegExp")

rwl.M20Thu.mne.sss = sss(rwl.M20Thu.mne, rwl.M20Thu.ids)
cut.M20Thu.mne = max(yr.M20Thu[rwl.M20Thu.mne.sss < 0.85])
yr.cut.M20Thu.mne = yr.M20Thu[yr.M20Thu > cut.M20Thu.mne]

rwl.M20Thu.mne.crn = chron(detrend(rwl.M20Thu[yr.M20Thu > cut.M20Thu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M20Thu.mne.crn, add.spline = TRUE, nyrs = 10)

# M26Dec (Site 10)
rwl.M26Dec = read.rwl("Ring Width Chronologies/M26/M26_Deciduous_1.rwl")
rwl.report(rwl.M26Dec)
rwl.M26Dec.ids = read.ids(rwl.M26Dec, stc = c(4, 2, 1))
yr.M26Dec = time(rwl.M26Dec)

rwl.M26Dec.mne = detrend(rwl.M26Dec, method = "ModNegExp")

rwl.M26Dec.mne.sss = sss(rwl.M26Dec.mne, rwl.M26Dec.ids)
cut.M26Dec.mne = max(yr.M26Dec[rwl.M26Dec.mne.sss < 0.85])
yr.cut.M26Dec.mne = yr.M26Dec[yr.M26Dec > cut.M26Dec.mne]

rwl.M26Dec.mne.crn = chron(detrend(rwl.M26Dec[yr.M26Dec > cut.M26Dec.mne,],
                                   method = "ModNegExp"))
plot(rwl.M26Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# M26Ace
#rwl.M26Ace = read.rwl("Ring Width Chronologies/M26/M26_Acer_1.rwl")
#rwl.report(rwl.M26Ace)
#rwl.M26Ace.ids = read.ids(rwl.M26Ace, stc = c(4, 2, 1))
#yr.M26Ace = time(rwl.M26Ace)

#rwl.M26Ace.mne = detrend(rwl.M26Ace, method = "ModNegExp")

#rwl.M26Ace.mne.sss = sss(rwl.M26Ace.mne, rwl.M26Ace.ids)
#cut.M26Ace.mne = max(yr.M26Ace[rwl.M26Ace.mne.sss < 0.85])
#yr.cut.M26Ace.mne = yr.M26Ace[yr.M26Ace > cut.M26Ace.mne]

#rwl.M26Ace.mne.crn = chron(detrend(rwl.M26Ace[yr.M26Ace > cut.M26Ace.mne,],
#                                   method = "ModNegExp"))
#plot(rwl.M26Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# M26Thu (Site 10)
rwl.M26Thu = read.rwl("Ring Width Chronologies/M26/M26_Thuja_1.rwl")
rwl.report(rwl.M26Thu)
rwl.M26Thu.ids = read.ids(rwl.M26Thu, stc = c(4, 2, 1))
yr.M26Thu = time(rwl.M26Thu)

rwl.M26Thu.mne = detrend(rwl.M26Thu, method = "ModNegExp")

rwl.M26Thu.mne.sss = sss(rwl.M26Thu.mne, rwl.M26Thu.ids)
cut.M26Thu.mne = max(yr.M26Thu[rwl.M26Thu.mne.sss < 0.85])
yr.cut.M26Thu.mne = yr.M26Thu[yr.M26Thu > cut.M26Thu.mne]

rwl.M26Thu.mne.crn = chron(detrend(rwl.M26Thu[yr.M26Thu > cut.M26Thu.mne,],
                                   method = "ModNegExp"))
plot(rwl.M26Thu.mne.crn, add.spline = TRUE, nyrs = 10)

# M27Pin (Site 09)
rwl.M27Pin = read.rwl("Ring Width Chronologies/M27/M27_6.rwl")
rwl.report(rwl.M27Pin)
rwl.M27Pin.ids = read.ids(rwl.M27Pin, stc = c(4, 2, 1))
yr.M27Pin = time(rwl.M27Pin)

rwl.M27Pin.mne = detrend(rwl.M27Pin, method = "ModNegExp")

rwl.M27Pin.mne.sss = sss(rwl.M27Pin.mne, rwl.M27Pin.ids)
cut.M27Pin.mne = max(yr.M27Pin[rwl.M27Pin.mne.sss < 0.85])
yr.cut.M27Pin.mne = yr.M27Pin[yr.M27Pin > cut.M27Pin.mne]

rwl.M27Pin.mne.crn = chron(detrend(rwl.M27Pin[yr.M27Pin > cut.M27Pin.mne,],
                                   method = "ModNegExp"))
plot(rwl.M27Pin.mne.crn, add.spline = TRUE, nyrs = 10)
#####

##### Bring in %CC LTS #####
#####
# F07
lts.F07 = read.csv("Ring Width Chronologies/F07/DetrendingLandsat/F07_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F07), lts.F07$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F07), lts.F07$Avg_CC_Fitted)

# F15
lts.F15 = read.csv("Ring Width Chronologies/F15/DetrendingLandsat/F15_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F15), lts.F15$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F15), lts.F15$Avg_CC_Fitted)

# F21
lts.F21 = read.csv("Ring Width Chronologies/F21/DetrendingLandsat/F21_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F21), lts.F21$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F21), lts.F21$Avg_CC_Fitted)

# F23
lts.F23 = read.csv("Ring Width Chronologies/F23/DetrendingLandsat/F23_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F23), lts.F23$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F23), lts.F23$Avg_CC_Fitted)

# F25
lts.F25 = read.csv("Ring Width Chronologies/F25/DetrendingLandsat/F25_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F25), lts.F25$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F25), lts.F25$Avg_CC_Fitted)

# F30
lts.F30 = read.csv("Ring Width Chronologies/F30/DetrendingLandsat/F30_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F30), lts.F30$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F30), lts.F30$Avg_CC_Fitted)

# F33
lts.F33 = read.csv("Ring Width Chronologies/F33/DetrendingLandsat/F33_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F33), lts.F33$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F33), lts.F33$Avg_CC_Fitted)

# M01
lts.M01 = read.csv("Ring Width Chronologies/M01/DetrendingLandsat/M01_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M01), lts.M01$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M01), lts.M01$Avg_CC_Fitted)

# M05
lts.M05 = read.csv("Ring Width Chronologies/M05/DetrendingLandsat/M05_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M05), lts.M05$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M05), lts.M05$Avg_CC_Fitted)

# M06
lts.M06 = read.csv("Ring Width Chronologies/M06/DetrendingLandsat/M06_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M06), lts.M06$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M06), lts.M06$Avg_CC_Fitted)

# M07
lts.M07 = read.csv("Ring Width Chronologies/M07/DetrendingLandsat/M07_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M07), lts.M07$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M07), lts.M07$Avg_CC_Fitted)

# M13
lts.M13 = read.csv("Ring Width Chronologies/M13/DetrendingLandsat/M13_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M13), lts.M13$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M13), lts.M13$Avg_CC_Fitted)

# M17
lts.M17 = read.csv("Ring Width Chronologies/M17/DetrendingLandsat/M17_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M17), lts.M17$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M17), lts.M17$Avg_CC_Fitted)

# M20
lts.M20 = read.csv("Ring Width Chronologies/M20/DetrendingLandsat/M20_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M20), lts.M20$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M20), lts.M20$Avg_CC_Fitted)

# M26
lts.M26 = read.csv("Ring Width Chronologies/M26/DetrendingLandsat/M26_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M26), lts.M26$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M26), lts.M26$Avg_CC_Fitted)

# M27
lts.M27 = read.csv("Ring Width Chronologies/M27/DetrendingLandsat/M27_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M27), lts.M27$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M27), lts.M27$Avg_CC_Fitted)
#####

### Neff function ###
calc.neff <- function(x,y){
  x.ar1 = acf(x,plot=F)
  sig.lvl = qnorm((1 + 0.95)/2)/sqrt(x.ar1$n.used)
  x.ar1 = x.ar1$acf[2,1,1]
  x.ar1 = ifelse(x.ar1 < sig.lvl, 0, x.ar1)
  
  y.ar1 = acf(y,plot=F)
  sig.lvl = qnorm((1 + 0.9)/2)/sqrt(y.ar1$n.used)
  y.ar1 = y.ar1$acf[2,1,1]
  y.ar1 = ifelse(y.ar1 < sig.lvl, 0, y.ar1)
  
  n <- length(x)
  neff <- round(n*(1-x.ar1*y.ar1)/(1+x.ar1*y.ar1)) # originally floor()
  neff
} 
# Finds first order autocorrelation, finds autocorrelation value at edge of significance
# If autocorrelation < edge autocorrelation then no penalty
# Else drop effective sample size based on size of autocorrelation (x and y)

### Adjusted cor.test function ###
my.cor.test <- function (x, y, alternative = c("two.sided", "less", "greater"),
                         method = c("pearson", "kendall", "spearman"), exact = NULL,
                         conf.level = 0.95, n = length(x), ...)
{
  alternative <- match.arg(alternative)
  method <- match.arg(method)
  DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  if (length(x) != length(y))
    stop("'x' and 'y' must have the same length")
  OK <- complete.cases(x, y)
  x <- x[OK]
  y <- y[OK]
  #    n <- length(x), added n as an input to allow for using neff
  PVAL <- NULL
  NVAL <- 0
  conf.int <- FALSE
  if (method == "pearson") {
    if (n < 3)
      stop("not enough finite observations")
    method <- "Pearson's product-moment correlation"
    names(NVAL) <- "correlation"
    r <- cor(x, y)
    df <- n - 2
    ESTIMATE <- c(cor = r)
    PARAMETER <- c(df = df)
    STATISTIC <- c(t = sqrt(df) * r/sqrt(1 - r^2))
    p <- pt(STATISTIC, df)
    if (n > 3) {
      if (!missing(conf.level) && (length(conf.level) !=
                                   1 || !is.finite(conf.level) || conf.level < 0 ||
                                   conf.level > 1))
        stop("'conf.level' must be a single number between 0 and 1")
      conf.int <- TRUE
      z <- atanh(r)
      sigma <- 1/sqrt(n - 3)
      cint <- switch(alternative, less = c(-Inf, z + sigma *
                                             qnorm(conf.level)), greater = c(z - sigma * qnorm(conf.level),
                                                                             Inf), two.sided = z + c(-1, 1) * sigma * qnorm((1 +
                                                                                                                               conf.level)/2))
      cint <- tanh(cint)
      attr(cint, "conf.level") <- conf.level
    }
  }
  else {
    if (n < 2)
      stop("not enough finite observations")
    PARAMETER <- NULL
    TIES <- (min(length(unique(x)), length(unique(y))) <
               n)
    if (method == "kendall") {
      method <- "Kendall's rank correlation tau"
      names(NVAL) <- "tau"
      r <- cor(x, y, method = "kendall")
      ESTIMATE <- c(tau = r)
      if (!is.finite(ESTIMATE)) {
        ESTIMATE[] <- NA
        STATISTIC <- c(T = NA)
        PVAL <- NA
      }
      else {
        if (is.null(exact))
          exact <- (n < 50)
        if (exact && !TIES) {
          q <- round((r + 1) * n * (n - 1)/4)
          pkendall <- function(q, n) {
            .C("pkendall", length(q), p = as.double(q),
               as.integer(n), PACKAGE = "stats")$p
          }
          PVAL <- switch(alternative, two.sided = {
            if (q > n * (n - 1)/4)
              p <- 1 - pkendall(q - 1, n)
            else p <- pkendall(q, n)
            min(2 * p, 1)
          }, greater = 1 - pkendall(q - 1, n), less = pkendall(q,
                                                               n))
          STATISTIC <- c(T = q)
        }
        else {
          xties <- table(x[duplicated(x)]) + 1
          yties <- table(y[duplicated(y)]) + 1
          T0 <- n * (n - 1)/2
          T1 <- sum(xties * (xties - 1))/2
          T2 <- sum(yties * (yties - 1))/2
          S <- r * sqrt((T0 - T1) * (T0 - T2))
          v0 <- n * (n - 1) * (2 * n + 5)
          vt <- sum(xties * (xties - 1) * (2 * xties +
                                             5))
          vu <- sum(yties * (yties - 1) * (2 * yties +
                                             5))
          v1 <- sum(xties * (xties - 1)) * sum(yties *
                                                 (yties - 1))
          v2 <- sum(xties * (xties - 1) * (xties - 2)) *
            sum(yties * (yties - 1) * (yties - 2))
          var_S <- (v0 - vt - vu)/18 + v1/(2 * n * (n -
                                                      1)) + v2/(9 * n * (n - 1) * (n - 2))
          STATISTIC <- c(z = S/sqrt(var_S))
          p <- pnorm(STATISTIC)
          if (exact && TIES)
            warning("Cannot compute exact p-value with ties")
        }
      }
    }
    else {
      method <- "Spearman's rank correlation rho"
      if (is.null(exact))
        exact <- TRUE
      names(NVAL) <- "rho"
      r <- cor(rank(x), rank(y))
      ESTIMATE <- c(rho = r)
      if (!is.finite(ESTIMATE)) {
        ESTIMATE[] <- NA
        STATISTIC <- c(S = NA)
        PVAL <- NA
      }
      else {
        pspearman <- function(q, n, lower.tail = TRUE) {
          if (n <= 1290 && exact)
            .C("prho", as.integer(n), as.double(round(q) +
                                                  lower.tail), p = double(1), integer(1),
               as.logical(lower.tail), PACKAGE = "stats")$p
          else {
            r <- 1 - 6 * q/(n * (n^2 - 1))
            pt(r/sqrt((1 - r^2)/(n - 2)), df = n - 2,
               lower.tail = !lower.tail)
          }
        }
        q <- (n^3 - n) * (1 - r)/6
        STATISTIC <- c(S = q)
        if (TIES && exact) {
          exact <- FALSE
          warning("Cannot compute exact p-values with ties")
        }
        PVAL <- switch(alternative, two.sided = {
          p <- if (q > (n^3 - n)/6)
            pspearman(q, n, lower.tail = FALSE)
          else pspearman(q, n, lower.tail = TRUE)
          min(2 * p, 1)
        }, greater = pspearman(q, n, lower.tail = TRUE),
        less = pspearman(q, n, lower.tail = FALSE))
      }
    }
  }
  if (is.null(PVAL))
    PVAL <- switch(alternative, less = p, greater = 1 - p,
                   two.sided = 2 * min(p, 1 - p))
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER,
               p.value = as.numeric(PVAL), estimate = ESTIMATE, null.value = NVAL,
               alternative = alternative, method = method, data.name = DNAME)
  if (conf.int)
    RVAL <- c(RVAL, list(conf.int = cint))
  class(RVAL) <- "htest"
  RVAL
}
# Just like cor.test but can replace n with neff

# Smoothing spline length
nyrs = 10

##### F07 Correlation #####
#####
### F07Dec ###
# Add Spline and then cutoff RWI pre-1972
#rwi.F07Dec = add_rownames(rwl.F07Dec.mne.crn, var = "year")
#rwi.F07Dec$year = as.numeric(rwi.F07Dec$year) # Years column in chr by default
#rwi.F07Dec$smooth = ffcsaps(rwi.F07Dec$xxxstd, nyrs = nyrs)
#rwi.F07Dec = subset(rwi.F07Dec, year >= 1972)

# Add Spline to LTS
#lts.F07$smooth = ffcsaps(lts.F07$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.F07Dec$year, rwi.F07Dec$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.F07Dec$year, rwi.F07Dec$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.F07$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.F07$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.F07Dec$xxxstd, lts.F07$Avg_CC_Median, alternative = "greater", 
#            method = "pearson", n = calc.neff(rwi.F07Dec$xxxstd, lts.F07$Avg_CC_Median))
#cor

# Running correlation
#width = 25 - 1 # Record 5 to 25
#end = nrow(rwi.F07Dec) - width
#start = floor(width / 2)

#rwi.F07Dec$cor = NA
#rwi.F07Dec$pvalue = NA
#for (i in 1:end) {
#   test = my.cor.test(rwi.F07Dec$xxxstd[i:(i + width)], 
#                    lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                    method = "pearson",
#                    n = calc.neff(rwi.F07Dec$xxxstd[i:(i + width)],
#                                  lts.F07$Avg_CC_Median[i:(i + width)]))
#   print(calc.neff(rwi.F07Dec$xxxstd[i:(i + width)],
#                   lts.F07$Avg_CC_Median[i:(i + width)]))
#   rwi.F07Dec$cor[i + start] = test$estimate
#   rwi.F07Dec$pvalue[i + start] = test$p.value
#}

#rwi.F07Dec$sig = NA
#for (i in 1:nrow(rwi.F07Dec)) {
#  if (!is.na(rwi.F07Dec$pvalue[i])) 
#  if (rwi.F07Dec$pvalue[i] <= 0.055) { 
#    rwi.F07Dec$sig[i] = 1
#  }
#}

#plot(rwi.F07Dec$year, rwi.F07Dec$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.F07Dec$year, rwi.F07Dec$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F07Ace (Site 08) ###
# Add Spline and then cutoff RWI pre-1972
rwi.F07Ace = add_rownames(rwl.F07Ace.mne.crn, var = "year")
rwi.F07Ace$year = as.numeric(rwi.F07Ace$year) # Years column in chr by default
rwi.F07Ace$smooth = ffcsaps(rwi.F07Ace$xxxstd, nyrs = nyrs)
rwi.F07Ace = subset(rwi.F07Ace, year >= 1972) #1971

# Add Spline to LTS
lts.F07$smooth = ffcsaps(lts.F07$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F07Ace$year, rwi.F07Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F07Ace$year, rwi.F07Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F07$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F07$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F07Ace.cor = my.cor.test(rwi.F07Ace$xxxstd, lts.F07$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F07Ace$xxxstd, lts.F07$Avg_CC_Median))
F07Ace.cor

# MSS only
F07Ace.cor1 = my.cor.test(rwi.F07Ace$xxxstd[1:12], lts.F07$Avg_CC_Median[1:12], alternative = "greater", 
                         method = "pearson", n = calc.neff(rwi.F07Ace$xxxstd[1:12], lts.F07$Avg_CC_Median[1:12]))
F07Ace.cor1

# Modern
F07Ace.cor2 = my.cor.test(rwi.F07Ace$xxxstd[13:47], lts.F07$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F07Ace$xxxstd[13:47], lts.F07$Avg_CC_Median[13:47]))
F07Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F07Ace.cor3 = my.cor.test(rwi.F07Ace$xxxstd[2:47], lts.F07$Avg_CC_Median[1:46], alternative = "greater", 
                         method = "pearson", n = calc.neff(rwi.F07Ace$xxxstd[2:47], lts.F07$Avg_CC_Median[1:46]))
F07Ace.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F07Ace.cor4 = my.cor.test(rwi.F07Ace$xxxstd[1:47], lts.F07$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F07Ace$xxxstd[1:47], lts.F07$Avg_CC_Median[1:47]))
F07Ace.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F07Ace) - width
start = floor(width / 2)

rwi.F07Ace$cor = NA
rwi.F07Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F07Ace$xxxstd[i:(i + width)], 
                     lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                                   lts.F07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                  lts.F07$Avg_CC_Median[i:(i + width)]))
  rwi.F07Ace$cor[i + start] = test$estimate
  rwi.F07Ace$pvalue[i + start] = test$p.value
}

rwi.F07Ace$sig = NA
for (i in 1:nrow(rwi.F07Ace)) {
  if (!is.na(rwi.F07Ace$pvalue[i])) 
    if (rwi.F07Ace$pvalue[i] <= 0.055) { 
      rwi.F07Ace$sig[i] = 1
    }
}

plot(rwi.F07Ace$year, rwi.F07Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F07Ace$year, rwi.F07Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F15 Correlation #####
#####
### F15Dec ###
# Add Spline and then cutoff RWI pre-1972
#rwi.F15Dec = add_rownames(rwl.F15Dec.mne.crn, var = "year")
#rwi.F15Dec$year = as.numeric(rwi.F15Dec$year) # Years column in chr by default
#rwi.F15Dec$smooth = ffcsaps(rwi.F15Dec$xxxstd, nyrs = nyrs)
#rwi.F15Dec = subset(rwi.F15Dec, year >= 1972)

# Add Spline to LTS
#lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.F15Dec$year, rwi.F15Dec$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.F15Dec$year, rwi.F15Dec$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.F15$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.F15$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.F15Dec$xxxstd, lts.F15$Avg_CC_Median, alternative = "greater", 
#                  method = "pearson", n = calc.neff(rwi.F15Dec$xxxstd, lts.F15$Avg_CC_Median))
#cor

# Running correlation
#width = 35 - 1 # Record 5 to 25
#end = nrow(rwi.F15Dec) - width
#start = floor(width / 2)

#rwi.F15Dec$cor = NA
#rwi.F15Dec$pvalue = NA
#for (i in 1:end) {
#  test = my.cor.test(rwi.F15Dec$xxxstd[i:(i + width)], 
#                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                     method = "pearson",
#                     n = calc.neff(rwi.F15Dec$xxxstd[i:(i + width)],
#                                   lts.F15$Avg_CC_Median[i:(i + width)]))
#  print(calc.neff(rwi.F15Dec$xxxstd[i:(i + width)],
#                  lts.F15$Avg_CC_Median[i:(i + width)]))
#  rwi.F15Dec$cor[i + start] = test$estimate
#  rwi.F15Dec$pvalue[i + start] = test$p.value
#}

#rwi.F15Dec$sig = NA
#for (i in 1:nrow(rwi.F15Dec)) {
#  if (!is.na(rwi.F15Dec$pvalue[i])) 
#    if (rwi.F15Dec$pvalue[i] <= 0.055) { 
#      rwi.F15Dec$sig[i] = 1
#    }
#}

#plot(rwi.F15Dec$year, rwi.F15Dec$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.F15Dec$year, rwi.F15Dec$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F15Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F15Ace = add_rownames(rwl.F15Ace.mne.crn, var = "year")
rwi.F15Ace$year = as.numeric(rwi.F15Ace$year) # Years column in chr by default
rwi.F15Ace$smooth = ffcsaps(rwi.F15Ace$xxxstd, nyrs = nyrs)
rwi.F15Ace = subset(rwi.F15Ace, year >= 1972)

# Add Spline to LTS
lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F15Ace$year, rwi.F15Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F15Ace$year, rwi.F15Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F15$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F15$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F15Ace.cor = my.cor.test(rwi.F15Ace$xxxstd, lts.F15$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F15Ace$xxxstd, lts.F15$Avg_CC_Median))
F15Ace.cor

# MSS only
F15Ace.cor1 = my.cor.test(rwi.F15Ace$xxxstd[1:12], lts.F15$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Ace$xxxstd[1:12], lts.F15$Avg_CC_Median[1:12]))
F15Ace.cor1

# Modern
F15Ace.cor2 = my.cor.test(rwi.F15Ace$xxxstd[13:47], lts.F15$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Ace$xxxstd[13:47], lts.F15$Avg_CC_Median[13:47]))
F15Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F15Ace.cor3 = my.cor.test(rwi.F15Ace$xxxstd[2:47], lts.F15$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Ace$xxxstd[2:47], lts.F15$Avg_CC_Median[1:46]))
F15Ace.cor3

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F15Ace) - width
start = floor(width / 2)

rwi.F15Ace$cor = NA
rwi.F15Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Ace$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Ace$cor[i + start] = test$estimate
  rwi.F15Ace$pvalue[i + start] = test$p.value
}

rwi.F15Ace$sig = NA
for (i in 1:nrow(rwi.F15Ace)) {
  if (!is.na(rwi.F15Ace$pvalue[i])) 
    if (rwi.F15Ace$pvalue[i] <= 0.055) { 
      rwi.F15Ace$sig[i] = 1
    }
}

plot(rwi.F15Ace$year, rwi.F15Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F15Ace$year, rwi.F15Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F15Car ###
# Add Spline and then cutoff RWI pre-1972
#rwi.F15Car = add_rownames(rwl.F15Car.mne.crn, var = "year")
#rwi.F15Car$year = as.numeric(rwi.F15Car$year) # Years column in chr by default
#rwi.F15Car$smooth = ffcsaps(rwi.F15Car$xxxstd, nyrs = nyrs)
#rwi.F15Car = subset(rwi.F15Car, year >= 1972)

# Add Spline to LTS
#lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.F15Car$year, rwi.F15Car$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.F15Car$year, rwi.F15Car$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.F15$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.F15$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.F15Car$xxxstd, lts.F15$Avg_CC_Median, alternative = "greater", 
#                  method = "pearson", n = calc.neff(rwi.F15Car$xxxstd, lts.F15$Avg_CC_Median))
#cor

# Running correlation
#width = 25 - 1 # Record 5 to 25
#end = nrow(rwi.F15Car) - width
#start = floor(width / 2)

#rwi.F15Car$cor = NA
#rwi.F15Car$pvalue = NA
#for (i in 1:end) {
#  test = my.cor.test(rwi.F15Car$xxxstd[i:(i + width)], 
#                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                     method = "pearson",
#                     n = calc.neff(rwi.F15Car$xxxstd[i:(i + width)],
#                                   lts.F15$Avg_CC_Median[i:(i + width)]))
#  print(calc.neff(rwi.F15Car$xxxstd[i:(i + width)],
#                  lts.F15$Avg_CC_Median[i:(i + width)]))
#  rwi.F15Car$cor[i + start] = test$estimate
#  rwi.F15Car$pvalue[i + start] = test$p.value
#}

#rwi.F15Car$sig = NA
#for (i in 1:nrow(rwi.F15Car)) {
#  if (!is.na(rwi.F15Car$pvalue[i])) 
#    if (rwi.F15Car$pvalue[i] <= 0.055) { 
#      rwi.F15Car$sig[i] = 1
#    }
#}

#plot(rwi.F15Car$year, rwi.F15Car$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.F15Car$year, rwi.F15Car$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F15Que ###
# Add Spline and then cutoff RWI pre-1972
rwi.F15Que = add_rownames(rwl.F15Que.mne.crn, var = "year")
rwi.F15Que$year = as.numeric(rwi.F15Que$year) # Years column in chr by default
rwi.F15Que$smooth = ffcsaps(rwi.F15Que$xxxstd, nyrs = nyrs)
rwi.F15Que = subset(rwi.F15Que, year >= 1972) #1971

# Add Spline to LTS
lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F15Que$year, rwi.F15Que$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F15Que$year, rwi.F15Que$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F15$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F15$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F15Que.cor = my.cor.test(rwi.F15Que$xxxstd, lts.F15$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F15Que$xxxstd, lts.F15$Avg_CC_Median))
F15Que.cor

# MSS only
F15Que.cor1 = my.cor.test(rwi.F15Que$xxxstd[1:12], lts.F15$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Que$xxxstd[1:12], lts.F15$Avg_CC_Median[1:12]))
F15Que.cor1

# Modern
F15Que.cor2 = my.cor.test(rwi.F15Que$xxxstd[13:47], lts.F15$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Que$xxxstd[13:47], lts.F15$Avg_CC_Median[13:47]))
F15Que.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F15Que.cor3 = my.cor.test(rwi.F15Que$xxxstd[2:47], lts.F15$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Que$xxxstd[2:47], lts.F15$Avg_CC_Median[1:46]))
F15Que.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F15Que.cor4 = my.cor.test(rwi.F15Que$xxxstd[1:47], lts.F15$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F15Que$xxxstd[1:47], lts.F15$Avg_CC_Median[1:47]))
F15Que.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F15Que) - width
start = floor(width / 2)

rwi.F15Que$cor = NA
rwi.F15Que$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Que$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Que$cor[i + start] = test$estimate
  rwi.F15Que$pvalue[i + start] = test$p.value
}

rwi.F15Que$sig = NA
for (i in 1:nrow(rwi.F15Que)) {
  if (!is.na(rwi.F15Que$pvalue[i])) 
    if (rwi.F15Que$pvalue[i] <= 0.055) { 
      rwi.F15Que$sig[i] = 1
    }
}

plot(rwi.F15Que$year, rwi.F15Que$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F15Que$year, rwi.F15Que$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F21 Correlation #####
#####
### F21Dec ###
# Add Spline and then cutoff RWI pre-1972
rwi.F21Dec = add_rownames(rwl.F21Dec.mne.crn, var = "year")
rwi.F21Dec$year = as.numeric(rwi.F21Dec$year) # Years column in chr by default
rwi.F21Dec$smooth = ffcsaps(rwi.F21Dec$xxxstd, nyrs = nyrs)
rwi.F21Dec = subset(rwi.F21Dec, year >= 1972) #1971

# Add Spline to LTS
lts.F21$smooth = ffcsaps(lts.F21$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F21Dec$year, rwi.F21Dec$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F21Dec$year, rwi.F21Dec$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F21$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F21$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F21Dec.cor = my.cor.test(rwi.F21Dec$xxxstd, lts.F21$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F21Dec$xxxstd, lts.F21$Avg_CC_Median))
F21Dec.cor

# MSS only
F21Dec.cor1 = my.cor.test(rwi.F21Dec$xxxstd[1:12], lts.F21$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F21Dec$xxxstd[1:12], lts.F21$Avg_CC_Median[1:12]))
F21Dec.cor1

# Modern
F21Dec.cor2 = my.cor.test(rwi.F21Dec$xxxstd[13:47], lts.F21$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F21Dec$xxxstd[13:47], lts.F21$Avg_CC_Median[13:47]))
F21Dec.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F21Dec.cor3 = my.cor.test(rwi.F21Dec$xxxstd[2:47], lts.F21$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F21Dec$xxxstd[2:47], lts.F21$Avg_CC_Median[1:46]))
F21Dec.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F21Dec.cor4 = my.cor.test(rwi.F21Dec$xxxstd[1:47], lts.F21$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F21Dec$xxxstd[1:47], lts.F21$Avg_CC_Median[1:47]))
F21Dec.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F21Dec) - width
start = floor(width / 2)

rwi.F21Dec$cor = NA
rwi.F21Dec$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F21Dec$xxxstd[i:(i + width)], 
                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                                   lts.F21$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                  lts.F21$Avg_CC_Median[i:(i + width)]))
  rwi.F21Dec$cor[i + start] = test$estimate
  rwi.F21Dec$pvalue[i + start] = test$p.value
}

rwi.F21Dec$sig = NA
for (i in 1:nrow(rwi.F21Dec)) {
  if (!is.na(rwi.F21Dec$pvalue[i])) 
    if (rwi.F21Dec$pvalue[i] <= 0.055) { 
      rwi.F21Dec$sig[i] = 1
    }
}

plot(rwi.F21Dec$year, rwi.F21Dec$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F21Dec$year, rwi.F21Dec$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F21Ace ###
# Add Spline and then cutoff RWI pre-1972
#rwi.F21Ace = add_rownames(rwl.F21Ace.mne.crn, var = "year")
#rwi.F21Ace$year = as.numeric(rwi.F21Ace$year) # Years column in chr by default
#rwi.F21Ace$smooth = ffcsaps(rwi.F21Ace$xxxstd, nyrs = nyrs)
#rwi.F21Ace = subset(rwi.F21Ace, year >= 1972)

# Add Spline to LTS
#lts.F21$smooth = ffcsaps(lts.F21$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.F21Ace$year, rwi.F21Ace$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.F21Ace$year, rwi.F21Ace$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.F21$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.F21$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.F21Ace$xxxstd, lts.F21$Avg_CC_Median, alternative = "greater", 
#                  method = "pearson", n = calc.neff(rwi.F21Ace$xxxstd, lts.F21$Avg_CC_Median))
#cor

# Running correlation
#width = 25 - 1 # Record 5 to 25
#end = nrow(rwi.F21Ace) - width
#start = floor(width / 2)

#rwi.F21Ace$cor = NA
#rwi.F21Ace$pvalue = NA
#for (i in 1:end) {
#  test = my.cor.test(rwi.F21Ace$xxxstd[i:(i + width)], 
#                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                     method = "pearson",
#                     n = calc.neff(rwi.F21Ace$xxxstd[i:(i + width)],
#                                   lts.F21$Avg_CC_Median[i:(i + width)]))
#  print(calc.neff(rwi.F21Ace$xxxstd[i:(i + width)],
#                  lts.F21$Avg_CC_Median[i:(i + width)]))
#  rwi.F21Ace$cor[i + start] = test$estimate
#  rwi.F21Ace$pvalue[i + start] = test$p.value
#}

#rwi.F21Ace$sig = NA
#for (i in 1:nrow(rwi.F21Ace)) {
#  if (!is.na(rwi.F21Ace$pvalue[i])) 
#    if (rwi.F21Ace$pvalue[i] <= 0.055) { 
#      rwi.F21Ace$sig[i] = 1
#    }
#}

#plot(rwi.F21Ace$year, rwi.F21Ace$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.F21Ace$year, rwi.F21Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F23 Correlation #####
#####
### F23Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F23Ace = add_rownames(rwl.F23Ace.mne.crn, var = "year")
rwi.F23Ace$year = as.numeric(rwi.F23Ace$year) # Years column in chr by default
rwi.F23Ace$smooth = ffcsaps(rwi.F23Ace$xxxstd, nyrs = nyrs)
rwi.F23Ace = subset(rwi.F23Ace, year >= 1972) #1971

# Add Spline to LTS
lts.F23$smooth = ffcsaps(lts.F23$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F23Ace$year, rwi.F23Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F23Ace$year, rwi.F23Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F23$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F23$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F23Ace.cor = my.cor.test(rwi.F23Ace$xxxstd, lts.F23$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F23Ace$xxxstd, lts.F23$Avg_CC_Median))
F23Ace.cor

# MSS only
F23Ace.cor1 = my.cor.test(rwi.F23Ace$xxxstd[1:12], lts.F23$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F23Ace$xxxstd[1:12], lts.F23$Avg_CC_Median[1:12]))
F23Ace.cor1

# Modern
F23Ace.cor2 = my.cor.test(rwi.F23Ace$xxxstd[13:47], lts.F23$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F23Ace$xxxstd[13:47], lts.F23$Avg_CC_Median[13:47]))
F23Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F23Ace.cor3 = my.cor.test(rwi.F23Ace$xxxstd[2:47], lts.F23$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F23Ace$xxxstd[2:47], lts.F23$Avg_CC_Median[1:46]))
F23Ace.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F23Ace.cor4 = my.cor.test(rwi.F23Ace$xxxstd[1:47], lts.F23$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F23Ace$xxxstd[1:47], lts.F23$Avg_CC_Median[1:47]))
F23Ace.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F23Ace) - width
start = floor(width / 2)

rwi.F23Ace$cor = NA
rwi.F23Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F23Ace$xxxstd[i:(i + width)], 
                     lts.F23$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                                   lts.F23$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                  lts.F23$Avg_CC_Median[i:(i + width)]))
  rwi.F23Ace$cor[i + start] = test$estimate
  rwi.F23Ace$pvalue[i + start] = test$p.value
}

rwi.F23Ace$sig = NA
for (i in 1:nrow(rwi.F23Ace)) {
  if (!is.na(rwi.F23Ace$pvalue[i])) 
    if (rwi.F23Ace$pvalue[i] <= 0.055) { 
      rwi.F23Ace$sig[i] = 1
    }
}

plot(rwi.F23Ace$year, rwi.F23Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F23Ace$year, rwi.F23Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F25 Correlation #####
#####
### F25Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F25Ace = add_rownames(rwl.F25Ace.mne.crn, var = "year")
rwi.F25Ace$year = as.numeric(rwi.F25Ace$year) # Years column in chr by default
rwi.F25Ace$smooth = ffcsaps(rwi.F25Ace$xxxstd, nyrs = nyrs)
rwi.F25Ace = subset(rwi.F25Ace, year >= 1972) #1971

# Add Spline to LTS
lts.F25$smooth = ffcsaps(lts.F25$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F25Ace$year, rwi.F25Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F25Ace$year, rwi.F25Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F25$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F25$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F25Ace.cor = my.cor.test(rwi.F25Ace$xxxstd, lts.F25$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F25Ace$xxxstd, lts.F25$Avg_CC_Median))
F25Ace.cor

# MSS only
F25Ace.cor1 = my.cor.test(rwi.F25Ace$xxxstd[1:12], lts.F25$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F25Ace$xxxstd[1:12], lts.F25$Avg_CC_Median[1:12]))
F25Ace.cor1

# Modern
F25Ace.cor2 = my.cor.test(rwi.F25Ace$xxxstd[13:47], lts.F25$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F25Ace$xxxstd[13:47], lts.F25$Avg_CC_Median[13:47]))
F25Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F25Ace.cor3 = my.cor.test(rwi.F25Ace$xxxstd[2:47], lts.F25$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F25Ace$xxxstd[2:47], lts.F25$Avg_CC_Median[1:46]))
F25Ace.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F25Ace.cor4 = my.cor.test(rwi.F25Ace$xxxstd[1:47], lts.F25$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F25Ace$xxxstd[1:47], lts.F25$Avg_CC_Median[1:47]))
F25Ace.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F25Ace) - width
start = floor(width / 2)

rwi.F25Ace$cor = NA
rwi.F25Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F25Ace$xxxstd[i:(i + width)], 
                     lts.F25$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                                   lts.F25$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                  lts.F25$Avg_CC_Median[i:(i + width)]))
  rwi.F25Ace$cor[i + start] = test$estimate
  rwi.F25Ace$pvalue[i + start] = test$p.value
}

rwi.F25Ace$sig = NA
for (i in 1:nrow(rwi.F25Ace)) {
  if (!is.na(rwi.F25Ace$pvalue[i])) 
    if (rwi.F25Ace$pvalue[i] <= 0.055) { 
      rwi.F25Ace$sig[i] = 1
    }
}

plot(rwi.F25Ace$year, rwi.F25Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F25Ace$year, rwi.F25Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F30 Correlation #####
#####
### F30Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F30Ace = add_rownames(rwl.F30Ace.mne.crn, var = "year")
rwi.F30Ace$year = as.numeric(rwi.F30Ace$year) # Years column in chr by default
rwi.F30Ace$smooth = ffcsaps(rwi.F30Ace$xxxstd, nyrs = nyrs)
rwi.F30Ace = subset(rwi.F30Ace, year >= 1972) #1971

# Add Spline to LTS
lts.F30$smooth = ffcsaps(lts.F30$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F30Ace$year, rwi.F30Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F30Ace$year, rwi.F30Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F30$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F30$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F30Ace.cor = my.cor.test(rwi.F30Ace$xxxstd, lts.F30$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F30Ace$xxxstd, lts.F30$Avg_CC_Median))
F30Ace.cor

# MSS only
F30Ace.cor1 = my.cor.test(rwi.F30Ace$xxxstd[1:12], lts.F30$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Ace$xxxstd[1:12], lts.F30$Avg_CC_Median[1:12]))
F30Ace.cor1

# Modern
F30Ace.cor2 = my.cor.test(rwi.F30Ace$xxxstd[13:47], lts.F30$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Ace$xxxstd[13:47], lts.F30$Avg_CC_Median[13:47]))
F30Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F30Ace.cor3 = my.cor.test(rwi.F30Ace$xxxstd[2:47], lts.F30$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Ace$xxxstd[2:47], lts.F30$Avg_CC_Median[1:46]))
F30Ace.cor3

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F30Ace) - width
start = floor(width / 2)

rwi.F30Ace$cor = NA
rwi.F30Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Ace$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Ace$cor[i + start] = test$estimate
  rwi.F30Ace$pvalue[i + start] = test$p.value
}

rwi.F30Ace$sig = NA
for (i in 1:nrow(rwi.F30Ace)) {
  if (!is.na(rwi.F30Ace$pvalue[i])) 
    if (rwi.F30Ace$pvalue[i] <= 0.055) { 
      rwi.F30Ace$sig[i] = 1
    }
}

plot(rwi.F30Ace$year, rwi.F30Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F30Ace$year, rwi.F30Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### F30Bet ###
# Add Spline and then cutoff RWI pre-1972
rwi.F30Bet = add_rownames(rwl.F30Bet.mne.crn, var = "year")
rwi.F30Bet$year = as.numeric(rwi.F30Bet$year) # Years column in chr by default
rwi.F30Bet$smooth = ffcsaps(rwi.F30Bet$xxxstd, nyrs = nyrs)
rwi.F30Bet = subset(rwi.F30Bet, year >= 1972) #1971

# Add Spline to LTS
lts.F30$smooth = ffcsaps(lts.F30$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F30Bet$year, rwi.F30Bet$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F30Bet$year, rwi.F30Bet$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F30$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F30$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F30Bet.cor = my.cor.test(rwi.F30Bet$xxxstd, lts.F30$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F30Bet$xxxstd, lts.F30$Avg_CC_Median))
F30Bet.cor

# MSS only
F30Bet.cor1 = my.cor.test(rwi.F30Bet$xxxstd[1:12], lts.F30$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Bet$xxxstd[1:12], lts.F30$Avg_CC_Median[1:12]))
F30Bet.cor1

# Modern
F30Bet.cor2 = my.cor.test(rwi.F30Bet$xxxstd[13:47], lts.F30$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Bet$xxxstd[13:47], lts.F30$Avg_CC_Median[13:47]))
F30Bet.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F30Bet.cor3 = my.cor.test(rwi.F30Bet$xxxstd[2:47], lts.F30$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Bet$xxxstd[2:47], lts.F30$Avg_CC_Median[1:46]))
F30Bet.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F30Bet.cor4 = my.cor.test(rwi.F30Bet$xxxstd[1:47], lts.F30$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F30Bet$xxxstd[1:47], lts.F30$Avg_CC_Median[1:47]))
F30Bet.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F30Bet) - width
start = floor(width / 2)

rwi.F30Bet$cor = NA
rwi.F30Bet$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Bet$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Bet$cor[i + start] = test$estimate
  rwi.F30Bet$pvalue[i + start] = test$p.value
}

rwi.F30Bet$sig = NA
for (i in 1:nrow(rwi.F30Bet)) {
  if (!is.na(rwi.F30Bet$pvalue[i])) 
    if (rwi.F30Bet$pvalue[i] <= 0.055) { 
      rwi.F30Bet$sig[i] = 1
    }
}

plot(rwi.F30Bet$year, rwi.F30Bet$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F30Bet$year, rwi.F30Bet$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### F33 Correlation #####
#####
### F33Pic ###
# Add Spline and then cutoff RWI pre-1972
rwi.F33Pic = add_rownames(rwl.F33Pic.mne.crn, var = "year")
rwi.F33Pic$year = as.numeric(rwi.F33Pic$year) # Years column in chr by default
rwi.F33Pic$smooth = ffcsaps(rwi.F33Pic$xxxstd, nyrs = nyrs)
rwi.F33Pic = subset(rwi.F33Pic, year >= 1972) #1971

# Add Spline to LTS
lts.F33$smooth = ffcsaps(lts.F33$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.F33Pic$year, rwi.F33Pic$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.F33Pic$year, rwi.F33Pic$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.F33$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.F33$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
F33Pic.cor = my.cor.test(rwi.F33Pic$xxxstd, lts.F33$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.F33Pic$xxxstd, lts.F33$Avg_CC_Median))
F33Pic.cor

# MSS only
F33Pic.cor1 = my.cor.test(rwi.F33Pic$xxxstd[1:12], lts.F33$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F33Pic$xxxstd[1:12], lts.F33$Avg_CC_Median[1:12]))
F33Pic.cor1

# Modern
F33Pic.cor2 = my.cor.test(rwi.F33Pic$xxxstd[13:47], lts.F33$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F33Pic$xxxstd[13:47], lts.F33$Avg_CC_Median[13:47]))
F33Pic.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
F33Pic.cor3 = my.cor.test(rwi.F33Pic$xxxstd[2:47], lts.F33$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F33Pic$xxxstd[2:47], lts.F33$Avg_CC_Median[1:46]))
F33Pic.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
F33Pic.cor4 = my.cor.test(rwi.F33Pic$xxxstd[1:47], lts.F33$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.F33Pic$xxxstd[1:47], lts.F33$Avg_CC_Median[1:47]))
F33Pic.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.F33Pic) - width
start = floor(width / 2)

rwi.F33Pic$cor = NA
rwi.F33Pic$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F33Pic$xxxstd[i:(i + width)], 
                     lts.F33$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                                   lts.F33$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                  lts.F33$Avg_CC_Median[i:(i + width)]))
  rwi.F33Pic$cor[i + start] = test$estimate
  rwi.F33Pic$pvalue[i + start] = test$p.value
}

rwi.F33Pic$sig = NA
for (i in 1:nrow(rwi.F33Pic)) {
  if (!is.na(rwi.F33Pic$pvalue[i])) 
    if (rwi.F33Pic$pvalue[i] <= 0.055) { 
      rwi.F33Pic$sig[i] = 1
    }
}

plot(rwi.F33Pic$year, rwi.F33Pic$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.F33Pic$year, rwi.F33Pic$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M01 Correlation #####
#####
### M01Pop ###
# Add Spline and then cutoff RWI pre-1972
rwi.M01Pop = add_rownames(rwl.M01Pop.mne.crn, var = "year")
rwi.M01Pop$year = as.numeric(rwi.M01Pop$year) # Years column in chr by default
rwi.M01Pop$smooth = ffcsaps(rwi.M01Pop$xxxstd, nyrs = nyrs)
rwi.M01Pop = subset(rwi.M01Pop, year >= 1972) #1971

# Add Spline to LTS
lts.M01$smooth = ffcsaps(lts.M01$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M01Pop$year, rwi.M01Pop$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M01Pop$year, rwi.M01Pop$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M01$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M01$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M01Pop.cor = my.cor.test(rwi.M01Pop$xxxstd, lts.M01$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M01Pop$xxxstd, lts.M01$Avg_CC_Median))
M01Pop.cor

# MSS only
M01Pop.cor1 = my.cor.test(rwi.M01Pop$xxxstd[1:12], lts.M01$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M01Pop$xxxstd[1:12], lts.M01$Avg_CC_Median[1:12]))
M01Pop.cor1

# Modern
M01Pop.cor2 = my.cor.test(rwi.M01Pop$xxxstd[13:47], lts.M01$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M01Pop$xxxstd[13:47], lts.M01$Avg_CC_Median[13:47]))
M01Pop.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M01Pop.cor3 = my.cor.test(rwi.M01Pop$xxxstd[2:47], lts.M01$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M01Pop$xxxstd[2:47], lts.M01$Avg_CC_Median[1:46]))
M01Pop.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M01Pop.cor4 = my.cor.test(rwi.M01Pop$xxxstd[1:47], lts.M01$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M01Pop$xxxstd[1:47], lts.M01$Avg_CC_Median[1:47]))
M01Pop.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M01Pop) - width
start = floor(width / 2)

rwi.M01Pop$cor = NA
rwi.M01Pop$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M01Pop$xxxstd[i:(i + width)], 
                     lts.M01$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                                   lts.M01$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                  lts.M01$Avg_CC_Median[i:(i + width)]))
  rwi.M01Pop$cor[i + start] = test$estimate
  rwi.M01Pop$pvalue[i + start] = test$p.value
}

rwi.M01Pop$sig = NA
for (i in 1:nrow(rwi.M01Pop)) {
  if (!is.na(rwi.M01Pop$pvalue[i])) 
    if (rwi.M01Pop$pvalue[i] <= 0.055) { 
      rwi.M01Pop$sig[i] = 1
    }
}

plot(rwi.M01Pop$year, rwi.M01Pop$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M01Pop$year, rwi.M01Pop$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M05 Correlation #####
#####
### M05Thu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M05Thu = add_rownames(rwl.M05Thu.mne.crn, var = "year")
rwi.M05Thu$year = as.numeric(rwi.M05Thu$year) # Years column in chr by default
rwi.M05Thu$smooth = ffcsaps(rwi.M05Thu$xxxstd, nyrs = nyrs)
rwi.M05Thu = subset(rwi.M05Thu, year >= 1972) #1971

# Add Spline to LTS
lts.M05$smooth = ffcsaps(lts.M05$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M05Thu$year, rwi.M05Thu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M05Thu$year, rwi.M05Thu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M05$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M05$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M05Thu.cor = my.cor.test(rwi.M05Thu$xxxstd, lts.M05$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M05Thu$xxxstd, lts.M05$Avg_CC_Median))
M05Thu.cor

# MSS only
M05Thu.cor1 = my.cor.test(rwi.M05Thu$xxxstd[1:12], lts.M05$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M05Thu$xxxstd[1:12], lts.M05$Avg_CC_Median[1:12]))
M05Thu.cor1

# Modern
M05Thu.cor2 = my.cor.test(rwi.M05Thu$xxxstd[13:47], lts.M05$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M05Thu$xxxstd[13:47], lts.M05$Avg_CC_Median[13:47]))
M05Thu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M05Thu.cor3 = my.cor.test(rwi.M05Thu$xxxstd[2:47], lts.M05$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M05Thu$xxxstd[2:47], lts.M05$Avg_CC_Median[1:46]))
M05Thu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M05Thu.cor4 = my.cor.test(rwi.M05Thu$xxxstd[1:47], lts.M05$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M05Thu$xxxstd[1:47], lts.M05$Avg_CC_Median[1:47]))
M05Thu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M05Thu) - width
start = floor(width / 2)

rwi.M05Thu$cor = NA
rwi.M05Thu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M05Thu$xxxstd[i:(i + width)], 
                     lts.M05$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                                   lts.M05$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                  lts.M05$Avg_CC_Median[i:(i + width)]))
  rwi.M05Thu$cor[i + start] = test$estimate
  rwi.M05Thu$pvalue[i + start] = test$p.value
}

rwi.M05Thu$sig = NA
for (i in 1:nrow(rwi.M05Thu)) {
  if (!is.na(rwi.M05Thu$pvalue[i])) 
    if (rwi.M05Thu$pvalue[i] <= 0.055) { 
      rwi.M05Thu$sig[i] = 1
    }
}

plot(rwi.M05Thu$year, rwi.M05Thu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M05Thu$year, rwi.M05Thu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M06 Correlation #####
#####
### M06Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.M06Ace = add_rownames(rwl.M06Ace.mne.crn, var = "year")
rwi.M06Ace$year = as.numeric(rwi.M06Ace$year) # Years column in chr by default
rwi.M06Ace$smooth = ffcsaps(rwi.M06Ace$xxxstd, nyrs = nyrs)
rwi.M06Ace = subset(rwi.M06Ace, year >= 1972) #1971

# Add Spline to LTS
lts.M06$smooth = ffcsaps(lts.M06$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M06Ace$year, rwi.M06Ace$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M06Ace$year, rwi.M06Ace$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M06$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M06$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M06Ace.cor = my.cor.test(rwi.M06Ace$xxxstd, lts.M06$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M06Ace$xxxstd, lts.M06$Avg_CC_Median))
M06Ace.cor

# MSS only
M06Ace.cor1 = my.cor.test(rwi.M06Ace$xxxstd[1:12], lts.M06$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Ace$xxxstd[1:12], lts.M06$Avg_CC_Median[1:12]))
M06Ace.cor1

# Modern
M06Ace.cor2 = my.cor.test(rwi.M06Ace$xxxstd[13:47], lts.M06$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Ace$xxxstd[13:47], lts.M06$Avg_CC_Median[13:47]))
M06Ace.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M06Ace.cor3 = my.cor.test(rwi.M06Ace$xxxstd[2:47], lts.M06$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Ace$xxxstd[2:47], lts.M06$Avg_CC_Median[1:46]))
M06Ace.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M06Ace.cor4 = my.cor.test(rwi.M06Ace$xxxstd[1:47], lts.M06$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Ace$xxxstd[1:47], lts.M06$Avg_CC_Median[1:47]))
M06Ace.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M06Ace) - width
start = floor(width / 2)

rwi.M06Ace$cor = NA
rwi.M06Ace$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Ace$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Ace$cor[i + start] = test$estimate
  rwi.M06Ace$pvalue[i + start] = test$p.value
}

rwi.M06Ace$sig = NA
for (i in 1:nrow(rwi.M06Ace)) {
  if (!is.na(rwi.M06Ace$pvalue[i])) 
    if (rwi.M06Ace$pvalue[i] <= 0.055) { 
      rwi.M06Ace$sig[i] = 1
    }
}

plot(rwi.M06Ace$year, rwi.M06Ace$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M06Ace$year, rwi.M06Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### M06Que ###
# Add Spline and then cutoff RWI pre-1972
rwi.M06Que = add_rownames(rwl.M06Que.mne.crn, var = "year")
rwi.M06Que$year = as.numeric(rwi.M06Que$year) # Years column in chr by default
rwi.M06Que$smooth = ffcsaps(rwi.M06Que$xxxstd, nyrs = nyrs)
rwi.M06Que = subset(rwi.M06Que, year >= 1972)

# Add Spline to LTS
lts.M06$smooth = ffcsaps(lts.M06$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M06Que$year, rwi.M06Que$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M06Que$year, rwi.M06Que$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M06$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M06$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M06Que.cor = my.cor.test(rwi.M06Que$xxxstd, lts.M06$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M06Que$xxxstd, lts.M06$Avg_CC_Median))
M06Que.cor

# MSS only
M06Que.cor1 = my.cor.test(rwi.M06Que$xxxstd[1:12], lts.M06$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Que$xxxstd[1:12], lts.M06$Avg_CC_Median[1:12]))
M06Que.cor1

# Modern
M06Que.cor2 = my.cor.test(rwi.M06Que$xxxstd[13:47], lts.M06$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Que$xxxstd[13:47], lts.M06$Avg_CC_Median[13:47]))
M06Que.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M06Que.cor3 = my.cor.test(rwi.M06Que$xxxstd[2:47], lts.M06$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M06Que$xxxstd[2:47], lts.M06$Avg_CC_Median[1:46]))
M06Que.cor3

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M06Que) - width
start = floor(width / 2)

rwi.M06Que$cor = NA
rwi.M06Que$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Que$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Que$cor[i + start] = test$estimate
  rwi.M06Que$pvalue[i + start] = test$p.value
}

rwi.M06Que$sig = NA
for (i in 1:nrow(rwi.M06Que)) {
  if (!is.na(rwi.M06Que$pvalue[i])) 
    if (rwi.M06Que$pvalue[i] <= 0.055) { 
      rwi.M06Que$sig[i] = 1
    }
}

plot(rwi.M06Que$year, rwi.M06Que$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M06Que$year, rwi.M06Que$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M07 Correlation #####
#####
### M07Tsu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M07Tsu = add_rownames(rwl.M07Tsu.mne.crn, var = "year")
rwi.M07Tsu$year = as.numeric(rwi.M07Tsu$year) # Years column in chr by default
rwi.M07Tsu$smooth = ffcsaps(rwi.M07Tsu$xxxstd, nyrs = nyrs)
rwi.M07Tsu = subset(rwi.M07Tsu, year >= 1972) #1971

# Add Spline to LTS
lts.M07$smooth = ffcsaps(lts.M07$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M07Tsu$year, rwi.M07Tsu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M07Tsu$year, rwi.M07Tsu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M07$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M07$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M07Tsu.cor = my.cor.test(rwi.M07Tsu$xxxstd, lts.M07$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M07Tsu$xxxstd, lts.M07$Avg_CC_Median))
M07Tsu.cor

# MSS only
M07Tsu.cor1 = my.cor.test(rwi.M07Tsu$xxxstd[1:12], lts.M07$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M07Tsu$xxxstd[1:12], lts.M07$Avg_CC_Median[1:12]))
M07Tsu.cor1

# Modern
M07Tsu.cor2 = my.cor.test(rwi.M07Tsu$xxxstd[13:47], lts.M07$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M07Tsu$xxxstd[13:47], lts.M07$Avg_CC_Median[13:47]))
M07Tsu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M07Tsu.cor3 = my.cor.test(rwi.M07Tsu$xxxstd[2:47], lts.M07$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M07Tsu$xxxstd[2:47], lts.M07$Avg_CC_Median[1:46]))
M07Tsu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M07Tsu.cor4 = my.cor.test(rwi.M07Tsu$xxxstd[1:47], lts.M07$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M07Tsu$xxxstd[1:47], lts.M07$Avg_CC_Median[1:47]))
M07Tsu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M07Tsu) - width
start = floor(width / 2)

rwi.M07Tsu$cor = NA
rwi.M07Tsu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M07Tsu$xxxstd[i:(i + width)], 
                     lts.M07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                                   lts.M07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                  lts.M07$Avg_CC_Median[i:(i + width)]))
  rwi.M07Tsu$cor[i + start] = test$estimate
  rwi.M07Tsu$pvalue[i + start] = test$p.value
}

rwi.M07Tsu$sig = NA
for (i in 1:nrow(rwi.M07Tsu)) {
  if (!is.na(rwi.M07Tsu$pvalue[i])) 
    if (rwi.M07Tsu$pvalue[i] <= 0.055) { 
      rwi.M07Tsu$sig[i] = 1
    }
}

plot(rwi.M07Tsu$year, rwi.M07Tsu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M07Tsu$year, rwi.M07Tsu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M13 Correlation #####
#####
### M13Tsu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M13Tsu = add_rownames(rwl.M13Tsu.mne.crn, var = "year")
rwi.M13Tsu$year = as.numeric(rwi.M13Tsu$year) # Years column in chr by default
rwi.M13Tsu$smooth = ffcsaps(rwi.M13Tsu$xxxstd, nyrs = nyrs)
rwi.M13Tsu = subset(rwi.M13Tsu, year >= 1972) #1971

# Add Spline to LTS
lts.M13$smooth = ffcsaps(lts.M13$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M13Tsu$year, rwi.M13Tsu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M13Tsu$year, rwi.M13Tsu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M13$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M13$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M13Tsu.cor = my.cor.test(rwi.M13Tsu$xxxstd, lts.M13$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M13Tsu$xxxstd, lts.M13$Avg_CC_Median))
M13Tsu.cor

# MSS only
M13Tsu.cor1 = my.cor.test(rwi.M13Tsu$xxxstd[1:12], lts.M13$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M13Tsu$xxxstd[1:12], lts.M13$Avg_CC_Median[1:12]))
M13Tsu.cor1

# Modern
M13Tsu.cor2 = my.cor.test(rwi.M13Tsu$xxxstd[13:47], lts.M13$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M13Tsu$xxxstd[13:47], lts.M13$Avg_CC_Median[13:47]))
M13Tsu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M13Tsu.cor3 = my.cor.test(rwi.M13Tsu$xxxstd[2:47], lts.M13$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M13Tsu$xxxstd[2:47], lts.M13$Avg_CC_Median[1:46]))
M13Tsu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M13Tsu.cor4 = my.cor.test(rwi.M13Tsu$xxxstd[1:47], lts.M13$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M13Tsu$xxxstd[1:47], lts.M13$Avg_CC_Median[1:47]))
M13Tsu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M13Tsu) - width
start = floor(width / 2)

rwi.M13Tsu$cor = NA
rwi.M13Tsu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M13Tsu$xxxstd[i:(i + width)], 
                     lts.M13$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                                   lts.M13$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                  lts.M13$Avg_CC_Median[i:(i + width)]))
  rwi.M13Tsu$cor[i + start] = test$estimate
  rwi.M13Tsu$pvalue[i + start] = test$p.value
}

rwi.M13Tsu$sig = NA
for (i in 1:nrow(rwi.M13Tsu)) {
  if (!is.na(rwi.M13Tsu$pvalue[i])) 
    if (rwi.M13Tsu$pvalue[i] <= 0.055) { 
      rwi.M13Tsu$sig[i] = 1
    }
}

plot(rwi.M13Tsu$year, rwi.M13Tsu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M13Tsu$year, rwi.M13Tsu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M17 Correlation #####
#####
### M17Thu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M17Thu = add_rownames(rwl.M17Thu.mne.crn, var = "year")
rwi.M17Thu$year = as.numeric(rwi.M17Thu$year) # Years column in chr by default
rwi.M17Thu$smooth = ffcsaps(rwi.M17Thu$xxxstd, nyrs = nyrs)
rwi.M17Thu = subset(rwi.M17Thu, year >= 1972) #1971

# Add Spline to LTS
lts.M17$smooth = ffcsaps(lts.M17$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M17Thu$year, rwi.M17Thu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M17Thu$year, rwi.M17Thu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M17$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M17$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M17Thu.cor = my.cor.test(rwi.M17Thu$xxxstd, lts.M17$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M17Thu$xxxstd, lts.M17$Avg_CC_Median))
M17Thu.cor

# MSS only
M17Thu.cor1 = my.cor.test(rwi.M17Thu$xxxstd[1:12], lts.M17$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M17Thu$xxxstd[1:12], lts.M17$Avg_CC_Median[1:12]))
M17Thu.cor1

# Modern
M17Thu.cor2 = my.cor.test(rwi.M17Thu$xxxstd[13:47], lts.M17$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M17Thu$xxxstd[13:47], lts.M17$Avg_CC_Median[13:47]))
M17Thu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M17Thu.cor3 = my.cor.test(rwi.M17Thu$xxxstd[2:47], lts.M17$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M17Thu$xxxstd[2:47], lts.M17$Avg_CC_Median[1:46]))
M17Thu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M17Thu.cor4 = my.cor.test(rwi.M17Thu$xxxstd[1:47], lts.M17$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M17Thu$xxxstd[1:47], lts.M17$Avg_CC_Median[1:47]))
M17Thu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M17Thu) - width
start = floor(width / 2)

rwi.M17Thu$cor = NA
rwi.M17Thu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M17Thu$xxxstd[i:(i + width)], 
                     lts.M17$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                                   lts.M17$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                  lts.M17$Avg_CC_Median[i:(i + width)]))
  rwi.M17Thu$cor[i + start] = test$estimate
  rwi.M17Thu$pvalue[i + start] = test$p.value
}

rwi.M17Thu$sig = NA
for (i in 1:nrow(rwi.M17Thu)) {
  if (!is.na(rwi.M17Thu$pvalue[i])) 
    if (rwi.M17Thu$pvalue[i] <= 0.055) { 
      rwi.M17Thu$sig[i] = 1
    }
}

plot(rwi.M17Thu$year, rwi.M17Thu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M17Thu$year, rwi.M17Thu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M20 Correlation #####
#####
### M20Con ###
# Add Spline and then cutoff RWI pre-1972
#rwi.M20Con = add_rownames(rwl.M20Con.mne.crn, var = "year")
#rwi.M20Con$year = as.numeric(rwi.M20Con$year) # Years column in chr by default
#rwi.M20Con$smooth = ffcsaps(rwi.M20Con$xxxstd, nyrs = nyrs)
#rwi.M20Con = subset(rwi.M20Con, year >= 1972)

# Add Spline to LTS
#lts.M20$smooth = ffcsaps(lts.M20$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.M20Con$year, rwi.M20Con$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.M20Con$year, rwi.M20Con$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.M20$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.M20$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.M20Con$xxxstd, lts.M20$Avg_CC_Median, alternative = "greater", 
#                  method = "pearson", n = calc.neff(rwi.M20Con$xxxstd, lts.M20$Avg_CC_Median))
#cor

# Running correlation
#width = 25 - 1 # Record 5 to 25
#end = nrow(rwi.M20Con) - width
#start = floor(width / 2)

#rwi.M20Con$cor = NA
#rwi.M20Con$pvalue = NA
#for (i in 1:end) {
#  test = my.cor.test(rwi.M20Con$xxxstd[i:(i + width)], 
#                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                     method = "pearson",
#                     n = calc.neff(rwi.M20Con$xxxstd[i:(i + width)],
#                                   lts.M20$Avg_CC_Median[i:(i + width)]))
#  print(calc.neff(rwi.M20Con$xxxstd[i:(i + width)],
#                  lts.M20$Avg_CC_Median[i:(i + width)]))
#  rwi.M20Con$cor[i + start] = test$estimate
#  rwi.M20Con$pvalue[i + start] = test$p.value
#}

#rwi.M20Con$sig = NA
#for (i in 1:nrow(rwi.M20Con)) {
#  if (!is.na(rwi.M20Con$pvalue[i])) 
#    if (rwi.M20Con$pvalue[i] <= 0.055) { 
#      rwi.M20Con$sig[i] = 1
#    }
#}

#plot(rwi.M20Con$year, rwi.M20Con$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.M20Con$year, rwi.M20Con$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### M20Thu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M20Thu = add_rownames(rwl.M20Thu.mne.crn, var = "year")
rwi.M20Thu$year = as.numeric(rwi.M20Thu$year) # Years column in chr by default
rwi.M20Thu$smooth = ffcsaps(rwi.M20Thu$xxxstd, nyrs = nyrs)
rwi.M20Thu = subset(rwi.M20Thu, year >= 1972) #1971

# Add Spline to LTS
lts.M20$smooth = ffcsaps(lts.M20$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M20Thu$year, rwi.M20Thu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M20Thu$year, rwi.M20Thu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M20$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M20$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M20Thu.cor = my.cor.test(rwi.M20Thu$xxxstd, lts.M20$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M20Thu$xxxstd, lts.M20$Avg_CC_Median))
M20Thu.cor

# MSS only
M20Thu.cor1 = my.cor.test(rwi.M20Thu$xxxstd[1:12], lts.M20$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M20Thu$xxxstd[1:12], lts.M20$Avg_CC_Median[1:12]))
M20Thu.cor1

# Modern
M20Thu.cor2 = my.cor.test(rwi.M20Thu$xxxstd[13:47], lts.M20$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M20Thu$xxxstd[13:47], lts.M20$Avg_CC_Median[13:47]))
M20Thu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M20Thu.cor3 = my.cor.test(rwi.M20Thu$xxxstd[2:47], lts.M20$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M20Thu$xxxstd[2:47], lts.M20$Avg_CC_Median[1:46]))
M20Thu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M20Thu.cor4 = my.cor.test(rwi.M20Thu$xxxstd[1:47], lts.M20$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M20Thu$xxxstd[1:47], lts.M20$Avg_CC_Median[1:47]))
M20Thu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M20Thu) - width
start = floor(width / 2)

rwi.M20Thu$cor = NA
rwi.M20Thu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M20Thu$xxxstd[i:(i + width)], 
                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                                   lts.M20$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                  lts.M20$Avg_CC_Median[i:(i + width)]))
  rwi.M20Thu$cor[i + start] = test$estimate
  rwi.M20Thu$pvalue[i + start] = test$p.value
}

rwi.M20Thu$sig = NA
for (i in 1:nrow(rwi.M20Thu)) {
  if (!is.na(rwi.M20Thu$pvalue[i])) 
    if (rwi.M20Thu$pvalue[i] <= 0.055) { 
      rwi.M20Thu$sig[i] = 1
    }
}

plot(rwi.M20Thu$year, rwi.M20Thu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M20Thu$year, rwi.M20Thu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M26 Correlation #####
#####
### M26Dec ###
# Add Spline and then cutoff RWI pre-1972
rwi.M26Dec = add_rownames(rwl.M26Dec.mne.crn, var = "year")
rwi.M26Dec$year = as.numeric(rwi.M26Dec$year) # Years column in chr by default
rwi.M26Dec$smooth = ffcsaps(rwi.M26Dec$xxxstd, nyrs = nyrs)
rwi.M26Dec = subset(rwi.M26Dec, year >= 1972) #1971

# Add Spline to LTS
lts.M26$smooth = ffcsaps(lts.M26$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M26Dec$year, rwi.M26Dec$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M26Dec$year, rwi.M26Dec$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M26$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M26$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M26Dec.cor = my.cor.test(rwi.M26Dec$xxxstd, lts.M26$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M26Dec$xxxstd, lts.M26$Avg_CC_Median))
M26Dec.cor

# MSS only
M26Dec.cor1 = my.cor.test(rwi.M26Dec$xxxstd[1:12], lts.M26$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Dec$xxxstd[1:12], lts.M26$Avg_CC_Median[1:12]))
M26Dec.cor1

# Modern
M26Dec.cor2 = my.cor.test(rwi.M26Dec$xxxstd[13:47], lts.M26$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Dec$xxxstd[13:47], lts.M26$Avg_CC_Median[13:47]))
M26Dec.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M26Dec.cor3 = my.cor.test(rwi.M26Dec$xxxstd[2:47], lts.M26$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Dec$xxxstd[2:47], lts.M26$Avg_CC_Median[1:46]))
M26Dec.cor3

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M26Dec) - width
start = floor(width / 2)

rwi.M26Dec$cor = NA
rwi.M26Dec$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Dec$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Dec$cor[i + start] = test$estimate
  rwi.M26Dec$pvalue[i + start] = test$p.value
}

rwi.M26Dec$sig = NA
for (i in 1:nrow(rwi.M26Dec)) {
  if (!is.na(rwi.M26Dec$pvalue[i])) 
    if (rwi.M26Dec$pvalue[i] <= 0.055) { 
      rwi.M26Dec$sig[i] = 1
    }
}

plot(rwi.M26Dec$year, rwi.M26Dec$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M26Dec$year, rwi.M26Dec$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### M26Ace ###
# Add Spline and then cutoff RWI pre-1972
#rwi.M26Ace = add_rownames(rwl.M26Ace.mne.crn, var = "year")
#rwi.M26Ace$year = as.numeric(rwi.M26Ace$year) # Years column in chr by default
#rwi.M26Ace$smooth = ffcsaps(rwi.M26Ace$xxxstd, nyrs = nyrs)
#rwi.M26Ace = subset(rwi.M26Ace, year >= 1972)

# Add Spline to LTS
#lts.M26$smooth = ffcsaps(lts.M26$Avg_CC_Median, nyrs = nyrs)

# Plot
#plot(rwi.M26Ace$year, rwi.M26Ace$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.M26Ace$year, rwi.M26Ace$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.M26$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.M26$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
#cor = my.cor.test(rwi.M26Ace$xxxstd, lts.M26$Avg_CC_Median, alternative = "greater", 
#                  method = "pearson", n = calc.neff(rwi.M26Ace$xxxstd, lts.M26$Avg_CC_Median))
#cor

# Running correlation
#width = 25 - 1 # Record 5 to 25
#end = nrow(rwi.M26Ace) - width
#start = floor(width / 2)

#rwi.M26Ace$cor = NA
#rwi.M26Ace$pvalue = NA
#for (i in 1:end) {
#  test = my.cor.test(rwi.M26Ace$xxxstd[i:(i + width)], 
#                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
#                     method = "pearson",
#                     n = calc.neff(rwi.M26Ace$xxxstd[i:(i + width)],
#                                   lts.M26$Avg_CC_Median[i:(i + width)]))
#  print(calc.neff(rwi.M26Ace$xxxstd[i:(i + width)],
#                  lts.M26$Avg_CC_Median[i:(i + width)]))
#  rwi.M26Ace$cor[i + start] = test$estimate
#  rwi.M26Ace$pvalue[i + start] = test$p.value
#}

#rwi.M26Ace$sig = NA
#for (i in 1:nrow(rwi.M26Ace)) {
#  if (!is.na(rwi.M26Ace$pvalue[i])) 
#    if (rwi.M26Ace$pvalue[i] <= 0.055) { 
#      rwi.M26Ace$sig[i] = 1
#    }
#}

#plot(rwi.M26Ace$year, rwi.M26Ace$cor, type = "l", ylab = "", xlab = "")
#abline(h = cor$estimate, lty = 2)
#par(new = TRUE)
#plot(rwi.M26Ace$year, rwi.M26Ace$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
#     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)

### M26Thu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M26Thu = add_rownames(rwl.M26Thu.mne.crn, var = "year")
rwi.M26Thu$year = as.numeric(rwi.M26Thu$year) # Years column in chr by default
rwi.M26Thu$smooth = ffcsaps(rwi.M26Thu$xxxstd, nyrs = nyrs)
rwi.M26Thu = subset(rwi.M26Thu, year >= 1972) #1971

# Add Spline to LTS
lts.M26$smooth = ffcsaps(lts.M26$Avg_CC_Median, nyrs = nyrs)

# Plot
plot(rwi.M26Thu$year, rwi.M26Thu$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M26Thu$year, rwi.M26Thu$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M26$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M26$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M26Thu.cor = my.cor.test(rwi.M26Thu$xxxstd, lts.M26$Avg_CC_Median, alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M26Thu$xxxstd, lts.M26$Avg_CC_Median))
M26Thu.cor

# MSS only
M26Thu.cor1 = my.cor.test(rwi.M26Thu$xxxstd[1:12], lts.M26$Avg_CC_Median[1:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Thu$xxxstd[1:12], lts.M26$Avg_CC_Median[1:12]))
M26Thu.cor1

# Modern
M26Thu.cor2 = my.cor.test(rwi.M26Thu$xxxstd[13:47], lts.M26$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Thu$xxxstd[13:47], lts.M26$Avg_CC_Median[13:47]))
M26Thu.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M26Thu.cor3 = my.cor.test(rwi.M26Thu$xxxstd[2:47], lts.M26$Avg_CC_Median[1:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Thu$xxxstd[2:47], lts.M26$Avg_CC_Median[1:46]))
M26Thu.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M26Thu.cor4 = my.cor.test(rwi.M26Thu$xxxstd[1:47], lts.M26$Avg_CC_Median[1:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M26Thu$xxxstd[1:47], lts.M26$Avg_CC_Median[1:47]))
M26Thu.cor4

# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M26Thu) - width
start = floor(width / 2)

rwi.M26Thu$cor = NA
rwi.M26Thu$pvalue = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Thu$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Thu$cor[i + start] = test$estimate
  rwi.M26Thu$pvalue[i + start] = test$p.value
}

rwi.M26Thu$sig = NA
for (i in 1:nrow(rwi.M26Thu)) {
  if (!is.na(rwi.M26Thu$pvalue[i])) 
    if (rwi.M26Thu$pvalue[i] <= 0.055) { 
      rwi.M26Thu$sig[i] = 1
    }
}

plot(rwi.M26Thu$year, rwi.M26Thu$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M26Thu$year, rwi.M26Thu$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### M27 Correlation #####
#####
### M27Pin ###
# Create data.frame (special for M27 since it is younger than LTS)
rwi.M27Pin = data.frame("year" = 1972:2018, "xxxstd" = NA, samp.depth = NA)
rwi.M27Pin$xxxstd[10:47] = rwl.M27Pin.mne.crn$xxxstd 
rwi.M27Pin$samp.depth[10:47] = rwl.M27Pin.mne.crn$samp.depth
rwi.M27Pin$smooth[10:47] = ffcsaps(rwi.M27Pin$xxxstd[10:47], nyrs = nyrs)

# Add Spline to LTS
lts.M27$smooth = ffcsaps(lts.M27$Avg_CC_Median, nyrs = nyrs)
lts.M27[1:9,] = NA

# Plot
plot(rwi.M27Pin$year, rwi.M27Pin$xxxstd, type = "l", col = "darkgreen", 
     ylab = "", xlab = "")
lines(rwi.M27Pin$year, rwi.M27Pin$smooth, col = "darkgreen", lwd = 3)
par(new = TRUE)
plot(1972:2018, lts.M27$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     type = "l", col = "red3")
axis(side = 4)
lines(1972:2018, lts.M27$smooth, col = "red3", lwd = 3)

# Calc Neff and Cor.test
M27Pin.cor = my.cor.test(rwi.M27Pin$xxxstd[10:47], lts.M27$Avg_CC_Median[10:47], alternative = "greater", 
                  method = "pearson", n = calc.neff(rwi.M27Pin$xxxstd[10:47], lts.M27$Avg_CC_Median[10:47]))
M27Pin.cor

# MSS only
M27Pin.cor1 = my.cor.test(rwi.M27Pin$xxxstd[10:12], lts.M27$Avg_CC_Median[10:12], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M27Pin$xxxstd[10:12], lts.M27$Avg_CC_Median[10:12]))
M27Pin.cor1

# Modern
M27Pin.cor2 = my.cor.test(rwi.M27Pin$xxxstd[13:47], lts.M27$Avg_CC_Median[13:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M27Pin$xxxstd[13:47], lts.M27$Avg_CC_Median[13:47]))
M27Pin.cor2

# Lag LTS -1 (2001 in RWI = 2000 in LTS)
M27Pin.cor3 = my.cor.test(rwi.M27Pin$xxxstd[11:47], lts.M27$Avg_CC_Median[10:46], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M27Pin$xxxstd[11:47], lts.M27$Avg_CC_Median[10:46]))
M27Pin.cor3

# Lag LTS +1 (1999 in RWI = 2000 in LTS)
M27Pin.cor4 = my.cor.test(rwi.M27Pin$xxxstd[10:46], lts.M27$Avg_CC_Median[11:47], alternative = "greater", 
                          method = "pearson", n = calc.neff(rwi.M27Pin$xxxstd[10:46], lts.M27$Avg_CC_Median[11:47]))
M27Pin.cor4


# Running correlation
width = 25 - 1 # Record 5 to 25
end = nrow(rwi.M27Pin) - width
start = floor(width / 2)

rwi.M27Pin$cor = NA
rwi.M27Pin$pvalue = NA
for (i in 10:end) {
  test = my.cor.test(rwi.M27Pin$xxxstd[i:(i + width)], 
                     lts.M27$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                                   lts.M27$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                  lts.M27$Avg_CC_Median[i:(i + width)]))
  rwi.M27Pin$cor[i + start] = test$estimate
  rwi.M27Pin$pvalue[i + start] = test$p.value
}

rwi.M27Pin$sig = NA
for (i in 10:nrow(rwi.M27Pin)) {
  if (!is.na(rwi.M27Pin$pvalue[i])) 
    if (rwi.M27Pin$pvalue[i] <= 0.055) { 
      rwi.M27Pin$sig[i] = 1
    }
}

plot(rwi.M27Pin$year, rwi.M27Pin$cor, type = "l", ylab = "", xlab = "")
abline(h = cor$estimate, lty = 2)
par(new = TRUE)
plot(rwi.M27Pin$year, rwi.M27Pin$sig, xaxt = "n", yaxt = "n", ylab = "", xlab = "", 
     type = "p", pch = 15, ylim = c(1,2), cex = 1.5)
#####

##### Create Final Figure - Correlation 1972 - 2018 #####
#####
# F07Ace (Site 08)
F07Ace.p = ggplot(data = rwi.F07Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "blue") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F07, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F07, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("08Ace (-0.12)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), labels = NULL,
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"))
F07Ace.p

# F15Que / F15Ace (Site 02)
F15Que.p = ggplot(data = rwi.F15Que, aes(x = year, y = xxxstd)) + 
  geom_line(data = rwi.F15Ace, aes(x = year, y = xxxstd), col = "darkgreen") +
  geom_line(data = rwi.F15Ace, aes(x = year, y = smooth), col = "darkgreen", lwd = 1) +
  geom_line(col = "blue") + 
  geom_line(data = rwi.F15Que, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  labs(title = "<span style= 'color:blue;' >**02Que (0.28*)**</span> / 
                <span style= 'color:darkgreen;' >02Ace (-0.13)</span>") + 
  #ggtitle("02Que (0.28*) / 02Ace (-0.13)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  coord_cartesian(ylim = c(0,2.3), expand = FALSE) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1))
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F15Que.p

# F21Dec (Site 06)
F21Dec.p = ggplot(data = rwi.F21Dec, aes(x = year, y = xxxstd)) + 
  geom_line(col = "blue") + 
  geom_line(data = rwi.F21Dec, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F21, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F21, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("06Dec (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"))
F21Dec.p

# F23Ace (Site 05)
F23Ace.p = ggplot(data = rwi.F23Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "blue") + 
  geom_line(data = rwi.F23Ace, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F23, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F23, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("05Ace (-0.21)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5),
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"))
F23Ace.p

# F25Ace (Site 11)
F25Ace.p = ggplot(data = rwi.F25Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "blue") + 
  geom_line(data = rwi.F25Ace, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F25, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F25, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("11Ace (-0.22)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "blue"))
F25Ace.p

# F30Bet / F30Ace (Site 15)
F30Bet.p = ggplot(data = rwi.F30Bet, aes(x = year, y = xxxstd)) + 
  geom_line(data = rwi.F30Ace, aes(x = year, y = xxxstd), col = "darkgreen") +
  geom_line(data = rwi.F30Ace, aes(x = year, y = smooth), col = "darkgreen", lwd = 1) +
  geom_line(col = "blue") + 
  geom_line(data = rwi.F30Bet, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  labs(title = "<span style= 'color:blue;' >15Bet (-0.10)</span> / 
                <span style= 'color:darkgreen;' >15Ace (-0.11)</span>") + 
  #ggtitle("15Bet (-0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1))
        #plot.title = element_text(size = 10, vjust = -1))
F30Bet.p

# F33Pic (Site 13)
F33Pic.p = ggplot(data = rwi.F33Pic, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.F33Pic, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.F33, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.F33, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("13Pic (0.31*)") +
  labs(y = "**RWI** (<span style= 'color:red3;' >red</span>: coniferous, 
                     <span style= 'color:blue;' >blue</span> / 
                     <span style= 'color:darkgreen;' >green</span>: deciduous)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5),
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_markdown(size = 12, hjust = -0.15), #hjust = 3.4
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"))
F33Pic.p

# M01Pop (Site 12)
M01Pop.p = ggplot(data = rwi.M01Pop, aes(x = year, y = xxxstd)) + 
  geom_line(col = "blue") + 
  geom_line(data = rwi.M01Pop, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.M01, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M01, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("12Pop (0.57**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, labels = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "blue"))
M01Pop.p

# M05Thu (Site 07)
M05Thu.p = ggplot(data = rwi.M05Thu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M05Thu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M05, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M05, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("07Thu (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), labels = NULL, 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"))
M05Thu.p

# M06Ace / M06Que (Site 03)
M06Ace.p = ggplot(data = rwi.M06Ace, aes(x = year, y = xxxstd)) + 
  geom_line(data = rwi.M06Que, aes(x = year, y = xxxstd), col = "darkgreen") +
  geom_line(data = rwi.M06Que, aes(x = year, y = smooth), col = "darkgreen", lwd = 1) +
  geom_line(col = "blue") + 
  geom_line(data = rwi.M06Ace, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  labs(title = "<span style= 'color:blue;' >**03Ace (0.24*)**</span> / 
                <span style= 'color:darkgreen;' >03Que (0.02)</span>") + 
  #ggtitle("03Ace (0.24*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1))
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M06Ace.p

# M07Tsu (Site 04)
M07Tsu.p = ggplot(data = rwi.M07Tsu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M07Tsu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M07, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M07, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("04Tsu (0.16)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), labels = NULL,
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"))
M07Tsu.p

# M13Tsu (Site 01)
M13Tsu.p = ggplot(data = rwi.M13Tsu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M13Tsu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M13, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M13, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("01Tsu (0.64**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3), breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5),
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"))
M13Tsu.p

# M17Thu (Site 16)
M17Thu.p = ggplot(data = rwi.M17Thu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M17Thu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M17, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M17, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("16Thu (0.43*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), labels = NULL,
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5), name = "CC (%)")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y.right = element_text(size = 12, angle = 90, hjust = 5.2, 
                                          color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold", color = "red3"))
M17Thu.p

# M20Thu (Site 14)
M20Thu.p = ggplot(data = rwi.M20Thu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M20Thu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M20, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M20, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("14Thu (0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5), 
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"))
M20Thu.p

# M26Thu / M26Dec (Site 10)
M26Thu.p = ggplot(data = rwi.M26Thu, aes(x = year, y = xxxstd)) + 
  geom_line(data = rwi.M26Dec, aes(x = year, y = xxxstd), col = "blue") +
  geom_line(data = rwi.M26Dec, aes(x = year, y = smooth), col = "blue", lwd = 1) +
  geom_line(col = "red3") + 
  geom_line(data = rwi.M26Thu, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  labs(title = "<span style= 'color:red3;' >**10Thu (0.55*)**</span> / 
                <span style= 'color:blue;' >10Dec (-0.17)</span>") + 
  #ggtitle("10Thu (0.55*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5), 
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_markdown(size = 7, vjust = -1))
        #plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M26Thu.p

# M27Pin (Site 09)
M27Pin.p = ggplot(data = rwi.M27Pin, aes(x = year, y = xxxstd)) + 
  geom_line(col = "red3") + 
  geom_line(data = rwi.M27Pin, aes(x = year, y = smooth), col = "red3", lwd = 1) +
  geom_line(data = lts.M27, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "black") +
  geom_line(data = lts.M27, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "black", lwd = 1) +
  ggtitle("09Pin (0.20)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5),
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5), 
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, color = "red3"))
M27Pin.p

#####
tiff("corplot2_20.tiff", units = "in", width = 6.5, height = 5, res = 300)
(M13Tsu.p | F15Que.p | M06Ace.p | M07Tsu.p) /
  (F23Ace.p | F21Dec.p | M05Thu.p | F07Ace.p) / 
  (M27Pin.p | M26Thu.p | F25Ace.p | M01Pop.p) /
  (F33Pic.p | M20Thu.p | F30Bet.p | M17Thu.p)
dev.off()

##### Create Final Figure - Correlation through time #####
#####
# F07Ace (Site 08)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F07Ace) - width
start = floor(width / 2)

rwi.F07Ace$cor7 = NA
rwi.F07Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F07Ace$xxxstd[i:(i + width)], 
                     lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                                   lts.F07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                  lts.F07$Avg_CC_Median[i:(i + width)]))
  rwi.F07Ace$cor7[i + start] = test$estimate
  rwi.F07Ace$pvalue7[i + start] = test$p.value
}

rwi.F07Ace$sig7 = NA
for (i in 1:nrow(rwi.F07Ace)) {
  if (!is.na(rwi.F07Ace$pvalue7[i])) 
    if (rwi.F07Ace$pvalue7[i] <= 0.055) { 
      rwi.F07Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F07Ace) - width
start = floor(width / 2)

rwi.F07Ace$cor11 = NA
rwi.F07Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F07Ace$xxxstd[i:(i + width)], 
                     lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                                   lts.F07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                  lts.F07$Avg_CC_Median[i:(i + width)]))
  rwi.F07Ace$cor11[i + start] = test$estimate
  rwi.F07Ace$pvalue11[i + start] = test$p.value
}

rwi.F07Ace$sig11 = NA
for (i in 1:nrow(rwi.F07Ace)) {
  if (!is.na(rwi.F07Ace$pvalue11[i])) 
    if (rwi.F07Ace$pvalue11[i] <= 0.055) { 
      rwi.F07Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F07Ace) - width
start = floor(width / 2)

rwi.F07Ace$cor17 = NA
rwi.F07Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F07Ace$xxxstd[i:(i + width)], 
                     lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                                   lts.F07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                  lts.F07$Avg_CC_Median[i:(i + width)]))
  rwi.F07Ace$cor17[i + start] = test$estimate
  rwi.F07Ace$pvalue17[i + start] = test$p.value
}

rwi.F07Ace$sig17 = NA
for (i in 1:nrow(rwi.F07Ace)) {
  if (!is.na(rwi.F07Ace$pvalue17[i])) 
    if (rwi.F07Ace$pvalue17[i] <= 0.055) { 
      rwi.F07Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F07Ace) - width
start = floor(width / 2)

rwi.F07Ace$cor25 = NA
rwi.F07Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F07Ace$xxxstd[i:(i + width)], 
                     lts.F07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                                   lts.F07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F07Ace$xxxstd[i:(i + width)],
                  lts.F07$Avg_CC_Median[i:(i + width)]))
  rwi.F07Ace$cor25[i + start] = test$estimate
  rwi.F07Ace$pvalue25[i + start] = test$p.value
}

rwi.F07Ace$sig25 = NA
for (i in 1:nrow(rwi.F07Ace)) {
  if (!is.na(rwi.F07Ace$pvalue25[i])) 
    if (rwi.F07Ace$pvalue25[i] <= 0.055) { 
      rwi.F07Ace$sig25[i] = 1
    }
}

# Plot
F07Ace.p1 = ggplot(data = rwi.F07Ace) + 
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_hline(yintercept = F07Ace.cor$estimate, lty = 2, col = "gray48") +
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("08Ace (-0.12)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5), labels = NULL) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F07Ace.p1

# F15Que (Site 02)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F15Que) - width
start = floor(width / 2)

rwi.F15Que$cor7 = NA
rwi.F15Que$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Que$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Que$cor7[i + start] = test$estimate
  rwi.F15Que$pvalue7[i + start] = test$p.value
}

rwi.F15Que$sig7 = NA
for (i in 1:nrow(rwi.F15Que)) {
  if (!is.na(rwi.F15Que$pvalue7[i])) 
    if (rwi.F15Que$pvalue7[i] <= 0.055) { 
      rwi.F15Que$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F15Que) - width
start = floor(width / 2)

rwi.F15Que$cor11 = NA
rwi.F15Que$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Que$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Que$cor11[i + start] = test$estimate
  rwi.F15Que$pvalue11[i + start] = test$p.value
}

rwi.F15Que$sig11 = NA
for (i in 1:nrow(rwi.F15Que)) {
  if (!is.na(rwi.F15Que$pvalue11[i])) 
    if (rwi.F15Que$pvalue11[i] <= 0.055) { 
      rwi.F15Que$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F15Que) - width
start = floor(width / 2)

rwi.F15Que$cor17 = NA
rwi.F15Que$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Que$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Que$cor17[i + start] = test$estimate
  rwi.F15Que$pvalue17[i + start] = test$p.value
}

rwi.F15Que$sig17 = NA
for (i in 1:nrow(rwi.F15Que)) {
  if (!is.na(rwi.F15Que$pvalue17[i])) 
    if (rwi.F15Que$pvalue17[i] <= 0.055) { 
      rwi.F15Que$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F15Que) - width
start = floor(width / 2)

rwi.F15Que$cor25 = NA
rwi.F15Que$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Que$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Que$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Que$cor25[i + start] = test$estimate
  rwi.F15Que$pvalue25[i + start] = test$p.value
}

rwi.F15Que$sig25 = NA
for (i in 1:nrow(rwi.F15Que)) {
  if (!is.na(rwi.F15Que$pvalue25[i])) 
    if (rwi.F15Que$pvalue25[i] <= 0.055) { 
      rwi.F15Que$sig25[i] = 1
    }
}

# Plot
F15Que.p1 = ggplot(data = rwi.F15Que) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F15Que.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("02Que (0.28*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F15Que.p1

# F15Ace (Site 02)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F15Ace) - width
start = floor(width / 2)

rwi.F15Ace$cor7 = NA
rwi.F15Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Ace$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Ace$cor7[i + start] = test$estimate
  rwi.F15Ace$pvalue7[i + start] = test$p.value
}

rwi.F15Ace$sig7 = NA
for (i in 1:nrow(rwi.F15Ace)) {
  if (!is.na(rwi.F15Ace$pvalue7[i])) 
    if (rwi.F15Ace$pvalue7[i] <= 0.055) { 
      rwi.F15Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F15Ace) - width
start = floor(width / 2)

rwi.F15Ace$cor11 = NA
rwi.F15Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Ace$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Ace$cor11[i + start] = test$estimate
  rwi.F15Ace$pvalue11[i + start] = test$p.value
}

rwi.F15Ace$sig11 = NA
for (i in 1:nrow(rwi.F15Ace)) {
  if (!is.na(rwi.F15Ace$pvalue11[i])) 
    if (rwi.F15Ace$pvalue11[i] <= 0.055) { 
      rwi.F15Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F15Ace) - width
start = floor(width / 2)

rwi.F15Ace$cor17 = NA
rwi.F15Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Ace$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Ace$cor17[i + start] = test$estimate
  rwi.F15Ace$pvalue17[i + start] = test$p.value
}

rwi.F15Ace$sig17 = NA
for (i in 1:nrow(rwi.F15Ace)) {
  if (!is.na(rwi.F15Ace$pvalue17[i])) 
    if (rwi.F15Ace$pvalue17[i] <= 0.055) { 
      rwi.F15Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F15Ace) - width
start = floor(width / 2)

rwi.F15Ace$cor25 = NA
rwi.F15Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F15Ace$xxxstd[i:(i + width)], 
                     lts.F15$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                                   lts.F15$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F15Ace$xxxstd[i:(i + width)],
                  lts.F15$Avg_CC_Median[i:(i + width)]))
  rwi.F15Ace$cor25[i + start] = test$estimate
  rwi.F15Ace$pvalue25[i + start] = test$p.value
}

rwi.F15Ace$sig25 = NA
for (i in 1:nrow(rwi.F15Ace)) {
  if (!is.na(rwi.F15Ace$pvalue25[i])) 
    if (rwi.F15Ace$pvalue25[i] <= 0.055) { 
      rwi.F15Ace$sig25[i] = 1
    }
}

# Plot
F15Ace.p1 = ggplot(data = rwi.F15Ace) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F15Ace.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("02Ace (-0.13)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F15Ace.p1

# F21Dec (Site 06)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F21Dec) - width
start = floor(width / 2)

rwi.F21Dec$cor7 = NA
rwi.F21Dec$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F21Dec$xxxstd[i:(i + width)], 
                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                                   lts.F21$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                  lts.F21$Avg_CC_Median[i:(i + width)]))
  rwi.F21Dec$cor7[i + start] = test$estimate
  rwi.F21Dec$pvalue7[i + start] = test$p.value
}

rwi.F21Dec$sig7 = NA
for (i in 1:nrow(rwi.F21Dec)) {
  if (!is.na(rwi.F21Dec$pvalue7[i])) 
    if (rwi.F21Dec$pvalue7[i] <= 0.055) { 
      rwi.F21Dec$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F21Dec) - width
start = floor(width / 2)

rwi.F21Dec$cor11 = NA
rwi.F21Dec$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F21Dec$xxxstd[i:(i + width)], 
                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                                   lts.F21$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                  lts.F21$Avg_CC_Median[i:(i + width)]))
  rwi.F21Dec$cor11[i + start] = test$estimate
  rwi.F21Dec$pvalue11[i + start] = test$p.value
}

rwi.F21Dec$sig11 = NA
for (i in 1:nrow(rwi.F21Dec)) {
  if (!is.na(rwi.F21Dec$pvalue11[i])) 
    if (rwi.F21Dec$pvalue11[i] <= 0.055) { 
      rwi.F21Dec$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F21Dec) - width
start = floor(width / 2)

rwi.F21Dec$cor17 = NA
rwi.F21Dec$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F21Dec$xxxstd[i:(i + width)], 
                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                                   lts.F21$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                  lts.F21$Avg_CC_Median[i:(i + width)]))
  rwi.F21Dec$cor17[i + start] = test$estimate
  rwi.F21Dec$pvalue17[i + start] = test$p.value
}

rwi.F21Dec$sig17 = NA
for (i in 1:nrow(rwi.F21Dec)) {
  if (!is.na(rwi.F21Dec$pvalue17[i])) 
    if (rwi.F21Dec$pvalue17[i] <= 0.055) { 
      rwi.F21Dec$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F21Dec) - width
start = floor(width / 2)

rwi.F21Dec$cor25 = NA
rwi.F21Dec$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F21Dec$xxxstd[i:(i + width)], 
                     lts.F21$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                                   lts.F21$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F21Dec$xxxstd[i:(i + width)],
                  lts.F21$Avg_CC_Median[i:(i + width)]))
  rwi.F21Dec$cor25[i + start] = test$estimate
  rwi.F21Dec$pvalue25[i + start] = test$p.value
}

rwi.F21Dec$sig25 = NA
for (i in 1:nrow(rwi.F21Dec)) {
  if (!is.na(rwi.F21Dec$pvalue25[i])) 
    if (rwi.F21Dec$pvalue25[i] <= 0.055) { 
      rwi.F21Dec$sig25[i] = 1
    }
}

# Plot
F21Dec.p1 = ggplot(data = rwi.F21Dec) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F21Dec.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("06Dec (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F21Dec.p1

# F23Ace (Site 05)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F23Ace) - width
start = floor(width / 2)

rwi.F23Ace$cor7 = NA
rwi.F23Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F23Ace$xxxstd[i:(i + width)], 
                     lts.F23$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                                   lts.F23$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                  lts.F23$Avg_CC_Median[i:(i + width)]))
  rwi.F23Ace$cor7[i + start] = test$estimate
  rwi.F23Ace$pvalue7[i + start] = test$p.value
}

rwi.F23Ace$sig7 = NA
for (i in 1:nrow(rwi.F23Ace)) {
  if (!is.na(rwi.F23Ace$pvalue7[i])) 
    if (rwi.F23Ace$pvalue7[i] <= 0.055) { 
      rwi.F23Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F23Ace) - width
start = floor(width / 2)

rwi.F23Ace$cor11 = NA
rwi.F23Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F23Ace$xxxstd[i:(i + width)], 
                     lts.F23$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                                   lts.F23$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                  lts.F23$Avg_CC_Median[i:(i + width)]))
  rwi.F23Ace$cor11[i + start] = test$estimate
  rwi.F23Ace$pvalue11[i + start] = test$p.value
}

rwi.F23Ace$sig11 = NA
for (i in 1:nrow(rwi.F23Ace)) {
  if (!is.na(rwi.F23Ace$pvalue11[i])) 
    if (rwi.F23Ace$pvalue11[i] <= 0.055) { 
      rwi.F23Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F23Ace) - width
start = floor(width / 2)

rwi.F23Ace$cor17 = NA
rwi.F23Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F23Ace$xxxstd[i:(i + width)], 
                     lts.F23$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                                   lts.F23$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                  lts.F23$Avg_CC_Median[i:(i + width)]))
  rwi.F23Ace$cor17[i + start] = test$estimate
  rwi.F23Ace$pvalue17[i + start] = test$p.value
}

rwi.F23Ace$sig17 = NA
for (i in 1:nrow(rwi.F23Ace)) {
  if (!is.na(rwi.F23Ace$pvalue17[i])) 
    if (rwi.F23Ace$pvalue17[i] <= 0.055) { 
      rwi.F23Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F23Ace) - width
start = floor(width / 2)

rwi.F23Ace$cor25 = NA
rwi.F23Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F23Ace$xxxstd[i:(i + width)], 
                     lts.F23$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                                   lts.F23$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F23Ace$xxxstd[i:(i + width)],
                  lts.F23$Avg_CC_Median[i:(i + width)]))
  rwi.F23Ace$cor25[i + start] = test$estimate
  rwi.F23Ace$pvalue25[i + start] = test$p.value
}

rwi.F23Ace$sig25 = NA
for (i in 1:nrow(rwi.F23Ace)) {
  if (!is.na(rwi.F23Ace$pvalue25[i])) 
    if (rwi.F23Ace$pvalue25[i] <= 0.055) { 
      rwi.F23Ace$sig25[i] = 1
    }
}

# Plot
F23Ace.p1 = ggplot(data = rwi.F23Ace) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F23Ace.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("05Ace (-0.21)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F23Ace.p1

# F25Ace (Site 11)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F25Ace) - width
start = floor(width / 2)

rwi.F25Ace$cor7 = NA
rwi.F25Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F25Ace$xxxstd[i:(i + width)], 
                     lts.F25$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                                   lts.F25$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                  lts.F25$Avg_CC_Median[i:(i + width)]))
  rwi.F25Ace$cor7[i + start] = test$estimate
  rwi.F25Ace$pvalue7[i + start] = test$p.value
}

rwi.F25Ace$sig7 = NA
for (i in 1:nrow(rwi.F25Ace)) {
  if (!is.na(rwi.F25Ace$pvalue7[i])) 
    if (rwi.F25Ace$pvalue7[i] <= 0.055) { 
      rwi.F25Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F25Ace) - width
start = floor(width / 2)

rwi.F25Ace$cor11 = NA
rwi.F25Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F25Ace$xxxstd[i:(i + width)], 
                     lts.F25$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                                   lts.F25$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                  lts.F25$Avg_CC_Median[i:(i + width)]))
  rwi.F25Ace$cor11[i + start] = test$estimate
  rwi.F25Ace$pvalue11[i + start] = test$p.value
}

rwi.F25Ace$sig11 = NA
for (i in 1:nrow(rwi.F25Ace)) {
  if (!is.na(rwi.F25Ace$pvalue11[i])) 
    if (rwi.F25Ace$pvalue11[i] <= 0.055) { 
      rwi.F25Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F25Ace) - width
start = floor(width / 2)

rwi.F25Ace$cor17 = NA
rwi.F25Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F25Ace$xxxstd[i:(i + width)], 
                     lts.F25$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                                   lts.F25$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                  lts.F25$Avg_CC_Median[i:(i + width)]))
  rwi.F25Ace$cor17[i + start] = test$estimate
  rwi.F25Ace$pvalue17[i + start] = test$p.value
}

rwi.F25Ace$sig17 = NA
for (i in 1:nrow(rwi.F25Ace)) {
  if (!is.na(rwi.F25Ace$pvalue17[i])) 
    if (rwi.F25Ace$pvalue17[i] <= 0.055) { 
      rwi.F25Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F25Ace) - width
start = floor(width / 2)

rwi.F25Ace$cor25 = NA
rwi.F25Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F25Ace$xxxstd[i:(i + width)], 
                     lts.F25$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                                   lts.F25$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F25Ace$xxxstd[i:(i + width)],
                  lts.F25$Avg_CC_Median[i:(i + width)]))
  rwi.F25Ace$cor25[i + start] = test$estimate
  rwi.F25Ace$pvalue25[i + start] = test$p.value
}

rwi.F25Ace$sig25 = NA
for (i in 1:nrow(rwi.F25Ace)) {
  if (!is.na(rwi.F25Ace$pvalue25[i])) 
    if (rwi.F25Ace$pvalue25[i] <= 0.055) { 
      rwi.F25Ace$sig25[i] = 1
    }
}

# Plot
F25Ace.p1 = ggplot(data = rwi.F25Ace) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F25Ace.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("11Ace (-0.22)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "italic"))
F25Ace.p1

# F30Bet (Site 15)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F30Bet) - width
start = floor(width / 2)

rwi.F30Bet$cor7 = NA
rwi.F30Bet$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Bet$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Bet$cor7[i + start] = test$estimate
  rwi.F30Bet$pvalue7[i + start] = test$p.value
}

rwi.F30Bet$sig7 = NA
for (i in 1:nrow(rwi.F30Bet)) {
  if (!is.na(rwi.F30Bet$pvalue7[i])) 
    if (rwi.F30Bet$pvalue7[i] <= 0.055) { 
      rwi.F30Bet$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F30Bet) - width
start = floor(width / 2)

rwi.F30Bet$cor11 = NA
rwi.F30Bet$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Bet$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Bet$cor11[i + start] = test$estimate
  rwi.F30Bet$pvalue11[i + start] = test$p.value
}

rwi.F30Bet$sig11 = NA
for (i in 1:nrow(rwi.F30Bet)) {
  if (!is.na(rwi.F30Bet$pvalue11[i])) 
    if (rwi.F30Bet$pvalue11[i] <= 0.055) { 
      rwi.F30Bet$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F30Bet) - width
start = floor(width / 2)

rwi.F30Bet$cor17 = NA
rwi.F30Bet$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Bet$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Bet$cor17[i + start] = test$estimate
  rwi.F30Bet$pvalue17[i + start] = test$p.value
}

rwi.F30Bet$sig17 = NA
for (i in 1:nrow(rwi.F30Bet)) {
  if (!is.na(rwi.F30Bet$pvalue17[i])) 
    if (rwi.F30Bet$pvalue17[i] <= 0.055) { 
      rwi.F30Bet$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F30Bet) - width
start = floor(width / 2)

rwi.F30Bet$cor25 = NA
rwi.F30Bet$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Bet$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Bet$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Bet$cor25[i + start] = test$estimate
  rwi.F30Bet$pvalue25[i + start] = test$p.value
}

rwi.F30Bet$sig25 = NA
for (i in 1:nrow(rwi.F30Bet)) {
  if (!is.na(rwi.F30Bet$pvalue25[i])) 
    if (rwi.F30Bet$pvalue25[i] <= 0.055) { 
      rwi.F30Bet$sig25[i] = 1
    }
}

# Plot
F30Bet.p1 = ggplot(data = rwi.F30Bet) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F30Bet.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("15Bet (-0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F30Bet.p1

# F30Ace (Site 15)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F30Ace) - width
start = floor(width / 2)

rwi.F30Ace$cor7 = NA
rwi.F30Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Ace$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Ace$cor7[i + start] = test$estimate
  rwi.F30Ace$pvalue7[i + start] = test$p.value
}

rwi.F30Ace$sig7 = NA
for (i in 1:nrow(rwi.F30Ace)) {
  if (!is.na(rwi.F30Ace$pvalue7[i])) 
    if (rwi.F30Ace$pvalue7[i] <= 0.055) { 
      rwi.F30Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F30Ace) - width
start = floor(width / 2)

rwi.F30Ace$cor11 = NA
rwi.F30Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Ace$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Ace$cor11[i + start] = test$estimate
  rwi.F30Ace$pvalue11[i + start] = test$p.value
}

rwi.F30Ace$sig11 = NA
for (i in 1:nrow(rwi.F30Ace)) {
  if (!is.na(rwi.F30Ace$pvalue11[i])) 
    if (rwi.F30Ace$pvalue11[i] <= 0.055) { 
      rwi.F30Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F30Ace) - width
start = floor(width / 2)

rwi.F30Ace$cor17 = NA
rwi.F30Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Ace$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Ace$cor17[i + start] = test$estimate
  rwi.F30Ace$pvalue17[i + start] = test$p.value
}

rwi.F30Ace$sig17 = NA
for (i in 1:nrow(rwi.F30Ace)) {
  if (!is.na(rwi.F30Ace$pvalue17[i])) 
    if (rwi.F30Ace$pvalue17[i] <= 0.055) { 
      rwi.F30Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F30Ace) - width
start = floor(width / 2)

rwi.F30Ace$cor25 = NA
rwi.F30Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F30Ace$xxxstd[i:(i + width)], 
                     lts.F30$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                                   lts.F30$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F30Ace$xxxstd[i:(i + width)],
                  lts.F30$Avg_CC_Median[i:(i + width)]))
  rwi.F30Ace$cor25[i + start] = test$estimate
  rwi.F30Ace$pvalue25[i + start] = test$p.value
}

rwi.F30Ace$sig25 = NA
for (i in 1:nrow(rwi.F30Ace)) {
  if (!is.na(rwi.F30Ace$pvalue25[i])) 
    if (rwi.F30Ace$pvalue25[i] <= 0.055) { 
      rwi.F30Ace$sig25[i] = 1
    }
}

# Plot
F30Ace.p1 = ggplot(data = rwi.F30Ace) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F30Ace.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("15Ace (-0.11)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F30Ace.p1

# F33Pic (Site 13)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.F33Pic) - width
start = floor(width / 2)

rwi.F33Pic$cor7 = NA
rwi.F33Pic$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F33Pic$xxxstd[i:(i + width)], 
                     lts.F33$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                                   lts.F33$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                  lts.F33$Avg_CC_Median[i:(i + width)]))
  rwi.F33Pic$cor7[i + start] = test$estimate
  rwi.F33Pic$pvalue7[i + start] = test$p.value
}

rwi.F33Pic$sig7 = NA
for (i in 1:nrow(rwi.F33Pic)) {
  if (!is.na(rwi.F33Pic$pvalue7[i])) 
    if (rwi.F33Pic$pvalue7[i] <= 0.055) { 
      rwi.F33Pic$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.F33Pic) - width
start = floor(width / 2)

rwi.F33Pic$cor11 = NA
rwi.F33Pic$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F33Pic$xxxstd[i:(i + width)], 
                     lts.F33$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                                   lts.F33$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                  lts.F33$Avg_CC_Median[i:(i + width)]))
  rwi.F33Pic$cor11[i + start] = test$estimate
  rwi.F33Pic$pvalue11[i + start] = test$p.value
}

rwi.F33Pic$sig11 = NA
for (i in 1:nrow(rwi.F33Pic)) {
  if (!is.na(rwi.F33Pic$pvalue11[i])) 
    if (rwi.F33Pic$pvalue11[i] <= 0.055) { 
      rwi.F33Pic$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.F33Pic) - width
start = floor(width / 2)

rwi.F33Pic$cor17 = NA
rwi.F33Pic$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F33Pic$xxxstd[i:(i + width)], 
                     lts.F33$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                                   lts.F33$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                  lts.F33$Avg_CC_Median[i:(i + width)]))
  rwi.F33Pic$cor17[i + start] = test$estimate
  rwi.F33Pic$pvalue17[i + start] = test$p.value
}

rwi.F33Pic$sig17 = NA
for (i in 1:nrow(rwi.F33Pic)) {
  if (!is.na(rwi.F33Pic$pvalue17[i])) 
    if (rwi.F33Pic$pvalue17[i] <= 0.055) { 
      rwi.F33Pic$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.F33Pic) - width
start = floor(width / 2)

rwi.F33Pic$cor25 = NA
rwi.F33Pic$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.F33Pic$xxxstd[i:(i + width)], 
                     lts.F33$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                                   lts.F33$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.F33Pic$xxxstd[i:(i + width)],
                  lts.F33$Avg_CC_Median[i:(i + width)]))
  rwi.F33Pic$cor25[i + start] = test$estimate
  rwi.F33Pic$pvalue25[i + start] = test$p.value
}

rwi.F33Pic$sig25 = NA
for (i in 1:nrow(rwi.F33Pic)) {
  if (!is.na(rwi.F33Pic$pvalue25[i])) 
    if (rwi.F33Pic$pvalue25[i] <= 0.055) { 
      rwi.F33Pic$sig25[i] = 1
    }
}

# Plot
F33Pic.p1 = ggplot(data = rwi.F33Pic) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = F33Pic.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("13Pic (0.31*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = "Correlation", expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 12, hjust = 105, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F33Pic.p1

# M01Pop (Site 12)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M01Pop) - width
start = floor(width / 2)

rwi.M01Pop$cor7 = NA
rwi.M01Pop$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M01Pop$xxxstd[i:(i + width)], 
                     lts.M01$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                                   lts.M01$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                  lts.M01$Avg_CC_Median[i:(i + width)]))
  rwi.M01Pop$cor7[i + start] = test$estimate
  rwi.M01Pop$pvalue7[i + start] = test$p.value
}

rwi.M01Pop$sig7 = NA
for (i in 1:nrow(rwi.M01Pop)) {
  if (!is.na(rwi.M01Pop$pvalue7[i])) 
    if (rwi.M01Pop$pvalue7[i] <= 0.055) { 
      rwi.M01Pop$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M01Pop) - width
start = floor(width / 2)

rwi.M01Pop$cor11 = NA
rwi.M01Pop$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M01Pop$xxxstd[i:(i + width)], 
                     lts.M01$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                                   lts.M01$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                  lts.M01$Avg_CC_Median[i:(i + width)]))
  rwi.M01Pop$cor11[i + start] = test$estimate
  rwi.M01Pop$pvalue11[i + start] = test$p.value
}

rwi.M01Pop$sig11 = NA
for (i in 1:nrow(rwi.M01Pop)) {
  if (!is.na(rwi.M01Pop$pvalue11[i])) 
    if (rwi.M01Pop$pvalue11[i] <= 0.055) { 
      rwi.M01Pop$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M01Pop) - width
start = floor(width / 2)

rwi.M01Pop$cor17 = NA
rwi.M01Pop$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M01Pop$xxxstd[i:(i + width)], 
                     lts.M01$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                                   lts.M01$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                  lts.M01$Avg_CC_Median[i:(i + width)]))
  rwi.M01Pop$cor17[i + start] = test$estimate
  rwi.M01Pop$pvalue17[i + start] = test$p.value
}

rwi.M01Pop$sig17 = NA
for (i in 1:nrow(rwi.M01Pop)) {
  if (!is.na(rwi.M01Pop$pvalue17[i])) 
    if (rwi.M01Pop$pvalue17[i] <= 0.055) { 
      rwi.M01Pop$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M01Pop) - width
start = floor(width / 2)

rwi.M01Pop$cor25 = NA
rwi.M01Pop$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M01Pop$xxxstd[i:(i + width)], 
                     lts.M01$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                                   lts.M01$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M01Pop$xxxstd[i:(i + width)],
                  lts.M01$Avg_CC_Median[i:(i + width)]))
  rwi.M01Pop$cor25[i + start] = test$estimate
  rwi.M01Pop$pvalue25[i + start] = test$p.value
}

rwi.M01Pop$sig25 = NA
for (i in 1:nrow(rwi.M01Pop)) {
  if (!is.na(rwi.M01Pop$pvalue25[i])) 
    if (rwi.M01Pop$pvalue25[i] <= 0.055) { 
      rwi.M01Pop$sig25[i] = 1
    }
}

# Plot
M01Pop.p1 = ggplot(data = rwi.M01Pop) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M01Pop.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("12Pop (0.57**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M01Pop.p1

# M05Thu (Site 07)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M05Thu) - width
start = floor(width / 2)

rwi.M05Thu$cor7 = NA
rwi.M05Thu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M05Thu$xxxstd[i:(i + width)], 
                     lts.M05$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                                   lts.M05$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                  lts.M05$Avg_CC_Median[i:(i + width)]))
  rwi.M05Thu$cor7[i + start] = test$estimate
  rwi.M05Thu$pvalue7[i + start] = test$p.value
}

rwi.M05Thu$sig7 = NA
for (i in 1:nrow(rwi.M05Thu)) {
  if (!is.na(rwi.M05Thu$pvalue7[i])) 
    if (rwi.M05Thu$pvalue7[i] <= 0.055) { 
      rwi.M05Thu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M05Thu) - width
start = floor(width / 2)

rwi.M05Thu$cor11 = NA
rwi.M05Thu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M05Thu$xxxstd[i:(i + width)], 
                     lts.M05$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                                   lts.M05$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                  lts.M05$Avg_CC_Median[i:(i + width)]))
  rwi.M05Thu$cor11[i + start] = test$estimate
  rwi.M05Thu$pvalue11[i + start] = test$p.value
}

rwi.M05Thu$sig11 = NA
for (i in 1:nrow(rwi.M05Thu)) {
  if (!is.na(rwi.M05Thu$pvalue11[i])) 
    if (rwi.M05Thu$pvalue11[i] <= 0.055) { 
      rwi.M05Thu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M05Thu) - width
start = floor(width / 2)

rwi.M05Thu$cor17 = NA
rwi.M05Thu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M05Thu$xxxstd[i:(i + width)], 
                     lts.M05$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                                   lts.M05$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                  lts.M05$Avg_CC_Median[i:(i + width)]))
  rwi.M05Thu$cor17[i + start] = test$estimate
  rwi.M05Thu$pvalue17[i + start] = test$p.value
}

rwi.M05Thu$sig17 = NA
for (i in 1:nrow(rwi.M05Thu)) {
  if (!is.na(rwi.M05Thu$pvalue17[i])) 
    if (rwi.M05Thu$pvalue17[i] <= 0.055) { 
      rwi.M05Thu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M05Thu) - width
start = floor(width / 2)

rwi.M05Thu$cor25 = NA
rwi.M05Thu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M05Thu$xxxstd[i:(i + width)], 
                     lts.M05$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                                   lts.M05$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M05Thu$xxxstd[i:(i + width)],
                  lts.M05$Avg_CC_Median[i:(i + width)]))
  rwi.M05Thu$cor25[i + start] = test$estimate
  rwi.M05Thu$pvalue25[i + start] = test$p.value
}

rwi.M05Thu$sig25 = NA
for (i in 1:nrow(rwi.M05Thu)) {
  if (!is.na(rwi.M05Thu$pvalue25[i])) 
    if (rwi.M05Thu$pvalue25[i] <= 0.055) { 
      rwi.M05Thu$sig25[i] = 1
    }
}

# Plot
M05Thu.p1 = ggplot(data = rwi.M05Thu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M05Thu.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("07Thu (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M05Thu.p1

# M06Ace (Site 03)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M06Ace) - width
start = floor(width / 2)

rwi.M06Ace$cor7 = NA
rwi.M06Ace$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Ace$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Ace$cor7[i + start] = test$estimate
  rwi.M06Ace$pvalue7[i + start] = test$p.value
}

rwi.M06Ace$sig7 = NA
for (i in 1:nrow(rwi.M06Ace)) {
  if (!is.na(rwi.M06Ace$pvalue7[i])) 
    if (rwi.M06Ace$pvalue7[i] <= 0.055) { 
      rwi.M06Ace$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M06Ace) - width
start = floor(width / 2)

rwi.M06Ace$cor11 = NA
rwi.M06Ace$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Ace$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Ace$cor11[i + start] = test$estimate
  rwi.M06Ace$pvalue11[i + start] = test$p.value
}

rwi.M06Ace$sig11 = NA
for (i in 1:nrow(rwi.M06Ace)) {
  if (!is.na(rwi.M06Ace$pvalue11[i])) 
    if (rwi.M06Ace$pvalue11[i] <= 0.055) { 
      rwi.M06Ace$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M06Ace) - width
start = floor(width / 2)

rwi.M06Ace$cor17 = NA
rwi.M06Ace$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Ace$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Ace$cor17[i + start] = test$estimate
  rwi.M06Ace$pvalue17[i + start] = test$p.value
}

rwi.M06Ace$sig17 = NA
for (i in 1:nrow(rwi.M06Ace)) {
  if (!is.na(rwi.M06Ace$pvalue17[i])) 
    if (rwi.M06Ace$pvalue17[i] <= 0.055) { 
      rwi.M06Ace$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M06Ace) - width
start = floor(width / 2)

rwi.M06Ace$cor25 = NA
rwi.M06Ace$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Ace$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Ace$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Ace$cor25[i + start] = test$estimate
  rwi.M06Ace$pvalue25[i + start] = test$p.value
}

rwi.M06Ace$sig25 = NA
for (i in 1:nrow(rwi.M06Ace)) {
  if (!is.na(rwi.M06Ace$pvalue25[i])) 
    if (rwi.M06Ace$pvalue25[i] <= 0.055) { 
      rwi.M06Ace$sig25[i] = 1
    }
}

# Plot
M06Ace.p1 = ggplot(data = rwi.M06Ace) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M06Ace.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("03Ace (0.24*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M06Ace.p1

# M06Que (Site 03)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M06Que) - width
start = floor(width / 2)

rwi.M06Que$cor7 = NA
rwi.M06Que$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Que$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Que$cor7[i + start] = test$estimate
  rwi.M06Que$pvalue7[i + start] = test$p.value
}

rwi.M06Que$sig7 = NA
for (i in 1:nrow(rwi.M06Que)) {
  if (!is.na(rwi.M06Que$pvalue7[i])) 
    if (rwi.M06Que$pvalue7[i] <= 0.055) { 
      rwi.M06Que$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M06Que) - width
start = floor(width / 2)

rwi.M06Que$cor11 = NA
rwi.M06Que$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Que$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Que$cor11[i + start] = test$estimate
  rwi.M06Que$pvalue11[i + start] = test$p.value
}

rwi.M06Que$sig11 = NA
for (i in 1:nrow(rwi.M06Que)) {
  if (!is.na(rwi.M06Que$pvalue11[i])) 
    if (rwi.M06Que$pvalue11[i] <= 0.055) { 
      rwi.M06Que$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M06Que) - width
start = floor(width / 2)

rwi.M06Que$cor17 = NA
rwi.M06Que$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Que$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Que$cor17[i + start] = test$estimate
  rwi.M06Que$pvalue17[i + start] = test$p.value
}

rwi.M06Que$sig17 = NA
for (i in 1:nrow(rwi.M06Que)) {
  if (!is.na(rwi.M06Que$pvalue17[i])) 
    if (rwi.M06Que$pvalue17[i] <= 0.055) { 
      rwi.M06Que$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M06Que) - width
start = floor(width / 2)

rwi.M06Que$cor25 = NA
rwi.M06Que$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M06Que$xxxstd[i:(i + width)], 
                     lts.M06$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                                   lts.M06$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M06Que$xxxstd[i:(i + width)],
                  lts.M06$Avg_CC_Median[i:(i + width)]))
  rwi.M06Que$cor25[i + start] = test$estimate
  rwi.M06Que$pvalue25[i + start] = test$p.value
}

rwi.M06Que$sig25 = NA
for (i in 1:nrow(rwi.M06Que)) {
  if (!is.na(rwi.M06Que$pvalue25[i])) 
    if (rwi.M06Que$pvalue25[i] <= 0.055) { 
      rwi.M06Que$sig25[i] = 1
    }
}

# Plot
M06Que.p1 = ggplot(data = rwi.M06Que) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M06Que.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("03Que (0.02)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
M06Que.p1

# M07Tsu (Site 04)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M07Tsu) - width
start = floor(width / 2)

rwi.M07Tsu$cor7 = NA
rwi.M07Tsu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M07Tsu$xxxstd[i:(i + width)], 
                     lts.M07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                                   lts.M07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                  lts.M07$Avg_CC_Median[i:(i + width)]))
  rwi.M07Tsu$cor7[i + start] = test$estimate
  rwi.M07Tsu$pvalue7[i + start] = test$p.value
}

rwi.M07Tsu$sig7 = NA
for (i in 1:nrow(rwi.M07Tsu)) {
  if (!is.na(rwi.M07Tsu$pvalue7[i])) 
    if (rwi.M07Tsu$pvalue7[i] <= 0.055) { 
      rwi.M07Tsu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M07Tsu) - width
start = floor(width / 2)

rwi.M07Tsu$cor11 = NA
rwi.M07Tsu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M07Tsu$xxxstd[i:(i + width)], 
                     lts.M07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                                   lts.M07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                  lts.M07$Avg_CC_Median[i:(i + width)]))
  rwi.M07Tsu$cor11[i + start] = test$estimate
  rwi.M07Tsu$pvalue11[i + start] = test$p.value
}

rwi.M07Tsu$sig11 = NA
for (i in 1:nrow(rwi.M07Tsu)) {
  if (!is.na(rwi.M07Tsu$pvalue11[i])) 
    if (rwi.M07Tsu$pvalue11[i] <= 0.055) { 
      rwi.M07Tsu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M07Tsu) - width
start = floor(width / 2)

rwi.M07Tsu$cor17 = NA
rwi.M07Tsu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M07Tsu$xxxstd[i:(i + width)], 
                     lts.M07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                                   lts.M07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                  lts.M07$Avg_CC_Median[i:(i + width)]))
  rwi.M07Tsu$cor17[i + start] = test$estimate
  rwi.M07Tsu$pvalue17[i + start] = test$p.value
}

rwi.M07Tsu$sig17 = NA
for (i in 1:nrow(rwi.M07Tsu)) {
  if (!is.na(rwi.M07Tsu$pvalue17[i])) 
    if (rwi.M07Tsu$pvalue17[i] <= 0.055) { 
      rwi.M07Tsu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M07Tsu) - width
start = floor(width / 2)

rwi.M07Tsu$cor25 = NA
rwi.M07Tsu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M07Tsu$xxxstd[i:(i + width)], 
                     lts.M07$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                                   lts.M07$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M07Tsu$xxxstd[i:(i + width)],
                  lts.M07$Avg_CC_Median[i:(i + width)]))
  rwi.M07Tsu$cor25[i + start] = test$estimate
  rwi.M07Tsu$pvalue25[i + start] = test$p.value
}

rwi.M07Tsu$sig25 = NA
for (i in 1:nrow(rwi.M07Tsu)) {
  if (!is.na(rwi.M07Tsu$pvalue25[i])) 
    if (rwi.M07Tsu$pvalue25[i] <= 0.055) { 
      rwi.M07Tsu$sig25[i] = 1
    }
}

# Plot
M07Tsu.p1 = ggplot(data = rwi.M07Tsu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M07Tsu.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("04Tsu (0.16)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
M07Tsu.p1

# M13Tsu (Site 01)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M13Tsu) - width
start = floor(width / 2)

rwi.M13Tsu$cor7 = NA
rwi.M13Tsu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M13Tsu$xxxstd[i:(i + width)], 
                     lts.M13$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                                   lts.M13$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                  lts.M13$Avg_CC_Median[i:(i + width)]))
  rwi.M13Tsu$cor7[i + start] = test$estimate
  rwi.M13Tsu$pvalue7[i + start] = test$p.value
}

rwi.M13Tsu$sig7 = NA
for (i in 1:nrow(rwi.M13Tsu)) {
  if (!is.na(rwi.M13Tsu$pvalue7[i])) 
    if (rwi.M13Tsu$pvalue7[i] <= 0.055) { 
      rwi.M13Tsu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M13Tsu) - width
start = floor(width / 2)

rwi.M13Tsu$cor11 = NA
rwi.M13Tsu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M13Tsu$xxxstd[i:(i + width)], 
                     lts.M13$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                                   lts.M13$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                  lts.M13$Avg_CC_Median[i:(i + width)]))
  rwi.M13Tsu$cor11[i + start] = test$estimate
  rwi.M13Tsu$pvalue11[i + start] = test$p.value
}

rwi.M13Tsu$sig11 = NA
for (i in 1:nrow(rwi.M13Tsu)) {
  if (!is.na(rwi.M13Tsu$pvalue11[i])) 
    if (rwi.M13Tsu$pvalue11[i] <= 0.055) { 
      rwi.M13Tsu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M13Tsu) - width
start = floor(width / 2)

rwi.M13Tsu$cor17 = NA
rwi.M13Tsu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M13Tsu$xxxstd[i:(i + width)], 
                     lts.M13$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                                   lts.M13$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                  lts.M13$Avg_CC_Median[i:(i + width)]))
  rwi.M13Tsu$cor17[i + start] = test$estimate
  rwi.M13Tsu$pvalue17[i + start] = test$p.value
}

rwi.M13Tsu$sig17 = NA
for (i in 1:nrow(rwi.M13Tsu)) {
  if (!is.na(rwi.M13Tsu$pvalue17[i])) 
    if (rwi.M13Tsu$pvalue17[i] <= 0.055) { 
      rwi.M13Tsu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M13Tsu) - width
start = floor(width / 2)

rwi.M13Tsu$cor25 = NA
rwi.M13Tsu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M13Tsu$xxxstd[i:(i + width)], 
                     lts.M13$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                                   lts.M13$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M13Tsu$xxxstd[i:(i + width)],
                  lts.M13$Avg_CC_Median[i:(i + width)]))
  rwi.M13Tsu$cor25[i + start] = test$estimate
  rwi.M13Tsu$pvalue25[i + start] = test$p.value
}

rwi.M13Tsu$sig25 = NA
for (i in 1:nrow(rwi.M13Tsu)) {
  if (!is.na(rwi.M13Tsu$pvalue25[i])) 
    if (rwi.M13Tsu$pvalue25[i] <= 0.055) { 
      rwi.M13Tsu$sig25[i] = 1
    }
}

# Plot
M13Tsu.p1 = ggplot(data = rwi.M13Tsu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M13Tsu.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("01Tsu (0.64**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M13Tsu.p1

# M17Thu (Site 16)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M17Thu) - width
start = floor(width / 2)

rwi.M17Thu$cor7 = NA
rwi.M17Thu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M17Thu$xxxstd[i:(i + width)], 
                     lts.M17$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                                   lts.M17$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                  lts.M17$Avg_CC_Median[i:(i + width)]))
  rwi.M17Thu$cor7[i + start] = test$estimate
  rwi.M17Thu$pvalue7[i + start] = test$p.value
}

rwi.M17Thu$sig7 = NA
for (i in 1:nrow(rwi.M17Thu)) {
  if (!is.na(rwi.M17Thu$pvalue7[i])) 
    if (rwi.M17Thu$pvalue7[i] <= 0.055) { 
      rwi.M17Thu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M17Thu) - width
start = floor(width / 2)

rwi.M17Thu$cor11 = NA
rwi.M17Thu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M17Thu$xxxstd[i:(i + width)], 
                     lts.M17$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                                   lts.M17$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                  lts.M17$Avg_CC_Median[i:(i + width)]))
  rwi.M17Thu$cor11[i + start] = test$estimate
  rwi.M17Thu$pvalue11[i + start] = test$p.value
}

rwi.M17Thu$sig11 = NA
for (i in 1:nrow(rwi.M17Thu)) {
  if (!is.na(rwi.M17Thu$pvalue11[i])) 
    if (rwi.M17Thu$pvalue11[i] <= 0.055) { 
      rwi.M17Thu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M17Thu) - width
start = floor(width / 2)

rwi.M17Thu$cor17 = NA
rwi.M17Thu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M17Thu$xxxstd[i:(i + width)], 
                     lts.M17$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                                   lts.M17$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                  lts.M17$Avg_CC_Median[i:(i + width)]))
  rwi.M17Thu$cor17[i + start] = test$estimate
  rwi.M17Thu$pvalue17[i + start] = test$p.value
}

rwi.M17Thu$sig17 = NA
for (i in 1:nrow(rwi.M17Thu)) {
  if (!is.na(rwi.M17Thu$pvalue17[i])) 
    if (rwi.M17Thu$pvalue17[i] <= 0.055) { 
      rwi.M17Thu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M17Thu) - width
start = floor(width / 2)

rwi.M17Thu$cor25 = NA
rwi.M17Thu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M17Thu$xxxstd[i:(i + width)], 
                     lts.M17$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                                   lts.M17$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M17Thu$xxxstd[i:(i + width)],
                  lts.M17$Avg_CC_Median[i:(i + width)]))
  rwi.M17Thu$cor25[i + start] = test$estimate
  rwi.M17Thu$pvalue25[i + start] = test$p.value
}

rwi.M17Thu$sig25 = NA
for (i in 1:nrow(rwi.M17Thu)) {
  if (!is.na(rwi.M17Thu$pvalue25[i])) 
    if (rwi.M17Thu$pvalue25[i] <= 0.055) { 
      rwi.M17Thu$sig25[i] = 1
    }
}

# Plot
M17Thu.p1 = ggplot(data = rwi.M17Thu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M17Thu.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("16Thu (0.43*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = "Correlation", expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M17Thu.p1

# M20Thu (Site 14)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M20Thu) - width
start = floor(width / 2)

rwi.M20Thu$cor7 = NA
rwi.M20Thu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M20Thu$xxxstd[i:(i + width)], 
                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                                   lts.M20$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                  lts.M20$Avg_CC_Median[i:(i + width)]))
  rwi.M20Thu$cor7[i + start] = test$estimate
  rwi.M20Thu$pvalue7[i + start] = test$p.value
}

rwi.M20Thu$sig7 = NA
for (i in 1:nrow(rwi.M20Thu)) {
  if (!is.na(rwi.M20Thu$pvalue7[i])) 
    if (rwi.M20Thu$pvalue7[i] <= 0.055) { 
      rwi.M20Thu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M20Thu) - width
start = floor(width / 2)

rwi.M20Thu$cor11 = NA
rwi.M20Thu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M20Thu$xxxstd[i:(i + width)], 
                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                                   lts.M20$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                  lts.M20$Avg_CC_Median[i:(i + width)]))
  rwi.M20Thu$cor11[i + start] = test$estimate
  rwi.M20Thu$pvalue11[i + start] = test$p.value
}

rwi.M20Thu$sig11 = NA
for (i in 1:nrow(rwi.M20Thu)) {
  if (!is.na(rwi.M20Thu$pvalue11[i])) 
    if (rwi.M20Thu$pvalue11[i] <= 0.055) { 
      rwi.M20Thu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M20Thu) - width
start = floor(width / 2)

rwi.M20Thu$cor17 = NA
rwi.M20Thu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M20Thu$xxxstd[i:(i + width)], 
                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                                   lts.M20$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                  lts.M20$Avg_CC_Median[i:(i + width)]))
  rwi.M20Thu$cor17[i + start] = test$estimate
  rwi.M20Thu$pvalue17[i + start] = test$p.value
}

rwi.M20Thu$sig17 = NA
for (i in 1:nrow(rwi.M20Thu)) {
  if (!is.na(rwi.M20Thu$pvalue17[i])) 
    if (rwi.M20Thu$pvalue17[i] <= 0.055) { 
      rwi.M20Thu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M20Thu) - width
start = floor(width / 2)

rwi.M20Thu$cor25 = NA
rwi.M20Thu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M20Thu$xxxstd[i:(i + width)], 
                     lts.M20$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                                   lts.M20$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M20Thu$xxxstd[i:(i + width)],
                  lts.M20$Avg_CC_Median[i:(i + width)]))
  rwi.M20Thu$cor25[i + start] = test$estimate
  rwi.M20Thu$pvalue25[i + start] = test$p.value
}

rwi.M20Thu$sig25 = NA
for (i in 1:nrow(rwi.M20Thu)) {
  if (!is.na(rwi.M20Thu$pvalue25[i])) 
    if (rwi.M20Thu$pvalue25[i] <= 0.055) { 
      rwi.M20Thu$sig25[i] = 1
    }
}

# Plot
M20Thu.p1 = ggplot(data = rwi.M20Thu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M20Thu.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("14Thu (0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
M20Thu.p1

# M26Thu (Site 10)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M26Thu) - width
start = floor(width / 2)

rwi.M26Thu$cor7 = NA
rwi.M26Thu$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Thu$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Thu$cor7[i + start] = test$estimate
  rwi.M26Thu$pvalue7[i + start] = test$p.value
}

rwi.M26Thu$sig7 = NA
for (i in 1:nrow(rwi.M26Thu)) {
  if (!is.na(rwi.M26Thu$pvalue7[i])) 
    if (rwi.M26Thu$pvalue7[i] <= 0.055) { 
      rwi.M26Thu$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M26Thu) - width
start = floor(width / 2)

rwi.M26Thu$cor11 = NA
rwi.M26Thu$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Thu$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Thu$cor11[i + start] = test$estimate
  rwi.M26Thu$pvalue11[i + start] = test$p.value
}

rwi.M26Thu$sig11 = NA
for (i in 1:nrow(rwi.M26Thu)) {
  if (!is.na(rwi.M26Thu$pvalue11[i])) 
    if (rwi.M26Thu$pvalue11[i] <= 0.055) { 
      rwi.M26Thu$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M26Thu) - width
start = floor(width / 2)

rwi.M26Thu$cor17 = NA
rwi.M26Thu$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Thu$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Thu$cor17[i + start] = test$estimate
  rwi.M26Thu$pvalue17[i + start] = test$p.value
}

rwi.M26Thu$sig17 = NA
for (i in 1:nrow(rwi.M26Thu)) {
  if (!is.na(rwi.M26Thu$pvalue17[i])) 
    if (rwi.M26Thu$pvalue17[i] <= 0.055) { 
      rwi.M26Thu$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M26Thu) - width
start = floor(width / 2)

rwi.M26Thu$cor25 = NA
rwi.M26Thu$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Thu$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Thu$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Thu$cor25[i + start] = test$estimate
  rwi.M26Thu$pvalue25[i + start] = test$p.value
}

rwi.M26Thu$sig25 = NA
for (i in 1:nrow(rwi.M26Thu)) {
  if (!is.na(rwi.M26Thu$pvalue25[i])) 
    if (rwi.M26Thu$pvalue25[i] <= 0.055) { 
      rwi.M26Thu$sig25[i] = 1
    }
}

# Plot
M26Thu.p1 = ggplot(data = rwi.M26Thu) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M26Thu.cor$estimate, lty = 2, col = "gray48") +
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("10Thu (0.55*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M26Thu.p1

# M26Dec (Site 10)
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M26Dec) - width
start = floor(width / 2)

rwi.M26Dec$cor7 = NA
rwi.M26Dec$pvalue7 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Dec$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Dec$cor7[i + start] = test$estimate
  rwi.M26Dec$pvalue7[i + start] = test$p.value
}

rwi.M26Dec$sig7 = NA
for (i in 1:nrow(rwi.M26Dec)) {
  if (!is.na(rwi.M26Dec$pvalue7[i])) 
    if (rwi.M26Dec$pvalue7[i] <= 0.055) { 
      rwi.M26Dec$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M26Dec) - width
start = floor(width / 2)

rwi.M26Dec$cor11 = NA
rwi.M26Dec$pvalue11 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Dec$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Dec$cor11[i + start] = test$estimate
  rwi.M26Dec$pvalue11[i + start] = test$p.value
}

rwi.M26Dec$sig11 = NA
for (i in 1:nrow(rwi.M26Dec)) {
  if (!is.na(rwi.M26Dec$pvalue11[i])) 
    if (rwi.M26Dec$pvalue11[i] <= 0.055) { 
      rwi.M26Dec$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M26Dec) - width
start = floor(width / 2)

rwi.M26Dec$cor17 = NA
rwi.M26Dec$pvalue17 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Dec$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Dec$cor17[i + start] = test$estimate
  rwi.M26Dec$pvalue17[i + start] = test$p.value
}

rwi.M26Dec$sig17 = NA
for (i in 1:nrow(rwi.M26Dec)) {
  if (!is.na(rwi.M26Dec$pvalue17[i])) 
    if (rwi.M26Dec$pvalue17[i] <= 0.055) { 
      rwi.M26Dec$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M26Dec) - width
start = floor(width / 2)

rwi.M26Dec$cor25 = NA
rwi.M26Dec$pvalue25 = NA
for (i in 1:end) {
  test = my.cor.test(rwi.M26Dec$xxxstd[i:(i + width)], 
                     lts.M26$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                                   lts.M26$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M26Dec$xxxstd[i:(i + width)],
                  lts.M26$Avg_CC_Median[i:(i + width)]))
  rwi.M26Dec$cor25[i + start] = test$estimate
  rwi.M26Dec$pvalue25[i + start] = test$p.value
}

rwi.M26Dec$sig25 = NA
for (i in 1:nrow(rwi.M26Dec)) {
  if (!is.na(rwi.M26Dec$pvalue25[i])) 
    if (rwi.M26Dec$pvalue25[i] <= 0.055) { 
      rwi.M26Dec$sig25[i] = 1
    }
}

# Plot
M26Dec.p1 = ggplot(data = rwi.M26Dec) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M26Dec.cor$estimate, lty = 2, col = "gray48") +
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("10Dec (-0.17)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
M26Dec.p1

# M27Pin
# Running correlation: 7
width = 7 - 1
end = nrow(rwi.M27Pin) - width
start = floor(width / 2)

rwi.M27Pin$cor7 = NA
rwi.M27Pin$pvalue7 = NA
for (i in 10:end) {
  test = my.cor.test(rwi.M27Pin$xxxstd[i:(i + width)], 
                     lts.M27$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                                   lts.M27$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                  lts.M27$Avg_CC_Median[i:(i + width)]))
  rwi.M27Pin$cor7[i + start] = test$estimate
  rwi.M27Pin$pvalue7[i + start] = test$p.value
}

rwi.M27Pin$sig7 = NA
for (i in 10:nrow(rwi.M27Pin)) {
  if (!is.na(rwi.M27Pin$pvalue7[i])) 
    if (rwi.M27Pin$pvalue7[i] <= 0.055) { 
      rwi.M27Pin$sig7[i] = 1
    }
}

# Running correlation: 11
width = 11 - 1
end = nrow(rwi.M27Pin) - width
start = floor(width / 2)

rwi.M27Pin$cor11 = NA
rwi.M27Pin$pvalue11 = NA
for (i in 10:end) {
  test = my.cor.test(rwi.M27Pin$xxxstd[i:(i + width)], 
                     lts.M27$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                                   lts.M27$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                  lts.M27$Avg_CC_Median[i:(i + width)]))
  rwi.M27Pin$cor11[i + start] = test$estimate
  rwi.M27Pin$pvalue11[i + start] = test$p.value
}

rwi.M27Pin$sig11 = NA
for (i in 10:nrow(rwi.M27Pin)) {
  if (!is.na(rwi.M27Pin$pvalue11[i])) 
    if (rwi.M27Pin$pvalue11[i] <= 0.055) { 
      rwi.M27Pin$sig11[i] = 1
    }
}

# Running correlation: 17
width = 17 - 1
end = nrow(rwi.M27Pin) - width
start = floor(width / 2)

rwi.M27Pin$cor17 = NA
rwi.M27Pin$pvalue17 = NA
for (i in 10:end) {
  test = my.cor.test(rwi.M27Pin$xxxstd[i:(i + width)], 
                     lts.M27$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                                   lts.M27$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                  lts.M27$Avg_CC_Median[i:(i + width)]))
  rwi.M27Pin$cor17[i + start] = test$estimate
  rwi.M27Pin$pvalue17[i + start] = test$p.value
}

rwi.M27Pin$sig17 = NA
for (i in 10:nrow(rwi.M27Pin)) {
  if (!is.na(rwi.M27Pin$pvalue17[i])) 
    if (rwi.M27Pin$pvalue17[i] <= 0.055) { 
      rwi.M27Pin$sig17[i] = 1
    }
}

# Running correlation: 25
width = 25 - 1
end = nrow(rwi.M27Pin) - width
start = floor(width / 2)

rwi.M27Pin$cor25 = NA
rwi.M27Pin$pvalue25 = NA
for (i in 10:end) {
  test = my.cor.test(rwi.M27Pin$xxxstd[i:(i + width)], 
                     lts.M27$Avg_CC_Median[i:(i + width)], alternative = "greater",
                     method = "pearson",
                     n = calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                                   lts.M27$Avg_CC_Median[i:(i + width)]))
  print(calc.neff(rwi.M27Pin$xxxstd[i:(i + width)],
                  lts.M27$Avg_CC_Median[i:(i + width)]))
  rwi.M27Pin$cor25[i + start] = test$estimate
  rwi.M27Pin$pvalue25[i + start] = test$p.value
}

rwi.M27Pin$sig25 = NA
for (i in 10:nrow(rwi.M27Pin)) {
  if (!is.na(rwi.M27Pin$pvalue25[i])) 
    if (rwi.M27Pin$pvalue25[i] <= 0.055) { 
      rwi.M27Pin$sig25[i] = 1
    }
}

# Plot
M27Pin.p1 = ggplot(data = rwi.M27Pin) + 
  geom_hline(yintercept = 0, col = "gray48") +
  geom_hline(yintercept = M27Pin.cor$estimate, lty = 2, col = "gray48") + 
  geom_line(aes(x = year, y = cor7), col = "black") +
  geom_point(aes(x = year, y = sig7 - 1.9), shape = 15, color = "black", size = 1.5) + 
  geom_line(aes(x = year, y = cor11), col = "red3") + 
  geom_point(aes(x = year, y = sig11 - 1.8), shape = 15, color = "red3", size = 1.5) +
  geom_line(aes(x = year, y = cor17), col = "darkgreen") + 
  geom_point(aes(x = year, y = sig17 - 1.7), shape = 15, color = "darkgreen", size = 1.5) +
  geom_line(aes(x = year, y = cor25), col = "blue") +
  geom_point(aes(x = year, y = sig25 - 1.6), shape = 15, color = "blue", size = 1.5) +
  ggtitle("09Pin (0.20)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1),
                     breaks = seq(-1,1,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "italic"))
M27Pin.p1
#####
tiff("tempcorplot2.tiff", units = "in", width = 6.5, height = 5, res = 300)
(M13Tsu.p1 | F15Que.p1 | M06Ace.p1 | M07Tsu.p1) /
  (F23Ace.p1 | F21Dec.p1 | M05Thu.p1 | F07Ace.p1) / 
  (M27Pin.p1 | M26Thu.p1 | F25Ace.p1 | M01Pop.p1) /
  (F33Pic.p1 | M20Thu.p1 | F30Bet.p1 | M17Thu.p1)
dev.off()

##### Correlation through time - flattened #####
#####

### Cor 7 ###
#cor7_avg = rowMeans(data.frame(rwi.F07Ace$cor7, rwi.F15Ace$cor7, rwi.F15Que$cor7, rwi.F21Dec$cor7,
#                               rwi.F23Ace$cor7, rwi.F25Ace$cor7, rwi.F30Bet$cor7, rwi.F30Bet$cor7,
#                               rwi.F33Pic$cor7, rwi.M01Pop$cor7, rwi.M05Thu$cor7, rwi.M06Que$cor7,
#                               rwi.M06Ace$cor7, rwi.M07Tsu$cor7, rwi.M13Tsu$cor7,
#                               rwi.M17Thu$cor7, rwi.M20Thu$cor7, rwi.M26Thu$cor7, rwi.M26Dec$cor7,
#                               rwi.M27Pin$cor7), na.rm = TRUE)
#cor7_avg

rwi.F07Ace$year[rwi.F07Ace$sig7 == 1][!is.na(rwi.F07Ace$year[rwi.F07Ace$sig7 == 1])]
rwi.F15Que$year[rwi.F15Que$sig7 == 1][!is.na(rwi.F15Que$year[rwi.F15Que$sig7 == 1])]
rwi.F15Ace$year[rwi.F15Ace$sig7 == 1][!is.na(rwi.F15Ace$year[rwi.F15Ace$sig7 == 1])]
rwi.F21Dec$year[rwi.F21Dec$sig7 == 1][!is.na(rwi.F21Dec$year[rwi.F21Dec$sig7 == 1])]
rwi.F23Ace$year[rwi.F23Ace$sig7 == 1][!is.na(rwi.F23Ace$year[rwi.F23Ace$sig7 == 1])]
rwi.F25Ace$year[rwi.F25Ace$sig7 == 1][!is.na(rwi.F25Ace$year[rwi.F25Ace$sig7 == 1])]
rwi.F30Bet$year[rwi.F30Bet$sig7 == 1][!is.na(rwi.F30Bet$year[rwi.F30Bet$sig7 == 1])]
rwi.F30Ace$year[rwi.F30Ace$sig7 == 1][!is.na(rwi.F30Ace$year[rwi.F30Ace$sig7 == 1])]
rwi.F33Pic$year[rwi.F33Pic$sig7 == 1][!is.na(rwi.F33Pic$year[rwi.F33Pic$sig7 == 1])]
rwi.M01Pop$year[rwi.M01Pop$sig7 == 1][!is.na(rwi.M01Pop$year[rwi.M01Pop$sig7 == 1])]
rwi.M05Thu$year[rwi.M05Thu$sig7 == 1][!is.na(rwi.M05Thu$year[rwi.M05Thu$sig7 == 1])]
rwi.M06Ace$year[rwi.M06Ace$sig7 == 1][!is.na(rwi.M06Ace$year[rwi.M06Ace$sig7 == 1])]
rwi.M06Que$year[rwi.M06Que$sig7 == 1][!is.na(rwi.M06Que$year[rwi.M06Que$sig7 == 1])]
rwi.M07Tsu$year[rwi.M07Tsu$sig7 == 1][!is.na(rwi.M07Tsu$year[rwi.M07Tsu$sig7 == 1])]
rwi.M13Tsu$year[rwi.M13Tsu$sig7 == 1][!is.na(rwi.M13Tsu$year[rwi.M13Tsu$sig7 == 1])]
rwi.M17Thu$year[rwi.M17Thu$sig7 == 1][!is.na(rwi.M17Thu$year[rwi.M17Thu$sig7 == 1])]
rwi.M20Thu$year[rwi.M20Thu$sig7 == 1][!is.na(rwi.M20Thu$year[rwi.M20Thu$sig7 == 1])]
rwi.M26Thu$year[rwi.M26Thu$sig7 == 1][!is.na(rwi.M26Thu$year[rwi.M26Thu$sig7 == 1])]
rwi.M26Dec$year[rwi.M26Dec$sig7 == 1][!is.na(rwi.M26Dec$year[rwi.M26Dec$sig7 == 1])]
rwi.M27Pin$year[rwi.M27Pin$sig7 == 1][!is.na(rwi.M27Pin$year[rwi.M27Pin$sig7 == 1])]

cor7_sig = read.csv("TempCorSig7Years.csv")

cor7_avg.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor7), alpha = 0.25) +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor7), alpha = 0.25) +
  #geom_line(aes(x = 1972:2018, y = cor7_avg), lwd = 1) +
  scale_y_continuous(name = "RWI-CC Correlation", expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 0.9, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))
cor7_avg.p

cor7_sig.p = ggplot(cor7_sig) +
  geom_histogram(aes(x = Year), fill = "black", binwidth = 1) +
  scale_y_continuous(name = "Frequency", expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 4.75, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"))
cor7_sig.p

cor7_avg.p | cor7_sig.p

### Cor 11 ###
#cor11_avg = rowMeans(data.frame(rwi.F07Ace$cor11, rwi.F15Que$cor11, rwi.F21Dec$cor11,
#                               rwi.F23Ace$cor11, rwi.F25Ace$cor11, rwi.F30Bet$cor11,
#                               rwi.F33Pic$cor11, rwi.M01Pop$cor11, rwi.M05Thu$cor11,
#                               rwi.M06Ace$cor11, rwi.M07Tsu$cor11, rwi.M13Tsu$cor11,
#                               rwi.M17Thu$cor11, rwi.M20Thu$cor11, rwi.M26Thu$cor11,
#                               rwi.M27Pin$cor11), na.rm = TRUE)
#cor11_avg

rwi.F07Ace$year[rwi.F07Ace$sig11 == 1][!is.na(rwi.F07Ace$year[rwi.F07Ace$sig11 == 1])]
rwi.F15Que$year[rwi.F15Que$sig11 == 1][!is.na(rwi.F15Que$year[rwi.F15Que$sig11 == 1])]
rwi.F15Ace$year[rwi.F15Ace$sig11 == 1][!is.na(rwi.F15Ace$year[rwi.F15Ace$sig11 == 1])]
rwi.F21Dec$year[rwi.F21Dec$sig11 == 1][!is.na(rwi.F21Dec$year[rwi.F21Dec$sig11 == 1])]
rwi.F23Ace$year[rwi.F23Ace$sig11 == 1][!is.na(rwi.F23Ace$year[rwi.F23Ace$sig11 == 1])]
rwi.F25Ace$year[rwi.F25Ace$sig11 == 1][!is.na(rwi.F25Ace$year[rwi.F25Ace$sig11 == 1])]
rwi.F30Bet$year[rwi.F30Bet$sig11 == 1][!is.na(rwi.F30Bet$year[rwi.F30Bet$sig11 == 1])]
rwi.F30Ace$year[rwi.F30Ace$sig11 == 1][!is.na(rwi.F30Ace$year[rwi.F30Ace$sig11 == 1])]
rwi.F33Pic$year[rwi.F33Pic$sig11 == 1][!is.na(rwi.F33Pic$year[rwi.F33Pic$sig11 == 1])]
rwi.M01Pop$year[rwi.M01Pop$sig11 == 1][!is.na(rwi.M01Pop$year[rwi.M01Pop$sig11 == 1])]
rwi.M05Thu$year[rwi.M05Thu$sig11 == 1][!is.na(rwi.M05Thu$year[rwi.M05Thu$sig11 == 1])]
rwi.M06Ace$year[rwi.M06Ace$sig11 == 1][!is.na(rwi.M06Ace$year[rwi.M06Ace$sig11 == 1])]
rwi.M06Que$year[rwi.M06Que$sig11 == 1][!is.na(rwi.M06Que$year[rwi.M06Que$sig11 == 1])]
rwi.M07Tsu$year[rwi.M07Tsu$sig11 == 1][!is.na(rwi.M07Tsu$year[rwi.M07Tsu$sig11 == 1])]
rwi.M13Tsu$year[rwi.M13Tsu$sig11 == 1][!is.na(rwi.M13Tsu$year[rwi.M13Tsu$sig11 == 1])]
rwi.M17Thu$year[rwi.M17Thu$sig11 == 1][!is.na(rwi.M17Thu$year[rwi.M17Thu$sig11 == 1])]
rwi.M20Thu$year[rwi.M20Thu$sig11 == 1][!is.na(rwi.M20Thu$year[rwi.M20Thu$sig11 == 1])]
rwi.M26Thu$year[rwi.M26Thu$sig11 == 1][!is.na(rwi.M26Thu$year[rwi.M26Thu$sig11 == 1])]
rwi.M26Dec$year[rwi.M26Dec$sig11 == 1][!is.na(rwi.M26Dec$year[rwi.M26Dec$sig11 == 1])]
rwi.M27Pin$year[rwi.M27Pin$sig11 == 1][!is.na(rwi.M27Pin$year[rwi.M27Pin$sig11 == 1])]

cor11_sig = read.csv("TempCorSig11Years.csv")

cor11_avg.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor11), alpha = 0.25, color = "red3") +
  #geom_line(aes(x = 1972:2018, y = cor11_avg), color = "red3", lwd = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))
cor11_avg.p

cor11_sig.p = ggplot(cor11_sig) +
  geom_histogram(aes(x = Year), fill = "red3", binwidth = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"))
cor11_sig.p

cor11_avg.p | cor11_sig.p

### Cor 17 ###
#cor17_avg = rowMeans(data.frame(rwi.F07Ace$cor17, rwi.F15Que$cor17, rwi.F21Dec$cor17,
#                                rwi.F23Ace$cor17, rwi.F25Ace$cor17, rwi.F30Bet$cor17,
#                                rwi.F33Pic$cor17, rwi.M01Pop$cor17, rwi.M05Thu$cor17,
#                                rwi.M06Ace$cor17, rwi.M07Tsu$cor17, rwi.M13Tsu$cor17,
#                                rwi.M17Thu$cor17, rwi.M20Thu$cor17, rwi.M26Thu$cor17,
#                                rwi.M27Pin$cor17), na.rm = TRUE)
#cor17_avg

rwi.F07Ace$year[rwi.F07Ace$sig17 == 1][!is.na(rwi.F07Ace$year[rwi.F07Ace$sig17 == 1])]
rwi.F15Que$year[rwi.F15Que$sig17 == 1][!is.na(rwi.F15Que$year[rwi.F15Que$sig17 == 1])]
rwi.F15Ace$year[rwi.F15Ace$sig17 == 1][!is.na(rwi.F15Ace$year[rwi.F15Ace$sig17 == 1])]
rwi.F21Dec$year[rwi.F21Dec$sig17 == 1][!is.na(rwi.F21Dec$year[rwi.F21Dec$sig17 == 1])]
rwi.F23Ace$year[rwi.F23Ace$sig17 == 1][!is.na(rwi.F23Ace$year[rwi.F23Ace$sig17 == 1])]
rwi.F25Ace$year[rwi.F25Ace$sig17 == 1][!is.na(rwi.F25Ace$year[rwi.F25Ace$sig17 == 1])]
rwi.F30Bet$year[rwi.F30Bet$sig17 == 1][!is.na(rwi.F30Bet$year[rwi.F30Bet$sig17 == 1])]
rwi.F30Ace$year[rwi.F30Ace$sig17 == 1][!is.na(rwi.F30Ace$year[rwi.F30Ace$sig17 == 1])]
rwi.F33Pic$year[rwi.F33Pic$sig17 == 1][!is.na(rwi.F33Pic$year[rwi.F33Pic$sig17 == 1])]
rwi.M01Pop$year[rwi.M01Pop$sig17 == 1][!is.na(rwi.M01Pop$year[rwi.M01Pop$sig17 == 1])]
rwi.M05Thu$year[rwi.M05Thu$sig17 == 1][!is.na(rwi.M05Thu$year[rwi.M05Thu$sig17 == 1])]
rwi.M06Ace$year[rwi.M06Ace$sig17 == 1][!is.na(rwi.M06Ace$year[rwi.M06Ace$sig17 == 1])]
rwi.M06Que$year[rwi.M06Que$sig17 == 1][!is.na(rwi.M06Que$year[rwi.M06Que$sig17 == 1])]
rwi.M07Tsu$year[rwi.M07Tsu$sig17 == 1][!is.na(rwi.M07Tsu$year[rwi.M07Tsu$sig17 == 1])]
rwi.M13Tsu$year[rwi.M13Tsu$sig17 == 1][!is.na(rwi.M13Tsu$year[rwi.M13Tsu$sig17 == 1])]
rwi.M17Thu$year[rwi.M17Thu$sig17 == 1][!is.na(rwi.M17Thu$year[rwi.M17Thu$sig17 == 1])]
rwi.M20Thu$year[rwi.M20Thu$sig17 == 1][!is.na(rwi.M20Thu$year[rwi.M20Thu$sig17 == 1])]
rwi.M26Thu$year[rwi.M26Thu$sig17 == 1][!is.na(rwi.M26Thu$year[rwi.M26Thu$sig17 == 1])]
rwi.M26Dec$year[rwi.M26Dec$sig17 == 1][!is.na(rwi.M26Dec$year[rwi.M26Dec$sig17 == 1])]
rwi.M27Pin$year[rwi.M27Pin$sig17 == 1][!is.na(rwi.M27Pin$year[rwi.M27Pin$sig17 == 1])]

cor17_sig = read.csv("TempCorSig17Years.csv")

cor17_avg.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor17), alpha = 0.25, color = "darkgreen") +
  #geom_line(aes(x = 1972:2018, y = cor17_avg), color = "darkgreen", lwd = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))
cor17_avg.p

cor17_sig.p = ggplot(cor17_sig) +
  geom_histogram(aes(x = Year), fill = "darkgreen", binwidth = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"))
cor17_sig.p

cor17_avg.p | cor17_sig.p

### Cor 25 ###
#cor25_avg = rowMeans(data.frame(rwi.F07Ace$cor25, rwi.F15Que$cor25, rwi.F21Dec$cor25,
#                                rwi.F23Ace$cor25, rwi.F25Ace$cor25, rwi.F30Bet$cor25,
#                                rwi.F33Pic$cor25, rwi.M01Pop$cor25, rwi.M05Thu$cor25,
#                                rwi.M06Ace$cor25, rwi.M07Tsu$cor25, rwi.M13Tsu$cor25,
#                                rwi.M17Thu$cor25, rwi.M20Thu$cor25, rwi.M26Thu$cor25,
#                                rwi.M27Pin$cor25), na.rm = TRUE)
#cor25_avg

rwi.F07Ace$year[rwi.F07Ace$sig25 == 1][!is.na(rwi.F07Ace$year[rwi.F07Ace$sig25 == 1])]
rwi.F15Que$year[rwi.F15Que$sig25 == 1][!is.na(rwi.F15Que$year[rwi.F15Que$sig25 == 1])]
rwi.F15Ace$year[rwi.F15Ace$sig25 == 1][!is.na(rwi.F15Ace$year[rwi.F15Ace$sig25 == 1])]
rwi.F21Dec$year[rwi.F21Dec$sig25 == 1][!is.na(rwi.F21Dec$year[rwi.F21Dec$sig25 == 1])]
rwi.F23Ace$year[rwi.F23Ace$sig25 == 1][!is.na(rwi.F23Ace$year[rwi.F23Ace$sig25 == 1])]
rwi.F25Ace$year[rwi.F25Ace$sig25 == 1][!is.na(rwi.F25Ace$year[rwi.F25Ace$sig25 == 1])]
rwi.F30Bet$year[rwi.F30Bet$sig25 == 1][!is.na(rwi.F30Bet$year[rwi.F30Bet$sig25 == 1])]
rwi.F30Ace$year[rwi.F30Ace$sig25 == 1][!is.na(rwi.F30Ace$year[rwi.F30Ace$sig25 == 1])]
rwi.F33Pic$year[rwi.F33Pic$sig25 == 1][!is.na(rwi.F33Pic$year[rwi.F33Pic$sig25 == 1])]
rwi.M01Pop$year[rwi.M01Pop$sig25 == 1][!is.na(rwi.M01Pop$year[rwi.M01Pop$sig25 == 1])]
rwi.M05Thu$year[rwi.M05Thu$sig25 == 1][!is.na(rwi.M05Thu$year[rwi.M05Thu$sig25 == 1])]
rwi.M06Ace$year[rwi.M06Ace$sig25 == 1][!is.na(rwi.M06Ace$year[rwi.M06Ace$sig25 == 1])]
rwi.M06Que$year[rwi.M06Que$sig25 == 1][!is.na(rwi.M06Que$year[rwi.M06Que$sig25 == 1])]
rwi.M07Tsu$year[rwi.M07Tsu$sig25 == 1][!is.na(rwi.M07Tsu$year[rwi.M07Tsu$sig25 == 1])]
rwi.M13Tsu$year[rwi.M13Tsu$sig25 == 1][!is.na(rwi.M13Tsu$year[rwi.M13Tsu$sig25 == 1])]
rwi.M17Thu$year[rwi.M17Thu$sig25 == 1][!is.na(rwi.M17Thu$year[rwi.M17Thu$sig25 == 1])]
rwi.M20Thu$year[rwi.M20Thu$sig25 == 1][!is.na(rwi.M20Thu$year[rwi.M20Thu$sig25 == 1])]
rwi.M26Thu$year[rwi.M26Thu$sig25 == 1][!is.na(rwi.M26Thu$year[rwi.M26Thu$sig25 == 1])]
rwi.M26Dec$year[rwi.M26Dec$sig25 == 1][!is.na(rwi.M26Dec$year[rwi.M26Dec$sig25 == 1])]
rwi.M27Pin$year[rwi.M27Pin$sig25 == 1][!is.na(rwi.M27Pin$year[rwi.M27Pin$sig25 == 1])]

cor25_sig = read.csv("TempCorSig25Years.csv")

cor25_avg.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor25), alpha = 0.25, color = "blue") +
  #geom_line(aes(x = 1972:2018, y = cor25_avg), color = "blue", lwd = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor25_sig.p = ggplot(cor25_sig) +
  geom_histogram(aes(x = Year), fill = "blue", binwidth = 1) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"))
cor25_sig.p

cor25_avg.p | cor25_sig.p
#####
tiff("tempcorflat.tiff", units = "in", width = 6.5, height = 4, res = 300)
(cor7_avg.p | cor11_avg.p | cor17_avg.p | cor25_avg.p) /
  (cor7_sig.p | cor11_sig.p | cor17_sig.p | cor25_sig.p) 
dev.off()

##### Subset plots #####
#####
# Rural vs Urban
cor7_avg_urb = rowMeans(data.frame(rwi.F15Que$cor7, rwi.F21Dec$cor7, rwi.F23Ace$cor7, 
                                   rwi.F30Bet$cor7, rwi.M06Ace$cor7, rwi.M07Tsu$cor7, 
                                   rwi.M13Tsu$cor7, rwi.M17Thu$cor7), na.rm = TRUE)
cor7_avg_rur = rowMeans(data.frame(rwi.F07Ace$cor7, rwi.F25Ace$cor7, rwi.F33Pic$cor7, 
                                   rwi.M01Pop$cor7, rwi.M05Thu$cor7, rwi.M20Thu$cor7, 
                                   rwi.M26Thu$cor7, rwi.M27Pin$cor7), na.rm = TRUE)
cor11_avg_urb = rowMeans(data.frame(rwi.F15Que$cor11, rwi.F21Dec$cor11, rwi.F23Ace$cor11, 
                                   rwi.F30Bet$cor11, rwi.M06Ace$cor11, rwi.M07Tsu$cor11, 
                                   rwi.M13Tsu$cor11, rwi.M17Thu$cor11), na.rm = TRUE)
cor11_avg_rur = rowMeans(data.frame(rwi.F07Ace$cor11, rwi.F25Ace$cor11, rwi.F33Pic$cor11, 
                                   rwi.M01Pop$cor11, rwi.M05Thu$cor11, rwi.M20Thu$cor11, 
                                   rwi.M26Thu$cor11, rwi.M27Pin$cor11), na.rm = TRUE)
cor17_avg_urb = rowMeans(data.frame(rwi.F15Que$cor17, rwi.F21Dec$cor17, rwi.F23Ace$cor17, 
                                    rwi.F30Bet$cor17, rwi.M06Ace$cor17, rwi.M07Tsu$cor17, 
                                    rwi.M13Tsu$cor17, rwi.M17Thu$cor17), na.rm = TRUE)
cor17_avg_rur = rowMeans(data.frame(rwi.F07Ace$cor17, rwi.F25Ace$cor17, rwi.F33Pic$cor17, 
                                    rwi.M01Pop$cor17, rwi.M05Thu$cor17, rwi.M20Thu$cor17, 
                                    rwi.M26Thu$cor17, rwi.M27Pin$cor17), na.rm = TRUE)
cor25_avg_urb = rowMeans(data.frame(rwi.F15Que$cor25, rwi.F21Dec$cor25, rwi.F23Ace$cor25, 
                                    rwi.F30Bet$cor25, rwi.M06Ace$cor25, rwi.M07Tsu$cor25, 
                                    rwi.M13Tsu$cor25, rwi.M17Thu$cor25), na.rm = TRUE)
cor25_avg_rur = rowMeans(data.frame(rwi.F07Ace$cor25, rwi.F25Ace$cor25, rwi.F33Pic$cor25, 
                                    rwi.M01Pop$cor25, rwi.M05Thu$cor25, rwi.M20Thu$cor25, 
                                    rwi.M26Thu$cor25, rwi.M27Pin$cor25), na.rm = TRUE)

cor7_avg_urbrur.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor7), alpha = 0.25, color = 'darkgreen') +
  geom_line(aes(x = 1972:2018, y = cor7_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor7_avg_urb), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor7_avg_rur), lwd = 1, color = "darkgreen") +
  scale_y_continuous(name = "Correlation", expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 0.9, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor7_sig_urbrur.p = ggplot(cor7_sig) +
  geom_histogram(aes(x = Year, fill = Landscape), binwidth = 1) +
  scale_fill_manual(values = c("darkgreen", "red3")) + 
  scale_y_continuous(name = "Frequency", expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 4.75, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

cor11_avg_urbrur.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor11), alpha = 0.25, color = 'darkgreen') +
  geom_line(aes(x = 1972:2018, y = cor11_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor11_avg_urb), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor11_avg_rur), lwd = 1, color = "darkgreen") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor11_sig_urbrur.p = ggplot(cor11_sig) +
  geom_histogram(aes(x = Year, fill = Landscape), binwidth = 1) +
  scale_fill_manual(values = c("darkgreen", "red3")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")
  
cor17_avg_urbrur.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor17), alpha = 0.25, color = 'darkgreen') +
  geom_line(aes(x = 1972:2018, y = cor17_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor17_avg_urb), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor17_avg_rur), lwd = 1, color = "darkgreen") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor17_sig_urbrur.p = ggplot(cor17_sig) +
  geom_histogram(aes(x = Year, fill = Landscape), binwidth = 1) +
  scale_fill_manual(values = c("darkgreen", "red3")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

cor25_avg_urbrur.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor25), alpha = 0.25, color = 'darkgreen') +
  geom_line(aes(x = 1972:2018, y = cor25_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor25_avg_urb), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor25_avg_rur), lwd = 1, color = "darkgreen") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor25_sig_urbrur.p = ggplot(cor25_sig) +
  geom_histogram(aes(x = Year, fill = Landscape), binwidth = 1) +
  scale_fill_manual(values = c("darkgreen", "red3")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

tiff("tempcorflat_urbrur.tiff", units = "in", width = 6.5, height = 4, res = 300)
(cor7_avg_urbrur.p | cor11_avg_urbrur.p | cor17_avg_urbrur.p | cor25_avg_urbrur.p) /
  (cor7_sig_urbrur.p | cor11_sig_urbrur.p | cor17_sig_urbrur.p | cor25_sig_urbrur.p) 
dev.off() 

# Dec vs. Con
cor7_avg_dec = rowMeans(data.frame(rwi.F15Que$cor7, rwi.F15Ace$cor7, rwi.F21Dec$cor7, rwi.F23Ace$cor7, 
                                   rwi.F30Bet$cor7, rwi.F30Ace$cor7, rwi.M06Ace$cor7, rwi.M06Que$cor7,
                                   rwi.F07Ace$cor7, rwi.F25Ace$cor7, rwi.M01Pop$cor7, rwi.M26Dec$cor7),
                        na.rm = TRUE)
cor7_avg_con = rowMeans(data.frame(rwi.F33Pic$cor7, rwi.M05Thu$cor7, rwi.M20Thu$cor7,
                                   rwi.M07Tsu$cor7, rwi.M13Tsu$cor7, rwi.M17Thu$cor7,
                                   rwi.M26Thu$cor7, rwi.M27Pin$cor7), na.rm = TRUE)
cor11_avg_dec = rowMeans(data.frame(rwi.F15Que$cor11, rwi.F15Ace$cor11, rwi.F21Dec$cor11, rwi.F23Ace$cor11, 
                                   rwi.F30Bet$cor11, rwi.F30Ace$cor11, rwi.M06Ace$cor11, rwi.M06Que$cor11,
                                   rwi.F07Ace$cor11, rwi.F25Ace$cor11, rwi.M01Pop$cor11, rwi.M26Dec$cor11),
                        na.rm = TRUE)
cor11_avg_con = rowMeans(data.frame(rwi.F33Pic$cor11, rwi.M05Thu$cor11, rwi.M20Thu$cor11,
                                   rwi.M07Tsu$cor11, rwi.M13Tsu$cor11, rwi.M17Thu$cor11,
                                   rwi.M26Thu$cor11, rwi.M27Pin$cor11), na.rm = TRUE)
cor17_avg_dec = rowMeans(data.frame(rwi.F15Que$cor17, rwi.F15Ace$cor17, rwi.F21Dec$cor17, rwi.F23Ace$cor17, 
                                   rwi.F30Bet$cor17, rwi.F30Ace$cor17, rwi.M06Ace$cor17, rwi.M06Que$cor17,
                                   rwi.F07Ace$cor17, rwi.F25Ace$cor17, rwi.M01Pop$cor17, rwi.M26Dec$cor17),
                        na.rm = TRUE)
cor17_avg_con = rowMeans(data.frame(rwi.F33Pic$cor17, rwi.M05Thu$cor17, rwi.M20Thu$cor17,
                                    rwi.M07Tsu$cor17, rwi.M13Tsu$cor17, rwi.M17Thu$cor17,
                                    rwi.M26Thu$cor17, rwi.M27Pin$cor17), na.rm = TRUE)
cor25_avg_dec = rowMeans(data.frame(rwi.F15Que$cor25, rwi.F15Ace$cor25, rwi.F21Dec$cor25, rwi.F23Ace$cor25, 
                                   rwi.F30Bet$cor25, rwi.F30Ace$cor25, rwi.M06Ace$cor25, rwi.M06Que$cor25,
                                   rwi.F07Ace$cor25, rwi.F25Ace$cor25, rwi.M01Pop$cor25, rwi.M26Dec$cor25),
                        na.rm = TRUE)
cor25_avg_con = rowMeans(data.frame(rwi.F33Pic$cor25, rwi.M05Thu$cor25, rwi.M20Thu$cor25,
                                    rwi.M07Tsu$cor25, rwi.M13Tsu$cor25, rwi.M17Thu$cor25,
                                    rwi.M26Thu$cor25, rwi.M27Pin$cor25), na.rm = TRUE)

cor7_avg_deccon.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor7_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor7_avg_con), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor7_avg_dec), lwd = 1, color = "blue") +
  scale_y_continuous(name = "RWI-CC Correlation", expand = c(0,0), limits = c(-1,1)) +
  ggtitle("7 Year") +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 0.9, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
cor7_avg_deccon.p

cor7_sig_deccon.p = ggplot(cor7_sig) +
  geom_histogram(aes(x = Year, fill = Type), binwidth = 1) +
  scale_fill_manual(values = c("red3", "blue")) + 
  scale_y_continuous(name = "Frequency", expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 5.65, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")
cor7_sig_deccon.p

cor11_avg_deccon.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor11_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor11_avg_con), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor11_avg_dec), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  ggtitle("11 Year") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
cor11_avg_deccon.p

cor11_sig_deccon.p = ggplot(cor11_sig) +
  geom_histogram(aes(x = Year, fill = Type), binwidth = 1) +
  scale_fill_manual(values = c("red3", "blue")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")
cor11_sig_deccon.p

cor17_avg_deccon.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor17_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor17_avg_con), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor17_avg_dec), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  ggtitle("17 Year") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
cor17_avg_deccon.p

cor17_sig_deccon.p = ggplot(cor17_sig) +
  geom_histogram(aes(x = Year, fill = Type), binwidth = 1) +
  scale_fill_manual(values = c("red3", "blue")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")
cor17_sig_deccon.p

cor25_avg_deccon.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Que, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F15Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F21Dec, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Bet, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F30Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F33Pic, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M01Pop, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M06Que, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M07Tsu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M13Tsu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Dec, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M27Pin, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor25_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor25_avg_con), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor25_avg_dec), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  ggtitle("25 Year") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
cor25_avg_deccon.p

cor25_sig_deccon.p = ggplot(cor25_sig) +
  geom_histogram(aes(x = Year, fill = Type), binwidth = 1) +
  scale_fill_manual(values = c("red3", "blue")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,6)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")
cor25_sig_deccon.p

tiff("tempcorflat_deccon2_20_2.tiff", units = "in", width = 6.5, height = 5, res = 300)
#(cor7_avg_deccon.p | cor11_avg_deccon.p | cor17_avg_deccon.p | cor25_avg_deccon.p) /
#  (cor7_sig_deccon.p | cor11_sig_deccon.p | cor17_sig_deccon.p | cor25_sig_deccon.p) 
(cor7_avg_deccon.p | cor11_avg_deccon.p | cor17_avg_deccon.p) /
  (cor7_sig_deccon.p | cor11_sig_deccon.p | cor17_sig_deccon.p)
dev.off() 

# Ace vs. Thu
cor7_avg_ace = rowMeans(data.frame(rwi.F23Ace$cor7, rwi.M06Ace$cor7, rwi.F07Ace$cor7,
                                   rwi.F25Ace$cor7, rwi.F30Ace$cor7, rwi.F15Ace$cor7), na.rm = TRUE)
cor7_avg_thu = rowMeans(data.frame(rwi.M05Thu$cor7, rwi.M20Thu$cor7, rwi.M17Thu$cor7,
                                   rwi.M26Thu$cor7), na.rm = TRUE)
cor11_avg_ace = rowMeans(data.frame(rwi.F23Ace$cor11, rwi.M06Ace$cor11, rwi.F07Ace$cor11,
                                   rwi.F25Ace$cor11, rwi.F30Ace$cor11, rwi.F15Ace$cor11), na.rm = TRUE)
cor11_avg_thu = rowMeans(data.frame(rwi.M05Thu$cor11, rwi.M20Thu$cor11, rwi.M17Thu$cor11,
                                   rwi.M26Thu$cor11), na.rm = TRUE)
cor17_avg_ace = rowMeans(data.frame(rwi.F23Ace$cor17, rwi.M06Ace$cor17, rwi.F07Ace$cor17,
                                   rwi.F25Ace$cor17, rwi.F30Ace$cor17, rwi.F15Ace$cor17), na.rm = TRUE)
cor17_avg_thu = rowMeans(data.frame(rwi.M05Thu$cor17, rwi.M20Thu$cor17, rwi.M17Thu$cor17,
                                   rwi.M26Thu$cor17), na.rm = TRUE)
#cor25_avg_ace = rowMeans(data.frame(rwi.F23Ace$cor25, rwi.M06Ace$cor25, rwi.F07Ace$cor25,
#                                   rwi.F25Ace$cor25), na.rm = TRUE)
#cor25_avg_thu = rowMeans(data.frame(rwi.M05Thu$cor25, rwi.M20Thu$cor25, rwi.M17Thu$cor25,
#                                   rwi.M26Thu$cor25), na.rm = TRUE)

cor7_avg_acethu.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor7), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor7), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor7_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor7_avg_thu), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor7_avg_ace), lwd = 1, color = "blue") +
  scale_y_continuous(name = "Correlation", expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 0.9, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor7sig_acethu = subset(cor7_sig, Genus == "Ace" | Genus == "Thu")

cor7_sig_acethu.p = ggplot(cor7sig_acethu) +
  geom_histogram(aes(x = Year, fill = Genus), binwidth = 1) +
  scale_fill_manual(values = c("blue", "red3")) + 
  scale_y_continuous(name = "Frequency", expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  annotate(geom = "text", x = 1975, y = 4.75, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

cor11_avg_acethu.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor11), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor11), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor11_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor11_avg_thu), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor11_avg_ace), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor11sig_acethu = subset(cor11_sig, Genus == "Ace" | Genus == "Thu")

cor11_sig_acethu.p = ggplot(cor11sig_acethu) +
  geom_histogram(aes(x = Year, fill = Genus), binwidth = 1) +
  scale_fill_manual(values = c("blue", "red3")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

cor17_avg_acethu.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor17), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor17), alpha = 0.25, color = 'red3') +
  #geom_line(aes(x = 1972:2018, y = cor17_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor17_avg_thu), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor17_avg_ace), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor17sig_acethu = subset(cor17_sig, Genus == "Ace" | Genus == "Thu")

cor17_sig_acethu.p = ggplot(cor17sig_acethu) +
  geom_histogram(aes(x = Year, fill = Genus), binwidth = 1) +
  scale_fill_manual(values = c("red3")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

cor25_avg_acethu.p = ggplot() +
  geom_hline(yintercept = 0, col = "gray48") + 
  geom_line(data = rwi.F07Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F23Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.F25Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M05Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M06Ace, aes(x = year, y = cor25), alpha = 0.25, color = 'blue') +
  geom_line(data = rwi.M17Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M20Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(data = rwi.M26Thu, aes(x = year, y = cor25), alpha = 0.25, color = 'red3') +
  geom_line(aes(x = 1972:2018, y = cor25_avg), lwd = 1) +
  geom_line(aes(x = 1972:2018, y = cor25_avg_thu), lwd = 1, color = "red3") +
  geom_line(aes(x = 1972:2018, y = cor25_avg_ace), lwd = 1, color = "blue") +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-1,1)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.15, 0), "cm"))

cor25sig_acethu = subset(cor25_sig, Genus == "Ace" | Genus == "Thu")

cor25_sig_acethu.p = ggplot(cor25sig_acethu) +
  geom_histogram(aes(x = Year, fill = Genus), binwidth = 1) +
  scale_fill_manual(values = c("blue", "red3")) + 
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,5)) +
  scale_x_continuous(expand = c(0,0), limits = c(1972,2018)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.15, 0, 0, 0), "cm"),
        legend.position = "none")

tiff("tempcorflat_acethu1.tiff", units = "in", width = 6.5, height = 4, res = 300)
(cor7_avg_acethu.p | cor11_avg_acethu.p | cor17_avg_acethu.p | cor25_avg_acethu.p) /
  (cor7_sig_acethu.p | cor11_sig_acethu.p | cor17_sig_acethu.p | cor25_sig_acethu.p) 
dev.off() 
#####