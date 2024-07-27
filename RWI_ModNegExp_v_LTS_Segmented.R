#####RWI (ModNegExp) vs. LTS (%CC) #####

##### Set up and Data #####
library(dplR)
library(segmented)
library(dplyr)
library(trend)
library(modifiedmk)
library(zyp)
library(ggplot2)
library(patchwork)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology")
# Desktop

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
# RWI names
#write.csv(rwl.F07Dec.mne.crn, "F07Dec_ModNegExp.csv") #rwl.F07Dec.mne.crn
#write.csv(rwl.F07Ace.mne.crn, "F07Ace_ModNegExp.csv") #rwl.F07Ace.mne.crn
#write.csv(rwl.F15Dec.mne.crn, "F15Dec_ModNegExp.csv") #rwl.F15Dec.mne.crn
#write.csv(rwl.F15Ace.mne.crn, "F15Ace_ModNegExp.csv") #rwl.F15Ace.mne.crn
#write.csv(rwl.F15Car.mne.crn, "F15Car_ModNegExp.csv") #rwl.F15Car.mne.crn
#write.csv(rwl.F15Que.mne.crn, "F15Que_ModNegExp.csv") #rwl.F15Que.mne.crn
#write.csv(rwl.F21Dec.mne.crn, "F21Dec_ModNegExp.csv") #rwl.F21Dec.mne.crn
#write.csv(rwl.F21Ace.mne.crn, "F21Ace_ModNegExp.csv") #rwl.F21Ace.mne.crn
#write.csv(rwl.F23Ace.mne.crn, "F23Ace_ModNegExp.csv") #rwl.F23Ace.mne.crn
#write.csv(rwl.F25Ace.mne.crn, "F25Ace_ModNegExp.csv") #rwl.F25Ace.mne.crn
#write.csv(rwl.F30Ace.mne.crn, "F30Ace_ModNegExp.csv") #rwl.F30Ace.mne.crn
#write.csv(rwl.F30Bet.mne.crn, "F30Bet_ModNegExp.csv") #rwl.F30Bet.mne.crn
#write.csv(rwl.F33Pic.mne.crn, "F33Pic_ModNegExp.csv") #rwl.F33Pic.mne.crn
#write.csv(rwl.M01Pop.mne.crn, "M01Pop_ModNegExp.csv") #rwl.M01Pop.mne.crn
#write.csv(rwl.M05Thu.mne.crn, "M05Thu_ModNegExp.csv") #rwl.M05Thu.mne.crn
#write.csv(rwl.M06Ace.mne.crn, "M06Ace_ModNegExp.csv") #rwl.M06Ace.mne.crn
#write.csv(rwl.M06Que.mne.crn, "M06Que_ModNegExp.csv") #rwl.M06Que.mne.crn
#write.csv(rwl.M07Tsu.mne.crn, "M07Tsu_ModNegExp.csv") #rwl.M07Tsu.mne.crn
#write.csv(rwl.M13Tsu.mne.crn, "M13Tsu_ModNegExp.csv") #rwl.M13Tsu.mne.crn
#write.csv(rwl.M17Thu.mne.crn, "M17Thu_ModNegExp.csv") #rwl.M17Thu.mne.crn
#write.csv(rwl.M20Con.mne.crn, "M20Con_ModNegExp.csv") #rwl.M20Con.mne.crn
#write.csv(rwl.M20Thu.mne.crn, "M20Thu_ModNegExp.csv") #rwl.M20Thu.mne.crn
#write.csv(rwl.M26Dec.mne.crn, "M26Dec_ModNegExp.csv") #rwl.M26Dec.mne.crn
#write.csv(rwl.M26Ace.mne.crn, "M26Ace_ModNegExp.csv") #rwl.M26Ace.mne.crn
#write.csv(rwl.M26Thu.mne.crn, "M26Thu_ModNegExp.csv") #rwl.M26Thu.mne.crn
#write.csv(rwl.M27Pin.mne.crn, "M27Pin_ModNegExp.csv") #rwl.M27Pin.mne.crn

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
# LTS names
#lts.F07
#lts.F15
#lts.F21
#lts.F23
#lts.F25
#lts.F30
#lts.F33
#lts.M01
#lts.M05
#lts.M06
#lts.M07
#lts.M13
#lts.M17
#lts.M20
#lts.M26
#lts.M27

##### Updated lines.segmented function #####
my.seglines = function (x, term, bottom = TRUE, shift = TRUE, conf.level = 0.95, 
          k = 50, pch = 18, rev.sgn = FALSE, ...) 
{
  if (missing(term)) {
    if (length(x$nameUV$Z) > 1) {
      stop("please, specify `term'")
    }
    else {
      term <- x$nameUV$Z
    }
  }
  ss <- list(...)
  metodo <- if (!is.null(ss$method)) 
    ss$method
  else "delta"
  colore <- if (is.null(ss$col)) 
    1
  else ss$col
  usr <- par("usr")
  h <- (usr[4] - usr[3])/abs(k)
  y <- if (bottom) 
    usr[3] + h
  else usr[4] - h
  m <- confint.segmented(object = x, parm = term, level = conf.level, 
                         rev.sgn = rev.sgn, digits = 4, method = metodo) # digits = 15
  m <- matrix(m, ncol = 3)
  if (nrow(m) > 1) 
    m <- m[order(m[, 1]), ]
  est.psi <- m[, 1]
  lower.psi <- m[, 2]
  upper.psi <- m[, 3]
  if (length(est.psi) > 1) {
    y <- if (shift) 
      y + seq(-h/2, h/2, length = length(est.psi))
    else rep(y, length(est.psi))
  }
  segments(lower.psi, y, upper.psi, y, ...)
  points(est.psi, y, type = "p", pch = pch, col = colore)
} # rounds to nearest integer (year)

##### F07 Segmented Regression #####
#####
### RWI: F07Dec ###
# rwi.F07Dec = add_rownames(rwl.F07Dec.mne.crn, var = "year")
# rwi.F07Dec$year = as.numeric(rwi.F07Dec$year) # Years column in chr by default
# 
# # Build and Plot Simple Linear Model #
# lm.F07Dec = lm(xxxstd ~ year, data = rwi.F07Dec)
# summary(lm.F07Dec) # Linear model
# 
# plot(lm.F07Dec) # Diagnostic plots
# # Looks good
# 
# plot(rwi.F07Dec$year, rwi.F07Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# lines(rwi.F07Dec$year, fitted(lm.F07Dec), lwd = 3, col = "gray48")
# # Not good fit
# # Est. breakpoints: 1965 (maybe), 1995
# 
# # Select Breakpoints #
# selgmented(lm.F07Dec, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# # Select number (max = 10) and estimated year of breakpoints
# # Using BIC allows to compare models with more than 2 breakpoints
# # Selected 1 breakpoint at 1994
# 
# davies.test(lm.F07Dec, k = round(nrow(rwi.F07Dec) / 5)) # Default k = 10
# # Estimate first break point. Check 20% of years
# # Best at 1996, significant
# 
# pscore.test(lm.F07Dec, k = round(nrow(rwi.F07Dec) / 5)) 
# # Tests for the existence of one breakpoint
# # Significant for 1 breakpoint
# 
# # Build Segmented Model # 
# # Run preliminary automatic segmentation and plot results
# #slm.F07Dec = segmented.lm(lm.F07Dec, psi = NA,
# #                       control = seg.control(n.boot = 0, fix.npsi = FALSE,
# #                                             display = TRUE, it.max = 100))
# #summary(slm.F07Dec) # Expected to overestimate number of breakpoints
# 
# slm.F07Dec = segmented.lm(lm.F07Dec, psi = c(1994), # psi based on selgmented
#                           control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# summary(slm.F07Dec)
# 
# davies.test(slm.F07Dec, k = round(nrow(rwi.F07Dec) / 5))
# # Estimate possible additional breakpoint
# # Not significant (1964, p = 0.55)
# 
# pscore.test(slm.F07Dec, k = round(nrow(rwi.F07Dec) / 5), more.break = TRUE) 
# # Test for an additional breakpoint
# # Not significant
# 
# draw.history(slm.F07Dec) # Changes if n.boot = 0
# dev.off()
# 
# plot(slm.F07Dec) # Preliminary plot
# 
# confint(slm.F07Dec, digits = 4) # 1.96 x SE at 95% CI
# 
# plot(rwi.F07Dec$year, rwi.F07Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# plot(slm.F07Dec, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
# my.seglines(slm.F07Dec, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
# abline(v = round(confint(slm.F07Dec)[1]), lty = 2, col = "blue") # Round vertical break
# 
# # MK Significant and TS Slope for Segments #
# brks = slm.F07Dec$psi
# brk1 = round(brks[1,2])
# brk1
# 
# seg1 = subset(rwi.F07Dec, year <= brk1)
# seg1$year
# seg2 = subset(rwi.F07Dec, year >= brk1)
# seg2$year
# 
# # mk.test(seg1$xxxstd) # Adjust based on breakpoint years from above
# # Mann-Kendall test for significant slope, no correction for autocorrelation
# sens.slope(seg1$xxxstd) # Includes mk.test p-value
# mmkh(seg1$xxxstd)
# #Mann-Kendal modified for autocorrelated data, shows old results too + Sen's slope
# 
# sens.slope(seg2$xxxstd)
# mmkh(seg2$xxxstd)
# 
# # Plot Sens splope using zyp method
# seg1.lm = zyp.sen(xxxstd ~ year, seg1)
# seg2.lm = zyp.sen(xxxstd ~ year, seg2)
# # Gives Sen slope and median intercept
# 
# seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1]
# seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1]
# 
# # Update Plot
# lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
# lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig

#bbsmk(rwi.F07Dec$xxxstd[51:77]) # Nonparametric block bootstrapped Mann-Kendal trend test
#bbssr(rwi.F07Dec$xxxstd[51:77]) # Nonparametric block bootstrapped Spearmans Rank Correlation trend test
#mkttest(rwi.F07Dec$xxxstd[51:77]) # Same as mk.test, but less readible 
#mmky(rwi.F07Dec$xxxstd[1:25]) # Other modified MK, seems to lower p-value
#tfpwmk(rwi.F07Dec$xxxstd[51:77]) #MK pre-whitened, includes Sen's slope
#ww.test(rwi.F07Dec$xxxstd[1:25]) # Wald-Wolf test for independence and stationarity

### RWI: F07Ace ###
rwi.F07Ace = add_rownames(rwl.F07Ace.mne.crn, var = "year")
rwi.F07Ace$year = as.numeric(rwi.F07Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F07Ace = lm(xxxstd ~ year, data = rwi.F07Ace)
summary(lm.F07Ace) # Linear model

#lm.F07Ace = lm(as.matrix(rwi.F07Ace[2]) ~ as.matrix(rwi.F07Ace[1]))
#summary(lm.F07Ace)

plot(lm.F07Ace) # Diagnostic plots
# Looks good

plot(rwi.F07Ace$year, rwi.F07Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F07Ace$year, fitted(lm.F07Ace), lwd = 3, col = "gray48")
# Not good fit
# Est. breakpoints: 1965 (maybe), 1990

# Select Breakpoints #
selgmented(lm.F07Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint at 1993

davies.test(lm.F07Ace, k = round(nrow(rwi.F07Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1991, significant

pscore.test(lm.F07Ace, k = round(nrow(rwi.F07Ace) / 5)) 
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.F07Ace = segmented.lm(lm.F07Ace, psi = c(1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F07Ace)

davies.test(slm.F07Ace, k = round(nrow(rwi.F07Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (2007, p = 0.92)

pscore.test(slm.F07Ace, k = round(nrow(rwi.F07Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F07Ace) # Changes if n.boot = 0
dev.off()

plot(slm.F07Ace) # Preliminary plot

confint(slm.F07Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F07Ace$year, rwi.F07Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F07Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F07Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F07Ace)[1]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F07Ace$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(rwi.F07Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.F07Ace, year >= brk1)
seg2$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
mmkh(seg2$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: F07 ###
lts.F07_1 = add_rownames(lts.F07, var = "year")
lts.F07_1$year = as.numeric(lts.F07_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF07 = lm(Avg_CC_Median ~ year, data = lts.F07_1)
summary(lm.ltsF07) # Linear model

plot(lm.ltsF07) # Diagnostic plots
# Residuals vs. fitted/leverage not normal

plot(lts.F07_1$year, lts.F07_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F07_1$year, fitted(lm.ltsF07), lwd = 3, col = "darkgreen")
# Significant, residuals not normal
# Est. breakpoints: 1980 (maybe), 2005

# Select Breakpoints #
selgmented(lm.ltsF07, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints at 1981, 1984, 2005 (1 breakpoint also low)

davies.test(lm.ltsF07, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2002, significant

pscore.test(lm.ltsF07, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF07 = segmented.lm(lm.ltsF07, psi = c(1981,1984,2005), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsF07)
plot(slm.ltsF07) # Preliminary plot

davies.test(slm.ltsF07, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1993, p = 0.81)

pscore.test(slm.ltsF07, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF07) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF07, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F07_1$year, lts.F07_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF07, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF07, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF07)[1:3]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF07$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(lts.F07_1, year <= brk1)
seg1$year
seg2 = subset(lts.F07_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.F07_1, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(lts.F07_1, year >= brk3)
seg4$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

sens.slope(seg4$Avg_CC_Median)
mmkh(seg4$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### F15 Segmented Regression #####
#####
### RWI: F15Dec ###
# rwi.F15Dec = add_rownames(rwl.F15Dec.mne.crn, var = "year")
# rwi.F15Dec$year = as.numeric(rwi.F15Dec$year) # Years column in chr by default
# 
# # Build and Plot Simple Linear Model #
# lm.F15Dec = lm(xxxstd ~ year, data = rwi.F15Dec)
# summary(lm.F15Dec) # Linear model
# 
# plot(lm.F15Dec) # Diagnostic plots
# # Looks good
# 
# plot(rwi.F15Dec$year, rwi.F15Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# lines(rwi.F15Dec$year, fitted(lm.F15Dec), lwd = 3, col = "darkgreen")
# # Significant
# # Est. breakpoints: 1915, 1935, 1980, 2000, 2010
# 
# # Select Breakpoints #
# selgmented(lm.F15Dec, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# # Select number (max = 10) and estimated year of breakpoints
# # Using BIC allows to compare models with more than 2 breakpoints
# # Selected 4 breakpoints at 1934, 1980, 1995, 2013
# 
# davies.test(lm.F15Dec, k = round(nrow(rwi.F15Dec) / 5)) # Default k = 10
# # Estimate first break point. Check 20% of years
# # Best at 1929, significant
# 
# pscore.test(lm.F15Dec, k = round(nrow(rwi.F15Dec) / 5)) 
# # Tests for the existence of one breakpoint
# # Significant for 1 breakpoint
# 
# # Build Segmented Model # 
# slm.F15Dec = segmented.lm(lm.F15Dec, psi = c(1934,1980,1995,2013), # psi based on selgmented
#                           control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# summary(slm.F15Dec)
# plot(slm.F15Dec) # Preliminary plot
# 
# davies.test(slm.F15Dec, k = round(nrow(rwi.F15Dec) / 5))
# # Estimate possible additional breakpoint
# # Not significant (1913, p = 1)
# 
# pscore.test(slm.F15Dec, k = round(nrow(rwi.F15Dec) / 5), more.break = TRUE) 
# # Test for an additional breakpoint
# # Not significant
# 
# draw.history(slm.F15Dec) # Changes if n.boot = 0
# dev.off()
# 
# confint(slm.F15Dec, digits = 4) # 1.96 x SE at 95% CI
# 
# plot(rwi.F15Dec$year, rwi.F15Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# plot(slm.F15Dec, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
# my.seglines(slm.F15Dec, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
# abline(v = round(confint(slm.F15Dec)[1:4]), lty = 2, col = "blue") # Round vertical break
# 
# # MK Significant and TS Slope for Segments #
# brks = slm.F15Dec$psi
# brk1 = round(brks[1,2])
# brk1
# brk2 = round(brks[2,2])
# brk2
# brk3 = round(brks[3,2])
# brk3
# brk4 = round(brks[4,2])
# brk4
# 
# seg1 = subset(rwi.F15Dec, year <= brk1)
# seg1$year
# seg2 = subset(rwi.F15Dec, year >= brk1 & year <= brk2)
# seg2$year
# seg3 = subset(rwi.F15Dec, year >= brk2 & year <= brk3)
# seg3$year
# seg4 = subset(rwi.F15Dec, year >= brk3 & year <= brk4)
# seg4$year
# seg5 = subset(rwi.F15Dec, year >= brk4)
# seg5$year
# 
# sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
# mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)
# 
# sens.slope(seg2$xxxstd)
# mmkh(seg2$xxxstd)
# 
# sens.slope(seg3$xxxstd)
# mmkh(seg3$xxxstd)
# 
# sens.slope(seg4$xxxstd)
# mmkh(seg4$xxxstd)
# 
# sens.slope(seg5$xxxstd)
# mmkh(seg5$xxxstd)
# 
# seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
# seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
# seg3.lm = zyp.sen(xxxstd ~ year, seg3)
# seg4.lm = zyp.sen(xxxstd ~ year, seg4)
# seg5.lm = zyp.sen(xxxstd ~ year, seg5)
# 
# seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
# seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
# seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
# seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
# seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
# 
# # Update Plot
# lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
# lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
# lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
# lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
# lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig

### RWI: F15Ace ###
rwi.F15Ace = add_rownames(rwl.F15Ace.mne.crn, var = "year")
rwi.F15Ace$year = as.numeric(rwi.F15Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F15Ace = lm(xxxstd ~ year, data = rwi.F15Ace)
summary(lm.F15Ace) # Linear model

plot(lm.F15Ace) # Diagnostic plots
# Some strong outliers

plot(rwi.F15Ace$year, rwi.F15Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F15Ace$year, fitted(lm.F15Ace), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1930, 1980, 1990, 2010

# Select Breakpoints #
selgmented(lm.F15Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints at 1929, 1979, 1996, 2009

davies.test(lm.F15Ace, k = round(nrow(rwi.F15Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1929, significant

pscore.test(lm.F15Ace, k = round(nrow(rwi.F15Ace) / 5)) 
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.F15Ace = segmented.lm(lm.F15Ace, psi = c(1929,1979,1996,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F15Ace)
plot(slm.F15Ace) # Preliminary plot

davies.test(slm.F15Ace, k = round(nrow(rwi.F15Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (1908, p = 0.82)

pscore.test(slm.F15Ace, k = round(nrow(rwi.F15Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F15Ace) # Changes if n.boot = 0
dev.off()

confint(slm.F15Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F15Ace$year, rwi.F15Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F15Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F15Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F15Ace)[1:4]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F15Ace$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(rwi.F15Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.F15Ace, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F15Ace, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F15Ace, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.F15Ace, year >= brk4)
seg5$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig

### RWI: F15Car ###
#rwi.F15Car = add_rownames(rwl.F15Car.mne.crn, var = "year")
#rwi.F15Car$year = as.numeric(rwi.F15Car$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
#lm.F15Car = lm(xxxstd ~ year, data = rwi.F15Car)
#summary(lm.F15Car) # Linear model

#plot(lm.F15Car) # Diagnostic plots
# A couple outliers

#plot(rwi.F15Car$year, rwi.F15Car$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
#lines(rwi.F15Car$year, fitted(lm.F15Car), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1950, 1955, 1985, 1995, 2010

# Select Breakpoints #
#selgmented(lm.F15Car, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 7 breakpoints at 1948, 1951, 1953, 1957, 1984, 1995, 2013

#davies.test(lm.F15Car, k = round(nrow(rwi.F15Car) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1962, significant

#pscore.test(lm.F15Car, k = round(nrow(rwi.F15Car) / 5)) 
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
#slm.F15Car = segmented.lm(lm.F15Car, npsi = 7, # psi based on selgmented
#                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
#psi = c(1948,1984,1995,2013) other option
#summary(slm.F15Car)
#plot(slm.F15Car) # Preliminary plot

#davies.test(slm.F15Car, k = round(nrow(rwi.F15Car) / 5))
# Estimate possible additional breakpoint
# Not significant (2007, p = 0.17)

#pscore.test(slm.F15Car, k = round(nrow(rwi.F15Car) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

#draw.history(slm.F15Car) # Changes if n.boot = 0
#dev.off()

#confint(slm.F15Car, digits = 4) # 1.96 x SE at 95% CI

#plot(rwi.F15Car$year, rwi.F15Car$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
#plot(slm.F15Car, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
#my.seglines(slm.F15Car, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
#abline(v = round(confint(slm.F15Car)[1:7]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
#brks = slm.F15Car$psi
#brk1 = round(brks[1,2])
#brk1
#brk2 = round(brks[2,2])
#brk2
#brk3 = round(brks[3,2])
#brk3
#brk4 = round(brks[4,2])
#brk4
#brk5 = round(brks[5,2])
#brk5
#brk6 = round(brks[6,2])
#brk6
#brk7 = round(brks[7,2])
#brk7

#seg1 = subset(rwi.F15Car, year <= brk1)
#seg1$year
#seg2 = subset(rwi.F15Car, year >= brk1 & year <= brk2)
#seg2$year
#seg3 = subset(rwi.F15Car, year >= brk2 & year <= brk3)
#seg3$year
#seg4 = subset(rwi.F15Car, year >= brk3 & year <= brk4)
#seg4$year
#seg5 = subset(rwi.F15Car, year >= brk4 & year <= brk5)
#seg5$year
#seg6 = subset(rwi.F15Car, year >= brk5 & year <= brk6)
#seg6$year
#seg7 = subset(rwi.F15Car, year >= brk6 & year <= brk7)
#seg7$year
#seg8 = subset(rwi.F15Car, year >= brk7)
#seg8$year

#sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
#mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

#sens.slope(seg2$xxxstd)
#mmkh(seg2$xxxstd)

#sens.slope(seg3$xxxstd)
#mmkh(seg3$xxxstd)

#sens.slope(seg4$xxxstd)
#mmkh(seg4$xxxstd)

#sens.slope(seg5$xxxstd)
#mmkh(seg5$xxxstd)

#sens.slope(seg6$xxxstd)
#mmkh(seg6$xxxstd)

#sens.slope(seg7$xxxstd)
#mmkh(seg7$xxxstd)

#sens.slope(seg8$xxxstd)
#mmkh(seg8$xxxstd)

#seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
#seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
#seg3.lm = zyp.sen(xxxstd ~ year, seg3)
#seg4.lm = zyp.sen(xxxstd ~ year, seg4)
#seg5.lm = zyp.sen(xxxstd ~ year, seg5)
#seg6.lm = zyp.sen(xxxstd ~ year, seg6)
#seg7.lm = zyp.sen(xxxstd ~ year, seg7)
#seg8.lm = zyp.sen(xxxstd ~ year, seg8)

#seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
#seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
#seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
#seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
#seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
#seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq
#seg7$slope = seg7.lm$coefficients[2] * seg7$year + seg7.lm$coefficients[1] # Eq
#seg8$slope = seg8.lm$coefficients[2] * seg8$year + seg8.lm$coefficients[1] # Eq

# Update Plot
#lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg5$year, seg5$slope, lwd = 3, col = "darkgreen") # Color based on sig
#lines(seg6$year, seg6$slope, lwd = 3, col = "red3") # Color based on sig
#lines(seg7$year, seg7$slope, lwd = 3, col = "darkgreen") # Color based on sig
#lines(seg8$year, seg8$slope, lwd = 3, col = "red3") # Color based on sig

### RWI: F15Que ###
rwi.F15Que = add_rownames(rwl.F15Que.mne.crn, var = "year")
rwi.F15Que$year = as.numeric(rwi.F15Que$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F15Que = lm(xxxstd ~ year, data = rwi.F15Que)
summary(lm.F15Que) # Linear model

plot(lm.F15Que) # Diagnostic plots
# Looks good

plot(rwi.F15Que$year, rwi.F15Que$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F15Que$year, fitted(lm.F15Que), lwd = 3, col = "gray48")
# Est. breakpoints: 1920, 1935, 1950, 1985, 2005, 2010

# Select Breakpoints #
selgmented(lm.F15Que, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints...

davies.test(lm.F15Que, k = round(nrow(rwi.F15Que) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1956, fringe significance (0.06)

pscore.test(lm.F15Que, k = round(nrow(rwi.F15Que) / 5)) 
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint (0.02)

# Build Segmented Model # 
slm.F15Que = segmented.lm(lm.F15Que, psi = c(1920,1935,1950,1985,2005,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F15Que)
plot(slm.F15Que) # Preliminary plot

davies.test(slm.F15Que, k = round(nrow(rwi.F15Que) / 5))
# Estimate possible additional breakpoint
# Not significant (1936, p = 0.11)

pscore.test(slm.F15Que, k = round(nrow(rwi.F15Que) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F15Que) # Changes if n.boot = 0
dev.off()

confint(slm.F15Que, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F15Que$year, rwi.F15Que$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F15Que, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F15Que, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F15Que)[1:6]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F15Que$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4
brk5 = round(brks[5,2])
brk5
brk6 = round(brks[6,2])
brk6

seg1 = subset(rwi.F15Que, year <= brk1)
seg1$year
seg2 = subset(rwi.F15Que, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F15Que, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F15Que, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.F15Que, year >= brk4 & year <= brk5)
seg5$year
seg6 = subset(rwi.F15Que, year >= brk5 & year <= brk6)
seg6$year
seg7 = subset(rwi.F15Que, year >= brk6)
seg7$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
mmkh(seg5$xxxstd)

sens.slope(seg6$xxxstd)
mmkh(seg6$xxxstd)

sens.slope(seg7$xxxstd)
mmkh(seg7$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)
seg6.lm = zyp.sen(xxxstd ~ year, seg6)
seg7.lm = zyp.sen(xxxstd ~ year, seg7)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq
seg7$slope = seg7.lm$coefficients[2] * seg7$year + seg7.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg6$year, seg6$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg7$year, seg7$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: F15 ###
lts.F15_1 = add_rownames(lts.F15, var = "year")
lts.F15_1$year = as.numeric(lts.F15_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF15 = lm(Avg_CC_Median ~ year, data = lts.F15_1)
summary(lm.ltsF15) # Linear model

plot(lm.ltsF15) # Diagnostic plots
# Looks good

plot(lts.F15_1$year, lts.F15_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F15_1$year, fitted(lm.ltsF15), lwd = 3, col = "red3")
# Significant, looks good
# Est. breakpoints: None probably (maybe 2000, 2010)

# Select Breakpoints #
selgmented(lm.ltsF15, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints

davies.test(lm.ltsF15, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1973, no significant

pscore.test(lm.ltsF15, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# No significant

# Build Segmented Model # 
# Not needed: linear model determined best
#####

##### F21 Segmented Regression #####
#####
### RWI: F21Dec ###
rwi.F21Dec = add_rownames(rwl.F21Dec.mne.crn, var = "year")
rwi.F21Dec$year = as.numeric(rwi.F21Dec$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F21Dec = lm(xxxstd ~ year, data = rwi.F21Dec)
summary(lm.F21Dec) # Linear model

plot(lm.F21Dec) # Diagnostic plots
# Residuals vs. fitted not normal

plot(rwi.F21Dec$year, rwi.F21Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F21Dec$year, fitted(lm.F21Dec), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1930, 1945, 1965, 1990

# Select Breakpoints #
selgmented(lm.F21Dec, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 2 breakpoints: 1966, 1985

davies.test(lm.F21Dec, k = round(nrow(rwi.F21Dec) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2002, significant

pscore.test(lm.F21Dec, k = round(nrow(rwi.F21Dec) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model # 
slm.F21Dec = segmented.lm(lm.F21Dec, psi = c(1966,1985), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
#psi = c(1930,1945,1965,1990) # Close to best BIC, maybe more reasonable visually
summary(slm.F21Dec)
plot(slm.F21Dec) # Preliminary plot

davies.test(slm.F21Dec, k = round(nrow(rwi.F21Dec) / 5))
# Estimate possible additional breakpoint
# Fringe significant (2002, p = 0.07)

pscore.test(slm.F21Dec, k = round(nrow(rwi.F21Dec) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F21Dec) # Changes if n.boot = 0
dev.off()

confint(slm.F21Dec, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F21Dec$year, rwi.F21Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F21Dec, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F21Dec, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F21Dec)[1:2]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F21Dec$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
#brk3 = round(brks[3,2])
#brk3
#brk4 = round(brks[4,2])
#brk4

seg1 = subset(rwi.F21Dec, year <= brk1)
seg1$year
seg2 = subset(rwi.F21Dec, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F21Dec, year >= brk2)
seg3$year
#seg3 = subset(rwi.F21Dec, year >= brk2 & year <= brk3)
#seg3$year
#seg4 = subset(rwi.F21Dec, year >= brk3 & year <= brk4)
#seg4$year
#seg5 = subset(rwi.F21Dec, year >= brk4)
#seg5$year

sens.slope(seg1$xxxstd)# Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Visual check on autocorrelation in segments
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd)
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd)
mmkh(seg3$xxxstd)

#sens.slope(seg4$xxxstd)
#acf(seg4$xxxstd)
#mmkh(seg4$xxxstd)

#sens.slope(seg5$xxxstd)
#acf(seg5$xxxstd)
#mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
#seg4.lm = zyp.sen(xxxstd ~ year, seg4)
#seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
#seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
#seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
#lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig

### RWI: F21Ace ###
# rwi.F21Ace = add_rownames(rwl.F21Ace.mne.crn, var = "year")
# rwi.F21Ace$year = as.numeric(rwi.F21Ace$year) # Years column in chr by default
# 
# # Build and Plot Simple Linear Model #
# lm.F21Ace = lm(xxxstd ~ year, data = rwi.F21Ace)
# summary(lm.F21Ace) # Linear model
# 
# plot(lm.F21Ace) # Diagnostic plots
# # Residuals vs. fitted not normal
# 
# plot(rwi.F21Ace$year, rwi.F21Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# lines(rwi.F21Ace$year, fitted(lm.F21Ace), lwd = 3, col = "darkgreen")
# # Significant
# # Est. breakpoints: 1930, 1965, 1985
# 
# # Select Breakpoints #
# selgmented(lm.F21Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# # Select number (max = 10) and estimated year of breakpoints
# # Using BIC allows to compare models with more than 2 breakpoints
# # Selected 2 breakpoints: 1966, 1982 (3 and 4 also low)
# 
# davies.test(lm.F21Ace, k = round(nrow(rwi.F21Ace) / 5)) # Default k = 10
# # Estimate first break point. Check 20% of years
# # Best at 2001 significant
# 
# pscore.test(lm.F21Ace, k = round(nrow(rwi.F21Ace) / 5)) 
# # Tests for the existence of one breakpoint
# # Not significant
# 
# # Build Segmented Model # 
# slm.F21Ace = segmented.lm(lm.F21Ace, psi = c(1930,1965,1985), # psi based on selgmented
#                           control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# # Best BIC: psi = c(1966,1982)
# summary(slm.F21Ace)
# plot(slm.F21Ace) # Preliminary plot
# 
# davies.test(slm.F21Ace, k = round(nrow(rwi.F21Ace) / 5))
# # Estimate possible additional breakpoint
# # Not significant (2001, p = 0.15)
# 
# pscore.test(slm.F21Ace, k = round(nrow(rwi.F21Ace) / 5), more.break = TRUE) 
# # Test for an additional breakpoint
# # Not significant
# 
# draw.history(slm.F21Ace) # Changes if n.boot = 0
# dev.off()
# 
# confint(slm.F21Ace, digits = 4) # 1.96 x SE at 95% CI
# 
# plot(rwi.F21Ace$year, rwi.F21Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# plot(slm.F21Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
# my.seglines(slm.F21Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
# abline(v = round(confint(slm.F21Ace)[1:3]), lty = 2, col = "blue") # Round vertical break
# 
# # MK Significant and TS Slope for Segments #
# brks = slm.F21Ace$psi
# brk1 = round(brks[1,2])
# brk1
# brk2 = round(brks[2,2])
# brk2
# brk3 = round(brks[3,2])
# brk3
# #brk4 = round(brks[4,2])
# #brk4
# 
# seg1 = subset(rwi.F21Ace, year <= brk1)
# seg1$year
# seg2 = subset(rwi.F21Ace, year >= brk1 & year <= brk2)
# seg2$year
# #seg3 = subset(rwi.F21Ace, year >= brk2)
# #seg3$year
# seg3 = subset(rwi.F21Ace, year >= brk2 & year <= brk3)
# seg3$year
# seg4 = subset(rwi.F21Ace, year >= brk3)
# seg4$year
# #seg4 = subset(rwi.F21Ace, year >= brk3 & year <= brk4)
# #seg4$year
# #seg5 = subset(rwi.F21Ace, year >= brk4)
# #seg5$year
# 
# sens.slope(seg1$xxxstd)# Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
# acf(seg1$xxxstd) # Visual check on autocorrelation in segments
# mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)
# 
# sens.slope(seg2$xxxstd)
# acf(seg2$xxxstd)
# mmkh(seg2$xxxstd)
# 
# sens.slope(seg3$xxxstd)
# acf(seg3$xxxstd)
# mmkh(seg3$xxxstd)
# 
# sens.slope(seg4$xxxstd)
# acf(seg4$xxxstd)
# mmkh(seg4$xxxstd)
# 
# #sens.slope(seg5$xxxstd)
# #acf(seg5$xxxstd)
# #mmkh(seg5$xxxstd)
# 
# seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
# seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
# seg3.lm = zyp.sen(xxxstd ~ year, seg3)
# seg4.lm = zyp.sen(xxxstd ~ year, seg4)
# #seg5.lm = zyp.sen(xxxstd ~ year, seg5)
# 
# seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
# seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
# seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
# seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
# #seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
# 
# # Update Plot
# lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
# lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
# lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
# lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig
# #lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: F21 ###
lts.F21_1 = add_rownames(lts.F21, var = "year")
lts.F21_1$year = as.numeric(lts.F21_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF21 = lm(Avg_CC_Median ~ year, data = lts.F21_1)
summary(lm.ltsF21) # Linear model

plot(lm.ltsF21) # Diagnostic plots
# Residuals vs. fitted/leverage not normal

plot(lts.F21_1$year, lts.F21_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F21_1$year, fitted(lm.ltsF21), lwd = 3, col = "gray48")
# Significant, residuals not normal
# Est. breakpoints: 1995, 2010

# Select Breakpoints #
selgmented(lm.ltsF21, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoints at 2006 (3 also low)

davies.test(lm.ltsF21, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.ltsF21, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF21 = segmented.lm(lm.ltsF21, psi = c(2006), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsF21)
plot(slm.ltsF21) # Preliminary plot

davies.test(slm.ltsF21, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1978, p = 0.69)

pscore.test(slm.ltsF21, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF21) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF21, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F21_1$year, lts.F21_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF21, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF21, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF21)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF21$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(lts.F21_1, year <= brk1)
seg1$year
seg2 = subset(lts.F21_1, year >= brk1)
seg2$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### F23 Segmented Regression #####
#####
### RWI: F23Ace ###
rwi.F23Ace = add_rownames(rwl.F23Ace.mne.crn, var = "year")
rwi.F23Ace$year = as.numeric(rwi.F23Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F23Ace = lm(xxxstd ~ year, data = rwi.F23Ace)
summary(lm.F23Ace) # Linear model

plot(lm.F23Ace) # Diagnostic plots
# A couple outliers

plot(rwi.F23Ace$year, rwi.F23Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F23Ace$year, fitted(lm.F23Ace), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1905, 1920, 1940, 1975, 2000, 2015

# Select Breakpoints #
selgmented(lm.F23Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints at 1904, 1918, 1972, 1996 (5 also low)

davies.test(lm.F23Ace, k = round(nrow(rwi.F23Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.F23Ace, k = round(nrow(rwi.F23Ace) / 5)) 
# Tests for the existence of one breakpoint
# No significant

# Build Segmented Model # 
slm.F23Ace = segmented.lm(lm.F23Ace, npsi = 5, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# Lowest BIC: psi = c(1904,1918,1972,1996)
summary(slm.F23Ace)
plot(slm.F23Ace) # Preliminary plot

davies.test(slm.F23Ace, k = round(nrow(rwi.F23Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (1942, p = 0.34)

pscore.test(slm.F23Ace, k = round(nrow(rwi.F23Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Fringe significance

draw.history(slm.F23Ace) # Changes if n.boot = 0
dev.off()

confint(slm.F23Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F23Ace$year, rwi.F23Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F23Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F23Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F23Ace)[1:5]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F23Ace$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4
brk5 = round(brks[5,2])
brk5

seg1 = subset(rwi.F23Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.F23Ace, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F23Ace, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F23Ace, year >= brk3 & year <= brk4)
seg4$year
#seg5 = subset(rwi.F23Ace, year >= brk4)
#seg5$year
seg5 = subset(rwi.F23Ace, year >= brk4 & year <= brk5)
seg5$year
seg6 = subset(rwi.F23Ace, year >= brk5)
seg6$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

sens.slope(seg6$xxxstd)
acf(seg6$xxxstd) 
mmkh(seg6$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)
seg6.lm = zyp.sen(xxxstd ~ year, seg6)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg6$year, seg6$slope, lwd = 3, col = "darkgreen") # Color based on sig

### LTS: F23 ###
lts.F23_1 = add_rownames(lts.F23, var = "year")
lts.F23_1$year = as.numeric(lts.F23_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF23 = lm(Avg_CC_Median ~ year, data = lts.F23_1)
summary(lm.ltsF23) # Linear model

plot(lm.ltsF23) # Diagnostic plots
# Residuals vs. fitted/leverage not normal

plot(lts.F23_1$year, lts.F23_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F23_1$year, fitted(lm.ltsF23), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 2010

# Select Breakpoints #
selgmented(lm.ltsF23, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoints at 2011 (2 also low)

davies.test(lm.ltsF23, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2012, significant

pscore.test(lm.ltsF23, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF23 = segmented.lm(lm.ltsF23, psi = c(2011), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsF23)
plot(slm.ltsF23) # Preliminary plot

davies.test(slm.ltsF23, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1978, p = 0.51)

pscore.test(slm.ltsF23, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF23) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF23, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F23_1$year, lts.F23_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF23, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF23, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF23)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF23$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(lts.F23_1, year <= brk1)
seg1$year
seg2 = subset(lts.F23_1, year >= brk1)
seg2$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### F25 Segmented Regression #####
#####
### RWI: F25Ace ###
rwi.F25Ace = add_rownames(rwl.F25Ace.mne.crn, var = "year")
rwi.F25Ace$year = as.numeric(rwi.F25Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F25Ace = lm(xxxstd ~ year, data = rwi.F25Ace)
summary(lm.F25Ace) # Linear model

plot(lm.F25Ace) # Diagnostic plots
# Not really normal

plot(rwi.F25Ace$year, rwi.F25Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F25Ace$year, fitted(lm.F25Ace), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1950, 1990, 2005

# Select Breakpoints #
selgmented(lm.F25Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 1951 (3 also low)

davies.test(lm.F25Ace, k = round(nrow(rwi.F25Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1952, significant

pscore.test(lm.F25Ace, k = round(nrow(rwi.F25Ace) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.F25Ace = segmented.lm(lm.F25Ace, psi = c(1951), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F25Ace)
plot(slm.F25Ace) # Preliminary plot

davies.test(slm.F25Ace, k = round(nrow(rwi.F25Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (2002, p = 0.84)

pscore.test(slm.F25Ace, k = round(nrow(rwi.F25Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F25Ace) # Changes if n.boot = 0
dev.off()

confint(slm.F25Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F25Ace$year, rwi.F25Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F25Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F25Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F25Ace)[1:1]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F25Ace$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(rwi.F25Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.F25Ace, year >= brk1)
seg2$year
                
sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: F25 ###
lts.F25_1 = add_rownames(lts.F25, var = "year")
lts.F25_1$year = as.numeric(lts.F25_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF25 = lm(Avg_CC_Median ~ year, data = lts.F25_1)
summary(lm.ltsF25) # Linear model

plot(lm.ltsF25) # Diagnostic plots
# Not normal

plot(lts.F25_1$year, lts.F25_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F25_1$year, fitted(lm.ltsF25), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1980, 1985

# Select Breakpoints #
selgmented(lm.ltsF25, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints at at 1978, 1986, 1991, 1995

davies.test(lm.ltsF25, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1997, significant

pscore.test(lm.ltsF25, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF25 = segmented.lm(lm.ltsF25, psi = c(1978,1986,1991,1995), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsF25)
plot(slm.ltsF25) # Preliminary plot

davies.test(slm.ltsF25, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.13)

pscore.test(slm.ltsF25, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF25) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF25, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F25_1$year, lts.F25_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF25, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF25, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF25)[1:4]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF25$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(lts.F25_1, year <= brk1)
seg1$year
seg2 = subset(lts.F25_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.F25_1, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(lts.F25_1, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(lts.F25_1, year >= brk4)
seg5$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

sens.slope(seg4$Avg_CC_Median)
acf(seg4$Avg_CC_Median)
mmkh(seg4$Avg_CC_Median)

sens.slope(seg5$Avg_CC_Median)
acf(seg5$Avg_CC_Median)
mmkh(seg5$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)
seg5.lm = zyp.sen(Avg_CC_Median ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### F30 Segmented Regression #####
#####
### RWI: F30Ace ###
rwi.F30Ace = add_rownames(rwl.F30Ace.mne.crn, var = "year")
rwi.F30Ace$year = as.numeric(rwi.F30Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F30Ace = lm(xxxstd ~ year, data = rwi.F30Ace)
summary(lm.F30Ace) # Linear model

plot(lm.F30Ace) # Diagnostic plots
# Not really normal

plot(rwi.F30Ace$year, rwi.F30Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F30Ace$year, fitted(lm.F30Ace), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1950 (maybe), 1965, 1975

# Select Breakpoints #
selgmented(lm.F30Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1952, 1967, 1972 (2 also low)

davies.test(lm.F30Ace, k = round(nrow(rwi.F30Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1980, significant

pscore.test(lm.F30Ace, k = round(nrow(rwi.F30Ace) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model #
slm.F30Ace = segmented.lm(lm.F30Ace, psi = c(1952,1967,1972), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F30Ace)
plot(slm.F30Ace) # Preliminary plot

davies.test(slm.F30Ace, k = round(nrow(rwi.F30Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (1949, p = 0.47)

pscore.test(slm.F30Ace, k = round(nrow(rwi.F30Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F30Ace) # Changes if n.boot = 0
dev.off()

confint(slm.F30Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F30Ace$year, rwi.F30Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F30Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F30Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F30Ace)[1:3]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F30Ace$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(rwi.F30Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.F30Ace, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F30Ace, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F30Ace, year >= brk3)
seg4$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig

### RWI: F30Bet ###
rwi.F30Bet = add_rownames(rwl.F30Bet.mne.crn, var = "year")
rwi.F30Bet$year = as.numeric(rwi.F30Bet$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F30Bet = lm(xxxstd ~ year, data = rwi.F30Bet)
summary(lm.F30Bet) # Linear model

plot(lm.F30Bet) # Diagnostic plots
# Mostly normal

plot(rwi.F30Bet$year, rwi.F30Bet$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F30Bet$year, fitted(lm.F30Bet), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1965 (maybe), 1990

# Select Breakpoints #
selgmented(lm.F30Bet, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints (second lowest = 1 and then 3)

davies.test(lm.F30Bet, k = round(nrow(rwi.F30Bet) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1985, not significant

pscore.test(lm.F30Bet, k = round(nrow(rwi.F30Bet) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model #
slm.F30Bet = segmented.lm(lm.F30Bet, npsi = 3, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.F30Bet)
plot(slm.F30Bet) # Preliminary plot

davies.test(slm.F30Bet, k = round(nrow(rwi.F30Bet) / 5))
# Estimate possible additional breakpoint
# Not significant (1964, p = 0.67)

pscore.test(slm.F30Bet, k = round(nrow(rwi.F30Bet) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F30Bet) # Changes if n.boot = 0
dev.off()

confint(slm.F30Bet, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F30Bet$year, rwi.F30Bet$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F30Bet, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F30Bet, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F30Bet)[1:3]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F30Bet$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(rwi.F30Bet, year <= brk1)
seg1$year
seg2 = subset(rwi.F30Bet, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F30Bet, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F30Bet, year >= brk3)
seg4$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: F30 ###
lts.F30_1 = add_rownames(lts.F30, var = "year")
lts.F30_1$year = as.numeric(lts.F30_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF30 = lm(Avg_CC_Median ~ year, data = lts.F30_1)
summary(lm.ltsF30) # Linear model

plot(lm.ltsF30) # Diagnostic plots
# Somewhat not normal

plot(lts.F30_1$year, lts.F30_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F30_1$year, fitted(lm.ltsF30), lwd = 3, col = "gray48")
# Significant
# Est. breakpoints: 1980 (maybe), 1990 (maybe), 2010

# Select Breakpoints #
selgmented(lm.ltsF30, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 2009 (2 and 3 also low)

davies.test(lm.ltsF30, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.ltsF30, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF30 = segmented.lm(lm.ltsF30, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# npsi = 3
summary(slm.ltsF30)
plot(slm.ltsF30) # Preliminary plot

davies.test(slm.ltsF30, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1978, p = 0.41)

pscore.test(slm.ltsF30, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF30) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF30, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F30_1$year, lts.F30_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF30, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF30, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF30)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF30$psi
brk1 = round(brks[1,2])
brk1
#brk2 = round(brks[2,2])
#brk2
#brk3 = round(brks[3,2])
#brk3

seg1 = subset(lts.F30_1, year <= brk1)
seg1$year
seg2 = subset(lts.F30_1, year >= brk1)
seg2$year
#seg2 = subset(lts.F30_1, year >= brk1 & year <= brk2)
#seg2$year
#seg3 = subset(lts.F30_1, year >= brk2 & year <= brk3)
#seg3$year
#seg4 = subset(lts.F30_1, year >= brk3)
#seg4$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

#sens.slope(seg3$Avg_CC_Median)
#acf(seg3$Avg_CC_Median)
#mmkh(seg3$Avg_CC_Median)

#sens.slope(seg4$Avg_CC_Median)
#acf(seg4$Avg_CC_Median)
#mmkh(seg4$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
#seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
#seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
#seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
#seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
#lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
#####

##### F33 Segmented Regression #####
#####
### RWI: F33Pic ###
rwi.F33Pic = add_rownames(rwl.F33Pic.mne.crn, var = "year")
rwi.F33Pic$year = as.numeric(rwi.F33Pic$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.F33Pic = lm(xxxstd ~ year, data = rwi.F33Pic)
summary(lm.F33Pic) # Linear model

plot(lm.F33Pic) # Diagnostic plots
# Mostly normal, one outlier

plot(rwi.F33Pic$year, rwi.F33Pic$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.F33Pic$year, fitted(lm.F33Pic), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1970, 1980, 2005, 2010

# Select Breakpoints #
selgmented(lm.F33Pic, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints: 1968, 1974, 2006, 2010 (3 also low)

davies.test(lm.F33Pic, k = round(nrow(rwi.F33Pic) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2001, not significant

pscore.test(lm.F33Pic, k = round(nrow(rwi.F33Pic) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model #
slm.F33Pic = segmented.lm(lm.F33Pic, psi = c(1968,1974,2006,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 0
summary(slm.F33Pic)
plot(slm.F33Pic) # Preliminary plot

davies.test(slm.F33Pic, k = round(nrow(rwi.F33Pic) / 5))
# Estimate possible additional breakpoint
# Not significant (1985, p = 0.69)

pscore.test(slm.F33Pic, k = round(nrow(rwi.F33Pic) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.F33Pic) # Changes if n.boot = 0
dev.off()

confint(slm.F33Pic, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.F33Pic$year, rwi.F33Pic$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.F33Pic, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.F33Pic, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.F33Pic)[1:4]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.F33Pic$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(rwi.F33Pic, year <= brk1)
seg1$year
seg2 = subset(rwi.F33Pic, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.F33Pic, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.F33Pic, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.F33Pic, year >= brk4)
seg5$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: F33 ###
lts.F33_1 = add_rownames(lts.F33, var = "year")
lts.F33_1$year = as.numeric(lts.F33_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsF33 = lm(Avg_CC_Median ~ year, data = lts.F33_1)
summary(lm.ltsF33) # Linear model

plot(lm.ltsF33) # Diagnostic plots
# Mostly normal

plot(lts.F33_1$year, lts.F33_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.F33_1$year, fitted(lm.ltsF33), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1980, 1985, 2000, 2010

# Select Breakpoints #
selgmented(lm.ltsF33, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints: 1979, 1987, 2000, 2004

davies.test(lm.ltsF33, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1988, not significant

pscore.test(lm.ltsF33, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Fringe significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsF33 = segmented.lm(lm.ltsF33, psi = c(1979,1987,2000,2004), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsF33)
plot(slm.ltsF33) # Preliminary plot

davies.test(slm.ltsF33, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1973, p = 0.93)

pscore.test(slm.ltsF33, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsF33) # Changes if n.boot = 0
dev.off()

confint(slm.ltsF33, digits = 4) # 1.96 x SE at 95% CI

plot(lts.F33_1$year, lts.F33_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsF33, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsF33, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsF33)[1:4]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsF33$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(lts.F33_1, year <= brk1)
seg1$year
seg2 = subset(lts.F33_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.F33_1, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(lts.F33_1, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(lts.F33_1, year >= brk4)
seg5$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

sens.slope(seg4$Avg_CC_Median)
acf(seg4$Avg_CC_Median)
mmkh(seg4$Avg_CC_Median)

sens.slope(seg5$Avg_CC_Median)
acf(seg5$Avg_CC_Median)
mmkh(seg5$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)
seg5.lm = zyp.sen(Avg_CC_Median ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M01 Segmented Regression #####
#####
### RWI: M01Pop ###
rwi.M01Pop = add_rownames(rwl.M01Pop.mne.crn, var = "year")
rwi.M01Pop$year = as.numeric(rwi.M01Pop$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M01Pop = lm(xxxstd ~ year, data = rwi.M01Pop)
summary(lm.M01Pop) # Linear model

plot(lm.M01Pop) # Diagnostic plots
# Good

plot(rwi.M01Pop$year, rwi.M01Pop$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M01Pop$year, fitted(lm.M01Pop), lwd = 3, col = "darkgreen")
# Not significant
# Est. breakpoints: 1960, 1970, 1985, 2010 (maybe)

# Select Breakpoints #
selgmented(lm.M01Pop, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints: 1960, 1965, 1980, 2012

davies.test(lm.M01Pop, k = round(nrow(rwi.M01Pop) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1957, fringe significance

pscore.test(lm.M01Pop, k = round(nrow(rwi.M01Pop) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model #
slm.M01Pop = segmented.lm(lm.M01Pop, psi = c(1960,1965,1980,2012), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 50
summary(slm.M01Pop)
plot(slm.M01Pop) # Preliminary plot

davies.test(slm.M01Pop, k = round(nrow(rwi.M01Pop) / 5))
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.23)

pscore.test(slm.M01Pop, k = round(nrow(rwi.M01Pop) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M01Pop) # Changes if n.boot = 0
dev.off()

confint(slm.M01Pop, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M01Pop$year, rwi.M01Pop$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M01Pop, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M01Pop, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M01Pop)[1:4]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M01Pop$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(rwi.M01Pop, year <= brk1)
seg1$year
seg2 = subset(rwi.M01Pop, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M01Pop, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M01Pop, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M01Pop, year >= brk4)
seg5$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M01 ###
lts.M01_1 = add_rownames(lts.M01, var = "year")
lts.M01_1$year = as.numeric(lts.M01_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM01 = lm(Avg_CC_Median ~ year, data = lts.M01_1)
summary(lm.ltsM01) # Linear model

plot(lm.ltsM01) # Diagnostic plots
# Somewhat non-normal

plot(lts.M01_1$year, lts.M01_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M01_1$year, fitted(lm.ltsM01), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1985, 1995 (maybe), 2005, 2015 (maybe)

# Select Breakpoints #
selgmented(lm.ltsM01, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 2 breakpoints: 1985, 1997 (3/1 also low)

davies.test(lm.ltsM01, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.ltsM01, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsM01 = segmented.lm(lm.ltsM01, npsi = 3, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
#psi = c(1985,1997) # Lowest BIC
summary(slm.ltsM01)
plot(slm.ltsM01) # Preliminary plot

davies.test(slm.ltsM01, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (2007, p = 0.28)

pscore.test(slm.ltsM01, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM01) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM01, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M01_1$year, lts.M01_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM01, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM01, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM01)[1:3]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM01$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(lts.M01_1, year <= brk1)
seg1$year
seg2 = subset(lts.M01_1, year >= brk1 & year <= brk2)
seg2$year
#seg3 = subset(lts.M01_1, year >= brk2)
#seg3$year
seg3 = subset(lts.M01_1, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(lts.M01_1, year >= brk3)
seg4$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

sens.slope(seg4$Avg_CC_Median)
acf(seg4$Avg_CC_Median)
mmkh(seg4$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M05 Segmented Regression #####
#####
### RWI: M05Thu ###
rwi.M05Thu = add_rownames(rwl.M05Thu.mne.crn, var = "year")
rwi.M05Thu$year = as.numeric(rwi.M05Thu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M05Thu = lm(xxxstd ~ year, data = rwi.M05Thu)
summary(lm.M05Thu) # Linear model

plot(lm.M05Thu) # Diagnostic plots
# Mostly normal

plot(rwi.M05Thu$year, rwi.M05Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M05Thu$year, fitted(lm.M05Thu), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1930, 1940, 1950, 1970, 1990 (maybe), 2000

# Select Breakpoints #
selgmented(lm.M05Thu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1942, 1944, 2009 (1 also low)

davies.test(lm.M05Thu, k = round(nrow(rwi.M05Thu) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.M05Thu, k = round(nrow(rwi.M05Thu) / 5)) 
# Tests for the existence of one breakpoint
# Fringe significance

# Build Segmented Model #
slm.M05Thu = segmented.lm(lm.M05Thu, psi = c(1942,1944,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
summary(slm.M05Thu)
plot(slm.M05Thu) # Preliminary plot

davies.test(slm.M05Thu, k = round(nrow(rwi.M05Thu) / 5))
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.11)

pscore.test(slm.M05Thu, k = round(nrow(rwi.M05Thu) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M05Thu) # Changes if n.boot = 0
dev.off()

confint(slm.M05Thu, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M05Thu$year, rwi.M05Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M05Thu, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M05Thu, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M05Thu)[1:3]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M05Thu$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(rwi.M05Thu, year <= brk1)
seg1$year
seg2 = subset(rwi.M05Thu, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M05Thu, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M05Thu, year >= brk3)
seg4$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M05 ###
lts.M05_1 = add_rownames(lts.M05, var = "year")
lts.M05_1$year = as.numeric(lts.M05_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM05 = lm(Avg_CC_Median ~ year, data = lts.M05_1)
summary(lm.ltsM05) # Linear model

plot(lm.ltsM05) # Diagnostic plots
# Somewhat non-normal

plot(lts.M05_1$year, lts.M05_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M05_1$year, fitted(lm.ltsM05), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1990 (maybe), 2010

# Select Breakpoints #
selgmented(lm.ltsM05, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 2 breakpoints: 1986, 2009 (1 also low)

davies.test(lm.ltsM05, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.ltsM05, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant for 1 breakpoint

# Build Segmented Model # 
slm.ltsM05 = segmented.lm(lm.ltsM05, psi = c(1986,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM05)
plot(slm.ltsM05) # Preliminary plot

davies.test(slm.ltsM05, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (2007, p = 0.28)

pscore.test(slm.ltsM05, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM05) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM05, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M05_1$year, lts.M05_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM05, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM05, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM05)[1:2]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM05$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2

seg1 = subset(lts.M05_1, year <= brk1)
seg1$year
seg2 = subset(lts.M05_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.M05_1, year >= brk2)
seg3$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M06 Segmented Regression #####
#####
### RWI: M06Ace ###
rwi.M06Ace = add_rownames(rwl.M06Ace.mne.crn, var = "year")
rwi.M06Ace$year = as.numeric(rwi.M06Ace$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M06Ace = lm(xxxstd ~ year, data = rwi.M06Ace)
summary(lm.M06Ace) # Linear model

plot(lm.M06Ace) # Diagnostic plots
# Mostly normal

plot(rwi.M06Ace$year, rwi.M06Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M06Ace$year, fitted(lm.M06Ace), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1910, 1925, 1935, 1955, 1965, 1970, 2000

# Select Breakpoints #
selgmented(lm.M06Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 6 breakpoints: 1911, 1927, 1957, 1961, 1967, 2004 (next best is 0)

davies.test(lm.M06Ace, k = round(nrow(rwi.M06Ace) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2007, Fringe significance

pscore.test(lm.M06Ace, k = round(nrow(rwi.M06Ace) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model #
slm.M06Ace = segmented.lm(lm.M06Ace, psi = c(1911,1927,1957,1961,1967,2004), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 50
# n.boot = 50 changes it a bit (0 is the lowest BIC)
summary(slm.M06Ace)
plot(slm.M06Ace) # Preliminary plot

davies.test(slm.M06Ace, k = round(nrow(rwi.M06Ace) / 5))
# Estimate possible additional breakpoint
# Not significant (1920, p = 0.88)

pscore.test(slm.M06Ace, k = round(nrow(rwi.M06Ace) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M06Ace) # Changes if n.boot = 0
dev.off()

confint(slm.M06Ace, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M06Ace$year, rwi.M06Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M06Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M06Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M06Ace)[1:6]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M06Ace$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4
brk5 = round(brks[5,2])
brk5
brk6 = round(brks[6,2])
brk6

seg1 = subset(rwi.M06Ace, year <= brk1)
seg1$year
seg2 = subset(rwi.M06Ace, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M06Ace, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M06Ace, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M06Ace, year >= brk4 & year <= brk5)
seg5$year
seg6 = subset(rwi.M06Ace, year >= brk5 & year <= brk6)
seg6$year
seg7 = subset(rwi.M06Ace, year >= brk6)
seg7$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

sens.slope(seg6$xxxstd)
acf(seg6$xxxstd) 
mmkh(seg6$xxxstd)

sens.slope(seg7$xxxstd)
acf(seg7$xxxstd) 
mmkh(seg7$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)
seg6.lm = zyp.sen(xxxstd ~ year, seg6)
seg7.lm = zyp.sen(xxxstd ~ year, seg7)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq
seg7$slope = seg7.lm$coefficients[2] * seg7$year + seg7.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg6$year, seg6$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg7$year, seg7$slope, lwd = 3, col = "red3") # Color based on sig

### RWI: M06Que ###
rwi.M06Que = add_rownames(rwl.M06Que.mne.crn, var = "year")
rwi.M06Que$year = as.numeric(rwi.M06Que$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M06Que = lm(xxxstd ~ year, data = rwi.M06Que)
summary(lm.M06Que) # Linear model

plot(lm.M06Que) # Diagnostic plots
# Mostly normal

plot(rwi.M06Que$year, rwi.M06Que$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M06Que$year, fitted(lm.M06Que), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1910, 2020, 1930, 1650, 1960, 1975 (maybe), 1990, 2000, 2010

# Select Breakpoints #
selgmented(lm.M06Que, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints: second best = 2, then 5

davies.test(lm.M06Que, k = round(nrow(rwi.M06Que) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1997, not significant

pscore.test(lm.M06Que, k = round(nrow(rwi.M06Que) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model #
slm.M06Que = segmented.lm(lm.M06Que, npsi = 5, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
summary(slm.M06Que)
plot(slm.M06Que) # Preliminary plot

davies.test(slm.M06Que, k = round(nrow(rwi.M06Que) / 5))
# Estimate possible additional breakpoint
# Not significant (1920, p = 0.88)

pscore.test(slm.M06Que, k = round(nrow(rwi.M06Que) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M06Que) # Changes if n.boot = 0
dev.off()

confint(slm.M06Que, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M06Que$year, rwi.M06Que$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M06Que, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M06Que, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M06Que)[1:5]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M06Que$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4
brk5 = round(brks[5,2])
brk5

seg1 = subset(rwi.M06Que, year <= brk1)
seg1$year
seg2 = subset(rwi.M06Que, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M06Que, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M06Que, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M06Que, year >= brk4 & year <= brk5)
seg5$year
seg6 = subset(rwi.M06Que, year >= brk5)
seg6$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

sens.slope(seg6$xxxstd)
acf(seg6$xxxstd) 
mmkh(seg6$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)
seg6.lm = zyp.sen(xxxstd ~ year, seg6)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg6$year, seg6$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M06 ###
lts.M06_1 = add_rownames(lts.M06, var = "year")
lts.M06_1$year = as.numeric(lts.M06_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM06 = lm(Avg_CC_Median ~ year, data = lts.M06_1)
summary(lm.ltsM06) # Linear model

plot(lm.ltsM06) # Diagnostic plots
# Normal

plot(lts.M06_1$year, lts.M06_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M06_1$year, fitted(lm.ltsM06), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1985 (maybe), 2010

# Select Breakpoints #
selgmented(lm.ltsM06, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints, 1 second best

davies.test(lm.ltsM06, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, not significant

pscore.test(lm.ltsM06, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model # 
slm.ltsM06 = segmented.lm(lm.ltsM06, npsi = 1, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM06)
plot(slm.ltsM06) # Preliminary plot

davies.test(slm.ltsM06, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1978, p = 0.91)

pscore.test(slm.ltsM06, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM06) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM06, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M06_1$year, lts.M06_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM06, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM06, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM06)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM06$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(lts.M06_1, year <= brk1)
seg1$year
seg2 = subset(lts.M06_1, year >= brk1)
seg2$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
#####

##### M07 Segmented Regression #####
#####
### RWI: M07Tsu ###
rwi.M07Tsu = add_rownames(rwl.M07Tsu.mne.crn, var = "year")
rwi.M07Tsu$year = as.numeric(rwi.M07Tsu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M07Tsu = lm(xxxstd ~ year, data = rwi.M07Tsu)
summary(lm.M07Tsu) # Linear model

plot(lm.M07Tsu) # Diagnostic plots
# Mostly normal

plot(rwi.M07Tsu$year, rwi.M07Tsu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M07Tsu$year, fitted(lm.M07Tsu), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1905, 1915, 1925, 1935, 1960, 1995, 2005, 2010

# Select Breakpoints #
selgmented(lm.M07Tsu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1923, 1960, 1994 (second lowest is 1)

davies.test(lm.M07Tsu, k = round(nrow(rwi.M07Tsu) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1961, significant

pscore.test(lm.M07Tsu, k = round(nrow(rwi.M07Tsu) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model #
slm.M07Tsu = segmented.lm(lm.M07Tsu, psi = c(1903,1910,1923,1930,1960,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 50
# psi = c(1923,1960,1994)
summary(slm.M07Tsu)
plot(slm.M07Tsu) # Preliminary plot

davies.test(slm.M07Tsu, k = round(nrow(rwi.M07Tsu) / 5))
# Estimate possible additional breakpoint
# Not significant (1910, p = 0.23)

pscore.test(slm.M07Tsu, k = round(nrow(rwi.M07Tsu) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Significant

draw.history(slm.M07Tsu) # Changes if n.boot = 0
dev.off()

confint(slm.M07Tsu, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M07Tsu$year, rwi.M07Tsu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M07Tsu, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M07Tsu, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M07Tsu)[1:6]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M07Tsu$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4
brk5 = round(brks[5,2])
brk5
brk6 = round(brks[6,2])
brk6

seg1 = subset(rwi.M07Tsu, year <= brk1)
seg1$year
seg2 = subset(rwi.M07Tsu, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M07Tsu, year >= brk2 & year <= brk3)
seg3$year
#seg4 = subset(rwi.M07Tsu, year >= brk3)
#seg4$year
seg4 = subset(rwi.M07Tsu, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M07Tsu, year >= brk4 & year <= brk5)
seg5$year
seg6 = subset(rwi.M07Tsu, year >= brk5 & year <= brk6)
seg6$year
seg7 = subset(rwi.M07Tsu, year >= brk6)
seg7$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

sens.slope(seg6$xxxstd)
acf(seg6$xxxstd) 
mmkh(seg6$xxxstd)

sens.slope(seg7$xxxstd)
acf(seg7$xxxstd) 
mmkh(seg7$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)
seg6.lm = zyp.sen(xxxstd ~ year, seg6)
seg7.lm = zyp.sen(xxxstd ~ year, seg7)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq
seg7$slope = seg7.lm$coefficients[2] * seg7$year + seg7.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg6$year, seg6$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg7$year, seg7$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: M07 ###
lts.M07_1 = add_rownames(lts.M07, var = "year")
lts.M07_1$year = as.numeric(lts.M07_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM07 = lm(Avg_CC_Median ~ year, data = lts.M07_1)
summary(lm.ltsM07) # Linear model

plot(lm.ltsM07) # Diagnostic plots
# Normal

plot(lts.M07_1$year, lts.M07_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M07_1$year, fitted(lm.ltsM07), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 2005 (maybe)

# Select Breakpoints #
selgmented(lm.ltsM07, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints, 1 second best

davies.test(lm.ltsM07, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, not significant

pscore.test(lm.ltsM07, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model # 
slm.ltsM07 = segmented.lm(lm.ltsM07, npsi = 1, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM07)
plot(slm.ltsM07) # Preliminary plot

davies.test(slm.ltsM07, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1973, p = 0.61)

pscore.test(slm.ltsM07, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM07) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM07, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M07_1$year, lts.M07_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM07, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM07, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM07)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM07$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(lts.M07_1, year <= brk1)
seg1$year
seg2 = subset(lts.M07_1, year >= brk1)
seg2$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
#####

##### M13 Segmented Regression #####
#####
### RWI: M13Tsu ###
rwi.M13Tsu = add_rownames(rwl.M13Tsu.mne.crn, var = "year")
rwi.M13Tsu$year = as.numeric(rwi.M13Tsu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M13Tsu = lm(xxxstd ~ year, data = rwi.M13Tsu)
summary(lm.M13Tsu) # Linear model

plot(lm.M13Tsu) # Diagnostic plots
# Not really normal

plot(rwi.M13Tsu$year, rwi.M13Tsu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M13Tsu$year, fitted(lm.M13Tsu), lwd = 3, col = "darkgreen")
# Not significant
# Est. breakpoints: 1915, 1930, 1940, 1955, 1990, 2010 (maybe)

# Select Breakpoints #
selgmented(lm.M13Tsu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints: 1913, 1926, 1988, 2010 (6 second lowest)

davies.test(lm.M13Tsu, k = round(nrow(rwi.M13Tsu) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1950, significant

pscore.test(lm.M13Tsu, k = round(nrow(rwi.M13Tsu) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model #
slm.M13Tsu = segmented.lm(lm.M13Tsu, psi = c(1913,1926,1988,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
#psi = c(1913,1926,1988,2010) lowest bic
summary(slm.M13Tsu)
plot(slm.M13Tsu) # Preliminary plot

davies.test(slm.M13Tsu, k = round(nrow(rwi.M13Tsu) / 5))
# Estimate possible additional breakpoint
# Not significant (1976, p = 0.55)

pscore.test(slm.M13Tsu, k = round(nrow(rwi.M13Tsu) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M13Tsu) # Changes if n.boot = 0
dev.off()

confint(slm.M13Tsu, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M13Tsu$year, rwi.M13Tsu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M13Tsu, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M13Tsu, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M13Tsu)[1:4]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M13Tsu$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(rwi.M13Tsu, year <= brk1)
seg1$year
seg2 = subset(rwi.M13Tsu, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M13Tsu, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M13Tsu, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M13Tsu, year >= brk4)
seg5$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M13 ###
lts.M13_1 = add_rownames(lts.M13, var = "year")
lts.M13_1$year = as.numeric(lts.M13_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM13 = lm(Avg_CC_Median ~ year, data = lts.M13_1)
summary(lm.ltsM13) # Linear model

plot(lm.ltsM13) # Diagnostic plots
# Not really normal

plot(lts.M13_1$year, lts.M13_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M13_1$year, fitted(lm.ltsM13), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 2010

# Select Breakpoints #
selgmented(lm.ltsM13, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 2010 (2 second best)

davies.test(lm.ltsM13, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2012, significant

pscore.test(lm.ltsM13, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.ltsM13 = segmented.lm(lm.ltsM13, npsi = 2, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
# psi = c(2010) lowest BIC
summary(slm.ltsM13)
plot(slm.ltsM13) # Preliminary plot

davies.test(slm.ltsM13, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1988, p = 0.23)

pscore.test(slm.ltsM13, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Fringe significant

draw.history(slm.ltsM13) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM13, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M13_1$year, lts.M13_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM13, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM13, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM13)[1:2]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM13$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2

seg1 = subset(lts.M13_1, year <= brk1)
seg1$year
seg2 = subset(lts.M13_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.M13_1, year >= brk2)
seg3$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "gray48") # Color based on sig
#####

##### M17 Segmented Regression #####
#####
### RWI: M17Thu ###
rwi.M17Thu = add_rownames(rwl.M17Thu.mne.crn, var = "year")
rwi.M17Thu$year = as.numeric(rwi.M17Thu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M17Thu = lm(xxxstd ~ year, data = rwi.M17Thu)
summary(lm.M17Thu) # Linear model

plot(lm.M17Thu) # Diagnostic plots
# Not really normal

plot(rwi.M17Thu$year, rwi.M17Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M17Thu$year, fitted(lm.M17Thu), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1955, 1975, 1995

# Select Breakpoints #
selgmented(lm.M17Thu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1953, 1975, 1993

davies.test(lm.M17Thu, k = round(nrow(rwi.M17Thu) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.M17Thu, k = round(nrow(rwi.M17Thu) / 5)) 
# Tests for the existence of one breakpoint
# Not significant

# Build Segmented Model #
slm.M17Thu = segmented.lm(lm.M17Thu, psi = c(1953,1975,1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
summary(slm.M17Thu)
plot(slm.M17Thu) # Preliminary plot

davies.test(slm.M17Thu, k = round(nrow(rwi.M17Thu) / 5))
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.67)

pscore.test(slm.M17Thu, k = round(nrow(rwi.M17Thu) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Significant (0.05)

draw.history(slm.M17Thu) # Changes if n.boot = 0
dev.off()

confint(slm.M17Thu, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M17Thu$year, rwi.M17Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M17Thu, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M17Thu, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M17Thu)[1:3]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M17Thu$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(rwi.M17Thu, year <= brk1)
seg1$year
seg2 = subset(rwi.M17Thu, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M17Thu, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M17Thu, year >= brk3)
seg4$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig

### LTS: M17 ###
lts.M17_1 = add_rownames(lts.M17, var = "year")
lts.M17_1$year = as.numeric(lts.M17_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM17 = lm(Avg_CC_Median ~ year, data = lts.M17_1)
summary(lm.ltsM17) # Linear model

plot(lm.ltsM17) # Diagnostic plots
# Not really normal

plot(lts.M17_1$year, lts.M17_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M17_1$year, fitted(lm.ltsM17), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1995, 2010

# Select Breakpoints #
selgmented(lm.ltsM17, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 1997 (2 also low)

davies.test(lm.ltsM17, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1997, significant

pscore.test(lm.ltsM17, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.ltsM17 = segmented.lm(lm.ltsM17, npsi = 2, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
#psi= c(1997) lowest BIC
summary(slm.ltsM17)
plot(slm.ltsM17) # Preliminary plot

davies.test(slm.ltsM17, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.28)

pscore.test(slm.ltsM17, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM17) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM17, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M17_1$year, lts.M17_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM17, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM17, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM17)[1:2]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM17$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2

seg1 = subset(lts.M17_1, year <= brk1)
seg1$year
#seg2 = subset(lts.M17_1, year >= brk1)
#seg2$year
seg2 = subset(lts.M17_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.M17_1, year >= brk2)
seg3$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M20 Segmented Regression #####
#####
### RWI: M20Con ###
# rwi.M20Con = add_rownames(rwl.M20Con.mne.crn, var = "year")
# rwi.M20Con$year = as.numeric(rwi.M20Con$year) # Years column in chr by default
# 
# # Build and Plot Simple Linear Model #
# lm.M20Con = lm(xxxstd ~ year, data = rwi.M20Con)
# summary(lm.M20Con) # Linear model
# 
# plot(lm.M20Con) # Diagnostic plots
# # Normal
# 
# plot(rwi.M20Con$year, rwi.M20Con$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# lines(rwi.M20Con$year, fitted(lm.M20Con), lwd = 3, col = "gray48")
# # Not significant
# # Est. breakpoints: none really..
# 
# # Select Breakpoints #
# selgmented(lm.M20Con, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# # Select number (max = 10) and estimated year of breakpoints
# # Using BIC allows to compare models with more than 2 breakpoints
# # Selected 0 breakpoints (2 2nd lowest)
# 
# # No breakpoints

### RWI: M20Thu ###
rwi.M20Thu = add_rownames(rwl.M20Thu.mne.crn, var = "year")
rwi.M20Thu$year = as.numeric(rwi.M20Thu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M20Thu = lm(xxxstd ~ year, data = rwi.M20Thu)
summary(lm.M20Thu) # Linear model

plot(lm.M20Thu) # Diagnostic plots
# Normal

plot(rwi.M20Thu$year, rwi.M20Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M20Thu$year, fitted(lm.M20Thu), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: none really

# Select Breakpoints #
selgmented(lm.M20Thu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected no breakpoints (4 second lowest)

# No breakpoints

### LTS: M20 ###
lts.M20_1 = add_rownames(lts.M20, var = "year")
lts.M20_1$year = as.numeric(lts.M20_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM20 = lm(Avg_CC_Median ~ year, data = lts.M20_1)
summary(lm.ltsM20) # Linear model

plot(lm.ltsM20) # Diagnostic plots
# Not really normal

plot(lts.M20_1$year, lts.M20_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M20_1$year, fitted(lm.ltsM20), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 2000, 2010 (maybe)

# Select Breakpoints #
selgmented(lm.ltsM20, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 1975, 1997 (1 second lowest)

davies.test(lm.ltsM20, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1997, significant

pscore.test(lm.ltsM20, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.ltsM20 = segmented.lm(lm.ltsM20, psi = c(1975,1997), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM20)
plot(slm.ltsM20) # Preliminary plot

davies.test(slm.ltsM20, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1983, p = 0.83)

pscore.test(slm.ltsM20, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM20) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM20, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M20_1$year, lts.M20_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM20, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM20, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM20)[1:2]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM20$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2

seg1 = subset(lts.M20_1, year <= brk1)
seg1$year
#seg2 = subset(lts.M20_1, year >= brk1)
#seg2$year
seg2 = subset(lts.M20_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.M20_1, year >= brk2)
seg3$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M26 Segmented Regression #####
#####
### RWI: M26Dec ###
rwi.M26Dec = add_rownames(rwl.M26Dec.mne.crn, var = "year")
rwi.M26Dec$year = as.numeric(rwi.M26Dec$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M26Dec = lm(xxxstd ~ year, data = rwi.M26Dec)
summary(lm.M26Dec) # Linear model

plot(lm.M26Dec) # Diagnostic plots
# Normal

plot(rwi.M26Dec$year, rwi.M26Dec$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M26Dec$year, fitted(lm.M26Dec), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1930, 1940, 1955, 1965, 1980, 2000 (could see 0)

# Select Breakpoints #
selgmented(lm.M26Dec, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 0 breakpoints (1 second best)

# No breakpoints

# ### RWI: M26Ace ###
# rwi.M26Ace = add_rownames(rwl.M26Ace.mne.crn, var = "year")
# rwi.M26Ace$year = as.numeric(rwi.M26Ace$year) # Years column in chr by default
# 
# # Build and Plot Simple Linear Model #
# lm.M26Ace = lm(xxxstd ~ year, data = rwi.M26Ace)
# summary(lm.M26Ace) # Linear model
# 
# plot(lm.M26Ace) # Diagnostic plots
# # Not really normal
# 
# plot(rwi.M26Ace$year, rwi.M26Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# lines(rwi.M26Ace$year, fitted(lm.M26Ace), lwd = 3, col = "gray48")
# # Not significant
# # Est. breakpoints: 1930, 1940, 1950, 1970, 1980, 2000
# 
# # Select Breakpoints #
# selgmented(lm.M26Ace, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# # Select number (max = 10) and estimated year of breakpoints
# # Using BIC allows to compare models with more than 2 breakpoints
# # Selected 1 breakpoint: 1928 (2 second lowest, also check 6)
# 
# davies.test(lm.M26Ace, k = round(nrow(rwi.M26Ace) / 5)) # Default k = 10
# # Estimate first break point. Check 20% of years
# # Best at 1930, significant
# 
# pscore.test(lm.M26Ace, k = round(nrow(rwi.M26Ace) / 5)) 
# # Tests for the existence of one breakpoint
# # Significant
# 
# # Build Segmented Model #
# slm.M26Ace = segmented.lm(lm.M26Ace, npsi = 5, # psi based on selgmented
#                           control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
# # psi = c(1928)
# # npsi = 2
# summary(slm.M26Ace)
# plot(slm.M26Ace) # Preliminary plot
# 
# davies.test(slm.M26Ace, k = round(nrow(rwi.M26Ace) / 5))
# # Estimate possible additional breakpoint
# # Not significant (2002, p = 0.59)
# 
# pscore.test(slm.M26Ace, k = round(nrow(rwi.M26Ace) / 5), more.break = TRUE) 
# # Test for an additional breakpoint
# # Not significant
# 
# draw.history(slm.M26Ace) # Changes if n.boot = 0
# dev.off()
# 
# confint(slm.M26Ace, digits = 4) # 1.96 x SE at 95% CI
# 
# plot(rwi.M26Ace$year, rwi.M26Ace$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
# plot(slm.M26Ace, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
# my.seglines(slm.M26Ace, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
# abline(v = round(confint(slm.M26Ace)[1:5]), lty = 2, col = "blue") # Round vertical break
# 
# # MK Significant and TS Slope for Segments #
# brks = slm.M26Ace$psi
# brk1 = round(brks[1,2])
# brk1
# brk2 = round(brks[2,2])
# brk2
# brk3 = round(brks[3,2])
# brk3
# brk4 = round(brks[4,2])
# brk4
# brk5 = round(brks[5,2])
# brk5
# 
# seg1 = subset(rwi.M26Ace, year <= brk1)
# seg1$year
# #seg2 = subset(rwi.M26Ace, year >= brk1)
# #seg2$year
# seg2 = subset(rwi.M26Ace, year >= brk1 & year <= brk2)
# seg2$year
# #seg3 = subset(rwi.M26Ace, year >= brk2)
# #seg3$year
# seg3 = subset(rwi.M26Ace, year >= brk2 & year <= brk3)
# seg3$year
# seg4 = subset(rwi.M26Ace, year >= brk3 & year <= brk4)
# seg4$year
# seg5 = subset(rwi.M26Ace, year >= brk4 & year <= brk5)
# seg5$year
# seg6 = subset(rwi.M26Ace, year >= brk5)
# seg6$year
# 
# sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
# acf(seg1$xxxstd) # Plot autocorrelation
# mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)
# 
# sens.slope(seg2$xxxstd)
# acf(seg2$xxxstd) 
# mmkh(seg2$xxxstd)
# 
# sens.slope(seg3$xxxstd)
# acf(seg3$xxxstd) 
# mmkh(seg3$xxxstd)
# 
# sens.slope(seg4$xxxstd)
# acf(seg4$xxxstd) 
# mmkh(seg4$xxxstd)
# 
# sens.slope(seg5$xxxstd)
# acf(seg5$xxxstd) 
# mmkh(seg5$xxxstd)
# 
# sens.slope(seg6$xxxstd)
# acf(seg6$xxxstd) 
# mmkh(seg6$xxxstd)
# 
# seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
# seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
# seg3.lm = zyp.sen(xxxstd ~ year, seg3)
# seg4.lm = zyp.sen(xxxstd ~ year, seg4)
# seg5.lm = zyp.sen(xxxstd ~ year, seg5)
# seg6.lm = zyp.sen(xxxstd ~ year, seg6)
# 
# seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
# seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
# seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
# seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
# seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq
# seg6$slope = seg6.lm$coefficients[2] * seg6$year + seg6.lm$coefficients[1] # Eq
# 
# # Update Plot
# lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
# lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
# lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
# lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig
# lines(seg5$year, seg5$slope, lwd = 3, col = "red3") # Color based on sig
# lines(seg6$year, seg6$slope, lwd = 3, col = "darkgreen") # Color based on sig

### RWI: M26Thu ###
rwi.M26Thu = add_rownames(rwl.M26Thu.mne.crn, var = "year")
rwi.M26Thu$year = as.numeric(rwi.M26Thu$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M26Thu = lm(xxxstd ~ year, data = rwi.M26Thu)
summary(lm.M26Thu) # Linear model

plot(lm.M26Thu) # Diagnostic plots
# Normal

plot(rwi.M26Thu$year, rwi.M26Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M26Thu$year, fitted(lm.M26Thu), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1930 (maybe), 1970, 1980, 2000

# Select Breakpoints #
selgmented(lm.M26Thu, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1969, 1983, 1993 (2 second best, also 4)

davies.test(lm.M26Thu, k = round(nrow(rwi.M26Thu) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 1987, significant

pscore.test(lm.M26Thu, k = round(nrow(rwi.M26Thu) / 5)) 
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model #
slm.M26Thu = segmented.lm(lm.M26Thu, psi = c(1969,1983,1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
# n.boot = 50
summary(slm.M26Thu)
plot(slm.M26Thu) # Preliminary plot

davies.test(slm.M26Thu, k = round(nrow(rwi.M26Thu) / 5))
# Estimate possible additional breakpoint
# Not significant (2012, p = 0.39)

pscore.test(slm.M26Thu, k = round(nrow(rwi.M26Thu) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M26Thu) # Changes if n.boot = 0
dev.off()

confint(slm.M26Thu, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M26Thu$year, rwi.M26Thu$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M26Thu, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M26Thu, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M26Thu)[1:3]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M26Thu$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(rwi.M26Thu, year <= brk1)
seg1$year
seg2 = subset(rwi.M26Thu, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M26Thu, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M26Thu, year >= brk3)
seg4$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M26 ###
lts.M26_1 = add_rownames(lts.M26, var = "year")
lts.M26_1$year = as.numeric(lts.M26_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM26 = lm(Avg_CC_Median ~ year, data = lts.M26_1)
summary(lm.ltsM26) # Linear model

plot(lm.ltsM26) # Diagnostic plots
# Not really normal

plot(lts.M26_1$year, lts.M26_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M26_1$year, fitted(lm.ltsM26), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1980, 1985, 2010

# Select Breakpoints #
selgmented(lm.ltsM26, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 3 breakpoints: 1980, 1981, 2010 (4 second lowest)

davies.test(lm.ltsM26, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.ltsM26, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.ltsM26 = segmented.lm(lm.ltsM26, psi = c(1979,1981,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM26)
plot(slm.ltsM26) # Preliminary plot

davies.test(slm.ltsM26, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1973, p = 0.62)

pscore.test(slm.ltsM26, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM26) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM26, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M26_1$year, lts.M26_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM26, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM26, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM26)[1:3]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM26$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3

seg1 = subset(lts.M26_1, year <= brk1)
seg1$year
seg2 = subset(lts.M26_1, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(lts.M26_1, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(lts.M26_1, year >= brk3)
seg4$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

sens.slope(seg3$Avg_CC_Median)
acf(seg3$Avg_CC_Median)
mmkh(seg3$Avg_CC_Median)

sens.slope(seg4$Avg_CC_Median)
acf(seg4$Avg_CC_Median)
mmkh(seg4$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(Avg_CC_Median ~ year, seg3)
seg4.lm = zyp.sen(Avg_CC_Median ~ year, seg4)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### M27 Segmented Regression #####
#####
### RWI: M27Pin ###
rwi.M27Pin = add_rownames(rwl.M27Pin.mne.crn, var = "year")
rwi.M27Pin$year = as.numeric(rwi.M27Pin$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.M27Pin = lm(xxxstd ~ year, data = rwi.M27Pin)
summary(lm.M27Pin) # Linear model

plot(lm.M27Pin) # Diagnostic plots
# Somewhat non-Normal

plot(rwi.M27Pin$year, rwi.M27Pin$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
lines(rwi.M27Pin$year, fitted(lm.M27Pin), lwd = 3, col = "gray48")
# Not significant
# Est. breakpoints: 1985, 2010, 2015

# Select Breakpoints #
selgmented(lm.M27Pin, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 4 breakpoints: 1985, 2000, 2008, 2012 (3 also low BIC)

davies.test(lm.M27Pin, k = round(nrow(rwi.M27Pin) / 5)) # Default k = 10
# Estimate first break point. Check 20% of years
# Best at 2007, significant

pscore.test(lm.M27Pin, k = round(nrow(rwi.M27Pin) / 5)) 
# Tests for the existence of one breakpoint
# Fringe Significant

# Build Segmented Model #
slm.M27Pin = segmented.lm(lm.M27Pin, psi = c(1985,2000,2008,2012), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 50
summary(slm.M27Pin)
plot(slm.M27Pin) # Preliminary plot

davies.test(slm.M27Pin, k = round(nrow(rwi.M27Pin) / 5))
# Estimate possible additional breakpoint
# Not significant (2002, p = 0.89)

pscore.test(slm.M27Pin, k = round(nrow(rwi.M27Pin) / 5), more.break = TRUE) 
# Test for an additional breakpoint
# Not significant

draw.history(slm.M27Pin) # Changes if n.boot = 0
dev.off()

confint(slm.M27Pin, digits = 4) # 1.96 x SE at 95% CI

plot(rwi.M27Pin$year, rwi.M27Pin$xxxstd, type = "l", ylab = "RWI", xlab = "Year")
plot(slm.M27Pin, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.M27Pin, col = "blue", bottom = TRUE) # Rounded breakpoint CIs
abline(v = round(confint(slm.M27Pin)[1:4]), lty = 2, col = "blue") # Round vertical break

# MK Significant and TS Slope for Segments #
brks = slm.M27Pin$psi
brk1 = round(brks[1,2])
brk1
brk2 = round(brks[2,2])
brk2
brk3 = round(brks[3,2])
brk3
brk4 = round(brks[4,2])
brk4

seg1 = subset(rwi.M27Pin, year <= brk1)
seg1$year
seg2 = subset(rwi.M27Pin, year >= brk1 & year <= brk2)
seg2$year
seg3 = subset(rwi.M27Pin, year >= brk2 & year <= brk3)
seg3$year
seg4 = subset(rwi.M27Pin, year >= brk3 & year <= brk4)
seg4$year
seg5 = subset(rwi.M27Pin, year >= brk4)
seg5$year

sens.slope(seg1$xxxstd) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$xxxstd) # Plot autocorrelation
mmkh(seg1$xxxstd) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$xxxstd)
acf(seg2$xxxstd) 
mmkh(seg2$xxxstd)

sens.slope(seg3$xxxstd)
acf(seg3$xxxstd) 
mmkh(seg3$xxxstd)

sens.slope(seg4$xxxstd)
acf(seg4$xxxstd) 
mmkh(seg4$xxxstd)

sens.slope(seg5$xxxstd)
acf(seg5$xxxstd) 
mmkh(seg5$xxxstd)

seg1.lm = zyp.sen(xxxstd ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(xxxstd ~ year, seg2) # Gives Sen slope and median intercept
seg3.lm = zyp.sen(xxxstd ~ year, seg3)
seg4.lm = zyp.sen(xxxstd ~ year, seg4)
seg5.lm = zyp.sen(xxxstd ~ year, seg5)

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq
seg3$slope = seg3.lm$coefficients[2] * seg3$year + seg3.lm$coefficients[1] # Eq
seg4$slope = seg4.lm$coefficients[2] * seg4$year + seg4.lm$coefficients[1] # Eq
seg5$slope = seg5.lm$coefficients[2] * seg5$year + seg5.lm$coefficients[1] # Eq

# Update Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg3$year, seg3$slope, lwd = 3, col = "red3") # Color based on sig
lines(seg4$year, seg4$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg5$year, seg5$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: M27 ###
lts.M27_1 = add_rownames(lts.M27, var = "year")
lts.M27_1$year = as.numeric(lts.M27_1$year) # Years column in chr by default

# Build and Plot Simple Linear Model #
lm.ltsM27 = lm(Avg_CC_Median ~ year, data = lts.M27_1)
summary(lm.ltsM27) # Linear model

plot(lm.ltsM27) # Diagnostic plots
# Not really normal

plot(lts.M27_1$year, lts.M27_1$Avg_CC_Median, type = "l", ylab = "RWI", xlab = "Year")
lines(lts.M27_1$year, fitted(lm.ltsM27), lwd = 3, col = "darkgreen")
# Significant
# Est. breakpoints: 1980 (maybe), 2000

# Select Breakpoints #
selgmented(lm.ltsM27, type = "bic", Kmax = 10) # Experimental, uses pscore / davies
# Select number (max = 10) and estimated year of breakpoints
# Using BIC allows to compare models with more than 2 breakpoints
# Selected 1 breakpoint: 1997 (2 also low)

davies.test(lm.ltsM27, k = 10) # 20% or 10 (whatever larger)
# Estimate first break point. Check 20% of years
# Best at 1997, significant

pscore.test(lm.ltsM27, k = 10) # 20% or 10
# Tests for the existence of one breakpoint
# Significant

# Build Segmented Model # 
slm.ltsM27 = segmented.lm(lm.ltsM27, psi = c(1997), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0
summary(slm.ltsM27)
plot(slm.ltsM27) # Preliminary plot

davies.test(slm.ltsM27, k = 10) # 20% or 10
# Estimate possible additional breakpoint
# Not significant (1978, p = 0.58)

pscore.test(slm.ltsM27, k = 10, more.break = TRUE) # 20% or 10
# Test for an additional breakpoint
# Not significant

draw.history(slm.ltsM27) # Changes if n.boot = 0
dev.off()

confint(slm.ltsM27, digits = 4) # 1.96 x SE at 95% CI

plot(lts.M27_1$year, lts.M27_1$Avg_CC_Median, type = "l", ylab = "%CC", xlab = "Year")
plot(slm.ltsM27, add  = TRUE, res = FALSE, lwd = 3, lty = 2) # Segmented fit
my.seglines(slm.ltsM27, col = "blue", bottom = TRUE) # Breakpoint CIs
abline(v = round(confint(slm.ltsM27)[1:1]), lty = 2, col = "blue") # Breakpoint vertical

# MK Significant and TS Slope for Segments #
brks = slm.ltsM27$psi
brk1 = round(brks[1,2])
brk1

seg1 = subset(lts.M27_1, year <= brk1)
seg1$year
seg2 = subset(lts.M27_1, year >= brk1)
seg2$year

sens.slope(seg1$Avg_CC_Median) # Theil-Sen slope (outlier resistant) + Mann-Kendall  p-value
acf(seg1$Avg_CC_Median) # Plot autocorrelation
mmkh(seg1$Avg_CC_Median) # Mann-Kendall modified for autocorrelated data (Neff)

sens.slope(seg2$Avg_CC_Median)
acf(seg2$Avg_CC_Median)
mmkh(seg2$Avg_CC_Median)

seg1.lm = zyp.sen(Avg_CC_Median ~ year, seg1) # Plot T-S slope using zyp method
seg2.lm = zyp.sen(Avg_CC_Median ~ year, seg2) # Gives Sen slope and median intercept

seg1$slope = seg1.lm$coefficients[2] * seg1$year + seg1.lm$coefficients[1] # Eq
seg2$slope = seg2.lm$coefficients[2] * seg2$year + seg2.lm$coefficients[1] # Eq

# Final Plot
lines(seg1$year, seg1$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2$year, seg2$slope, lwd = 3, col = "red3") # Color based on sig
#####

##### Combined Plot #####
#####
# F07Ace
rwi.F07Ace = add_rownames(rwl.F07Ace.mne.crn, var = "year")
rwi.F07Ace$year = as.numeric(rwi.F07Ace$year) # Years column in chr by default

lm.F07Ace = lm(xxxstd ~ year, data = rwi.F07Ace)

slm.F07Ace = segmented.lm(lm.F07Ace, psi = c(1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F07Ace.ci = confint(slm.F07Ace, digits = 4) # 1.96 x SE at 95% CI

brks.F07Ace = slm.F07Ace$psi
brk1.F07Ace = round(brks.F07Ace[1,2])

seg1.F07Ace = subset(rwi.F07Ace, year <= brk1.F07Ace)
seg2.F07Ace = subset(rwi.F07Ace, year >= brk1.F07Ace)

seg1.F07Ace.lm = zyp.sen(xxxstd ~ year, seg1.F07Ace) # Plot T-S slope using zyp method
seg2.F07Ace.lm = zyp.sen(xxxstd ~ year, seg2.F07Ace)

seg1.F07Ace$slope = seg1.F07Ace.lm$coefficients[2] * 
  seg1.F07Ace$year + seg1.F07Ace.lm$coefficients[1] # Eq
seg2.F07Ace$slope = seg2.F07Ace.lm$coefficients[2] * 
  seg2.F07Ace$year + seg2.F07Ace.lm$coefficients[1] # Eq

F07Ace.p = ggplot(data = rwi.F07Ace) +
  #geom_point(aes(x = slm.F07Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F07Ace.ci[1,2], xend = slm.F07Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F07Ace.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F07Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F07Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F07Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("08Ace (-0.12)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
F07Ace.p

# F07LTS (Top Left 2)
lts.F07_1 = add_rownames(lts.F07, var = "year")
lts.F07_1$year = as.numeric(lts.F07_1$year) # Years column in chr by default

lm.ltsF07 = lm(Avg_CC_Median ~ year, data = lts.F07_1)

slm.ltsF07 = segmented.lm(lm.ltsF07, psi = c(1981,1984,2005), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF07.ci = confint(slm.ltsF07, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF07 = slm.ltsF07$psi
brk1.ltsF07 = round(brks.ltsF07[1,2])
brk2.ltsF07 = round(brks.ltsF07[2,2])
brk3.ltsF07 = round(brks.ltsF07[3,2])

seg1.ltsF07 = subset(lts.F07_1, year <= brk1.ltsF07)
seg2.ltsF07 = subset(lts.F07_1, year >= brk1.ltsF07 & year <= brk2.ltsF07)
seg3.ltsF07 = subset(lts.F07_1, year >= brk2.ltsF07 & year <= brk3.ltsF07)
seg4.ltsF07 = subset(lts.F07_1, year >= brk3.ltsF07)

seg1.ltsF07.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF07) 
seg2.ltsF07.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF07) 
seg3.ltsF07.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsF07)
seg4.ltsF07.lm = zyp.sen(Avg_CC_Median ~ year, seg4.ltsF07)

seg1.ltsF07$slope = seg1.ltsF07.lm$coefficients[2] * 
  seg1.ltsF07$year + seg1.ltsF07.lm$coefficients[1] # Eq
seg2.ltsF07$slope = seg2.ltsF07.lm$coefficients[2] * 
  seg2.ltsF07$year + seg2.ltsF07.lm$coefficients[1] # Eq
seg3.ltsF07$slope = seg3.ltsF07.lm$coefficients[2] * 
  seg3.ltsF07$year + seg3.ltsF07.lm$coefficients[1] # Eq
seg4.ltsF07$slope = seg4.ltsF07.lm$coefficients[2] * 
  seg4.ltsF07$year + seg4.ltsF07.lm$coefficients[1] # Eq

ltsF07.p = ggplot(data = lts.F07_1) +
  #geom_point(aes(x = slm.ltsF07.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF07.ci[1,2], xend = slm.ltsF07.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF07.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF07.ci[2,1], y = 73.5), col = "gray48") +
  geom_segment(aes(x = slm.ltsF07.ci[2,2], xend = slm.ltsF07.ci[2,3], y = 73.5, 
                   yend = 73.5), col = "gray48") +
  geom_vline(xintercept = slm.ltsF07.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF07.ci[3,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF07.ci[3,2], xend = slm.ltsF07.ci[3,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF07.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF07$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF07, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.ltsF07, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.ltsF07, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.ltsF07, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "%CC", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"))
ltsF07.p

# F15Que
rwi.F15Que = add_rownames(rwl.F15Que.mne.crn, var = "year")
rwi.F15Que$year = as.numeric(rwi.F15Que$year) # Years column in chr by default

lm.F15Que = lm(xxxstd ~ year, data = rwi.F15Que)

slm.F15Que = segmented.lm(lm.F15Que, psi = c(1920,1935,1950,1985,2005,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F15Que.ci = confint(slm.F15Que, digits = 4) # 1.96 x SE at 95% CI

brks.F15Que = slm.F15Que$psi
brk1.F15Que = round(brks.F15Que[1,2])
brk2.F15Que = round(brks.F15Que[2,2])
brk3.F15Que = round(brks.F15Que[3,2])
brk4.F15Que = round(brks.F15Que[4,2])
brk5.F15Que = round(brks.F15Que[5,2])
brk6.F15Que = round(brks.F15Que[6,2])

seg1.F15Que = subset(rwi.F15Que, year <= brk1.F15Que)
seg2.F15Que = subset(rwi.F15Que, year >= brk1.F15Que & year <= brk2.F15Que)
seg3.F15Que = subset(rwi.F15Que, year >= brk2.F15Que & year <= brk3.F15Que)
seg4.F15Que = subset(rwi.F15Que, year >= brk3.F15Que & year <= brk4.F15Que)
seg5.F15Que = subset(rwi.F15Que, year >= brk4.F15Que & year <= brk5.F15Que)
seg6.F15Que = subset(rwi.F15Que, year >= brk5.F15Que & year <= brk6.F15Que)
seg7.F15Que = subset(rwi.F15Que, year >= brk6.F15Que)

seg1.F15Que.lm = zyp.sen(xxxstd ~ year, seg1.F15Que) 
seg2.F15Que.lm = zyp.sen(xxxstd ~ year, seg2.F15Que) 
seg3.F15Que.lm = zyp.sen(xxxstd ~ year, seg3.F15Que)
seg4.F15Que.lm = zyp.sen(xxxstd ~ year, seg4.F15Que)
seg5.F15Que.lm = zyp.sen(xxxstd ~ year, seg5.F15Que)
seg6.F15Que.lm = zyp.sen(xxxstd ~ year, seg6.F15Que)
seg7.F15Que.lm = zyp.sen(xxxstd ~ year, seg7.F15Que)

seg1.F15Que$slope = seg1.F15Que.lm$coefficients[2] * 
  seg1.F15Que$year + seg1.F15Que.lm$coefficients[1] # Eq
seg2.F15Que$slope = seg2.F15Que.lm$coefficients[2] * 
  seg2.F15Que$year + seg2.F15Que.lm$coefficients[1] # Eq
seg3.F15Que$slope = seg3.F15Que.lm$coefficients[2] * 
  seg3.F15Que$year + seg3.F15Que.lm$coefficients[1] # Eq
seg4.F15Que$slope = seg4.F15Que.lm$coefficients[2] * 
  seg4.F15Que$year + seg4.F15Que.lm$coefficients[1] # Eq
seg5.F15Que$slope = seg5.F15Que.lm$coefficients[2] * 
  seg5.F15Que$year + seg5.F15Que.lm$coefficients[1] # Eq
seg6.F15Que$slope = seg6.F15Que.lm$coefficients[2] * 
  seg6.F15Que$year + seg6.F15Que.lm$coefficients[1] # Eq
seg7.F15Que$slope = seg7.F15Que.lm$coefficients[2] * 
  seg7.F15Que$year + seg7.F15Que.lm$coefficients[1] # Eq

F15Que.p = ggplot(data = rwi.F15Que) +
  #geom_point(aes(x = slm.F15Que.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[1,2], xend = slm.F15Que.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Que.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[2,2], xend = slm.F15Que.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Que.ci[3,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[3,2], xend = slm.F15Que.ci[3,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Que.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[4,2], xend = slm.F15Que.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[4,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Que.ci[5,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[5,2], xend = slm.F15Que.ci[5,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[5,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Que.ci[6,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F15Que.ci[6,2], xend = slm.F15Que.ci[6,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F15Que.ci[6,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F15Que$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F15Que, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.F15Que, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.F15Que, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.F15Que, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg5.F15Que, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg6.F15Que, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg7.F15Que, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("02Que (0.28*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
F15Que.p

# F15LTS
lts.F15_1 = add_rownames(lts.F15, var = "year")
lts.F15_1$year = as.numeric(lts.F15_1$year) # Years column in chr by default

lm.ltsF15 = lm(Avg_CC_Median ~ year, data = lts.F15_1)

ltsF15.p = ggplot(data = lts.F15_1) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  geom_line(aes(x = year, y = lm.ltsF15$fitted.values), lwd = 1.25, col = "red3") + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsF15.p

# F21Dec
rwi.F21Dec = add_rownames(rwl.F21Dec.mne.crn, var = "year")
rwi.F21Dec$year = as.numeric(rwi.F21Dec$year) # Years column in chr by default

lm.F21Dec = lm(xxxstd ~ year, data = rwi.F21Dec)

slm.F21Dec = segmented.lm(lm.F21Dec, psi = c(1927,1968,1981,2001), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F21Dec.ci = confint(slm.F21Dec, digits = 4) # 1.96 x SE at 95% CI

brks.F21Dec = slm.F21Dec$psi
brk1.F21Dec = round(brks.F21Dec[1,2])
brk2.F21Dec = round(brks.F21Dec[2,2])
brk3.F21Dec = round(brks.F21Dec[3,2])
brk4.F21Dec = round(brks.F21Dec[4,2])

seg1.F21Dec = subset(rwi.F21Dec, year <= brk1.F21Dec)
seg2.F21Dec = subset(rwi.F21Dec, year >= brk1.F21Dec & year <= brk2.F21Dec)
seg3.F21Dec = subset(rwi.F21Dec, year >= brk2.F21Dec & year <= brk3.F21Dec)
seg4.F21Dec = subset(rwi.F21Dec, year >= brk3.F21Dec & year <= brk4.F21Dec)
seg5.F21Dec = subset(rwi.F21Dec, year >= brk4.F21Dec)

seg1.F21Dec.lm = zyp.sen(xxxstd ~ year, seg1.F21Dec) 
seg2.F21Dec.lm = zyp.sen(xxxstd ~ year, seg2.F21Dec) 
seg3.F21Dec.lm = zyp.sen(xxxstd ~ year, seg3.F21Dec)
seg4.F21Dec.lm = zyp.sen(xxxstd ~ year, seg4.F21Dec)
seg5.F21Dec.lm = zyp.sen(xxxstd ~ year, seg5.F21Dec)

seg1.F21Dec$slope = seg1.F21Dec.lm$coefficients[2] * 
  seg1.F21Dec$year + seg1.F21Dec.lm$coefficients[1] # Eq
seg2.F21Dec$slope = seg2.F21Dec.lm$coefficients[2] * 
  seg2.F21Dec$year + seg2.F21Dec.lm$coefficients[1] # Eq
seg3.F21Dec$slope = seg3.F21Dec.lm$coefficients[2] * 
  seg3.F21Dec$year + seg3.F21Dec.lm$coefficients[1] # Eq
seg4.F21Dec$slope = seg4.F21Dec.lm$coefficients[2] * 
  seg4.F21Dec$year + seg4.F21Dec.lm$coefficients[1] # Eq
seg5.F21Dec$slope = seg5.F21Dec.lm$coefficients[2] * 
  seg5.F21Dec$year + seg5.F21Dec.lm$coefficients[1] # Eq

F21Dec.p = ggplot(data = rwi.F21Dec) +
  #geom_point(aes(x = slm.F21Dec.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F21Dec.ci[1,2], xend = slm.F21Dec.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F21Dec.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F21Dec.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F21Dec.ci[2,2], xend = slm.F21Dec.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F21Dec.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F21Dec.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F21Dec.ci[3,2], xend = slm.F21Dec.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F21Dec.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F21Dec.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F21Dec.ci[4,2], xend = slm.F21Dec.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F21Dec.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F21Dec$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F21Dec, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.F21Dec, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.F21Dec, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.F21Dec, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg5.F21Dec, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("06Dec (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
F21Dec.p

# F21LTS (Top Middle-Right 2)
lts.F21_1 = add_rownames(lts.F21, var = "year")
lts.F21_1$year = as.numeric(lts.F21_1$year) # Years column in chr by default

lm.ltsF21 = lm(Avg_CC_Median ~ year, data = lts.F21_1)

slm.ltsF21 = segmented.lm(lm.ltsF21, psi = c(2006), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF21.ci = confint(slm.ltsF21, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF21 = slm.ltsF21$psi
brk1.ltsF21 = round(brks.ltsF21[1,2])

seg1.ltsF21 = subset(lts.F21_1, year <= brk1.ltsF21)
seg2.ltsF21 = subset(lts.F21_1, year >= brk1.ltsF21)

seg1.ltsF21.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF21) 
seg2.ltsF21.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF21) 

seg1.ltsF21$slope = seg1.ltsF21.lm$coefficients[2] * 
  seg1.ltsF21$year + seg1.ltsF21.lm$coefficients[1] # Eq
seg2.ltsF21$slope = seg2.ltsF21.lm$coefficients[2] * 
  seg2.ltsF21$year + seg2.ltsF21.lm$coefficients[1] # Eq

ltsF21.p = ggplot(data = lts.F21_1) +
  #geom_point(aes(x = slm.ltsF21.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF21.ci[1,2], xend = slm.ltsF21.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF21.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF21$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF21, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsF21, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsF21.p

# F23Ace
rwi.F23Ace = add_rownames(rwl.F23Ace.mne.crn, var = "year")
rwi.F23Ace$year = as.numeric(rwi.F23Ace$year) # Years column in chr by default

lm.F23Ace = lm(xxxstd ~ year, data = rwi.F23Ace)

slm.F23Ace = segmented.lm(lm.F23Ace, psi = c(1904,1918,1972,1996), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F23Ace.ci = confint(slm.F23Ace, digits = 4) # 1.96 x SE at 95% CI

brks.F23Ace = slm.F23Ace$psi
brk1.F23Ace = round(brks.F23Ace[1,2])
brk2.F23Ace = round(brks.F23Ace[2,2])
brk3.F23Ace = round(brks.F23Ace[3,2])
brk4.F23Ace = round(brks.F23Ace[4,2])

seg1.F23Ace = subset(rwi.F23Ace, year <= brk1.F23Ace)
seg2.F23Ace = subset(rwi.F23Ace, year >= brk1.F23Ace & year <= brk2.F23Ace)
seg3.F23Ace = subset(rwi.F23Ace, year >= brk2.F23Ace & year <= brk3.F23Ace)
seg4.F23Ace = subset(rwi.F23Ace, year >= brk3.F23Ace & year <= brk4.F23Ace)
seg5.F23Ace = subset(rwi.F23Ace, year >= brk4.F23Ace)

seg1.F23Ace.lm = zyp.sen(xxxstd ~ year, seg1.F23Ace) 
seg2.F23Ace.lm = zyp.sen(xxxstd ~ year, seg2.F23Ace) 
seg3.F23Ace.lm = zyp.sen(xxxstd ~ year, seg3.F23Ace)
seg4.F23Ace.lm = zyp.sen(xxxstd ~ year, seg4.F23Ace)
seg5.F23Ace.lm = zyp.sen(xxxstd ~ year, seg5.F23Ace)

seg1.F23Ace$slope = seg1.F23Ace.lm$coefficients[2] * 
  seg1.F23Ace$year + seg1.F23Ace.lm$coefficients[1] # Eq
seg2.F23Ace$slope = seg2.F23Ace.lm$coefficients[2] * 
  seg2.F23Ace$year + seg2.F23Ace.lm$coefficients[1] # Eq
seg3.F23Ace$slope = seg3.F23Ace.lm$coefficients[2] * 
  seg3.F23Ace$year + seg3.F23Ace.lm$coefficients[1] # Eq
seg4.F23Ace$slope = seg4.F23Ace.lm$coefficients[2] * 
  seg4.F23Ace$year + seg4.F23Ace.lm$coefficients[1] # Eq
seg5.F23Ace$slope = seg5.F23Ace.lm$coefficients[2] * 
  seg5.F23Ace$year + seg5.F23Ace.lm$coefficients[1] # Eq

F23Ace.p = ggplot(data = rwi.F23Ace) +
  #geom_point(aes(x = slm.F23Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F23Ace.ci[1,2], xend = slm.F23Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F23Ace.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F23Ace.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F23Ace.ci[2,2], xend = slm.F23Ace.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F23Ace.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F23Ace.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F23Ace.ci[3,2], xend = slm.F23Ace.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F23Ace.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F23Ace.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F23Ace.ci[4,2], xend = slm.F23Ace.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F23Ace.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F23Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F23Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F23Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.F23Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.F23Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg5.F23Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  ggtitle("05Ace (-0.21)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F23Ace.p

# F23LTS
lts.F23_1 = add_rownames(lts.F23, var = "year")
lts.F23_1$year = as.numeric(lts.F23_1$year) # Years column in chr by default

lm.ltsF23 = lm(Avg_CC_Median ~ year, data = lts.F23_1)

slm.ltsF23 = segmented.lm(lm.ltsF23, psi = c(2011), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF23.ci = confint(slm.ltsF23, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF23 = slm.ltsF23$psi
brk1.ltsF23 = round(brks.ltsF23[1,2])

seg1.ltsF23 = subset(lts.F23_1, year <= brk1.ltsF23)
seg2.ltsF23 = subset(lts.F23_1, year >= brk1.ltsF23)

seg1.ltsF23.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF23) 
seg2.ltsF23.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF23) 

seg1.ltsF23$slope = seg1.ltsF23.lm$coefficients[2] * 
  seg1.ltsF23$year + seg1.ltsF23.lm$coefficients[1] # Eq
seg2.ltsF23$slope = seg2.ltsF23.lm$coefficients[2] * 
  seg2.ltsF23$year + seg2.ltsF23.lm$coefficients[1] # Eq

ltsF23.p = ggplot(data = lts.F23_1) +
  #geom_point(aes(x = slm.ltsF23.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF23.ci[1,2], xend = slm.ltsF23.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF23.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF23$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF23, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsF23, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ltsF23.p

# F25Ace
rwi.F25Ace = add_rownames(rwl.F25Ace.mne.crn, var = "year")
rwi.F25Ace$year = as.numeric(rwi.F25Ace$year) # Years column in chr by default

lm.F25Ace = lm(xxxstd ~ year, data = rwi.F25Ace)

slm.F25Ace = segmented.lm(lm.F25Ace, psi = c(1951), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F25Ace.ci = confint(slm.F25Ace, digits = 4) # 1.96 x SE at 95% CI

brks.F25Ace = slm.F25Ace$psi
brk1.F25Ace = round(brks.F25Ace[1,2])

seg1.F25Ace = subset(rwi.F25Ace, year <= brk1.F25Ace)
seg2.F25Ace = subset(rwi.F25Ace, year >= brk1.F25Ace)

seg1.F25Ace.lm = zyp.sen(xxxstd ~ year, seg1.F25Ace) # Plot T-S slope using zyp method
seg2.F25Ace.lm = zyp.sen(xxxstd ~ year, seg2.F25Ace)

seg1.F25Ace$slope = seg1.F25Ace.lm$coefficients[2] * 
  seg1.F25Ace$year + seg1.F25Ace.lm$coefficients[1] # Eq
seg2.F25Ace$slope = seg2.F25Ace.lm$coefficients[2] * 
  seg2.F25Ace$year + seg2.F25Ace.lm$coefficients[1] # Eq

F25Ace.p = ggplot(data = rwi.F25Ace) +
  #geom_point(aes(x = slm.F25Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F25Ace.ci[1,2], xend = slm.F25Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F25Ace.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F25Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F25Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F25Ace, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("11Ace (-0.22)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
F25Ace.p

# F25LTS
lts.F25_1 = add_rownames(lts.F25, var = "year")
lts.F25_1$year = as.numeric(lts.F25_1$year) # Years column in chr by default

lm.ltsF25 = lm(Avg_CC_Median ~ year, data = lts.F25_1)

slm.ltsF25 = segmented.lm(lm.ltsF25, psi = c(1978,1986,1991,1995), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF25.ci = confint(slm.ltsF25, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF25 = slm.ltsF25$psi
brk1.ltsF25 = round(brks.ltsF25[1,2])
brk2.ltsF25 = round(brks.ltsF25[2,2])
brk3.ltsF25 = round(brks.ltsF25[3,2])
brk4.ltsF25 = round(brks.ltsF25[4,2])

seg1.ltsF25 = subset(lts.F25_1, year <= brk1.ltsF25)
seg2.ltsF25 = subset(lts.F25_1, year >= brk1.ltsF25 & year <= brk2.ltsF25)
seg3.ltsF25 = subset(lts.F25_1, year >= brk2.ltsF25 & year <= brk3.ltsF25)
seg4.ltsF25 = subset(lts.F25_1, year >= brk3.ltsF25 & year <= brk4.ltsF25)
seg5.ltsF25 = subset(lts.F25_1, year >= brk4.ltsF25)

seg1.ltsF25.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF25) 
seg2.ltsF25.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF25) 
seg3.ltsF25.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsF25)
seg4.ltsF25.lm = zyp.sen(Avg_CC_Median ~ year, seg4.ltsF25)
seg5.ltsF25.lm = zyp.sen(Avg_CC_Median ~ year, seg5.ltsF25)

seg1.ltsF25$slope = seg1.ltsF25.lm$coefficients[2] * 
  seg1.ltsF25$year + seg1.ltsF25.lm$coefficients[1] # Eq
seg2.ltsF25$slope = seg2.ltsF25.lm$coefficients[2] * 
  seg2.ltsF25$year + seg2.ltsF25.lm$coefficients[1] # Eq
seg3.ltsF25$slope = seg3.ltsF25.lm$coefficients[2] * 
  seg3.ltsF25$year + seg3.ltsF25.lm$coefficients[1] # Eq
seg4.ltsF25$slope = seg4.ltsF25.lm$coefficients[2] * 
  seg4.ltsF25$year + seg4.ltsF25.lm$coefficients[1] # Eq
seg5.ltsF25$slope = seg5.ltsF25.lm$coefficients[2] * 
  seg5.ltsF25$year + seg5.ltsF25.lm$coefficients[1] # Eq

ltsF25.p = ggplot(data = lts.F25_1) +
  #geom_point(aes(x = slm.ltsF25.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF25.ci[1,2], xend = slm.ltsF25.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF25.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF25.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF25.ci[2,2], xend = slm.ltsF25.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF25.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF25.ci[3,1], y = 73.5), col = "gray48") +
  geom_segment(aes(x = slm.ltsF25.ci[3,2], xend = slm.ltsF25.ci[3,3], y = 73.5, 
                   yend = 73.5), col = "gray48") +
  geom_vline(xintercept = slm.ltsF25.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF25.ci[4,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF25.ci[4,2], xend = slm.ltsF25.ci[4,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF25.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF25$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF25, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.ltsF25, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsF25, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.ltsF25, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg5.ltsF25, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "%CC", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsF25.p

# F30Bet
rwi.F30Bet = add_rownames(rwl.F30Bet.mne.crn, var = "year")
rwi.F30Bet$year = as.numeric(rwi.F30Bet$year) # Years column in chr by default

lm.F30Bet = lm(xxxstd ~ year, data = rwi.F30Bet)

slm.F30Bet = segmented.lm(lm.F30Bet, psi = c(1952,1962,1978), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F30Bet.ci = confint(slm.F30Bet, digits = 4) # 1.96 x SE at 95% CI

brks.F30Bet = slm.F30Bet$psi
brk1.F30Bet = round(brks.F30Bet[1,2])
brk2.F30Bet = round(brks.F30Bet[2,2])
brk3.F30Bet = round(brks.F30Bet[3,2])

seg1.F30Bet = subset(rwi.F30Bet, year <= brk1.F30Bet)
seg2.F30Bet = subset(rwi.F30Bet, year >= brk1.F30Bet & year <= brk2.F30Bet)
seg3.F30Bet = subset(rwi.F30Bet, year >= brk2.F30Bet & year <= brk3.F30Bet)
seg4.F30Bet = subset(rwi.F30Bet, year >= brk3.F30Bet)

seg1.F30Bet.lm = zyp.sen(xxxstd ~ year, seg1.F30Bet) 
seg2.F30Bet.lm = zyp.sen(xxxstd ~ year, seg2.F30Bet) 
seg3.F30Bet.lm = zyp.sen(xxxstd ~ year, seg3.F30Bet)
seg4.F30Bet.lm = zyp.sen(xxxstd ~ year, seg4.F30Bet)

seg1.F30Bet$slope = seg1.F30Bet.lm$coefficients[2] * 
  seg1.F30Bet$year + seg1.F30Bet.lm$coefficients[1] # Eq
seg2.F30Bet$slope = seg2.F30Bet.lm$coefficients[2] * 
  seg2.F30Bet$year + seg2.F30Bet.lm$coefficients[1] # Eq
seg3.F30Bet$slope = seg3.F30Bet.lm$coefficients[2] * 
  seg3.F30Bet$year + seg3.F30Bet.lm$coefficients[1] # Eq
seg4.F30Bet$slope = seg4.F30Bet.lm$coefficients[2] * 
  seg4.F30Bet$year + seg4.F30Bet.lm$coefficients[1] # Eq

F30Bet.p = ggplot(data = rwi.F30Bet) +
  #geom_point(aes(x = slm.F30Bet.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F30Bet.ci[1,2], xend = slm.F30Bet.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F30Bet.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F30Bet.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F30Bet.ci[2,2], xend = slm.F30Bet.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F30Bet.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F30Bet.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F30Bet.ci[3,2], xend = slm.F30Bet.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F30Bet.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F30Bet$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F30Bet, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F30Bet, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.F30Bet, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg4.F30Bet, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("15Bet (-0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
F30Bet.p

# F30LTS
lts.F30_1 = add_rownames(lts.F30, var = "year")
lts.F30_1$year = as.numeric(lts.F30_1$year) # Years column in chr by default

lm.ltsF30 = lm(Avg_CC_Median ~ year, data = lts.F30_1)

slm.ltsF30 = segmented.lm(lm.ltsF30, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF30.ci = confint(slm.ltsF30, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF30 = slm.ltsF30$psi
brk1.ltsF30 = round(brks.ltsF30[1,2])

seg1.ltsF30 = subset(lts.F30_1, year <= brk1.ltsF30)
seg2.ltsF30 = subset(lts.F30_1, year >= brk1.ltsF30)

seg1.ltsF30.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF30) 
seg2.ltsF30.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF30) 

seg1.ltsF30$slope = seg1.ltsF30.lm$coefficients[2] * 
  seg1.ltsF30$year + seg1.ltsF30.lm$coefficients[1] # Eq
seg2.ltsF30$slope = seg2.ltsF30.lm$coefficients[2] * 
  seg2.ltsF30$year + seg2.ltsF30.lm$coefficients[1] # Eq

ltsF30.p = ggplot(data = lts.F30_1) +
  #geom_point(aes(x = slm.ltsF30.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF30.ci[1,2], xend = slm.ltsF30.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF30.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF30$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF30, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsF30, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsF30.p

# F33Pic (Middle-Top Middle-Right 1)
rwi.F33Pic = add_rownames(rwl.F33Pic.mne.crn, var = "year")
rwi.F33Pic$year = as.numeric(rwi.F33Pic$year) # Years column in chr by default

lm.F33Pic = lm(xxxstd ~ year, data = rwi.F33Pic)

slm.F33Pic = segmented.lm(lm.F33Pic, psi = c(1968,1974,2006,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 0

slm.F33Pic.ci = confint(slm.F33Pic, digits = 4) # 1.96 x SE at 95% CI

brks.F33Pic = slm.F33Pic$psi
brk1.F33Pic = round(brks.F33Pic[1,2])
brk2.F33Pic = round(brks.F33Pic[2,2])
brk3.F33Pic = round(brks.F33Pic[3,2])
brk4.F33Pic = round(brks.F33Pic[4,2])

seg1.F33Pic = subset(rwi.F33Pic, year <= brk1.F33Pic)
seg2.F33Pic = subset(rwi.F33Pic, year >= brk1.F33Pic & year <= brk2.F33Pic)
seg3.F33Pic = subset(rwi.F33Pic, year >= brk2.F33Pic & year <= brk3.F33Pic)
seg4.F33Pic = subset(rwi.F33Pic, year >= brk3.F33Pic & year <= brk4.F33Pic)
seg5.F33Pic = subset(rwi.F33Pic, year >= brk4.F33Pic)

seg1.F33Pic.lm = zyp.sen(xxxstd ~ year, seg1.F33Pic) 
seg2.F33Pic.lm = zyp.sen(xxxstd ~ year, seg2.F33Pic) 
seg3.F33Pic.lm = zyp.sen(xxxstd ~ year, seg3.F33Pic)
seg4.F33Pic.lm = zyp.sen(xxxstd ~ year, seg4.F33Pic)
seg5.F33Pic.lm = zyp.sen(xxxstd ~ year, seg5.F33Pic)

seg1.F33Pic$slope = seg1.F33Pic.lm$coefficients[2] * 
  seg1.F33Pic$year + seg1.F33Pic.lm$coefficients[1] # Eq
seg2.F33Pic$slope = seg2.F33Pic.lm$coefficients[2] * 
  seg2.F33Pic$year + seg2.F33Pic.lm$coefficients[1] # Eq
seg3.F33Pic$slope = seg3.F33Pic.lm$coefficients[2] * 
  seg3.F33Pic$year + seg3.F33Pic.lm$coefficients[1] # Eq
seg4.F33Pic$slope = seg4.F33Pic.lm$coefficients[2] * 
  seg4.F33Pic$year + seg4.F33Pic.lm$coefficients[1] # Eq
seg5.F33Pic$slope = seg5.F33Pic.lm$coefficients[2] * 
  seg5.F33Pic$year + seg5.F33Pic.lm$coefficients[1] # Eq

F33Pic.p = ggplot(data = rwi.F33Pic) +
  #geom_point(aes(x = slm.F33Pic.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F33Pic.ci[1,2], xend = slm.F33Pic.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F33Pic.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F33Pic.ci[2,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F33Pic.ci[2,2], xend = slm.F33Pic.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F33Pic.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F33Pic.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F33Pic.ci[3,2], xend = slm.F33Pic.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F33Pic.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F33Pic.ci[4,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F33Pic.ci[4,2], xend = slm.F33Pic.ci[4,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F33Pic.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F33Pic$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F33Pic, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F33Pic, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.F33Pic, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg4.F33Pic, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg5.F33Pic, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("13Pic (0.31*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F33Pic.p

# F33LTS
lts.F33_1 = add_rownames(lts.F33, var = "year")
lts.F33_1$year = as.numeric(lts.F33_1$year) # Years column in chr by default

lm.ltsF33 = lm(Avg_CC_Median ~ year, data = lts.F33_1)

slm.ltsF33 = segmented.lm(lm.ltsF33, psi = c(1980,1987,2000,2004), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF33.ci = confint(slm.ltsF33, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF33 = slm.ltsF33$psi
brk1.ltsF33 = round(brks.ltsF33[1,2])
brk2.ltsF33 = round(brks.ltsF33[2,2])
brk3.ltsF33 = round(brks.ltsF33[3,2])
brk4.ltsF33 = round(brks.ltsF33[4,2])

seg1.ltsF33 = subset(lts.F33_1, year <= brk1.ltsF33)
seg2.ltsF33 = subset(lts.F33_1, year >= brk1.ltsF33 & year <= brk2.ltsF33)
seg3.ltsF33 = subset(lts.F33_1, year >= brk2.ltsF33 & year <= brk3.ltsF33)
seg4.ltsF33 = subset(lts.F33_1, year >= brk3.ltsF33 & year <= brk4.ltsF33)
seg5.ltsF33 = subset(lts.F33_1, year >= brk4.ltsF33)

seg1.ltsF33.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF33) 
seg2.ltsF33.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF33) 
seg3.ltsF33.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsF33)
seg4.ltsF33.lm = zyp.sen(Avg_CC_Median ~ year, seg4.ltsF33)
seg5.ltsF33.lm = zyp.sen(Avg_CC_Median ~ year, seg5.ltsF33)

seg1.ltsF33$slope = seg1.ltsF33.lm$coefficients[2] * 
  seg1.ltsF33$year + seg1.ltsF33.lm$coefficients[1] # Eq
seg2.ltsF33$slope = seg2.ltsF33.lm$coefficients[2] * 
  seg2.ltsF33$year + seg2.ltsF33.lm$coefficients[1] # Eq
seg3.ltsF33$slope = seg3.ltsF33.lm$coefficients[2] * 
  seg3.ltsF33$year + seg3.ltsF33.lm$coefficients[1] # Eq
seg4.ltsF33$slope = seg4.ltsF33.lm$coefficients[2] * 
  seg4.ltsF33$year + seg4.ltsF33.lm$coefficients[1] # Eq
seg5.ltsF33$slope = seg5.ltsF33.lm$coefficients[2] * 
  seg5.ltsF33$year + seg5.ltsF33.lm$coefficients[1] # Eq

ltsF33.p = ggplot(data = lts.F33_1) +
  #geom_point(aes(x = slm.ltsF33.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF33.ci[1,2], xend = slm.ltsF33.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF33.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF33.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF33.ci[2,2], xend = slm.ltsF33.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF33.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF33.ci[3,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF33.ci[3,2], xend = slm.ltsF33.ci[3,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF33.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsF33.ci[4,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF33.ci[4,2], xend = slm.ltsF33.ci[4,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF33.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF33$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF33, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.ltsF33, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsF33, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.ltsF33, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg5.ltsF33, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ltsF33.p

# M01Pop
rwi.M01Pop = add_rownames(rwl.M01Pop.mne.crn, var = "year")
rwi.M01Pop$year = as.numeric(rwi.M01Pop$year) # Years column in chr by default

lm.M01Pop = lm(xxxstd ~ year, data = rwi.M01Pop)

slm.M01Pop = segmented.lm(lm.M01Pop, psi = c(1960,1965,1983,1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M01Pop.ci = confint(slm.M01Pop, digits = 4) # 1.96 x SE at 95% CI

brks.M01Pop = slm.M01Pop$psi
brk1.M01Pop = round(brks.M01Pop[1,2])
brk2.M01Pop = round(brks.M01Pop[2,2])
brk3.M01Pop = round(brks.M01Pop[3,2])
brk4.M01Pop = round(brks.M01Pop[4,2])

seg1.M01Pop = subset(rwi.M01Pop, year <= brk1.M01Pop)
seg2.M01Pop = subset(rwi.M01Pop, year >= brk1.M01Pop & year <= brk2.M01Pop)
seg3.M01Pop = subset(rwi.M01Pop, year >= brk2.M01Pop & year <= brk3.M01Pop)
seg4.M01Pop = subset(rwi.M01Pop, year >= brk3.M01Pop & year <= brk4.M01Pop)
seg5.M01Pop = subset(rwi.M01Pop, year >= brk4.M01Pop)

seg1.M01Pop.lm = zyp.sen(xxxstd ~ year, seg1.M01Pop) 
seg2.M01Pop.lm = zyp.sen(xxxstd ~ year, seg2.M01Pop) 
seg3.M01Pop.lm = zyp.sen(xxxstd ~ year, seg3.M01Pop)
seg4.M01Pop.lm = zyp.sen(xxxstd ~ year, seg4.M01Pop)
seg5.M01Pop.lm = zyp.sen(xxxstd ~ year, seg5.M01Pop)

seg1.M01Pop$slope = seg1.M01Pop.lm$coefficients[2] * 
  seg1.M01Pop$year + seg1.M01Pop.lm$coefficients[1] # Eq
seg2.M01Pop$slope = seg2.M01Pop.lm$coefficients[2] * 
  seg2.M01Pop$year + seg2.M01Pop.lm$coefficients[1] # Eq
seg3.M01Pop$slope = seg3.M01Pop.lm$coefficients[2] * 
  seg3.M01Pop$year + seg3.M01Pop.lm$coefficients[1] # Eq
seg4.M01Pop$slope = seg4.M01Pop.lm$coefficients[2] * 
  seg4.M01Pop$year + seg4.M01Pop.lm$coefficients[1] # Eq
seg5.M01Pop$slope = seg5.M01Pop.lm$coefficients[2] * 
  seg5.M01Pop$year + seg5.M01Pop.lm$coefficients[1] # Eq

M01Pop.p = ggplot(data = rwi.M01Pop) +
  #geom_point(aes(x = slm.M01Pop.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M01Pop.ci[1,2], xend = slm.M01Pop.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M01Pop.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M01Pop.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M01Pop.ci[2,2], xend = slm.M01Pop.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M01Pop.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M01Pop.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M01Pop.ci[3,2], xend = slm.M01Pop.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M01Pop.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M01Pop.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M01Pop.ci[4,2], xend = slm.M01Pop.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M01Pop.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M01Pop$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M01Pop, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.M01Pop, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.M01Pop, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.M01Pop, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.M01Pop, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("12Pop (0.57**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M01Pop.p

# M01LTS
lts.M01_1 = add_rownames(lts.M01, var = "year")
lts.M01_1$year = as.numeric(lts.M01_1$year) # Years column in chr by default

lm.ltsM01 = lm(Avg_CC_Median ~ year, data = lts.M01_1)

slm.ltsM01 = segmented.lm(lm.ltsM01, psi = c(1985,1997), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM01.ci = confint(slm.ltsM01, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM01 = slm.ltsM01$psi
brk1.ltsM01 = round(brks.ltsM01[1,2])
brk2.ltsM01 = round(brks.ltsM01[2,2])

seg1.ltsM01 = subset(lts.M01_1, year <= brk1.ltsM01)
seg2.ltsM01 = subset(lts.M01_1, year >= brk1.ltsM01 & year <= brk2.ltsM01)
seg3.ltsM01 = subset(lts.M01_1, year >= brk2.ltsM01)

seg1.ltsM01.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM01) 
seg2.ltsM01.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM01) 
seg3.ltsM01.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM01)

seg1.ltsM01$slope = seg1.ltsM01.lm$coefficients[2] * 
  seg1.ltsM01$year + seg1.ltsM01.lm$coefficients[1] # Eq
seg2.ltsM01$slope = seg2.ltsM01.lm$coefficients[2] * 
  seg2.ltsM01$year + seg2.ltsM01.lm$coefficients[1] # Eq
seg3.ltsM01$slope = seg3.ltsM01.lm$coefficients[2] * 
  seg3.ltsM01$year + seg3.ltsM01.lm$coefficients[1] # Eq

ltsM01.p = ggplot(data = lts.M01_1) +
  #geom_point(aes(x = slm.ltsM01.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM01.ci[1,2], xend = slm.ltsM01.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM01.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM01.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM01.ci[2,2], xend = slm.ltsM01.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM01.ci[2,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM01$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM01, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.ltsM01, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsM01, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"))
ltsM01.p

# M05Thu (Middle-Bottom Left 1)
rwi.M05Thu = add_rownames(rwl.M05Thu.mne.crn, var = "year")
rwi.M05Thu$year = as.numeric(rwi.M05Thu$year) # Years column in chr by default

lm.M05Thu = lm(xxxstd ~ year, data = rwi.M05Thu)

slm.M05Thu = segmented.lm(lm.M05Thu, psi = c(1942,1948,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M05Thu.ci = confint(slm.M05Thu, digits = 4) # 1.96 x SE at 95% CI

brks.M05Thu = slm.M05Thu$psi
brk1.M05Thu = round(brks.M05Thu[1,2])
brk2.M05Thu = round(brks.M05Thu[2,2])
brk3.M05Thu = round(brks.M05Thu[3,2])

seg1.M05Thu = subset(rwi.M05Thu, year <= brk1.M05Thu)
seg2.M05Thu = subset(rwi.M05Thu, year >= brk1.M05Thu & year <= brk2.M05Thu)
seg3.M05Thu = subset(rwi.M05Thu, year >= brk2.M05Thu & year <= brk3.M05Thu)
seg4.M05Thu = subset(rwi.M05Thu, year >= brk3.M05Thu)

seg1.M05Thu.lm = zyp.sen(xxxstd ~ year, seg1.M05Thu) 
seg2.M05Thu.lm = zyp.sen(xxxstd ~ year, seg2.M05Thu) 
seg3.M05Thu.lm = zyp.sen(xxxstd ~ year, seg3.M05Thu)
seg4.M05Thu.lm = zyp.sen(xxxstd ~ year, seg4.M05Thu)

seg1.M05Thu$slope = seg1.M05Thu.lm$coefficients[2] * 
  seg1.M05Thu$year + seg1.M05Thu.lm$coefficients[1] # Eq
seg2.M05Thu$slope = seg2.M05Thu.lm$coefficients[2] * 
  seg2.M05Thu$year + seg2.M05Thu.lm$coefficients[1] # Eq
seg3.M05Thu$slope = seg3.M05Thu.lm$coefficients[2] * 
  seg3.M05Thu$year + seg3.M05Thu.lm$coefficients[1] # Eq
seg4.M05Thu$slope = seg4.M05Thu.lm$coefficients[2] * 
  seg4.M05Thu$year + seg4.M05Thu.lm$coefficients[1] # Eq

M05Thu.p = ggplot(data = rwi.M05Thu) +
  #geom_point(aes(x = slm.M05Thu.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M05Thu.ci[1,2], xend = slm.M05Thu.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M05Thu.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M05Thu.ci[2,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.M05Thu.ci[2,2], xend = slm.M05Thu.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M05Thu.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M05Thu.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M05Thu.ci[3,2], xend = slm.M05Thu.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M05Thu.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M05Thu$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M05Thu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.M05Thu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.M05Thu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.M05Thu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("07Thu (0.37*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M05Thu.p

# M05LTS (Middle-Bottom Left 2)
lts.M05_1 = add_rownames(lts.M05, var = "year")
lts.M05_1$year = as.numeric(lts.M05_1$year) # Years column in chr by default

lm.ltsM05 = lm(Avg_CC_Median ~ year, data = lts.M05_1)

slm.ltsM05 = segmented.lm(lm.ltsM05, psi = c(1986,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM05.ci = confint(slm.ltsM05, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM05 = slm.ltsM05$psi
brk1.ltsM05 = round(brks.ltsM05[1,2])
brk2.ltsM05 = round(brks.ltsM05[2,2])

seg1.ltsM05 = subset(lts.M05_1, year <= brk1.ltsM05)
seg2.ltsM05 = subset(lts.M05_1, year >= brk1.ltsM05 & year <= brk2.ltsM05)
seg3.ltsM05 = subset(lts.M05_1, year >= brk2.ltsM05)

seg1.ltsM05.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM05) 
seg2.ltsM05.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM05) 
seg3.ltsM05.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM05)

seg1.ltsM05$slope = seg1.ltsM05.lm$coefficients[2] * 
  seg1.ltsM05$year + seg1.ltsM05.lm$coefficients[1] # Eq
seg2.ltsM05$slope = seg2.ltsM05.lm$coefficients[2] * 
  seg2.ltsM05$year + seg2.ltsM05.lm$coefficients[1] # Eq
seg3.ltsM05$slope = seg3.ltsM05.lm$coefficients[2] * 
  seg3.ltsM05$year + seg3.ltsM05.lm$coefficients[1] # Eq

ltsM05.p = ggplot(data = lts.M05_1) +
  #geom_point(aes(x = slm.ltsM05.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM05.ci[1,2], xend = slm.ltsM05.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM05.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM05.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM05.ci[2,2], xend = slm.ltsM05.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM05.ci[2,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM05$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM05, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM05, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsM05, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "%CC", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM05.p

# M06Ace
rwi.M06Ace = add_rownames(rwl.M06Ace.mne.crn, var = "year")
rwi.M06Ace$year = as.numeric(rwi.M06Ace$year) # Years column in chr by default

lm.M06Ace = lm(xxxstd ~ year, data = rwi.M06Ace)

slm.M06Ace = segmented.lm(lm.M06Ace, psi = c(1911,1927,1957,1961,1967,2004), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 50

slm.M06Ace.ci = confint(slm.M06Ace, digits = 4) # 1.96 x SE at 95% CI

brks.M06Ace = slm.M06Ace$psi
brk1.M06Ace = round(brks.M06Ace[1,2])
brk2.M06Ace = round(brks.M06Ace[2,2])
brk3.M06Ace = round(brks.M06Ace[3,2])
brk4.M06Ace = round(brks.M06Ace[4,2])
brk5.M06Ace = round(brks.M06Ace[5,2])
brk6.M06Ace = round(brks.M06Ace[6,2])

seg1.M06Ace = subset(rwi.M06Ace, year <= brk1.M06Ace)
seg2.M06Ace = subset(rwi.M06Ace, year >= brk1.M06Ace & year <= brk2.M06Ace)
seg3.M06Ace = subset(rwi.M06Ace, year >= brk2.M06Ace & year <= brk3.M06Ace)
seg4.M06Ace = subset(rwi.M06Ace, year >= brk3.M06Ace & year <= brk4.M06Ace)
seg5.M06Ace = subset(rwi.M06Ace, year >= brk4.M06Ace & year <= brk5.M06Ace)
seg6.M06Ace = subset(rwi.M06Ace, year >= brk5.M06Ace & year <= brk6.M06Ace)
seg7.M06Ace = subset(rwi.M06Ace, year >= brk6.M06Ace)

seg1.M06Ace.lm = zyp.sen(xxxstd ~ year, seg1.M06Ace) 
seg2.M06Ace.lm = zyp.sen(xxxstd ~ year, seg2.M06Ace) 
seg3.M06Ace.lm = zyp.sen(xxxstd ~ year, seg3.M06Ace)
seg4.M06Ace.lm = zyp.sen(xxxstd ~ year, seg4.M06Ace)
seg5.M06Ace.lm = zyp.sen(xxxstd ~ year, seg5.M06Ace)
seg6.M06Ace.lm = zyp.sen(xxxstd ~ year, seg6.M06Ace)
seg7.M06Ace.lm = zyp.sen(xxxstd ~ year, seg7.M06Ace)

seg1.M06Ace$slope = seg1.M06Ace.lm$coefficients[2] * 
  seg1.M06Ace$year + seg1.M06Ace.lm$coefficients[1] # Eq
seg2.M06Ace$slope = seg2.M06Ace.lm$coefficients[2] * 
  seg2.M06Ace$year + seg2.M06Ace.lm$coefficients[1] # Eq
seg3.M06Ace$slope = seg3.M06Ace.lm$coefficients[2] * 
  seg3.M06Ace$year + seg3.M06Ace.lm$coefficients[1] # Eq
seg4.M06Ace$slope = seg4.M06Ace.lm$coefficients[2] * 
  seg4.M06Ace$year + seg4.M06Ace.lm$coefficients[1] # Eq
seg5.M06Ace$slope = seg5.M06Ace.lm$coefficients[2] * 
  seg5.M06Ace$year + seg5.M06Ace.lm$coefficients[1] # Eq
seg6.M06Ace$slope = seg6.M06Ace.lm$coefficients[2] * 
  seg6.M06Ace$year + seg6.M06Ace.lm$coefficients[1] # Eq
seg7.M06Ace$slope = seg7.M06Ace.lm$coefficients[2] * 
  seg7.M06Ace$year + seg7.M06Ace.lm$coefficients[1] # Eq

M06Ace.p = ggplot(data = rwi.M06Ace) +
  #geom_point(aes(x = slm.M06Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[1,2], xend = slm.M06Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M06Ace.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[2,2], xend = slm.M06Ace.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M06Ace.ci[3,1], y = 0.10), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[3,2], xend = slm.M06Ace.ci[3,3], y = 0.10, 
                   yend = 0.10), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M06Ace.ci[4,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[4,2], xend = slm.M06Ace.ci[4,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[4,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M06Ace.ci[5,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[5,2], xend = slm.M06Ace.ci[5,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[5,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M06Ace.ci[6,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M06Ace.ci[6,2], xend = slm.M06Ace.ci[6,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Ace.ci[6,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M06Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M06Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.M06Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.M06Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.M06Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.M06Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg6.M06Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg7.M06Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("03Ace (0.24*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M06Ace.p

# M06LTS
lts.M06_1 = add_rownames(lts.M06, var = "year")
lts.M06_1$year = as.numeric(lts.M06_1$year) # Years column in chr by default

lm.ltsM06 = lm(Avg_CC_Median ~ year, data = lts.M06_1)

slm.ltsM06 = segmented.lm(lm.ltsM06, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM06.ci = confint(slm.ltsM06, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM06 = slm.ltsM06$psi
brk1.ltsM06 = round(brks.ltsM06[1,2])

seg1.ltsM06 = subset(lts.M06_1, year <= brk1.ltsM06)
seg2.ltsM06 = subset(lts.M06_1, year >= brk1.ltsM06)

seg1.ltsM06.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM06) 
seg2.ltsM06.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM06) 

seg1.ltsM06$slope = seg1.ltsM06.lm$coefficients[2] * 
  seg1.ltsM06$year + seg1.ltsM06.lm$coefficients[1] # Eq
seg2.ltsM06$slope = seg2.ltsM06.lm$coefficients[2] * 
  seg2.ltsM06$year + seg2.ltsM06.lm$coefficients[1] # Eq

ltsM06.p = ggplot(data = lts.M06_1) +
  #geom_point(aes(x = slm.ltsM06.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM06.ci[1,2], xend = slm.ltsM06.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM06.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM06$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM06, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM06, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM06.p

# M07Tsu
rwi.M07Tsu = add_rownames(rwl.M07Tsu.mne.crn, var = "year")
rwi.M07Tsu$year = as.numeric(rwi.M07Tsu$year) # Years column in chr by default

lm.M07Tsu = lm(xxxstd ~ year, data = rwi.M07Tsu)

slm.M07Tsu = segmented.lm(lm.M07Tsu, psi = c(1903,1910,1923,1930,1960,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 0)) # n.boot 50

slm.M07Tsu.ci = confint(slm.M07Tsu, digits = 4) # 1.96 x SE at 95% CI

brks.M07Tsu = slm.M07Tsu$psi
brk1.M07Tsu = round(brks.M07Tsu[1,2])
brk2.M07Tsu = round(brks.M07Tsu[2,2])
brk3.M07Tsu = round(brks.M07Tsu[3,2])
brk4.M07Tsu = round(brks.M07Tsu[4,2])
brk5.M07Tsu = round(brks.M07Tsu[5,2])
brk6.M07Tsu = round(brks.M07Tsu[6,2])

seg1.M07Tsu = subset(rwi.M07Tsu, year <= brk1.M07Tsu)
seg2.M07Tsu = subset(rwi.M07Tsu, year >= brk1.M07Tsu & year <= brk2.M07Tsu)
seg3.M07Tsu = subset(rwi.M07Tsu, year >= brk2.M07Tsu & year <= brk3.M07Tsu)
seg4.M07Tsu = subset(rwi.M07Tsu, year >= brk3.M07Tsu & year <= brk4.M07Tsu)
seg5.M07Tsu = subset(rwi.M07Tsu, year >= brk4.M07Tsu & year <= brk5.M07Tsu)
seg6.M07Tsu = subset(rwi.M07Tsu, year >= brk5.M07Tsu & year <= brk6.M07Tsu)
seg7.M07Tsu = subset(rwi.M07Tsu, year >= brk6.M07Tsu)

seg1.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg1.M07Tsu) 
seg2.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg2.M07Tsu) 
seg3.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg3.M07Tsu)
seg4.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg4.M07Tsu)
seg5.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg5.M07Tsu)
seg6.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg6.M07Tsu)
seg7.M07Tsu.lm = zyp.sen(xxxstd ~ year, seg7.M07Tsu)

seg1.M07Tsu$slope = seg1.M07Tsu.lm$coefficients[2] * 
  seg1.M07Tsu$year + seg1.M07Tsu.lm$coefficients[1] # Eq
seg2.M07Tsu$slope = seg2.M07Tsu.lm$coefficients[2] * 
  seg2.M07Tsu$year + seg2.M07Tsu.lm$coefficients[1] # Eq
seg3.M07Tsu$slope = seg3.M07Tsu.lm$coefficients[2] * 
  seg3.M07Tsu$year + seg3.M07Tsu.lm$coefficients[1] # Eq
seg4.M07Tsu$slope = seg4.M07Tsu.lm$coefficients[2] * 
  seg4.M07Tsu$year + seg4.M07Tsu.lm$coefficients[1] # Eq
seg5.M07Tsu$slope = seg5.M07Tsu.lm$coefficients[2] * 
  seg5.M07Tsu$year + seg5.M07Tsu.lm$coefficients[1] # Eq
seg6.M07Tsu$slope = seg6.M07Tsu.lm$coefficients[2] * 
  seg6.M07Tsu$year + seg6.M07Tsu.lm$coefficients[1] # Eq
seg7.M07Tsu$slope = seg7.M07Tsu.lm$coefficients[2] * 
  seg7.M07Tsu$year + seg7.M07Tsu.lm$coefficients[1] # Eq

M07Tsu.p = ggplot(data = rwi.M07Tsu) +
  #geom_point(aes(x = slm.M07Tsu.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[1,2], xend = slm.M07Tsu.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M07Tsu.ci[2,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[2,2], xend = slm.M07Tsu.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M07Tsu.ci[3,1], y = 0.10), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[3,2], xend = slm.M07Tsu.ci[3,3], y = 0.10, 
                   yend = 0.10), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M07Tsu.ci[4,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[4,2], xend = slm.M07Tsu.ci[4,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[4,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M07Tsu.ci[5,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[5,2], xend = slm.M07Tsu.ci[5,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[5,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M07Tsu.ci[6,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M07Tsu.ci[6,2], xend = slm.M07Tsu.ci[6,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M07Tsu.ci[6,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M07Tsu$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M07Tsu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.M07Tsu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.M07Tsu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.M07Tsu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg5.M07Tsu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg6.M07Tsu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg7.M07Tsu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("04Tsu (0.16)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M07Tsu.p

# M07LTS (Middle-Top Middle-Right 2)
lts.M07_1 = add_rownames(lts.M07, var = "year")
lts.M07_1$year = as.numeric(lts.M07_1$year) # Years column in chr by default

lm.ltsM07 = lm(Avg_CC_Median ~ year, data = lts.M07_1)

slm.ltsM07 = segmented.lm(lm.ltsM07, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM07.ci = confint(slm.ltsM07, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM07 = slm.ltsM07$psi
brk1.ltsM07 = round(brks.ltsM07[1,2])

seg1.ltsM07 = subset(lts.M07_1, year <= brk1.ltsM07)
seg2.ltsM07 = subset(lts.M07_1, year >= brk1.ltsM07)

seg1.ltsM07.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM07) 
seg2.ltsM07.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM07) 

seg1.ltsM07$slope = seg1.ltsM07.lm$coefficients[2] * 
  seg1.ltsM07$year + seg1.ltsM07.lm$coefficients[1] # Eq
seg2.ltsM07$slope = seg2.ltsM07.lm$coefficients[2] * 
  seg2.ltsM07$year + seg2.ltsM07.lm$coefficients[1] # Eq

ltsM07.p = ggplot(data = lts.M07_1) +
  #geom_point(aes(x = slm.ltsM07.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM07.ci[1,2], xend = slm.ltsM07.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM07.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM07$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM07, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM07, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"))
ltsM07.p

# M13Tsu
rwi.M13Tsu = add_rownames(rwl.M13Tsu.mne.crn, var = "year")
rwi.M13Tsu$year = as.numeric(rwi.M13Tsu$year) # Years column in chr by default

lm.M13Tsu = lm(xxxstd ~ year, data = rwi.M13Tsu)

slm.M13Tsu = segmented.lm(lm.M13Tsu, psi = c(1913,1926,1988,2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M13Tsu.ci = confint(slm.M13Tsu, digits = 4) # 1.96 x SE at 95% CI

brks.M13Tsu = slm.M13Tsu$psi
brk1.M13Tsu = round(brks.M13Tsu[1,2])
brk2.M13Tsu = round(brks.M13Tsu[2,2])
brk3.M13Tsu = round(brks.M13Tsu[3,2])
brk4.M13Tsu = round(brks.M13Tsu[4,2])

seg1.M13Tsu = subset(rwi.M13Tsu, year <= brk1.M13Tsu)
seg2.M13Tsu = subset(rwi.M13Tsu, year >= brk1.M13Tsu & year <= brk2.M13Tsu)
seg3.M13Tsu = subset(rwi.M13Tsu, year >= brk2.M13Tsu & year <= brk3.M13Tsu)
seg4.M13Tsu = subset(rwi.M13Tsu, year >= brk3.M13Tsu & year <= brk4.M13Tsu)
seg5.M13Tsu = subset(rwi.M13Tsu, year >= brk4.M13Tsu)

seg1.M13Tsu.lm = zyp.sen(xxxstd ~ year, seg1.M13Tsu) 
seg2.M13Tsu.lm = zyp.sen(xxxstd ~ year, seg2.M13Tsu) 
seg3.M13Tsu.lm = zyp.sen(xxxstd ~ year, seg3.M13Tsu)
seg4.M13Tsu.lm = zyp.sen(xxxstd ~ year, seg4.M13Tsu)
seg5.M13Tsu.lm = zyp.sen(xxxstd ~ year, seg5.M13Tsu)

seg1.M13Tsu$slope = seg1.M13Tsu.lm$coefficients[2] * 
  seg1.M13Tsu$year + seg1.M13Tsu.lm$coefficients[1] # Eq
seg2.M13Tsu$slope = seg2.M13Tsu.lm$coefficients[2] * 
  seg2.M13Tsu$year + seg2.M13Tsu.lm$coefficients[1] # Eq
seg3.M13Tsu$slope = seg3.M13Tsu.lm$coefficients[2] * 
  seg3.M13Tsu$year + seg3.M13Tsu.lm$coefficients[1] # Eq
seg4.M13Tsu$slope = seg4.M13Tsu.lm$coefficients[2] * 
  seg4.M13Tsu$year + seg4.M13Tsu.lm$coefficients[1] # Eq
seg5.M13Tsu$slope = seg5.M13Tsu.lm$coefficients[2] * 
  seg5.M13Tsu$year + seg5.M13Tsu.lm$coefficients[1] # Eq

M13Tsu.p = ggplot(data = rwi.M13Tsu) +
  #geom_point(aes(x = slm.M13Tsu.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M13Tsu.ci[1,2], xend = slm.M13Tsu.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M13Tsu.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M13Tsu.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M13Tsu.ci[2,2], xend = slm.M13Tsu.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M13Tsu.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M13Tsu.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M13Tsu.ci[3,2], xend = slm.M13Tsu.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M13Tsu.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M13Tsu.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M13Tsu.ci[4,2], xend = slm.M13Tsu.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M13Tsu.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M13Tsu$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M13Tsu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.M13Tsu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.M13Tsu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.M13Tsu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.M13Tsu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("01Tsu (0.64**)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M13Tsu.p

# M13LTS
lts.M13_1 = add_rownames(lts.M13, var = "year")
lts.M13_1$year = as.numeric(lts.M13_1$year) # Years column in chr by default

lm.ltsM13 = lm(Avg_CC_Median ~ year, data = lts.M13_1)

slm.ltsM13 = segmented.lm(lm.ltsM13, psi = c(2010), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM13.ci = confint(slm.ltsM13, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM13 = slm.ltsM13$psi
brk1.ltsM13 = round(brks.ltsM13[1,2])

seg1.ltsM13 = subset(lts.M13_1, year <= brk1.ltsM13)
seg2.ltsM13 = subset(lts.M13_1, year >= brk1.ltsM13)

seg1.ltsM13.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM13) 
seg2.ltsM13.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM13) 

seg1.ltsM13$slope = seg1.ltsM13.lm$coefficients[2] * 
  seg1.ltsM13$year + seg1.ltsM13.lm$coefficients[1] # Eq
seg2.ltsM13$slope = seg2.ltsM13.lm$coefficients[2] * 
  seg2.ltsM13$year + seg2.ltsM13.lm$coefficients[1] # Eq

ltsM13.p = ggplot(data = lts.M13_1) +
  #geom_point(aes(x = slm.ltsM13.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM13.ci[1,2], xend = slm.ltsM13.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM13.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM13$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM13, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM13, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ltsM13.p

# M17Thu
rwi.M17Thu = add_rownames(rwl.M17Thu.mne.crn, var = "year")
rwi.M17Thu$year = as.numeric(rwi.M17Thu$year) # Years column in chr by default

lm.M17Thu = lm(xxxstd ~ year, data = rwi.M17Thu)

slm.M17Thu = segmented.lm(lm.M17Thu, psi = c(1953,1975,1993), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M17Thu.ci = confint(slm.M17Thu, digits = 4) # 1.96 x SE at 95% CI

brks.M17Thu = slm.M17Thu$psi
brk1.M17Thu = round(brks.M17Thu[1,2])
brk2.M17Thu = round(brks.M17Thu[2,2])
brk3.M17Thu = round(brks.M17Thu[3,2])

seg1.M17Thu = subset(rwi.M17Thu, year <= brk1.M17Thu)
seg2.M17Thu = subset(rwi.M17Thu, year >= brk1.M17Thu & year <= brk2.M17Thu)
seg3.M17Thu = subset(rwi.M17Thu, year >= brk2.M17Thu & year <= brk3.M17Thu)
seg4.M17Thu = subset(rwi.M17Thu, year >= brk3.M17Thu)

seg1.M17Thu.lm = zyp.sen(xxxstd ~ year, seg1.M17Thu) 
seg2.M17Thu.lm = zyp.sen(xxxstd ~ year, seg2.M17Thu) 
seg3.M17Thu.lm = zyp.sen(xxxstd ~ year, seg3.M17Thu)
seg4.M17Thu.lm = zyp.sen(xxxstd ~ year, seg4.M17Thu)

seg1.M17Thu$slope = seg1.M17Thu.lm$coefficients[2] * 
  seg1.M17Thu$year + seg1.M17Thu.lm$coefficients[1] # Eq
seg2.M17Thu$slope = seg2.M17Thu.lm$coefficients[2] * 
  seg2.M17Thu$year + seg2.M17Thu.lm$coefficients[1] # Eq
seg3.M17Thu$slope = seg3.M17Thu.lm$coefficients[2] * 
  seg3.M17Thu$year + seg3.M17Thu.lm$coefficients[1] # Eq
seg4.M17Thu$slope = seg4.M17Thu.lm$coefficients[2] * 
  seg4.M17Thu$year + seg4.M17Thu.lm$coefficients[1] # Eq

M17Thu.p = ggplot(data = rwi.M17Thu) +
  #geom_point(aes(x = slm.M17Thu.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M17Thu.ci[1,2], xend = slm.M17Thu.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M17Thu.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M17Thu.ci[2,1], y = 0.10), col = "gray48") +
  geom_segment(aes(x = slm.M17Thu.ci[2,2], xend = slm.M17Thu.ci[2,3], y = 0.10, 
                   yend = 0.10), col = "gray48") +
  geom_vline(xintercept = slm.M17Thu.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M17Thu.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M17Thu.ci[3,2], xend = slm.M17Thu.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M17Thu.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M17Thu$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M17Thu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.M17Thu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.M17Thu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.M17Thu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("16Thu (0.43*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M17Thu.p

# M17LTS (Bottom Left 2)
lts.M17_1 = add_rownames(lts.M17, var = "year")
lts.M17_1$year = as.numeric(lts.M17_1$year) # Years column in chr by default

lm.ltsM17 = lm(Avg_CC_Median ~ year, data = lts.M17_1)

slm.ltsM17 = segmented.lm(lm.ltsM17, psi = c(1997), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM17.ci = confint(slm.ltsM17, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM17 = slm.ltsM17$psi
brk1.ltsM17 = round(brks.ltsM17[1,2])

seg1.ltsM17 = subset(lts.M17_1, year <= brk1.ltsM17)
seg2.ltsM17 = subset(lts.M17_1, year >= brk1.ltsM17)

seg1.ltsM17.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM17) 
seg2.ltsM17.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM17) 

seg1.ltsM17$slope = seg1.ltsM17.lm$coefficients[2] * 
  seg1.ltsM17$year + seg1.ltsM17.lm$coefficients[1] # Eq
seg2.ltsM17$slope = seg2.ltsM17.lm$coefficients[2] * 
  seg2.ltsM17$year + seg2.ltsM17.lm$coefficients[1] # Eq

ltsM17.p = ggplot(data = lts.M17_1) +
  #geom_point(aes(x = slm.ltsM17.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM17.ci[1,2], xend = slm.ltsM17.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM17.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM17$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM17, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM17, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = "%CC", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"))
ltsM17.p

# M20Thu
rwi.M20Thu = add_rownames(rwl.M20Thu.mne.crn, var = "year")
rwi.M20Thu$year = as.numeric(rwi.M20Thu$year) # Years column in chr by default

lm.M20Thu = lm(xxxstd ~ year, data = rwi.M20Thu)

M20Thu.p = ggplot(data = rwi.M20Thu) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  geom_line(aes(x = year, y = lm.M20Thu$fitted.values), lwd = 1.25, col = "black") + 
  ggtitle("14Thu (0.10)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M20Thu.p

# M20LTS
lts.M20_1 = add_rownames(lts.M20, var = "year")
lts.M20_1$year = as.numeric(lts.M20_1$year) # Years column in chr by default

lm.ltsM20 = lm(Avg_CC_Median ~ year, data = lts.M20_1)

slm.ltsM20 = segmented.lm(lm.ltsM20, psi = c(1975,1996), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM20.ci = confint(slm.ltsM20, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM20 = slm.ltsM20$psi
brk1.ltsM20 = round(brks.ltsM20[1,2])
brk2.ltsM20 = round(brks.ltsM20[2,2])

seg1.ltsM20 = subset(lts.M20_1, year <= brk1.ltsM20)
seg2.ltsM20 = subset(lts.M20_1, year >= brk1.ltsM20 & year <= brk2.ltsM20)
seg3.ltsM20 = subset(lts.M20_1, year >= brk2.ltsM20)

seg1.ltsM20.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM20) 
seg2.ltsM20.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM20) 
seg3.ltsM20.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM20)

seg1.ltsM20$slope = seg1.ltsM20.lm$coefficients[2] * 
  seg1.ltsM20$year + seg1.ltsM20.lm$coefficients[1] # Eq
seg2.ltsM20$slope = seg2.ltsM20.lm$coefficients[2] * 
  seg2.ltsM20$year + seg2.ltsM20.lm$coefficients[1] # Eq
seg3.ltsM20$slope = seg3.ltsM20.lm$coefficients[2] * 
  seg3.ltsM20$year + seg3.ltsM20.lm$coefficients[1] # Eq

ltsM20.p = ggplot(data = lts.M20_1) +
  #geom_point(aes(x = slm.ltsM20.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM20.ci[1,2], xend = slm.ltsM20.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM20.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM20.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM20.ci[2,2], xend = slm.ltsM20.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM20.ci[2,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM20$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM20, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.ltsM20, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsM20, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM20.p

# M26Thu
rwi.M26Thu = add_rownames(rwl.M26Thu.mne.crn, var = "year")
rwi.M26Thu$year = as.numeric(rwi.M26Thu$year) # Years column in chr by default

lm.M26Thu = lm(xxxstd ~ year, data = rwi.M26Thu)

slm.M26Thu = segmented.lm(lm.M26Thu, psi = c(1971,1978,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M26Thu.ci = confint(slm.M26Thu, digits = 4) # 1.96 x SE at 95% CI

brks.M26Thu = slm.M26Thu$psi
brk1.M26Thu = round(brks.M26Thu[1,2])
brk2.M26Thu = round(brks.M26Thu[2,2])
brk3.M26Thu = round(brks.M26Thu[3,2])

seg1.M26Thu = subset(rwi.M26Thu, year <= brk1.M26Thu)
seg2.M26Thu = subset(rwi.M26Thu, year >= brk1.M26Thu & year <= brk2.M26Thu)
seg3.M26Thu = subset(rwi.M26Thu, year >= brk2.M26Thu & year <= brk3.M26Thu)
seg4.M26Thu = subset(rwi.M26Thu, year >= brk3.M26Thu)

seg1.M26Thu.lm = zyp.sen(xxxstd ~ year, seg1.M26Thu) 
seg2.M26Thu.lm = zyp.sen(xxxstd ~ year, seg2.M26Thu) 
seg3.M26Thu.lm = zyp.sen(xxxstd ~ year, seg3.M26Thu)
seg4.M26Thu.lm = zyp.sen(xxxstd ~ year, seg4.M26Thu)

seg1.M26Thu$slope = seg1.M26Thu.lm$coefficients[2] * 
  seg1.M26Thu$year + seg1.M26Thu.lm$coefficients[1] # Eq
seg2.M26Thu$slope = seg2.M26Thu.lm$coefficients[2] * 
  seg2.M26Thu$year + seg2.M26Thu.lm$coefficients[1] # Eq
seg3.M26Thu$slope = seg3.M26Thu.lm$coefficients[2] * 
  seg3.M26Thu$year + seg3.M26Thu.lm$coefficients[1] # Eq
seg4.M26Thu$slope = seg4.M26Thu.lm$coefficients[2] * 
  seg4.M26Thu$year + seg4.M26Thu.lm$coefficients[1] # Eq

M26Thu.p = ggplot(data = rwi.M26Thu) +
  #geom_point(aes(x = slm.M26Thu.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M26Thu.ci[1,2], xend = slm.M26Thu.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M26Thu.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M26Thu.ci[2,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.M26Thu.ci[2,2], xend = slm.M26Thu.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M26Thu.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M26Thu.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M26Thu.ci[3,2], xend = slm.M26Thu.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M26Thu.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M26Thu$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M26Thu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.M26Thu, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg3.M26Thu, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.M26Thu, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("10Thu (0.55*)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
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
M26Thu.p

# M26LTS (Top Left 2)
lts.M26_1 = add_rownames(lts.M26, var = "year")
lts.M26_1$year = as.numeric(lts.M26_1$year) # Years column in chr by default

lm.ltsM26 = lm(Avg_CC_Median ~ year, data = lts.M26_1)

slm.ltsM26 = segmented.lm(lm.ltsM26, psi = c(1980,1985,2008), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM26.ci = confint(slm.ltsM26, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM26 = slm.ltsM26$psi
brk1.ltsM26 = round(brks.ltsM26[1,2])
brk2.ltsM26 = round(brks.ltsM26[2,2])
brk3.ltsM26 = round(brks.ltsM26[3,2])

seg1.ltsM26 = subset(lts.M26_1, year <= brk1.ltsM26)
seg2.ltsM26 = subset(lts.M26_1, year >= brk1.ltsM26 & year <= brk2.ltsM26)
seg3.ltsM26 = subset(lts.M26_1, year >= brk2.ltsM26 & year <= brk3.ltsM26)
seg4.ltsM26 = subset(lts.M26_1, year >= brk3.ltsM26)

seg1.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM26) 
seg2.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM26) 
seg3.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM26)
seg4.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg4.ltsM26)

seg1.ltsM26$slope = seg1.ltsM26.lm$coefficients[2] * 
  seg1.ltsM26$year + seg1.ltsM26.lm$coefficients[1] # Eq
seg2.ltsM26$slope = seg2.ltsM26.lm$coefficients[2] * 
  seg2.ltsM26$year + seg2.ltsM26.lm$coefficients[1] # Eq
seg3.ltsM26$slope = seg3.ltsM26.lm$coefficients[2] * 
  seg3.ltsM26$year + seg3.ltsM26.lm$coefficients[1] # Eq
seg4.ltsM26$slope = seg4.ltsM26.lm$coefficients[2] * 
  seg4.ltsM26$year + seg4.ltsM26.lm$coefficients[1] # Eq

ltsM26.p = ggplot(data = lts.M26_1) +
  #geom_point(aes(x = slm.ltsM26.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[1,2], xend = slm.ltsM26.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM26.ci[2,1], y = 73.5), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[2,2], xend = slm.ltsM26.ci[2,3], y = 73.5, 
                   yend = 73.5), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM26.ci[3,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[3,2], xend = slm.ltsM26.ci[3,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM26$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM26, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.ltsM26, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.ltsM26, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.ltsM26, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM26.p

# M27Pin
rwi.M27Pin = add_rownames(rwl.M27Pin.mne.crn, var = "year")
rwi.M27Pin$year = as.numeric(rwi.M27Pin$year) # Years column in chr by default

lm.M27Pin = lm(xxxstd ~ year, data = rwi.M27Pin)

slm.M27Pin = segmented.lm(lm.M27Pin, psi = c(1985,2000,2008,2013), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M27Pin.ci = confint(slm.M27Pin, digits = 4) # 1.96 x SE at 95% CI

brks.M27Pin = slm.M27Pin$psi
brk1.M27Pin = round(brks.M27Pin[1,2])
brk2.M27Pin = round(brks.M27Pin[2,2])
brk3.M27Pin = round(brks.M27Pin[3,2])
brk4.M27Pin = round(brks.M27Pin[4,2])

seg1.M27Pin = subset(rwi.M27Pin, year <= brk1.M27Pin)
seg2.M27Pin = subset(rwi.M27Pin, year >= brk1.M27Pin & year <= brk2.M27Pin)
seg3.M27Pin = subset(rwi.M27Pin, year >= brk2.M27Pin & year <= brk3.M27Pin)
seg4.M27Pin = subset(rwi.M27Pin, year >= brk3.M27Pin & year <= brk4.M27Pin)
seg5.M27Pin = subset(rwi.M27Pin, year >= brk4.M27Pin)

seg1.M27Pin.lm = zyp.sen(xxxstd ~ year, seg1.M27Pin) 
seg2.M27Pin.lm = zyp.sen(xxxstd ~ year, seg2.M27Pin) 
seg3.M27Pin.lm = zyp.sen(xxxstd ~ year, seg3.M27Pin)
seg4.M27Pin.lm = zyp.sen(xxxstd ~ year, seg4.M27Pin)
seg5.M27Pin.lm = zyp.sen(xxxstd ~ year, seg5.M27Pin)

seg1.M27Pin$slope = seg1.M27Pin.lm$coefficients[2] * 
  seg1.M27Pin$year + seg1.M27Pin.lm$coefficients[1] # Eq
seg2.M27Pin$slope = seg2.M27Pin.lm$coefficients[2] * 
  seg2.M27Pin$year + seg2.M27Pin.lm$coefficients[1] # Eq
seg3.M27Pin$slope = seg3.M27Pin.lm$coefficients[2] * 
  seg3.M27Pin$year + seg3.M27Pin.lm$coefficients[1] # Eq
seg4.M27Pin$slope = seg4.M27Pin.lm$coefficients[2] * 
  seg4.M27Pin$year + seg4.M27Pin.lm$coefficients[1] # Eq
seg5.M27Pin$slope = seg5.M27Pin.lm$coefficients[2] * 
  seg5.M27Pin$year + seg5.M27Pin.lm$coefficients[1] # Eq

M27Pin.p = ggplot(data = rwi.M27Pin) +
  #geom_point(aes(x = slm.M27Pin.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M27Pin.ci[1,2], xend = slm.M27Pin.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M27Pin.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M27Pin.ci[2,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M27Pin.ci[2,2], xend = slm.M27Pin.ci[2,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M27Pin.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M27Pin.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M27Pin.ci[3,2], xend = slm.M27Pin.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M27Pin.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.M27Pin.ci[4,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.M27Pin.ci[4,2], xend = slm.M27Pin.ci[4,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M27Pin.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.M27Pin$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.M27Pin, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.M27Pin, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.M27Pin, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.M27Pin, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.M27Pin, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("09Pin (0.20)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0.5,2,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
M27Pin.p

# M27LTS (Bottom Right 2)
lts.M27_1 = add_rownames(lts.M27, var = "year")
lts.M27_1$year = as.numeric(lts.M27_1$year) # Years column in chr by default
lts.M27_1 = lts.M27_1[-c(1:9),]

lm.ltsM27 = lm(Avg_CC_Median ~ year, data = lts.M27_1)
summary(lm.ltsM27)

selgmented(lm.ltsM27, type = "bic", Kmax = 10) 

slm.ltsM27 = segmented.lm(lm.ltsM27, psi = c(1984,1997), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM27.ci = confint(slm.ltsM27, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM27 = slm.ltsM27$psi
brk1.ltsM27 = round(brks.ltsM27[1,2])
brk2.ltsM27 = round(brks.ltsM27[2,2])

seg1.ltsM27 = subset(lts.M27_1, year <= brk1.ltsM27)
seg2.ltsM27 = subset(lts.M27_1, year >= brk1.ltsM27 & year <= brk2.ltsM27)
seg3.ltsM27 = subset(lts.M27_1, year >= brk2.ltsM27)

seg1.ltsM27.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM27) 
seg2.ltsM27.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM27) 
seg3.ltsM27.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM27) 

seg1.ltsM27$slope = seg1.ltsM27.lm$coefficients[2] * 
  seg1.ltsM27$year + seg1.ltsM27.lm$coefficients[1] # Eq
seg2.ltsM27$slope = seg2.ltsM27.lm$coefficients[2] * 
  seg2.ltsM27$year + seg2.ltsM27.lm$coefficients[1] # Eq
seg3.ltsM27$slope = seg3.ltsM27.lm$coefficients[2] * 
  seg3.ltsM27$year + seg3.ltsM27.lm$coefficients[1] # Eq

sens.slope(seg1.ltsM27$Avg_CC_Median) 
acf(seg1.ltsM27$Avg_CC_Median) 
mmkh(seg1.ltsM27$Avg_CC_Median)

sens.slope(seg2.ltsM27$Avg_CC_Median)
acf(seg2.ltsM27$Avg_CC_Median) 
mmkh(seg2.ltsM27$Avg_CC_Median)

sens.slope(seg3.ltsM27$Avg_CC_Median)
acf(seg3.ltsM27$Avg_CC_Median) 
mmkh(seg3.ltsM27$Avg_CC_Median)

ltsM27.p = ggplot(data = lts.M27_1) +
  #geom_point(aes(x = slm.ltsM27.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM27.ci[1,2], xend = slm.ltsM27.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM27.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM27.ci[2,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM27.ci[2,2], xend = slm.ltsM27.ci[2,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM27.ci[2,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM27$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM27, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg2.ltsM27, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.ltsM27, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ltsM27.p
#####
tiff("segmentedplot5.tiff", units = "in", width = 6.5, height = 8, res = 300)
((M13Tsu.p / ltsM13.p) | (F15Que.p / ltsF15.p) | 
    (M06Ace.p / ltsM06.p) | (M07Tsu.p / ltsM07.p)) /
  ((F23Ace.p / ltsF23.p) | (F21Dec.p / ltsF21.p) | 
     (M05Thu.p / ltsM05.p) | (F07Ace.p / ltsF07.p)) /
  ((M27Pin.p / ltsM27.p) | (M26Thu.p / ltsM26.p) | 
     (F25Ace.p / ltsF25.p) | (M01Pop.p / ltsM01.p)) /
  ((F33Pic.p / ltsF33.p) | (M20Thu.p / ltsM20.p) | 
     (F30Bet.p / ltsF30.p) | (M17Thu.p / ltsM17.p))
dev.off()

##### Second chronology plots #####
# F15Ace / 02Ace
rwi.F15Ace = add_rownames(rwl.F15Ace.mne.crn, var = "year")
rwi.F15Ace$year = as.numeric(rwi.F15Ace$year) # Years column in chr by default

lm.F15Ace = lm(xxxstd ~ year, data = rwi.F15Ace)

slm.F15Ace = segmented.lm(lm.F15Ace, psi = c(1929,1979,1996,2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F15Ace.ci = confint(slm.F15Ace, digits = 4) # 1.96 x SE at 95% CI

brks.F15Ace = slm.F15Ace$psi
brk1.F15Ace = round(brks.F15Ace[1,2])
brk2.F15Ace = round(brks.F15Ace[2,2])
brk3.F15Ace = round(brks.F15Ace[3,2])
brk4.F15Ace = round(brks.F15Ace[4,2])

seg1.F15Ace = subset(rwi.F15Ace, year <= brk1.F15Ace)
seg2.F15Ace = subset(rwi.F15Ace, year >= brk1.F15Ace & year <= brk2.F15Ace)
seg3.F15Ace = subset(rwi.F15Ace, year >= brk2.F15Ace & year <= brk3.F15Ace)
seg4.F15Ace = subset(rwi.F15Ace, year >= brk3.F15Ace & year <= brk4.F15Ace)
seg5.F15Ace = subset(rwi.F15Ace, year >= brk4.F15Ace)

seg1.F15Ace.lm = zyp.sen(xxxstd ~ year, seg1.F15Ace) 
seg2.F15Ace.lm = zyp.sen(xxxstd ~ year, seg2.F15Ace) 
seg3.F15Ace.lm = zyp.sen(xxxstd ~ year, seg3.F15Ace)
seg4.F15Ace.lm = zyp.sen(xxxstd ~ year, seg4.F15Ace)
seg5.F15Ace.lm = zyp.sen(xxxstd ~ year, seg5.F15Ace)

seg1.F15Ace$slope = seg1.F15Ace.lm$coefficients[2] * 
  seg1.F15Ace$year + seg1.F15Ace.lm$coefficients[1] # Eq
seg2.F15Ace$slope = seg2.F15Ace.lm$coefficients[2] * 
  seg2.F15Ace$year + seg2.F15Ace.lm$coefficients[1] # Eq
seg3.F15Ace$slope = seg3.F15Ace.lm$coefficients[2] * 
  seg3.F15Ace$year + seg3.F15Ace.lm$coefficients[1] # Eq
seg4.F15Ace$slope = seg4.F15Ace.lm$coefficients[2] * 
  seg4.F15Ace$year + seg4.F15Ace.lm$coefficients[1] # Eq
seg5.F15Ace$slope = seg5.F15Ace.lm$coefficients[2] * 
  seg5.F15Ace$year + seg5.F15Ace.lm$coefficients[1] # Eq

F15Ace.p = ggplot(data = rwi.F15Ace) +
  #geom_point(aes(x = slm.F15Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Ace.ci[1,2], xend = slm.F15Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Ace.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Ace.ci[2,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F15Ace.ci[2,2], xend = slm.F15Ace.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F15Ace.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Ace.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F15Ace.ci[3,2], xend = slm.F15Ace.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F15Ace.ci[3,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F15Ace.ci[4,1], y = 0.15), col = "gray48") +
  geom_segment(aes(x = slm.F15Ace.ci[4,2], xend = slm.F15Ace.ci[4,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.F15Ace.ci[4,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F15Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F15Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.F15Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.F15Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.F15Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.F15Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("02Ace (-0.13)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.7),
                     breaks = seq(0.5,2.5,0.5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F15Ace.p

# F15LTS
lts.F15_1 = add_rownames(lts.F15, var = "year")
lts.F15_1$year = as.numeric(lts.F15_1$year) # Years column in chr by default

lm.ltsF15 = lm(Avg_CC_Median ~ year, data = lts.F15_1)

ltsF15.p = ggplot(data = lts.F15_1) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  geom_line(aes(x = year, y = lm.ltsF15$fitted.values), lwd = 1.25, col = "red3") + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm"))
ltsF15.p

# M06Que / 03Que
rwi.M06Que = add_rownames(rwl.M06Que.mne.crn, var = "year")
rwi.M06Que$year = as.numeric(rwi.M06Que$year) # Years column in chr by default

lm.M06Que = lm(xxxstd ~ year, data = rwi.M06Que)

slm.M06Que = segmented.lm(lm.M06Que, npsi = 5, # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.M06Que.ci = confint(slm.M06Que, digits = 4) # 1.96 x SE at 95% CI

brks.M06Que = slm.M06Que$psi
brk1.M06Que = round(brks.M06Que[1,2])
brk2.M06Que = round(brks.M06Que[2,2])
brk3.M06Que = round(brks.M06Que[3,2])
brk4.M06Que = round(brks.M06Que[4,2])
brk5.M06Que = round(brks.M06Que[5,2])

seg1.M06Que = subset(rwi.M06Que, year <= brk1.M06Que)
seg2.M06Que = subset(rwi.M06Que, year >= brk1.M06Que & year <= brk2.M06Que)
seg3.M06Que = subset(rwi.M06Que, year >= brk2.M06Que & year <= brk3.M06Que)
seg4.M06Que = subset(rwi.M06Que, year >= brk3.M06Que & year <= brk4.M06Que)
seg5.M06Que = subset(rwi.M06Que, year >= brk4.M06Que & year <= brk5.M06Que)
seg6.M06Que = subset(rwi.M06Que, year >= brk5.M06Que)

seg1.M06Que.lm = zyp.sen(xxxstd ~ year, seg1.M06Que) 
seg2.M06Que.lm = zyp.sen(xxxstd ~ year, seg2.M06Que) 
seg3.M06Que.lm = zyp.sen(xxxstd ~ year, seg3.M06Que)
seg4.M06Que.lm = zyp.sen(xxxstd ~ year, seg4.M06Que)
seg5.M06Que.lm = zyp.sen(xxxstd ~ year, seg5.M06Que)
seg6.M06Que.lm = zyp.sen(xxxstd ~ year, seg6.M06Que)

seg1.M06Que$slope = seg1.M06Que.lm$coefficients[2] * 
  seg1.M06Que$year + seg1.M06Que.lm$coefficients[1] # Eq
seg2.M06Que$slope = seg2.M06Que.lm$coefficients[2] * 
  seg2.M06Que$year + seg2.M06Que.lm$coefficients[1] # Eq
seg3.M06Que$slope = seg3.M06Que.lm$coefficients[2] * 
  seg3.M06Que$year + seg3.M06Que.lm$coefficients[1] # Eq
seg4.M06Que$slope = seg4.M06Que.lm$coefficients[2] * 
  seg4.M06Que$year + seg4.M06Que.lm$coefficients[1] # Eq
seg5.M06Que$slope = seg5.M06Que.lm$coefficients[2] * 
  seg5.M06Que$year + seg5.M06Que.lm$coefficients[1] # Eq
seg6.M06Que$slope = seg6.M06Que.lm$coefficients[2] * 
  seg6.M06Que$year + seg6.M06Que.lm$coefficients[1] # Eq

M06Que.p = ggplot(data = rwi.M06Que) +
  geom_segment(aes(x = slm.M06Que.ci[1,2], xend = slm.M06Que.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Que.ci[1,1], col = "gray48", lty = 2) +
  geom_segment(aes(x = slm.M06Que.ci[2,2], xend = slm.M06Que.ci[2,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M06Que.ci[2,1], col = "gray48", lty = 2) +
  geom_segment(aes(x = slm.M06Que.ci[3,2], xend = slm.M06Que.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Que.ci[3,1], col = "gray48", lty = 2) +
  geom_segment(aes(x = slm.M06Que.ci[4,2], xend = slm.M06Que.ci[4,3], y = 0.15, 
                   yend = 0.15), col = "gray48") +
  geom_vline(xintercept = slm.M06Que.ci[4,1], col = "gray48", lty = 2) +
  geom_segment(aes(x = slm.M06Que.ci[5,2], xend = slm.M06Que.ci[5,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.M06Que.ci[5,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  geom_line(data = seg1.M06Que, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.M06Que, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg3.M06Que, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg4.M06Que, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg5.M06Que, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg6.M06Que, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  ggtitle("03Que (0.03)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.7),
                     breaks = seq(0.5,2.5,0.5)) +
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
M06Que.p

# M06LTS
lts.M06_1 = add_rownames(lts.M06, var = "year")
lts.M06_1$year = as.numeric(lts.M06_1$year) # Years column in chr by default

lm.ltsM06 = lm(Avg_CC_Median ~ year, data = lts.M06_1)

slm.ltsM06 = segmented.lm(lm.ltsM06, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM06.ci = confint(slm.ltsM06, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM06 = slm.ltsM06$psi
brk1.ltsM06 = round(brks.ltsM06[1,2])

seg1.ltsM06 = subset(lts.M06_1, year <= brk1.ltsM06)
seg2.ltsM06 = subset(lts.M06_1, year >= brk1.ltsM06)

seg1.ltsM06.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM06) 
seg2.ltsM06.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM06) 

seg1.ltsM06$slope = seg1.ltsM06.lm$coefficients[2] * 
  seg1.ltsM06$year + seg1.ltsM06.lm$coefficients[1] # Eq
seg2.ltsM06$slope = seg2.ltsM06.lm$coefficients[2] * 
  seg2.ltsM06$year + seg2.ltsM06.lm$coefficients[1] # Eq

ltsM06.p = ggplot(data = lts.M06_1) +
  #geom_point(aes(x = slm.ltsM06.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM06.ci[1,2], xend = slm.ltsM06.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM06.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM06$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM06, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsM06, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM06.p

# M26Dec
rwi.M26Dec = add_rownames(rwl.M26Dec.mne.crn, var = "year")
rwi.M26Dec$year = as.numeric(rwi.M26Dec$year) # Years column in chr by default

lm.M26Dec = lm(xxxstd ~ year, data = rwi.M26Dec)

M26Dec.p = ggplot(data = rwi.M26Dec) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  geom_line(aes(x = year, y = lm.M26Dec$fitted.values), lwd = 1.25) + 
  ggtitle("10Dec (-0.17)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.7),
                     breaks = seq(0.5,2.5,0.5)) +
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
M26Dec.p

# M26LTS
lts.M26_1 = add_rownames(lts.M26, var = "year")
lts.M26_1$year = as.numeric(lts.M26_1$year) # Years column in chr by default

lm.ltsM26 = lm(Avg_CC_Median ~ year, data = lts.M26_1)

slm.ltsM26 = segmented.lm(lm.ltsM26, psi = c(1980,1985,2008), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsM26.ci = confint(slm.ltsM26, digits = 4) # 1.96 x SE at 95% CI

brks.ltsM26 = slm.ltsM26$psi
brk1.ltsM26 = round(brks.ltsM26[1,2])
brk2.ltsM26 = round(brks.ltsM26[2,2])
brk3.ltsM26 = round(brks.ltsM26[3,2])

seg1.ltsM26 = subset(lts.M26_1, year <= brk1.ltsM26)
seg2.ltsM26 = subset(lts.M26_1, year >= brk1.ltsM26 & year <= brk2.ltsM26)
seg3.ltsM26 = subset(lts.M26_1, year >= brk2.ltsM26 & year <= brk3.ltsM26)
seg4.ltsM26 = subset(lts.M26_1, year >= brk3.ltsM26)

seg1.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsM26) 
seg2.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsM26) 
seg3.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg3.ltsM26)
seg4.ltsM26.lm = zyp.sen(Avg_CC_Median ~ year, seg4.ltsM26)

seg1.ltsM26$slope = seg1.ltsM26.lm$coefficients[2] * 
  seg1.ltsM26$year + seg1.ltsM26.lm$coefficients[1] # Eq
seg2.ltsM26$slope = seg2.ltsM26.lm$coefficients[2] * 
  seg2.ltsM26$year + seg2.ltsM26.lm$coefficients[1] # Eq
seg3.ltsM26$slope = seg3.ltsM26.lm$coefficients[2] * 
  seg3.ltsM26$year + seg3.ltsM26.lm$coefficients[1] # Eq
seg4.ltsM26$slope = seg4.ltsM26.lm$coefficients[2] * 
  seg4.ltsM26$year + seg4.ltsM26.lm$coefficients[1] # Eq

ltsM26.p = ggplot(data = lts.M26_1) +
  #geom_point(aes(x = slm.ltsM26.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[1,2], xend = slm.ltsM26.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM26.ci[2,1], y = 73.5), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[2,2], xend = slm.ltsM26.ci[2,3], y = 73.5, 
                   yend = 73.5), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.ltsM26.ci[3,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsM26.ci[3,2], xend = slm.ltsM26.ci[3,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsM26.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsM26$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsM26, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  geom_line(data = seg2.ltsM26, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.ltsM26, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.ltsM26, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0.1, 0, 0.1), "cm"))
ltsM26.p

# F30Ace
rwi.F30Ace = add_rownames(rwl.F30Ace.mne.crn, var = "year")
rwi.F30Ace$year = as.numeric(rwi.F30Ace$year) # Years column in chr by default

lm.F30Ace = lm(xxxstd ~ year, data = rwi.F30Ace)

slm.F30Ace = segmented.lm(lm.F30Ace, psi = c(1952,1967,1972), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.F30Ace.ci = confint(slm.F30Ace, digits = 4) # 1.96 x SE at 95% CI

brks.F30Ace = slm.F30Ace$psi
brk1.F30Ace = round(brks.F30Ace[1,2])
brk2.F30Ace = round(brks.F30Ace[2,2])
brk3.F30Ace = round(brks.F30Ace[3,2])

seg1.F30Ace = subset(rwi.F30Ace, year <= brk1.F30Ace)
seg2.F30Ace = subset(rwi.F30Ace, year >= brk1.F30Ace & year <= brk2.F30Ace)
seg3.F30Ace = subset(rwi.F30Ace, year >= brk2.F30Ace & year <= brk3.F30Ace)
seg4.F30Ace = subset(rwi.F30Ace, year >= brk3.F30Ace)

seg1.F30Ace.lm = zyp.sen(xxxstd ~ year, seg1.F30Ace) 
seg2.F30Ace.lm = zyp.sen(xxxstd ~ year, seg2.F30Ace) 
seg3.F30Ace.lm = zyp.sen(xxxstd ~ year, seg3.F30Ace)
seg4.F30Ace.lm = zyp.sen(xxxstd ~ year, seg4.F30Ace)

seg1.F30Ace$slope = seg1.F30Ace.lm$coefficients[2] * 
  seg1.F30Ace$year + seg1.F30Ace.lm$coefficients[1] # Eq
seg2.F30Ace$slope = seg2.F30Ace.lm$coefficients[2] * 
  seg2.F30Ace$year + seg2.F30Ace.lm$coefficients[1] # Eq
seg3.F30Ace$slope = seg3.F30Ace.lm$coefficients[2] * 
  seg3.F30Ace$year + seg3.F30Ace.lm$coefficients[1] # Eq
seg4.F30Ace$slope = seg4.F30Ace.lm$coefficients[2] * 
  seg4.F30Ace$year + seg4.F30Ace.lm$coefficients[1] # Eq

F30Ace.p = ggplot(data = rwi.F30Ace) +
  #geom_point(aes(x = slm.F30Ace.ci[1,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F30Ace.ci[1,2], xend = slm.F30Ace.ci[1,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F30Ace.ci[1,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F30Ace.ci[2,1], y = 0.10), col = "gray48") +
  geom_segment(aes(x = slm.F30Ace.ci[2,2], xend = slm.F30Ace.ci[2,3], y = 0.10, 
                   yend = 0.10), col = "gray48") +
  geom_vline(xintercept = slm.F30Ace.ci[2,1], col = "gray48", lty = 2) +
  #geom_point(aes(x = slm.F30Ace.ci[3,1], y = 0.1), col = "gray48") +
  geom_segment(aes(x = slm.F30Ace.ci[3,2], xend = slm.F30Ace.ci[3,3], y = 0.1, 
                   yend = 0.1), col = "gray48") +
  geom_vline(xintercept = slm.F30Ace.ci[3,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = xxxstd), col = "gray8") +
  #geom_line(aes(x = year, y = slm.F30Ace$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.F30Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.F30Ace, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  geom_line(data = seg3.F30Ace, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg4.F30Ace, aes(x = year, y = slope), col = "red3", lwd = 1.25) +
  ggtitle("15Ace (-0.11)") +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.7),
                     breaks = seq(0.5,2.5,0.5)) +
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
F30Ace.p

# F30LTS
lts.F30_1 = add_rownames(lts.F30, var = "year")
lts.F30_1$year = as.numeric(lts.F30_1$year) # Years column in chr by default

lm.ltsF30 = lm(Avg_CC_Median ~ year, data = lts.F30_1)

slm.ltsF30 = segmented.lm(lm.ltsF30, psi = c(2009), # psi based on selgmented
                          control = seg.control(display = TRUE, n.boot = 50)) # n.boot 0

slm.ltsF30.ci = confint(slm.ltsF30, digits = 4) # 1.96 x SE at 95% CI

brks.ltsF30 = slm.ltsF30$psi
brk1.ltsF30 = round(brks.ltsF30[1,2])

seg1.ltsF30 = subset(lts.F30_1, year <= brk1.ltsF30)
seg2.ltsF30 = subset(lts.F30_1, year >= brk1.ltsF30)

seg1.ltsF30.lm = zyp.sen(Avg_CC_Median ~ year, seg1.ltsF30) 
seg2.ltsF30.lm = zyp.sen(Avg_CC_Median ~ year, seg2.ltsF30) 

seg1.ltsF30$slope = seg1.ltsF30.lm$coefficients[2] * 
  seg1.ltsF30$year + seg1.ltsF30.lm$coefficients[1] # Eq
seg2.ltsF30$slope = seg2.ltsF30.lm$coefficients[2] * 
  seg2.ltsF30$year + seg2.ltsF30.lm$coefficients[1] # Eq

ltsF30.p = ggplot(data = lts.F30_1) +
  #geom_point(aes(x = slm.ltsF30.ci[1,1], y = 73), col = "gray48") +
  geom_segment(aes(x = slm.ltsF30.ci[1,2], xend = slm.ltsF30.ci[1,3], y = 73, 
                   yend = 73), col = "gray48") +
  geom_vline(xintercept = slm.ltsF30.ci[1,1], col = "gray48", lty = 2) +
  geom_line(aes(x = year, y = Avg_CC_Median), col = "gray8") +
  #geom_line(aes(x = year, y = slm.ltsF30$fitted.values), lty = 2, lwd = 1.25) + 
  geom_line(data = seg1.ltsF30, aes(x = year, y = slope), col = "darkgreen", lwd = 1.25) +
  geom_line(data = seg2.ltsF30, aes(x = year, y = slope), col = "black", lwd = 1.25) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1891,2018), 
                     breaks = seq(1900,2010,10), labels = c(1900,"",1920,"",1940,"",1960,
                                                            "",1980,"",2000,"")) +
  scale_y_continuous(name = "%CC", expand = c(0,0), limits = c(72,100),
                     breaks = seq(75,95,5)) +
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
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"))
ltsF30.p
#####
tiff("segmentedplot5_extra.tiff", units = "in", width = 6.5, height = 2.5, res = 300)
((F15Ace.p / ltsF15.p) | (M06Que.p / ltsM06.p) | 
    (M26Dec.p / ltsM26.p) | (F30Ace.p / ltsF30.p))
dev.off()

##### Squished Plot #####

##### Building RWI and LTS tables #####
#####

possig = 1
negsig = -1
insig = 0

#seg1.M13Tsu.lm$coefficients[2] #Slope of segment 1
#seg1.M13Tsu.lm$x # Years of segment 1

### RWI ###
yrseg_rwi = read.csv("YearlySegmented_RWI.csv")

# M13Tsu / 01Tsu
yrseg_rwi$X01Tsu_slp = ifelse(yrseg_rwi$Year >= min(seg1.M13Tsu.lm$x) + 1 & # Fill in Seg1 slope
                            yrseg_rwi$Year <= max(seg1.M13Tsu.lm$x), seg1.M13Tsu.lm$coefficients[2], 
                   ifelse(yrseg_rwi$Year >= min(seg2.M13Tsu.lm$x) + 1 & # Fill in Seg2 slope
                            yrseg_rwi$Year <= max(seg2.M13Tsu.lm$x), seg2.M13Tsu.lm$coefficients[2],
                   ifelse(yrseg_rwi$Year >= min(seg3.M13Tsu.lm$x) + 1 & # Fill in Seg3 slope
                            yrseg_rwi$Year <= max(seg3.M13Tsu.lm$x), seg3.M13Tsu.lm$coefficients[2],
                   ifelse(yrseg_rwi$Year >= min(seg4.M13Tsu.lm$x) + 1 & # Fill in Seg4 slope
                            yrseg_rwi$Year <= max(seg4.M13Tsu.lm$x), seg4.M13Tsu.lm$coefficients[2],
                   ifelse(yrseg_rwi$Year >= min(seg5.M13Tsu.lm$x) + 1 & # Fill in Seg5 slope
                            yrseg_rwi$Year <= max(seg5.M13Tsu.lm$x), seg5.M13Tsu.lm$coefficients[2], 
                   NA)))))

yrseg_rwi$X01Tsu_sig = ifelse(yrseg_rwi$Year >= min(seg1.M13Tsu.lm$x) + 1 & # Fill in Seg1 slope
                           yrseg_rwi$Year <= max(seg1.M13Tsu.lm$x), possig, 
                   ifelse(yrseg_rwi$Year >= min(seg2.M13Tsu.lm$x) + 1 & # Fill in Seg2 slope
                           yrseg_rwi$Year <= max(seg2.M13Tsu.lm$x), negsig,
                   ifelse(yrseg_rwi$Year >= min(seg3.M13Tsu.lm$x) + 1 & # Fill in Seg3 slope
                           yrseg_rwi$Year <= max(seg3.M13Tsu.lm$x), possig,
                   ifelse(yrseg_rwi$Year >= min(seg4.M13Tsu.lm$x) + 1 & # Fill in Seg4 slope
                           yrseg_rwi$Year <= max(seg4.M13Tsu.lm$x), possig,
                   ifelse(yrseg_rwi$Year >= min(seg5.M13Tsu.lm$x) + 1 & # Fill in Seg5 slope
                           yrseg_rwi$Year <= max(seg5.M13Tsu.lm$x), insig, 
                   NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X01Tsu_sigc[yrseg_rwi$X01Tsu_sig == insig] = "black"
yrseg_rwi$X01Tsu_sigc[yrseg_rwi$X01Tsu_sig == possig] = "darkgreen"
yrseg_rwi$X01Tsu_sigc[yrseg_rwi$X01Tsu_sig == negsig] = "red3"

yrseg_rwi$X01Tsu_brk = ifelse(yrseg_rwi$Year >= min(seg1.M13Tsu.lm$x) + 1, 0, NA)
yrseg_rwi$X01Tsu_brk[yrseg_rwi$Year == brk1.M13Tsu] = 1
yrseg_rwi$X01Tsu_brk[yrseg_rwi$Year == brk2.M13Tsu] = 1
yrseg_rwi$X01Tsu_brk[yrseg_rwi$Year == brk3.M13Tsu] = 1
yrseg_rwi$X01Tsu_brk[yrseg_rwi$Year == brk4.M13Tsu] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X01Tsu_slp, pch = 15, col = yrseg_rwi$X01Tsu_sigc)
abline(0,0)
abline(v = brk1.M13Tsu, lty = 2)
abline(v = brk2.M13Tsu, lty = 2)
abline(v = brk3.M13Tsu, lty = 2)
abline(v = brk4.M13Tsu, lty = 2)

sum(yrseg_rwi$X01Tsu_slp, na.rm = TRUE)

# F15Que / 02Que
yrseg_rwi$X02Que_slp = ifelse(yrseg_rwi$Year >= min(seg1.F15Que.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F15Que.lm$x), seg1.F15Que.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F15Que.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F15Que.lm$x), seg2.F15Que.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F15Que.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F15Que.lm$x), seg3.F15Que.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F15Que.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F15Que.lm$x), seg4.F15Que.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F15Que.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F15Que.lm$x), seg5.F15Que.lm$coefficients[2], 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.F15Que.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.F15Que.lm$x), seg6.F15Que.lm$coefficients[2],
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.F15Que.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.F15Que.lm$x), seg7.F15Que.lm$coefficients[2],
                                                                        NA)))))))

yrseg_rwi$X02Que_sig = ifelse(yrseg_rwi$Year >= min(seg1.F15Que.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F15Que.lm$x), insig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F15Que.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F15Que.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F15Que.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F15Que.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F15Que.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F15Que.lm$x), insig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F15Que.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F15Que.lm$x), negsig, 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.F15Que.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.F15Que.lm$x), possig,
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.F15Que.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.F15Que.lm$x), insig, 
                                                                 NA)))))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X02Que_sigc[yrseg_rwi$X02Que_sig == insig] = "black"
yrseg_rwi$X02Que_sigc[yrseg_rwi$X02Que_sig == possig] = "darkgreen"
yrseg_rwi$X02Que_sigc[yrseg_rwi$X02Que_sig == negsig] = "red3"

yrseg_rwi$X02Que_brk = ifelse(yrseg_rwi$Year >= min(seg1.F15Que.lm$x) + 1, 0, NA)
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk1.F15Que] = 1
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk2.F15Que] = 1
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk3.F15Que] = 1
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk4.F15Que] = 1
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk5.F15Que] = 1
yrseg_rwi$X02Que_brk[yrseg_rwi$Year == brk6.F15Que] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X02Que_slp, pch = 15, col = yrseg_rwi$X02Que_sigc)
abline(0,0)
abline(v = brk1.F15Que, lty = 2)
abline(v = brk2.F15Que, lty = 2)
abline(v = brk3.F15Que, lty = 2)
abline(v = brk4.F15Que, lty = 2)
abline(v = brk5.F15Que, lty = 2)
abline(v = brk6.F15Que, lty = 2)

sum(yrseg_rwi$X02Que_slp, na.rm = TRUE)

# M06Ace / 03Ace
yrseg_rwi$X03Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.M06Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M06Ace.lm$x), seg1.M06Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M06Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M06Ace.lm$x), seg2.M06Ace.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M06Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M06Ace.lm$x), seg3.M06Ace.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M06Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M06Ace.lm$x), seg4.M06Ace.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M06Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M06Ace.lm$x), seg5.M06Ace.lm$coefficients[2], 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M06Ace.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M06Ace.lm$x), seg6.M06Ace.lm$coefficients[2],
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.M06Ace.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.M06Ace.lm$x), seg7.M06Ace.lm$coefficients[2],
                                                                        NA)))))))

# Remember to change sig for this section
yrseg_rwi$X03Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.M06Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M06Ace.lm$x), negsig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M06Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M06Ace.lm$x), possig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M06Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M06Ace.lm$x), negsig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M06Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M06Ace.lm$x), possig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M06Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M06Ace.lm$x), negsig, 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M06Ace.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M06Ace.lm$x), possig,
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.M06Ace.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.M06Ace.lm$x), negsig, 
                                                                        NA)))))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X03Ace_sigc[yrseg_rwi$X03Ace_sig == insig] = "black"
yrseg_rwi$X03Ace_sigc[yrseg_rwi$X03Ace_sig == possig] = "darkgreen"
yrseg_rwi$X03Ace_sigc[yrseg_rwi$X03Ace_sig == negsig] = "red3"

yrseg_rwi$X03Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.M06Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk1.M06Ace] = 1
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk2.M06Ace] = 1
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk3.M06Ace] = 1
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk4.M06Ace] = 1
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk5.M06Ace] = 1
yrseg_rwi$X03Ace_brk[yrseg_rwi$Year == brk6.M06Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X03Ace_slp, pch = 15, col = yrseg_rwi$X03Ace_sigc)
abline(0,0)
abline(v = brk1.M06Ace, lty = 2)
abline(v = brk2.M06Ace, lty = 2)
abline(v = brk3.M06Ace, lty = 2)
abline(v = brk4.M06Ace, lty = 2)
abline(v = brk5.M06Ace, lty = 2)
abline(v = brk6.M06Ace, lty = 2)

sum(yrseg_rwi$X03Ace_slp, na.rm = TRUE)

# M07Tsu / 04Tsu
yrseg_rwi$X04Tsu_slp = ifelse(yrseg_rwi$Year >= min(seg1.M07Tsu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M07Tsu.lm$x), seg1.M07Tsu.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M07Tsu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M07Tsu.lm$x), seg2.M07Tsu.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M07Tsu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M07Tsu.lm$x), seg3.M07Tsu.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M07Tsu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M07Tsu.lm$x), seg4.M07Tsu.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M07Tsu.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M07Tsu.lm$x), seg5.M07Tsu.lm$coefficients[2], 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M07Tsu.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M07Tsu.lm$x), seg6.M07Tsu.lm$coefficients[2],
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.M07Tsu.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.M07Tsu.lm$x), seg7.M07Tsu.lm$coefficients[2],
                                                                        NA)))))))

# Remember to change sig for this section
yrseg_rwi$X04Tsu_sig = ifelse(yrseg_rwi$Year >= min(seg1.M07Tsu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M07Tsu.lm$x), insig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M07Tsu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M07Tsu.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M07Tsu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M07Tsu.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M07Tsu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M07Tsu.lm$x), negsig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M07Tsu.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M07Tsu.lm$x), insig, 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M07Tsu.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M07Tsu.lm$x), possig,
                                                                 ifelse(yrseg_rwi$Year >= min(seg7.M07Tsu.lm$x) + 1 & # Fill in Seg7 slope
                                                                          yrseg_rwi$Year <= max(seg7.M07Tsu.lm$x), negsig, 
                                                                        NA)))))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X04Tsu_sigc[yrseg_rwi$X04Tsu_sig == insig] = "black"
yrseg_rwi$X04Tsu_sigc[yrseg_rwi$X04Tsu_sig == possig] = "darkgreen"
yrseg_rwi$X04Tsu_sigc[yrseg_rwi$X04Tsu_sig == negsig] = "red3"

yrseg_rwi$X04Tsu_brk = ifelse(yrseg_rwi$Year >= min(seg1.M07Tsu.lm$x) + 1, 0, NA)
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk1.M07Tsu] = 1
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk2.M07Tsu] = 1
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk3.M07Tsu] = 1
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk4.M07Tsu] = 1
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk5.M07Tsu] = 1
yrseg_rwi$X04Tsu_brk[yrseg_rwi$Year == brk6.M07Tsu] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X04Tsu_slp, pch = 15, col = yrseg_rwi$X04Tsu_sigc)
abline(0,0)
abline(v = brk1.M07Tsu, lty = 2)
abline(v = brk2.M07Tsu, lty = 2)
abline(v = brk3.M07Tsu, lty = 2)
abline(v = brk4.M07Tsu, lty = 2)
abline(v = brk5.M07Tsu, lty = 2)
abline(v = brk6.M07Tsu, lty = 2)

sum(yrseg_rwi$X04Tsu_slp, na.rm = TRUE)

# F23Ace / 05Ace
yrseg_rwi$X05Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.F23Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F23Ace.lm$x), seg1.F23Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F23Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F23Ace.lm$x), seg2.F23Ace.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F23Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F23Ace.lm$x), seg3.F23Ace.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F23Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F23Ace.lm$x), seg4.F23Ace.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F23Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F23Ace.lm$x), seg5.F23Ace.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X05Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.F23Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F23Ace.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F23Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F23Ace.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F23Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F23Ace.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F23Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F23Ace.lm$x), negsig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F23Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F23Ace.lm$x), possig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X05Ace_sigc[yrseg_rwi$X05Ace_sig == insig] = "black"
yrseg_rwi$X05Ace_sigc[yrseg_rwi$X05Ace_sig == possig] = "darkgreen"
yrseg_rwi$X05Ace_sigc[yrseg_rwi$X05Ace_sig == negsig] = "red3"

yrseg_rwi$X05Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.F23Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X05Ace_brk[yrseg_rwi$Year == brk1.F23Ace] = 1
yrseg_rwi$X05Ace_brk[yrseg_rwi$Year == brk2.F23Ace] = 1
yrseg_rwi$X05Ace_brk[yrseg_rwi$Year == brk3.F23Ace] = 1
yrseg_rwi$X05Ace_brk[yrseg_rwi$Year == brk4.F23Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X05Ace_slp, pch = 15, col = yrseg_rwi$X05Ace_sigc)
abline(0,0)
abline(v = brk1.F23Ace, lty = 2)
abline(v = brk2.F23Ace, lty = 2)
abline(v = brk3.F23Ace, lty = 2)
abline(v = brk4.F23Ace, lty = 2)

sum(yrseg_rwi$X05Ace_slp, na.rm = TRUE)

# F21Dec / 06Dec
yrseg_rwi$X06Dec_slp = ifelse(yrseg_rwi$Year >= min(seg1.F21Dec.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F21Dec.lm$x), seg1.F21Dec.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F21Dec.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F21Dec.lm$x), seg2.F21Dec.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F21Dec.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F21Dec.lm$x), seg3.F21Dec.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F21Dec.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F21Dec.lm$x), seg4.F21Dec.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F21Dec.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F21Dec.lm$x), seg5.F21Dec.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X06Dec_sig = ifelse(yrseg_rwi$Year >= min(seg1.F21Dec.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F21Dec.lm$x), negsig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F21Dec.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F21Dec.lm$x), insig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F21Dec.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F21Dec.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F21Dec.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F21Dec.lm$x), insig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F21Dec.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F21Dec.lm$x), negsig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X06Dec_sigc[yrseg_rwi$X06Dec_sig == insig] = "black"
yrseg_rwi$X06Dec_sigc[yrseg_rwi$X06Dec_sig == possig] = "darkgreen"
yrseg_rwi$X06Dec_sigc[yrseg_rwi$X06Dec_sig == negsig] = "red3"

yrseg_rwi$X06Dec_brk = ifelse(yrseg_rwi$Year >= min(seg1.F21Dec.lm$x) + 1, 0, NA)
yrseg_rwi$X06Dec_brk[yrseg_rwi$Year == brk1.F21Dec] = 1
yrseg_rwi$X06Dec_brk[yrseg_rwi$Year == brk2.F21Dec] = 1
yrseg_rwi$X06Dec_brk[yrseg_rwi$Year == brk3.F21Dec] = 1
yrseg_rwi$X06Dec_brk[yrseg_rwi$Year == brk4.F21Dec] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X06Dec_slp, pch = 15, col = yrseg_rwi$X06Dec_sigc)
abline(0,0)
abline(v = brk1.F21Dec, lty = 2)
abline(v = brk2.F21Dec, lty = 2)
abline(v = brk3.F21Dec, lty = 2)
abline(v = brk4.F21Dec, lty = 2)

sum(yrseg_rwi$X06Dec_slp, na.rm = TRUE)

# M05Thu / 07Thu
yrseg_rwi$X07Thu_slp = ifelse(yrseg_rwi$Year >= min(seg1.M05Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M05Thu.lm$x), seg1.M05Thu.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M05Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M05Thu.lm$x), seg2.M05Thu.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M05Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M05Thu.lm$x), seg3.M05Thu.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M05Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M05Thu.lm$x), seg4.M05Thu.lm$coefficients[2],
                                                          NA))))

yrseg_rwi$X07Thu_sig = ifelse(yrseg_rwi$Year >= min(seg1.M05Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M05Thu.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M05Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M05Thu.lm$x), insig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M05Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M05Thu.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M05Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M05Thu.lm$x), insig,
                                                          NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X07Thu_sigc[yrseg_rwi$X07Thu_sig == insig] = "black"
yrseg_rwi$X07Thu_sigc[yrseg_rwi$X07Thu_sig == possig] = "darkgreen"
yrseg_rwi$X07Thu_sigc[yrseg_rwi$X07Thu_sig == negsig] = "red3"

yrseg_rwi$X07Thu_brk = ifelse(yrseg_rwi$Year >= min(seg1.M05Thu.lm$x) + 1, 0, NA)
yrseg_rwi$X07Thu_brk[yrseg_rwi$Year == brk1.M05Thu] = 1
yrseg_rwi$X07Thu_brk[yrseg_rwi$Year == brk2.M05Thu] = 1
yrseg_rwi$X07Thu_brk[yrseg_rwi$Year == brk3.M05Thu] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X07Thu_slp, pch = 15, col = yrseg_rwi$X07Thu_sigc)
abline(0,0)
abline(v = brk1.M05Thu, lty = 2)
abline(v = brk2.M05Thu, lty = 2)
abline(v = brk3.M05Thu, lty = 2)

sum(yrseg_rwi$X07Thu_slp, na.rm = TRUE)

# F07Ace / 08Ace
yrseg_rwi$X08Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.F07Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F07Ace.lm$x), seg1.F07Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F07Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F07Ace.lm$x), seg2.F07Ace.lm$coefficients[2],
                                                   NA))

yrseg_rwi$X08Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.F07Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F07Ace.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F07Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F07Ace.lm$x), negsig,
                                                   NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X08Ace_sigc[yrseg_rwi$X08Ace_sig == insig] = "black"
yrseg_rwi$X08Ace_sigc[yrseg_rwi$X08Ace_sig == possig] = "darkgreen"
yrseg_rwi$X08Ace_sigc[yrseg_rwi$X08Ace_sig == negsig] = "red3"

yrseg_rwi$X08Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.F07Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X08Ace_brk[yrseg_rwi$Year == brk1.F07Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X08Ace_slp, pch = 15, col = yrseg_rwi$X08Ace_sigc)
abline(0,0)
abline(v = brk1.F07Ace, lty = 2)

sum(yrseg_rwi$X08Ace_slp, na.rm = TRUE)

# M27Pin / 09Pin
yrseg_rwi$X09Pin_slp = ifelse(yrseg_rwi$Year >= min(seg1.M27Pin.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M27Pin.lm$x), seg1.M27Pin.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M27Pin.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M27Pin.lm$x), seg2.M27Pin.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M27Pin.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M27Pin.lm$x), seg3.M27Pin.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M27Pin.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M27Pin.lm$x), seg4.M27Pin.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M27Pin.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M27Pin.lm$x), seg5.M27Pin.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X09Pin_sig = ifelse(yrseg_rwi$Year >= min(seg1.M27Pin.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M27Pin.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M27Pin.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M27Pin.lm$x), insig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M27Pin.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M27Pin.lm$x), negsig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M27Pin.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M27Pin.lm$x), possig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M27Pin.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M27Pin.lm$x), insig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X09Pin_sigc[yrseg_rwi$X09Pin_sig == insig] = "black"
yrseg_rwi$X09Pin_sigc[yrseg_rwi$X09Pin_sig == possig] = "darkgreen"
yrseg_rwi$X09Pin_sigc[yrseg_rwi$X09Pin_sig == negsig] = "red3"

yrseg_rwi$X09Pin_brk = ifelse(yrseg_rwi$Year >= min(seg1.M27Pin.lm$x) + 1, 0, NA)
yrseg_rwi$X09Pin_brk[yrseg_rwi$Year == brk1.M27Pin] = 1
yrseg_rwi$X09Pin_brk[yrseg_rwi$Year == brk2.M27Pin] = 1
yrseg_rwi$X09Pin_brk[yrseg_rwi$Year == brk3.M27Pin] = 1
yrseg_rwi$X09Pin_brk[yrseg_rwi$Year == brk4.M27Pin] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X09Pin_slp, pch = 15, col = yrseg_rwi$X09Pin_sigc)
abline(0,0)
abline(v = brk1.M27Pin, lty = 2)
abline(v = brk2.M27Pin, lty = 2)
abline(v = brk3.M27Pin, lty = 2)
abline(v = brk4.M27Pin, lty = 2)

sum(yrseg_rwi$X09Pin_slp, na.rm = TRUE)

# M26Thu / 10Thu
yrseg_rwi$X10Thu_slp = ifelse(yrseg_rwi$Year >= min(seg1.M26Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M26Thu.lm$x), seg1.M26Thu.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M26Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M26Thu.lm$x), seg2.M26Thu.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M26Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M26Thu.lm$x), seg3.M26Thu.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M26Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M26Thu.lm$x), seg4.M26Thu.lm$coefficients[2],
                                                   NA))))

yrseg_rwi$X10Thu_sig = ifelse(yrseg_rwi$Year >= min(seg1.M26Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M26Thu.lm$x), insig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M26Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M26Thu.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M26Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M26Thu.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M26Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M26Thu.lm$x), insig,
                                                   NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X10Thu_sigc[yrseg_rwi$X10Thu_sig == insig] = "black"
yrseg_rwi$X10Thu_sigc[yrseg_rwi$X10Thu_sig == possig] = "darkgreen"
yrseg_rwi$X10Thu_sigc[yrseg_rwi$X10Thu_sig == negsig] = "red3"

yrseg_rwi$X10Thu_brk = ifelse(yrseg_rwi$Year >= min(seg1.M26Thu.lm$x) + 1, 0, NA)
yrseg_rwi$X10Thu_brk[yrseg_rwi$Year == brk1.M26Thu] = 1
yrseg_rwi$X10Thu_brk[yrseg_rwi$Year == brk2.M26Thu] = 1
yrseg_rwi$X10Thu_brk[yrseg_rwi$Year == brk3.M26Thu] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X10Thu_slp, pch = 15, col = yrseg_rwi$X10Thu_sigc)
abline(0,0)
abline(v = brk1.M26Thu, lty = 2)
abline(v = brk2.M26Thu, lty = 2)
abline(v = brk3.M26Thu, lty = 2)

sum(yrseg_rwi$X10Thu_slp, na.rm = TRUE)

# F25Ace / 11Ace
yrseg_rwi$X11Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.F25Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F25Ace.lm$x), seg1.F25Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F25Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F25Ace.lm$x), seg2.F25Ace.lm$coefficients[2],
                                     NA))

yrseg_rwi$X11Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.F25Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F25Ace.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F25Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F25Ace.lm$x), insig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X11Ace_sigc[yrseg_rwi$X11Ace_sig == insig] = "black"
yrseg_rwi$X11Ace_sigc[yrseg_rwi$X11Ace_sig == possig] = "darkgreen"
yrseg_rwi$X11Ace_sigc[yrseg_rwi$X11Ace_sig == negsig] = "red3"

yrseg_rwi$X11Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.F25Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X11Ace_brk[yrseg_rwi$Year == brk1.F25Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X11Ace_slp, pch = 15, col = yrseg_rwi$X11Ace_sigc)
abline(0,0)
abline(v = brk1.F25Ace, lty = 2)

sum(yrseg_rwi$X11Ace_slp, na.rm = TRUE)

# M01Pop / 12Pop
yrseg_rwi$X12Pop_slp = ifelse(yrseg_rwi$Year >= min(seg1.M01Pop.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M01Pop.lm$x), seg1.M01Pop.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M01Pop.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M01Pop.lm$x), seg2.M01Pop.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M01Pop.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M01Pop.lm$x), seg3.M01Pop.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M01Pop.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M01Pop.lm$x), seg4.M01Pop.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M01Pop.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M01Pop.lm$x), seg5.M01Pop.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X12Pop_sig = ifelse(yrseg_rwi$Year >= min(seg1.M01Pop.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M01Pop.lm$x), negsig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M01Pop.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M01Pop.lm$x), possig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M01Pop.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M01Pop.lm$x), negsig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M01Pop.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M01Pop.lm$x), possig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M01Pop.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M01Pop.lm$x), insig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X12Pop_sigc[yrseg_rwi$X12Pop_sig == insig] = "black"
yrseg_rwi$X12Pop_sigc[yrseg_rwi$X12Pop_sig == possig] = "darkgreen"
yrseg_rwi$X12Pop_sigc[yrseg_rwi$X12Pop_sig == negsig] = "red3"

yrseg_rwi$X12Pop_brk = ifelse(yrseg_rwi$Year >= min(seg1.M01Pop.lm$x) + 1, 0, NA)
yrseg_rwi$X12Pop_brk[yrseg_rwi$Year == brk1.M01Pop] = 1
yrseg_rwi$X12Pop_brk[yrseg_rwi$Year == brk2.M01Pop] = 1
yrseg_rwi$X12Pop_brk[yrseg_rwi$Year == brk3.M01Pop] = 1
yrseg_rwi$X12Pop_brk[yrseg_rwi$Year == brk4.M01Pop] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X12Pop_slp, pch = 15, col = yrseg_rwi$X12Pop_sigc)
abline(0,0)
abline(v = brk1.M01Pop, lty = 2)
abline(v = brk2.M01Pop, lty = 2)
abline(v = brk3.M01Pop, lty = 2)
abline(v = brk4.M01Pop, lty = 2)

sum(yrseg_rwi$X12Pop_slp, na.rm = TRUE)

# F33Pic / 13Pic
yrseg_rwi$X13Pic_slp = ifelse(yrseg_rwi$Year >= min(seg1.F33Pic.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F33Pic.lm$x), seg1.F33Pic.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F33Pic.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F33Pic.lm$x), seg2.F33Pic.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F33Pic.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F33Pic.lm$x), seg3.F33Pic.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F33Pic.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F33Pic.lm$x), seg4.F33Pic.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F33Pic.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F33Pic.lm$x), seg5.F33Pic.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X13Pic_sig = ifelse(yrseg_rwi$Year >= min(seg1.F33Pic.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F33Pic.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F33Pic.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F33Pic.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F33Pic.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F33Pic.lm$x), insig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F33Pic.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F33Pic.lm$x), insig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F33Pic.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F33Pic.lm$x), negsig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X13Pic_sigc[yrseg_rwi$X13Pic_sig == insig] = "black"
yrseg_rwi$X13Pic_sigc[yrseg_rwi$X13Pic_sig == possig] = "darkgreen"
yrseg_rwi$X13Pic_sigc[yrseg_rwi$X13Pic_sig == negsig] = "red3"

yrseg_rwi$X13Pic_brk = ifelse(yrseg_rwi$Year >= min(seg1.F33Pic.lm$x) + 1, 0, NA)
yrseg_rwi$X13Pic_brk[yrseg_rwi$Year == brk1.F33Pic] = 1
yrseg_rwi$X13Pic_brk[yrseg_rwi$Year == brk2.F33Pic] = 1
yrseg_rwi$X13Pic_brk[yrseg_rwi$Year == brk3.F33Pic] = 1
yrseg_rwi$X13Pic_brk[yrseg_rwi$Year == brk4.F33Pic] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X13Pic_slp, pch = 15, col = yrseg_rwi$X13Pic_sigc)
abline(0,0)
abline(v = brk1.F33Pic, lty = 2)
abline(v = brk2.F33Pic, lty = 2)
abline(v = brk3.F33Pic, lty = 2)
abline(v = brk4.F33Pic, lty = 2)

sum(yrseg_rwi$X13Pic_slp, na.rm = TRUE)

# M20Thu / 14Thu
yrseg_rwi$X14Thu_slp = ifelse(yrseg_rwi$Year >= min(lm.M20Thu$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(lm.M20Thu$model$year), lm.M20Thu$coefficients[2], 
                                     NA)

yrseg_rwi$X14Thu_sig = ifelse(yrseg_rwi$Year >= min(lm.M20Thu$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(lm.M20Thu$model$year), insig, 
                                     NA)
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X14Thu_sigc[yrseg_rwi$X14Thu_sig == insig] = "black"
yrseg_rwi$X14Thu_sigc[yrseg_rwi$X14Thu_sig == possig] = "darkgreen"
yrseg_rwi$X14Thu_sigc[yrseg_rwi$X14Thu_sig == negsig] = "red3"

yrseg_rwi$X14Thu_brk = ifelse(yrseg_rwi$Year >= min(lm.M20Thu$model$year) + 1, 0, NA)
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X14Thu_slp, pch = 15, col = yrseg_rwi$X14Thu_sigc)
abline(0,0)

sum(yrseg_rwi$X14Thu_slp, na.rm = TRUE)

# F30Bet / 15Bet
yrseg_rwi$X15Bet_slp = ifelse(yrseg_rwi$Year >= min(seg1.F30Bet.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F30Bet.lm$x), seg1.F30Bet.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F30Bet.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F30Bet.lm$x), seg2.F30Bet.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F30Bet.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F30Bet.lm$x), seg3.F30Bet.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F30Bet.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F30Bet.lm$x), seg4.F30Bet.lm$coefficients[2],
                                                   NA))))

yrseg_rwi$X15Bet_sig = ifelse(yrseg_rwi$Year >= min(seg1.F30Bet.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F30Bet.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F30Bet.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F30Bet.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F30Bet.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F30Bet.lm$x), insig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F30Bet.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F30Bet.lm$x), negsig,
                                                   NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X15Bet_sigc[yrseg_rwi$X15Bet_sig == insig] = "black"
yrseg_rwi$X15Bet_sigc[yrseg_rwi$X15Bet_sig == possig] = "darkgreen"
yrseg_rwi$X15Bet_sigc[yrseg_rwi$X15Bet_sig == negsig] = "red3"

yrseg_rwi$X15Bet_brk = ifelse(yrseg_rwi$Year >= min(seg1.F30Bet.lm$x) + 1, 0, NA)
yrseg_rwi$X15Bet_brk[yrseg_rwi$Year == brk1.F30Bet] = 1
yrseg_rwi$X15Bet_brk[yrseg_rwi$Year == brk2.F30Bet] = 1
yrseg_rwi$X15Bet_brk[yrseg_rwi$Year == brk3.F30Bet] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X15Bet_slp, pch = 15, col = yrseg_rwi$X15Bet_sigc)
abline(0,0)
abline(v = brk1.F30Bet, lty = 2)
abline(v = brk2.F30Bet, lty = 2)
abline(v = brk3.F30Bet, lty = 2)

sum(yrseg_rwi$X15Bet_slp, na.rm = TRUE)

# M17Thu / 16Thu
yrseg_rwi$X16Thu_slp = ifelse(yrseg_rwi$Year >= min(seg1.M17Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M17Thu.lm$x), seg1.M17Thu.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M17Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M17Thu.lm$x), seg2.M17Thu.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M17Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M17Thu.lm$x), seg3.M17Thu.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M17Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M17Thu.lm$x), seg4.M17Thu.lm$coefficients[2],
                                                   NA))))

yrseg_rwi$X16Thu_sig = ifelse(yrseg_rwi$Year >= min(seg1.M17Thu.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M17Thu.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M17Thu.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M17Thu.lm$x), negsig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M17Thu.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M17Thu.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M17Thu.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M17Thu.lm$x), negsig,
                                                   NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X16Thu_sigc[yrseg_rwi$X16Thu_sig == insig] = "black"
yrseg_rwi$X16Thu_sigc[yrseg_rwi$X16Thu_sig == possig] = "darkgreen"
yrseg_rwi$X16Thu_sigc[yrseg_rwi$X16Thu_sig == negsig] = "red3"

yrseg_rwi$X16Thu_brk = ifelse(yrseg_rwi$Year >= min(seg1.M17Thu.lm$x) + 1, 0, NA)
yrseg_rwi$X16Thu_brk[yrseg_rwi$Year == brk1.M17Thu] = 1
yrseg_rwi$X16Thu_brk[yrseg_rwi$Year == brk2.M17Thu] = 1
yrseg_rwi$X16Thu_brk[yrseg_rwi$Year == brk3.M17Thu] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X16Thu_slp, pch = 15, col = yrseg_rwi$X16Thu_sigc)
abline(0,0)
abline(v = brk1.M17Thu, lty = 2)
abline(v = brk2.M17Thu, lty = 2)
abline(v = brk3.M17Thu, lty = 2)

sum(yrseg_rwi$X16Thu_slp, na.rm = TRUE)

# NEW 4 SITES
# F15Ace / 02Ace
yrseg_rwi$X02Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.F15Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F15Ace.lm$x), seg1.F15Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F15Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F15Ace.lm$x), seg2.F15Ace.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F15Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F15Ace.lm$x), seg3.F15Ace.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F15Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F15Ace.lm$x), seg4.F15Ace.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F15Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F15Ace.lm$x), seg5.F15Ace.lm$coefficients[2], 
                                                          NA)))))

yrseg_rwi$X02Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.F15Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F15Ace.lm$x), negsig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F15Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F15Ace.lm$x), possig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F15Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F15Ace.lm$x), negsig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F15Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F15Ace.lm$x), possig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.F15Ace.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.F15Ace.lm$x), negsig, 
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X02Ace_sigc[yrseg_rwi$X02Ace_sig == insig] = "black"
yrseg_rwi$X02Ace_sigc[yrseg_rwi$X02Ace_sig == possig] = "darkgreen"
yrseg_rwi$X02Ace_sigc[yrseg_rwi$X02Ace_sig == negsig] = "red3"

yrseg_rwi$X02Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.F15Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X02Ace_brk[yrseg_rwi$Year == brk1.F15Ace] = 1
yrseg_rwi$X02Ace_brk[yrseg_rwi$Year == brk2.F15Ace] = 1
yrseg_rwi$X02Ace_brk[yrseg_rwi$Year == brk3.F15Ace] = 1
yrseg_rwi$X02Ace_brk[yrseg_rwi$Year == brk4.F15Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X02Ace_slp, pch = 15, col = yrseg_rwi$X02Ace_sigc)
abline(0,0)
abline(v = brk1.F15Ace, lty = 2)
abline(v = brk2.F15Ace, lty = 2)
abline(v = brk3.F15Ace, lty = 2)
abline(v = brk4.F15Ace, lty = 2)

sum(yrseg_rwi$X02Ace_slp, na.rm = TRUE)

# M06Que / 03Que
yrseg_rwi$X03Que_slp = ifelse(yrseg_rwi$Year >= min(seg1.M06Que.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M06Que.lm$x), seg1.M06Que.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.M06Que.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M06Que.lm$x), seg2.M06Que.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.M06Que.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M06Que.lm$x), seg3.M06Que.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.M06Que.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M06Que.lm$x), seg4.M06Que.lm$coefficients[2],
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M06Que.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M06Que.lm$x), seg5.M06Que.lm$coefficients[2],
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M06Que.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M06Que.lm$x), seg6.M06Que.lm$coefficients[2],
                                                          NA))))))

yrseg_rwi$X03Que_sig = ifelse(yrseg_rwi$Year >= min(seg1.M06Que.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.M06Que.lm$x), negsig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.M06Que.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.M06Que.lm$x), possig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.M06Que.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.M06Que.lm$x), negsig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.M06Que.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.M06Que.lm$x), possig,
                                                   ifelse(yrseg_rwi$Year >= min(seg5.M06Que.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_rwi$Year <= max(seg5.M06Que.lm$x), insig, 
                                                          ifelse(yrseg_rwi$Year >= min(seg6.M06Que.lm$x) + 1 & # Fill in Seg6 slope
                                                                   yrseg_rwi$Year <= max(seg6.M06Que.lm$x), insig, 
                                                          NA))))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X03Que_sigc[yrseg_rwi$X03Que_sig == insig] = "black"
yrseg_rwi$X03Que_sigc[yrseg_rwi$X03Que_sig == possig] = "darkgreen"
yrseg_rwi$X03Que_sigc[yrseg_rwi$X03Que_sig == negsig] = "red3"

yrseg_rwi$X03Que_brk = ifelse(yrseg_rwi$Year >= min(seg1.M06Que.lm$x) + 1, 0, NA)
yrseg_rwi$X03Que_brk[yrseg_rwi$Year == brk1.M06Que] = 1
yrseg_rwi$X03Que_brk[yrseg_rwi$Year == brk2.M06Que] = 1
yrseg_rwi$X03Que_brk[yrseg_rwi$Year == brk3.M06Que] = 1
yrseg_rwi$X03Que_brk[yrseg_rwi$Year == brk4.M06Que] = 1
yrseg_rwi$X03Que_brk[yrseg_rwi$Year == brk5.M06Que] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X03Que_slp, pch = 15, col = yrseg_rwi$X03Que_sigc)
abline(0,0)
abline(v = brk1.M06Que, lty = 2)
abline(v = brk2.M06Que, lty = 2)
abline(v = brk3.M06Que, lty = 2)
abline(v = brk4.M06Que, lty = 2)
abline(v = brk5.M06Que, lty = 2)

sum(yrseg_rwi$X03Que_slp, na.rm = TRUE)

# M26Dec / 10Dec
yrseg_rwi$X10Dec_slp = ifelse(yrseg_rwi$Year >= min(lm.M26Dec$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(lm.M26Dec$model$year), lm.M26Dec$coefficients[2], 
                              NA)

yrseg_rwi$X10Dec_sig = ifelse(yrseg_rwi$Year >= min(lm.M26Dec$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(lm.M26Dec$model$year), insig, 
                              NA)
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X10Dec_sigc[yrseg_rwi$X10Dec_sig == insig] = "black"
yrseg_rwi$X10Dec_sigc[yrseg_rwi$X10Dec_sig == possig] = "darkgreen"
yrseg_rwi$X10Dec_sigc[yrseg_rwi$X10Dec_sig == negsig] = "red3"

yrseg_rwi$X10Dec_brk = ifelse(yrseg_rwi$Year >= min(lm.M26Dec$model$year) + 1, 0, NA)
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X10Dec_slp, pch = 15, col = yrseg_rwi$X10Dec_sigc)
abline(0,0)

sum(yrseg_rwi$X10Dec_slp, na.rm = TRUE)

# F30Ace / 15Ace
yrseg_rwi$X15Ace_slp = ifelse(yrseg_rwi$Year >= min(seg1.F30Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F30Ace.lm$x), seg1.F30Ace.lm$coefficients[2], 
                              ifelse(yrseg_rwi$Year >= min(seg2.F30Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F30Ace.lm$x), seg2.F30Ace.lm$coefficients[2],
                                     ifelse(yrseg_rwi$Year >= min(seg3.F30Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F30Ace.lm$x), seg3.F30Ace.lm$coefficients[2],
                                            ifelse(yrseg_rwi$Year >= min(seg4.F30Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F30Ace.lm$x), seg4.F30Ace.lm$coefficients[2],
                                                   NA))))

yrseg_rwi$X15Ace_sig = ifelse(yrseg_rwi$Year >= min(seg1.F30Ace.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_rwi$Year <= max(seg1.F30Ace.lm$x), possig, 
                              ifelse(yrseg_rwi$Year >= min(seg2.F30Ace.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_rwi$Year <= max(seg2.F30Ace.lm$x), insig,
                                     ifelse(yrseg_rwi$Year >= min(seg3.F30Ace.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_rwi$Year <= max(seg3.F30Ace.lm$x), possig,
                                            ifelse(yrseg_rwi$Year >= min(seg4.F30Ace.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_rwi$Year <= max(seg4.F30Ace.lm$x), negsig,
                                                   NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_rwi$X15Ace_sigc[yrseg_rwi$X15Ace_sig == insig] = "black"
yrseg_rwi$X15Ace_sigc[yrseg_rwi$X15Ace_sig == possig] = "darkgreen"
yrseg_rwi$X15Ace_sigc[yrseg_rwi$X15Ace_sig == negsig] = "red3"

yrseg_rwi$X15Ace_brk = ifelse(yrseg_rwi$Year >= min(seg1.F30Ace.lm$x) + 1, 0, NA)
yrseg_rwi$X15Ace_brk[yrseg_rwi$Year == brk1.F30Ace] = 1
yrseg_rwi$X15Ace_brk[yrseg_rwi$Year == brk2.F30Ace] = 1
yrseg_rwi$X15Ace_brk[yrseg_rwi$Year == brk3.F30Ace] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_rwi$Year, yrseg_rwi$X15Ace_slp, pch = 15, col = yrseg_rwi$X15Ace_sigc)
abline(0,0)
abline(v = brk1.F30Ace, lty = 2)
abline(v = brk2.F30Ace, lty = 2)
abline(v = brk3.F30Ace, lty = 2)

sum(yrseg_rwi$X15Ace_slp, na.rm = TRUE)

#write.csv(yrseg_rwi, "YearlySegmented_RWI1_20.csv")

### LTS ###
yrseg_lts = read.csv("YearlySegmented_LTS.csv")

# M13LTS / 01LTS
yrseg_lts$X01LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM13.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM13.lm$x), seg1.ltsM13.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM13.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM13.lm$x), seg2.ltsM13.lm$coefficients[2],
                                     NA))

yrseg_lts$X01LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM13.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM13.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM13.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM13.lm$x), negsig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X01LTS_sigc[yrseg_lts$X01LTS_sig == insig] = "black"
yrseg_lts$X01LTS_sigc[yrseg_lts$X01LTS_sig == possig] = "darkgreen"
yrseg_lts$X01LTS_sigc[yrseg_lts$X01LTS_sig == negsig] = "red3"

yrseg_lts$X01LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM13.lm$x) + 1, 0, NA)
yrseg_lts$X01LTS_brk[yrseg_lts$Year == brk1.ltsM13] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X01LTS_slp, pch = 15, col = yrseg_lts$X01LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM13, lty = 2)

sum(yrseg_lts$X01LTS_slp, na.rm = TRUE)

# F15LTS / 02LTS
yrseg_lts$X02LTS_slp = ifelse(yrseg_lts$Year >= min(lm.ltsF15$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(lm.ltsF15$model$year), lm.ltsF15$coefficients[2], 
                                     NA)

yrseg_lts$X02LTS_sig = ifelse(yrseg_lts$Year >= min(lm.ltsF15$model$year) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(lm.ltsF15$model$year), negsig, 
                                     NA)
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X02LTS_sigc[yrseg_lts$X02LTS_sig == insig] = "black"
yrseg_lts$X02LTS_sigc[yrseg_lts$X02LTS_sig == possig] = "darkgreen"
yrseg_lts$X02LTS_sigc[yrseg_lts$X02LTS_sig == negsig] = "red3"

yrseg_lts$X02LTS_brk = ifelse(yrseg_lts$Year >= min(lm.ltsF15$model$year) + 1, 0, NA)
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X02LTS_slp, pch = 15, col = yrseg_lts$X02LTS_sigc)
abline(0,0)

sum(yrseg_lts$X02LTS_slp, na.rm = TRUE)

# M06LTS / 03LTS
yrseg_lts$X03LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM06.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM06.lm$x), seg1.ltsM06.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM06.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM06.lm$x), seg2.ltsM06.lm$coefficients[2],
                                     NA))

yrseg_lts$X03LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM06.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM06.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM06.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM06.lm$x), insig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X03LTS_sigc[yrseg_lts$X03LTS_sig == insig] = "black"
yrseg_lts$X03LTS_sigc[yrseg_lts$X03LTS_sig == possig] = "darkgreen"
yrseg_lts$X03LTS_sigc[yrseg_lts$X03LTS_sig == negsig] = "red3"

yrseg_lts$X03LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM06.lm$x) + 1, 0, NA)
yrseg_lts$X03LTS_brk[yrseg_lts$Year == brk1.ltsM06] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X03LTS_slp, pch = 15, col = yrseg_lts$X03LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM06, lty = 2)

sum(yrseg_lts$X03LTS_slp, na.rm = TRUE)

# M07LTS / 04LTS
yrseg_lts$X04LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM07.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM07.lm$x), seg1.ltsM07.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM07.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM07.lm$x), seg2.ltsM07.lm$coefficients[2],
                                     NA))

yrseg_lts$X04LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM07.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM07.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM07.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM07.lm$x), insig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X04LTS_sigc[yrseg_lts$X04LTS_sig == insig] = "black"
yrseg_lts$X04LTS_sigc[yrseg_lts$X04LTS_sig == possig] = "darkgreen"
yrseg_lts$X04LTS_sigc[yrseg_lts$X04LTS_sig == negsig] = "red3"

yrseg_lts$X04LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM07.lm$x) + 1, 0, NA)
yrseg_lts$X04LTS_brk[yrseg_lts$Year == brk1.ltsM07] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X04LTS_slp, pch = 15, col = yrseg_lts$X04LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM07, lty = 2)

sum(yrseg_lts$X04LTS_slp, na.rm = TRUE)

# F23LTS / 05LTS
yrseg_lts$X05LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF23.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF23.lm$x), seg1.ltsF23.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF23.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF23.lm$x), seg2.ltsF23.lm$coefficients[2],
                                     NA))

yrseg_lts$X05LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF23.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF23.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF23.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF23.lm$x), negsig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X05LTS_sigc[yrseg_lts$X05LTS_sig == insig] = "black"
yrseg_lts$X05LTS_sigc[yrseg_lts$X05LTS_sig == possig] = "darkgreen"
yrseg_lts$X05LTS_sigc[yrseg_lts$X05LTS_sig == negsig] = "red3"

yrseg_lts$X05LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF23.lm$x) + 1, 0, NA)
yrseg_lts$X05LTS_brk[yrseg_lts$Year == brk1.ltsF23] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X05LTS_slp, pch = 15, col = yrseg_lts$X05LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF23, lty = 2)

sum(yrseg_lts$X05LTS_slp, na.rm = TRUE)

# F21LTS / 06LTS
yrseg_lts$X06LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF21.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF21.lm$x), seg1.ltsF21.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF21.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF21.lm$x), seg2.ltsF21.lm$coefficients[2],
                                     NA))

yrseg_lts$X06LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF21.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF21.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF21.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF21.lm$x), negsig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X06LTS_sigc[yrseg_lts$X06LTS_sig == insig] = "black"
yrseg_lts$X06LTS_sigc[yrseg_lts$X06LTS_sig == possig] = "darkgreen"
yrseg_lts$X06LTS_sigc[yrseg_lts$X06LTS_sig == negsig] = "red3"

yrseg_lts$X06LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF21.lm$x) + 1, 0, NA)
yrseg_lts$X06LTS_brk[yrseg_lts$Year == brk1.ltsF21] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X06LTS_slp, pch = 15, col = yrseg_lts$X06LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF21, lty = 2)

sum(yrseg_lts$X06LTS_slp, na.rm = TRUE)

# M05LTS / 07LTS
yrseg_lts$X07LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM05.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM05.lm$x), seg1.ltsM05.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM05.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM05.lm$x), seg2.ltsM05.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM05.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM05.lm$x), seg3.ltsM05.lm$coefficients[2],
                                     NA)))

yrseg_lts$X07LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM05.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM05.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM05.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM05.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM05.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM05.lm$x), negsig,
                                     NA)))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X07LTS_sigc[yrseg_lts$X07LTS_sig == insig] = "black"
yrseg_lts$X07LTS_sigc[yrseg_lts$X07LTS_sig == possig] = "darkgreen"
yrseg_lts$X07LTS_sigc[yrseg_lts$X07LTS_sig == negsig] = "red3"

yrseg_lts$X07LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM05.lm$x) + 1, 0, NA)
yrseg_lts$X07LTS_brk[yrseg_lts$Year == brk1.ltsM05] = 1
yrseg_lts$X07LTS_brk[yrseg_lts$Year == brk2.ltsM05] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X07LTS_slp, pch = 15, col = yrseg_lts$X07LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM05, lty = 2)
abline(v = brk2.ltsM05, lty = 2)

sum(yrseg_lts$X07LTS_slp, na.rm = TRUE)

# F07LTS / 08LTS
yrseg_lts$X08LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF07.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF07.lm$x), seg1.ltsF07.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF07.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF07.lm$x), seg2.ltsF07.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF07.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF07.lm$x), seg3.ltsF07.lm$coefficients[2],
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF07.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF07.lm$x), seg4.ltsF07.lm$coefficients[2],
                                            NA))))

yrseg_lts$X08LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF07.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF07.lm$x), insig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF07.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF07.lm$x), insig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF07.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF07.lm$x), possig,
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF07.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF07.lm$x), negsig,
                                            NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X08LTS_sigc[yrseg_lts$X08LTS_sig == insig] = "black"
yrseg_lts$X08LTS_sigc[yrseg_lts$X08LTS_sig == possig] = "darkgreen"
yrseg_lts$X08LTS_sigc[yrseg_lts$X08LTS_sig == negsig] = "red3"

yrseg_lts$X08LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF07.lm$x) + 1, 0, NA)
yrseg_lts$X08LTS_brk[yrseg_lts$Year == brk1.ltsF07] = 1
yrseg_lts$X08LTS_brk[yrseg_lts$Year == brk2.ltsF07] = 1
yrseg_lts$X08LTS_brk[yrseg_lts$Year == brk3.ltsF07] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X08LTS_slp, pch = 15, col = yrseg_lts$X08LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF07, lty = 2)
abline(v = brk2.ltsF07, lty = 2)
abline(v = brk3.ltsF07, lty = 2)

sum(yrseg_lts$X08LTS_slp, na.rm = TRUE)

# M27LTS / 09LTS
yrseg_lts$X09LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM27.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM27.lm$x), seg1.ltsM27.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM27.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM27.lm$x), seg2.ltsM27.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM27.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM27.lm$x), seg3.ltsM27.lm$coefficients[2],
                                            NA)))

yrseg_lts$X09LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM27.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM27.lm$x), insig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM27.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM27.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM27.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM27.lm$x), negsig,
                                            NA)))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X09LTS_sigc[yrseg_lts$X09LTS_sig == insig] = "black"
yrseg_lts$X09LTS_sigc[yrseg_lts$X09LTS_sig == possig] = "darkgreen"
yrseg_lts$X09LTS_sigc[yrseg_lts$X09LTS_sig == negsig] = "red3"

yrseg_lts$X09LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM27.lm$x) + 1, 0, NA)
yrseg_lts$X09LTS_brk[yrseg_lts$Year == brk1.ltsM27] = 1
yrseg_lts$X09LTS_brk[yrseg_lts$Year == brk2.ltsM27] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X09LTS_slp, pch = 15, col = yrseg_lts$X09LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM27, lty = 2)
abline(v = brk2.ltsM27, lty = 2)

sum(yrseg_lts$X09LTS_slp, na.rm = TRUE)

# M26LTS / 10LTS
yrseg_lts$X10LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM26.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM26.lm$x), seg1.ltsM26.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM26.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM26.lm$x), seg2.ltsM26.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM26.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM26.lm$x), seg3.ltsM26.lm$coefficients[2],
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsM26.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsM26.lm$x), seg4.ltsM26.lm$coefficients[2],
                                                   NA))))

yrseg_lts$X10LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM26.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM26.lm$x), negsig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM26.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM26.lm$x), insig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM26.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM26.lm$x), possig,
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsM26.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsM26.lm$x), negsig,
                                                   NA))))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X10LTS_sigc[yrseg_lts$X10LTS_sig == insig] = "black"
yrseg_lts$X10LTS_sigc[yrseg_lts$X10LTS_sig == possig] = "darkgreen"
yrseg_lts$X10LTS_sigc[yrseg_lts$X10LTS_sig == negsig] = "red3"

yrseg_lts$X10LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM26.lm$x) + 1, 0, NA)
yrseg_lts$X10LTS_brk[yrseg_lts$Year == brk1.ltsM26] = 1
yrseg_lts$X10LTS_brk[yrseg_lts$Year == brk2.ltsM26] = 1
yrseg_lts$X10LTS_brk[yrseg_lts$Year == brk3.ltsM26] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X10LTS_slp, pch = 15, col = yrseg_lts$X10LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM26, lty = 2)
abline(v = brk2.ltsM26, lty = 2)
abline(v = brk3.ltsM26, lty = 2)

sum(yrseg_lts$X10LTS_slp, na.rm = TRUE)

# F25LTS / 11LTS
yrseg_lts$X11LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF25.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF25.lm$x), seg1.ltsF25.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF25.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF25.lm$x), seg2.ltsF25.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF25.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF25.lm$x), seg3.ltsF25.lm$coefficients[2],
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF25.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF25.lm$x), seg4.ltsF25.lm$coefficients[2],
                                                   ifelse(yrseg_lts$Year >= min(seg5.ltsF25.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_lts$Year <= max(seg5.ltsF25.lm$x), seg5.ltsF25.lm$coefficients[2],
                                                   NA)))))

yrseg_lts$X11LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF25.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF25.lm$x), negsig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF25.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF25.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF25.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF25.lm$x), negsig,
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF25.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF25.lm$x), insig,
                                                   ifelse(yrseg_lts$Year >= min(seg5.ltsF25.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_lts$Year <= max(seg5.ltsF25.lm$x), negsig,
                                                   NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X11LTS_sigc[yrseg_lts$X11LTS_sig == insig] = "black"
yrseg_lts$X11LTS_sigc[yrseg_lts$X11LTS_sig == possig] = "darkgreen"
yrseg_lts$X11LTS_sigc[yrseg_lts$X11LTS_sig == negsig] = "red3"

yrseg_lts$X11LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF25.lm$x) + 1, 0, NA)
yrseg_lts$X11LTS_brk[yrseg_lts$Year == brk1.ltsF25] = 1
yrseg_lts$X11LTS_brk[yrseg_lts$Year == brk2.ltsF25] = 1
yrseg_lts$X11LTS_brk[yrseg_lts$Year == brk3.ltsF25] = 1
yrseg_lts$X11LTS_brk[yrseg_lts$Year == brk4.ltsF25] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X11LTS_slp, pch = 15, col = yrseg_lts$X11LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF25, lty = 2)
abline(v = brk2.ltsF25, lty = 2)
abline(v = brk3.ltsF25, lty = 2)
abline(v = brk4.ltsF25, lty = 2)

sum(yrseg_lts$X11LTS_slp, na.rm = TRUE)

# M01LTS / 12LTS
yrseg_lts$X12LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM01.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM01.lm$x), seg1.ltsM01.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM01.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM01.lm$x), seg2.ltsM01.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM01.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM01.lm$x), seg3.ltsM01.lm$coefficients[2],
                                            NA)))

yrseg_lts$X12LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM01.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM01.lm$x), insig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM01.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM01.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM01.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM01.lm$x), negsig,
                                            NA)))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X12LTS_sigc[yrseg_lts$X12LTS_sig == insig] = "black"
yrseg_lts$X12LTS_sigc[yrseg_lts$X12LTS_sig == possig] = "darkgreen"
yrseg_lts$X12LTS_sigc[yrseg_lts$X12LTS_sig == negsig] = "red3"

yrseg_lts$X12LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM01.lm$x) + 1, 0, NA)
yrseg_lts$X12LTS_brk[yrseg_lts$Year == brk1.ltsM01] = 1
yrseg_lts$X12LTS_brk[yrseg_lts$Year == brk2.ltsM01] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X12LTS_slp, pch = 15, col = yrseg_lts$X12LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM01, lty = 2)
abline(v = brk2.ltsM01, lty = 2)

sum(yrseg_lts$X12LTS_slp, na.rm = TRUE)

# F33LTS / 13LTS
yrseg_lts$X13LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF33.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF33.lm$x), seg1.ltsF33.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF33.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF33.lm$x), seg2.ltsF33.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF33.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF33.lm$x), seg3.ltsF33.lm$coefficients[2],
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF33.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF33.lm$x), seg4.ltsF33.lm$coefficients[2],
                                                   ifelse(yrseg_lts$Year >= min(seg5.ltsF33.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_lts$Year <= max(seg5.ltsF33.lm$x), seg5.ltsF33.lm$coefficients[2],
                                                          NA)))))

yrseg_lts$X13LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF33.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF33.lm$x), insig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF33.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF33.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsF33.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsF33.lm$x), negsig,
                                            ifelse(yrseg_lts$Year >= min(seg4.ltsF33.lm$x) + 1 & # Fill in Seg4 slope
                                                     yrseg_lts$Year <= max(seg4.ltsF33.lm$x), insig,
                                                   ifelse(yrseg_lts$Year >= min(seg5.ltsF33.lm$x) + 1 & # Fill in Seg5 slope
                                                            yrseg_lts$Year <= max(seg5.ltsF33.lm$x), negsig,
                                                          NA)))))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X13LTS_sigc[yrseg_lts$X13LTS_sig == insig] = "black"
yrseg_lts$X13LTS_sigc[yrseg_lts$X13LTS_sig == possig] = "darkgreen"
yrseg_lts$X13LTS_sigc[yrseg_lts$X13LTS_sig == negsig] = "red3"

yrseg_lts$X13LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF33.lm$x) + 1, 0, NA)
yrseg_lts$X13LTS_brk[yrseg_lts$Year == brk1.ltsF33] = 1
yrseg_lts$X13LTS_brk[yrseg_lts$Year == brk2.ltsF33] = 1
yrseg_lts$X13LTS_brk[yrseg_lts$Year == brk3.ltsF33] = 1
yrseg_lts$X13LTS_brk[yrseg_lts$Year == brk4.ltsF33] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X13LTS_slp, pch = 15, col = yrseg_lts$X13LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF33, lty = 2)
abline(v = brk2.ltsF33, lty = 2)
abline(v = brk3.ltsF33, lty = 2)
abline(v = brk4.ltsF33, lty = 2)

sum(yrseg_lts$X13LTS_slp, na.rm = TRUE)

# M14LTS / 14LTS
yrseg_lts$X14LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM20.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM20.lm$x), seg1.ltsM20.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM20.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM20.lm$x), seg2.ltsM20.lm$coefficients[2],
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM20.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM20.lm$x), seg3.ltsM20.lm$coefficients[2],
                                            NA)))

yrseg_lts$X14LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM20.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM20.lm$x), insig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM20.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM20.lm$x), possig,
                                     ifelse(yrseg_lts$Year >= min(seg3.ltsM20.lm$x) + 1 & # Fill in Seg3 slope
                                              yrseg_lts$Year <= max(seg3.ltsM20.lm$x), negsig,
                                            NA)))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X14LTS_sigc[yrseg_lts$X14LTS_sig == insig] = "black"
yrseg_lts$X14LTS_sigc[yrseg_lts$X14LTS_sig == possig] = "darkgreen"
yrseg_lts$X14LTS_sigc[yrseg_lts$X14LTS_sig == negsig] = "red3"

yrseg_lts$X14LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM20.lm$x) + 1, 0, NA)
yrseg_lts$X14LTS_brk[yrseg_lts$Year == brk1.ltsM20] = 1
yrseg_lts$X14LTS_brk[yrseg_lts$Year == brk2.ltsM20] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X14LTS_slp, pch = 15, col = yrseg_lts$X14LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM20, lty = 2)
abline(v = brk2.ltsM20, lty = 2)

sum(yrseg_lts$X14LTS_slp, na.rm = TRUE)

# F30LTS / 15LTS
yrseg_lts$X15LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsF30.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF30.lm$x), seg1.ltsF30.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF30.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF30.lm$x), seg2.ltsF30.lm$coefficients[2],
                                     NA))

yrseg_lts$X15LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsF30.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsF30.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsF30.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsF30.lm$x), insig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X15LTS_sigc[yrseg_lts$X15LTS_sig == insig] = "black"
yrseg_lts$X15LTS_sigc[yrseg_lts$X15LTS_sig == possig] = "darkgreen"
yrseg_lts$X15LTS_sigc[yrseg_lts$X15LTS_sig == negsig] = "red3"

yrseg_lts$X15LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsF30.lm$x) + 1, 0, NA)
yrseg_lts$X15LTS_brk[yrseg_lts$Year == brk1.ltsF30] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X15LTS_slp, pch = 15, col = yrseg_lts$X15LTS_sigc)
abline(0,0)
abline(v = brk1.ltsF30, lty = 2)

sum(yrseg_lts$X15LTS_slp, na.rm = TRUE)

# M17LTS / 16LTS
yrseg_lts$X16LTS_slp = ifelse(yrseg_lts$Year >= min(seg1.ltsM17.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM17.lm$x), seg1.ltsM17.lm$coefficients[2], 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM17.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM17.lm$x), seg2.ltsM17.lm$coefficients[2],
                                     NA))

yrseg_lts$X16LTS_sig = ifelse(yrseg_lts$Year >= min(seg1.ltsM17.lm$x) + 1 & # Fill in Seg1 slope
                                yrseg_lts$Year <= max(seg1.ltsM17.lm$x), possig, 
                              ifelse(yrseg_lts$Year >= min(seg2.ltsM17.lm$x) + 1 & # Fill in Seg2 slope
                                       yrseg_lts$Year <= max(seg2.ltsM17.lm$x), negsig,
                                     NA))
# possig = 1, negsig = -1, insig = 0  

yrseg_lts$X16LTS_sigc[yrseg_lts$X16LTS_sig == insig] = "black"
yrseg_lts$X16LTS_sigc[yrseg_lts$X16LTS_sig == possig] = "darkgreen"
yrseg_lts$X16LTS_sigc[yrseg_lts$X16LTS_sig == negsig] = "red3"

yrseg_lts$X16LTS_brk = ifelse(yrseg_lts$Year >= min(seg1.ltsM17.lm$x) + 1, 0, NA)
yrseg_lts$X16LTS_brk[yrseg_lts$Year == brk1.ltsM17] = 1
# breakpoint = 1, no breakpoint = 0

plot(yrseg_lts$Year, yrseg_lts$X16LTS_slp, pch = 15, col = yrseg_lts$X16LTS_sigc)
abline(0,0)
abline(v = brk1.ltsM17, lty = 2)

sum(yrseg_lts$X16LTS_slp, na.rm = TRUE)

#write.csv(yrseg_lts, "YearlySegmented_LTS1_20.csv")
#####

##### Stacked noisy RWI and LTS values #####
#####
combined_rwi = read.csv("FINAL TIMESERIES/RWI/combined_rwi.csv")

noise_rwi.p = ggplot() +
  geom_line(data = rwl.F07Ace.mne.crn, aes(x = 1943:2018, y = xxxstd), alpha = 0.25) + 
  geom_line(data = rwl.F15Que.mne.crn, aes(x = 1909:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.F21Dec.mne.crn, aes(x = 1916:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.F23Ace.mne.crn, aes(x = 1891:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.F25Ace.mne.crn, aes(x = 1936:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.F30Bet.mne.crn, aes(x = 1942:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.F33Pic.mne.crn, aes(x = 1958:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M01Pop.mne.crn, aes(x = 1951:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M05Thu.mne.crn, aes(x = 1932:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M06Ace.mne.crn, aes(x = 1899:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M07Tsu.mne.crn, aes(x = 1894:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M13Tsu.mne.crn, aes(x = 1902:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M17Thu.mne.crn, aes(x = 1937:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M20Thu.mne.crn, aes(x = 1936:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M26Thu.mne.crn, aes(x = 1904:2018, y = xxxstd), alpha = 0.25) +
  geom_line(data = rwl.M27Pin.mne.crn, aes(x = 1981:2018, y = xxxstd), alpha = 0.25) +
  #geom_line(data = combined_rwi, aes(x = Year, y = UrbAvg), lwd = 1, col = "darkblue") +
  #geom_line(data = combined_rwi, aes(x = Year, y = RurAvg), lwd = 1, col = "lightblue") +
  #geom_line(data = combined_rwi, aes(x = Year, y = ConAvg), lwd = 1, col = "red3") +
  #geom_line(data = combined_rwi, aes(x = Year, y = DecAvg), lwd = 1, col = "darkgreen") +
  #geom_line(data = combined_rwi, aes(x = Year, y = ThuAvg), lwd = 1, col = "red3") +
  #geom_line(data = combined_rwi, aes(x = Year, y = AceAvg), lwd = 1, col = "darkgreen") +
  geom_line(data = combined_rwi, aes(x = Year, y = OvAvg), lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0)) +
  coord_cartesian(ylim = c(0.25,1.75)) +
  annotate(geom = "text", x = 1902, y = 1.6, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
  
combined_lts = read.csv("FINAL TIMESERIES/LTS/combined_lts.csv")
noise_lts.p = ggplot() +
  geom_line(data = lts.F07_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F15_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F21_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F23_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F25_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F30_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.F33_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M01_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M05_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M06_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M07_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M13_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M17_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M20_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M26_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  geom_line(data = lts.M27_1, aes(x = year, y = Avg_CC_Median), alpha = 0.25) +
  #geom_line(data = combined_lts, aes(x = Year, y = UrbAvg), lwd = 1, col = "darkblue") +
  #geom_line(data = combined_lts, aes(x = Year, y = RurAvg), lwd = 1, col = "lightblue") +
  #geom_line(data = combined_lts, aes(x = Year, y = ConAvg), lwd = 1, col = "red3") +
  #geom_line(data = combined_lts, aes(x = Year, y = DecAvg), lwd = 1, col = "darkgreen") +
  #geom_line(data = combined_lts, aes(x = Year, y = ThuAvg), lwd = 1, col = "red3") +
  #geom_line(data = combined_lts, aes(x = Year, y = AceAvg), lwd = 1, col = "darkgreen") +
  geom_line(data = combined_lts, aes(x = Year, y = OvAvg), lwd = 1) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "%CC") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0), "cm"),
        legend.position = "none")

#####
noise_rwi.p / noise_lts.p

##### Histogram of breakpoint years #####
#####

### RWI ###
yrseg_rwi$brksum = rowSums(yrseg_rwi[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65)], na.rm = TRUE)
sum(yrseg_rwi$brksum)
plot(yrseg_rwi$Year, yrseg_rwi$brksum, type = "l")

rwi_brks = data_frame(c(brk1.F07Ace, brk1.F15Que, brk1.F21Dec, brk1.F23Ace, brk1.F25Ace, brk1.F30Bet, 
             brk1.F33Pic, brk1.M01Pop, brk1.M05Thu, brk1.M06Ace, brk1.M07Tsu, brk1.M13Tsu,
             brk1.M17Thu, brk1.M26Thu, brk1.M27Pin, brk2.F15Que, brk2.F21Dec, brk2.F23Ace,
             brk2.F30Bet, brk2.F33Pic, brk2.M01Pop, brk2.M05Thu, brk2.M06Ace, brk2.M07Tsu,
             brk2.M13Tsu, brk2.M17Thu, brk2.M26Thu, brk2.M27Pin, brk3.F15Que, brk3.F21Dec,
             brk3.F23Ace, brk3.F30Bet, brk3.F33Pic, brk3.M01Pop, brk3.M05Thu, brk3.M06Ace,
             brk3.M07Tsu, brk3.M13Tsu, brk3.M17Thu, brk3.M26Thu, brk3.M27Pin, brk4.F15Que,
             brk4.F21Dec, brk4.F23Ace, brk4.F33Pic, brk4.M01Pop, brk4.M06Ace, brk4.M07Tsu,
             brk4.M13Tsu, brk4.M27Pin, brk5.F15Que, brk5.M06Ace, brk5.M07Tsu, brk6.F15Que,
             brk6.M06Ace, brk6.M07Tsu))
rwi_brks$trend = c("red3", "red3", "black", "red3", "black", "red3",
                   "red3", "darkgreen", "black", "darkgreen", "red3", "red3",
                   "red3", "red3", "black", "darkgreen", "darkgreen", "darkgreen",
                   "black", "black", "red3", "darkgreen", "red3", "darkgreen",
                   "darkgreen", "darkgreen", "darkgreen", "red3", "black", "black",
                   "red3", "red3", "black", "darkgreen", "black", "darkgreen",
                   "red3", "darkgreen", "red3", "black", "darkgreen", "red3",
                   "red3", "darkgreen", "red3", "black", "red3", "black",
                   "black", "black", "darkgreen", "darkgreen", "darkgreen", "black",
                   "red3", "red3")

brk_rwi.p = ggplot(rwi_brks) + 
  #geom_histogram(aes(x = `c(...)`), breaks = c(1895,1900,1905,1910,1915,1920,1925,1930,1935,1940,
  #                                             1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,
  #                                             1995,2000,2005,2010,2015)) +
  geom_histogram(aes(x = `c(...)`, fill = trend), binwidth = 5) +
  scale_fill_manual(values = c("red3" = "red3", "darkgreen" = "darkgreen","black" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(RWI)", breaks = seq(1,7,2)) +
  annotate(geom = "text", x = 1902, y = 6.5, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")
# Look into coloring bar plots based on slope of post-breakpoint segment (black = insig etc.)

### LTS ###
yrseg_lts$brksum = rowSums(yrseg_lts[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65)], na.rm = TRUE)
sum(yrseg_lts$brksum)
plot(yrseg_lts$Year, yrseg_lts$brksum, type = "l")

lts_brks = data_frame(c(brk1.ltsF07, brk1.ltsF21, brk1.ltsF23, brk1.ltsF25, brk1.ltsF30, brk1.ltsF33,
                        brk1.ltsM01, brk1.ltsM05, brk1.ltsM06, brk1.ltsM07, brk1.ltsM13, brk1.ltsM17,
                        brk1.ltsM20, brk1.ltsM26, brk1.ltsM27, brk2.ltsF07, brk2.ltsF25, brk2.ltsF33,
                        brk2.ltsM01, brk2.ltsM05, brk2.ltsM20, brk2.ltsM26, brk2.ltsM27, brk3.ltsF07,
                        brk3.ltsF25, brk3.ltsF33, brk3.ltsM26, brk4.ltsF25, brk4.ltsF33))
lts_brks$trend = c("black", "red3", "red3", "darkgreen", "black", "darkgreen",
                   "darkgreen", "darkgreen", "black", "black", "red3", "red3",
                   "darkgreen", "black", "darkgreen", "darkgreen", "red3", "red3",
                   "red3", "red3", "red3", "darkgreen", "red3", "red3",
                   "black", "black", "red3", "red3", "red3")

brk_lts.p = ggplot(lts_brks) + 
  #geom_histogram(aes(x = `c(...)`), breaks = c(1895,1900,1905,1910,1915,1920,1925,1930,1935,1940,
  #                                             1945,1950,1955,1960,1965,1970,1975,1980,1985,1990,
  #                                             1995,2000,2005,2010,2015)) +
  geom_histogram(aes(x = `c(...)`, fill = trend), binwidth = 5, position = "stack") +
  scale_fill_manual(values = c("red3" = "red3", "darkgreen" = "darkgreen","black" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(%CC)", breaks = seq(1,7,2)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0), "cm"),
        legend.position = "none")
#####
brk_rwi.p / brk_lts.p

##### Stacked segments #####
#####

### RWI ###
# Sig columns need to be factors
yrseg_rwi$X01Tsu_sig = as.factor(yrseg_rwi$X01Tsu_sig)
yrseg_rwi$X02Que_sig = as.factor(yrseg_rwi$X02Que_sig)
yrseg_rwi$X04Ace_sig = as.factor(yrseg_rwi$X04Ace_sig)
yrseg_rwi$X05Tsu_sig = as.factor(yrseg_rwi$X05Tsu_sig)
yrseg_rwi$X11Ace_sig = as.factor(yrseg_rwi$X11Ace_sig)
yrseg_rwi$X12Dec_sig = as.factor(yrseg_rwi$X12Dec_sig)
yrseg_rwi$X13Thu_sig = as.factor(yrseg_rwi$X13Thu_sig)
yrseg_rwi$X14Ace_sig = as.factor(yrseg_rwi$X14Ace_sig)
yrseg_rwi$X15Pin_sig = as.factor(yrseg_rwi$X15Pin_sig)
yrseg_rwi$X17Thu_sig = as.factor(yrseg_rwi$X17Thu_sig)
yrseg_rwi$X19Ace_sig = as.factor(yrseg_rwi$X19Ace_sig)
yrseg_rwi$X20Pop_sig = as.factor(yrseg_rwi$X20Pop_sig)
yrseg_rwi$X23Pic_sig = as.factor(yrseg_rwi$X23Pic_sig)
yrseg_rwi$X25Thu_sig = as.factor(yrseg_rwi$X25Thu_sig)
yrseg_rwi$X27Bet_sig = as.factor(yrseg_rwi$X27Bet_sig)
yrseg_rwi$X28Thu_sig = as.factor(yrseg_rwi$X28Thu_sig)

# Add average columns
yrseg_rwi$OvAvg = rowMeans(yrseg_rwi[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62)], na.rm = TRUE)
yrseg_rwi$UrbAvg = rowMeans(yrseg_rwi[c(2,6,10,14,18,22,58,62)], na.rm = TRUE)
yrseg_rwi$RurAvg = rowMeans(yrseg_rwi[c(26,30,34,38,42,46,50,54)], na.rm = TRUE)
yrseg_rwi$ConAvg = rowMeans(yrseg_rwi[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_rwi$DecAvg = rowMeans(yrseg_rwi[c(6,10,18,22,30,42,46,58)], na.rm = TRUE)
yrseg_rwi$ThuAvg = rowMeans(yrseg_rwi[c(26,38,54,62)], na.rm = TRUE)
yrseg_rwi$AceAvg = rowMeans(yrseg_rwi[c(10,18,30,42)], na.rm = TRUE)

seg_rwi.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01Tsu_slp, color = X01Tsu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X02Que_slp, color = X02Que_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X04Ace_slp, color = X04Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X05Tsu_slp, color = X05Tsu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X11Ace_slp, color = X11Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X12Dec_slp, color = X12Dec_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X13Thu_slp, color = X13Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X14Ace_slp, color = X14Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X15Pin_slp, color = X15Pin_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X17Thu_slp, color = X17Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X19Ace_slp, color = X19Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X20Pop_slp, color = X20Pop_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X23Pic_slp, color = X23Pic_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X25Thu_slp, color = X25Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X27Bet_slp, color = X27Bet_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X28Thu_slp, color = X28Thu_sig), shape = 15, alpha = 0.5) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  #geom_line(aes(x = Year, y = OvAvg), color = "gray48", lwd = 1.5) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), breaks = seq(-0.2,0.3,0.1),
                     labels = c("",-0.1,0.0,0.1,0.2,0.3)) +
  annotate(geom = "text", x = 1902, y = 0.275, label = "C", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")

### LTS ###
# Sig columns need to be factors
yrseg_lts$X01LTS_sig = as.factor(yrseg_lts$X01LTS_sig)
yrseg_lts$X02LTS_sig = as.factor(yrseg_lts$X02LTS_sig)
yrseg_lts$X04LTS_sig = as.factor(yrseg_lts$X04LTS_sig)
yrseg_lts$X05LTS_sig = as.factor(yrseg_lts$X05LTS_sig)
yrseg_lts$X11LTS_sig = as.factor(yrseg_lts$X11LTS_sig)
yrseg_lts$X12LTS_sig = as.factor(yrseg_lts$X12LTS_sig)
yrseg_lts$X13LTS_sig = as.factor(yrseg_lts$X13LTS_sig)
yrseg_lts$X14LTS_sig = as.factor(yrseg_lts$X14LTS_sig)
yrseg_lts$X15LTS_sig = as.factor(yrseg_lts$X15LTS_sig)
yrseg_lts$X17LTS_sig = as.factor(yrseg_lts$X17LTS_sig)
yrseg_lts$X19LTS_sig = as.factor(yrseg_lts$X19LTS_sig)
yrseg_lts$X20LTS_sig = as.factor(yrseg_lts$X20LTS_sig)
yrseg_lts$X23LTS_sig = as.factor(yrseg_lts$X23LTS_sig)
yrseg_lts$X25LTS_sig = as.factor(yrseg_lts$X25LTS_sig)
yrseg_lts$X27LTS_sig = as.factor(yrseg_lts$X27LTS_sig)
yrseg_lts$X28LTS_sig = as.factor(yrseg_lts$X28LTS_sig)

yrseg_lts$OvAvg = rowMeans(yrseg_lts[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62)], na.rm = TRUE)
yrseg_lts$UrbAvg = rowMeans(yrseg_lts[c(2,6,10,14,18,22,58,62)], na.rm = TRUE)
yrseg_lts$RurAvg = rowMeans(yrseg_lts[c(26,30,34,38,42,46,50,54)], na.rm = TRUE)
yrseg_lts$ConAvg = rowMeans(yrseg_lts[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_lts$DecAvg = rowMeans(yrseg_lts[c(6,10,18,22,30,42,46,58)], na.rm = TRUE)
yrseg_lts$ThuAvg = rowMeans(yrseg_lts[c(26,38,54,62)], na.rm = TRUE)
yrseg_lts$AceAvg = rowMeans(yrseg_lts[c(10,18,30,42)], na.rm = TRUE)

seg_lts.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01LTS_slp, color = X01LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X02LTS_slp, color = X02LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X04LTS_slp, color = X04LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X05LTS_slp, color = X05LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X11LTS_slp, color = X11LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X12LTS_slp, color = X12LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X13LTS_slp, color = X13LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X14LTS_slp, color = X14LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X15LTS_slp, color = X15LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X17LTS_slp, color = X17LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X19LTS_slp, color = X19LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X20LTS_slp, color = X20LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X23LTS_slp, color = X23LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X25LTS_slp, color = X25LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X27LTS_slp, color = X27LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X28LTS_slp, color = X28LTS_sig), shape = 15, alpha = 0.5) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  #geom_line(aes(x = Year, y = OvAvg), color = "gray48", lwd = 1.5) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("%CC yr"^"-1"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0), "cm"),
        legend.position = "none")
#####
seg_rwi.p / seg_lts.p 

#### Avg of Stacked segments #####
#####
segavg_rwi.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1.5) + 
  #geom_line(aes(x = Year, y = RurAvg), color = "lightblue") + 
  #geom_line(aes(x = Year, y = UrbAvg), color = "darkblue") + 
  #geom_line(aes(x = Year, y = ConAvg), color = "red3") + 
  #geom_line(aes(x = Year, y = DecAvg), color = "darkgreen") + 
  #geom_line(aes(x = Year, y = ThuAvg), color = "red3") + 
  #geom_line(aes(x = Year, y = AceAvg), color = "darkgreen") + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0),
                     limits = c(-0.03,0.04)) +
  annotate(geom = "text", x = 1902, y = 0.0325, label = "D", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")

segavg_lts.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1.5) + 
  #geom_line(aes(x = Year, y = RurAvg), color = "lightblue") + 
  #geom_line(aes(x = Year, y = UrbAvg), color = "darkblue") + 
  #geom_line(aes(x = Year, y = ConAvg), color = "red3") + 
  #geom_line(aes(x = Year, y = DecAvg), color = "darkgreen") + 
  #geom_line(aes(x = Year, y = ThuAvg), color = "red3") + 
  #geom_line(aes(x = Year, y = AceAvg), color = "darkgreen") + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("%CC yr"^"-1"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
#####
segavg_rwi.p / segavg_lts.p

tiff("segmentedflat2.tiff", units = "in", width = 6.5, height = 8, res = 300)
noise_rwi.p / noise_lts.p / brk_rwi.p / brk_lts.p / 
  seg_rwi.p / seg_lts.p /segavg_rwi.p / segavg_lts.p
dev.off()

#### Avg of Stacked segments - subsets #####
#####

# Urban vs. Rural
segavg_rwi_urbrur.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = RurAvg), color = "darkgreen", lwd = 1) + 
  geom_line(aes(x = Year, y = UrbAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")

segavg_lts_urbrur.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = RurAvg), color = "darkgreen", lwd = 1) + 
  geom_line(aes(x = Year, y = UrbAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("%CC yr"^"-1"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")

tiff("segmentedflat_urbrur.tiff", units = "in", width = 6.5, height = 3, res = 300)
segavg_rwi_urbrur.p / segavg_lts_urbrur.p
dev.off()

# Deciduous vs. Coniferous
segavg_rwi_deccon.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = DecAvg), color = "blue", lwd = 1) + 
  geom_line(aes(x = Year, y = ConAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")

segavg_lts_deccon.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = DecAvg), color = "blue", lwd = 1) + 
  geom_line(aes(x = Year, y = ConAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("%CC yr"^"-1"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")

tiff("segmentedflat_deccon1.tiff", units = "in", width = 6.5, height = 3, res = 300)
segavg_rwi_deccon.p / segavg_lts_deccon.p
dev.off()

# Acer vs. Thuja
segavg_rwi_acethu.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = AceAvg), color = "blue", lwd = 1) + 
  geom_line(aes(x = Year, y = ThuAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0), "cm"),
        legend.position = "none")

segavg_lts_acethu.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = OvAvg), color = "black", lwd = 1) + 
  geom_line(aes(x = Year, y = AceAvg), color = "blue", lwd = 1) + 
  geom_line(aes(x = Year, y = ThuAvg), color = "red3", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("%CC yr"^"-1"))) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")

tiff("segmentedflat_acethu1.tiff", units = "in", width = 6.5, height = 3, res = 300)
segavg_rwi_acethu.p / segavg_lts_acethu.p
dev.off()
#####

#### Final comparison plot ###
tiff("finalRWICCcomp_plot2.tif", units = "in", width = 9, height = 4.35, res = 300)

par(mfrow = c(1,2), mar = c(3,3.5,0.25,1.5))
plot(1973:2018, yrseg_rwi$OvAvg[82:127], type = "l", col = "black", lwd = 2, xlab = "", ylab = "",
     ylim = c(-0.033, 0.033), xlim = c(1973,2018), xaxs = "i", 
     panel.first = abline(h = 0, col = "gray48"))
mtext(expression(paste("RWI yr"^"-1")), side = 2, line = 2, col = "black")
text(1976, 0.0305,"A", font = 2, cex = 1.5)
par(new = TRUE)
plot(1973:2018, yrseg_lts$OvAvg[82:127], type = "l", col = "red3", lwd = 2, xaxt = "n", yaxt = "n", 
     ylab = "", xlab = "", ylim = c(-0.8,0.8), xlim = c(1973,2018), xaxs = "i")
axis(side = 4)
mtext(expression(paste("%CC yr"^"-1")), side = 4, line = 2, col = "red3")

par(mar = c(3,5,0.25,0.25))
plot(yrseg_lts$OvAvg[82:127], yrseg_rwi$OvAvg[82:127], xlab = "", ylab = "", pch = 16,
     panel.first = c(abline(h = 0, col = "gray48", lty = 2), abline(v = 0, col = "gray48", lty = 2)))
mtext(expression(paste("RWI yr"^"-1")), side = 2, line = 2)
mtext(expression(paste("%CC yr"^"-1")), side = 1, line = 2)
text(-0.65, 0.0325,"B", font = 2, cex = 1.5)
text(0.5, 0.0325, "y = 0.02x + 0.0002")
text(0.5, 0.029, "R² = 0.41***")
abline(lm(yrseg_rwi$OvAvg[82:127] ~ yrseg_lts$OvAvg[82:127]))

#segltsvrwi = lm(yrseg_rwi$OvAvg[82:127] ~ yrseg_lts$OvAvg[82:127])
#summary(segltsvrwi)
dev.off()

#plot(rwi.F07Dec$year, rwi.F07Dec$xxxstd, type = "l", col = "darkgreen", 
#     ylab = "", xlab = "")
#lines(rwi.F07Dec$year, rwi.F07Dec$smooth, col = "darkgreen", lwd = 3)
#par(new = TRUE)
#plot(1972:2018, lts.F07$Avg_CC_Median, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
#     type = "l", col = "red3")
#axis(side = 4)
#lines(1972:2018, lts.F07$smooth, col = "red3", lwd = 3)
