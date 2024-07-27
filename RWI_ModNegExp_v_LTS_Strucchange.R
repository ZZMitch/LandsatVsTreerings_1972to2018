#####RWI (ModNegExp) vs. LTS (%CC) #####

##### Set up and Data #####
library(dplR)
#library(segmented)
library(strucchange)
library(dplyr)
library(trend)
library(modifiedmk)
library(zyp)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop

##### Bring in RWLs and apply ModNegExp detrending #####
#####
# F07Dec
rwl.F07Dec = read.rwl("Ring Width Chronologies/F07/F07_12.rwl")
rwl.report(rwl.F07Dec)
rwl.F07Dec.ids = read.ids(rwl.F07Dec, stc = c(4, 2, 1))
yr.F07Dec = time(rwl.F07Dec)

rwl.F07Dec.mne = detrend(rwl.F07Dec, method = "ModNegExp")

rwl.F07Dec.mne.sss = sss(rwl.F07Dec.mne, rwl.F07Dec.ids)
cut.F07Dec.mne = max(yr.F07Dec[rwl.F07Dec.mne.sss < 0.85])
yr.cut.F07Dec.mne = yr.F07Dec[yr.F07Dec > cut.F07Dec.mne]

rwl.F07Dec.mne.crn = chron(detrend(rwl.F07Dec[yr.F07Dec > cut.F07Dec.mne,],
                                   method = "ModNegExp"))
plot(rwl.F07Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# F07Ace
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
rwl.F15Dec = read.rwl("Ring Width Chronologies/F15/F15_4.rwl")
rwl.report(rwl.F15Dec)
rwl.F15Dec.ids = read.ids(rwl.F15Dec, stc = c(4, 2, 1))
yr.F15Dec = time(rwl.F15Dec)

rwl.F15Dec.mne = detrend(rwl.F15Dec, method = "ModNegExp")

rwl.F15Dec.mne.sss = sss(rwl.F15Dec.mne, rwl.F15Dec.ids)
cut.F15Dec.mne = max(yr.F15Dec[rwl.F15Dec.mne.sss < 0.85])
yr.cut.F15Dec.mne = yr.F15Dec[yr.F15Dec > cut.F15Dec.mne]

rwl.F15Dec.mne.crn = chron(detrend(rwl.F15Dec[yr.F15Dec > cut.F15Dec.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Dec.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Ace
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
rwl.F15Car = read.rwl("Ring Width Chronologies/F15/F15_Carya_1.rwl")
rwl.report(rwl.F15Car)
rwl.F15Car.ids = read.ids(rwl.F15Car, stc = c(4, 2, 1))
yr.F15Car = time(rwl.F15Car)

rwl.F15Car.mne = detrend(rwl.F15Car, method = "ModNegExp")

rwl.F15Car.mne.sss = sss(rwl.F15Car.mne, rwl.F15Car.ids)
cut.F15Car.mne = max(yr.F15Car[rwl.F15Car.mne.sss < 0.85])
yr.cut.F15Car.mne = yr.F15Car[yr.F15Car > cut.F15Car.mne]

rwl.F15Car.mne.crn = chron(detrend(rwl.F15Car[yr.F15Car > cut.F15Car.mne,],
                                   method = "ModNegExp"))
plot(rwl.F15Car.mne.crn, add.spline = TRUE, nyrs = 10)

# F15Que
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

# F21Dec
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
rwl.F21Ace = read.rwl("Ring Width Chronologies/F21/F21_Acer_7.rwl")
rwl.report(rwl.F21Ace)
rwl.F21Ace.ids = read.ids(rwl.F21Ace, stc = c(4, 2, 1))
yr.F21Ace = time(rwl.F21Ace)

rwl.F21Ace.mne = detrend(rwl.F21Ace, method = "ModNegExp")

rwl.F21Ace.mne.sss = sss(rwl.F21Ace.mne, rwl.F21Ace.ids)
cut.F21Ace.mne = max(yr.F21Ace[rwl.F21Ace.mne.sss < 0.85])
yr.cut.F21Ace.mne = yr.F21Ace[yr.F21Ace > cut.F21Ace.mne]

rwl.F21Ace.mne.crn = chron(detrend(rwl.F21Ace[yr.F21Ace > cut.F21Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.F21Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# F23Ace
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

# F25Ace
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

# F30Ace
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

# F30Bet
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

# F33Pic
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

# M01Pop
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

# M05Thu
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

# M06Ace
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

# M06Que
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

# M07Tsu
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

# M13Tsu
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

# M17Thu
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
rwl.M20Con = read.rwl("Ring Width Chronologies/M20/M20_3.rwl")
rwl.report(rwl.M20Con)
rwl.M20Con.ids = read.ids(rwl.M20Con, stc = c(4, 2, 1))
yr.M20Con = time(rwl.M20Con)

rwl.M20Con.mne = detrend(rwl.M20Con, method = "ModNegExp")

rwl.M20Con.mne.sss = sss(rwl.M20Con.mne, rwl.M20Con.ids)
cut.M20Con.mne = max(yr.M20Con[rwl.M20Con.mne.sss < 0.85])
yr.cut.M20Con.mne = yr.M20Con[yr.M20Con > cut.M20Con.mne]

rwl.M20Con.mne.crn = chron(detrend(rwl.M20Con[yr.M20Con > cut.M20Con.mne,],
                                   method = "ModNegExp"))
plot(rwl.M20Con.mne.crn, add.spline = TRUE, nyrs = 10)

# M20Thu
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

# M26Dec
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
rwl.M26Ace = read.rwl("Ring Width Chronologies/M26/M26_Acer_1.rwl")
rwl.report(rwl.M26Ace)
rwl.M26Ace.ids = read.ids(rwl.M26Ace, stc = c(4, 2, 1))
yr.M26Ace = time(rwl.M26Ace)

rwl.M26Ace.mne = detrend(rwl.M26Ace, method = "ModNegExp")

rwl.M26Ace.mne.sss = sss(rwl.M26Ace.mne, rwl.M26Ace.ids)
cut.M26Ace.mne = max(yr.M26Ace[rwl.M26Ace.mne.sss < 0.85])
yr.cut.M26Ace.mne = yr.M26Ace[yr.M26Ace > cut.M26Ace.mne]

rwl.M26Ace.mne.crn = chron(detrend(rwl.M26Ace[yr.M26Ace > cut.M26Ace.mne,],
                                   method = "ModNegExp"))
plot(rwl.M26Ace.mne.crn, add.spline = TRUE, nyrs = 10)

# M26Thu
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

# M27Pin
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
write.csv(rwl.F07Dec.mne.crn, "F07Dec_ModNegExp.csv") #rwl.F07Dec.mne.crn
write.csv(rwl.F07Ace.mne.crn, "F07Ace_ModNegExp.csv") #rwl.F07Ace.mne.crn
write.csv(rwl.F15Dec.mne.crn, "F15Dec_ModNegExp.csv") #rwl.F15Dec.mne.crn
write.csv(rwl.F15Ace.mne.crn, "F15Ace_ModNegExp.csv") #rwl.F15Ace.mne.crn
write.csv(rwl.F15Car.mne.crn, "F15Car_ModNegExp.csv") #rwl.F15Car.mne.crn
write.csv(rwl.F15Que.mne.crn, "F15Que_ModNegExp.csv") #rwl.F15Que.mne.crn
write.csv(rwl.F21Dec.mne.crn, "F21Dec_ModNegExp.csv") #rwl.F21Dec.mne.crn
write.csv(rwl.F21Ace.mne.crn, "F21Ace_ModNegExp.csv") #rwl.F21Ace.mne.crn
write.csv(rwl.F23Ace.mne.crn, "F23Ace_ModNegExp.csv") #rwl.F23Ace.mne.crn
write.csv(rwl.F25Ace.mne.crn, "F25Ace_ModNegExp.csv") #rwl.F25Ace.mne.crn
write.csv(rwl.F30Ace.mne.crn, "F30Ace_ModNegExp.csv") #rwl.F30Ace.mne.crn
write.csv(rwl.F30Bet.mne.crn, "F30Bet_ModNegExp.csv") #rwl.F30Bet.mne.crn
write.csv(rwl.F33Pic.mne.crn, "F33Pic_ModNegExp.csv") #rwl.F33Pic.mne.crn
write.csv(rwl.M01Pop.mne.crn, "M01Pop_ModNegExp.csv") #rwl.M01Pop.mne.crn
write.csv(rwl.M05Thu.mne.crn, "M05Thu_ModNegExp.csv") #rwl.M05Thu.mne.crn
write.csv(rwl.M06Ace.mne.crn, "M06Ace_ModNegExp.csv") #rwl.M06Ace.mne.crn
write.csv(rwl.M06Que.mne.crn, "M06Que_ModNegExp.csv") #rwl.M06Que.mne.crn
write.csv(rwl.M07Tsu.mne.crn, "M07Tsu_ModNegExp.csv") #rwl.M07Tsu.mne.crn
write.csv(rwl.M13Tsu.mne.crn, "M13Tsu_ModNegExp.csv") #rwl.M13Tsu.mne.crn
write.csv(rwl.M17Thu.mne.crn, "M17Thu_ModNegExp.csv") #rwl.M17Thu.mne.crn
write.csv(rwl.M20Con.mne.crn, "M20Con_ModNegExp.csv") #rwl.M20Con.mne.crn
write.csv(rwl.M20Thu.mne.crn, "M20Thu_ModNegExp.csv") #rwl.M20Thu.mne.crn
write.csv(rwl.M26Dec.mne.crn, "M26Dec_ModNegExp.csv") #rwl.M26Dec.mne.crn
write.csv(rwl.M26Ace.mne.crn, "M26Ace_ModNegExp.csv") #rwl.M26Ace.mne.crn
write.csv(rwl.M26Thu.mne.crn, "M26Thu_ModNegExp.csv") #rwl.M26Thu.mne.crn
write.csv(rwl.M27Pin.mne.crn, "M27Pin_ModNegExp.csv") #rwl.M27Pin.mne.crn

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

##### F07 Segmented Regression #####
#####
### RWI: F07Dec ###
# Convert to time-series
rwi.F07Dec = ts(rwl.F07Dec.mne.crn$xxxstd, 
                start = as.numeric(rownames(rwl.F07Dec.mne.crn[1,])),
                end = as.numeric(rownames(rwl.F07Dec.mne.crn[nrow(rwl.F07Dec.mne.crn),])),
                frequency = 1) # yearly
plot(rwi.F07Dec, ylab = "RWI")

# Simple linear model
tt = 1:length(rwi.F07Dec)
lm.F07Dec = lm(rwi.F07Dec ~ tt)
summary(lm.F07Dec)

plot(rwi.F07Dec, ylab = "RWI")
lines(ts(fitted(lm.F07Dec), start = as.numeric(rownames(rwl.F07Dec.mne.crn[1,])), 
         frequency = 1), lwd = 3, col = "gray48")

# Find breakpoints, choose best number based on lowest BIC
rwi.F07Dec.brk = breakpoints(rwi.F07Dec ~ tt, h  = 5) # Minimum 5-year segments
summary(rwi.F07Dec.brk)

plot(rwi.F07Dec.brk) # Lowest BIC = 1
n.brk = 1

yr.brk = breakdates(rwi.F07Dec.brk, breaks = n.brk)
yr.brk

confint(rwi.F07Dec.brk, breaks = n.brk)

# Plot
plot(rwi.F07Dec, ylab = "RWI")
lines(fitted(rwi.F07Dec.brk, breaks = n.brk), lwd = 2)
lines(confint(rwi.F07Dec.brk, breaks = n.brk), col = "blue")

# Significance and slope of segmented trends
seg1 = window(rwi.F07Dec, end = yr.brk)
seg1
seg2 = window(rwi.F07Dec, start = yr.brk + 1)
seg2

#mk.test(seg1) # Adjust based on breakpoint years from above
# Mann-Kendall test for significant slope, no correction for autocorrelation
sens.slope(seg1) # Includes mk.test p-value
mmkh(as.numeric(seg1))
#Mann-Kendal modified for autocorrelated data, shows old results too + Sen's slope

sens.slope(seg2)
mmkh(as.numeric(seg2))

# Plot Sens slope using zyp method
seg1.df = data.frame(RWI = as.matrix(seg1), year = as.numeric(time(seg1)))
seg2.df = data.frame(RWI = as.matrix(seg2), year = as.numeric(time(seg2)))

seg1.lm = zyp.sen(RWI ~ year, seg1.df)
seg2.lm = zyp.sen(RWI ~ year, seg2.df)
# Gives Sen slope and median intercept

seg1.df$slope = seg1.lm$coefficients[2] * seg1.df$year + seg1.lm$coefficients[1]
seg2.df$slope = seg2.lm$coefficients[2] * seg2.df$year + seg2.lm$coefficients[1]

# Plot
plot(rwi.F07Dec, ylab = "RWI")
lines(fitted(rwi.F07Dec.brk, breaks = n.brk), lwd = 2)
lines(confint(rwi.F07Dec.brk, breaks = n.brk), col = "blue")
lines(seg1.df$year, seg1.df$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2.df$year, seg2.df$slope, lwd = 3, col = "gray48") # Color based on sig

### RWI: F07Ace ###
# Convert to time-series
rwi.F07Ace = ts(rwl.F07Ace.mne.crn$xxxstd, 
                start = as.numeric(rownames(rwl.F07Ace.mne.crn[1,])),
                end = as.numeric(rownames(rwl.F07Ace.mne.crn[nrow(rwl.F07Ace.mne.crn),])),
                frequency = 1) # yearly
plot(rwi.F07Ace, ylab = "RWI")

# Simple linear model
tt = 1:length(rwi.F07Ace)
lm.F07Ace = lm(rwi.F07Ace ~ tt)
summary(lm.F07Ace)

plot(rwi.F07Ace, ylab = "RWI")
lines(ts(fitted(lm.F07Ace), start = as.numeric(rownames(rwl.F07Ace.mne.crn[1,])), 
         frequency = 1), lwd = 3, col = "gray48")

# Find breakpoints, choose best number based on lowest BIC
rwi.F07Ace.brk = breakpoints(rwi.F07Ace ~ tt, h  = 5) # Minimum 5-year segments
summary(rwi.F07Ace.brk)

plot(rwi.F07Ace.brk) # Lowest BIC = 1
n.brk = 1

yr.brk = breakdates(rwi.F07Ace.brk, breaks = n.brk)
yr.brk

confint(rwi.F07Ace.brk, breaks = n.brk)

# Plot
plot(rwi.F07Ace, ylab = "RWI")
lines(fitted(rwi.F07Ace.brk, breaks = n.brk), lwd = 2)
lines(confint(rwi.F07Ace.brk, breaks = n.brk), col = "blue")

# Significance and slope of segmented trends
seg1 = window(rwi.F07Ace, end = yr.brk)
seg1
seg2 = window(rwi.F07Ace, start = yr.brk + 1)
seg2

#mk.test(seg1) # Adjust based on breakpoint years from above
# Mann-Kendall test for significant slope, no correction for autocorrelation
sens.slope(seg1) # Includes mk.test p-value
mmkh(as.numeric(seg1))
#Mann-Kendal modified for autocorrelated data, shows old results too + Sen's slope

sens.slope(seg2)
mmkh(as.numeric(seg2))

# Plot Sens slope using zyp method
seg1.df = data.frame(RWI = as.matrix(seg1), year = as.numeric(time(seg1)))
seg2.df = data.frame(RWI = as.matrix(seg2), year = as.numeric(time(seg2)))

seg1.lm = zyp.sen(RWI ~ year, seg1.df)
seg2.lm = zyp.sen(RWI ~ year, seg2.df)
# Gives Sen slope and median intercept

seg1.df$slope = seg1.lm$coefficients[2] * seg1.df$year + seg1.lm$coefficients[1]
seg2.df$slope = seg2.lm$coefficients[2] * seg2.df$year + seg2.lm$coefficients[1]

# Add segmented slopes to plot
lines(seg1.df$year, seg1.df$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg2.df$year, seg2.df$slope, lwd = 3, col = "gray48") # Color based on sig

### LTS: F07 ###
# Convert to time-series
lts.F07.ts = ts(lts.F07$Avg_CC_Median, 
                start = as.numeric(rownames(lts.F07[1,])),
                end = as.numeric(rownames(lts.F07[nrow(lts.F07),])),
                frequency = 1) # yearly
plot(lts.F07.ts, ylab = "%CC")

# Simple linear model
tt = 1:length(lts.F07.ts)
lm.ltsF07 = lm(lts.F07.ts ~ tt)
summary(lm.ltsF07)

lines(ts(fitted(lm.ltsF07), start = as.numeric(rownames(lts.F07[1,])), 
         frequency = 1), lwd = 3, col = "darkgreen")

# Find breakpoints, choose best number based on lowest BIC
lts.F07.brk = breakpoints(lts.F07.ts ~ tt) # Usually minimum 5-year segments
summary(lts.F07.brk)

plot(lts.F07.brk) # Lowest BIC = 2
n.brk = 2

yr.brk = breakdates(lts.F07.brk, breaks = n.brk)
yr.brk

confint(lts.F07.brk, breaks = n.brk)

# Plot
plot(lts.F07.ts, ylab = "%CC")
lines(fitted(lts.F07.brk, breaks = n.brk), lwd = 2)
lines(confint(lts.F07.brk, breaks = n.brk), col = "blue")

# Significance and slope of segmented trends
seg1 = window(lts.F07.ts, end = yr.brk[1])
seg1
seg2 = window(lts.F07.ts, start = yr.brk[1] + 1, end = yr.brk[2])
seg2
seg3 = window(lts.F07.ts, start = yr.brk[2] + 1)
seg3

#mk.test(seg1) # Adjust based on breakpoint years from above
# Mann-Kendall test for significant slope, no correction for autocorrelation
sens.slope(seg1) # Includes mk.test p-value
mmkh(as.numeric(seg1))
#Mann-Kendal modified for autocorrelated data, shows old results too + Sen's slope

sens.slope(seg2)
mmkh(as.numeric(seg2))

sens.slope(seg3)
mmkh(as.numeric(seg3))

# Plot Sens slope using zyp method
seg1.df = data.frame(RWI = as.matrix(seg1), year = as.numeric(time(seg1)))
seg2.df = data.frame(RWI = as.matrix(seg2), year = as.numeric(time(seg2)))
seg3.df = data.frame(RWI = as.matrix(seg3), year = as.numeric(time(seg3)))

seg1.lm = zyp.sen(RWI ~ year, seg1.df)
seg2.lm = zyp.sen(RWI ~ year, seg2.df)
seg3.lm = zyp.sen(RWI ~ year, seg3.df)
# Gives Sen slope and median intercept

seg1.df$slope = seg1.lm$coefficients[2] * seg1.df$year + seg1.lm$coefficients[1]
seg2.df$slope = seg2.lm$coefficients[2] * seg2.df$year + seg2.lm$coefficients[1]
seg3.df$slope = seg3.lm$coefficients[2] * seg3.df$year + seg3.lm$coefficients[1]

# Add segmented slopes to plot
lines(seg1.df$year, seg1.df$slope, lwd = 3, col = "gray48")
lines(seg2.df$year, seg2.df$slope, lwd = 3, col = "gray48") 
lines(seg3.df$year, seg3.df$slope, lwd = 3, col = "red3") 
#####

##### F15 Segmented Regression #####
### RWI: F15Dec ###
# Convert to time-series
rwi.F15Dec = ts(rwl.F15Dec.mne.crn$xxxstd, 
                start = as.numeric(rownames(rwl.F15Dec.mne.crn[1,])),
                end = as.numeric(rownames(rwl.F15Dec.mne.crn[nrow(rwl.F15Dec.mne.crn),])),
                frequency = 1) # yearly
plot(rwi.F15Dec, ylab = "RWI")

# Simple linear model
tt = 1:length(rwi.F15Dec)
lm.F15Dec = lm(rwi.F15Dec ~ tt)
summary(lm.F15Dec)

plot(rwi.F15Dec, ylab = "RWI")
lines(ts(fitted(lm.F15Dec), start = as.numeric(rownames(rwl.F15Dec.mne.crn[1,])), 
         frequency = 1), lwd = 3, col = "darkgreen")

# Find breakpoints, choose best number based on lowest BIC
rwi.F15Dec.brk = breakpoints(rwi.F15Dec ~ tt, h  = 5) # Minimum 5-year segments
summary(rwi.F15Dec.brk)

plot(rwi.F15Dec.brk) # Lowest BIC = 1
n.brk = 3

yr.brk = breakdates(rwi.F15Dec.brk, breaks = n.brk)
yr.brk

confint(rwi.F15Dec.brk, breaks = n.brk)

# Plot
plot(rwi.F15Dec, ylab = "RWI")
lines(fitted(rwi.F15Dec.brk, breaks = n.brk), lwd = 2)
lines(confint(rwi.F15Dec.brk, breaks = n.brk), col = "blue")

# Significance and slope of segmented trends
seg1 = window(rwi.F15Dec, end = yr.brk[1])
seg1
seg2 = window(rwi.F15Dec, start = yr.brk[1] + 1, end = yr.brk[2])
seg2
seg3 = window(rwi.F15Dec, start = yr.brk[2] + 1, end = yr.brk[3])
seg3
seg4 = window(rwi.F15Dec, start = yr.brk[3] + 1)
seg4

#mk.test(seg1) # Adjust based on breakpoint years from above
# Mann-Kendall test for significant slope, no correction for autocorrelation
sens.slope(seg1) # Includes mk.test p-value
mmkh(as.numeric(seg1))
#Mann-Kendal modified for autocorrelated data, shows old results too + Sen's slope

sens.slope(seg2)
mmkh(as.numeric(seg2))

sens.slope(seg3)
mmkh(as.numeric(seg3))

sens.slope(seg4)
mmkh(as.numeric(seg4))

# Plot Sens slope using zyp method
seg1.df = data.frame(RWI = as.matrix(seg1), year = as.numeric(time(seg1)))
seg2.df = data.frame(RWI = as.matrix(seg2), year = as.numeric(time(seg2)))
seg3.df = data.frame(RWI = as.matrix(seg3), year = as.numeric(time(seg3)))
seg4.df = data.frame(RWI = as.matrix(seg4), year = as.numeric(time(seg4)))

seg1.lm = zyp.sen(RWI ~ year, seg1.df)
seg2.lm = zyp.sen(RWI ~ year, seg2.df)
seg3.lm = zyp.sen(RWI ~ year, seg3.df)
seg4.lm = zyp.sen(RWI ~ year, seg4.df)
# Gives Sen slope and median intercept

seg1.df$slope = seg1.lm$coefficients[2] * seg1.df$year + seg1.lm$coefficients[1]
seg2.df$slope = seg2.lm$coefficients[2] * seg2.df$year + seg2.lm$coefficients[1]
seg3.df$slope = seg3.lm$coefficients[2] * seg3.df$year + seg3.lm$coefficients[1]
seg4.df$slope = seg4.lm$coefficients[2] * seg4.df$year + seg4.lm$coefficients[1]

# Add segmented slopes to plot
lines(seg1.df$year, seg1.df$slope, lwd = 3, col = "gray48") # Color based on sig
lines(seg2.df$year, seg2.df$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg3.df$year, seg3.df$slope, lwd = 3, col = "darkgreen") # Color based on sig
lines(seg4.df$year, seg4.df$slope, lwd = 3, col = "red3") # Color based on sig
