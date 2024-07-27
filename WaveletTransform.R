# Wavelet Trasform #

##### Set up and Data #####
library(dplR)
library(dplyr)

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

nyrs = 10

##### F07 Wavelet #####
#####
### RWI: F07Dec ###
rwi.F07Dec = add_rownames(rwl.F07Dec.mne.crn, var = "year")
rwi.F07Dec$year = as.numeric(rwi.F07Dec$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F07Dec$xxxstd, x1 = rwi.F07Dec$year) 
# p2 controls size of Period, dj changes whole figure (keep default)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F07Ace ###
rwi.F07Ace = add_rownames(rwl.F07Ace.mne.crn, var = "year")
rwi.F07Ace$year = as.numeric(rwi.F07Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F07Ace$xxxstd, x1 = rwi.F07Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F07 ###
lts.F07_1 = add_rownames(lts.F07, var = "year")
lts.F07_1$year = as.numeric(lts.F07_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F07_1$Avg_CC_Median, x1 = lts.F07_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F15 Wavelet #####
#####
### RWI: F15Dec ###
rwi.F15Dec = add_rownames(rwl.F15Dec.mne.crn, var = "year")
rwi.F15Dec$year = as.numeric(rwi.F15Dec$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F15Dec$xxxstd, x1 = rwi.F15Dec$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F15Ace ###
rwi.F15Ace = add_rownames(rwl.F15Ace.mne.crn, var = "year")
rwi.F15Ace$year = as.numeric(rwi.F15Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F15Ace$xxxstd, x1 = rwi.F15Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F15Car ###
rwi.F15Car = add_rownames(rwl.F15Car.mne.crn, var = "year")
rwi.F15Car$year = as.numeric(rwi.F15Car$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F15Car$xxxstd, x1 = rwi.F15Car$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F15Que ###
rwi.F15Que = add_rownames(rwl.F15Que.mne.crn, var = "year")
rwi.F15Que$year = as.numeric(rwi.F15Que$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F15Que$xxxstd, x1 = rwi.F15Que$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F15 ###
lts.F15_1 = add_rownames(lts.F15, var = "year")
lts.F15_1$year = as.numeric(lts.F15_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F15_1$Avg_CC_Median, x1 = lts.F15_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F21 Wavelet #####
#####
### RWI: F21Dec ###
rwi.F21Dec = add_rownames(rwl.F21Dec.mne.crn, var = "year")
rwi.F21Dec$year = as.numeric(rwi.F21Dec$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F21Dec$xxxstd, x1 = rwi.F21Dec$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F21Ace ###
rwi.F21Ace = add_rownames(rwl.F21Ace.mne.crn, var = "year")
rwi.F21Ace$year = as.numeric(rwi.F21Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F21Ace$xxxstd, x1 = rwi.F21Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F21 ###
lts.F21_1 = add_rownames(lts.F21, var = "year")
lts.F21_1$year = as.numeric(lts.F21_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F21_1$Avg_CC_Median, x1 = lts.F21_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F23 Wavelet #####
#####
### RWI: F23Ace ###
rwi.F23Ace = add_rownames(rwl.F23Ace.mne.crn, var = "year")
rwi.F23Ace$year = as.numeric(rwi.F23Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F23Ace$xxxstd, x1 = rwi.F23Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F23 ###
lts.F23_1 = add_rownames(lts.F23, var = "year")
lts.F23_1$year = as.numeric(lts.F23_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F23_1$Avg_CC_Median, x1 = lts.F23_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F25 Wavelet #####
#####
### RWI: F25Ace ###
rwi.F25Ace = add_rownames(rwl.F25Ace.mne.crn, var = "year")
rwi.F25Ace$year = as.numeric(rwi.F25Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F25Ace$xxxstd, x1 = rwi.F25Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F25 ###
lts.F25_1 = add_rownames(lts.F25, var = "year")
lts.F25_1$year = as.numeric(lts.F25_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F25_1$Avg_CC_Median, x1 = lts.F25_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F30 Wavelet #####
#####
### RWI: F30Ace ###
rwi.F30Ace = add_rownames(rwl.F30Ace.mne.crn, var = "year")
rwi.F30Ace$year = as.numeric(rwi.F30Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F30Ace$xxxstd, x1 = rwi.F30Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: F30Bet ###
rwi.F30Bet = add_rownames(rwl.F30Bet.mne.crn, var = "year")
rwi.F30Bet$year = as.numeric(rwi.F30Bet$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F30Bet$xxxstd, x1 = rwi.F30Bet$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F30 ###
lts.F30_1 = add_rownames(lts.F30, var = "year")
lts.F30_1$year = as.numeric(lts.F30_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F30_1$Avg_CC_Median, x1 = lts.F30_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### F33 Wavelet #####
#####
### RWI: F33Pic ###
rwi.F33Pic = add_rownames(rwl.F33Pic.mne.crn, var = "year")
rwi.F33Pic$year = as.numeric(rwi.F33Pic$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.F33Pic$xxxstd, x1 = rwi.F33Pic$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: F33 ###
lts.F33_1 = add_rownames(lts.F33, var = "year")
lts.F33_1$year = as.numeric(lts.F33_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.F33_1$Avg_CC_Median, x1 = lts.F33_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M01 Wavelet #####
#####
### RWI: M01Pop ###
rwi.M01Pop = add_rownames(rwl.M01Pop.mne.crn, var = "year")
rwi.M01Pop$year = as.numeric(rwi.M01Pop$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M01Pop$xxxstd, x1 = rwi.M01Pop$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M01 ###
lts.M01_1 = add_rownames(lts.M01, var = "year")
lts.M01_1$year = as.numeric(lts.M01_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M01_1$Avg_CC_Median, x1 = lts.M01_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M05 Wavelet #####
#####
### RWI: M05Thu ###
rwi.M05Thu = add_rownames(rwl.M05Thu.mne.crn, var = "year")
rwi.M05Thu$year = as.numeric(rwi.M05Thu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M05Thu$xxxstd, x1 = rwi.M05Thu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M05 ###
lts.M05_1 = add_rownames(lts.M05, var = "year")
lts.M05_1$year = as.numeric(lts.M05_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M05_1$Avg_CC_Median, x1 = lts.M05_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M06 Wavelet #####
#####
### RWI: M06Ace ###
rwi.M06Ace = add_rownames(rwl.M06Ace.mne.crn, var = "year")
rwi.M06Ace$year = as.numeric(rwi.M06Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M06Ace$xxxstd, x1 = rwi.M06Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: M06Que ###
rwi.M06Que = add_rownames(rwl.M06Que.mne.crn, var = "year")
rwi.M06Que$year = as.numeric(rwi.M06Que$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M06Que$xxxstd, x1 = rwi.M06Que$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M06 ###
lts.M06_1 = add_rownames(lts.M06, var = "year")
lts.M06_1$year = as.numeric(lts.M06_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M06_1$Avg_CC_Median, x1 = lts.M06_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M07 Wavelet #####
#####
### RWI: M07Tsu ###
rwi.M07Tsu = add_rownames(rwl.M07Tsu.mne.crn, var = "year")
rwi.M07Tsu$year = as.numeric(rwi.M07Tsu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M07Tsu$xxxstd, x1 = rwi.M07Tsu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M07 ###
lts.M07_1 = add_rownames(lts.M07, var = "year")
lts.M07_1$year = as.numeric(lts.M07_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M07_1$Avg_CC_Median, x1 = lts.M07_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M13 Wavelet #####
#####
### RWI: M13Tsu ###
rwi.M13Tsu = add_rownames(rwl.M13Tsu.mne.crn, var = "year")
rwi.M13Tsu$year = as.numeric(rwi.M13Tsu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M13Tsu$xxxstd, x1 = rwi.M13Tsu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M13 ###
lts.M13_1 = add_rownames(lts.M13, var = "year")
lts.M13_1$year = as.numeric(lts.M13_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M13_1$Avg_CC_Median, x1 = lts.M13_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M17 Wavelet #####
#####
### RWI: M17Thu ###
rwi.M17Thu = add_rownames(rwl.M17Thu.mne.crn, var = "year")
rwi.M17Thu$year = as.numeric(rwi.M17Thu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M17Thu$xxxstd, x1 = rwi.M17Thu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M17 ###
lts.M17_1 = add_rownames(lts.M17, var = "year")
lts.M17_1$year = as.numeric(lts.M17_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M17_1$Avg_CC_Median, x1 = lts.M17_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M20 Wavelet #####
#####
### RWI: M20Con ###
rwi.M20Con = add_rownames(rwl.M20Con.mne.crn, var = "year")
rwi.M20Con$year = as.numeric(rwi.M20Con$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M20Con$xxxstd, x1 = rwi.M20Con$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: M20Thu ###
rwi.M20Thu = add_rownames(rwl.M20Thu.mne.crn, var = "year")
rwi.M20Thu$year = as.numeric(rwi.M20Thu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M20Thu$xxxstd, x1 = rwi.M20Thu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M20 ###
lts.M20_1 = add_rownames(lts.M20, var = "year")
lts.M20_1$year = as.numeric(lts.M20_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M20_1$Avg_CC_Median, x1 = lts.M20_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M26 Wavelet #####
#####
### RWI: M26Dec ###
rwi.M26Dec = add_rownames(rwl.M26Dec.mne.crn, var = "year")
rwi.M26Dec$year = as.numeric(rwi.M26Dec$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M26Dec$xxxstd, x1 = rwi.M26Dec$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: M26Ace ###
rwi.M26Ace = add_rownames(rwl.M26Ace.mne.crn, var = "year")
rwi.M26Ace$year = as.numeric(rwi.M26Ace$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M26Ace$xxxstd, x1 = rwi.M26Ace$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### RWI: M26Thu ###
rwi.M26Thu = add_rownames(rwl.M26Thu.mne.crn, var = "year")
rwi.M26Thu$year = as.numeric(rwi.M26Thu$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M26Thu$xxxstd, x1 = rwi.M26Thu$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M26 ###
lts.M26_1 = add_rownames(lts.M26, var = "year")
lts.M26_1$year = as.numeric(lts.M26_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M26_1$Avg_CC_Median, x1 = lts.M26_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####

##### M27 Wavelet #####
#####
### RWI: M27Pin ###
rwi.M27Pin = add_rownames(rwl.M27Pin.mne.crn, var = "year")
rwi.M27Pin$year = as.numeric(rwi.M27Pin$year) # Years column in chr by default

out.wave = morlet(y1 = rwi.M27Pin$xxxstd, x1 = rwi.M27Pin$year) 
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)

### LTS: M27 ###
lts.M27_1 = add_rownames(lts.M27, var = "year")
lts.M27_1$year = as.numeric(lts.M27_1$year) # Years column in chr by default

out.wave = morlet(y1 = lts.M27_1$Avg_CC_Median, x1 = lts.M27_1$year)
wavelet.plot(out.wave, add.spline = TRUE, nyrs = nyrs)
#####