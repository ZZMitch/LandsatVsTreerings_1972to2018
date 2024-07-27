# Overall Correlation #

##### Set up and Data #####
library(dplR)
library(dplyr)
library(gtools)
library(ggplot2)
library(patchwork)
setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop

##### Bring in RWLs and apply ModNegExp detrending #####
#####
### F15 / 02 ###
# F15Que / 02Que
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

# F15Ace / 02Ace
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

# F15Car / 02Car
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

### M06 / 04 ###
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

### M26 / 17 ###
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

### F30 / 27 ###
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
#####

##### Bring in %CC LTS #####
#####
# F15 / 02
lts.F15 = read.csv("Ring Width Chronologies/F15/DetrendingLandsat/F15_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F15), lts.F15$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F15), lts.F15$Avg_CC_Fitted)

# M06 / 04
lts.M06 = read.csv("Ring Width Chronologies/M06/DetrendingLandsat/M06_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M06), lts.M06$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M06), lts.M06$Avg_CC_Fitted)

# M26 / 17
lts.M26 = read.csv("Ring Width Chronologies/M26/DetrendingLandsat/M26_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.M26), lts.M26$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.M26), lts.M26$Avg_CC_Fitted)

# F30 / 27
lts.F30 = read.csv("Ring Width Chronologies/F30/DetrendingLandsat/F30_Landsat_1.csv",
                   row.names = 1)
plot(rownames(lts.F30), lts.F30$Avg_CC_Median, ylab = "Landsat-derived %CC", xlab = "")
lines(rownames(lts.F30), lts.F30$Avg_CC_Fitted)

# Smoothing spline length
nyrs = 10

##### Create Plots #####
#####
### F15Que / 02 Que ###
# Add Spline and then cutoff RWI pre-1972
rwi.F15Que = add_rownames(rwl.F15Que.mne.crn, var = "year")
rwi.F15Que$year = as.numeric(rwi.F15Que$year) # Years column in chr by default
rwi.F15Que$smooth = ffcsaps(rwi.F15Que$xxxstd, nyrs = nyrs)
rwi.F15Que = subset(rwi.F15Que, year >= 1972)

# Add Spline to LTS
lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

F15Que.p = ggplot(data = rwi.F15Que, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.F15Que, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("02Que (0.28*)") +
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
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
F15Que.p

### F15Ace / 02Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F15Ace = add_rownames(rwl.F15Ace.mne.crn, var = "year")
rwi.F15Ace$year = as.numeric(rwi.F15Ace$year) # Years column in chr by default
rwi.F15Ace$smooth = ffcsaps(rwi.F15Ace$xxxstd, nyrs = nyrs)
rwi.F15Ace = subset(rwi.F15Ace, year >= 1972)

# Add Spline to LTS
lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

F15Ace.p = ggplot(data = rwi.F15Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.F15Ace, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("02Ace (-0.13)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5),
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, -1, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F15Ace.p

### F15Car / 02Car ###
# Add Spline and then cutoff RWI pre-1972
rwi.F15Car = add_rownames(rwl.F15Car.mne.crn, var = "year")
rwi.F15Car$year = as.numeric(rwi.F15Car$year) # Years column in chr by default
rwi.F15Car$smooth = ffcsaps(rwi.F15Car$xxxstd, nyrs = nyrs)
rwi.F15Car = subset(rwi.F15Car, year >= 1972)

# Add Spline to LTS
lts.F15$smooth = ffcsaps(lts.F15$Avg_CC_Median, nyrs = nyrs)

F15Car.p = ggplot(data = rwi.F15Car, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.F15Car, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.F15, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("02Car (-0.32)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), 
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5),
                                         labels = NULL)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(0.15, "cm"),
        axis.ticks.length.y.right = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F15Car.p

### M06Ace / 04Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.M06Ace = add_rownames(rwl.M06Ace.mne.crn, var = "year")
rwi.M06Ace$year = as.numeric(rwi.M06Ace$year) # Years column in chr by default
rwi.M06Ace$smooth = ffcsaps(rwi.M06Ace$xxxstd, nyrs = nyrs)
rwi.M06Ace = subset(rwi.M06Ace, year >= 1972)

# Add Spline to LTS
lts.M06$smooth = ffcsaps(lts.M06$Avg_CC_Median, nyrs = nyrs)

M06Ace.p = ggplot(data = rwi.M06Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.M06Ace, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("03Ace (0.24*)") +
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
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M06Ace.p

### M06Que / 04Que ###
# Add Spline and then cutoff RWI pre-1972
rwi.M06Que = add_rownames(rwl.M06Que.mne.crn, var = "year")
rwi.M06Que$year = as.numeric(rwi.M06Que$year) # Years column in chr by default
rwi.M06Que$smooth = ffcsaps(rwi.M06Que$xxxstd, nyrs = nyrs)
rwi.M06Que = subset(rwi.M06Que, year >= 1972)

# Add Spline to LTS
lts.M06$smooth = ffcsaps(lts.M06$Avg_CC_Median, nyrs = nyrs)

M06Que.p = ggplot(data = rwi.M06Que, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.M06Que, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.M06, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("03Que (0.02)") +
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
        plot.title = element_text(size = 10, vjust = -1))
M06Que.p

### M26Thu / 17Thu ###
# Add Spline and then cutoff RWI pre-1972
rwi.M26Thu = add_rownames(rwl.M26Thu.mne.crn, var = "year")
rwi.M26Thu$year = as.numeric(rwi.M26Thu$year) # Years column in chr by default
rwi.M26Thu$smooth = ffcsaps(rwi.M26Thu$xxxstd, nyrs = nyrs)
rwi.M26Thu = subset(rwi.M26Thu, year >= 1972)

# Add Spline to LTS
lts.M26$smooth = ffcsaps(lts.M26$Avg_CC_Median, nyrs = nyrs)

M26Thu.p = ggplot(data = rwi.M26Thu, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.M26Thu, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("10Thu (0.55*)") +
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
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
M26Thu.p

### M26Dec / 17Dec ###
# Add Spline and then cutoff RWI pre-1972
rwi.M26Dec = add_rownames(rwl.M26Dec.mne.crn, var = "year")
rwi.M26Dec$year = as.numeric(rwi.M26Dec$year) # Years column in chr by default
rwi.M26Dec$smooth = ffcsaps(rwi.M26Dec$xxxstd, nyrs = nyrs)
rwi.M26Dec = subset(rwi.M26Dec, year >= 1972)

# Add Spline to LTS
lts.M26$smooth = ffcsaps(lts.M26$Avg_CC_Median, nyrs = nyrs)

M26Dec.p = ggplot(data = rwi.M26Dec, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.M26Dec, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.M26, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("10Dec (-0.17)") +
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
        plot.title = element_text(size = 10, vjust = -1))
M26Dec.p

### F30Bet / 27Bet ###
# Add Spline and then cutoff RWI pre-1972
rwi.F30Bet = add_rownames(rwl.F30Bet.mne.crn, var = "year")
rwi.F30Bet$year = as.numeric(rwi.F30Bet$year) # Years column in chr by default
rwi.F30Bet$smooth = ffcsaps(rwi.F30Bet$xxxstd, nyrs = nyrs)
rwi.F30Bet = subset(rwi.F30Bet, year >= 1972)

# Add Spline to LTS
lts.F30$smooth = ffcsaps(lts.F30$Avg_CC_Median, nyrs = nyrs)

F30Bet.p = ggplot(data = rwi.F30Bet, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.F30Bet, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("15Bet (-0.10)") +
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
        plot.title = element_text(size = 10, vjust = -1))
F30Bet.p

### F30Ace / 27Ace ###
# Add Spline and then cutoff RWI pre-1972
rwi.F30Ace = add_rownames(rwl.F30Ace.mne.crn, var = "year")
rwi.F30Ace$year = as.numeric(rwi.F30Ace$year) # Years column in chr by default
rwi.F30Ace$smooth = ffcsaps(rwi.F30Ace$xxxstd, nyrs = nyrs)
rwi.F30Ace = subset(rwi.F30Ace, year >= 1972)

# Add Spline to LTS
lts.F30$smooth = ffcsaps(lts.F30$Avg_CC_Median, nyrs = nyrs)

F30Ace.p = ggplot(data = rwi.F30Ace, aes(x = year, y = xxxstd)) + 
  geom_line(col = "black") + 
  geom_line(data = rwi.F30Ace, aes(x = year, y = smooth), col = "black", lwd = 1) +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (Avg_CC_Median - 72)/ 12.18),
            col = "red3") +
  geom_line(data = lts.F30, aes(x = 1972:2018, y = (smooth - 72) / 12.18),
            col = "red3", lwd = 1) +
  ggtitle("15Ace (-0.11)") +
  scale_x_continuous(name = NULL, expand = c(0,0), breaks = seq(1980,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(0,2.3),
                     breaks = seq(0,2,0.5), labels = NULL,
                     sec.axis = sec_axis(~ . * 12.18 + 72, breaks = seq(75,100,5), name = "%CC")) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y.right = element_text(size = 12, angle = 90, 
                                          color = "red3", face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y.left = unit(-0.15, "cm"),
        axis.ticks.length.y.right = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0, 0.1), "cm"),
        plot.title = element_text(size = 10, vjust = -1))
F30Ace.p
#####
tiff("multiRWIsites1.tiff", units = "in", width = 6.5, height = 4, res = 300)
F15Que.p + M06Ace.p + M26Thu.p + F30Bet.p + 
  F15Ace.p + M06Que.p + M26Dec.p + F30Ace.p + 
  F15Car.p + plot_layout(ncol = 4)
dev.off()
