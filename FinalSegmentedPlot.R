##### Final Segmented Regression Plot #####

##### Set up and Data #####
library(ggplot2)
library(patchwork)
setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 
#Laptop
#setwd("C:/Users/bonneymi/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology")
# Desktop

rwi = read.csv("FINAL TIMESERIES/RWI/combined_rwi_20.csv")
lts = read.csv("FINAL TIMESERIES/LTS/combined_lts.csv")

yrseg_rwi = read.csv("YearlySegmented_RWI1_20.csv")
yrseg_lts = read.csv("YearlySegmented_LTS1_20.csv")
#####

##### Original Plot #####
### Stacked noisy RWI and LTS values ###
###
noise_rwi.p = ggplot(data = rwi) +
  geom_line(aes(x = Year, y = F07Ace), alpha = 0.25) + 
  geom_line(aes(x = Year, y = F15Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F15Que), alpha = 0.25) +
  geom_line(aes(x = Year, y = F21Dec), alpha = 0.25) +
  geom_line(aes(x = Year, y = F23Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F25Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30Bet), alpha = 0.25) +
  geom_line(aes(x = Year, y = F33Pic), alpha = 0.25) +
  geom_line(aes(x = Year, y = M01Pop), alpha = 0.25) +
  geom_line(aes(x = Year, y = M05Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06Que), alpha = 0.25) +
  geom_line(aes(x = Year, y = M07Tsu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M13Tsu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M17Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M20Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26Dec), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M27Pin), alpha = 0.25) +
  geom_line(aes(x = Year, y = Avg), lwd = 1) + 
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
noise_rwi.p

noise_lts.p = ggplot(data = lts) +
  geom_line(aes(x = Year, y = F07), alpha = 0.25) +
  geom_line(aes(x = Year, y = F15), alpha = 0.25) +
  geom_line(aes(x = Year, y = F21), alpha = 0.25) +
  geom_line(aes(x = Year, y = F23), alpha = 0.25) +
  geom_line(aes(x = Year, y = F25), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30), alpha = 0.25) +
  geom_line(aes(x = Year, y = F33), alpha = 0.25) +
  geom_line(aes(x = Year, y = M01), alpha = 0.25) +
  geom_line(aes(x = Year, y = M05), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06), alpha = 0.25) +
  geom_line(aes(x = Year, y = M07), alpha = 0.25) +
  geom_line(aes(x = Year, y = M13), alpha = 0.25) +
  geom_line(aes(x = Year, y = M17), alpha = 0.25) +
  geom_line(aes(x = Year, y = M20), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26), alpha = 0.25) +
  geom_line(aes(x = Year, y = M27), alpha = 0.25) +
  geom_line(aes(x = Year, y = Avg), lwd = 1) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "CC (%)") +
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
noise_lts.p

###
noise_rwi.p / noise_lts.p

### Histogram of breakpoint years ###
###

### RWI ###
yrseg_rwi$brksum = rowSums(yrseg_rwi[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81)], 
                           na.rm = TRUE)
sum(yrseg_rwi$brksum)
plot(yrseg_rwi$Year, yrseg_rwi$brksum, type = "l")

rwi_brks = data.frame(matrix(ncol = 2, nrow = sum(yrseg_rwi$brksum)))
colnames(rwi_brks) = c("brkyr", "ntrend")
#write.csv(rwi_brks, "rwi_brks.csv")

yrseg_rwi_brk = yrseg_rwi[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65,69,73,77,81)]
yrseg_rwi_brk[is.na(yrseg_rwi_brk)] = 0 # Helps for/if statement work
yrseg_rwi_sig = yrseg_rwi[c(3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63,67,71,75,79)]
#yrseg_rwi_sigc = yrseg_rwi[c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80)]

for (i in 1:nrow(yrseg_rwi_brk)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_rwi_brk)) {
 if(yrseg_rwi_brk[i,j] == 1)  {
    #rwi_brks[i,1] = yrseg_rwi[i,1]
    #print(yrseg_rwi[i,1])
    #rwi_brks[i,2] = yrseg_rwi_sig[i,j] 
   print(yrseg_rwi_sig[i + 1,j]) # Trend during the after the breakpoint
 } 
  }}
#rwi_brks = rwi_brks[complete.cases(rwi_brks),]
rwi_brks = read.csv("rwi_brks.csv")
rwi_brks$ntrend = as.factor(rwi_brks$ntrend)
levels(rwi_brks$ntrend)
rwi_brks$ntrend = factor(rwi_brks$ntrend, levels = c("1", "0", "-1"))

brk_rwi.p = ggplot(rwi_brks) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(RWI)", breaks = seq(1,7,2)) +
  annotate(geom = "text", x = 1902, y = 7.25, label = "B", fontface = "bold") +
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
brk_rwi.p

### LTS ###
yrseg_lts$brksum = rowSums(yrseg_lts[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65)], na.rm = TRUE)
sum(yrseg_lts$brksum)
plot(yrseg_lts$Year, yrseg_lts$brksum, type = "l")

lts_brks = data.frame(matrix(ncol = 2, nrow = sum(yrseg_lts$brksum)))
colnames(lts_brks) = c("brkyr", "ntrend")
#write.csv(lts_brks, "lts_brks.csv")

yrseg_lts_brk = yrseg_lts[c(5,9,13,17,21,25,29,33,37,41,45,49,53,57,61,65)]
yrseg_lts_brk[is.na(yrseg_lts_brk)] = 0 # Helps for/if statement work
yrseg_lts_sig = yrseg_lts[c(3,7,11,15,19,23,27,31,35,39,43,47,51,55,59,63)]

for (i in 1:nrow(yrseg_lts_brk)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_lts_brk)) {
    if(yrseg_lts_brk[i,j] == 1)  {
      #lts_brks[i,1] = yrseg_lts[i,1]
      #print(yrseg_lts[i,1])
      #lts_brks[i,2] = yrseg_lts_sig[i,j] 
      print(yrseg_lts_sig[i + 1,j]) # Trend during the after the breakpoint
    } 
  }}

lts_brks = read.csv("lts_brks.csv")
lts_brks$ntrend = as.factor(lts_brks$ntrend)
levels(lts_brks$ntrend)
lts_brks$ntrend = factor(lts_brks$ntrend, levels = c("1", "0", "-1"))

brk_lts.p = ggplot(lts_brks) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(CC)", breaks = seq(1,7,2)) +
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
brk_lts.p
###
brk_rwi.p / brk_lts.p

### Stacked segments ###
###

### RWI ###
# Sig columns need to be factors
yrseg_rwi$X01Tsu_sig = as.factor(yrseg_rwi$X01Tsu_sig)
yrseg_rwi$X02Que_sig = as.factor(yrseg_rwi$X02Que_sig)
yrseg_rwi$X03Ace_sig = as.factor(yrseg_rwi$X03Ace_sig)
yrseg_rwi$X04Tsu_sig = as.factor(yrseg_rwi$X04Tsu_sig)
yrseg_rwi$X05Ace_sig = as.factor(yrseg_rwi$X05Ace_sig)
yrseg_rwi$X06Dec_sig = as.factor(yrseg_rwi$X06Dec_sig)
yrseg_rwi$X07Thu_sig = as.factor(yrseg_rwi$X07Thu_sig)
yrseg_rwi$X08Ace_sig = as.factor(yrseg_rwi$X08Ace_sig)
yrseg_rwi$X09Pin_sig = as.factor(yrseg_rwi$X09Pin_sig)
yrseg_rwi$X10Thu_sig = as.factor(yrseg_rwi$X10Thu_sig)
yrseg_rwi$X11Ace_sig = as.factor(yrseg_rwi$X11Ace_sig)
yrseg_rwi$X12Pop_sig = as.factor(yrseg_rwi$X12Pop_sig)
yrseg_rwi$X13Pic_sig = as.factor(yrseg_rwi$X13Pic_sig)
yrseg_rwi$X14Thu_sig = as.factor(yrseg_rwi$X14Thu_sig)
yrseg_rwi$X15Bet_sig = as.factor(yrseg_rwi$X15Bet_sig)
yrseg_rwi$X16Thu_sig = as.factor(yrseg_rwi$X16Thu_sig)
yrseg_rwi$X02Ace_sig = as.factor(yrseg_rwi$X02Ace_sig)
yrseg_rwi$X03Que_sig = as.factor(yrseg_rwi$X03Que_sig)
yrseg_rwi$X10Dec_sig = as.factor(yrseg_rwi$X10Dec_sig)
yrseg_rwi$X15Ace_sig = as.factor(yrseg_rwi$X15Ace_sig)

# Add average columns
yrseg_rwi$Avg = rowMeans(yrseg_rwi[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,66,70,74,78)], 
                         na.rm = TRUE)
yrseg_rwi$Avg[is.na(rwi$Avg[2:nrow(rwi)])] = NA
yrseg_rwi$ConAvg = rowMeans(yrseg_rwi[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_rwi$ConAvg[is.na(rwi$ConAvg[2:nrow(rwi)])] = NA
yrseg_rwi$DecAvg = rowMeans(yrseg_rwi[c(6,10,18,22,30,42,46,58,66,70,74,78)], na.rm = TRUE)
yrseg_rwi$DecAvg[is.na(rwi$DecAvg[2:nrow(rwi)])] = NA

seg_rwi.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01Tsu_slp, color = X01Tsu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X02Que_slp, color = X02Que_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X02Ace_slp, color = X02Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X03Ace_slp, color = X03Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X03Que_slp, color = X03Que_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X04Tsu_slp, color = X04Tsu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X05Ace_slp, color = X05Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X06Dec_slp, color = X06Dec_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X07Thu_slp, color = X07Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X08Ace_slp, color = X08Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X09Pin_slp, color = X09Pin_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X10Thu_slp, color = X10Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X10Dec_slp, color = X10Dec_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X11Ace_slp, color = X11Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X12Pop_slp, color = X12Pop_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X13Pic_slp, color = X13Pic_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X14Thu_slp, color = X14Thu_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X15Bet_slp, color = X15Bet_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X15Ace_slp, color = X15Ace_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X16Thu_slp, color = X16Thu_sig), shape = 15, alpha = 0.5) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), breaks = seq(-0.2,0.3,0.1),
                     labels = c("",-0.1,0.0,0.1,0.2,0.3)) +
  annotate(geom = "text", x = 1902, y = 0.265, label = "C", fontface = "bold") +
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
seg_rwi.p

### LTS ###
# Sig columns need to be factors
yrseg_lts$X01LTS_sig = as.factor(yrseg_lts$X01LTS_sig)
yrseg_lts$X02LTS_sig = as.factor(yrseg_lts$X02LTS_sig)
yrseg_lts$X03LTS_sig = as.factor(yrseg_lts$X03LTS_sig)
yrseg_lts$X04LTS_sig = as.factor(yrseg_lts$X04LTS_sig)
yrseg_lts$X05LTS_sig = as.factor(yrseg_lts$X05LTS_sig)
yrseg_lts$X06LTS_sig = as.factor(yrseg_lts$X06LTS_sig)
yrseg_lts$X07LTS_sig = as.factor(yrseg_lts$X07LTS_sig)
yrseg_lts$X08LTS_sig = as.factor(yrseg_lts$X08LTS_sig)
yrseg_lts$X09LTS_sig = as.factor(yrseg_lts$X09LTS_sig)
yrseg_lts$X10LTS_sig = as.factor(yrseg_lts$X10LTS_sig)
yrseg_lts$X11LTS_sig = as.factor(yrseg_lts$X11LTS_sig)
yrseg_lts$X12LTS_sig = as.factor(yrseg_lts$X12LTS_sig)
yrseg_lts$X13LTS_sig = as.factor(yrseg_lts$X13LTS_sig)
yrseg_lts$X14LTS_sig = as.factor(yrseg_lts$X14LTS_sig)
yrseg_lts$X15LTS_sig = as.factor(yrseg_lts$X15LTS_sig)
yrseg_lts$X16LTS_sig = as.factor(yrseg_lts$X16LTS_sig)

yrseg_lts$Avg = rowMeans(yrseg_lts[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62)], na.rm = TRUE)
yrseg_lts$ConAvg = rowMeans(yrseg_lts[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_lts$DecAvg = rowMeans(yrseg_lts[c(6,10,18,22,30,42,46,58)], na.rm = TRUE)

seg_lts.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01LTS_slp, color = X01LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X02LTS_slp, color = X02LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X03LTS_slp, color = X03LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X04LTS_slp, color = X04LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X05LTS_slp, color = X05LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X06LTS_slp, color = X06LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X07LTS_slp, color = X07LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X08LTS_slp, color = X08LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X09LTS_slp, color = X09LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X10LTS_slp, color = X10LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X11LTS_slp, color = X11LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X12LTS_slp, color = X12LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X13LTS_slp, color = X13LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X14LTS_slp, color = X14LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X15LTS_slp, color = X15LTS_sig), shape = 15, alpha = 0.5) +
  geom_point(aes(x = Year, y = X16LTS_slp, color = X16LTS_sig), shape = 15, alpha = 0.5) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("CC (%) yr"^"-1"))) +
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
seg_lts.p
###
seg_rwi.p / seg_lts.p 

### Avg of Stacked segments ###
###
segavg_rwi.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = Avg), color = "black", lwd = 1.5) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0), limits = c(-0.04,0.04)) +
  annotate(geom = "text", x = 1902, y = 0.0315, label = "D", fontface = "bold") +
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
segavg_rwi.p

segavg_lts.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = Avg), color = "black", lwd = 1.5) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("CC (%) yr"^"-1")), expand = c(0,0), 
                     limits = c(-0.9,0.9)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
segavg_lts.p
###
segavg_rwi.p / segavg_lts.p
#####
#tiff("segmentedflat2_20.tiff", units = "in", width = 6.5, height = 8, res = 300)
noise_rwi.p / noise_lts.p / brk_rwi.p / brk_lts.p / 
  seg_rwi.p / seg_lts.p /segavg_rwi.p / segavg_lts.p
#dev.off()

##### Split Forest Type Plot #####
##### Stacked noisy RWI and LTS values #####
#####
noise_rwi_con.p = ggplot(data = rwi) +
  geom_line(aes(x = Year, y = F33Pic), alpha = 0.25) +
  geom_line(aes(x = Year, y = M05Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M07Tsu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M13Tsu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M17Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M20Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26Thu), alpha = 0.25) +
  geom_line(aes(x = Year, y = M27Pin), alpha = 0.25) +
  geom_line(aes(x = Year, y = ConAvg), lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "RWI", expand = c(0,0)) +
  coord_cartesian(ylim = c(0.25,1.75)) +
  ggtitle("Coniferous") +
  annotate(geom = "text", x = 1905, y = 1.5, label = "A", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0,0), "cm"),
        legend.position = "none",
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
noise_rwi_con.p

noise_rwi_dec.p = ggplot(data = rwi) +
  geom_line(aes(x = Year, y = F07Ace), alpha = 0.25) + 
  geom_line(aes(x = Year, y = F15Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F15Que), alpha = 0.25) +
  geom_line(aes(x = Year, y = F21Dec), alpha = 0.25) +
  geom_line(aes(x = Year, y = F23Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F25Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30Bet), alpha = 0.25) +
  geom_line(aes(x = Year, y = M01Pop), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06Ace), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06Que), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26Dec), alpha = 0.25) +
  geom_line(aes(x = Year, y = DecAvg), lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0)) +
  coord_cartesian(ylim = c(0.25,1.75)) +
  ggtitle("Deciduous") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0.1), "cm"),
        legend.position = "none",
        plot.title = element_text(size = 10, vjust = -1, face = "bold"))
noise_rwi_dec.p

noise_lts_con.p = ggplot(data = lts) +
  geom_line(aes(x = Year, y = F33), alpha = 0.25) +
  geom_line(aes(x = Year, y = M05), alpha = 0.25) +
  geom_line(aes(x = Year, y = M07), alpha = 0.25) +
  geom_line(aes(x = Year, y = M13), alpha = 0.25) +
  geom_line(aes(x = Year, y = M17), alpha = 0.25) +
  geom_line(aes(x = Year, y = M20), alpha = 0.25) +
  geom_line(aes(x = Year, y = M26), alpha = 0.25) +
  geom_line(aes(x = Year, y = M27), alpha = 0.25) +
  geom_line(aes(x = Year, y = ConAvg), lwd = 1) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "CC (%)", expand = c(0,0), limits = c(72,101)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0), "cm"),
        legend.position = "none")
noise_lts_con.p

noise_lts_dec.p = ggplot(data = lts) +
  geom_line(aes(x = Year, y = F07), alpha = 0.25) +
  geom_line(aes(x = Year, y = F15), alpha = 0.25) +
  geom_line(aes(x = Year, y = F21), alpha = 0.25) +
  geom_line(aes(x = Year, y = F23), alpha = 0.25) +
  geom_line(aes(x = Year, y = F25), alpha = 0.25) +
  geom_line(aes(x = Year, y = F30), alpha = 0.25) +
  geom_line(aes(x = Year, y = M01), alpha = 0.25) +
  geom_line(aes(x = Year, y = M06), alpha = 0.25) +
  #geom_line(aes(x = Year, y = M26), alpha = 0.25) + # Mixed site (not in average currently)
  geom_line(aes(x = Year, y = DecAvg), lwd = 1) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(72,101)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.1), "cm"),
        legend.position = "none")
noise_lts_dec.p
#####
(noise_rwi_con.p | noise_rwi_dec.p) /
  (noise_lts_con.p | noise_lts_dec.p)

##### Histogram of breakpoint years #####
#####
### RWI ###
yrseg_rwi$brksum_con = rowSums(yrseg_rwi[c(5,17,29,37,41,53,57,65)], 
                           na.rm = TRUE)
sum(yrseg_rwi$brksum_con)
plot(yrseg_rwi$Year,yrseg_rwi$brksum_con, type = "l")

rwi_brks_con = data.frame(matrix(ncol = 2, nrow = sum(yrseg_rwi$brksum_con)))
colnames(rwi_brks_con) = c("brkyr", "ntrend")
#write.csv(rwi_brks_con, "rwi_brks_con.csv")

yrseg_rwi_brk_con = yrseg_rwi[c(5,17,29,37,41,53,57,65)]
yrseg_rwi_brk_con[is.na(yrseg_rwi_brk_con)] = 0 # Helps for/if statement work
yrseg_rwi_sig_con = yrseg_rwi[c(3,15,27,35,39,51,55,63)]

for (i in 1:nrow(yrseg_rwi_brk_con)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_rwi_brk_con)) {
    if(yrseg_rwi_brk_con[i,j] == 1)  {
      #rwi_brks[i,1] = yrseg_rwi[i,1]
      #print(yrseg_rwi[i,1])
      #rwi_brks[i,2] = yrseg_rwi_sig[i,j] 
      print(yrseg_rwi_sig_con[i + 1,j]) # Trend during the after the breakpoint
    } 
  }}

rwi_brks_con = read.csv("rwi_brks_con.csv")
rwi_brks_con$ntrend = as.factor(rwi_brks_con$ntrend)
levels(rwi_brks_con$ntrend)
rwi_brks_con$ntrend = factor(rwi_brks_con$ntrend, levels = c("1", "0", "-1"))

brk_rwi_con.p = ggplot(rwi_brks_con) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(RWI)", limits = c(0,7), breaks = seq(1,7,2)) +
  annotate(geom = "text", x = 1905, y = 6.25, label = "B", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
brk_rwi_con.p

yrseg_rwi$brksum_dec = rowSums(yrseg_rwi[c(9,13,21,25,33,45,49,61,69,73,77,81)], 
                           na.rm = TRUE)
sum(yrseg_rwi$brksum_dec)
plot(yrseg_rwi$Year, yrseg_rwi$brksum_dec, type = "l")

rwi_brks_dec = data.frame(matrix(ncol = 2, nrow = sum(yrseg_rwi$brksum_dec)))
colnames(rwi_brks_dec) = c("brkyr", "ntrend")
#write.csv(rwi_brks_dec, "rwi_brks_dec.csv")

yrseg_rwi_brk_dec = yrseg_rwi[c(9,13,21,25,33,45,49,61,69,73,77,81)]
yrseg_rwi_brk_dec[is.na(yrseg_rwi_brk_dec)] = 0 # Helps for/if statement work
yrseg_rwi_sig_dec = yrseg_rwi[c(7,11,19,23,31,43,47,59,67,71,75,79)]
#yrseg_rwi_sigc = yrseg_rwi[c(4,8,12,16,20,24,28,32,36,40,44,48,52,56,60,64,68,72,76,80)]

for (i in 1:nrow(yrseg_rwi_brk_dec)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_rwi_brk_dec)) {
    if(yrseg_rwi_brk_dec[i,j] == 1)  {
      #rwi_brks[i,1] = yrseg_rwi[i,1]
      #print(yrseg_rwi[i,1])
      #rwi_brks[i,2] = yrseg_rwi_sig[i,j] 
      print(yrseg_rwi_sig_dec[i + 1,j]) # Trend during the after the breakpoint
    } 
  }}

rwi_brks_dec = read.csv("rwi_brks_dec.csv")
rwi_brks_dec$ntrend = as.factor(rwi_brks_dec$ntrend)
levels(rwi_brks_dec$ntrend)
rwi_brks_dec$ntrend = factor(rwi_brks_dec$ntrend, levels = c("1", "0", "-1"))

brk_rwi_dec.p = ggplot(rwi_brks_dec) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, limits = c(0,7), breaks = seq(1,7,2)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        legend.position = "none")
brk_rwi_dec.p

### LTS ###
yrseg_lts$brksum_con = rowSums(yrseg_lts[c(5,17,29,37,41,53,57,65)], na.rm = TRUE)
sum(yrseg_lts$brksum_con)
plot(yrseg_lts$Year, yrseg_lts$brksum_con, type = "l")

lts_brks_con = data.frame(matrix(ncol = 2, nrow = sum(yrseg_lts$brksum_con)))
colnames(lts_brks_con) = c("brkyr", "ntrend")
#write.csv(lts_brks_con, "lts_brks_con.csv")

yrseg_lts_brk_con = yrseg_lts[c(5,17,29,37,41,53,57,65)]
yrseg_lts_brk_con[is.na(yrseg_lts_brk_con)] = 0 # Helps for/if statement work
yrseg_lts_sig_con = yrseg_lts[c(3,15,27,35,39,51,55,63)]

for (i in 1:nrow(yrseg_lts_brk_con)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_lts_brk_con)) {
    if(yrseg_lts_brk_con[i,j] == 1)  {
      #lts_brks[i,1] = yrseg_lts[i,1]
      #print(yrseg_lts[i,1])
      #lts_brks[i,2] = yrseg_lts_sig[i,j] 
      print(yrseg_lts_sig_con[i + 1,j]) # Trend during the after the breakpoint
    } 
  }}

lts_brks_con = read.csv("lts_brks_con.csv")
lts_brks_con$ntrend = as.factor(lts_brks_con$ntrend)
levels(lts_brks_con$ntrend)
lts_brks_con$ntrend = factor(lts_brks_con$ntrend, levels = c("1", "0", "-1"))

brk_lts_con.p = ggplot(lts_brks_con) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = "Frequency\n(CC)", limits = c(0,4), breaks = seq(1,4,1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0), "cm"),
        legend.position = "none")
brk_lts_con.p

yrseg_lts$brksum_dec = rowSums(yrseg_lts[c(9,13,21,25,33,45,49,61)], na.rm = TRUE)
sum(yrseg_lts$brksum_dec)
plot(yrseg_lts$Year, yrseg_lts$brksum_dec, type = "l")

lts_brks_dec = data.frame(matrix(ncol = 2, nrow = sum(yrseg_lts$brksum_dec)))
colnames(lts_brks_dec) = c("brkyr", "ntrend")
#write.csv(lts_brks_dec, "lts_brks_dec.csv")

yrseg_lts_brk_dec = yrseg_lts[c(9,13,21,25,33,45,49,61)]
yrseg_lts_brk_dec[is.na(yrseg_lts_brk_dec)] = 0 # Helps for/if statement work
yrseg_lts_sig_dec = yrseg_lts[c(7,11,19,23,31,43,47,59)]

for (i in 1:nrow(yrseg_lts_brk_dec)) { # Future: vectorized functions better?
  for(j in 1:ncol(yrseg_lts_brk_dec)) {
    if(yrseg_lts_brk_dec[i,j] == 1)  {
      #lts_brks[i,1] = yrseg_lts[i,1]
      #print(yrseg_lts[i,1])
      #lts_brks[i,2] = yrseg_lts_sig[i,j] 
      print(yrseg_lts_sig_dec[i + 1,j]) # Trend during the after the breakpoint
    } 
  }}

lts_brks_dec = read.csv("lts_brks_dec.csv")
lts_brks_dec$ntrend = as.factor(lts_brks_dec$ntrend)
levels(lts_brks_dec$ntrend)
lts_brks_dec$ntrend = factor(lts_brks_dec$ntrend, levels = c("1", "0", "-1"))

brk_lts_dec.p = ggplot(lts_brks_dec) + 
  geom_histogram(aes(x = brkyr, fill = ntrend), binwidth = 5) +
  scale_fill_manual(values = c("-1" = "red3", "1" = "darkgreen", "0" = "black")) +
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, limits = c(0,4), breaks = seq(1,4,1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.1), "cm"),
        legend.position = "none")
brk_lts_dec.p
#####
(brk_rwi_con.p | brk_rwi_dec.p) /
  (brk_lts_con.p | brk_lts_dec.p)

##### Stacked segments #####
#####
### RWI ###
# Sig columns need to be factors
yrseg_rwi$X01Tsu_sig = as.factor(yrseg_rwi$X01Tsu_sig)
yrseg_rwi$X02Que_sig = as.factor(yrseg_rwi$X02Que_sig)
yrseg_rwi$X03Ace_sig = as.factor(yrseg_rwi$X03Ace_sig)
yrseg_rwi$X04Tsu_sig = as.factor(yrseg_rwi$X04Tsu_sig)
yrseg_rwi$X05Ace_sig = as.factor(yrseg_rwi$X05Ace_sig)
yrseg_rwi$X06Dec_sig = as.factor(yrseg_rwi$X06Dec_sig)
yrseg_rwi$X07Thu_sig = as.factor(yrseg_rwi$X07Thu_sig)
yrseg_rwi$X08Ace_sig = as.factor(yrseg_rwi$X08Ace_sig)
yrseg_rwi$X09Pin_sig = as.factor(yrseg_rwi$X09Pin_sig)
yrseg_rwi$X10Thu_sig = as.factor(yrseg_rwi$X10Thu_sig)
yrseg_rwi$X11Ace_sig = as.factor(yrseg_rwi$X11Ace_sig)
yrseg_rwi$X12Pop_sig = as.factor(yrseg_rwi$X12Pop_sig)
yrseg_rwi$X13Pic_sig = as.factor(yrseg_rwi$X13Pic_sig)
yrseg_rwi$X14Thu_sig = as.factor(yrseg_rwi$X14Thu_sig)
yrseg_rwi$X15Bet_sig = as.factor(yrseg_rwi$X15Bet_sig)
yrseg_rwi$X16Thu_sig = as.factor(yrseg_rwi$X16Thu_sig)
yrseg_rwi$X02Ace_sig = as.factor(yrseg_rwi$X02Ace_sig)
yrseg_rwi$X03Que_sig = as.factor(yrseg_rwi$X03Que_sig)
yrseg_rwi$X10Dec_sig = as.factor(yrseg_rwi$X10Dec_sig)
yrseg_rwi$X15Ace_sig = as.factor(yrseg_rwi$X15Ace_sig)

# Add average columns
yrseg_rwi$Avg = rowMeans(yrseg_rwi[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62,66,70,74,78)], 
                         na.rm = TRUE)
yrseg_rwi$Avg[is.na(rwi$Avg[2:nrow(rwi)])] = NA
yrseg_rwi$ConAvg = rowMeans(yrseg_rwi[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_rwi$ConAvg[is.na(rwi$ConAvg[2:nrow(rwi)])] = NA
yrseg_rwi$DecAvg = rowMeans(yrseg_rwi[c(6,10,18,22,30,42,46,58,66,70,74,78)], na.rm = TRUE)
yrseg_rwi$DecAvg[is.na(rwi$DecAvg[2:nrow(rwi)])] = NA

seg_rwi_con.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01Tsu_slp, color = X01Tsu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X04Tsu_slp, color = X04Tsu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X07Thu_slp, color = X07Thu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X09Pin_slp, color = X09Pin_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X10Thu_slp, color = X10Thu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X13Pic_slp, color = X13Pic_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X14Thu_slp, color = X14Thu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X16Thu_slp, color = X16Thu_sig), shape = 15, alpha = 0.5, size = 0.75) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), limits = c(-0.203,0.3), 
                     breaks = seq(-0.2,0.3,0.1), labels = c("",-0.1,0.0,0.1,0.2,0.3)) +
  annotate(geom = "text", x = 1905, y = 0.25, label = "C", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
seg_rwi_con.p

seg_rwi_dec.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X02Que_slp, color = X02Que_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X02Ace_slp, color = X02Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X03Ace_slp, color = X03Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X03Que_slp, color = X03Que_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X05Ace_slp, color = X05Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X06Dec_slp, color = X06Dec_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X08Ace_slp, color = X08Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X10Dec_slp, color = X10Dec_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X11Ace_slp, color = X11Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X12Pop_slp, color = X12Pop_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X15Bet_slp, color = X15Bet_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X15Ace_slp, color = X15Ace_sig), shape = 15, alpha = 0.5, size = 0.75) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, limits = c(-0.203,0.3), breaks = seq(-0.2,0.3,0.1)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        legend.position = "none")
seg_rwi_dec.p

### LTS ###
# Sig columns need to be factors
yrseg_lts$X01LTS_sig = as.factor(yrseg_lts$X01LTS_sig)
yrseg_lts$X02LTS_sig = as.factor(yrseg_lts$X02LTS_sig)
yrseg_lts$X03LTS_sig = as.factor(yrseg_lts$X03LTS_sig)
yrseg_lts$X04LTS_sig = as.factor(yrseg_lts$X04LTS_sig)
yrseg_lts$X05LTS_sig = as.factor(yrseg_lts$X05LTS_sig)
yrseg_lts$X06LTS_sig = as.factor(yrseg_lts$X06LTS_sig)
yrseg_lts$X07LTS_sig = as.factor(yrseg_lts$X07LTS_sig)
yrseg_lts$X08LTS_sig = as.factor(yrseg_lts$X08LTS_sig)
yrseg_lts$X09LTS_sig = as.factor(yrseg_lts$X09LTS_sig)
yrseg_lts$X10LTS_sig = as.factor(yrseg_lts$X10LTS_sig)
yrseg_lts$X11LTS_sig = as.factor(yrseg_lts$X11LTS_sig)
yrseg_lts$X12LTS_sig = as.factor(yrseg_lts$X12LTS_sig)
yrseg_lts$X13LTS_sig = as.factor(yrseg_lts$X13LTS_sig)
yrseg_lts$X14LTS_sig = as.factor(yrseg_lts$X14LTS_sig)
yrseg_lts$X15LTS_sig = as.factor(yrseg_lts$X15LTS_sig)
yrseg_lts$X16LTS_sig = as.factor(yrseg_lts$X16LTS_sig)

yrseg_lts$Avg = rowMeans(yrseg_lts[c(2,6,10,14,18,22,26,30,34,38,42,46,50,54,58,62)], na.rm = TRUE)
yrseg_lts$ConAvg = rowMeans(yrseg_lts[c(2,14,26,34,38,50,54,62)], na.rm = TRUE)
yrseg_lts$DecAvg = rowMeans(yrseg_lts[c(6,10,18,22,30,42,46,58)], na.rm = TRUE)

seg_lts_con.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X01LTS_slp, color = X01LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X04LTS_slp, color = X04LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X07LTS_slp, color = X07LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X09LTS_slp, color = X09LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X10LTS_slp, color = X10LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X13LTS_slp, color = X13LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X14LTS_slp, color = X14LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X16LTS_slp, color = X16LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("CC (%) yr"^"-1")), limits = c(-1.7,3.05)) + #6.3
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0.1,0), "cm"),
        legend.position = "none")
seg_lts_con.p

seg_lts_dec.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_point(aes(x = Year, y = X02LTS_slp, color = X02LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X03LTS_slp, color = X03LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X05LTS_slp, color = X05LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X06LTS_slp, color = X06LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X08LTS_slp, color = X08LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X11LTS_slp, color = X11LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X12LTS_slp, color = X12LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  geom_point(aes(x = Year, y = X15LTS_slp, color = X15LTS_sig), shape = 15, alpha = 0.5, size = 0.75) +
  scale_color_manual(values = c("0" = "black", "-1" = "red3", "1" = "darkgreen")) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018),
                     breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, limits = c(-1.7,3.05)) + #6.3
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0.1,0.1), "cm"),
        legend.position = "none")
seg_lts_dec.p
#####
(seg_rwi_con.p | seg_rwi_dec.p) / 
  (seg_lts_con.p | seg_lts_dec.p)

##### Avg of Stacked segments #####
#####
segavg_rwi_con.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = ConAvg), color = "black", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("RWI yr"^"-1")), expand = c(0,0), limits = c(-0.05,0.07),
                     breaks = seq(-0.04,0.06,0.02)) +
  annotate(geom = "text", x = 1905, y = 0.0515, label = "D", fontface = "bold") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0.1,0,0), "cm"),
        legend.position = "none")
segavg_rwi_con.p

segavg_rwi_dec.p = ggplot(data = yrseg_rwi) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = DecAvg), color = "black", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = NULL, expand = c(0,0), limits = c(-0.05,0.07), 
                     breaks = seq(-0.04,0.06,0.02)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(-0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0.1,0,0,0.1), "cm"),
        legend.position = "none")
segavg_rwi_dec.p

segavg_lts_con.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = ConAvg), color = "black", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("CC (%) yr"^"-1")), expand = c(0,0)) +
  coord_cartesian(ylim = c(-1,1.4)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0.1,0,0), "cm"),
        legend.position = "none")
segavg_lts_con.p

segavg_lts_dec.p = ggplot(data = yrseg_lts) +
  geom_hline(yintercept = 0, color = "gray48") + 
  geom_line(aes(x = Year, y = DecAvg), color = "black", lwd = 1) + 
  scale_x_continuous(name = NULL, expand = c(0,0), limits = c(1900,2018), breaks = seq(1900,2010,10)) +
  scale_y_continuous(name = expression(paste("CC (%) yr"^"-1")), expand = c(0,0), 
                     limits = c(-1,1.4)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0.1), "cm"),
        legend.position = "none")
segavg_lts_dec.p
#####
(segavg_rwi_con.p | segavg_rwi_dec.p) /
  (segavg_lts_con.p | segavg_lts_dec.p)

#####
#tiff("segmentedflat2_20_foresttype.tiff", units = "in", width = 6.5, height = 8, res = 300)
(noise_rwi_con.p | noise_rwi_dec.p) /
  (noise_lts_con.p | noise_lts_dec.p) / 
  (brk_rwi_con.p | brk_rwi_dec.p) /
  (brk_lts_con.p | brk_lts_dec.p) /
  (seg_rwi_con.p | seg_rwi_dec.p) / 
  (seg_lts_con.p | seg_lts_dec.p) / 
  (segavg_rwi_con.p | segavg_rwi_dec.p) /
  (segavg_lts_con.p | segavg_lts_dec.p)
#dev.off()

# R2: RWI vs. LTS (average slopes)
summary(lm(yrseg_lts$Avg[82:nrow(yrseg_lts)] ~ yrseg_rwi$Avg[82:nrow(yrseg_rwi)]))
plot(yrseg_lts$Avg[82:nrow(yrseg_lts)] ~ yrseg_rwi$Avg[82:nrow(yrseg_rwi)])
# All: R2 = 0.36***
summary(lm(yrseg_lts$ConAvg[82:nrow(yrseg_lts)] ~ yrseg_rwi$ConAvg[82:nrow(yrseg_rwi)]))
plot(yrseg_lts$ConAvg[82:nrow(yrseg_lts)] ~ yrseg_rwi$ConAvg[82:nrow(yrseg_rwi)])
# Con: R2 = 0.36***
summary(lm(yrseg_lts$DecAvg[82:nrow(yrseg_lts)] ~ yrseg_rwi$DecAvg[82:nrow(yrseg_rwi)]))
plot(yrseg_lts$DecAvg[82:nrow(yrseg_lts)] ~ yrseg_rwi$DecAvg[82:nrow(yrseg_rwi)])
# Dec: R2 = 0.15**

##### Adjust cor.test (used elsewhere in paper) #####
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
#####

# r: RWI vs. LTS (input time-series)
summary(lm(lts$Avg[82:nrow(lts)] ~ rwi$Avg[82:nrow(rwi)]))
plot(lts$Avg[82:nrow(lts)] ~ rwi$Avg[82:nrow(rwi)])
# All: r = 0.05
summary(lm(lts$ConAvg[82:nrow(lts)] ~ rwi$ConAvg[82:nrow(rwi)]))
plot(lts$ConAvg[82:nrow(lts)] ~ rwi$ConAvg[82:nrow(rwi)])
abline(lm(lts$ConAvg[82:nrow(lts)] ~ rwi$ConAvg[82:nrow(rwi)]))
# Con: r = 0.56***
summary(lm(lts$DecAvg[82:nrow(lts)] ~ rwi$DecAvg[82:nrow(rwi)]))
plot(lts$DecAvg[82:nrow(lts)] ~ rwi$DecAvg[82:nrow(rwi)])
abline(lm(lts$DecAvg[82:nrow(lts)] ~ rwi$DecAvg[82:nrow(rwi)]))
# Dec: r = -0.26

my.cor.test(lts$ConAvg[82:nrow(lts)], rwi$ConAvg[82:nrow(rwi)], alternative = "greater", method = "pearson", conf.level = 0.95,
            n = calc.neff(lts$ConAvg[82:nrow(lts)], rwi$ConAvg[82:nrow(rwi)])) # r = 0.56**
my.cor.test(lts$DecAvg[82:nrow(lts)], rwi$DecAvg[82:nrow(rwi)], alternative = "greater", method = "pearson", conf.level = 0.95,
            n = calc.neff(lts$DecAvg[82:nrow(lts)], rwi$DecAvg[82:nrow(rwi)])) # r = -0.27


