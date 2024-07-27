setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology") 

library(ggplot2)
library(patchwork)

treeringsites = read.csv("DendroSites/treeringsites1.csv")

treeringsites$FINAL = as.factor(treeringsites$FINAL)
treeringsites$SIG = as.factor(treeringsites$SIG)
treeringsites$SIG_LAG = as.factor(treeringsites$SIG_LAG)

treeringsites = subset(treeringsites, FINAL == 1)

Cor.p = ggplot(data = treeringsites) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_point(aes(x = 1, y = COR, color = SIG)) +
  scale_color_manual(values = c("0" = "black", "1" = "red3")) + 
  geom_point(aes(x = 1, y = mean(COR)), shape = 4, size = 4) +
  scale_x_continuous(name = "All") +
  scale_y_continuous(name = "RWI-CC Correlation", expand = c(0,0), limits = c(-0.23, 0.65)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
Cor.p

#Cor.Lag.p = ggplot(data = treeringsites) +
#  geom_hline(yintercept = 0, col = "gray48") +
#  geom_point(aes(x = 1, y = COR_LAG, color = SIG_LAG)) +
#  scale_color_manual(values = c("0" = "black", "1" = "red3")) + 
#  geom_point(aes(x = 1, y = mean(COR_LAG)), shape = 4, size = 4) +
#  scale_x_continuous(name = "All (previous-year CC)") +
#  scale_y_continuous(expand = c(0,0), limits = c(-0.35, 0.65)) +
#  theme(axis.text.x = element_blank(),
#        axis.title.x = element_text(size = 10),
#        axis.title.y = element_blank(),
#        axis.text.y = element_blank(),
#        axis.ticks = element_line(color = "black"),
#        axis.ticks.length.y = unit(-0.15, "cm"),
#        axis.ticks.length.x = unit(0, "cm"),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = "black", fill = NA),
#        plot.margin = unit(c(0,0,0,0), "cm"),
#        legend.position = "none")
#Cor.Lag.p

Cor.Land.p = ggplot(data = treeringsites) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_point(aes(x = LAND, y = COR, color = SIG)) +
  scale_color_manual(values = c("0" = "black", "1" = "red3")) + 
  geom_point(aes(x = 1, y = mean(COR[LAND == "Rur"])), shape = 4, size = 4) +
  geom_point(aes(x = 2, y = mean(COR[LAND == "Urb"])), shape = 4, size = 4) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.23, 0.65)) +
  scale_x_discrete(name = "Landscape") +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
Cor.Land.p

Cor.Type.p = ggplot(data = treeringsites) +
  geom_hline(yintercept = 0, col = "gray48") +
  geom_point(aes(x = TYPE, y = COR, color = SIG)) +
  scale_color_manual(values = c("0" = "black", "1" = "red3")) + 
  geom_point(aes(x = 1, y = mean(COR[TYPE == "Con"])), shape = 4, size = 4) +
  geom_point(aes(x = 2, y = mean(COR[TYPE == "Dec"])), shape = 4, size = 4) +
  scale_x_discrete(name = "Forest Type") +
  scale_y_continuous(expand = c(0,0), limits = c(-0.23, 0.65)) +
  theme(axis.text.x = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(-0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.position = "none")
Cor.Type.p

#Cor.Genus.p = ggplot(data = subset(treeringsites, GENUS == "Ace" | GENUS == "Thu")) +
#  geom_hline(yintercept = 0, col = "gray48") +
#  geom_point(aes(x = GENUS, y = COR, color = SIG)) +
#  scale_color_manual(values = c("0" = "black", "1" = "red3")) + 
#  geom_point(aes(x = 1, y = mean(COR[GENUS == "Ace"])), shape = 4, size = 4) +
#  geom_point(aes(x = 2, y = mean(COR[GENUS == "Thu"])), shape = 4, size = 4) +
#  scale_x_discrete(name = "Genus") +
#  scale_y_continuous(expand = c(0,0), limits = c(-0.23, 0.65)) +
#  theme(axis.text.x = element_text(size = 10),
#        axis.title.x = element_text(size = 10),
#        axis.title.y = element_blank(),
#        axis.text.y = element_blank(),
#        axis.ticks = element_line(color = "black"),
#        axis.ticks.length.y = unit(-0.15, "cm"),
#        axis.ticks.length.x = unit(0.15, "cm"),
#        panel.background = element_blank(),
#        panel.border = element_rect(color = "black", fill = NA),
#        plot.margin = unit(c(0,0,0,0), "cm"),
#        legend.position = "none")
#Cor.Genus.p

tiff("SiteCor2_20.tiff", units = "in", width = 3, height = 3.5, res = 300)
Cor.p | Cor.Land.p | Cor.Type.p
dev.off()
