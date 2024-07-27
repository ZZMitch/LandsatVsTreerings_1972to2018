##### Create scatterplot of extra dendro/forest density variables vs. correlation #####

setwd("C:/Users/mitch/Google Drive/PHD/Thesis Research/Landsat Time-Series/Validate/Change Validation/Dendrochronology")

library(ggplot2)
library(patchwork)

tbl = read.csv("Cor_ExtraVars.csv")

### # of cores ###
ncores.p = ggplot(data = tbl, aes(x = ncores, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 43, y = 0.60, label = "r = 0.09, p = 0.70") +
  annotate(geom = "text", x = 17, y = 0.60, label = "A", fontface = "bold") +
  scale_x_continuous(name = "Number of cores") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0), "cm"))
ncores.p

### Mean series age (years) ###
age.p = ggplot(data = tbl, aes(x = age, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 80, y = 0.60, label = "r = 0.03, p = 0.91") +
  annotate(geom = "text", x = 39, y = 0.60, label = "B", fontface = "bold") +
  scale_x_continuous(name = "Mean series age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0.1, 0.1, 0.1), "cm"))
age.p

### Mean TRW (mm) ###
trw.p = ggplot(data = tbl, aes(x = trw, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 2.65, y = 0.60, label = "r = 0.13, p = 0.58") +
  annotate(geom = "text", x = 1.4, y = 0.60, label = "C", fontface = "bold") +
  scale_x_continuous(name = "Mean TRW (mm)") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0, 0, 0.1, 0.1), "cm"))
trw.p

### Series inter-correlation ###
si.p = ggplot(data = tbl, aes(x = si, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 0.635, y = 0.60, label = "r = -0.38, p = 0.10") +
  annotate(geom = "text", x = 0.5, y = 0.60, label = "D", fontface = "bold") +
  scale_x_continuous(name = "Series inter-correlation", breaks = seq(0.5,0.7,0.1)) +
  scale_y_continuous(name = "RWI-CC Correlation") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0), "cm"))
si.p

### AR1 ###
ar1.p = ggplot(data = tbl, aes(x = ar1, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 0.75, y = 0.60, label = "r = 0.30, p = 0.20") +
  annotate(geom = "text", x = 0.525, y = 0.60, label = "E", fontface = "bold") +
  scale_x_continuous(name = "AR1") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
ar1.p

### EPS ###
eps.p = ggplot(data = tbl, aes(x = eps, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 0.925, y = 0.60, label = "r = -0.17, p = 0.47") +
  annotate(geom = "text", x = 0.84, y = 0.60, label = "F", fontface = "bold") +
  scale_x_continuous(name = "EPS") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0, 0.1, 0.1), "cm"))
eps.p

### CC (%) ###
cc.p = ggplot(data = tbl, aes(x = cc, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 86.5, y = 0.60, label = "r = -0.14, p = 0.54") +
  annotate(geom = "text", x = 76, y = 0.60, label = "G", fontface = "bold") +
  scale_x_continuous(name = "CC (%)") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0), "cm"))
cc.p

### Stem density (stems per ha) ###
sd.p = ggplot(data = tbl, aes(x = sd, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 1100, y = 0.60, label = "r = -0.09, p = 0.71") +
  annotate(geom = "text", x = 450, y = 0.60, label = "H", fontface = "bold") +
  scale_x_continuous(name = "Stem density (stems per ha)") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0.1, 0, 0.1), "cm"))
sd.p

### Basal area (m² per ha) ###
ba.p = ggplot(data = tbl, aes(x = ba, y = Cor)) +
  geom_hline(yintercept = 0, color = "gray48") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red3") + 
  annotate(geom = "text", x = 75, y = 0.60, label = "r = 0.29, p = 0.22") +
  annotate(geom = "text", x = 25, y = 0.60, label = "I", fontface = "bold") +
  scale_x_continuous(name = "Basal area (m² per ha)") +
  theme(axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_line(color = "black"),
        axis.ticks.length.y = unit(0.15, "cm"),
        axis.ticks.length.x = unit(0.15, "cm"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        plot.margin = unit(c(0.1, 0, 0, 0.1), "cm"))
ba.p

tiff("cor_extravars.tif", units = "in", width = 6.5, height = 6.5, res = 300)
(ncores.p | age.p | trw.p) /
  (si.p | ar1.p | eps.p) /
  (cc.p | sd.p | ba.p)
dev.off()