library(tidyverse)
library(rcompanion)
library(fitdistrplus)
#library(QuantPsyc)
library(betareg)
library(ggpubr)
#library(jtools)
theme_set(theme_bw())

#data = readRDS("results/bb-data.rds")
source("analysis_scripts/filter-out-unsuccessful-runs.R")

parameters = c(
  "cycle-duration",  # 100, 250
  "veg-cycle-start", # productive, unproductive
  "veg-distribution", # random, clustered
  "burnt-neighbor-limit", # 1, 4, 8
  "burn-cost", # 0, 50
  "burn-veg-type-threshold", # 4, 7
  "movement-model" #Random, Directed
)
data$`cycle-duration` = as.factor(data$`cycle-duration`)
data$`veg-cycle-start` = as.factor(data$`veg-cycle-start`)
data$`veg-distribution` = as.factor(data$`veg-distribution`)
data$`burnt-neighbor-limit` = as.factor(data$`burnt-neighbor-limit`)
data$`burn-cost` = as.factor(data$`burn-cost`)
data$`burn-veg-type-threshold` = as.factor(data$`burn-veg-type-threshold`)
data$`movement-model` = as.factor(data$`movement-model`)

data$adj.fi = data$mean.fi/data$pop.count
data$pop.dens = data$pop.count/(51*51)
#data$benefit.ratio = data$benefit_self/data$benefit_other
data$bself.prop = data$benefit_self / (data$pop.count * data$mean.fm)
data$bother.prop = data$benefit_other / (data$pop.count * data$mean.fm)
data$burnt = data$`veg_-1`


# plotNormalHistogram(data$burnt)
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `natural-ignition`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `cycle-duration`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `veg-cycle-start`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `veg-distribution`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `burnt-neighbor-limit`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `burn-cost`))
# #decreasing burn cost shifts the distribution right
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `burn-veg-type-threshold`))
# 
# ggplot(data) +
#   geom_density(aes(x = burnt, color = `movement-model`))
# 
# # ggplot(data) +
# #   geom_smooth(aes(x = ticks, y = burnt, color = `burn-cost`), se = T)

plot.data = data %>% select_at(c("ticks", "burnt", parameters)) %>%
  filter(`burnt-neighbor-limit` %in% c(8,1))

bt.labs = c("can disturb veg types 1-4", "can disturb veg types 1-7")
names(bt.labs) = c(4, 7)
bc.labs = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")
names(bc.labs) = c(0, 100, 200, 300)
bn.labs = c("disturb with 8 disturbed neighbors", 
            "disturb with 4 disturbed neighbors", 
            "disturb with 1 disturbed neighbor")
names(bn.labs) = c(8, 4, 1)

bprop.plot = ggplot(plot.data) +
  geom_point(data = plot.data %>% filter(`burn-cost` == 0), mapping =
               aes(x = ticks, y = burnt, group = ticks, color = `burn-cost`), alpha = 0.1, size = 0.1) +
  geom_point(data = plot.data %>% filter(`burn-cost` == 100), mapping =
               aes(x = ticks, y = burnt, group = ticks, color = `burn-cost`), alpha = 0.1, size = 0.1) +
  geom_point(data = plot.data %>% filter(`burn-cost` == 200), mapping =
               aes(x = ticks, y = burnt, group = ticks, color = `burn-cost`), alpha = 0.1, size = 0.1) +
  geom_point(data = plot.data %>% filter(`burn-cost` == 300), mapping =
               aes(x = ticks, y = burnt, group = ticks, color = `burn-cost`), alpha = 0.1, size = 0.1) +
  geom_smooth(aes(x = ticks, y = burnt, color = `burn-cost`), se = F) +
  geom_smooth(aes(x = ticks, y = burnt), se = F, color = "black") +
  geom_hline(yintercept = 1.0, linetype = "dotted") +
  facet_grid(`burnt-neighbor-limit` ~ `burn-cost`, labeller = 
               labeller(`burnt-neighbor-limit` = bn.labs, 
                        `burn-cost` = bc.labs)) +
  labs(y = "proportion of patches that are disturbed") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 8), 
        strip.text = element_text(size = 6), axis.text = element_text(size = 8), axis.title = element_text(size = 8))
# ggsave(filename = "preliminary_figures/proportion-burnt-landscape.png", plot = bprop.plot, 
#        dpi = 300, width = 8, height = 5)


#### plot all burning behaviors ####
mbp.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = `burn-cost`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "mean probability of disturbance", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8))

br.plot2 = ggplot(data %>% filter(`cycle-duration` == 100) %>% filter(`veg-cycle-start` == "\"productive\"")) +
  geom_smooth(aes(x = ticks, y = bself.prop, group = exp, color = `burn-cost`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = bself.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "ticks", y = "proportion of disturbance benefits for self") +
  theme(legend.position = "none", axis.title = element_text(size = 8))

img = png::readPNG("preliminary_figures/ForageModv02_view.png")
img.in = grid::rasterGrob(img, interpolate = T)
img.plot = ggplot()+
  geom_blank()+
  annotation_custom(img.in, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_minimal()


all.plot = ggarrange(
  ggarrange(
    ggarrange(mbp.plot2, br.plot2, labels = "AUTO", 
              nrow = 1) + theme(legend.title = element_text(size = 7)), 
    img.plot, nrow = 1, labels = c("", "C"), widths = c(2,1)), 
  bprop.plot, nrow = 2, labels = c("", "D"), widths = c(1, 1.75)
)

ggsave(filename = "figures/disturbance-behaviors-plot.png", plot = all.plot,
       dpi = 300, width = 8, height = 7)
