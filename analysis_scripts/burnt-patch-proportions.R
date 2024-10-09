library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
library(ggpubr)
library(jtools)
theme_set(theme_bw())

#data = readRDS("results/bb-data.rds")
source("analysis_scripts/filter-out-unsuccessful-runs.R")

parameters = c(
  "natural-ignition", # 0.00, 0.05
  "cycle-duration",  # 100, 250
  "veg-cycle-start", # productive, unproductive
  "veg-distribution", # random, clustered
  "burnt-neighbor-limit", # 1, 4, 8
  "burn-cost", # 0, 50
  "burn-veg-type-threshold", # 4, 7
  "movement-model" #Random, Directed
)
data$`natural-ignition` = as.factor(data$`natural-ignition`)
data$`cycle-duration` = as.factor(data$`cycle-duration`)
data$`veg-cycle-start` = as.factor(data$`veg-cycle-start`)
data$`veg-distribution` = as.factor(data$`veg-distribution`)
data$`burnt-neighbor-limit` = as.factor(data$`burnt-neighbor-limit`)
data$`burn-cost` = as.factor(data$`burn-cost`)
data$`burn-veg-type-threshold` = as.factor(data$`burn-veg-type-threshold`)
data$`movement-model` = as.factor(data$`movement-model`)

data$adj.fi = data$mean.fi/data$pop.count
data$pop.dens = data$pop.count/(51*51)
data$benefit.ratio = data$benefit_self/data$benefit_other
data$burnt = data$`veg_-1`

outputs = c(
  "pop.count",
  "pop.dens", #pop count/world size
  "burnt", 
  "mean.burn.prob", 
  #"mean.fi", #average forager interaction count
  "adj.fi", #average interaction count/ population count
  "mean.fm",  #average forager movements per capita
  "veg.morans.i", 
  "veg.simpsons.div", 
  "benefit_self", #
  "benefit_other"
)

plotNormalHistogram(data$burnt)

ggplot(data) +
  geom_density(aes(x = burnt, color = `natural-ignition`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `cycle-duration`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `veg-cycle-start`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `veg-distribution`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `burnt-neighbor-limit`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `burn-cost`))
#decreasing burn cost shifts the distribution right

ggplot(data) +
  geom_density(aes(x = burnt, color = `burn-veg-type-threshold`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `movement-model`))

# ggplot(data) +
#   geom_smooth(aes(x = ticks, y = burnt, color = `burn-cost`), se = T)

plot.data = data %>% select_at(c("ticks", "burnt", parameters)) %>%
  filter(`burnt-neighbor-limit` %in% c(8,1))

bt.labs = c("can burn veg types 1-4", "can burn veg types 1-7")
names(bt.labs) = c(4, 7)
bc.labs = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")
names(bc.labs) = c(0, 100, 200, 300)
bn.labs = c("burn with 8 burnt neighbors", 
            "burn with 4 burnt neighbors", 
            "burn with 1 burnt neighbor")
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
  geom_smooth(aes(x = ticks, y = burnt), se = F) +
  geom_hline(yintercept = 1.0, linetype = "dotted") +
  facet_grid(`burnt-neighbor-limit` ~ `burn-cost`, labeller = 
               labeller(`burnt-neighbor-limit` = bn.labs, 
                        `burn-cost` = bc.labs)) +
  labs(y = "proportion of patches that are burnt") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 9), 
        strip.text = element_text(size = 7), axis.text = element_text(size = 8))
ggsave(filename = "preliminary_figures/proportion-burnt-landscape.png", plot = bprop.plot, 
       dpi = 300, width = 8, height = 5)


beta.data = data %>% mutate(burnt = ifelse(burnt == 0, burnt + 0.00001, burnt)) %>%
  mutate(burnt = ifelse(burnt == 1, burnt - 0.00001, burnt))

burnt.burnn1 = beta.data %>% filter(`burnt-neighbor-limit` == 1)
# plotNormalHistogram(veg_0.burnn1$veg_0)
# descdist(veg_0.burnn1$veg_0)
burnn1.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

burnt.burnn1 = data %>% filter(`burnt-neighbor-limit` == 1)
burnn1.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn1.fit1)
AIC(burnn1.fit2)

burnt.burnn4 = beta.data %>% filter(`burnt-neighbor-limit` == 4)
# plotNormalHistogram(veg_0.burnn4$veg_0)
# descdist(veg_0.burnn4$veg_0, discrete = F)
burnn4.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn4 = data %>% filter(`burnt-neighbor-limit` == 4)
burnn4.fit2 = lm(burnt ~ ticks*(.), data =  burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn4.fit1)
AIC(burnn4.fit2)

burnt.burnn8 = beta.data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn8 = data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn8.fit1)
AIC(burnn8.fit2)

plot_summs(burnn1.fit2, burnn4.fit2, burnn8.fit2, scale = T, digits = 6,
           model.names = c("BNL = 1", "BNL = 4", "BNL = 8"))


#### plot all burning behaviors ####
mbp.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = `burn-cost`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no burn cost (0)", "low burn cost (100)", "high burn cost (200)", "highest burn cost (300)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "mean probability of burning", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8))

br.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = benefit.ratio, group = exp, color = `burn-cost`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = benefit.ratio), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no burn cost (0)", "low burn cost (100)", "high burn cost (200)", "highest burn cost (300)")) +
  geom_hline(yintercept = 1, linetype = "dotted") +
  labs(x = "ticks", y = "ratio of self benefit to other benefit") +
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
  bprop.plot, nrow = 2, labels = c("", "D")
)

ggsave(filename = "figures/burning-behaviors-plot.png", plot = all.plot,
       dpi = 300, width = 8, height = 7)
