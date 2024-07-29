library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
theme_set(theme_bw())

data = readRDS("results/bb-data.rds")

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

outputs = c(
  "pop.count",
  "pop.dens", #pop count/world size
  "veg_0", 
  "mean.burn.prob", 
  #"mean.fi", #average forager interaction count
  "adj.fi", #average interaction count/ population count
  "mean.fm",  #average forager movements per capita
  "veg.morans.i", 
  "veg.simpsons.div", 
  "benefit_self", #
  "benefit_other"
)

hist(data$veg.morans.i)
hist(data$veg.simpsons.div)

clean.data = data %>%
  select_at(c("ticks", "veg.morans.i", "veg.simpsons.div", parameters)) %>%
  rename(natural_ignition = `natural-ignition`, 
         cycle_duration = `cycle-duration`, 
         veg_cycle_start = `veg-cycle-start`, 
         veg_distribution = `veg-distribution`, 
         burnt_neighbor_limit = `burnt-neighbor-limit`, 
         burn_veg_type_threshold = `burn-veg-type-threshold`, 
         burn_cost = `burn-cost`, 
         movement_model = `movement-model`)
  

mi.fit1 = lm(veg.morans.i ~ ticks*(.), data = clean.data[,-3])
summary(mi.fit1)

mi.fit2 = lm(veg.morans.i ~ ., data = clean.data[,-c(1,3)])
TukeyHSD(aov(mi.fit2))


mi.plot = ggplot(data) +
  geom_point(aes(x = ticks, y = veg.morans.i, color = `movement-model`), alpha = 0.01, size = 1) +
  geom_smooth(aes(x = ticks, y = veg.morans.i, color = `movement-model`)) +
  geom_hline(yintercept = 0) +
  labs(y = "Moran's I") +
  scale_color_brewer(palette = "Dark2", labels = c("Directed Walk", "Random Walk")) +
  theme(legend.title = element_blank(), legend.position = "bottom")
ggsave(filename = "preliminary_figures/morans-i.png", plot = mi.plot,
       dpi = 300, width = 6, height = 4)


sd.fit1 = lm(veg.simpsons.div ~ ., data = clean.data[,-c(1,2)])
TukeyHSD(aov(sd.fit1))

sd.plot = ggplot(data) +
  geom_point(aes(x = ticks, y = veg.simpsons.div, color = `movement-model`), alpha = 0.01, size = 1) +
  geom_smooth(aes(x = ticks, y = veg.simpsons.div, color = `movement-model`)) +
  labs(y = "Simpson's Diversity Index") +
  scale_color_brewer(palette = "Dark2", labels = c("Directed Walk", "Random Walk")) +
  theme(legend.title = element_blank(), legend.position = "bottom")
#plot(sd.plot)
ggsave(filename = "preliminary_figures/simpsons-diversity.png", plot = sd.plot,
       dpi = 300, width = 6, height = 4)

# ggsave(filename = "preliminary_figures/vegetation-distributions.png",
#   plot = ggpubr::ggarrange(plotlist = c(mi.plot, sd.plot), common.legend = T, legend = "bottom"), 
#   dpi = 300, width = 9, height = 6
# )
