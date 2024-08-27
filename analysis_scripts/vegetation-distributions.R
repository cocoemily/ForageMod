library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(ggpubr)
library(vegan)
library(jtools)
theme_set(theme_bw())

parameters = c(
  "natural_ignition", # 0.00, 0.05
  "cycle_duration",  # 100, 250
  "veg_cycle_start", # productive, unproductive
  "veg_distribution", # random, clustered
  "burnt_neighbor_limit", # 1, 4, 8
  "burn_cost", # 0, 50
  "burn_veg_type_threshold", # 4, 7
  "movement_model" #Random, Directed
)

veg.100.prod = readRDS("results/outputs/veg_moransi_100-prod.rds")
veg.100.unprod = readRDS("results/outputs/veg_moransi_100-unprod.rds")
veg.250.prod = readRDS("results/outputs/veg_moransi_250-prod.rds")
veg.250.unprod = readRDS("results/outputs/veg_moransi_250-unprod.rds")
#veg.data = bind_rows(veg.100.prod, veg.100.unprod, veg.250.prod, veg.250.unprod)

#filtering out ticks used for correlations
veg.100.prod = veg.100.prod %>% filter(ticks != 1000) %>% filter(ticks != 2000)
veg.100.unprod = veg.100.unprod %>% filter(ticks != 1000) %>% filter(ticks != 2000)
veg.250.prod = veg.250.prod %>% filter(ticks != 1000) %>% filter(ticks != 2000)
veg.250.unprod = veg.250.unprod %>% filter(ticks != 1000) %>% filter(ticks != 2000)

reg.params = parameters[-c(2:3)]

#### cycle duration = 100 & productive start ####
##### morans i #####
hist(veg.100.prod$morans.i)
# veg.100.prod[,reg.params] = lapply(veg.100.prod[,reg.params] , as.factor)
# 
# veg.100.prod.fit1 = lm(morans.i ~ ticks*(.), data = veg.100.prod %>% filter(signif == T) %>% select_at(c("ticks", "morans.i", reg.params)))
# plot_summs(veg.100.prod.fit1, scale = T)

prod100.plot = ggplot(veg.100.prod %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_wrap(~ movement_model) +
  labs(color = "model climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

##### shannon diversity #####
hist(veg.100.prod$shannon.div)
descdist(veg.100.prod$shannon.div)

ggplot(veg.100.prod) +
  geom_density(aes(x = shannon.div, color = movement_model))

ggplot(veg.100.prod) +
  geom_boxplot(mapping = aes(x = ticks, y = 1 - shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = 1 - shannon.div)) +
  facet_wrap(~movement_model)


####cycle duration = 100 & unproductive start ####
hist(veg.100.unprod$morans.i)
# veg.100.unprod[,reg.params] = lapply(veg.100.unprod[,reg.params] , as.factor)
# 
# veg.100.unprod.fit1 = lm(morans.i ~ ticks*(.), data = veg.100.unprod %>% filter(signif == T) %>% select_at(c("ticks", "morans.i", reg.params)))
# plot_summs(veg.100.unprod.fit1, scale = T)

unprod100.plot = ggplot(veg.100.unprod %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_wrap(~ movement_model) +
  labs(color = "model climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")

##### shannon diversity #####
hist(veg.100.unprod$shannon.div)
descdist(veg.100.unprod$shannon.div)

ggplot(veg.100.unprod) +
  geom_density(aes(x = shannon.div, color = movement_model))

ggplot(veg.100.unprod) +
  geom_boxplot(mapping = aes(x = ticks, y = 1 - shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = 1 - shannon.div)) +
  facet_wrap(~movement_model)

####cycle duration = 250 & productive start ####
hist(veg.250.prod$morans.i)

hist(veg.250.prod$shannon.div)
ggplot(veg.250.prod) +
  geom_density(aes(x = shannon.div, color = movement_model))

####cycle duration = 250 & unproductive start ####
hist(veg.250.unprod$morans.i)

hist(veg.250.unprod$shannon.div)
ggplot(veg.250.unprod) +
  geom_density(aes(x = shannon.div, color = movement_model))


####vegetation spatial autocorrelation####
veg.data = bind_rows(veg.100.prod, veg.100.unprod, veg.250.prod, veg.250.unprod) 

#create labels
start.labs = c("start with unproductive vegetation", "start with productive vegetation")
names(start.labs) = c("\"unproductive\"", "\"productive\"")
cycle.labs = c("cycle every 250 ticks", "cycle every 100 ticks")
names(cycle.labs) = c(250, 100)
move.labs = c("random walks", "directed walks")
names(move.labs) = c("\"Random Walk\"", "\"Directed Walk\"")

veg.cluster.plot = ggplot(veg.data %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_grid(movement_model ~ cycle_duration + veg_cycle_start , labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", strip.text = element_text(size = 6.5),)

ggsave(filename = "preliminary_figures/veg-type-clustering.png", plot = veg.cluster.plot,
       dpi = 300, width = 8, height = 5.5)

#### vegetation diversity####
veg.div.plot = ggplot(veg.data %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_grid(movement_model ~ cycle_duration + veg_cycle_start , labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "climate condition", y = "Shannon Diversity Index") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", strip.text = element_text(size = 6.5),)
#plot(veg.div.plot)
ggsave(filename = "preliminary_figures/veg-type-diversity.png", plot = veg.div.plot,
       dpi = 300, width = 8, height = 5.5)


##all data together
# long.veg = veg.data %>% pivot_longer(c(morans.i, shannon.div), names_to = "metric", values_to = "value")
# metric.labs = c("Global Moran's I", "Shannon Diversity Index")
# names(metric.labs) = c("morans.i", "shannon.div")
# 
# all.plot = ggplot(long.veg %>% filter(signif == T) %>% filter(cycle_duration == 100) %>% filter(veg_cycle_start == "\"productive\"")) +
#   geom_boxplot(mapping = aes(x = ticks, y = value, color = climate.condition, group = ticks)) +
#   geom_smooth(mapping = aes(x = ticks, y = value), method = "gam", color = "black") +
#   facet_grid(metric ~ movement_model, labeller = 
#                labeller(movement_model = move.labs, metric = metric.labs), scales = "free") +
#   labs(color = "climate condition") +
#   scale_color_brewer(palette = "Dark2") +
#   theme(legend.position = "bottom")

all.plot =  ggarrange(
  ggplot(veg.data %>% filter(signif == T) %>% filter(cycle_duration == 100) %>% filter(veg_cycle_start == "\"productive\"")) +
    geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
    geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
    facet_grid(cycle_duration + veg_cycle_start ~ movement_model, labeller = 
                 labeller(cycle_duration = cycle.labs, 
                          veg_cycle_start = start.labs, 
                          movement_model = move.labs)) +
    labs(color = "climate condition", y = "Global Moran's I") +
    scale_color_brewer(palette = "Dark2") +
    theme(legend.position = "bottom", 
          strip.text = element_text(size = 6), 
          axis.title = element_text(size = 7.5)), 
  ggplot(veg.data %>% filter(signif == T) %>% filter(cycle_duration == 100) %>% filter(veg_cycle_start == "\"productive\""))  +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_grid(cycle_duration + veg_cycle_start ~ movement_model, labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "climate condition", y = "Shannon Diversity Index") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 6), 
        axis.title = element_text(size = 7.5)), 
  ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO"
) +
  theme(axis.title = element_text(size = 7))

ggsave(filename = "figures/veg-type-clustering+diversity.png", plot = all.plot,
       dpi = 300, width = 8, height = 5)
