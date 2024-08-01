library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(jtools)
library(mgcv)
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

data$veg.simpsons.div = 1 - data$veg.simpsons.div #fixing Simpson's D calculation from model

hist(data$veg.morans.i)
hist(data$veg.simpsons.div)


clean.data = data %>%
  select_at(c("ticks", "veg.morans.i", "veg.simpsons.div", parameters, "climate.condition")) %>%
  rename(natural_ignition = `natural-ignition`, 
         cycle_duration = `cycle-duration`, 
         veg_cycle_start = `veg-cycle-start`, 
         veg_distribution = `veg-distribution`, 
         burnt_neighbor_limit = `burnt-neighbor-limit`, 
         burn_veg_type_threshold = `burn-veg-type-threshold`, 
         burn_cost = `burn-cost`, 
         movement_model = `movement-model`)

#### SPATIAL HETEROGENEITY ####

mi.fit1 = lm(veg.morans.i ~ ticks*(.), data = clean.data[,-c(3, 12)])
mi.fit2 = lm(veg.morans.i ~ ., data = clean.data[,-c(1,3, 12)])
mi.fit3 = lm(veg.morans.i ~ ticks*(.), data = clean.data[,-3])
anova(mi.fit2, mi.fit1, mi.fit3)
summary(mi.fit1)
plot_summs(mi.fit1, mi.fit3, scale = T)

mi.plot = ggplot(data) +
  geom_point(aes(x = ticks, y = veg.morans.i, color = `movement-model`), alpha = 0.01, size = 1) +
  geom_smooth(aes(x = ticks, y = veg.morans.i, color = `movement-model`)) +
  geom_hline(yintercept = 0) +
  labs(y = "Moran's I") +
  scale_color_brewer(palette = "Dark2", labels = c("Directed Walk", "Random Walk")) +
  theme(legend.title = element_blank(), legend.position = "bottom") 
plot(mi.plot)

mi.plot2 = mi.plot +
  facet_grid(`cycle-duration` + `veg-cycle-start` ~ climate.condition)
plot(mi.plot2)

ggsave(filename = "preliminary_figures/morans-i.png", plot = mi.plot,
       dpi = 300, width = 6, height = 4)


#### VEGETATION DIVERSITY ####
sd.plot = ggplot(data) +
  geom_point(aes(x = ticks, y = veg.simpsons.div, color = `movement-model`), alpha = 0.01, size = 1) +
  geom_smooth(aes(x = ticks, y = veg.simpsons.div, color = `movement-model`)) +
  labs(y = "Simpson's Diversity Index") +
  scale_color_brewer(palette = "Dark2", labels = c("Directed Walk", "Random Walk")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

start.labs = c("start with unproductive vegetation", "start with productive vegetation")
names(start.labs) = c("\"unproductive\"", "\"productive\"")
cycle.labs = c("cycle every 250 ticks", "cycle every 100 ticks")
names(cycle.labs) = c(250, 100)
sd.plot2 = sd.plot +
  facet_grid(`cycle-duration`~`veg-cycle-start`, 
             labeller = labeller(`cycle-duration` = cycle.labs,`veg-cycle-start` = start.labs)) +
  theme(legend.title = element_blank(), legend.position = "bottom", strip.text = element_text(size = 7))
#plot(sd.plot2)

ggsave(filename = "preliminary_figures/simpsons-diversity.png", plot = sd.plot2,
       dpi = 300, width = 6, height = 4)

ggplot(data) +
  geom_density(aes(x = veg.simpsons.div, color = `movement-model`))

##### random walk data ####
rw.data = data %>% filter(`movement-model` == "\"Random Walk\"")

ggplot(rw.data) +
  geom_point(aes(x = ticks, y = veg.simpsons.div, color = `burn-veg-type-threshold`), alpha = 0.01, size = 1) +
  geom_smooth(aes(x = ticks, y = veg.simpsons.div, color = `burn-veg-type-threshold`)) +
  labs(y = "Simpson's Diversity Index") +
  facet_grid(`burnt-neighbor-limit` ~`veg-cycle-start` + `cycle-duration`)

rw.gam1 = gam(veg.simpsons.div ~ s(ticks), data = rw.data)
#plot(rw.gam1)

rw.fit1 = glm(veg.simpsons.div ~ ticks*(.), data = rw.data %>% select_at(c(parameters, "ticks", "veg.simpsons.div")) %>% dplyr::select(-`movement-model`), family = "Gamma")

rw.gam2 = gam(veg.simpsons.div ~ 
                s(ticks, by = cycle_duration) + 
                s(ticks, by = veg_cycle_start) +
                s(ticks, by = veg_distribution),
              data = clean.data %>% filter(movement_model == "\"Random Walk\""))

rw.gam3 = gam(veg.simpsons.div ~  s(ticks, by = natural_ignition) +
                s(ticks, by = cycle_duration) +
                s(ticks, by = veg_cycle_start) +
                s(ticks, by = veg_distribution) +
                s(ticks, by = burn_cost)  + 
                s(ticks, by = burn_veg_type_threshold) +
                s(ticks, by = as.factor(burnt_neighbor_limit)),
              data = clean.data %>% filter(movement_model == "\"Random Walk\""))
anova(rw.gam1, rw.gam2, rw.gam3, test = "Chisq")
AIC(rw.fit1)
AIC(rw.gam3)

par(mar=c(1,1,1,1))
plot(rw.gam3, pages = 1)
summary(rw.gam3)

###### split by cycle duration length #####
rw.c100 = clean.data %>% filter(movement_model == "\"Random Walk\"") %>% filter(cycle_duration == 100)
rw.c250 = clean.data %>% filter(movement_model == "\"Random Walk\"") %>% filter(cycle_duration == 250)

rw.gam4 = gam(veg.simpsons.div ~  s(ticks, by = natural_ignition) +
                s(ticks, by = veg_cycle_start) +
                s(ticks, by = veg_distribution) +
                s(ticks, by = burn_cost)  + 
                s(ticks, by = burn_veg_type_threshold) +
                s(ticks, by = as.factor(burnt_neighbor_limit)),
              data = rw.c100)
summary(rw.gam4)

par(mfrow=c(1,3), cex=1.1)
plot(rw.gam4, select=11, shade=T)
abline(h=0, lty = 2)
plot(rw.gam4, select=12, shade=T)
abline(h=0, lty = 2)
plot(rw.gam4, select=13, shade=T)
abline(h=0, lty = 2)


rw.gam5 = gam(veg.simpsons.div ~  s(ticks, by = natural_ignition) +
                s(ticks, by = veg_cycle_start) +
                s(ticks, by = veg_distribution) +
                s(ticks, by = burn_cost)  + 
                s(ticks, by = burn_veg_type_threshold) +
                s(ticks, by = as.factor(burnt_neighbor_limit)),
              data = rw.c250)
summary(rw.gam5)


##### directed walk data #####
dw.data = data %>% filter(`movement-model` == "\"Directed Walk\"")

# ggplot(dw.data) +
#   geom_point(aes(x = ticks, y = veg.simpsons.div, color = `burn-veg-type-threshold`), alpha = 0.01, size = 1) +
#   geom_smooth(aes(x = ticks, y = veg.simpsons.div, color = `burn-veg-type-threshold`)) +
#   labs(y = "Simpson's Diversity Index") +
#   facet_grid(`burnt-neighbor-limit` ~`veg-cycle-start` + `cycle-duration`)

#hist(rw.data$veg.simpsons.div)
#descdist((rw.data %>% filter(!is.na(veg.simpsons.div)))$veg.simpsons.div)
#hist(dw.data$veg.simpsons.div)
#descdist((dw.data %>% filter(!is.na(veg.simpsons.div)))$veg.simpsons.div

dw.gam1 = gam(veg.simpsons.div ~  s(ticks, by = natural_ignition) +
                s(ticks, by = cycle_duration) +
                s(ticks, by = veg_cycle_start) +
                s(ticks, by = veg_distribution) +
                s(ticks, by = burn_cost)  + 
                s(ticks, by = burn_veg_type_threshold) +
                s(ticks, by = as.factor(burnt_neighbor_limit)),
              data = clean.data %>% filter(movement_model == "\"Directed Walk\""))
summary(dw.gam1)
plot(dw.gam1)




#### visualizing vegetation proportions ####
veg.props = data %>% dplyr::select_at(c("ticks", parameters, "climate.condition", "veg_0", "veg_1", "veg_2", "veg_3", "veg_4", "veg_5", "veg_6", "veg_7")) %>%
  pivot_longer(cols = c("veg_0", "veg_1", "veg_2", "veg_3", "veg_4", "veg_5", "veg_6", "veg_7"), 
               names_to = "veg_type", values_to = "prop")

# ggplot(veg.props) +
#   geom_point(aes(x = ticks, y = prop, color = veg_type), alpha = 0.1, size = 0.1) +
#   facet_grid(`movement-model` ~ `veg-cycle-start` + `cycle-duration`)

ggplot(veg.props) +
  geom_smooth(aes(x = ticks, y = prop, color = veg_type, group = veg_type)) +
  facet_grid(`movement-model` ~ `veg-cycle-start` + `cycle-duration`)



