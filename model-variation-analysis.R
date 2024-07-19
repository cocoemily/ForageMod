library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(lme4)

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

outputs = c(
  "pop.count", 
  "veg_0", 
  "mean.burn.prob", 
  #"mean.fi", #average forager interaction count
  "adj.fi", #average interaction count/ population count
  "mean.fm",  #average forager movements per capita
  "veg.morans.i", 
  "veg.simpsons.div"
)

#### Variation between model runs ####
var.data = data %>% group_by_at(c("ticks", parameters)) %>%
  summarize(pop.count.cv = sd(pop.count)/mean(pop.count), 
            veg_0.cv = sd(veg_0)/mean(veg_0), 
            mean.burn.prob.cv = sd(mean.burn.prob)/mean(mean.burn.prob), 
            adj.fi.cv = sd(adj.fi)/mean(adj.fi), 
            mean.fm.cv = sd(mean.fm)/mean(mean.fm), 
            veg.simpsons.cv = sd(veg.simpsons.div)/mean(veg.simpsons.div), 
            veg.morans.cv = sd(veg.morans.i)/mean(veg.morans.i)) %>%
  group_by_at(c(parameters)) %>%
  mutate(group_id = cur_group_id()) %>%
  pivot_longer(cols = c("pop.count.cv", "veg_0.cv", "mean.burn.prob.cv", 
                        "adj.fi.cv", "mean.fm.cv", "veg.simpsons.cv", "veg.morans.cv"), 
               names_to = "CV", values_to = "value")

ggplot(var.data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, y = value, group = as.factor(group_id), color = as.factor(group_id))) +
  facet_wrap(~CV, scales = "free") +
  guides(color = "none")

ggplot(var.data %>% filter(ticks > 0) %>% filter(group_id == 1)) +
  geom_point(aes(x = ticks, y = value))  +
  facet_wrap(~CV, scales = "free")

var.data.wide = data %>% group_by_at(c("ticks", parameters)) %>%
  summarize(pop.count.cv = sd(pop.count)/mean(pop.count), 
            veg_0.cv = sd(veg_0)/mean(veg_0), 
            mean.burn.prob.cv = sd(mean.burn.prob)/mean(mean.burn.prob), 
            adj.fi.cv = sd(adj.fi)/mean(adj.fi), 
            mean.fm.cv = sd(mean.fm)/mean(mean.fm), 
            veg.simpsons.cv = sd(veg.simpsons.div)/mean(veg.simpsons.div), 
            veg.morans.cv = sd(veg.morans.i)/mean(veg.morans.i)) %>%
  group_by_at(c(parameters)) %>%
  mutate(group_id = cur_group_id())

summary(var.data.wide$pop.count.cv)
summary(var.data.wide$veg_0.cv)
summary(var.data.wide$mean.burn.prob.cv)
summary(var.data.wide$adj.fi.cv)
summary(var.data.wide$mean.fm.cv)
summary(var.data.wide$veg.simpsons.cv)
summary(var.data.wide$veg.morans.cv)

bp.lmer = lmer(mean.burn.prob.cv ~ . + (1 | ticks), data = var.data.wide %>% select_at(c(parameters, "ticks", "mean.burn.prob.cv")))
summary(bp.lmer)
