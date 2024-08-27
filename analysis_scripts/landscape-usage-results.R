library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(ggpubr)

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

be.morans.100 <- readRDS("results/outputs/be_morans.i_cycle.100.rds")
be.morans.100[,parameters[-2]] = lapply(be.morans.100[,parameters[-2]] , as.factor)
be.morans.250 <- readRDS("results/outputs/be_morans.i_cycle.250.rds")
be.morans.250[,parameters[-2]] = lapply(be.morans.250[,parameters[-2]] , as.factor)

#### Global Moran's I, cycle duration = 100 ####
ggplot(be.morans.100 %>% filter(signif == T)) +
  geom_point(mapping = aes(x = ticks, y = morans.i, shape = signif)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i, color = as.factor(burnt_neighbor_limit), group = as.factor(burnt_neighbor_limit)), method = "lm") +
  facet_grid(burn_veg_type_threshold~movement_model + veg_distribution, scales = "free")

hist(be.morans.100$morans.i)
fit1 = lm(morans.i ~ ticks*(.), data = be.morans.100 %>% select_at(c("ticks", parameters, "morans.i")))
summary(fit1)

fit.log = glm(signif ~ ticks*(.), data = be.morans.100 %>% select_at(c("ticks", parameters, "signif")), family = binomial())
summary(fit.log)

#### Global Moran's I, cycle duration = 250 ####
ggplot(all.morans.250 %>% filter(signif == T)) +
  geom_point(mapping = aes(x = ticks, y = morans.i, shape = signif)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i, color = as.factor(`burnt-neighbor-limit`), group = as.factor(`burnt-neighbor-limit`)), method = "lm") +
  facet_grid(`burn-veg-type-threshold`~`movement-model` + `veg-distribution`, scales = "free")
