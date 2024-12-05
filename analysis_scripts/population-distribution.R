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
  "cycle_duration",  # 100, 250
  "veg_cycle_start", # productive, unproductive
  "veg_distribution", # random, clustered
  "burnt_neighbor_limit", # 1, 4, 8
  "burn_cost", # 0, 50
  "burn_veg_type_threshold", # 4, 7
  "movement_model" #Random, Directed
)

pop.100 = readRDS("results/outputs/tl_morans.i_cycle.100.rds")
pop.250 = readRDS("results/outputs/tl_morans.i_cycle.250.rds")

plot100 = ggplot(pop.100 %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, group = ticks)) +
  facet_wrap(~ movement_model) +
  labs(y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
plot(plot100)

plot100.rw = ggplot(pop.100 %>% filter(signif == T) %>% filter(movement_model == "\"Random Walk\"")) +
  geom_line(mapping = aes(x = ticks, y = morans.i, group = exp, color = exp)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i)) +
  facet_grid(burn_cost ~ burnt_neighbor_limit, labeller = label_both) +
  labs(y = "Global Moran's I") +
  theme(legend.position = "none")
plot(plot100.rw)

plot100.dw = ggplot(pop.100 %>% filter(signif == T) %>% filter(movement_model == "\"Directed Walk\"")) +
  geom_line(mapping = aes(x = ticks, y = morans.i, group = exp, color = exp)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i)) +
  facet_grid(burn_cost ~ burnt_neighbor_limit, labeller = label_both) +
  labs(y = "Global Moran's I") +
  theme(legend.position = "none")
plot(plot100.dw)

plot250 = ggplot(pop.250 %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, group = ticks)) +
  facet_wrap(~ movement_model) +
  labs(color = "model climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
plot(plot250)

plot250.rw = ggplot(pop.250 %>% filter(signif == T) %>% filter(movement_model == "\"Random Walk\"")) +
  geom_line(mapping = aes(x = ticks, y = morans.i, group = exp, color = exp)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i)) +
  facet_grid(burn_cost ~ burnt_neighbor_limit, labeller = label_both) +
  labs(y = "Global Moran's I") +
  theme(legend.position = "none")
plot(plot250.rw)

plot250.dw = ggplot(pop.250 %>% filter(signif == T) %>% filter(movement_model == "\"Directed Walk\"")) +
  geom_line(mapping = aes(x = ticks, y = morans.i, group = exp, color = exp)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i)) +
  facet_grid(burn_cost ~ burnt_neighbor_limit, labeller = label_both) +
  labs(y = "Global Moran's I") +
  theme(legend.position = "none")
plot(plot100.dw)


