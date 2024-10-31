library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
library(segmented)
library(lme4)
library(lmerTest)
library(ggthemes)
library(mgcv)
library(ggpubr)
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
data$burnt = data$`veg_-1`
data$benefit.ratio = data$benefit_self/data$benefit_other


#### mobility analysis ####
hist(data$mean.fm)
descdist(data$mean.fm)

start.labs = c("start with unproductive vegetation", "start with productive vegetation")
names(start.labs) = c("\"unproductive\"", "\"productive\"")
cycle.labs = c("cycle every 250 ticks", "cycle every 100 ticks")
names(cycle.labs) = c(250, 100)
dist.labs = c("random environmental potential", "clustered environmental potential")
names(dist.labs) = c("\"random\"", "\"clustered\"")

fm.plot = ggplot(data %>% filter(ticks > 0)) +
  geom_point(aes(x = ticks, y = mean.fm, color = `movement-model`), alpha = 0.01, size = 0.1) +
  geom_smooth(aes(x = ticks, y = mean.fm, color = `movement-model`), method = "lm") +
  #geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = labeller(
    `veg-cycle-start` = start.labs, `cycle-duration` = cycle.labs
  )) +
  scale_color_colorblind(labels = c("Directed Walk", "Random Walk")) +
  theme(strip.text = element_text(size = 6), legend.title = element_blank(), legend.position = "bottom") +
  labs(y = "average steps per foraging bout")

ggsave(filename = "preliminary_figures/average-forager-movement.png", plot = fm.plot,
       dpi = 300, width = 6, height = 5)



# fit1 = lm(mean.fm ~ ticks , data = data)
# seg1 = segmented(fit1, seg.Z = ~ ticks, psi = NA)
# summary(seg1)

##split by cycle duration
#filter out ticks = 0
c100 = data %>% filter(`cycle-duration` == 100) %>% filter(ticks > 0)
c250 = data %>% filter(`cycle-duration` == 250) %>% filter(ticks > 0)

mm.fit1 = lmer(mean.fm ~ ticks  + (1 | `veg-cycle-start`), data = c100)
summary(mm.fit1)

mm.fit1.1 = lmer(mean.fm ~ ticks + (1 | `movement-model`)  + (1 | `veg-cycle-start`), data = c100)
anova(mm.fit1, mm.fit1.1)

mm.fit1.2 = lmer(mean.fm ~ ticks + `veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `natural-ignition` + (1 | `veg-cycle-start`), data = c100)

mm.fit1.3 = lmer(mean.fm ~ ticks*(`veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `natural-ignition`) + (1 | `veg-cycle-start`), data = c100)

mm.fit1.4 = lmer(mean.fm ~ ticks*(`veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `natural-ignition`) + (1 | `veg-cycle-start`) + (1 | `movement-model`), data = c100)
anova(mm.fit1.1, mm.fit1.2, mm.fit1.3, mm.fit1.4)
summary(mm.fit1.4)

ggeffects::ggpredict(mm.fit1.4, terms = c("ticks", "veg-distribution")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit1.4, terms = c("ticks", "burnt-neighbor-limit")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit1.4, terms = c("ticks", "burn-cost")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit1.4, terms = c("ticks", "burn-veg-type-threshold")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

mm.fit2.4 = lmer(mean.fm ~ ticks*(`veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `natural-ignition`) + (1 | `veg-cycle-start`) + (1 | `movement-model`), data = c250)
summary(mm.fit2.4)

ggeffects::ggpredict(mm.fit2.4, terms = c("ticks", "veg-distribution")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit2.4, terms = c("ticks", "burnt-neighbor-limit")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit2.4, terms = c("ticks", "burn-cost")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(mm.fit2.4, terms = c("ticks", "burn-veg-type-threshold")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

# c100 = c100 %>% rename(natural_ignition = `natural-ignition`, 
#                        cycle_duration = `cycle-duration`, 
#                        veg_cycle_start = `veg-cycle-start`,  
#                        veg_distribution = `veg-distribution`, 
#                        burnt_neighbor_limit = `burnt-neighbor-limit`, 
#                        burn_veg_type_threshold = `burn-veg-type-threshold`, 
#                        burn_cost = `burn-cost`, 
#                        movement_model = `movement-model`)
# 
# gamm.fit1 = gam(mean.fm ~ 
#                   s(ticks, by = natural_ignition) +
#                   s(ticks, by = veg_distribution) +
#                   s(ticks, by = burnt_neighbor_limit) +
#                   s(ticks, by = burn_cost) +
#                   s(ticks, by = burn_veg_type_threshold) +
#                   s(veg_cycle_start, bs = "re"), 
#                 data = c100)
# 
# gamm.fit1.1 = gam(mean.fm ~ 
#                     s(ticks, by = natural_ignition) +
#                     s(ticks, by = veg_distribution) +
#                     s(ticks, by = burnt_neighbor_limit) +
#                     s(ticks, by = burn_cost) +
#                     s(ticks, by = burn_veg_type_threshold) +
#                     s(veg_cycle_start, ticks, bs = "re"), 
#                   data = c100)
# 
# gamm.fit1.2 = gam(mean.fm ~ 
#                     s(ticks, by = natural_ignition) +
#                     s(ticks, by = veg_distribution) +
#                     s(ticks, by = burnt_neighbor_limit) +
#                     s(ticks, by = burn_cost) +
#                     s(ticks, by = burn_veg_type_threshold) +
#                     s(veg_cycle_start, ticks, bs = "re") +
#                     s(veg_cycle_start, bs = "re"), 
#                   data = c100)
# 
# gamm.fit1.3 = gam(mean.fm ~ 
#                     s(ticks, by = natural_ignition) +
#                     s(ticks, by = veg_distribution) +
#                     s(ticks, by = burnt_neighbor_limit) +
#                     s(ticks, by = burn_cost) +
#                     s(ticks, by = burn_veg_type_threshold) +
#                     s(veg_cycle_start, ticks, bs = "fs", m = 1), 
#                   data = c100)
# 
# anova(gamm.fit1, gamm.fit1.1, gamm.fit1.2, gamm.fit1.3, test = "Chisq")
# AIC(gamm.fit1, gamm.fit1.1, gamm.fit1.2, gamm.fit1.3)
# gam.check(gamm.fit1.3)
# summary(gamm.fit1.3)
# plot(gamm.fit1.3, scale = 0)

#### interaction analysis ####
summary(data$mean.fi)
cor(data$mean.fi, data$pop.count)

summary(data$adj.fi)
cor(data$adj.fi, data$pop.dens)

ggplot(data %>% filter(ticks > 0)) +
  geom_density(aes(x = log(adj.fi)))
summary(data$adj.fi)

fi.fit1 = lm(adj.fi ~ ticks*(.), data = data %>% dplyr::select_at(c(parameters, "ticks", "adj.fi")))
summary(fi.fit1)

fi.plot = ggplot(data %>% filter(ticks > 0)) +
  geom_point(aes(x = ticks, y = adj.fi, color = `movement-model`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = adj.fi, color = `movement-model`), se = T) +
  facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = labeller(
    `veg-cycle-start` = start.labs, `cycle-duration` = cycle.labs
  )) +
  scale_color_colorblind(labels = c("Directed Walk", "Random Walk")) +
  theme(strip.text = element_text(size = 6), legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 0.025)) +
  labs(y = "average proportion of population a forager interacts with")

ggsave(filename = "preliminary_figures/average-adjusted-interaction.png", plot = fi.plot,
       dpi = 300, width = 6, height = 5)

ggplot(data %>% filter(ticks > 0)) +
  geom_point(aes(x = ticks, y = adj.fi, color = `movement-model`), alpha = 0.01, size = 0.05) +
  geom_smooth(aes(x = ticks, y = adj.fi, color = `movement-model`), se = T, method = "lm") +
  facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = labeller(
    `veg-cycle-start` = start.labs, `cycle-duration` = cycle.labs
  )) +
  scale_color_colorblind(labels = c("Directed Walk", "Random Walk")) +
  theme(strip.text = element_text(size = 6), legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 0.025))

summary(fi.fit1)
ggeffects::ggpredict(fi.fit1, terms = c("ticks", "veg-cycle-start")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "veg-distribution")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "burnt-neighbor-limit")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "burn-veg-type-threshold")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "burn-cost")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "natural-ignition")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "movement-model")) %>%
  plot(show_data = FALSE, show_ci = FALSE)

ggeffects::ggpredict(fi.fit1, terms = c("ticks", "cycle-duration")) %>%
  plot(show_data = FALSE, show_ci = FALSE)


fi.fit1 = lm(adj.fi ~ ticks*(`veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `veg-cycle-start` + `movement-model` + `cycle-duration` + `natural-ignition`), data = data)
fi.fit2 = lmer(adj.fi ~ ticks*(`veg-distribution` + `burnt-neighbor-limit` + `burn-cost` + `burn-veg-type-threshold` + `veg-cycle-start` + `natural-ignition`) + (1 | `movement-model`) , data = data)
anova(fi.fit2, fi.fit1)

####plotting####
plot.data = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 100) %>% filter(`veg-cycle-start` == "\"productive\"")
plot.data$`burn-cost` =  as.factor(plot.data$`burn-cost`)

move.labs = c("Directed Walk", "Random Walk")
names(move.labs) = c("\"Directed Walk\"", "\"Random Walk\"")

##### final figure #####
all.plot = ggarrange(
  ggplot(plot.data) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 0), mapping =
                  aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 100), mapping =
                  aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 200), mapping =
                  aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 300), mapping =
                  aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_smooth(mapping = aes(x = ticks, y = mean.fm, color = `burn-cost`), method = "lm") +
    facet_grid(`cycle-duration` ~ `movement-model`, 
               labeller = labeller(`movement-model` = move.labs, `cycle-duration` = cycle.labs)) +
    scale_color_brewer(palette = "Set2",
                       labels = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")) +
    theme(strip.text = element_text(size = 6), 
          legend.title = element_blank(), 
          legend.position = "bottom", 
          axis.title = element_text(size = 7.5)) +
    scale_y_continuous(limits = c(8.000, 10.500), labels = scales::number_format(accuracy = 0.001)) + 
    labs(y = "average steps per foraging bout"),
  ggplot(plot.data) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 0), mapping =
                 aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 100), mapping =
                 aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 200), mapping =
                 aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_point(data = plot.data %>% filter(`burn-cost` == 300), mapping =
                 aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
    geom_smooth(mapping = aes(x = ticks, y = adj.fi, color = `burn-cost`)) +
    facet_grid(`cycle-duration` ~ `movement-model`, 
               labeller = labeller(`movement-model` = move.labs, `cycle-duration` = cycle.labs)) +
    scale_color_brewer(palette = "Set2",
                       labels = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")) +
    theme(strip.text = element_text(size = 6), 
          legend.title = element_blank(), 
          legend.position = "bottom", 
          axis.title = element_text(size = 7.5)) +
    scale_y_continuous(limits = c(0, 0.020)) +
    labs(y = "interaction proportion per bout"), 
  ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO"
)

#####smoothed step plots#####
burn.labs = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")
names(burn.labs) = c(0, 100, 200, 300)

plot.data$climate.condition = factor(plot.data$climate.condition, levels = c("productive", "unproductive"))

ggplot(plot.data) +
  geom_smooth(data = plot.data, 
              mapping = aes(x = ticks, y = mean.fm), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 0 & ticks <= 100), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 100 & ticks <= 200), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 200 & ticks <= 300), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 300 & ticks <= 400), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 400 & ticks <= 500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 500 & ticks <= 600), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 600 & ticks <= 700), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 700 & ticks <= 800), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 800 & ticks <= 900), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 900 & ticks <= 1000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1000 & ticks <= 1100), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1100 & ticks <= 1200), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1200 & ticks <= 1300), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1300 & ticks <= 1400), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1400 & ticks <= 1500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1500 & ticks <= 1600), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1600 & ticks <= 1700), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1700 & ticks <= 1800), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1800 & ticks <= 1900), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data %>% filter(ticks > 1900 & ticks <= 2000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_colorblind(labels = c("productive", "unproductive")) +
  theme(strip.text = element_text(size = 6), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "average forager movements")


plot.data2 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`movement-model` == "\"Directed Walk\"")
plot.data$`burn-cost` =  as.factor(plot.data$`burn-cost`)

mobility.plot = ggplot(plot.data2) +
  geom_smooth(data = plot.data2, 
              mapping = aes(x = ticks, y = mean.fm, color = `burn-cost`), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 0 & ticks <= 250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 250 & ticks <= 500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 500 & ticks <= 750), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 750 & ticks <= 1000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 1000 & ticks <= 1250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 1250 & ticks <= 1500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 1500 & ticks <= 1750), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 1750 & ticks <= 2000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_brewer(palette = "Set2", 
                     labels = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)", 
                                "productive", "unproductive")) +
  theme(strip.text = element_text(size = 6), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "average forager movements")

plot.data3 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"productive\"")
plot.data3$`burn-cost` =  as.factor(plot.data3$`burn-cost`)

interaction.plot = ggplot(plot.data3) +
  geom_smooth(data = plot.data3, 
              mapping = aes(x = ticks, y = adj.fi, color = `burn-cost`), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 0 & ticks <= 250), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 250 & ticks <= 500), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 500 & ticks <= 750), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 750 & ticks <= 1000), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 1000 & ticks <= 1250), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 1250 & ticks <= 1500), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 1500 & ticks <= 1750), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 1750 & ticks <= 2000), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_brewer(palette = "Set2", 
                     labels = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)", 
                                "productive", "unproductive")) +
  theme(strip.text = element_text(size = 6), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "average interaction proportion")

all.plot = ggarrange(
  mobility.plot, interaction.plot, 
  ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO"
)
ggsave(filename = "figures/average-mobility+adjusted-interaction.png", plot = all.plot,
       dpi = 300, width = 8, height = 5)
