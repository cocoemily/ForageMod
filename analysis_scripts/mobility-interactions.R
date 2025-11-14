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
library(jtools)
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

data$same.fi.prop = data$mean.same.fi / data$mean.fi

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


#### interaction analysis ####
summary(data$mean.fi)
cor(data$mean.fi, data$pop.count)

summary(data$adj.fi)
cor(data$adj.fi, data$pop.dens)

# ggplot(data %>% filter(ticks > 0)) +
#   geom_density(aes(x = log(adj.fi)))
# summary(data$adj.fi)

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

####plotting####
# plot.data = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 100) %>% filter(`veg-cycle-start` == "\"productive\"")
# plot.data$`burn-cost` =  as.factor(plot.data$`burn-cost`)

move.labs = c("directed walks", "random walks")
names(move.labs) = c("\"Directed Walk\"", "\"Random Walk\"")

##### figure with points #####
# all.plot = ggarrange(
#   ggplot(plot.data) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 0), mapping =
#                   aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 100), mapping =
#                   aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 200), mapping =
#                   aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 300), mapping =
#                   aes(x = ticks, y = mean.fm, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_smooth(mapping = aes(x = ticks, y = mean.fm, color = `burn-cost`), method = "lm") +
#     facet_grid(`cycle-duration` ~ `movement-model`, 
#                labeller = labeller(`movement-model` = move.labs, `cycle-duration` = cycle.labs)) +
#     scale_color_brewer(palette = "Set2",
#                        labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")) +
#     theme(strip.text = element_text(size = 6), 
#           legend.title = element_blank(), 
#           legend.position = "bottom", 
#           axis.title = element_text(size = 7.5)) +
#     scale_y_continuous(limits = c(8.000, 10.500), labels = scales::number_format(accuracy = 0.001)) + 
#     labs(y = "average steps per foraging bout"),
#   ggplot(plot.data) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 0), mapping =
#                  aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 100), mapping =
#                  aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 200), mapping =
#                  aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_point(data = plot.data %>% filter(`burn-cost` == 300), mapping =
#                  aes(x = ticks, y = adj.fi, group = ticks, color = `burn-cost`), alpha = 0.01, size = 0.05) +
#     geom_smooth(mapping = aes(x = ticks, y = adj.fi, color = `burn-cost`)) +
#     facet_grid(`cycle-duration` ~ `movement-model`, 
#                labeller = labeller(`movement-model` = move.labs, `cycle-duration` = cycle.labs)) +
#     scale_color_brewer(palette = "Set2",
#                        labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)")) +
#     theme(strip.text = element_text(size = 6), 
#           legend.title = element_blank(), 
#           legend.position = "bottom", 
#           axis.title = element_text(size = 7.5)) +
#     scale_y_continuous(limits = c(0, 0.020)) +
#     labs(y = "interaction proportion per bout"), 
#   ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO"
# )

####FINAL -- smoothed step plots#####
burn.labs = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")
names(burn.labs) = c(0, 100, 200, 300)

# plot.data$climate.condition = factor(plot.data$climate.condition, levels = c("productive", "unproductive"))
# plot.data$`burn-cost` =  as.factor(plot.data$`burn-cost`)

period.colors = RColorBrewer::brewer.pal(6, "Set2")[5:6]

plot.data2 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"productive\"")
mobility.plot = ggplot(plot.data2) +
  geom_smooth(data = plot.data2, 
              mapping = aes(x = ticks, y = mean.fm), method = "lm", color = "grey20") +
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
  geom_smooth(data = plot.data2 %>% filter(ticks > 2000 & ticks <= 2250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2250 & ticks <= 2500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2500 & ticks <= 2750), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2750 & ticks <= 3000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 3000 & ticks <= 3250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 3250 & ticks <= 3500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_manual(values = period.colors, 
                     labels = c("productive period", "unproductive period")) +
  theme(strip.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "average forager movements")


plot.data2 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"unproductive\"")
mobility.plot2 = ggplot(plot.data2) +
  geom_smooth(data = plot.data2, 
              mapping = aes(x = ticks, y = mean.fm), method = "lm", color = "grey20") +
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
  geom_smooth(data = plot.data2 %>% filter(ticks > 2000 & ticks <= 2250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2250 & ticks <= 2500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2500 & ticks <= 2750), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 2750 & ticks <= 3000), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 3000 & ticks <= 3250), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data2 %>% filter(ticks > 3250 & ticks <= 3500), 
              mapping = aes(x = ticks, y = mean.fm, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_manual(values = period.colors, 
                     labels = c("productive period", "unproductive period")) +
  theme(strip.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "average forager movements")
#plot(mobility.plot2)

ggarrange(mobility.plot, mobility.plot2, 
          ncol = 1, nrow = 2)

ggsave(filename = "figures/average-mobility.png", plot = mobility.plot,
       dpi = 300, width = 8, height = 4)


plot.data3 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"unproductive\"")
plot.data3$`burn-cost` =  as.factor(plot.data3$`burn-cost`)

interaction.plot = ggplot(plot.data3) +
  geom_smooth(data = plot.data3, 
              mapping = aes(x = ticks, y = adj.fi), method = "lm", color = "grey20") +
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
  geom_smooth(data = plot.data3 %>% filter(ticks > 2000 & ticks <= 2250), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 2250 & ticks <= 2500), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 2500 & ticks <= 2750), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 2750 & ticks <= 3000), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 3000 & ticks <= 3250), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data3 %>% filter(ticks > 3250 & ticks <= 3500), 
              mapping = aes(x = ticks, y = adj.fi, color = climate.condition), method = "lm") +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, 
                                 `burn-cost` = burn.labs)) +
  scale_color_manual(values = period.colors, 
                     labels = c("productive period", "unproductive period")) +
  theme(strip.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "proportion of population interacted with")



#### social network maintenance ####
hist(data$mean.same.fi)
hist(data$same.fi.prop)

# ggplot(data) +
#   geom_smooth(aes(x = ticks, y = same.fi.prop))  +
#   facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = label_both)
# 
# fit.sfi = lm(same.fi.prop ~ ticks*(.), data = data %>% select_at(c("ticks", parameters, "same.fi.prop")))
# plot_summs(fit.sfi, scale = T)

# sn.plot = ggplot(data) +
#   geom_point(aes(x = ticks, y = same.fi.prop, color = `movement-model`), size = 0.01, alpha = 0.01) +
#   geom_smooth(aes(x = ticks, y = same.fi.prop, color = `movement-model`)) +
#   facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = label_both) +
#   scale_color_colorblind()

plot.data4 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`veg-cycle-start` == "\"unproductive\"") %>%
  filter(`movement-model` == "\"Directed Walk\"")
sfi.plot = ggplot(plot.data4) +
  geom_smooth(data = plot.data4, 
              mapping = aes(x = ticks, y = 1 - same.fi.prop), color = "grey20") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 0 & ticks <= 250), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 250 & ticks <= 500), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 500 & ticks <= 750), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 750 & ticks <= 1000), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1000 & ticks <= 1250), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1250 & ticks <= 1500), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1500 & ticks <= 1750), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1750 & ticks <= 2000), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2000 & ticks <= 2250), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2250 & ticks <= 2500), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2500 & ticks <= 2750), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2750 & ticks <= 3000), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 3000 & ticks <= 3250), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 3250 & ticks <= 3500), 
              mapping = aes(x = ticks, y = 1 - same.fi.prop, color = climate.condition), method = "lm") +
  facet_wrap(~ `burn-cost`,
             labeller = labeller(`burn-cost` = burn.labs), scales = "free") +
  scale_color_manual(values = period.colors, 
                     labels = c("productive period", "unproductive period")) +
  theme(strip.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "proportion of interactions that are new")

plot.data5 = data %>% filter(ticks > 0) %>% filter(`cycle-duration` == 250) %>% filter(`movement-model` == "\"Directed Walk\"")
pop.plot = ggplot(plot.data5) +
  geom_smooth(data = plot.data5 %>% filter(ticks > 0), 
              mapping = aes(x = ticks, y = pop.dens), color = "grey20") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 0 & ticks <= 250), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 250 & ticks <= 500), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 500 & ticks <= 750), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 750 & ticks <= 1000), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1000 & ticks <= 1250), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1250 & ticks <= 1500), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1500 & ticks <= 1750), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 1750 & ticks <= 2000), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2000 & ticks <= 2250), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2250 & ticks <= 2500), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2500 & ticks <= 2750), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 2750 & ticks <= 3000), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 3000 & ticks <= 3250), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  geom_smooth(data = plot.data4 %>% filter(ticks > 3250 & ticks <= 3500), 
              mapping = aes(x = ticks, y = pop.dens, color = climate.condition), method = "lm") +
  facet_wrap(~ `burn-cost`, nrow = 1,
             labeller = labeller(`burn-cost` = burn.labs), scales = "free") +
  scale_color_manual(values = period.colors, 
                     labels = c("productive period", "unproductive period")) +
  theme(strip.text = element_text(size = 8), 
        legend.title = element_blank(), 
        legend.position = "bottom", 
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7)) +
  guides(fill = "none") + labs(y = "population density")
plot(pop.plot)

all.plot = ggarrange(
  interaction.plot, sfi.plot,
  ncol = 1, nrow = 2, common.legend = T, legend = "bottom", labels = "AUTO"
)
ggsave(filename = "preliminary_figures/interaction_fig5a.png", plot = interaction.plot, 
       dpi = 300, width = 8, height = 3.5)
ggsave(filename = "preliminary_figures/interaction_fig5b.png", plot = sfi.plot, 
       dpi = 300, width = 8, height = 4)

ggsave(filename = "figures/adjusted-interaction+social-network.png", plot = all.plot,
       dpi = 300, width = 8, height = 6)


