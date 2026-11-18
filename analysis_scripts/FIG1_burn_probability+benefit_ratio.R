library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
library(jtools)
theme_set(theme_bw())

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

data$burnt = data$`veg_-1`

data$bself.prop = data$benefit_self / ((data$pop.density * 51 * 51) * data$mean.fm) #proportion of average total foraging events that result in a benefit to self
data$bother.prop = data$benefit_other / ((data$pop.density * 51 * 51) * data$mean.fm) #proportion of average total foraging events that result in a benefit to self

sample = data %>%
  group_by(exp) %>%
  slice(1) %>%
  group_by(across(all_of(parameters))) %>%
  slice_sample(prop = 0.20) %>%
  pull(exp)

sampled_data = data %>% filter(exp %in% sample)

bc.labs = c("no burn cost", "low burn cost", "medium burn cost", "high burn cost")
names(bc.labs) = c(0, 100, 200, 300)
bt.labs = c("can burn limited resource types", "can burn all resource types")
names(bt.labs) = c(4, 7)
bn.labs = c("burn with 8 burnt neighbors", 
            "burn with 4 burnt neighbors", 
            "burn with 1 burnt neighbor")
names(bn.labs) = c(8, 4, 1)

cd.labs = c("cycle = 100 ticks", "cycle = 250 ticks")
names(cd.labs) = c(100, 250)
vs.labs = c("productive start", "unproductive start")
names(vs.labs) = c("\"productive\"", "\"unproductive\"")


##### mean burn probability #####
hist(data$mean.burn.prob)

mbp.plot = ggplot(sampled_data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth (aes(x = ticks, y = mean.burn.prob), color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = bc.labs) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(`movement-model` ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = bc.labs, `burn-veg-type-threshold` = bt.labs, 
                        `burnt-neighbor-limit` = bn.labs)) +
  labs(y = "mean burn preference", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8))
plot(mbp.plot)
ggsave(filename = "preliminary_figures/sampled_burn_prob_over_time.png", plot = mbp.plot, 
       dpi = 300, width = 8, height = 3.5)


##### benefit ratio #####
summary(data$bself.prop)

br.plot = ggplot(sampled_data %>% filter(ticks > 0)) +
  geom_smooth(aes(x = ticks, y = bself.prop, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = bself.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = bc.labs) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid( `movement-model` ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = bc.labs)) +
  labs(y = "proportion of burning benefits for self", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8), strip.text = element_text(size = 6))
plot(br.plot)
ggsave(filename = "preliminary_figures/sampled_self_benefit_prop_over_time.png", plot = br.plot, 
       dpi = 300, width = 8, height = 3.5)

# hist((sampled_data %>% filter(ticks > 0))$bself.prop)
# fit1.bself = glm(bself.prop ~ ticks*(.), data = sampled_data %>% select_at(c("ticks", parameters, "bself.prop"), family = "poisson"))
# plot_summs(fit1.bself, scale = T)

####FIGURE 1####
mbp.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = bc.labs, 
                     guide = "legend") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "mean burning preference", x = "ticks", color = "") +
  theme(axis.title = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

br.plot2 = ggplot(data %>% filter(ticks > 0)) +
  geom_smooth(aes(x = ticks, y = bself.prop, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = bself.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = bc.labs, 
                     guide = "legend") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(y = "proportion of burning benefits for self", x = "ticks", color = "") +
  theme(axis.title = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

all.plot = ggpubr::ggarrange(mbp.plot2, br.plot2, labels = "AUTO", 
          nrow = 1, common.legend = T, legend = "right")

ggsave(filename = "figures/burning_behaviors_plot.png", plot = all.plot,
       dpi = 300, width = 8, height = 3)
