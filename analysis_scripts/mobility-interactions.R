library(tidyverse)
library(ggthemes)
library(scales)
library(rcompanion)
library(lme4)
library(lmerTest)
library(broom.mixed)
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
  slice_sample(prop = 0.10) %>%
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
move.labs = c("random walks", "directed walks")
names(move.labs) = c("\"Random Walk\"", "\"Directed Walk\"")

#### mobility analysis ####
hist(data$mean.fm)

fm.plot = ggplot(sampled_data %>% filter(ticks > 0)) +
  geom_point(aes(x = ticks, y = mean.fm, color = `movement-model`), alpha = 0.01, size = 0.1) +
  #geom_smooth(aes(x = ticks, y = mean.fm, color = `movement-model`), method = "lm") +
  facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = labeller(
    `veg-cycle-start` = vs.labs, `cycle-duration` = cd.labs
  )) +
  scale_color_colorblind(labels = label_dictionary(move.labs)) +
  theme(strip.text = element_text(size = 6), legend.title = element_blank(), legend.position = "bottom") +
  labs(y = "average steps per foraging bout")
plot(fm.plot)

ggsave(filename = "preliminary_figures/average_forager_movement.png", plot = fm.plot,
       dpi = 300, width = 6, height = 5)

#### interaction analysis ####
summary(data$mean.fi)
cor(data$mean.fi, data$pop.density)

fi.plot = ggplot(sampled_data %>% filter(ticks > 0)) +
  geom_point(aes(x = ticks, y = mean.fi, color = `movement-model`), alpha = 0.01, size = 0.05) +
  #geom_smooth(aes(x = ticks, y = adj.fi, color = `movement-model`), se = T) +
  facet_grid(`cycle-duration` ~ `veg-cycle-start`, labeller = labeller(
    `veg-cycle-start` = vs.labs, `cycle-duration` = cd.labs
  )) +
  scale_color_colorblind(labels = label_dictionary(move.labs)) +
  theme(strip.text = element_text(size = 6), legend.title = element_blank(), legend.position = "bottom") +
  scale_y_continuous(limits = c(0, 0.025)) +
  labs(y = "average proportion of population a forager interacts with")
plot(fi.plot)

ggsave(filename = "preliminary_figures/average-adjusted-interaction.png", plot = fi.plot,
       dpi = 300, width = 6, height = 5)

####FIGURES#####

period.colors = RColorBrewer::brewer.pal(6, "Set2")[5:6]

##### forager mobility #####
plot.data <- sampled_data %>% 
  filter(ticks > 0, `veg-cycle-start` == "\"productive\"") %>%
  group_by(`movement-model`, `burn-cost`, climate.condition, ticks) %>%
  summarize(
    mean.fm = mean(mean.fm, na.rm = TRUE), 
    high.fm = mean(high.fm, na.rm = TRUE), 
    low.fm = mean(low.fm, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  group_by(`movement-model`, `burn-cost`, climate.condition) %>%
  do({
    fit.mean <- loess(mean.fm ~ ticks, data = ., span = 0.75)
    fit.high <- loess(high.fm ~ ticks, data = ., span = 0.75)
    fit.low  <- loess(low.fm ~ ticks, data = ., span = 0.75)
    
    data.frame(
      ticks = .$ticks, 
      mean.fm = predict(fit.mean, newdata = data.frame(ticks = .$ticks)), 
      high.fm = predict(fit.high, newdata = data.frame(ticks = .$ticks)), 
      low.fm  = predict(fit.low,  newdata = data.frame(ticks = .$ticks))
    )
  }) %>%
  ungroup()
  

mobility.plot = ggplot(plot.data) +
  geom_smooth(aes(x = ticks, y = mean.fm), method = "lm", color = "grey20", se = FALSE) +
  geom_line(aes(x = ticks, y = mean.fm, color = climate.condition), linewidth = 1) +
  # geom_line(aes(x = ticks, y = high.fm, color = climate.condition), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  # geom_line(aes(x = ticks, y = low.fm,  color = climate.condition), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, `burn-cost` = bc.labs)) +
  scale_color_manual(values = period.colors) +
  scale_fill_manual(values = period.colors) +
  theme(
    strip.text = element_text(size = 8), 
    legend.title = element_blank(), 
    legend.position = "bottom", 
    axis.title = element_text(size = 7.5),
    axis.text = element_text(size = 7)
  ) + 
  labs(y = "average forager movements")
plot(mobility.plot)


analysis_data = data %>% filter(ticks > 0) %>%
  mutate(
    exp = as.factor(exp), 
    climate.condition = as.factor(climate.condition),
    ticks_scaled = ticks / 1000
    )

fm_results_table = analysis_data %>% 
  group_by(across(all_of(parameters))) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~ tryCatch(
      lmer(mean.fm ~ poly(ticks_scaled, 2) * climate.condition + (1 | exp), data = .x),
      error = function(e) NULL
    )),
    tidy_results = purrr::map(model, ~ {
      if (is.null(.x)) {
        NULL
      } else {
        tidy(.x, effects = "fixed")
      }
    })
  ) %>%
  dplyr::select(-data, -model) %>%
  unnest(tidy_results) %>%
  ungroup()

nrow(fm_results_table %>% filter(term == "poly(ticks_scaled, 2)1"))
nrow(fm_results_table %>% filter(term == "poly(ticks_scaled, 2)1", p.value < 0.05, estimate < 0))


##### forager interaction proportions #####
plot.data <- sampled_data %>% 
  filter(ticks > 0, `veg-cycle-start` == "\"productive\"") %>%
  group_by(`movement-model`, `burn-cost`, climate.condition, ticks) %>%
  summarize(
    mean.fi = mean(mean.fi, na.rm = TRUE), 
    high.fi = mean(high.fi, na.rm = TRUE), 
    low.fi = mean(low.fi, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  group_by(`movement-model`, `burn-cost`, climate.condition) %>%
  do({
    fit.mean <- loess(mean.fi ~ ticks, data = ., span = 0.75)
    fit.high <- loess(high.fi ~ ticks, data = ., span = 0.75)
    fit.low  <- loess(low.fi ~ ticks, data = ., span = 0.75)
    
    data.frame(
      ticks = .$ticks, 
      mean.fi = predict(fit.mean, newdata = data.frame(ticks = .$ticks)), 
      high.fi = predict(fit.high, newdata = data.frame(ticks = .$ticks)), 
      low.fi = predict(fit.low,  newdata = data.frame(ticks = .$ticks))
    )
  }) %>%
  ungroup()


interaction.plot = ggplot(plot.data) +
  geom_smooth(aes(x = ticks, y = mean.fi), method = "lm", color = "grey20", se = FALSE) +
  geom_line(aes(x = ticks, y = mean.fi, color = climate.condition), linewidth = 1) +
  geom_line(aes(x = ticks, y = high.fi, color = climate.condition), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  geom_line(aes(x = ticks, y = low.fi,  color = climate.condition), linetype = "dashed", linewidth = 0.5, alpha = 0.6) +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, `burn-cost` = bc.labs)) +
  scale_color_manual(values = period.colors) +
  scale_fill_manual(values = period.colors) +
  theme(
    strip.text = element_text(size = 8), 
    legend.title = element_blank(), 
    legend.position = "bottom", 
    axis.title = element_text(size = 7.5),
    axis.text = element_text(size = 7)
  ) + 
  labs(y = "proportion of population interacted with")
plot(interaction.plot)

fi_results_table = analysis_data %>% 
  group_by(across(all_of(parameters))) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~ tryCatch(
      lmer(mean.fi ~ poly(ticks_scaled, 2) * climate.condition + (1 | exp), data = .x),
      error = function(e) NULL
    )),
    tidy_results = purrr::map(model, ~ {
      if (is.null(.x)) {
        NULL
      } else {
        tidy(.x, effects = "fixed")
      }
    })
  ) %>%
  dplyr::select(-data, -model) %>%
  unnest(tidy_results) %>%
  ungroup()

nrow(fi_results_table %>% filter(term == "poly(ticks_scaled, 2)1", `movement-model` == "\"Directed Walk\""))
nrow(fi_results_table %>% filter(term == "poly(ticks_scaled, 2)1", `movement-model` == "\"Directed Walk\"", p.value < 0.05, estimate < 0))

##### forager network similarity #####

#trying to show that network similarity increases over time, but clearly it levels off quickly by around 1000 ticks
#need to look at same forager interaction counts/proportions

ggplot(sampled_data) +
  geom_smooth(aes(x = ticks, y = net.sim)) +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, `burn-cost` = bc.labs))

##### forager network distances #####

#the distances between networks drops consistently across the first half of model run
#need to relate this to the population density, when this reaches carrying capacity
#there may be something to say here about social structures becoming fixed early? but then those networks getting pushed into smaller areas? does that make any sense?

ggplot(sampled_data) +
  geom_smooth(aes(x = ticks, y = net.clust)) +
  facet_grid(`movement-model` ~ `burn-cost`, 
             labeller = labeller(`movement-model` = move.labs, `burn-cost` = bc.labs))


