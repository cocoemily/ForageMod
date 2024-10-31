#comparing rates of change with other outputs
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
data$prop.self.benefit = data$benefit_self/data$pop.count
data$unforageable = data$veg_0 + data$burnt

rates = data.frame(
  exp = character(0), 
  tick.range = character(0), 
  climate.condition = character(0),
  start.burn.prob = numeric(0),
  end.burn.prob = numeric(0),
  burn.prob.slope = numeric(0), 
  start.pop.density = numeric(0),
  end.pop.density = numeric(0),
  pop.dens.slope = numeric(0), 
  start.steps = numeric(0),
  end.steps = numeric(0),
  steps.slope = numeric(0), 
  start.interactions = numeric(0), 
  end.interactions = numeric(0), 
  interactions.slope = numeric(0), 
  average.prop.unforageable = numeric(0), 
  sd.prop.unforageable = numeric(0), 
  start.propbene = numeric(0), 
  end.propbene = numeric(0), 
  propbene.slope = numeric(0)
)

experiments = unique(data$exp)
for(x in experiments) {
  expdata = data %>% filter(exp == x) 
  tick.seq = NULL
  if(first(expdata$`cycle-duration`) == 250) {
    tick.seq = seq(from = 0, to = 2000, by = 250)
  } else {
    tick.seq = seq(from = 0, to = 2000, by = 100)
  }
  
  for(i in 1:(length(tick.seq) - 1)) {
    lmdata = expdata %>% filter(ticks > tick.seq[i] & ticks <= tick.seq[i + 1])
    bp.fit = lm(mean.burn.prob ~ ticks, data = lmdata)
    pd.fit = lm(pop.dens ~ ticks, data = lmdata)
    mv.fit = lm(mean.fm ~ ticks, data = lmdata)
    int.fit = lm(adj.fi ~ ticks, data = lmdata)
    br.fit = lm(prop.self.benefit ~ ticks, data = lmdata)
    rates[nrow(rates) + 1, ] <- c(x, paste0(tick.seq[i] + 1, "-", tick.seq[i+1]), 
                                  first(lmdata$climate.condition),
                                  summary(bp.fit)$coefficients[1,1] + (tick.seq[i] * summary(bp.fit)$coefficients[2,1]), #calculate starting burning probability at beginning of tick range
                                  summary(bp.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(bp.fit)$coefficients[2,1]), #calculate final burning probability at end of tick range
                                  summary(bp.fit)$coefficients[2,1], #burning probability slope
                                  summary(pd.fit)$coefficients[1,1] + (tick.seq[i] * summary(pd.fit)$coefficients[2,1]), #calculate starting population density at beginning of tick range
                                  summary(pd.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(pd.fit)$coefficients[2,1]), #calculate final population density at end of tick range
                                  summary(pd.fit)$coefficients[2,1], #population density slope
                                  summary(mv.fit)$coefficients[1,1] + (tick.seq[i] * summary(mv.fit)$coefficients[2,1]), #calculate starting forage moves at beginning of tick range
                                  summary(mv.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(mv.fit)$coefficients[2,1]), #calculate final forage moves at end of tick range
                                  summary(mv.fit)$coefficients[2,1], #forage moves slope
                                  summary(int.fit)$coefficients[1,1] + (tick.seq[i] * summary(int.fit)$coefficients[2,1]), #calculate starting forage interactions at beginning of tick range
                                  summary(int.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(int.fit)$coefficients[2,1]), #calculate final forage interactions at end of tick range
                                  summary(int.fit)$coefficients[2,1], #forage interactions slope
                                  mean(lmdata$unforageable), 
                                  sd(lmdata$unforageable), 
                                  summary(br.fit)$coefficients[1,1] + (tick.seq[i] * summary(br.fit)$coefficients[2,1]), #calculate starting benefit ratio at beginning of tick range
                                  summary(br.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(br.fit)$coefficients[2,1]), #calculate final benefit ratio at end of tick range
                                  summary(br.fit)$coefficients[2,1] #benefit ratio slope
    )
  }
}
write_rds(rates, file = "results/outputs/output-evolution-comparison.rds")

rates <- readRDS("results/outputs/output-evolution-comparison.rds")
rates = rates %>% left_join(data %>% select_at(c("exp", parameters)), by = "exp", multiple = "first")
rates[,4:20] <- lapply(rates[,4:20], as.numeric)

tick.seq1 = seq(from = 0, to = 2000, by = 100)
tick.levels = c()
for(i in 1:(length(tick.seq1) - 1)){
  tick.levels = c(tick.levels, paste0(tick.seq1[i] + 1, "-", tick.seq1[i+1]))
}
tick.seq2 = seq(from = 0, to = 2000, by = 250)
for(i in 1:(length(tick.seq2) - 1)){
  tick.levels = c(tick.levels, paste0(tick.seq2[i] + 1, "-", tick.seq2[i+1]))
}
rates$tick.range = factor(rates$tick.range, levels = tick.levels)

#### population change ####
ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"")) +
  geom_boxplot(aes(x = tick.range, y = pop.dens.slope, color = climate.condition)) +
  facet_wrap(~ `cycle-duration`, scales = "free") +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"")) +
  geom_boxplot(aes(x = tick.range, y = end.pop.density, color = climate.condition)) +
  facet_wrap(`cycle-duration` ~ `movement-model`, scales = "free_x") +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "100")) +
  geom_point(aes(x = pop.dens.slope, y = average.prop.unforageable, color = climate.condition)) +
  geom_smooth(aes(x = pop.dens.slope, y = average.prop.unforageable), method = "lm") +
  geom_vline(aes(xintercept = 0), color = "black") +
  geom_hline(aes(yintercept = 1), color = "black") +
  stat_cor(mapping = aes(x = pop.dens.slope, y = average.prop.unforageable), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free") +
  scale_color_brewer(palette = "Set2") 

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
  geom_point(aes(x = pop.dens.slope, y = average.prop.unforageable, color = climate.condition)) +
  geom_smooth(aes(x = pop.dens.slope, y = average.prop.unforageable), method = "lm") +
  geom_vline(aes(xintercept = 0), color = "black") +
  geom_hline(aes(yintercept = 1), color = "black") +
  stat_cor(mapping = aes(x = pop.dens.slope, y = average.prop.unforageable), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
  geom_point(aes(x = pop.dens.slope, y = sd.prop.unforageable, color = climate.condition)) +
  geom_smooth(aes(x = pop.dens.slope, y = sd.prop.unforageable), method = "lm") +
  geom_vline(aes(xintercept = 0), color = "black") +
  stat_cor(mapping = aes(x = pop.dens.slope, y = sd.prop.unforageable), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free") +
  scale_color_brewer(palette = "Set2")

# ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
#   geom_boxplot(aes(x = tick.range, y = start.pop.density, color = climate.condition))  +
#   scale_color_brewer(palette = "Set2")
# 
# ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
#   geom_boxplot(aes(x = tick.range, y = pop.dens.slope, color = climate.condition))  +
#   geom_hline(aes(yintercept = 0), color = "black") +
#   scale_color_brewer(palette = "Set2")

#### forager mobility ####

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"")) +
  geom_boxplot(aes(x = tick.range, y = steps.slope, color = climate.condition)) +
  facet_grid(`movement-model`~ `cycle-duration`, scales = "free", labeller = label_both) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"")) +
  geom_boxplot(aes(x = tick.range, y = start.steps, color = climate.condition)) +
  facet_grid(`movement-model`~ `cycle-duration`, scales = "free", labeller = label_both) +
  scale_color_brewer(palette = "Set2")

# ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "100")) +
#   geom_point(aes(y = average.prop.unforageable, x = steps.slope, color = climate.condition)) +
#   geom_smooth(aes(y = average.prop.unforageable, x = steps.slope), method = "lm") +
#   geom_vline(aes(xintercept = 0), color = "black") +
#   geom_hline(aes(yintercept = 1), color = "black") +
#   stat_cor(mapping = aes(y = average.prop.unforageable, x = steps.slope), 
#            method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
#   facet_wrap(~ tick.range, scales = "free") +
#   scale_color_brewer(palette = "Set2")


ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "100")) +
  geom_point(aes(y = pop.dens.slope, x = start.steps, color = climate.condition)) +
  geom_smooth(aes(y = pop.dens.slope, x = start.steps), method = "lm") +
  geom_hline(aes(yintercept = 0), color = "black") +
  stat_cor(mapping = aes(y = pop.dens.slope, x = start.steps), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(`movement-model`~ climate.condition, scales = "free") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "100") %>%
         filter(`movement-model` == "\"Directed Walk\"")) +
  geom_point(aes(y = end.pop.density, x = start.steps, color = climate.condition)) +
  geom_smooth(aes(y = end.pop.density, x = start.steps), method = "lm") +
  stat_cor(mapping = aes(y = end.pop.density, x = start.steps), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range) +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "100") %>%
         filter(`movement-model` == "\"Random Walk\"")) +
  geom_point(aes(y = end.pop.density, x = start.steps, color = climate.condition)) +
  geom_smooth(aes(y = end.pop.density, x = start.steps), method = "lm") +
  stat_cor(mapping = aes(y = end.pop.density, x = start.steps), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free_y") +
  scale_color_brewer(palette = "Set2")


#### forager interactions ####
ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
  geom_boxplot(aes(x = tick.range, y = interactions.slope, color = climate.condition)) +
  facet_grid( `movement-model` ~ `burn-cost`, scales = "free", labeller = label_both) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% filter(`cycle-duration` == "250")) +
  geom_boxplot(aes(x = tick.range, y = start.interactions, color = climate.condition)) +
  facet_grid( `cycle-duration` ~ `burn-cost`, scales = "free", labeller = label_both) +
  scale_color_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45))

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% 
         filter(`cycle-duration` == "100") %>% 
         filter(`movement-model` == "\"Directed Walk\"") %>%
         filter(tick.range == "1001-1100")) +
  geom_point(aes(y = start.steps, x = start.interactions, color = climate.condition)) +
  geom_smooth(aes(y = start.steps, x = start.interactions), method = "lm") +
  stat_cor(mapping = aes(y = start.steps, x = start.interactions), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ `burn-cost`, scales = "free_x") +
  scale_color_brewer(palette = "Set2")


ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% 
         filter(`cycle-duration` == "100") %>% 
         filter(`movement-model` == "\"Directed Walk\"") %>%
         filter(tick.range == "1001-1100")) +
  geom_point(aes(y = start.pop.density, x = start.interactions, color = climate.condition)) +
  geom_smooth(aes(y = start.pop.density, x = start.interactions), method = "lm") +
  stat_cor(mapping = aes(y = start.pop.density, x = start.interactions), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ `burn-cost`, scales = "free_x") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% 
         filter(`cycle-duration` == "100") %>% 
         filter(`movement-model` == "\"Directed Walk\"")  %>% 
         filter(`burn-cost` == 0)) +
  geom_point(aes(y = average.prop.unforageable, x = start.interactions, color = climate.condition)) +
  geom_smooth(aes(y = average.prop.unforageable, x = start.interactions), method = "lm") +
  stat_cor(mapping = aes(y = average.prop.unforageable, x = start.interactions), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free_x") +
  scale_color_brewer(palette = "Set2")

ggplot(rates %>% filter(`veg-cycle-start` == "\"productive\"") %>% 
         filter(`cycle-duration` == "100") %>% 
         filter(`movement-model` == "\"Directed Walk\"")  %>% 
         filter(`burn-cost` == 300)) +
  geom_point(aes(y = average.prop.unforageable, x = start.interactions, color = climate.condition)) +
  geom_smooth(aes(y = average.prop.unforageable, x = start.interactions), method = "lm") +
  stat_cor(mapping = aes(y = average.prop.unforageable, x = start.interactions), 
           method = "pearson", p.accuracy = 0.01, r.accuracy = 0.01) +
  facet_wrap(~ tick.range, scales = "free_x") +
  scale_color_brewer(palette = "Set2")

#TODO forager interactions are related to what when burning is frequent?

