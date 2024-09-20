library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
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

# data = data %>%
#   mutate(benefit_self_0 = ifelse(benefit_self == 0, 0.000001, benefit_self), 
#          benefit_other_0 = ifelse(benefit_other == 0, 0.000001, benefit_other))
data$benefit.ratio = data$benefit_self/data$benefit_other


mbp.plot = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = `burn-cost`), alpha = 0.25) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "red") +
  scale_color_manual(values = c("grey0", "grey30", "grey60", "grey80")) +
  labs(y = "mean probability of burning", x = "ticks", color = "cost of burning") +
  theme(legend.position = "bottom")
#plot(mbp.plot)
ggsave(filename = "preliminary_figures/all_burn-prob.png", plot = mbp.plot, 
       dpi = 300, width = 6, height = 4)

##### effects of parameters on mean burn probability over time ####
plotNormalHistogram(data$mean.burn.prob)
descdist(data$mean.burn.prob) #uniform

mbp.fit1 = lm(mean.burn.prob ~ ticks + (.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
mbp.fit2 = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
mbp.fit3 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))

anova(mbp.fit1, mbp.fit2, mbp.fit3)
summary(mbp.fit3)

ggplot(data) +
  geom_point(aes(x = ticks, y = mean.burn.prob)) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob)) +
  facet_grid(`burn-cost` ~ `burnt-neighbor-limit` +  `burn-veg-type-threshold`)

##### benefit ratio ####
ggplot(data %>% filter(ticks > 0)) +
  geom_density(aes(x = benefit.ratio))
ggplot(data) +
  geom_point(aes(x = benefit_self, y = mean.burn.prob))

br.plot = ggplot(data) +
  geom_smooth(aes(x = ticks, y = benefit.ratio, group = exp, color = `burn-cost`), alpha = 0.25) +
  geom_smooth(aes(x = ticks, y = benefit.ratio)) +
  scale_color_manual(values = c("grey0", "grey30", "grey60", "grey80")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "ticks", y = "ratio of self benefit to other benefit")
#plot(br.plot)
ggsave(filename = "preliminary_figures/all_benefit-ratio.png", plot = br.plot, 
       dpi = 300, width = 6, height = 4)


##### Rate of Increase in Burning Probability ####
#first, determine slope of line for each experiment
bp.rates = data.frame(
  exp = character(0), 
  slope = numeric(0)
)
experiments = unique(data$exp)
for(x in experiments) {
  lmdata = data %>% filter(exp == x)
  fit = lm(mean.burn.prob ~ ticks, data = lmdata)
  bp.rates[nrow(bp.rates) + 1, ] <- c(x, summary(fit)$coefficients[2,1])
}
bp.rates = bp.rates %>% left_join(data %>% select_at(c("exp", parameters)), by = "exp", multiple = "first")
bp.rates$slope = as.numeric(bp.rates$slope)

hist(bp.rates$slope)
descdist(bp.rates$slope)

rate.fit1 = lm(slope ~ ., data = bp.rates %>% select_at(c("slope", parameters)))
rate.fit2 = lm(slope ~ .:., data = bp.rates %>% select_at(c("slope", parameters)))
rate.fit3 = lm(slope ~ .*., data = bp.rates %>% select_at(c("slope", parameters)))
anova(rate.fit1, rate.fit2, rate.fit3)

summary(rate.fit2)


#how to best visualize this?
# ggplot(bp.rates) +
#   geom_histogram(aes(x = slope, group = `burn-veg-type-threshold`, fill = `burn-veg-type-threshold`), bins = 50) +
#   geom_vline(xintercept = 0) +
#   facet_grid(`burn-cost` ~ `burnt-neighbor-limit`)

#how to compare to benefit gained?