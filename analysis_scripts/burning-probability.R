library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
library(jtools)
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

outputs = c(
  "pop.count",
  "pop.dens", #pop count/world size
  "burnt", 
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
data$benefit.ratio2 = data$benefit_other/data$benefit_self

bc.labs = c("no burn cost (0)", "low burn cost (100)", "medium burn cost (200)", "high burn cost (300)")
names(bc.labs) = c(0, 100, 200, 300)
bt.labs = c("can burn veg types 1-4", "can burn veg types 1-7")
names(bt.labs) = c(4, 7)
bn.labs = c("burn with 8 burnt neighbors", 
            "burn with 4 burnt neighbors", 
            "burn with 1 burnt neighbor")
names(bn.labs) = c(8, 4, 1)

mbp.plot = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no burn cost (0)", "low burn cost (100)", "high burn cost (200)", "highest burn cost (300)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid( ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = bc.labs, `burn-veg-type-threshold` = bt.labs, 
                        `burnt-neighbor-limit` = bn.labs)) +
  labs(y = "mean probability of burning", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8))
plot(mbp.plot)
ggsave(filename = "preliminary_figures/all_burn-prob.png", plot = mbp.plot, 
       dpi = 300, width = 8, height = 4)

##### effects of parameters on mean burn probability over time ####
plotNormalHistogram(data$mean.burn.prob)

mbp.fit1 = lm(mean.burn.prob ~ ticks + (.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
mbp.fit2 = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
mbp.fit3 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))

anova(mbp.fit1, mbp.fit2, mbp.fit3)
summary(mbp.fit3)

ggplot(data) +
  #geom_point(aes(x = ticks, y = mean.burn.prob)) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob)) +
  facet_grid(`burn-cost` ~ `burnt-neighbor-limit` +  `burn-veg-type-threshold`)

##### benefit ratio ####
ggplot(data) +
  geom_point(aes(x = benefit_self, y = mean.burn.prob))

data$bself.prop = data$benefit_self / data$pop.count
ggplot(data) +
  geom_smooth(aes(x = ticks, y = bself.prop)) +
  facet_grid(`burn-cost` ~ `burnt-neighbor-limit` +  `burn-veg-type-threshold`)

summary(data$benefit.ratio2)


br.plot = ggplot(data) +
  geom_smooth(aes(x = ticks, y = benefit.ratio, group = exp, color = `burn-cost`), alpha = 0.25) +
  geom_smooth(aes(x = ticks, y = benefit.ratio)) +
  scale_color_manual(values = c("grey0", "grey30", "grey60", "grey80")) +
  geom_hline(yintercept = 1, linetype = "dotted") +
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
#descdist(bp.rates$slope)
summary(bp.rates$slope)

rate.fit1 = lm(slope ~ ., data = bp.rates %>% select_at(c("slope", parameters)))
rate.fit2 = lm(slope ~ .:., data = bp.rates %>% select_at(c("slope", parameters)))
rate.fit3 = lm(slope ~ .*., data = bp.rates %>% select_at(c("slope", parameters)))
anova(rate.fit1, rate.fit2, rate.fit3)

summary(rate.fit2)
summ(rate.fit2)

rate.fit.df = rate.fit2 %>% tidy() %>%
  mutate(p.signif = ifelse(p.value < 0.05, TRUE, FALSE)) %>%
  filter(p.signif == F)

plot_summs(rate.fit2, scale = T, omit.coefs = c("(Intercept)", rate.fit.df$term))
plot_summs(rate.fit2, scale = F, omit.coefs = c("(Intercept)", rate.fit.df$term))

neg.bprate = bp.rates %>% filter(slope < 0) %>% select_at(c(parameters)) %>% distinct()
lapply(neg.bprate[,parameters], unique)
table(neg.bprate$`burn-cost`)
length(unique((bp.rates %>% filter(slope < 0))$exp))/length(unique(bp.rates$exp))

#how to compare to benefit gained?
bpr.benefit = bp.rates %>% right_join(data %>% dplyr::select(exp, benefit.ratio, bself.prop), by = c("exp")) %>%
  group_by_at(c("exp", parameters)) %>%
  summarize(slope = first(slope), 
            mean.benefit.ratio = mean(benefit.ratio)) #need to figure out how to deal with Inf values here and NaN values

summary((bpr.benefit %>% filter(benefit.ratio < 1))$slope)
summary((bpr.benefit %>% filter(benefit.ratio > 1))$slope)
