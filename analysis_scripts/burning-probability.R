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

data$benefit.ratio = data$benefit_self/data$benefit_other
data$benefit.ratio2 = data$benefit_other/data$benefit_self


##### mean burn probability #####
dc.labs = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")
names(dc.labs) = c(0, 100, 200, 300)
bt.labs = c("can disturb limited resource types", "can disturb all resource types")
names(bt.labs) = c(4, 7)
bn.labs = c("disturb with 8 disturbed neighbors", 
            "disturb with 4 disturbed neighbors", 
            "disturb with 1 disturbed neighbor")
names(bn.labs) = c(8, 4, 1)

hist(data$mean.burn.prob)

mbp.plot = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = dc.labs) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(`movement-model` ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = dc.labs, `burn-veg-type-threshold` = bt.labs, 
                        `burnt-neighbor-limit` = bn.labs)) +
  labs(y = "mean probability of disturbance", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8))
plot(mbp.plot)
ggsave(filename = "preliminary_figures/all_burn-prob.png", plot = mbp.plot, 
       dpi = 300, width = 8, height = 3.5)

###### effects of parameters on  over time ######
# plotNormalHistogram(data$mean.burn.prob)
# 
# mbp.fit1 = lm(mean.burn.prob ~ ticks + (.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
# mbp.fit2 = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
# mbp.fit3 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
# 
# anova(mbp.fit1, mbp.fit2, mbp.fit3)
# summary(mbp.fit3)

###### high burning costs #####
high.burn = data %>% filter(`burn-cost` == 300)

ggplot(high.burn) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  geom_hline(yintercept = 0, linetype = "dotted")

hist((high.burn %>% filter(ticks == 2000))$mean.burn.prob)

ggplot(high.burn %>% filter(ticks == 2000)) +
  geom_density(aes(x = mean.burn.prob)) +
  facet_wrap(~ `movement-model`, labeller = label_both)

ggplot(high.burn %>% filter(ticks == 2000)) +
  geom_density(aes(x = pop.dens)) +
  facet_wrap(~ `movement-model`, labeller = label_both)
  

# ggplot(high.burn) +
#   geom_smooth(aes(x = ticks, y = veg_7), color = "darkgreen") +
#   geom_smooth(aes(x = ticks, y = veg_6), color = "forestgreen") +
#   geom_smooth(aes(x = ticks, y = veg_5), color = "green") +
#   facet_wrap(~ `movement-model`, labeller = label_both)

##### benefit ratio #####
ggplot(data) +
  geom_point(aes(x = benefit_self, y = mean.burn.prob))

summary((data$veg_0 + data$burnt))


data$bself.prop = data$benefit_self / (data$pop.count * data$mean.fm)
summary(data$bself.prop)
#proportion of average total foraging events that result in a benefit to self
#lapply(data %>% filter(is.nan(bself.prop)) %>% select_at(c(parameters)), unique)
summary((data %>% filter(is.nan(bself.prop)))$ticks)

data$bother.prop = data$benefit_other / (data$pop.count * data$mean.fm)
#proportion of average total foraging events that result in a benefit to self


cd.labs = c("cycle = 100 ticks", "cycle = 250 ticks")
names(cd.labs) = c(100, 250)
vs.labs = c("productive start", "unproductive start")
names(vs.labs) = c("\"productive\"", "\"unproductive\"")

br.plot = ggplot(data %>% filter(ticks > 0)) +
  geom_smooth(aes(x = ticks, y = bself.prop, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = bself.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = dc.labs) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid( `movement-model` ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = dc.labs)) +
  labs(y = "proportion of disturbance benefits for self", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8), strip.text = element_text(size = 6))
#plot(br.plot)
ggsave(filename = "preliminary_figures/all_self-benefit-proportion.png", plot = br.plot, 
       dpi = 300, width = 8, height = 3.5)

hist((data %>% filter(ticks > 0))$bself.prop)
fit1.bself = glm(bself.prop ~ ticks*(.), data = data %>% select_at(c("ticks", parameters, "bself.prop"), family = "poisson"))
plot_summs(fit1.bself, scale = T)

ggplot(data %>% filter(ticks > 0)) +
  geom_smooth(aes(x = ticks, y = bother.prop, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = bother.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no burn cost (0)", "low burn cost (100)", "high burn cost (200)", "highest burn cost (300)")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_grid(`cycle-duration` + `veg-cycle-start` ~ `burn-cost`, labeller = 
               labeller(`burn-cost` = bc.labs, `cycle-duration` = cd.labs)) +
  labs(y = "proportion of burning benefits for others", x = "ticks", color = "cost of burning") +
  theme(legend.position = "none", axis.title = element_text(size = 8), strip.text = element_text(size = 6))

#####all plots #####
dc.labs = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")
names(dc.labs) = c(0, 100, 200, 300)

mbp.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = dc.labs, 
                     guide = "legend") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #facet_grid( ~ `burn-cost`, labeller = labeller(`burn-cost` = dc.labs)) +
  labs(y = "mean disturbance preference", x = "ticks", color = "") +
  theme(axis.title = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))
#plot(mbp.plot2)

br.plot2 = ggplot(data %>% filter(ticks > 0)) +
  geom_smooth(aes(x = ticks, y = bself.prop, group = exp, color = as.factor(`burn-cost`)), alpha = 0.01, linewidth = 0.05) +
  geom_smooth(aes(x = ticks, y = bself.prop), se = F, color = "black") +
  scale_color_brewer(palette = "Set2",
                     labels = dc.labs, 
                     guide = "legend") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #facet_grid( ~ `burn-cost`, labeller = labeller(`burn-cost` = dc.labs)) +
  labs(y = "proportion of disturbance benefits for self", x = "ticks", color = "") +
  theme(axis.title = element_text(size = 8)) +
  guides(colour = guide_legend(override.aes = list(linewidth = 2)))

####FIGURE 1####
all.plot = ggpubr::ggarrange(mbp.plot2, br.plot2, labels = "AUTO", 
          nrow = 1, common.legend = T, legend = "right")

ggsave(filename = "figures/disturbance-probabilities-plot.png", plot = all.plot,
       dpi = 300, width = 8, height = 3)

##### burning probability rate of change ####
# #first, determine slope of line for each experiment
# bp.rates = data.frame(
#   exp = character(0), 
#   slope = numeric(0)
# )
# experiments = unique(data$exp)
# for(x in experiments) {
#   lmdata = data %>% filter(exp == x)
#   fit = lm(mean.burn.prob ~ ticks, data = lmdata)
#   bp.rates[nrow(bp.rates) + 1, ] <- c(x, summary(fit)$coefficients[2,1])
# }
# bp.rates = bp.rates %>% left_join(data %>% select_at(c("exp", parameters)), by = "exp", multiple = "first")
# bp.rates$slope = as.numeric(bp.rates$slope)
# 
# hist(bp.rates$slope)
# #descdist(bp.rates$slope)
# summary(bp.rates$slope)
# 
# rate.fit1 = lm(slope ~ ., data = bp.rates %>% select_at(c("slope", parameters)))
# rate.fit2 = lm(slope ~ .:., data = bp.rates %>% select_at(c("slope", parameters)))
# rate.fit3 = lm(slope ~ .*., data = bp.rates %>% select_at(c("slope", parameters)))
# anova(rate.fit1, rate.fit2, rate.fit3)
# 
# summary(rate.fit2)
# summ(rate.fit2)
# 
# rate.fit.df = rate.fit2 %>% tidy() %>%
#   mutate(p.signif = ifelse(p.value < 0.05, TRUE, FALSE)) %>%
#   filter(p.signif == F)
# 
# plot_summs(rate.fit2, scale = T, omit.coefs = c("(Intercept)", rate.fit.df$term))
# plot_summs(rate.fit2, scale = F, omit.coefs = c("(Intercept)", rate.fit.df$term))
# 
# neg.bprate = bp.rates %>% filter(slope < 0) %>% select_at(c(parameters)) %>% distinct()
# lapply(neg.bprate[,parameters], unique)
# table(neg.bprate$`burn-cost`)
# length(unique((bp.rates %>% filter(slope < 0))$exp))/length(unique(bp.rates$exp))
# 
# #how to compare to benefit gained?
# bpr.benefit = bp.rates %>% right_join(data %>% dplyr::select(exp, benefit.ratio, bself.prop), by = c("exp")) %>%
#   group_by_at(c("exp", parameters)) %>%
#   summarize(slope = first(slope), 
#             mean.benefit.ratio = mean(benefit.ratio)) #need to figure out how to deal with Inf values here and NaN values
# 
# summary((bpr.benefit %>% filter(benefit.ratio < 1))$slope)
# summary((bpr.benefit %>% filter(benefit.ratio > 1))$slope)
