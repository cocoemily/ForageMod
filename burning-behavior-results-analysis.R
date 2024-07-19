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

outputs = c(
  "pop.count", 
  "veg_0", 
  "mean.burn.prob", 
  #"mean.fi", #average forager interaction count
  "adj.fi", #average interaction count/ population count
  "mean.fm",  #average forager movements per capita
  "veg.morans.i", 
  "veg.simpsons.div", 
  "benefit_self", 
  "benefit_other"
)
  
#finished = data %>% filter(ticks == 2000)
# length(unique(finished$exp))

#### Visualizing trends ####
long.data = data %>% dplyr::select(all_of(c("ticks", parameters, outputs))) %>%
  pivot_longer(cols = outputs, names_to = "output", values_to = "value")

trend.plot = ggplot(long.data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, y = value)) +
  geom_smooth(aes(x = ticks, y = value), method = "lm") +
  facet_grid(output ~ `veg-cycle-start` + `cycle-duration`, scales = "free")
ggsave(filename = "preliminary_figures/trendlines_outputs.png", plot = trend.plot, 
       dpi = 100, width = 11, height = 8.5)
rm(list = c("long.data", "trend.plot"))

corrplot::corrplot(cor(data %>% select_at(outputs), use = "pairwise.complete.obs"), method = "number")


#### Preliminary parameter effect testing ####
hist(log(data$veg_0))
descdist(data$veg_0)
summary(data$veg_0)

veg0.fit = glm(veg_0 ~ ticks*(.), data = data %>% dplyr::select_at(c("veg_0", "ticks", parameters)), family = quasibinomial("logit"))
summary(veg0.fit)

#veg0.fit2 = glm(veg_0 ~ ticks:(.), data = data %>% dplyr::select_at(c("veg_0", "ticks", parameters)), family = quasibinomial("logit"))
#car::Anova(veg0.fit2, veg0.fit, type = 2)

##### effects of parameters on mean burn probability over time ####
plotNormalHistogram(data$mean.burn.prob)
descdist(data$mean.burn.prob) #normal
mbp.fit = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
mbp.fit2 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))

anova(mbp.fit, mbp.fit2)
summary(mbp.fit2)

#split model by cycle-duration
mbp.fit1 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)) %>% filter(`cycle-duration` == 100) %>% dplyr::select(-`cycle-duration`))
mbp.fit2 = lm(mean.burn.prob ~ ticks*(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)) %>% filter(`cycle-duration` == 250) %>% dplyr::select(-`cycle-duration`))
estimates = as.data.frame(summary(mbp.fit1)$coefficients[,1:2]) %>% rownames_to_column() %>%
  mutate(cycle_duration = 100)
estimates = rbind(estimates, 
                  as.data.frame(summary(mbp.fit2)$coefficients[,1:2]) %>% rownames_to_column() %>%
                    mutate(cycle_duration = 250))
colnames(estimates) = c("term", "Estimate", "error", "cycle_duration")

ggplot(estimates %>% filter(term != "(Intercept)") %>% filter(!str_detect(term, "ticks"))) + 
  geom_point(aes(x = term, y = Estimate, color = as.factor(cycle_duration), group = cycle_duration)) +
  geom_errorbar(aes(x = term, ymin = Estimate - error, ymax = Estimate + error)) +
  coord_flip()
ggplot(estimates %>% filter(term != "(Intercept)") %>% filter(str_detect(term, "ticks"))) + 
  geom_point(aes(x = term, y = Estimate, color = as.factor(cycle_duration), group = cycle_duration)) +
  geom_errorbar(aes(x = term, ymin = Estimate - error, ymax = Estimate + error)) +
  coord_flip()

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
rate.fit = lm(slope ~ ., data = bp.rates %>% select_at(c("slope", parameters)))
summary(rate.fit)
#how to compare to benefit gained 

##### effects of parameters on population over time ####
hist(data$pop.count) #normal
descdist(data$pop.count, discrete = T)
pop.fit = glm(pop.count ~ ticks*(.), data = data %>% dplyr::select_at(c("pop.count", "ticks", parameters)), family = "poisson")
summary(pop.fit)

##### forager movements ####
hist(data$mean.fm)
descdist(data$mean.fm)
#normal distribution is probably not best
fm.fit = lm(mean.fm ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.fm", "ticks", parameters)))
summary(fm.fit)

unprod = data %>% filter(climate.condition == "unproductive")
hist(unprod$mean.fm)
prod = data %>% filter(climate.condition == "productive")
hist(prod$mean.fm)
descdist(prod$mean.fm) #normal
fm.fit1 = lm(mean.fm ~ ticks*(.), data = prod %>% filter(ticks > 0) %>% dplyr::select_at(c("mean.fm", "ticks", parameters)))
summary(fm.fit1)

fm.fit2 = lm(mean.fm ~ ., data = prod %>% filter(ticks > 0) %>% dplyr::select_at(c("mean.fm", parameters)))
summary(fm.fit2)
anova(fm.fit2, fm.fit1)

fm.fit3 = lm(mean.fm ~ ticks*(.), data = prod %>% filter(ticks > 0) %>% dplyr::select_at(c("mean.fm", "ticks", parameters, "veg.morans.i")))
#anova(fm.fit3, fm.fit1) not allowing comparison

ggplot(data %>% filter(ticks > 0)) +
  geom_point(aes(x = mean.fm, y = veg.morans.i))
data.ticks1 = data %>% filter(ticks > 0)
cor(x = as.numeric(data.ticks1$mean.fm), y = as.numeric(data.ticks1$veg.morans.i), method = "spearman")


##### forager interactions ####
plotNormalHistogram(data$adj.fi)
descdist(data$adj.fi)
fi.fit = lm(adj.fi ~ ticks*(.), data = data %>% dplyr::select_at(c("adj.fi", "ticks", parameters)))
summary(fi.fit)

ggplot(data) +
  geom_histogram(aes(x = adj.fi), binwidth = 0.0001)

fi.rates = data.frame(
  exp = character(0), 
  slope = numeric(0)
)
experiments = unique(data$exp)
for(x in experiments) {
  lmdata = data %>% filter(exp == x)
  fit = lm(adj.fi ~ ticks, data = lmdata)
  fi.rates[nrow(fi.rates) + 1, ] <- c(x, summary(fit)$coefficients[2,1])
}
fi.rates = fi.rates %>% left_join(data %>% select_at(c("exp", parameters)), by = "exp", multiple = "first")
fi.rates$slope = as.numeric(fi.rates$slope)
hist(fi.rates$slope)
descdist(fi.rates$slope)
fi.rate.fit = lm(slope ~ ., data = fi.rates %>% select_at(c("slope", parameters)))
summary(fi.rate.fit)
summary(fi.rates$slope)

##### spatial autocorrelation ####
plotNormalHistogram(data$veg.morans.i)
summary(data$veg.morans.i)
descdist((data %>% filter(veg.morans.i != "NA"))$veg.morans.i)
morans.i = data %>% filter(veg.morans.i != "NA") %>% filter(ticks > 0)
plotNormalHistogram(morans.i$veg.morans.i)
mi.fit = lm(veg.morans.i ~ ticks*(.), data = morans.i %>% dplyr::select_at(c("veg.morans.i", "ticks", parameters)))
summary(mi.fit)

##### vegetation diversity ####
plotNormalHistogram((data %>% filter(veg.simpsons.div != "NA") %>% filter(ticks > 0))$veg.simpsons.div)
descdist((data %>% filter(veg.simpsons.div != "NA") %>% filter(ticks > 0))$veg.simpsons.div)
ggplot(data) +
  geom_density(aes(x = veg.simpsons.div, group = `movement-model`, color = `movement-model`))
#bimodality caused by movement strategy
adj.parameters = c(
  "natural-ignition", "cycle-duration", "veg-cycle-start", "veg-distribution", "burnt-neighbor-limit", "burn-cost", "burn-veg-type-threshold" # 4, 7
)

vd.random = data %>% filter(`movement-model` == "\"Random Walk\"") %>% filter(ticks > 0) %>% filter(veg.simpsons.div != "NA")
hist(vd.random$veg.simpsons.div)
descdist(vd.random$veg.simpsons.div)
#summary(betareg(veg.simpsons.div ~ ticks*(.), data = vd.random %>% dplyr::select_at(c("veg.simpsons.div", "ticks", adj.parameters))))
sd.rand.fit1 = glm(veg.simpsons.div ~ ticks*(.), data = vd.random %>% dplyr::select_at(c("veg.simpsons.div", "ticks", adj.parameters)), family = Gamma(link = "log"))
summary(sd.rand.fit1)

vd.directed = data %>% filter(`movement-model` == "\"Directed Walk\"") %>% filter(ticks > 0) %>% filter(veg.simpsons.div != "NA")
hist(vd.directed$veg.simpsons.div)
descdist(vd.directed$veg.simpsons.div)
sd.dir.fit1 = glm(veg.simpsons.div ~ ticks*(.), data = vd.directed %>% select_at(c("veg.simpsons.div", "ticks", adj.parameters)), family = Gamma(link = "log"))
summary(sd.dir.fit1)


#### Mean forager burning probability ####
ggplot(data) +
  geom_line(aes(x = ticks, 
                y = mean.burn.prob)) +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)

rate = data %>%
  mutate(diff_tick = ticks - lag(ticks), 
         diff_prob = mean.burn.prob - lag(mean.burn.prob), 
         rate = (diff_prob / diff_tick)/mean.burn.prob) %>%
  group_by_at(c("exp", parameters)) %>%
  summarize(avg.rate = mean(rate, na.rm = T))

summary(lm(avg.rate ~ ., data = rate[,2:ncol(rate)]))

#### Proportion of landscape with veg-type = 0 (burnt) ####
ggplot(data) +
  geom_line(aes(x = ticks, 
                y = veg_0)) + 
  geom_smooth(aes(x = ticks, y = veg_0), method = "lm") +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)

ggplot(data %>% filter(`cycle-duration` == 100)) +
  geom_line(aes(x = ticks, 
                y = veg_0)) + 
  geom_smooth(aes(x = ticks, y = veg_0), method = "lm") +
  facet_grid(`burn-cost` ~ `burnt-neighbor-limit` + `burn-veg-type-threshold`, 
             labeller = label_both)

#### Population ####
ggplot(data) +
  geom_line(aes(x = ticks, y = pop.count)) +
  geom_smooth(aes(x = ticks, y = pop.count)) +
  facet_wrap(~`cycle-duration`, labeller = label_both)

# library(segmented)
# fit_lm = lm(pop.count ~ 1 + ticks, data = data %>% filter(`cycle-duration` == 100))
# fit_segmented = segmented(fit_lm, seg.Z = ~ticks, npsi = 19)

#### Average forager interactions ####
ggplot(data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, 
                y = mean.fi/pop.count))  +
  geom_smooth(aes(x = ticks, y = mean.fi/pop.count)) +
  facet_wrap(~`cycle-duration`, labeller = label_both)



#### Average forager movements ####
ggplot(data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, y = mean.fm)) +
  geom_smooth(aes(x = ticks, y = mean.fm), method = "lm")

ggplot(data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, y = veg.morans.i)) +
  geom_smooth(aes(x = ticks, y = veg.morans.i), method = "lm")
  
