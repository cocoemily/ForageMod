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
  "veg.simpsons.div"
)
  
# finished = data %>% filter(ticks == 2000)
# length(unique(finished$exp))

#### Visualizing trends ####
long.data = data %>% dplyr::select(c("ticks", parameters, outputs)) %>%
  pivot_longer(cols = outputs, names_to = "output", values_to = "value")

trend.plot = ggplot(long.data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, y = value)) +
  geom_smooth(aes(x = ticks, y = value), method = "lm") +
  facet_grid(output ~ `veg-cycle-start` + `cycle-duration`, scales = "free")
ggsave(filename = "preliminary_figures/trendlines_outputs.png", plot = trend.plot, 
       dpi = 100, width = 11, height = 8.5)
rm(list = c("long.data", "trend.plot"))

#### Preliminary parameter effect testing ####
hist(log(data$veg_0))
descdist(data$veg_0)
summary(data$veg_0)
data$veg_0 = ifelse(data$veg_0 == 0, data$veg_0 + 0.00001, data$veg_0)

#veg0.fit = betareg(veg_0 ~ ticks:(.), data = data %>% dplyr::select_at(c("veg_0", "ticks", parameters)))
#car::Anova(veg0.fit)
#summary(veg0.fit)$coef[, 1, drop=F]

##### effects of parameters on mean burn probability over time ####
plotNormalHistogram(data$mean.burn.prob)
descdist(data$mean.burn.prob) #normal
mbp.fit = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)))
summary(mbp.fit)

#split model by cycle-duration
mbp.fit1 = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)) %>% filter(`cycle-duration` == 100) %>% dplyr::select(-`cycle-duration`))
mbp.fit2 = lm(mean.burn.prob ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.burn.prob", "ticks", parameters)) %>% filter(`cycle-duration` == 250) %>% dplyr::select(-`cycle-duration`))
estimates = as.data.frame(summary(mbp.fit1)$coefficients[,1:2]) %>% rownames_to_column() %>%
  mutate(cycle_duration = 100)
estimates = rbind(estimates, 
                  as.data.frame(summary(mbp.fit2)$coefficients[,1:2]) %>% rownames_to_column() %>%
                    mutate(cycle_duration = 250))
colnames(estimates) = c("term", "Estimate", "error", "cycle_duration")

ggplot(estimates %>% filter(term != "(Intercept)") %>% filter(term != "ticks")) + 
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
rate.fit = lm(slope ~ ., data = bp.rates %>% select_at(c("slope", parameters)))
summary(rate.fit)

##### effects of parameters on population over time ####
hist(data$pop.count) #normal
descdist(data$pop.count)
pop.fit = lm(pop.count ~ ., data = data %>% dplyr::select_at(c("pop.count", "ticks", parameters)))
summary(pop.fit)


##### forager movements ####
hist(data$mean.fm)
descdist(data$mean.fm)
#normal distribution is probably not best
fm.fit = lm(mean.fm ~ ticks:(.), data = data %>% dplyr::select_at(c("mean.fm", "ticks", parameters)))
summary(fm.fit)

##how to investigate relationship between spatial autocorrelation and forager movement while controlling for effects of other variables

##### forager interactions ####
plotNormalHistogram(data$adj.fi)
descdist(data$adj.fi)
fi.fit = lm(adj.fi ~ ticks*(.), data = data %>% dplyr::select_at(c("adj.fi", "ticks", parameters)))
summary(fi.fit)

ggplot(data) +
  geom_histogram(aes(x = adj.fi), binwidth = 0.0001) #the distribution is bimodal
ggplot(data) +
  geom_density(aes(x = adj.fi, group = `movement-model`, color = `movement-model`))
#bimodality being caused by what type of movement model

fi.random = data %>% filter(`movement-model` == "\"Random Walk\"")
fi.directed = data %>% filter(`movement-model` == "\"Directed Walk\"")

adj.parameters = c(
  "natural-ignition", "cycle-duration", "veg-cycle-start", "veg-distribution", "burnt-neighbor-limit", "burn-cost", "burn-veg-type-threshold" # 4, 7
)
fir.fit = lm(adj.fi ~ ticks*(.), data = fi.random %>% dplyr::select_at(c("adj.fi", "ticks", adj.parameters)))
summary(fir.fit)
fid.fit = lm(adj.fi ~ ticks*(.), data = fi.directed %>% dplyr::select_at(c("adj.fi", "ticks", adj.parameters)))
summary(fid.fit)

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
mi.fit = lm(veg.morans.i ~ ticks:(.), data = data %>% dplyr::select_at(c("veg.morans.i", "ticks", parameters)))
summary(mi.fit)

##### vegetation diversity ####
plotNormalHistogram((data %>% filter(veg.simpsons.div != "NA") %>% filter(ticks > 0))$veg.simpsons.div)
descdist((data %>% filter(veg.simpsons.div != "NA") %>% filter(ticks > 0))$veg.simpsons.div)
ggplot(data) +
  geom_density(aes(x = veg.simpsons.div, group = `movement-model`))
#bimodality caused by movement strategy
vd.random = data %>% filter(`movement-model` == "\"Random Walk\"")
hist((vd.random %>% filter(veg.simpsons.div != "NA"))$veg.simpsons.div)
vd.directed = data %>% filter(`movement-model` == "\"Directed Walk\"")
hist(vd.directed$veg.simpsons.div)

#### Variation between model runs ####
var.data = data %>% group_by_at(c("ticks", parameters)) %>%
  summarize(pop.count.cv = sd(pop.count)/mean(pop.count), 
            veg_0.cv = sd(veg_0)/mean(veg_0), 
            mean.burn.prob.cv = sd(mean.burn.prob)/mean(mean.burn.prob), 
            adj.fi.cv = sd(adj.fi)/mean(adj.fi), 
            mean.fm.cv = sd(mean.fm)/mean(mean.fm), 
            veg.simpsons.cv = sd(veg.simpsons.div)/mean(veg.simpsons.div), 
            veg.morans.cv = sd(veg.morans.i)/mean(veg.morans.i)) %>%
  pivot_longer(cols = c("pop.count.cv", "veg_0.cv", "mean.burn.prob.cv", 
                        "adj.fi.cv", "mean.fm.cv", "veg.simpsons.cv", "veg.morans.cv"), 
               names_to = "CV", values_to = "value")

ggplot(var.data %>% filter(ticks > 0)) +
  geom_boxplot(aes(x = ticks, y = value, group = ticks)) +
  facet_grid(`cycle-duration`~ CV, scales = "free")

var.data.wide = data %>% group_by_at(c("ticks", parameters)) %>%
  summarize(pop.count.cv = sd(pop.count)/mean(pop.count), 
            veg_0.cv = sd(veg_0)/mean(veg_0), 
            mean.burn.prob.cv = sd(mean.burn.prob)/mean(mean.burn.prob), 
            adj.fi.cv = sd(adj.fi)/mean(adj.fi), 
            mean.fm.cv = sd(mean.fm)/mean(mean.fm), 
            veg.simpsons.cv = sd(veg.simpsons.div)/mean(veg.simpsons.div), 
            veg.morans.cv = sd(veg.morans.i)/mean(veg.morans.i))
var.data.wide$`veg-distribution` = as.numeric(as.factor(var.data.wide$`veg-distribution`)) #clustered = 1, random = 2
var.data.wide$`veg-cycle-start` = as.numeric(as.factor(var.data.wide$`veg-cycle-start`)) #productive = 1, unproductive = 2
var.data.wide$`movement-model` = as.numeric(as.factor(var.data.wide$`movement-model`)) #Directed = 1, Random = 2
#not sure modeling them as a 1 and a 2 really is correct

hist(log(var.data.wide$pop.count.cv))
fit.pop.cv = lm(pop.count.cv ~ ., data = var.data.wide %>% dplyr::select_at(c("pop.count.cv", "ticks", parameters)))
summary(fit.pop.cv)
lm.beta(fit.pop.cv)

hist(var.data.wide$mean.burn.prob.cv)
fit.mbp.cv = lm(mean.burn.prob.cv ~ ., data = var.data.wide %>% dplyr::select_at(c("mean.burn.prob.cv", "ticks", parameters)))
lm.beta(fit.mbp.cv)

test = lapply(var.data.wide[,c(parameters)], scale)

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
  
