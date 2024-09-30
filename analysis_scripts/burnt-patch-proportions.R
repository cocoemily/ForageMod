library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(QuantPsyc)
library(betareg)
library(ggpubr)
library(jtools)
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
data$benefit.ratio = data$benefit_self/data$benefit_other

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

data$burnt = data$`veg_-1`

plotNormalHistogram(data$burnt)
descdist(data$burnt)

ggplot(data) +
  geom_density(aes(x = burnt, color = `natural-ignition`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `cycle-duration`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `veg-cycle-start`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `veg-distribution`))

ggplot(data) +
  geom_density(aes(x = burnt, color = `burnt-neighbor-limit`))
#bimodal distribution is driven by burnt neighbor limit

ggplot(data) +
  geom_density(aes(x = burnt, color = `burn-cost`))
#decreasing burn cost shifts the distribution right

ggplot(data) +
  geom_density(aes(x = burnt, color = `burn-veg-type-threshold`))
#increasing veg type threshold shifts the distribution right

ggplot(data) +
  geom_density(aes(x = burnt, color = `movement-model`))



plot.data = data %>% select_at(c("ticks", "burnt", parameters)) %>%
  group_by_at(c(parameters)) %>%
  mutate(high.burnt = mean(burnt) + sd(burnt), 
         low.burnt = mean(burnt) - sd(burnt)) %>%
  filter(`burn-cost` %in% c(0, 150))

bt.labs = c("can burn veg types 1-4", "can burn veg types 1-7")
names(bt.labs) = c(4, 7)
bc.labs = c("no burn cost (0)", "high burn cost (150)")
names(bc.labs) = c(0, 150)

bprop.plot = ggplot(plot.data) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 8), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 4), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 1), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_smooth(aes(x = ticks, y = burnt, color = `burnt-neighbor-limit`), se = F) +
  geom_hline(yintercept = 1.0, linetype = "dotted") +
  facet_grid(`burn-cost` ~ `burn-veg-type-threshold`, labeller = labeller(
    `burn-veg-type-threshold` = bt.labs, 
    `burn-cost` = bc.labs
  )) +
  labs(y = "proportion of patches that are burnt") +
  scale_color_brewer(palette = "Set1",
                     labels = c("can burn patch with 8 burnt adjacent neighbors", 
                                "can burn patch with 4 burnt adjacent neighbors", 
                                "can burn patch with 1 burnt adjacent neighbor")) +
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 7))
ggsave(filename = "preliminary_figures/proportion-burnt-landscape.png", plot = bprop.plot, 
       dpi = 300, width = 8, height = 6)


beta.data = data %>% mutate(burnt = ifelse(burnt == 0, burnt + 0.00001, burnt))

burnt.burnn1 = beta.data %>% filter(`burnt-neighbor-limit` == 1)
# plotNormalHistogram(veg_0.burnn1$veg_0)
# descdist(veg_0.burnn1$veg_0)
burnn1.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

burnt.burnn1 = data %>% filter(`burnt-neighbor-limit` == 1)
burnn1.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn1.fit1)
AIC(burnn1.fit2)

burnt.burnn4 = beta.data %>% filter(`burnt-neighbor-limit` == 4)
# plotNormalHistogram(veg_0.burnn4$veg_0)
# descdist(veg_0.burnn4$veg_0, discrete = F)
burnn4.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn4 = data %>% filter(`burnt-neighbor-limit` == 4)
burnn4.fit2 = lm(burnt ~ ticks*(.), data =  burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn4.fit1)
AIC(burnn4.fit2)

burnt.burnn8 = beta.data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn8 = data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn8.fit1)
AIC(burnn8.fit2)

plot_summs(burnn1.fit2, burnn4.fit2, burnn8.fit2, scale = T, digits = 6,
           model.names = c("BNL = 1", "BNL = 4", "BNL = 8"))


#### plot all burning behaviors ####
mbp.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob, group = exp, color = `burn-cost`), alpha = 0.25) +
  geom_smooth(aes(x = ticks, y = mean.burn.prob), se = F, color = "red") +
  scale_color_manual(values = c("grey0", "grey30", "grey60", "grey80")) +
  labs(y = "mean probability of burning", x = "ticks", color = "cost of burning") +
  theme(legend.position = "bottom", axis.title = element_text(size = 5.5))

br.plot2 = ggplot(data) +
  geom_smooth(aes(x = ticks, y = benefit.ratio, group = exp, color = `burn-cost`), alpha = 0.25) +
  geom_smooth(aes(x = ticks, y = benefit.ratio)) +
  scale_color_manual(values = c("grey0", "grey30", "grey60", "grey80")) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "ticks", y = "ratio of self benefit to other benefit") +
  theme(legend.position = "bottom", axis.title = element_text(size = 5.5))

img = png::readPNG("preliminary_figures/ForageModv02_view.png")
img.in = grid::rasterGrob(img, interpolate = T)
img.plot = ggplot()+
  geom_blank()+
  annotation_custom(img.in, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_minimal()

bprop.plot2 = ggplot(plot.data) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 8), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 4), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_point(data = plot.data %>% filter(`burnt-neighbor-limit` == 1), mapping = 
               aes(x = ticks, y = burnt, group = ticks, color = `burnt-neighbor-limit`), alpha = 0.01, size = 0.3) +
  geom_smooth(aes(x = ticks, y = burnt, color = `burnt-neighbor-limit`), se = F) +
  geom_hline(yintercept = 1.0, linetype = "dotted") +
  facet_wrap(`burn-veg-type-threshold` ~ `burn-cost`, labeller = labeller(
    `burn-veg-type-threshold` = bt.labs, 
    `burn-cost` = bc.labs
  )) +
  labs(y = "proportion of patches that are burnt") +
  scale_color_brewer(palette = "Set1",
                     labels = c("can burn patch with 8 burnt adjacent neighbors", 
                                "can burn patch with 4 burnt adjacent neighbors", 
                                "can burn patch with 1 burnt adjacent neighbor")) +
  theme(legend.title = element_blank(), legend.position = "bottom", 
        legend.text = element_text(size = 7), 
        strip.text = element_text(size = 7), axis.title = element_text(size = 7))

all.plot = ggarrange(
  ggarrange(
    ggarrange(mbp.plot2, br.plot2,
              common.legend = T, legend = "top", labels = "AUTO", 
              nrow = 1) + theme(legend.title = element_text(size = 7)), 
    img.plot, nrow = 1, labels = c("", "C"), widths = c(2,1)), 
  bprop.plot2, nrow = 2, labels = c("", "D")
)

ggsave(filename = "figures/burning-behaviors-plot.png", plot = all.plot,
       dpi = 300, width = 8, height = 7)
