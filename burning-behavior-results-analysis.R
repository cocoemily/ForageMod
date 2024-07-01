library(tidyverse)
theme_set(theme_bw())

source("read-data.R")

# final.data = `prelim-bb-data`
# rm(`prelim-bb-data`)

parameters = c(
  "`natural-ignition`", # 0.00, 0.05
  "`cycle-duration`",  # 100, 2501
  "`burnt-neighbor-limit`", # 1, 5, 8
  "`burn-cost`", # 0, 50
  "`veg-distribution`", # random, clustered
  "`burn-veg-type-threshold`" # 4, 7
)

outputs = c(
  "pop.count", 
  "veg_0", 
  "mean.burn.prob", 
  "benefit_self", 
  "benefit_other", 
  "mean.fi", #average forager interaction count
  "mean.fm" #average forager movements per capita
)
  
finished = final.data %>% filter(ticks == 2000)
length(unique(finished$exp))

#### Mean forager burning probability ####
ggplot(final.data) +
  geom_line(aes(x = ticks, 
                y = mean.burn.prob)) +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)

#### Proportion of landscape with veg-type = 0 (burnt) ####
ggplot(final.data) +
  geom_line(aes(x = ticks, 
                y = veg_0)) + 
  geom_smooth(aes(x = ticks, y = veg_0), method = "lm") +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)

ggplot(final.data %>% filter(`cycle-duration` == 100)) +
  geom_line(aes(x = ticks, 
                y = veg_0)) + 
  geom_smooth(aes(x = ticks, y = veg_0), method = "lm") +
  facet_grid(`burn-cost` ~ `burnt-neighbor-limit` + `burn-veg-type-threshold`, 
             labeller = label_both)

#### Average forager interactions ####
ggplot(final.data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, 
                y = mean.fi/pop.count))  +
  geom_smooth(aes(x = ticks, y = mean.fi/pop.count), method = "lm") +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)

#### Average forager movements ####
ggplot(final.data %>% filter(ticks > 0)) +
  geom_line(aes(x = ticks, 
                y = mean.fm))  +
  geom_smooth(aes(x = ticks, y = mean.fm), method = "lm") +
  facet_grid(`veg-distribution`~`cycle-duration`, labeller = label_both)
