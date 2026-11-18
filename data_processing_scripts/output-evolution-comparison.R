#comparing rates of change with other outputs
library(tidyverse)
library(here)
library(foreach)
library(doParallel)

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
data$unforageable = data$veg_0 + data$burnt

data$bself.prop = data$benefit_self / ((data$pop.density * 51 * 51) * data$mean.fm) #proportion of average total foraging events that result in a benefit to self
data$bother.prop = data$benefit_other / ((data$pop.density * 51 * 51) * data$mean.fm) #proportion of average total foraging events that result in a benefit to self

experiments = unique(data$exp)
exp_list = split(data, data$exp)

num_cores <- detectCores() - 8
cl <- makeCluster(num_cores)
registerDoParallel(cl)

final.data <- foreach(
  x = exp_list, 
  .packages = c("tidyverse"),
  .combine = bind_rows,
  .errorhandling = "stop"
) %dopar% {
  
  tick.seq = NULL
  if(first(x$`cycle-duration`) == 250) {
    tick.seq = seq(from = 0, to = 4000, by = 250)
  } else {
    tick.seq = seq(from = 0, to = 4000, by = 100)
  }
  
  tick_results <- map_dfr(1:(length(tick.seq) - 1), function(i) {
    lmdata = x %>% filter(ticks > tick.seq[i] & ticks <= tick.seq[i + 1])
    
    if(tick.seq[i] == 0) {lmdata = x %>% filter(ticks > (tick.seq[i] + 1) & ticks <= tick.seq[i + 1])}
    
    if(nrow(lmdata) < 2) return(NULL)
    
    bp.fit = lm(mean.burn.prob ~ ticks, data = lmdata)
    pd.fit = lm(pop.density ~ ticks, data = lmdata)
    mv.fit = lm(mean.fm ~ ticks, data = lmdata)
    int.fit = lm(mean.fi ~ ticks, data = lmdata)
    sint.fit = lm(mean.same.fi ~ ticks, data = lmdata)
    br1.fit = lm(bself.prop ~ ticks, data = lmdata)
    br2.fit = lm(bother.prop ~ ticks, data = lmdata)
    sim.fit = lm(net.sim ~ ticks, data = lmdata)
    dist.fit = lm(net.clust ~ ticks, data = lmdata)
    
    data.frame(
      exp = first(x$exp), 
      tick_range = paste0(tick.seq[i] + 1, "-", tick.seq[i+1]),
      climate.condition = first(lmdata$climate.condition),
      start.burn.prob = summary(bp.fit)$coefficients[1,1] + (tick.seq[i] * summary(bp.fit)$coefficients[2,1]),
      end.burn.prob = summary(bp.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(bp.fit)$coefficients[2,1]),
      slope.burn.prob = summary(bp.fit)$coefficients[2,1],
      start.pop.density = summary(pd.fit)$coefficients[1,1] + (tick.seq[i] * summary(pd.fit)$coefficients[2,1]),
      end.pop.density = summary(pd.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(pd.fit)$coefficients[2,1]),
      slope.pop.density = summary(pd.fit)$coefficients[2,1],
      start.steps = summary(mv.fit)$coefficients[1,1] + (tick.seq[i] * summary(mv.fit)$coefficients[2,1]),
      end.steps = summary(mv.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(mv.fit)$coefficients[2,1]),
      slope.steps = summary(mv.fit)$coefficients[2,1],
      start.interactions = summary(int.fit)$coefficients[1,1] + (tick.seq[i] * summary(int.fit)$coefficients[2,1]),
      end.interactions = summary(int.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(int.fit)$coefficients[2,1]),
      slope.interactions = summary(int.fit)$coefficients[2,1],
      start.same.interactions = summary(sint.fit)$coefficients[1,1] + (tick.seq[i] * summary(sint.fit)$coefficients[2,1]),
      end.same.interactions = summary(sint.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(sint.fit)$coefficients[2,1]),
      slope.same.interactions = summary(sint.fit)$coefficients[2,1],
      avg.prop.unforageable = mean(lmdata$unforageable),
      sd.prop.unforageable = sd(lmdata$unforageable),
      start.bself.prop = summary(br1.fit)$coefficients[1,1] + (tick.seq[i] * summary(br1.fit)$coefficients[2,1]),
      end.bself.prop = summary(br1.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(br1.fit)$coefficients[2,1]),
      slope.bself.prop = summary(br1.fit)$coefficients[2,1],
      start.bother.prop = summary(br2.fit)$coefficients[1,1] + (tick.seq[i] * summary(br2.fit)$coefficients[2,1]), 
      end.bother.prop = summary(br2.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(br2.fit)$coefficients[2,1]),
      slope.bother.prop = summary(br2.fit)$coefficients[2,1],
      start.net.sim = summary(sim.fit)$coefficients[1,1] + (tick.seq[i] * summary(sim.fit)$coefficients[2,1]),
      end.net.sim = summary(sim.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(sim.fit)$coefficients[2,1]),
      slope.net.sim = summary(sim.fit)$coefficients[2,1],
      start.net.dist = summary(dist.fit)$coefficients[1,1] + (tick.seq[i] * summary(dist.fit)$coefficients[2,1]),
      end.net.dist = summary(dist.fit)$coefficients[1,1] + (tick.seq[i + 1] * summary(dist.fit)$coefficients[2,1]),
      slope.net.dist = summary(dist.fit)$coefficients[2,1]
    )
  })
  return(tick_results)
}

stopCluster(cl)

write_rds(final.data, file = "results/outputs/output-evolution-comparison.rds")


