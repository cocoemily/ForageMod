library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)


data = readRDS("results/bb-hb-data.rds")

#hist(data$times.burned)

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

grid.data = data[,c("x", "y", "ticks", "times.burned", parameters)]
grid.data = grid.data  %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
rm(list = c("data"))

grid.data.100 = grid.data %>% filter(`cycle-duration` == 100)
grid.data.250 = grid.data %>% filter(`cycle-duration` == 250)
rm(list = c("grid.data"))

grid.data.100 = grid.data.100 %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
grid.data.250 = grid.data.250 %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())


#### Global Moran's I, cycle duration = 100 ####
morans.data = list()
exp.list = unique(grid.data.100$exp.group)[1]

#for(exp in exp.list) { #for testing
for(exp in unique(grid.data.100$exp.group)) {
  tick.grid = grid.data.100 %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    
    for(t in c(250, 500, 1000, 1500, 2000)) {
      #for(t in unique(run.grid$ticks)) {
      grid = run.grid %>% filter(ticks == t)
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      #spplot(grid, c("times.burned"))
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      #grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      if(moran(grid$times.burned, lw, length(nb), Szero(lw))$I > 0){
        MC <- moran.mc(grid$times.burned, lw, nsim = 999, alternative = "greater")
      } else {
        MC <- moran.mc(grid$times.burned, lw, nsim = 999, alternative = "less")
      }
      
      #create data frame 
      output.data = as.data.frame(grid@data) %>% 
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value)
      
      morans.data[[length(morans.data) + 1]] <- output.data
    }
  }
}

all.morans.100 = do.call("rbind", morans.data[1:length(morans.data)])

all.morans.100 = all.morans.100 %>%
  mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(all.morans.100, file = "outputs/morans.i_cycle.100.rds")

#### Global Moran's I, cycle duration = 250 ####
morans.data = list()
exp.list = unique(grid.data.250$exp.group)[1]

#for(exp in exp.list) { #for testing
for(exp in unique(grid.data.250$exp.group)) {
  tick.grid = grid.data.250 %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    
    for(t in c(250, 500, 1000, 1500, 2000)) {
      #for(t in unique(run.grid$ticks)) {
      grid = run.grid %>% filter(ticks == t)
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      #spplot(grid, c("times.burned"))
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      #grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      if(moran(grid$times.burned, lw, length(nb), Szero(lw))$I > 0){
        MC <- moran.mc(grid$times.burned, lw, nsim = 999, alternative = "greater")
      } else {
        MC <- moran.mc(grid$times.burned, lw, nsim = 999, alternative = "less")
      }
      
      #create data frame 
      output.data = as.data.frame(grid@data) %>% 
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value)
      
      morans.data[[length(morans.data) + 1]] <- output.data
    }
  }
}

all.morans.250 = do.call("rbind", morans.data[1:length(morans.data)])
all.morans.250 = all.morans.250 %>%
  mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(all.morans.250, file = "outputs/morans.i_cycle.250.rds")