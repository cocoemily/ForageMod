library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)

data = readRDS("results/bb-hb-data.rds")
ncol(data)
columns = colnames(data)
columns[4] = "ticks"
colnames(data) <- columns
colnames(data)

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
rm(list = c("data"))

grid.data.100 = grid.data %>% filter(`cycle-duration` == 100)
grid.data.250 = grid.data %>% filter(`cycle-duration` == 250)
rm(list = c("grid.data"))

grid.data.100 = grid.data.100 %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
grid.data.250 = grid.data.250 %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())


hotspot.data = list()

for(exp in unique(grid.data.100$exp.group)) {
  tick.grid = grid.data.100 %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in tick.grid$run) {
    run.grid = tick.grid %>% filter(run == r)
    
    for(t in unique(run.grid$ticks)) {
      grid = run.grid %>% filter(ticks == t)
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      #spplot(grid, c("times.burned"))
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      hotspot.data[[length(hotspot.data) + 1]] <- grid
    }
  }
}

