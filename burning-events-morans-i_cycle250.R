library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)


data = readRDS("bb-hb-data.rds")
#colnames(data)

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

grid.data = data %>% select_at(c("exp", "x", "y", "ticks", "times.burned", parameters)) %>%
  rename_with(~ tolower(gsub("-", "_", .x, fixed = TRUE)))
rm(list = c("data"))
#colnames(grid.data)

parameters = c(
  "natural_ignition", # 0.00, 0.05
  "cycle_duration",  # 100, 250
  "veg_cycle_start", # productive, unproductive
  "veg_distribution", # random, clustered
  "burnt_neighbor_limit", # 1, 4, 8
  "burn_cost", # 0, 50
  "burn_veg_type_threshold", # 4, 7
  "movement_model" #Random, Directed
)

grid.data.250 = grid.data %>% filter(cycle_duration == 250)
print("processing cycle = 250")
rm(list = c("grid.data"))


#### Global Moran's I, cycle duration = 250 ####
morans.data = list()

#for(exp in exp.list) { #for testing
for(e in unique(grid.data.250$exp)) {
  run.grid = grid.data.250 %>% filter(exp == e)
  
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
      dplyr::select_at(c("ticks", "exp", parameters)) %>%
      first() %>%
      mutate(morans.i = MC$statistic, 
             p.value = MC$p.value)
    
    morans.data[[length(morans.data) + 1]] <- output.data
  }
}

all.morans.250 = do.call("rbind", morans.data[1:length(morans.data)])
all.morans.250 = all.morans.250 %>%
  mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(all.morans.250, file = "outputs/be_morans.i_cycle.250.rds")
