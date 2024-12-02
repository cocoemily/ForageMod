library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(vegan)

theme_set(theme_bw())

data = readRDS("results/bb-vt-data.rds")
#colnames(data)

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

grid.data = data %>% select_at(c("exp", "x", "y", "ticks", "veg.type", parameters)) %>%
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

grid.data.100.prod = grid.data %>% 
  filter(cycle_duration == 100) %>% 
  filter(veg_cycle_start == "\"productive\"") 
print("processing cycle = 100 & start = productive")
rm(list = c("grid.data"))

#### get data for cycle duration = 100 & productive start ####
veg.data = list()
for(e in unique(grid.data.100.prod$exp)) {
  print(e)
  run.grid = grid.data.100.prod %>% filter(exp == e)
  print(nrow(run.grid))
  print(length(unique(run.grid$ticks)))
  
  run.grid$climate.condition = "productive"
  cc = "productive"
  
  #for(t in c(50, 150, 250, 350)) {
  for(t in c(50, 150, 250, 350, 450, 550, 650,
             750, 850, 950, 1000, 1050, 1150, 1250, 1350,
             1450, 1550, 1650, 1750, 1850, 1950, 2000, 2050, 
             2150, 2250, 2350, 2450, 2550, 2650, 2750, 2850, 2950, 
             3000, 3050, 3150, 3250, 3350, 3450, 3500)) {
    grid = run.grid %>% filter(ticks == t)
    
    if(nrow(grid) > 0){
      if(nrow(grid) == 51*51*2) { #dealing with the case where model ends at a recording tick
        grid = grid[1:2601,]
      }
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      #grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      grid$veg.type = grid$veg.type + 1
      
      MC <- moran.mc(grid$veg.type, lw, nsim=99, alternative="two.sided")
      
      counts = as.data.frame(grid@data) %>% count(veg.type)
      #print(counts)
      div = diversity(counts$n, index = "shannon")
      
      counts2 = as.data.frame(grid@data) %>% filter(veg.type != 0) %>% count(veg.type)
      #print(counts)
      div2 = diversity(counts2$n, index = "shannon")
      
      #create data frame
      output.data = as.data.frame(grid@data) %>%
        dplyr::select_at(c("ticks", "exp", parameters)) %>%
        first() %>%
        mutate(morans.i = MC$statistic,
               p.value = MC$p.value,
               shannon.div = div, 
               shannon.div.no0 = div2)
      
    } else {
      output.data = run.grid %>% 
        dplyr::select_at(c("ticks", "exp", parameters)) %>%
        first() %>%
        mutate(ticks = t,
               morans.i = NA, 
               p.value = NA, 
               shannon.div = NA, 
               shannon.div.no0 = NA)
    }
    
    output.data$climate.condition = cc
    cc = ifelse(cc == "productive", "unproductive", "productive")
    
    veg.data[[length(veg.data) + 1]] <- output.data
  }
}

veg.100.prod = do.call("rbind", veg.data[1:length(veg.data)])
veg.100.prod = veg.100.prod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.100.prod, file = "results/outputs/veg_moransi_100-prod.rds")
