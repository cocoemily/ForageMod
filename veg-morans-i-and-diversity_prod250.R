library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(vegan)

theme_set(theme_bw())

data = readRDS("bb-vt-data.rds")
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


grid.data.250.prod = grid.data %>% 
  filter(cycle_duration == 250) %>% 
  filter(veg_cycle_start == "\"productive\"") 
print("processing cycle = 250 & start = productive")
rm(list = c("grid.data"))

#### get data for cycle duration = 250 & productive start ####
veg.data = list()
for(e in unique(grid.data.250.prod$exp)) {
  run.grid = grid.data.250.prod %>% filter(exp == e)
  
  run.grid$climate.condition = "productive"
  cc = "productive"
  
  #for(t in c(50, 150, 250, 350)) {
  #for(t in c(150, 400, 650, 900, 1150, 1400, 1650, 1900)) {
  for(t in c(100, 200, 350, 450, 600, 700, 850, 950, 1000,
             1100, 1200, 1350, 1450, 1600, 1700,
             1850, 1950, 2000)) {
    grid = run.grid %>% filter(ticks == t)
    
    if(nrow(grid) > 0){
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      #spplot(grid, c("times.burned"))
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      #grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      MC <- moran.mc(grid$veg.type, lw, nsim=99, alternative="two.sided")
      
      counts = as.data.frame(grid@data) %>% count(veg.type)
      div = diversity(counts$n, index = "shannon")
      
      #create data frame
      output.data = as.data.frame(grid@data) %>%
        dplyr::select_at(c("ticks", "exp", parameters)) %>%
        first() %>%
        mutate(morans.i = MC$statistic,
               p.value = MC$p.value,
               shannon.div = div)
      
    } else {
      output.data = run.grid %>% 
        dplyr::select_at(c("ticks", "exp", parameters)) %>%
        first() %>%
        mutate(ticks = t,
               morans.i = NA, 
               p.value = NA, 
               shannon.div = NA)
    }
    
    output.data$climate.condition = cc
    cc = ifelse(cc == "productive", "unproductive", "productive")
    
    veg.data[[length(veg.data) + 1]] <- output.data
  }
}

veg.250.prod = do.call("rbind", veg.data[1:length(veg.data)])
veg.250.prod = veg.250.prod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.250.prod, file = "outputs/veg_moransi_250-prod.rds")

