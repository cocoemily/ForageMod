library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(ggpubr)
library(vegan)
library(jtools)
theme_set(theme_bw())

data = readRDS("results/bb-vt-data.rds")

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

grid.data = data[,c("x", "y", "ticks", "veg.type", parameters)]
grid.data = grid.data  %>% group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
rm(list = c("data"))

grid.data.100.prod = grid.data %>% 
  filter(`cycle-duration` == 100) %>% 
  filter(`veg-cycle-start` == "\"productive\"") %>% 
  group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
grid.data.100.unprod = grid.data %>% 
  filter(`cycle-duration` == 100) %>% 
  filter(`veg-cycle-start` == "\"unproductive\"") %>% 
  group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())

grid.data.250.prod = grid.data %>% 
  filter(`cycle-duration` == 250) %>% 
  filter(`veg-cycle-start` == "\"productive\"") %>% 
  group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())
grid.data.250.unprod = grid.data %>% 
  filter(`cycle-duration` == 250) %>% 
  filter(`veg-cycle-start` == "\"unproductive\"") %>% 
  group_by_at(c(parameters)) %>% mutate(exp.group = cur_group_id())

rm(list = c("grid.data"))

#### get data for cycle duration = 100 & productive start ####
veg.data = list()
for(exp in unique(grid.data.100.prod$exp.group)) {
  tick.grid = grid.data.100.prod %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    run.grid$climate.condition = "productive"
    cc = "productive"
    
    #for(t in c(50, 150, 250, 350)) {
    for(t in c(50, 150, 250, 350, 450, 550, 650,
               750, 850, 950, 1050, 1150, 1250, 1350,
               1450, 1550, 1650, 1750, 1850, 1950)) {
      grid = run.grid %>% filter(ticks == t)
      coordinates(grid) = ~y+x
      gridded(grid) = TRUE
      grid = as(grid, "SpatialPolygonsDataFrame")
      #spplot(grid, c("times.burned"))
      
      nb = poly2nb(grid, queen = T)
      lw = nb2listw(nb, zero.policy = T)
      #grid$Gi.stat = localG_perm(grid$times.burned, lw, nsim = 100, zero.policy = T)
      #spplot(grid, c("Gi.stat"))
      
      # if(moran(grid$veg.type, lw, length(nb), Szero(lw))$I > 0){
      #   MC <- moran.mc(grid$veg.type, lw, nsim = 999, alternative = "greater")
      # } else {
      #   MC <- moran.mc(grid$veg.type, lw, nsim = 999, alternative = "less")
      # }
      MC <- moran.mc(grid$veg.type, lw, nsim=99, alternative="two.sided")
      
      counts = as.data.frame(grid@data) %>% count(veg.type)
      div = diversity(counts$n, index = "shannon")
      
      #create data frame 
      output.data = as.data.frame(grid@data) %>% 
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value, 
               shannon.div = div)
      output.data$climate.condition = cc
      cc = ifelse(cc == "productive", "unproductive", "productive")
      
      veg.data[[length(veg.data) + 1]] <- output.data
    }
  }
}

veg.100.prod = do.call("rbind", veg.data[1:length(veg.data)])
veg.100.prod = veg.100.prod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.100.prod, file = "outputs/veg_moransi_100-prod.rds")

#### get data for cycle duration = 100 & unproductive start ####
veg.data = list()
for(exp in unique(grid.data.100.unprod$exp.group)) {
  tick.grid = grid.data.100.unprod %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    run.grid$climate.condition = "unproductive"
    cc = "unproductive"
    
    #for(t in c(50, 150, 250, 350)) {
    for(t in c(50, 150, 250, 350, 450, 550, 650,
               750, 850, 950, 1050, 1150, 1250, 1350,
               1450, 1550, 1650, 1750, 1850, 1950)) {
      grid = run.grid %>% filter(ticks == t)
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
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value, 
               shannon.div = div)
      output.data$climate.condition = cc
      cc = ifelse(cc == "productive", "unproductive", "productive")
      
      veg.data[[length(veg.data) + 1]] <- output.data
    }
  }
}

veg.100.unprod = do.call("rbind", veg.data[1:length(veg.data)])
veg.100.unprod = veg.100.unprod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.100.unprod, file = "outputs/veg_moransi_100-unprod.rds")

#### get data for cycle duration = 250 & productive start ####
veg.data = list()
for(exp in unique(grid.data.250.prod$exp.group)) {
  tick.grid = grid.data.250.prod %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    run.grid$climate.condition = "productive"
    cc = "productive"
    
    #for(t in c(50, 150, 250, 350)) {
    #for(t in c(150, 400, 650, 900, 1150, 1400, 1650, 1900)) {
    for(t in c(100, 200, 350, 450, 600, 700, 850, 950, 
               1100, 1200, 1350, 1450, 1600, 1700, 
               1850, 1950)) {
      grid = run.grid %>% filter(ticks == t)
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
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value, 
               shannon.div = div)
      output.data$climate.condition = cc
      cc = ifelse(cc == "productive", "unproductive", "productive")
      
      veg.data[[length(veg.data) + 1]] <- output.data
    }
  }
}

veg.250.prod = do.call("rbind", veg.data[1:length(veg.data)])
veg.250.prod = veg.250.prod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.250.prod, file = "outputs/veg_moransi_250-prod.rds")

#### get data for cycle duration = 250 & unproductive start ####
veg.data = list()
for(exp in unique(grid.data.250.unprod$exp.group)) {
  tick.grid = grid.data.250.unprod %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    run.grid$climate.condition = "unproductive"
    cc = "unproductive"
    
    #for(t in c(50, 150, 250, 350)) {
    #for(t in c(150, 400, 650, 900, 1150, 1400, 1650, 1900)) {
    for(t in c(100, 200, 350, 450, 600, 700, 850, 950, 
               1100, 1200, 1350, 1450, 1600, 1700, 
               1850, 1950)) {
      grid = run.grid %>% filter(ticks == t)
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
        dplyr::select_at(c("ticks", parameters, "exp.group", "run")) %>%
        first() %>%
        mutate(morans.i = MC$statistic, 
               p.value = MC$p.value, 
               shannon.div = div)
      output.data$climate.condition = cc
      cc = ifelse(cc == "productive", "unproductive", "productive")
      
      veg.data[[length(veg.data) + 1]] <- output.data
    }
  }
}

veg.250.unprod = do.call("rbind", veg.data[1:length(veg.data)])
veg.250.unprod = veg.250.prod %>% mutate(signif = ifelse(p.value < 0.05, T, F))
write_rds(veg.250.unprod, file = "outputs/veg_moransi_250-unprod.rds")