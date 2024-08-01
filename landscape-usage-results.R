library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(ggpubr)

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


#### Global Moran's I, cycle duration = 100 ####
morans.data = list()
exp.list = unique(grid.data.100$exp.group)[1]

#for(exp in exp.list) { #for testing
for(exp in unique(grid.data.100$exp.group)) {
  tick.grid = grid.data.100 %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    
    #for(t in c(250, 500, 1000, 1500, 2000)) {
    for(t in unique(run.grid$ticks)) {
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
write_rds(all.morans.100, file = "results/outputs/morans.i_cycle.100.rds")

theme_set(theme_bw())
ggplot(all.morans.100 %>% filter(signif == T)) +
  geom_point(mapping = aes(x = ticks, y = morans.i, shape = signif)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i, color = as.factor(`burnt-neighbor-limit`), group = as.factor(`burnt-neighbor-limit`)), method = "lm") +
  facet_grid(`burn-veg-type-threshold`~`movement-model` + `veg-distribution`, scales = "free")

hist(all.morans.100$morans.i)
fit1 = lm(morans.i ~ ticks*(.), data = all.morans.100 %>% select_at(c("ticks", parameters, "morans.i")))
summary(fit1)

fit.log = glm(signif ~ ticks*(.), data = all.morans.100 %>% select_at(c("ticks", parameters, "signif")), family = binomial())
summary(fit.log)

##### visualization -- clustering #####
viz.100 = grid.data.100 %>% 
  filter(`movement-model` == "\"Random Walk\"") %>%
  filter(`veg-distribution` == "\"clustered\"") %>%
  filter(`burnt-neighbor-limit` == 8) %>%
  filter(`burn-veg-type-threshold` == 7)

unique(viz.100$exp.group)
viz.100.exp = viz.100 %>% filter(exp.group == 24)
viz.100.exp$run = rep(1:5, each = (51*51*length(unique(viz.100.exp$ticks))) - 1)
viz.100.run = viz.100.exp %>% filter(run == 3)

plot.list = list()
for(t in c(250, 1000, 2000)) {
  grid = viz.100.run %>% filter(ticks == t)
  coordinates(grid) = ~y+x
  gridded(grid) = TRUE
  grid = as(grid, "SpatialPolygonsDataFrame")
  spplot(grid, c("times.burned"))
  
  nb = poly2nb(grid, queen = T)
  lw = nb2listw(nb, zero.policy = T)
  moran.i = moran.mc(grid$times.burned, lw, nsim = 999, alternative = "greater")$statistic
  p.value = moran.mc(grid$times.burned, lw, nsim = 999, alternative = "greater")$p.value
  
  plot = ggplot(st_as_sf(grid)) +
    geom_sf(aes(fill = times.burned)) +
    theme_minimal() +
    scale_fill_viridis_c() +
    labs(fill = "burn event count", title = paste(t, "ticks")) +
    annotate("rect", xmin = 13, xmax = 25, ymin = 21, ymax = 25, fill = "white") +
    annotate("text", x = 19, y = 24, label = paste0("Z = ", round(moran.i, digits = 4))) +
    annotate("text", x = 19, y = 22, label = paste0("p = ", round(p.value, digits = 4))) +
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 5))
  
  plot.list[[length(plot.list) + 1]] <- plot
}
ggsave(filename = "preliminary_figures/landscape_use_clustering.png", 
       plot = ggarrange(plotlist = plot.list, ncol = 3, nrow = 1, legend = "none"), 
       width = 12, height = 5, dpi = 300
)

##### visualization -- dispersion #####
viz.100 = grid.data.100 %>% 
  filter(`movement-model` == "\"Random Walk\"") %>%
  filter(`veg-distribution` == "\"random\"") %>%
  filter(`burnt-neighbor-limit` == 1) %>%
  filter(`burn-veg-type-threshold` == 4)

unique(viz.100$exp.group)
viz.100.exp = viz.100 %>% filter(exp.group == 26)
viz.100.exp$run = rep(1:5, each = (51*51*length(unique(viz.100.exp$ticks))) - 1)
viz.100.run = viz.100.exp %>% filter(run == 2)

plot.list = list()
for(t in c(250, 1000, 2000)) {
  grid = viz.100.run %>% filter(ticks == t)
  coordinates(grid) = ~y+x
  gridded(grid) = TRUE
  grid = as(grid, "SpatialPolygonsDataFrame")
  #spplot(grid, c("times.burned"))
  
  nb = poly2nb(grid, queen = T)
  lw = nb2listw(nb, zero.policy = T)
  moran.i = moran.mc(grid$times.burned, lw, nsim = 999, alternative = "less")$statistic
  p.value = moran.mc(grid$times.burned, lw, nsim = 999, alternative = "less")$p.value
  
  plot = ggplot(st_as_sf(grid)) +
    geom_sf(aes(fill = times.burned)) +
    theme_minimal() +
    scale_fill_viridis_c() +
    labs(fill = "burn event count", title = paste(t, "ticks")) +
    annotate("rect", xmin = 13, xmax = 25, ymin = 21, ymax = 25, fill = "white") +
    annotate("text", x = 19, y = 24, label = paste0("Z = ", round(moran.i, digits = 4))) +
    annotate("text", x = 19, y = 22, label = paste0("p = ", round(p.value, digits = 4))) +
    theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 5))
  
  plot.list[[length(plot.list) + 1]] <- plot
}
ggsave(filename = "preliminary_figures/landscape_use_dispersion.png", 
       plot = ggarrange(plotlist = plot.list, ncol = 3, nrow = 1, legend = "none"), 
       width = 12, height = 5, dpi = 300
)


#### Global Moran's I, cycle duration = 250 ####
morans.data = list()
exp.list = unique(grid.data.250$exp.group)[1]

#for(exp in exp.list) { #for testing
for(exp in unique(grid.data.250$exp.group)) {
  tick.grid = grid.data.250 %>% filter(exp.group == exp)
  tick.grid$run = rep(1:5, each = (51*51*length(unique(tick.grid$ticks))) - 1)
  
  for(r in unique(tick.grid$run)) {
    run.grid = tick.grid %>% filter(run == r)
    
    #for(t in c(250, 500, 1000, 1500, 2000)) {
    for(t in unique(run.grid$ticks)) {
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
write_rds(all.morans.250, file = "results/outputs/morans.i_cycle.250.rds")

theme_set(theme_bw())
ggplot(all.morans.250 %>% filter(signif == T)) +
  geom_point(mapping = aes(x = ticks, y = morans.i, shape = signif)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i, color = as.factor(`burnt-neighbor-limit`), group = as.factor(`burnt-neighbor-limit`)), method = "lm") +
  facet_grid(`burn-veg-type-threshold`~`movement-model` + `veg-distribution`, scales = "free")
