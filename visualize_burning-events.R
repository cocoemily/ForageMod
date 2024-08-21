library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)

data = readRDS("results/bb-hb-data.rds")
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

grid.data.100 = grid.data %>% filter(cycle_duration == 100)
rm(list = c("grid.data"))

##### visualization -- clustering #####
viz.100 = grid.data.100 %>% 
  filter(movement_model == "\"Random Walk\"") %>%
  filter(veg_distribution == "\"clustered\"") %>%
  filter(burnt_neighbor_limit == 8) %>%
  filter(burn_veg_type_threshold == 7)

viz.100.exp = viz.100 %>% filter(exp == "0.7374532202200634")
print(head(viz.100.exp))

plot.list = list()
for(t in c(250, 1000, 2000)) {
  grid = viz.100.exp %>% filter(ticks == t)
  coordinates(grid) = ~y+x
  gridded(grid) = TRUE
  grid = as(grid, "SpatialPolygonsDataFrame")
  #spplot(grid, c("times.burned"))
  
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
  filter(movement_model == "\"Random Walk\"") %>%
  filter(veg_distribution == "\"random\"") %>%
  filter(burnt_neighbor_limit == 1) %>%
  filter(burn_veg_type_threshold == 4)

viz.100.exp = viz.100 %>% filter(exp == "0.8411285183431887")
print(head(viz.100.exp))

plot.list = list()
for(t in c(250, 1000, 2000)) {
  grid = viz.100.exp %>% filter(ticks == t)
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
