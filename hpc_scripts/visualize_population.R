library(tidyverse)
library(rcompanion)
library(fitdistrplus)
library(sp)
library(ggspatial)
library(spdep)
library(ggpubr)

data = readRDS("results/bb-tl-data.rds")
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

grid.data = data %>% select_at(c("exp", "x", "y", "ticks", "turtle_count", parameters)) %>%
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
  filter(movement_model == "\"Directed Walk\"") %>%
  filter(burnt_neighbor_limit != 1) %>%
  filter(burn_cost == 100)

# viz.100.exp = viz.100 %>% filter(exp == "0.43236824148938036")
# #possiblities
# #	0.7517798120598171
# # 0.15933762127450224
# # 0.5045667019271891
# print(head(viz.100.exp))

plot.list = list()
for(e in unique(viz.100$exp)){
  for(t in c(2000, 3500)) {
    grid = viz.100 %>% filter(exp == e) %>% filter(ticks == t)
    coordinates(grid) = ~y+x
    gridded(grid) = TRUE
    grid = as(grid, "SpatialPolygonsDataFrame")
    spplot(grid, c("turtle_count"))
    
    nb = poly2nb(grid, queen = T)
    lw = nb2listw(nb, zero.policy = T)
    moran.i = moran.mc(grid$turtle_count, lw, nsim = 999, alternative = "greater")$statistic
    p.value = moran.mc(grid$turtle_count, lw, nsim = 999, alternative = "greater")$p.value
    
    plot = ggplot(st_as_sf(grid)) +
      geom_sf(aes(fill = turtle_count)) +
      theme_minimal() +
      scale_fill_viridis_c() +
      labs(fill = "turtle count", title = paste(t, "ticks")) +
      annotate("rect", xmin = 13, xmax = 25, ymin = 21, ymax = 25, fill = "white") +
      annotate("text", x = 19, y = 24, label = paste0("Z = ", round(moran.i, digits = 4))) +
      annotate("text", x = 19, y = 22, label = paste0("p = ", round(p.value, digits = 4))) +
      theme(axis.title = element_blank(), plot.title = element_text(hjust = 0.5), axis.text = element_text(size = 5))
    
    plot.list[[paste0(e, "_", t)]] <- plot
  }
}

write_rds(plot.list, file = "results/outputs/pop_plots.rds")

# ggsave(filename = "preliminary_figures/population_clustering.png", 
#        plot = ggarrange(plotlist = plot.list, ncol = 3, nrow = 1, legend = "none"), 
#        width = 12, height = 5, dpi = 300
# )

#### plotting off the HPC ####
parameters = c(
  "cycle-duration",  # 100, 250
  "veg-cycle-start", # productive, unproductive
  "veg-distribution", # random, clustered
  "burnt-neighbor-limit", # 1, 4, 8
  "burn-cost", # 0, 50
  "burn-veg-type-threshold", # 4, 7
  "movement-model" #Random, Directed
)

pop_plots <- readRDS("~/Desktop/Yale/ABM-development/ForageMod/results/outputs/pop_plots.rds")
source("analysis_scripts/filter-out-unsuccessful-runs.R")
names(pop_plots)

plot(pop_plots[[2]])
as.vector(data %>% filter(exp == "0.00444187064819368") %>% select_at(parameters) %>% distinct())

plot(pop_plots[[44]])
