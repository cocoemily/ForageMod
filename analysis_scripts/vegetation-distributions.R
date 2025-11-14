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

parameters = c(
  "cycle_duration",  # 100, 250
  "veg_cycle_start", # productive, unproductive
  "veg_distribution", # random, clustered
  "burnt_neighbor_limit", # 1, 4, 8
  "burn_cost", # 0, 50
  "burn_veg_type_threshold", # 4, 7
  "movement_model" #Random, Directed
)

veg.100.prod = readRDS("results/outputs/veg_moransi_100-prod.rds")
veg.100.unprod = readRDS("results/outputs/veg_moransi_100-unprod.rds")
veg.250.prod = readRDS("results/outputs/veg_moransi_250-prod.rds")
veg.250.unprod = readRDS("results/outputs/veg_moransi_250-unprod.rds")
#veg.data = bind_rows(veg.100.prod, veg.100.unprod, veg.250.prod, veg.250.unprod)

reg.params = parameters[-c(2:3)]

#### cycle duration = 100 & productive start ####
##### morans i #####
hist(veg.100.prod$morans.i)
# veg.100.prod[,reg.params] = lapply(veg.100.prod[,reg.params] , as.factor)
# 
# veg.100.prod.fit1 = lm(morans.i ~ ticks*(.), data = veg.100.prod %>% filter(signif == T) %>% select_at(c("ticks", "morans.i", reg.params)))
# plot_summs(veg.100.prod.fit1, scale = T)

prod100.plot = ggplot(veg.100.prod %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_wrap(~ movement_model) +
  labs(color = "model climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
plot(prod100.plot)

##### shannon diversity #####
hist(veg.100.prod$shannon.div)

ggplot(veg.100.prod) +
  geom_density(aes(x = shannon.div, color = movement_model))

ggplot(veg.100.prod) +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_wrap(~movement_model)


####cycle duration = 100 & unproductive start ####
hist(veg.100.unprod$morans.i)
# veg.100.unprod[,reg.params] = lapply(veg.100.unprod[,reg.params] , as.factor)
# 
# veg.100.unprod.fit1 = lm(morans.i ~ ticks*(.), data = veg.100.unprod %>% filter(signif == T) %>% select_at(c("ticks", "morans.i", reg.params)))
# plot_summs(veg.100.unprod.fit1, scale = T)

unprod100.plot = ggplot(veg.100.unprod %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_wrap(~ movement_model) +
  labs(color = "model climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
plot(unprod100.plot)

##### shannon diversity #####
hist(veg.100.unprod$shannon.div)

ggplot(veg.100.unprod) +
  geom_density(aes(x = shannon.div, color = movement_model))

ggplot(veg.100.unprod) +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_wrap(~movement_model)

####cycle duration = 250 & productive start ####
hist(veg.250.prod$morans.i)

hist(veg.250.prod$shannon.div)
ggplot(veg.250.prod) +
  geom_density(aes(x = shannon.div, color = movement_model))

####cycle duration = 250 & unproductive start ####
hist(veg.250.unprod$morans.i)

hist(veg.250.unprod$shannon.div)
ggplot(veg.250.unprod) +
  geom_density(aes(x = shannon.div, color = movement_model))


####vegetation spatial autocorrelation####
veg.data = bind_rows(veg.100.prod, veg.100.unprod, veg.250.prod, veg.250.unprod) 

#create labels
start.labs = c("start with unproductive vegetation", "start with productive vegetation")
names(start.labs) = c("\"unproductive\"", "\"productive\"")
cycle.labs = c("cycle every 250 ticks", "cycle every 100 ticks")
names(cycle.labs) = c(250, 100)
move.labs = c("random walks", "directed walks")
names(move.labs) = c("\"Random Walk\"", "\"Directed Walk\"")
bc.labs = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")
names(bc.labs) = c(0, 100, 200, 300)
bt.labs = c("can disturb limited resource types", "can disturb all resource types")
names(bt.labs) = c(4, 7)
bn.labs = c("can disturb with 1 disturbed neighbor", "can disturb with 4 disturbed neighbors", "can disturb with 8 disturbed neighbors")
names(bn.labs) = c(1, 4, 8)

veg.cluster.plot = ggplot(veg.data %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = morans.i, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i), method = "lm") +
  facet_grid(movement_model ~ cycle_duration + veg_cycle_start , labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "climate condition", y = "Global Moran's I") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", strip.text = element_text(size = 6.5),)
plot(veg.cluster.plot)

ggsave(filename = "preliminary_figures/veg-type-clustering.png", plot = veg.cluster.plot,
       dpi = 300, width = 8, height = 5.5)

img = png::readPNG("preliminary_figures/ForageModv02_250ticks.png")
img.in = grid::rasterGrob(img, interpolate = T)
img.plot1 = ggplot()+
  geom_blank()+
  annotation_custom(img.in, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_minimal()

img = png::readPNG("preliminary_figures/ForageModv02_1401ticks.png")
img.in = grid::rasterGrob(img, interpolate = T)
img.plot2 = ggplot()+
  geom_blank()+
  annotation_custom(img.in, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_minimal()

img = png::readPNG("preliminary_figures/ForageModv02_3003ticks.png")
img.in = grid::rasterGrob(img, interpolate = T)
img.plot3 = ggplot()+
  geom_blank()+
  annotation_custom(img.in, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_minimal()

veg.data$morans.i = as.numeric(veg.data$morans.i)
plot.data = veg.data %>% filter(signif == T) %>% filter(cycle_duration == 100) %>% filter(veg_cycle_start == "\"productive\"")
plot.data$burn_cost = as.factor(plot.data$burn_cost)

p1 = ggplot(plot.data) +
  geom_jitter(data = plot.data %>% filter(burn_cost == 0), mapping =
                aes(x = ticks, y = morans.i, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data %>% filter(burn_cost == 100), mapping =
                aes(x = ticks, y = morans.i, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data %>% filter(burn_cost == 200), mapping =
                aes(x = ticks, y = morans.i, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data %>% filter(burn_cost == 300), mapping =
                aes(x = ticks, y = morans.i, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_smooth(mapping = aes(x = ticks, y = morans.i, color = burn_cost)) +
  facet_grid(cycle_duration + veg_cycle_start ~ movement_model, labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "", y = "Global Moran's I") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 7.5), 
        axis.title = element_text(size = 7.5))
ggsave(filename = "preliminary_figures/veg_morans_i_fig2d.png", plot = p1, 
       dpi = 300, width = 8, height = 4)

all.plot =  ggarrange(
  ggarrange(img.plot1, img.plot2, img.plot3, labels = "AUTO", nrow = 1),
  p1, ncol = 1, nrow = 2, 
  common.legend = T, legend = "bottom", labels = c("", "D"), heights = c(1.5, 1.25)) +
  theme(axis.title = element_text(size = 7))

ggsave(filename = "figures/veg-type-dispersion.png", plot = all.plot,
       dpi = 300, width = 8, height = 6)

#### vegetation diversity####
veg.div.plot = ggplot(veg.data %>% filter(signif == T)) +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_grid(movement_model ~ cycle_duration + veg_cycle_start , labeller = 
               labeller(cycle_duration = cycle.labs, 
                        veg_cycle_start = start.labs, 
                        movement_model = move.labs)) +
  labs(color = "climate condition", y = "Shannon Diversity Index") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", strip.text = element_text(size = 6.5),)
plot(veg.div.plot)

ggsave(filename = "preliminary_figures/veg-type-diversity.png", plot = veg.div.plot,
       dpi = 300, width = 8, height = 5.5)

veg.div.plot2 = ggplot(veg.data %>% filter(signif == T) %>% 
                         filter(cycle_duration == 100) %>% 
                         filter(veg_cycle_start == "\"productive\"")) +
  geom_boxplot(mapping = aes(x = ticks, y = shannon.div, color = climate.condition, group = ticks)) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div)) +
  facet_grid(movement_model ~ burn_cost) +
  labs(color = "climate condition", y = "Shannon Diversity Index") +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", strip.text = element_text(size = 6.5),)
plot(veg.div.plot2)

veg.data$morans.i = as.numeric(veg.data$morans.i)
plot.data = veg.data %>% filter(signif == T) %>% filter(cycle_duration == 100) %>% filter(veg_cycle_start == "\"productive\"")
plot.data$burn_cost = as.factor(plot.data$burn_cost)



plot.data2 = plot.data %>% filter(burnt_neighbor_limit != 4)
p2 = ggplot(plot.data2) +
  geom_jitter(data = plot.data2 %>% filter(burn_cost == 0), mapping =
                aes(x = ticks, y = shannon.div, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data2 %>% filter(burn_cost == 100), mapping =
                aes(x = ticks, y = shannon.div, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data2 %>% filter(burn_cost == 200), mapping =
                aes(x = ticks, y = shannon.div, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_jitter(data = plot.data2 %>% filter(burn_cost == 300), mapping =
                aes(x = ticks, y = shannon.div, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
  geom_smooth(mapping = aes(x = ticks, y = shannon.div, color = burn_cost)) +
  facet_grid(movement_model ~ burnt_neighbor_limit + burn_veg_type_threshold, labeller = 
               labeller(burnt_neighbor_limit = bn.labs,
                        burn_veg_type_threshold = bt.labs,
                        movement_model = move.labs)) +
  labs(color = "", y = "Shannon Diversity Index") +
  scale_color_brewer(palette = "Set2",
                     labels = c("no disturbance cost", "low disturbance cost", "medium disturbance cost", "high disturbance cost")) +
  theme(legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        axis.title = element_text(size = 7.5))
plot(p2)
ggsave(filename = "preliminary_figures/veg_diversity_fig3a.png", plot = p2, 
       dpi = 300, width = 8, height = 4)

# p3 = ggplot(plot.data) +
#   geom_jitter(data = plot.data %>% filter(burn_cost == 0), mapping =
#                 aes(x = ticks, y = shannon.div.no0, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
#   geom_jitter(data = plot.data %>% filter(burn_cost == 100), mapping =
#                 aes(x = ticks, y = shannon.div.no0, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
#   geom_jitter(data = plot.data %>% filter(burn_cost == 200), mapping =
#                 aes(x = ticks, y = shannon.div.no0, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
#   geom_jitter(data = plot.data %>% filter(burn_cost == 300), mapping =
#                 aes(x = ticks, y = shannon.div.no0, group = ticks, color = burn_cost), alpha = 0.1, size = 0.25) +
#   geom_smooth(mapping = aes(x = ticks, y = shannon.div.no0, color = burn_cost)) +
#   facet_grid(movement_model ~ burnt_neighbor_limit + burn_veg_type_threshold, labeller = 
#                labeller(burnt_neighbor_limit = bn.labs,
#                         burn_veg_type_threshold = bt.labs,
#                         movement_model = move.labs)) +
#   labs(color = "", y = "Shannon Diversity Index") +
#   scale_color_brewer(palette = "Set2",
#                      labels = c("no disturbance cost (0)", "low disturbance cost (100)", "medium disturbance cost (200)", "high disturbance cost (300)", "productive interval", "unproductive interval")) +
#   theme(legend.position = "bottom", 
#         strip.text = element_text(size = 6), 
#         axis.title = element_text(size = 7.5))
# plot(p3)

bprop.plot = readRDS("preliminary_figures/proportion-burnt-landscape.rds")
all.plot2 = ggpubr::ggarrange(p2, bprop.plot + theme(legend.position = "none"), nrow = 2, labels = "AUTO")
ggsave(filename = "figures/veg-type-diversity.png", plot = all.plot2,
       dpi = 300, width = 8, height = 8)
