library(tidyverse)
library(ggthemes)
library(here)

theme_set(theme_bw())

file.list = list.files("sensitivity-analysis/SA-test-all", full.names = T)
exp.list = list.files("sensitivity-analysis/SA-test-all", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
outputs = c(
  "vegetation-types",
  "burning-behavior", 
  "benefit-distribution", 
  "forager-interactions",
  "forager-moves",
  "population", 
  "human-burning-amounts"
)

data = list()
hb.data = list()

for (x in experiments) {
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)
  
  vt = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]], skip = 23)
  vt.df = bind_rows(
    vt[,1:2] %>% mutate(veg.type = 7), 
    vt[,5:6] %>% mutate(veg.type = 6), 
    vt[,9:10] %>% mutate(veg.type = 5), 
    vt[,13:14] %>% mutate(veg.type = 4), 
    vt[,17:18] %>% mutate(veg.type = 3), 
    vt[,21:22] %>% mutate(veg.type = 2), 
    vt[,25:26] %>% mutate(veg.type = 1), 
    vt[,29:30] %>% mutate(veg.type = 0)
  )
  colnames(vt.df) = c("ticks", "count", "veg.type")
  
  #vt.df2 = vt.df %>% pivot_wider(names_from = veg.type, names_glue = "veg_{veg.type}", values_from = count)
  
  bb = read_csv(exp.files[[which(str_detect(exp.files, outputs[2]))]], skip = 18)
  bb.df = bb[,1:2]
  colnames(bb.df) = c("ticks", "mean.burn.prob")
  
  bd = read_csv(exp.files[[which(str_detect(exp.files, outputs[3]))]], skip = 17)
  bd.df = bind_rows(
    bd[,1:2] %>% mutate(benefit = "self"), 
    bd[,5:6] %>% mutate(benefit = "other")
  )
  colnames(bd.df) = c("ticks", "count", "benefit")
  
  pop = read_csv(exp.files[[which(str_detect(exp.files, outputs[6]))]], skip = 16)
  pop.df = pop[,1:2]
  colnames(pop.df) = c("ticks", "pop.count")
  
  fi = read_csv(exp.files[[which(str_detect(exp.files, outputs[4]))]], skip = 18)
  fi.df = fi[,c(1:2, 6, 10)]
  colnames(fi.df) = c("ticks", "mean.fi", "high.fi", "low.fi")
  
  fm = read_csv(exp.files[[which(str_detect(exp.files, outputs[5]))]], skip = 18)
  fm.df = fm[,c(1:2, 6, 10)]
  colnames(fm.df) = c("ticks", "mean.fm", "high.fm", "low.fm")
  
  hb.df = read_csv(exp.files[[which(str_detect(exp.files, outputs[7]))]])
  colnames(hb.df) = c("x", "y", "times.burned")
  
  all.data = pop.df %>% left_join(
    vt.df %>% pivot_wider(names_from = veg.type, names_glue = "veg_{veg.type}", values_from = count),
    by = c("ticks")
  ) %>% left_join(
    bb.df, 
    by = c("ticks")
  ) %>% left_join(
    bd.df %>% pivot_wider(names_from = benefit, names_glue = "benefit_{benefit}", values_from = count),
    by = c("ticks")
  ) %>% left_join(
    fi.df,
    by = c("ticks")
  ) %>% left_join(
    fm.df,
    by = c("ticks")
  )
  
  if(model.parameters$`veg-cycle-start` == "productive") {
    cc.seq = rep(rep(c("productive", "unproductive"), each = model.parameters$`cycle-duration`), 
                 (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
    all.data$climate.condition = c("productive", cc.seq[1:(nrow(all.data) - 1)])
  } else { #(model.parameters$`veg-cycle-start` == "unproductive") 
    cc.seq = rep(rep(c("unproductive", "productive"), each = model.parameters$`cycle-duration`), 
                 (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
    all.data$climate.condition = c("unproductive", cc.seq[1:(nrow(all.data) - 1)])
  }
  
  final.exp.df = bind_cols(all.data, model.parameters)
  final.exp.df$exp = x
  final.hb.df = bind_cols(hb.df, model.parameters)
  
  data = bind_rows(data, final.exp.df)
  hb.data = bind_rows(hb.data, final.hb.df)
}


hist(data$pop.count)
ggplot(data) +
  geom_line(aes(y = pop.count, x = ticks,
                     color = as.factor(`veg-type-modifier`), 
                     group = as.factor(`veg-type-modifier`)))

summary((data %>% filter(`veg-type-modifier` == 100))$pop.count)
summary((data %>% filter(`veg-type-modifier` == 100))$ticks)

summary((data %>% filter(`veg-type-modifier` == 300))$pop.count)
summary((data %>% filter(`veg-type-modifier` == 300))$ticks)

bdata = data %>% filter(`veg-type-modifier` == 300)

plot300 = ggplot(bdata) +
  geom_line(aes(x = ticks, y = pop.count, color = as.factor(`forager-energy-requirement`))) +
  facet_grid(`movement-cost` ~ `reproduction-cost` + `reproduction-threshold` , labeller = label_both)
  

ggarrange(plot200, plot300, plot500, labels = c("200", "300", "500"), common.legend = T)

ggplot(bdata %>% filter(`forager-energy-requirement` == 1500)) +
  geom_line(aes(x = ticks, y = pop.count, color = as.factor(`movement-cost`))) +
  facet_grid(`reproduction-cost` ~ `reproduction-threshold` , labeller = label_both)

ggplot(bdata %>% filter(`forager-energy-requirement` == 1500)) +
  geom_line(aes(x = ticks, y = mean.fi, color = as.factor(`movement-cost`))) +
  facet_grid(`reproduction-cost` ~ `reproduction-threshold` , labeller = label_both)

ggplot(bdata %>% filter(`forager-energy-requirement` == 1500)) +
  geom_line(aes(x = ticks, y = mean.fm, color = as.factor(`movement-cost`))) +
  facet_grid(`reproduction-cost` ~ `reproduction-threshold` , labeller = label_both)

ggplot(bdata %>% filter(`forager-energy-requirement` == 1500)) +
  geom_line(aes(x = ticks, y = mean.burn.prob, color = as.factor(`movement-cost`))) +
  facet_grid(`reproduction-cost` ~ `reproduction-threshold` , labeller = label_both)

#filtering to remove data as model is reaching equilibrium
# adata = data %>% filter(ticks >= 400)
# 
# ggplot(data) +
#   geom_line(aes(x = ticks, y = pop.count)) + 
#   geom_smooth(aes(x = ticks, y = pop.count)) +
#   facet_grid(`movement-model` ~ `burn-cost`)
# 
# ggplot(adata) +
#   geom_point(aes(x = ticks, y = mean.burn.prob, color = climate.condition)) + 
#   geom_line(aes(x = ticks, y = mean.burn.prob, color = as.factor(`burnt-neighbor-limit`))) + 
#   facet_grid(`movement-model` ~ `veg-distribution`, scales = "free") +
#   #geom_vline(xintercept = seq(0, 4000, by = 500)) +
#   scale_color_colorblind()
# 
# ggplot(adata) +
#   geom_line(aes(x = ticks, y = veg_0, color = as.factor(`burnt-neighbor-limit`))) + 
#   facet_grid(`movement-model` ~ `veg-distribution`) +
#   scale_color_colorblind()
# 
# 
# benefit = adata %>% pivot_longer(cols = c("benefit_self", "benefit_other"), 
#                                  names_to = "benefit", values_to = "count")
# ##how to assign each tick a vegetation cycle value to visualize trends during productive vs unproductive portions of model run
# 
# ggplot(benefit %>% 
#          filter(benefit == "benefit_self")
# ) +
#   geom_line(aes(x = ticks, y = count/pop.count, color = as.factor(`burnt-neighbor-limit`))) +
#   #geom_smooth(aes(x = ticks, y = count/pop.count, color = as.factor(`burnt-neighbor-limit`))) +
#   #geom_vline(xintercept = seq(0, 4000, by = 500)) +
#   facet_grid(`movement-model` ~ `veg-distribution`, scales = "free") +
#   scale_color_colorblind()
# 
# summary(lm(mean.fm ~ ticks, data = data %>% filter(ticks > 0) %>% filter(`burnt-neighbor-limit` == 2)))
# summary(lm(mean.fm ~ ticks, data = data %>% filter(ticks > 0) %>% filter(`burnt-neighbor-limit` == 4)))
# summary(lm(mean.fm ~ ticks, data = data %>% filter(ticks > 0) %>% filter(`burnt-neighbor-limit` == 6)))
# ggplot(data %>% filter(ticks > 0)) +
#   #geom_line(aes(x = ticks, y = mean.fm, color = as.factor(`burnt-neighbor-limit`))) +
#   geom_smooth(aes(x = ticks, y = mean.fm, color = as.factor(`burnt-neighbor-limit`)), method = "lm") +
#   facet_wrap(`movement-model` ~ `veg-distribution`,nrow = 1) +
#   scale_color_colorblind()
# 
# 
# ggplot(hb.data) +
#   geom_density(aes(x = times.burned, fill = as.factor(`burnt-neighbor-limit`)), alpha = 0.75) +
#   facet_grid(`movement-model` ~ `veg-distribution`, scales = "free") +
#   scale_fill_colorblind()
#   
# 
