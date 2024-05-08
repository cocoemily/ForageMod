library(tidyverse)
library(ggthemes)
library(ggpubr)
library(here)

theme_set(theme_bw())

file.list = list.files("sensitivity-analysis/SA-test", full.names = T)
exp.list = list.files("sensitivity-analysis/SA-test", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, 120, by = 3)]
outputs = c(
  "population" 
)

full.runs = list()
data = list()
finished = list()

for (x in experiments) {
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)
  
  pop = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]], skip = 16)
  pop.df = pop[,1:2]
  colnames(pop.df) = c("ticks", "pop.count")
  
  all.data = pop.df
  
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
  
  finished.exp.df = final.exp.df 
  if(max(final.exp.df$ticks) == 1000) {
    finished.exp.df = finished.exp.df %>% mutate(finished = TRUE)
    finished.exp.df = finished.exp.df[nrow(finished.exp.df),]
    
    full.runs = bind_rows(full.runs, final.exp.df)
    
  } else {
    finished.exp.df = finished.exp.df %>% mutate(finished = FALSE)
    finished.exp.df = finished.exp.df[nrow(finished.exp.df),]
  }
  
  data = bind_rows(data, final.exp.df)
  finished = bind_rows(finished, finished.exp.df)
}

comp.data = data %>% filter(`veg-cycle-start` == "productive")

plot200 = ggplot(data %>% filter(`veg-type-modifier` == 200)) +
  geom_line(aes(x = ticks, y = pop.count, color = as.factor(`forager-energy-requirement`))) +
  facet_grid(`movement-cost` ~ `reproduction-cost` + `reproduction-threshold` , labeller = label_both)

plot500 = ggplot(data %>% filter(`veg-type-modifier` == 500)) +
  geom_line(aes(x = ticks, y = pop.count, color = as.factor(`forager-energy-requirement`))) +
  facet_grid(`movement-cost` ~ `reproduction-cost` + `reproduction-threshold` , labeller = label_both)
