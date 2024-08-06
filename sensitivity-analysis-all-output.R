library(tidyverse)
library(here)

theme_set(theme_bw())

file.list = list.files("sensitivity-analysis", full.names = T)
exp.list = list.files("sensitivity-analysis", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
outputs = c(
  "burning-behavior", 
  "forager-moves",
  "population"
)

data = list()

i = 1
for (x in experiments) {
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)
  
  bb = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]], skip = 18)
  bb.df = bb[,1:2]
  colnames(bb.df) = c("ticks", "mean.burn.prob")
  
  fm = read_csv(exp.files[[which(str_detect(exp.files, outputs[2]))]], skip = 18)
  fm.df = fm[,c(1:2, 6, 10)]
  colnames(fm.df) = c("ticks", "mean.fm", "high.fm", "low.fm")
  
  pop = read_csv(exp.files[[which(str_detect(exp.files, outputs[3]))]], skip = 16)
  pop.df = pop[,1:2]
  colnames(pop.df) = c("ticks", "pop.count")
  
  all.data = pop.df %>% 
    left_join(
      bb.df, 
      by = c("ticks")
    ) %>% left_join(
      fm.df,
      by = c("ticks")
    ) 
  
  rm(list = c("pop.df", "bb.df", "fm.df", "pop", "bb", "fm"))
  
  if(nrow(all.data) > 1) {
    if(model.parameters$`veg-cycle-start` == "productive") {
      cc.seq = rep(rep(c("productive", "unproductive"), each = model.parameters$`cycle-duration`), 
                   (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
      all.data$climate.condition = c("productive", cc.seq[1:(nrow(all.data) - 1)])
    } else { #(model.parameters$`veg-cycle-start` == "unproductive") 
      cc.seq = rep(rep(c("unproductive", "productive"), each = model.parameters$`cycle-duration`), 
                   (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
      all.data$climate.condition = c("unproductive", cc.seq[1:(nrow(all.data) - 1)])
    }
  }
  
  final.exp.df = bind_cols(all.data, model.parameters)
  final.exp.df$exp = x
  
  data[[x]] = final.exp.df
  i = i + 1
}

final.data = bind_rows(data)

saveRDS(final.data, file = "sensitivity-analysis/SA-data.rds")
