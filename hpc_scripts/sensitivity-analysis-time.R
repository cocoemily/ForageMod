library(tidyverse)
library(here)

theme_set(theme_bw())

file.list = list.files("sensitivity-analysis_time", pattern = "\\.csv$", full.names = T)
exp.list = list.files("sensitivity-analysis_time", pattern = "\\.csv$", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
outputs = c(
  "burning-behavior", 
  "forager-moves",
  "vegetation-types",
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
  
  pop = read_csv(exp.files[[which(str_detect(exp.files, outputs[4]))]], skip = 16)
  pop.df = pop[,1:2]
  colnames(pop.df) = c("ticks", "pop.count")
  
  all.data = pop.df %>% 
    left_join(
      bb.df, 
      by = c("ticks")
    )
  
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

saveRDS(final.data, file = "sensitivity-analysis_time/SA-data_time.rds")
