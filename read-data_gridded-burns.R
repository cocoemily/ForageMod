library(tidyverse)
library(here)
library(data.table)

file.list = list.files("results", full.names = T)
exp.list = list.files("results", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
print(experiments)
print(length(experiments))

outputs = c(
  "human-burning-amounts"
)

hb.data = list()

i = 1
for (x in experiments) {
  print(paste0("reading data from exp: ", i))
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)
  
  hb.df = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]])
  colnames(hb.df) = c("x", "y", "times.burned", "ticks")
  
  final.hb.df = bind_cols(hb.df, model.parameters)
  final.hb.df$exp = x
  
  hb.data[[x]] = final.hb.df
  i = i + 1
}

final.hb.data = rbindlist(hb.data)

saveRDS(final.hb.data, file = "bb-hb-data.rds")
