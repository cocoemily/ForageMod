library(tidyverse)
library(here)
library(data.table)

file.list = list.files("results", pattern = "\\.csv$", full.names = T)
exp.list = list.files("results", pattern = "\\.csv$", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
print(length(experiments))

outputs = c(
  "turtle-locations"
)

tl.data = list()

for (x in experiments) {
  print(paste0("reading data from exp: ", x))
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)

  tl.df = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]])
  tl.df[nrow(tl.df) + 1, ] <- as.list(as.numeric(colnames(tl.df)))
  colnames(tl.df) = c("x", "y", "who", "ticks")

  final.tl.df = bind_cols(tl.df, model.parameters)
  final.tl.df$exp = x

  tl.data[[x]] = final.tl.df
}

final.tl.data = rbindlist(tl.data)

saveRDS(final.tl.data, file = "results/bb-tl-data.rds")
