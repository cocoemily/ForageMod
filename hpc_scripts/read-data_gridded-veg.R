library(tidyverse)
library(here)
library(data.table)

file.list = list.files("results", pattern = "\\.csv$", full.names = T)
exp.list = list.files("results", pattern = "\\.csv$", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
print(length(experiments))

outputs = c(
  "gridded-veg-types"
)

vt.data = list()

i = 1
for (x in experiments) {
  print(paste0("reading data from exp: ", i))
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)

  gvt.df = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]])
  gvt.df[nrow(gvt.df) + 1, ] <- as.list(as.numeric(colnames(gvt.df)))
  colnames(gvt.df) = c("x", "y", "veg.type", "ticks")
  gvt.df$veg.type = as.numeric(gvt.df$veg.type)

  final.vt.df = bind_cols(gvt.df, model.parameters)
  final.vt.df$exp = x

  vt.data[[x]] = final.vt.df
  i = i + 1
}

final.vt.data = rbindlist(vt.data)

saveRDS(final.vt.data, file = "results/bb-vt-data.rds")
