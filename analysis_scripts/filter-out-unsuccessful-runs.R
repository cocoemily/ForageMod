library(tidyverse)

data = readRDS("results/full_experiments_data.rds")

end.tick = unique(data$`tick-limit`)
fd = data %>% filter(ticks == end.tick)
finished.exp = unique(fd$exp)

data = data %>% filter(exp %in% finished.exp)

rm(list = c("fd", "end.tick", "finished.exp"))
