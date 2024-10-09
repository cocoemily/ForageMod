data = readRDS("results/bb-data.rds")

end.tick = unique(data$`tick-limit`)
fd = data %>% filter(ticks == end.tick)
finished.exp = unique(fd$exp)

data = data %>% filter(exp %in% finished.exp)