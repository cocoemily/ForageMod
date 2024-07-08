library(tidyverse)
library(here)

file.list = list.files("results", full.names = T)
exp.list = list.files("results", full.names = F)
experiments = unlist(str_split(exp.list, "_"))[seq(2, (length(exp.list) * 3), by = 3)]
experiments = unique(experiments)
outputs = c(
  "vegetation-types",
  "burning-behavior", 
  "benefit-distribution", 
  "forager-interactions",
  "forager-moves",
  "population", 
  "human-burning-amounts", 
  "veg-simpsons-diversity", 
  "veg-spat-autocorrelation"
)

data = list()
hb.data = list()
 
i = 1
for (x in experiments) {
  print(paste0("reading data from exp: ", i))
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
  
  sd = read_csv(exp.files[[which(str_detect(exp.files, outputs[8]))]], skip = 16)
  sd.df = sd[,1:2]
  colnames(sd.df) = c("ticks", "veg.simpsons.div")
  
  mi = read_csv(exp.files[[which(str_detect(exp.files, outputs[9]))]], skip = 16)
  mi.df = mi[,1:2]
  colnames(mi.df) = c("ticks", "veg.morans.i")
  
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
  ) %>% left_join(
    sd.df,
    by = c("ticks")
  ) %>% left_join(
    mi.df,
    by = c("ticks")
  ) 
  
  rm(list = c("pop.df", "vt.df", "bb.df", "bd.df", "fi.df", "fm.df", "sd.df", "mi.df", 
              "pop", "vt", "bb", "bd", "fi", "fm", "sd", "mis"))
  
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
  
  data[[x]] = final.exp.df
  hb.data[[x]] = final.hb.df
  i = i + 1
}

final.data = bind_rows(data)
final.hb.data = bind_rows(hb.data)

saveRDS(final.data, file = "bb-data.rds")
saveRDS(final.hb.data, file = "bb-hb-data.rds")
