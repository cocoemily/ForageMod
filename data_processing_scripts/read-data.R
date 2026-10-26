library(tidyverse)
library(here)
library(foreach)
library(doParallel)

file.list = list.files("results", pattern = "\\.csv$", full.names = T)
exp.list = list.files("results", pattern = "\\.csv$", full.names = F)
experiments = sub("^experiment_([^_]+)_.*$", "\\1", exp.list)
experiments = unique(experiments)
#print(length(experiments))

outputs = c(
  "vegetation-types",
  "burning-behavior", 
  "benefit-distribution", 
  "forager-interactions",
  "forager-moves",
  "population", 
  "network_similarity", 
  "network_spat_clustering"
)

num_cores <- detectCores() - 8
cl <- makeCluster(num_cores)
registerDoParallel(cl)

final.data <- foreach(
  x = experiments, 
  .packages = c("tidyverse"), 
  .combine = bind_rows,
  .errorhandling = "pass"
) %dopar% {
  print(paste0("reading data from exp: ", x))
  exp.files = file.list[which(str_detect(file.list, as.character(x)))]
  
  model.parameters = read_csv(exp.files[1], skip = 5, n_max = 1)
  
  vt = read_csv(exp.files[[which(str_detect(exp.files, outputs[1]))]], skip = 24)
  vt.df = bind_rows(
    vt[,1:2] %>% mutate(veg.type = 7), 
    vt[,5:6] %>% mutate(veg.type = 6), 
    vt[,9:10] %>% mutate(veg.type = 5), 
    vt[,13:14] %>% mutate(veg.type = 4), 
    vt[,17:18] %>% mutate(veg.type = 3), 
    vt[,21:22] %>% mutate(veg.type = 2), 
    vt[,25:26] %>% mutate(veg.type = 1), 
    vt[,29:30] %>% mutate(veg.type = 0), 
    vt[,33:34] %>% mutate(veg.type = -1)
  )
  colnames(vt.df) = c("ticks", "proportion", "veg.type")
  
  bb = read_csv(exp.files[[which(str_detect(exp.files, outputs[2]))]], skip = 18)
  bb.df = bb[,c(1:2, 6, 10)]
  colnames(bb.df) = c("ticks", "mean.burn.prob", "high.burn.prob", "low.burn.prob")
  
  bd = read_csv(exp.files[[which(str_detect(exp.files, outputs[3]))]], skip = 17)
  bd.df = bind_rows(
    bd[,1:2] %>% mutate(benefit = "self"), 
    bd[,5:6] %>% mutate(benefit = "other")
  )
  colnames(bd.df) = c("ticks", "count", "benefit")
  
  pop = read_csv(exp.files[[which(str_detect(exp.files, outputs[6]))]], skip = 16)
  pop.df = pop[,1:2]
  colnames(pop.df) = c("ticks", "pop.density")
  
  fi = read_csv(exp.files[[which(str_detect(exp.files, outputs[4]))]], skip = 21)
  fi.df = fi[,c(1:2, 6, 10, 14, 18, 22)]
  colnames(fi.df) = c("ticks", "mean.fi", "mean.same.fi", "high.fi", "low.fi", "high.same.fi", "low.same.fi")
  
  fm = read_csv(exp.files[[which(str_detect(exp.files, outputs[5]))]], skip = 18)
  fm.df = fm[,c(1:2, 6, 10)]
  colnames(fm.df) = c("ticks", "mean.fm", "high.fm", "low.fm")
  
  netsim = read_csv(exp.files[[which(str_detect(exp.files, outputs[7]))]], skip = 16, show_col_types = FALSE)
  netsim.df = netsim[,1:2]
  colnames(netsim.df) = c("ticks", "net.sim")
  
  netclust = read_csv(exp.files[[which(str_detect(exp.files, outputs[8]))]], skip = 16, show_col_types = FALSE)
  netclust.df = netclust[,1:2]
  colnames(netclust.df) = c("ticks", "net.clust")
 
  all.data = pop.df %>% 
    left_join(bb.df, by = c("ticks")) %>% 
    left_join(
      bd.df %>% pivot_wider(names_from = benefit, names_glue = "benefit_{benefit}", values_from = count),
      by = c("ticks")
    ) %>%
    left_join(fm.df, by = c("ticks")) %>% 
    left_join(fi.df,by = c("ticks")) %>%
    left_join(netsim.df, by = c("ticks")) %>% 
    left_join(netclust.df, by = c("ticks")) %>% 
    left_join(
      vt.df %>% pivot_wider(names_from = veg.type, names_glue = "veg_{veg.type}", values_from = proportion),
      by = c("ticks")
    )

  
  if(nrow(all.data) > 1) {
    if(model.parameters$`veg-cycle-start` == "productive") {
      cc.seq = rep(rep(c("productive", "unproductive"), each = model.parameters$`cycle-duration`), 
                   (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
      all.data$climate.condition = c("productive", cc.seq[1:(nrow(all.data) - 1)])     } 
    else {        
      cc.seq = rep(rep(c("unproductive", "productive"), each = model.parameters$`cycle-duration`),                                                                                                            (model.parameters$`tick-limit`/model.parameters$`cycle-duration`)/2)
      all.data$climate.condition = c("unproductive", cc.seq[1:(nrow(all.data) - 1)])
    }
  }
  
  final.exp.df = bind_cols(all.data, model.parameters)
  final.exp.df$exp = x
  
  return(final.exp.df)
}

stopCluster(cl)

saveRDS(final.data, file = "results/full_experiments_data.rds")
