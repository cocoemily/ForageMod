
##burnt neighbor limit = 1
burnt.burnn1 = beta.data %>% filter(`burnt-neighbor-limit` == 1)
burnn1.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

burnt.burnn1 = data %>% filter(`burnt-neighbor-limit` == 1)
burnn1.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn1 %>% select_at(c("ticks", "burnt", parameters[-5])))

AIC(burnn1.fit1)
AIC(burnn1.fit2)

##burnt neighbor limit = 4
burnt.burnn4 = beta.data %>% filter(`burnt-neighbor-limit` == 4)
burnn4.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn4 = data %>% filter(`burnt-neighbor-limit` == 4)
burnn4.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn4 %>% select_at(c("ticks", "burnt", parameters[-5])))
AIC(burnn4.fit1)
AIC(burnn4.fit2)

##burnt neighbor limit = 8 (no limit)
burnt.burnn8 = beta.data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit1 = betareg(burnt ~ ticks*(ticks + `natural-ignition` + `cycle-duration` + 
                                       `veg-cycle-start` + `veg-distribution` + `burn-cost` + `burn-veg-type-threshold` + 
                                       `movement-model`), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))
burnt.burnn8 = data %>% filter(`burnt-neighbor-limit` == 8)
burnn8.fit2 = lm(burnt ~ ticks*(.), data = burnt.burnn8 %>% select_at(c("ticks", "burnt", parameters[-5])))
AIC(burnn8.fit1)
AIC(burnn8.fit2)

par(mfrow=c(1,3), cex=0.8)
plot(burnn1.fit2, which = 2)
plot(burnn4.fit2, which = 2)
plot(burnn8.fit2, which = 2)

plot_summs(burnn1.fit2, burnn4.fit2, burnn8.fit2, scale = T, 
           model.names = c("BNL = 1", "BNL = 4", "BNL = 8"))

rm(list = c("beta.data", "veg_0.burnn1", "veg_0.burnn4", "veg_0.burnn8"))

# plot_summs(veg.100.prod.fit1, veg.100.unprod.fit1,veg.250.prod.fit1, veg.250.unprod.fit1, model.names = c(
#   "start = productive, cycle = 100", 
#   "start = unproductive, cycle = 100", 
#   "start = productive, cycle = 250", 
#   "start = unproductive, cycle = 250"
# ), omit.coefs = c("(Intercept)","ticks", "natural_ignition0.05", "veg_distribution\"clustered\"", "burnt_neighbor_limit4", "burnt_neighbor_limit8", "burn_cost100", "burn_cost200", "burn_cost300", "burn_veg_type_threshold7", "movement_model\"Directed Walk\"")) +
#   theme(legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 6))

rw.fit2 = segmented::segmented(rw.fit1, seg.Z = ~ ticks, psi = list(ticks = c(250)))
#summary(rw.fit2)
rw.fit2$psi
segmented::slope(rw.fit2)

# plotNormalDensity((rw.data %>% filter(ticks < 471))$adj.fi)
# plotNormalDensity((rw.data %>% filter(ticks > 471))$adj.fi)

rw.fit3 = lm(adj.fi ~ ticks*(.), data = rw.data %>% filter(ticks > 0) %>% select_at(c("ticks", "adj.fi", parameters[-8])) %>% filter(ticks < 471))

rw.fit4 = lm(adj.fi ~ ticks*(.), data = rw.data %>% filter(ticks > 0) %>% select_at(c("ticks", "adj.fi", parameters[-8])) %>% filter(ticks > 471))

# dw.fit1 %>% broom::tidy() %>% 
#   filter(p.value <= 0.05) %>%
#   mutate(p.value = scales::pvalue(p.value)) %>%
#   kable()

rw.fit3 %>% broom::tidy() %>% full_join((rw.fit4 %>% broom::tidy()), by = "term") %>%
  select(term, estimate.x, p.value.x, estimate.y, p.value.y) %>%
  filter(p.value.y <= 0.05 | p.value.x <= 0.05) %>%
  mutate(p.value.x = scales::pvalue(p.value.x), 
         p.value.y =scales::pvalue(p.value.y)) %>%
  rename(part_1_estimate = estimate.x, 
         part_1_pvalue = p.value.x, 
         part_2_estimate = estimate.y, 
         part_2_pvalue = p.value.y) %>%
  kable()

par(mfrow = c(1,2))
#plot at 2000
pvalue.r = psych::corr.test(data2000 %>% filter(`veg-distribution` == "\"random\"") %>% select(mean.fm, adj.fi, veg_morans.i, be_morans.i))$p
corrplot::corrplot(cor(data2000 %>% filter(`veg-distribution` == "\"random\"") %>% select(mean.fm, adj.fi, veg_morans.i, be_morans.i), use = "pairwise.complete.obs"),
                   method = "square", addCoef.col = "black", 
                   col = scales::alpha(corrplot::COL2("RdBu"), alpha = 0.85),
                   p.mat = pvalue.r, insig="blank", sig.level=0.1, 
                   type = "lower", number.cex = 0.5, diag=F, 
                   title = "random distribution", mar=c(0,0,1,0))

pvalue.c = psych::corr.test(data2000 %>% filter(`veg-distribution` == "\"clustered\"") %>% select(mean.fm, adj.fi, veg_morans.i, be_morans.i))$p
corrplot::corrplot(cor(data2000 %>% filter(`veg-distribution` == "\"clustered\"") %>% select(mean.fm, adj.fi, veg_morans.i, be_morans.i), use = "pairwise.complete.obs"),
                   method = "square", addCoef.col = "black", 
                   col = scales::alpha(corrplot::COL2("RdBu"), alpha = 0.85),
                   p.mat = pvalue.c, insig="blank", sig.level=0.1, 
                   type = "lower", number.cex = 0.5, diag=F, 
                   title = "clustered distribution", mar=c(0,0,1,0))