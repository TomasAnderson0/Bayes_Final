library(rstan)
library(readr)
library(dplyr)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))
cerebros <- cerebros %>% mutate(res1 = ifelse(resonador_fab == "GE", 1, 0)) %>% 
  mutate(res2 = ifelse(resonador_fab == "Philips", 1, 0))

init_list <- list(
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0)
)


lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = cerebros$edad,
              sex = (as.numeric(as.factor(cerebros$sexo))-1),
              vhi = cerebros$lh_subcx_hippocampus_volume/1000,
              vi = cerebros$xh_general_etiv_volume/100000,
              ecsf = cerebros$lh_cortex_superiorfrontal_thickness,
              vcf = cerebros$lh_cortex_fusiform_volume/1000,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = cerebros$res1,
              res1 = cerebros$res2
)

modelob0 <- stan(
  file = "Modelo1b0.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo1

traceplot(modelo1)

