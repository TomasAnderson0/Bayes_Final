library(rstan)
library(readr)
library(dplyr)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))

init_list <- list(
  list(b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0),
  list(b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0),
  list(b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0),
  list(b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0, b7 = 0, b8 = 0)
)


lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = cerebros$edad,
              sex = as.numeric(as.factor(cerebros$sexo)),
              vhi = cerebros$lh_subcx_hippocampus_volume,
              vi = cerebros$xh_general_etiv_volume,
              ecsf = cerebros$lh_cortex_superiorfrontal_thickness,
              vcf = cerebros$lh_cortex_fusiform_volume,
              inte = as.numeric(as.factor(cerebros$intensidad_campo)),
              res = as.numeric(as.factor(cerebros$resonador_fab))
              )

modelo1 <- stan(
  file = "Modelo1.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo1

traceplot(modelo1)
