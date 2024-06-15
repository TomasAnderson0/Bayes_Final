library(rstan)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))

lista <- list(N = nrow(cerebros),
              diag = as.numeric(as.factor(cerebros$diag)),
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
  warmup = 500,
  iter = 5000
)
