library(rstan)
library(readr)
library(dplyr)
library(ggplot2)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))
cerebros <- cerebros %>% mutate(res1 = ifelse(resonador_fab == "GE", 1, 0)) %>% 
  mutate(res2 = ifelse(resonador_fab == "Philips", 1, 0))

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              sexo = (as.numeric(as.factor(cerebros$sexo))-1),
              ver = cerebros$lh_subcx_hippocampus_volume/1000,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = cerebros$res1,
              res2 = cerebros$res2
)

modeloregcomp <- stan(
  file = "Regresion_Completa.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorcomp <- data.frame(extract(modeloregcomp, c("beta1", "beta2", "beta3", "beta4", "beta5")))

ggplot(posteriorcomp) + geom_density(aes(x = beta5))

# ---------------------------------------------------------------------

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              sexo = (as.numeric(as.factor(cerebros$sexo))-1),
              ver = cerebros$xh_general_etiv_volume/100000,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = cerebros$res1,
              res2 = cerebros$res2
)

modeloregcomp2 <- stan(
  file = "Regresion_Completa.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorcomp2 <- data.frame(extract(modeloregcomp2, c("beta1", "beta3", "beta4", "beta5")))

ggplot(posteriorcomp2) + geom_density(aes(x = beta1))

# --------------------------------------------------------------

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              sexo = (as.numeric(as.factor(cerebros$sexo))-1),
              ver = cerebros$lh_cortex_superiorfrontal_thickness,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = cerebros$res1,
              res2 = cerebros$res2
)

modeloregcomp3 <- stan(
  file = "Regresion_Completa.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorcomp3 <- data.frame(extract(modeloregcomp3, c("beta3", "beta4", "beta5")))

ggplot(posteriorcomp3) + geom_density(aes(x = beta5))

# ---------------------------------------------------------------------------------

lista <- list(N = nrow(cerebros),
              edad = cerebros$edad,
              sexo = (as.numeric(as.factor(cerebros$sexo))-1),
              ver = cerebros$lh_cortex_fusiform_volume/1000,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = cerebros$res1,
              res2 = cerebros$res2
)

modeloregcomp4 <- stan(
  file = "Regresion_Completa.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

posteriorcomp4 <- data.frame(extract(modeloregcomp4, c("beta3", "beta4", "beta5")))

ggplot(posteriorcomp4) + geom_density(aes(x = beta5))

