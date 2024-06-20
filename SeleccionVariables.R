library(rstan)
library(readr)
library(dplyr)
library(loo)
library(kableExtra)

cerebros <- read_csv("cerebros.csv")
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"), lh_subcx_hippocampus_volume = lh_subcx_hippocampus_volume/1000, xh_general_etiv_volume = xh_general_etiv_volume/100000, lh_cortex_fusiform_volume = lh_cortex_fusiform_volume/1000, intensidad_campo = factor(ifelse(intensidad_campo == 1.5, "1.5T", "3T"), levels = c("3T", "1.5T")), sexo = factor(ifelse(sexo == "female", "Femenino", "Masculino"), levels = c("Masculino", "Femenino")), resonador_fab = factor(resonador_fab, levels = c("Siemens", "Philips", "GE")))

# --------------------- EDAD -------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = cerebros$edad
)

modelo_edad <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_edad
# APORTA
Bedad <- data.frame(rstan::extract(modelo_edad, c("b1")))
Bedad <- Bedad %>% mutate(variable = "Edad")

#------------------------------ SEXO -----------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = as.numeric(cerebros$sexo)-1
)

modelo_sexo <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_sexo
# NOOOOOOOOOOOOOO APORTA

Bsexo <- data.frame(rstan::extract(modelo_sexo, c("b1")))
Bsexo <- Bsexo %>% mutate(variable = "Sexo")

#------------------------------------VHI---------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = cerebros$lh_subcx_hippocampus_volume
)

modelo_VHI <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_VHI
# APORTA

BVHI <- data.frame(rstan::extract(modelo_VHI, c("b1")))
BVHI <- BVHI %>% mutate(variable = "VHI")

#-----------------------------------VI----------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = cerebros$xh_general_etiv_volume
)

modelo_VI <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_VI
# NOOOOOOOOOOOOOO APORTA

BVI <- data.frame(rstan::extract(modelo_VI, c("b1")))
BVI <- BVI %>% mutate(variable = "VI")

#-----------------------------------ECSF----------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = cerebros$lh_cortex_superiorfrontal_thickness
)

modelo_ecsf <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_ecsf
# APORTA

Becsf <- data.frame(rstan::extract(modelo_ecsf, c("b1")))
Becsf <- Becsf %>% mutate(variable = "ECSF")

#--------------------------------VCF-------------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = cerebros$lh_cortex_fusiform_volume
)

modelo_vcf <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_vcf
# APORTA

Bvcf <- data.frame(rstan::extract(modelo_vcf, c("b1")))
Bvcf <- Bvcf %>% mutate(variable = "VCF")

#--------------------------------INTENSIDAD-------------------------------------------

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver = as.numeric(cerebros$intensidad_campo)-1
)

modelo_inte <- stan(
  file = "RegLogUnivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_inte
# APORTA

Binte <- data.frame(rstan::extract(modelo_inte, c("b1")))
Binte <- Binte %>% mutate(variable = "Intensidad")

#-------------------------------RESONADOR--------------------------------------------

modeloresonador <- cerebros %>% mutate(res1 = ifelse(resonador_fab == "GE", 1, 0)) %>% 
  mutate(res2 = ifelse(resonador_fab == "Philips", 1, 0))

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              ver1 = modeloresonador$res1,
              ver2 = modeloresonador$res2
)

modelo_resonador <- stan(
  file = "RegLogUnivarResonador.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_resonador
# APORTA

Bres <- data.frame(rstan::extract(modelo_resonador, c("b1", "b2")))
Bres <- data.frame(b1 = c(Bres$b1, Bres$b2))
Bres <- Bres %>% mutate(variable = c(rep("Resonador GE", 5400), rep("Resonador Philips", 5400)))

grafico_betas <- rbind(Bedad, Bsexo, BVHI, BVI, Becsf, Bvcf, Binte, Bres)

ggplot(grafico_betas) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = "#057057") + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal()

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
#  -------------------------- MODELO COMPLETO ---------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = cerebros$edad,
              vhi = cerebros$lh_subcx_hippocampus_volume,
              ecsf = cerebros$lh_cortex_superiorfrontal_thickness,
              vcf = cerebros$lh_cortex_fusiform_volume,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              res1 = modeloresonador$res1,
              res2 = modeloresonador$res2
)

modeloMulti <- stan(
  file = "RegLogMultivar.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

betas <- data.frame(rstan::extract(modeloMulti, c("b1", "b2","b3", "b4","b5", "b6","b7")))
betas <- data.frame(b1 = c(betas$b1, betas$b2, betas$b3, betas$b4, betas$b5, betas$b6, betas$b7))
betas <- betas %>% mutate(variable = c(rep("Edad", 5400), rep("VHI", 5400), rep("ECSF", 5400), rep("VCF", 5400), rep("Intensidad", 5400), rep("Resonador GE", 5400), rep("Resonador Philips", 5400)))

ggplot(betas) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = "#057057") + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal()

looM1 <- loo(modeloMulti)



lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = cerebros$edad,
              vhi = cerebros$lh_subcx_hippocampus_volume,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1)
)

modeloMulti2 <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

looM2 <- loo(modeloMulti2)

loo_compare(looM1, looM2)


lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = cerebros$edad,
              vhi = cerebros$lh_subcx_hippocampus_volume,
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              axv = cerebros$edad * cerebros$lh_subcx_hippocampus_volume,
              vxi = cerebros$lh_subcx_hippocampus_volume * (as.numeric(as.factor(cerebros$intensidad_campo))-1),
              axi = (as.numeric(as.factor(cerebros$intensidad_campo))-1) * cerebros$edad)


init_list <- list(
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0),
  list(b0 = 0 ,b1 = 0, b2 = 0, b3 = 0, b4 = 0, b5 = 0, b6 = 0)
)


modeloMulti3 <- stan(
  file = "RegLogMultivarInterac.stan",
  data = lista,
  chains = 4,
  init = init_list,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

traceplot(modeloMulti3)









