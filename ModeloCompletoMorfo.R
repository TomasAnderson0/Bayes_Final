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
posteriorcomp <- data.frame(b1 = c(posteriorcomp$beta1, posteriorcomp$beta2, posteriorcomp$beta3,
                                   posteriorcomp$beta4, posteriorcomp$beta5, posteriorcomp$beta6,
                                   posteriorcomp$beta7))
posteriorcomp <- posteriorcomp %>% 
  mutate(variable = c(rep("Edad", 5400), rep("Sexo", 5400), rep("Resonador GE", 5400), rep("Resonador Philips", 5400), rep("Intensidad", 5400)))

reglin1 = ggplot(posteriorcomp) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = c("#057057","#057057","#F32835","#F32835","#057057")) + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal() +
  ggtitle("VHI") + theme(plot.title = element_text(hjust = .5))


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

posteriorcomp2 <- data.frame(rstan::extract(modeloregcomp2, c("beta1", "beta2", "beta3", "beta4", "beta5")))
posteriorcomp2 <- data.frame(b1 = c(posteriorcomp2$beta1, posteriorcomp2$beta2, posteriorcomp2$beta3,
                                   posteriorcomp2$beta4, posteriorcomp2$beta5, posteriorcomp2$beta6,
                                   posteriorcomp2$beta7))
posteriorcomp2 <- posteriorcomp2 %>% 
  mutate(variable = c(rep("Edad", 5400), rep("Sexo", 5400), rep("Resonador GE", 5400), rep("Resonador Philips", 5400), rep("Intensidad", 5400)))

reglin2 = ggplot(posteriorcomp2) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = c("#F32835","#F32835","#F32835","#F32835","#057057")) + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal() +
  ggtitle("VI") + theme(plot.title = element_text(hjust = .5))

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

traceplot(modeloregcomp3)

posteriorcomp3 <- data.frame(rstan::extract(modeloregcomp3, c("beta1", "beta2", "beta3", "beta4", "beta5")))
posteriorcomp3 <- data.frame(b1 = c(posteriorcomp3$beta1, posteriorcomp3$beta2, posteriorcomp3$beta3,
                                    posteriorcomp3$beta4, posteriorcomp3$beta5, posteriorcomp3$beta6,
                                    posteriorcomp3$beta7))
posteriorcomp3 <- posteriorcomp3 %>% 
  mutate(variable = c(rep("Edad", 5400), rep("Sexo", 5400), rep("Resonador GE", 5400), rep("Resonador Philips", 5400), rep("Intensidad", 5400)))

reglin3 = ggplot(posteriorcomp3) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = c("#057057","#057057","#F32835","#F32835","#F32835")) + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal() +
  ggtitle("ECSF") + theme(plot.title = element_text(hjust = .5))

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

posteriorcomp4 <- data.frame(rstan::extract(modeloregcomp4, c("beta1", "beta2", "beta3", "beta4", "beta5")))
posteriorcomp4 <- data.frame(b1 = c(posteriorcomp4$beta1, posteriorcomp4$beta2, posteriorcomp4$beta3,
                                    posteriorcomp4$beta4, posteriorcomp4$beta5, posteriorcomp4$beta6,
                                    posteriorcomp4$beta7))
posteriorcomp4 <- posteriorcomp4 %>% 
  mutate(variable = c(rep("Edad", 5400), rep("Sexo", 5400), rep("Resonador GE", 5400), rep("Resonador Philips", 5400), rep("Intensidad", 5400)))

reglin4 = ggplot(posteriorcomp4) + stat_summary(aes(y = variable, x = b1), fun.data = mean_sdl, color = c("#057057","#057057","#F32835","#F32835","#057057")) + 
  geom_vline(xintercept = 0, linetype = 2) + labs(y = "Variable", x = "Coeficiente") + theme_minimal() +
  ggtitle("VCF") + theme(plot.title = element_text(hjust = .5))

save(reglin1, reglin2, reglin3, reglin4, file = "reglin.RData")





