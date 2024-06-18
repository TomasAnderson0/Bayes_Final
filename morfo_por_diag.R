library(rstan)
library(readr)
library(dplyr)
library(bayesplot)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"), lh_subcx_hippocampus_volume = lh_subcx_hippocampus_volume/1000, xh_general_etiv_volume = xh_general_etiv_volume/100000, lh_cortex_fusiform_volume = lh_cortex_fusiform_volume/1000, intensidad_campo = ifelse(intensidad_campo == 1.5, "1.5T", "3T"), diag = ifelse(diag == "female", "Femenino", "Masculino"))



#VHI


lista_vhi_diag <- list(N = nrow(cerebros),
                  masc = ifelse(cerebros$diag == "HC", 1, 0),
                  fem = ifelse(cerebros$diag == "HC", 0, 1),
                  vhi = cerebros$lh_subcx_hippocampus_volume
)





modelo_vhi_diag <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vhi_diag,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_vhi_diag

traceplot(modelo_vhi_diag)

b0_vhi_diag = extract(modelo_vhi_diag,c("b0"))[[1]]
b1_vhi_diag = extract(modelo_vhi_diag,c("b1"))[[1]]
sigma_vhi_diag =extract(modelo_vhi_diag,c("sigma"))[[1]]

vhi_diag_df = data.frame(x = c(b0_vhi_diag, b1_vhi_diag), cual = rep(c("HC","MCI&AD"), each = length(b0_vhi_diag)))

set.seed(123)

masc_vhi_diag = rnorm(length(b0_vhi_diag), b0_vhi_diag, sigma_vhi_diag)
fem_vhi_diag = rnorm(length(b1_vhi_diag), b1_vhi_diag, sigma_vhi_diag)
vhi_diag_ppp = data.frame(x = c(masc_vhi_diag, fem_vhi_diag), cual = rep(c("HC", "MCI&AD"), each = length(masc_vhi_diag)) )


#VI



lista_vi_diag <- list(N = nrow(cerebros),
                 masc = ifelse(cerebros$diag == "HC", 1, 0),
                 fem = ifelse(cerebros$diag == "HC", 0, 1),
                 vhi = cerebros$xh_general_etiv_volume
)




modelo_vi_diag <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vi_diag,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_vi_diag

traceplot(modelo_vi_diag)

b0_vi_diag = extract(modelo_vi_diag,c("b0"))[[1]]
b1_vi_diag = extract(modelo_vi_diag,c("b1"))[[1]]
sigma_vi_diag =extract(modelo_vhi_diag,c("sigma"))[[1]]

vi_diag_df = data.frame(x = c(b0_vi_diag, b1_vi_diag), cual = rep(c("HC","MCI&AD"), each = length(b0_vi_diag)))

set.seed(123)

masc_vi_diag = rnorm(length(b0_vi_diag), b0_vi_diag, sigma_vi_diag)
fem_vi_diag = rnorm(length(b1_vi_diag), b1_vi_diag, sigma_vi_diag)

vi_diag_ppp = data.frame(x = c(masc_vi_diag, fem_vi_diag), cual = rep(c("HC","MCI&AD"), each = length(masc_vi_diag)) )


#ECSF



lista_ecsf_diag <- list(N = nrow(cerebros),
                   masc = ifelse(cerebros$diag == "HC", 1, 0),
                   fem = ifelse(cerebros$diag == "HC", 0, 1),
                   vhi = cerebros$lh_cortex_superiorfrontal_thickness
)



modelo_ecsf_diag <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_ecsf_diag,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_ecsf_diag

traceplot(modelo_ecsf_diag)

b0_ecsf_diag = extract(modelo_ecsf_diag,c("b0"))[[1]]
b1_ecsf_diag = extract(modelo_ecsf_diag,c("b1"))[[1]]
sigma_ecsf_diag =extract(modelo_ecsf_diag,c("sigma"))[[1]]

ecsf_diag_df = data.frame(x = c(b0_ecsf_diag, b1_ecsf_diag), cual = rep(c("HC","MCI&AD"), each = length(b0_ecsf_diag)))


set.seed(123)

masc_ecsf_diag = rnorm(length(b0_ecsf_diag), b0_ecsf_diag, sigma_ecsf_diag)
fem_ecsf_diag = rnorm(length(b1_ecsf_diag), sigma_ecsf_diag)

ecsf_diag_ppp = data.frame(x = c(masc_ecsf_diag, fem_ecsf_diag), cual = rep(c("HC","MCI&AD"), each = length(masc_ecsf_diag)) )






#VCF



lista_vcf_diag <- list(N = nrow(cerebros),
                  masc = ifelse(cerebros$diag == "HC", 1, 0),
                  fem = ifelse(cerebros$diag == "HC", 0, 1),
                  vhi = cerebros$lh_cortex_fusiform_volume
)



modelo_vcf_diag <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vcf_diag,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)


modelo_vcf_diag

traceplot(modelo_vcf_diag)


b0_vcf_diag = extract(modelo_vcf_diag,c("b0"))[[1]]
b1_vcf_diag = extract(modelo_vcf_diag,c("b1"))[[1]]
sigma_vcf_diag =extract(modelo_vcf_diag,c("sigma"))[[1]]

vcf_diag_df = data.frame(x = c(b0_vcf_diag, b1_vcf_diag), cual = rep(c("HC","MCI&AD"), each = length(b0_vcf_diag)))

set.seed(123)

masc_vcf_diag = rnorm(length(b0_vcf_diag), b0_vcf_diag, sigma_vcf_diag)
fem_vcf_diag = rnorm(length(b1_vcf_diag), b1_vcf_diag, sigma_vcf_diag)

vcf_diag_ppp = data.frame(x = c(masc_vcf_diag, fem_vcf_diag), cual = rep(c("HC","MCI&AD"), each = length(masc_vcf_diag)) )



#Graficos

morfo_diag_ppp = rbind(vhi_diag_ppp, vi_diag_ppp, ecsf_diag_ppp, vcf_diag_ppp)
morfo_diag_ppp$morfo = factor(rep(c("VHI", "VI", "ECSF", "VCF"), each = 10800), levels = c("VHI", "VI", "ECSF", "VCF"))


ggplot(morfo_diag_ppp) + geom_density(aes(x = x, fill = cual), alpha = .5) + facet_wrap(~morfo, scales = "free") +
  scale_y_continuous(name = "Densidad") + theme(legend.position = "top", legend.title = element_blank()) + scale_x_continuous(name = "")

morfo_diag_mean = rbind(vhi_diag_df, vi_diag_df, ecsf_diag_df, vcf_diag_df)
morfo_diag_mean$morfo = factor(rep(c("VHI", "VI", "ECSF", "VCF"), each = 10800), levels = c("VHI", "VI", "ECSF", "VCF"))

ggplot(morfo_diag_mean) + geom_density(aes(x = x, fill = cual), alpha = .5) + facet_wrap(~morfo, scales = "free") +
  scale_y_continuous(name = "Densidad") + theme(legend.position = "top", legend.title = element_blank()) + scale_x_continuous(name = "")

