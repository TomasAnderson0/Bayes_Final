library(rstan)
library(readr)
library(dplyr)
library(bayesplot)

cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"), lh_subcx_hippocampus_volume = lh_subcx_hippocampus_volume/1000, xh_general_etiv_volume = xh_general_etiv_volume/100000, lh_cortex_fusiform_volume = lh_cortex_fusiform_volume/1000, intensidad_campo = ifelse(intensidad_campo == 1.5, "1.5T", "3T"), sexo = ifelse(sexo == "female", "Femenino", "Masculino"))



#VHI


lista_vhi_sexo <- list(N = nrow(cerebros),
              masc = ifelse(cerebros$sexo == "Femenino", 0, 1),
              fem = ifelse(cerebros$sexo == "Femenino", 1, 0),
              vhi = cerebros$lh_subcx_hippocampus_volume
)





modelo_vhi_sexo <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vhi_sexo,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_vhi_sexo

traceplot(modelo_vhi_sexo)

b0_vhi_sexo = extract(modelo_vhi_sexo,c("b0"))[[1]]
b1_vhi_sexo = extract(modelo_vhi_sexo,c("b1"))[[1]]
sigma_vhi_sexo =extract(modelo_vhi_sexo,c("sigma"))[[1]]

vhi_sexo_df = data.frame(x = c(b0_vhi_sexo, b1_vhi_sexo), cual = rep(c("Masculino","Femenino"), each = length(b0_vhi_sexo)))

set.seed(949)

masc_vhi_sexo = rnorm(length(b0_vhi_sexo), b0_vhi_sexo, sigma_vhi_sexo)
fem_vhi_sexo = rnorm(length(b1_vhi_sexo), b1_vhi_sexo, sigma_vhi_sexo)
vhi_sexo_ppp = data.frame(x = c(masc_vhi_sexo, fem_vhi_sexo), cual = rep(c("Masculino","Femenino"), each = length(masc_vhi_sexo)) )


#VI



lista_vi_sexo <- list(N = nrow(cerebros),
                  masc = ifelse(cerebros$sexo == "Femenino", 0, 1),
                  fem = ifelse(cerebros$sexo == "Femenino", 1, 0),
                  vhi = cerebros$xh_general_etiv_volume
)




modelo_vi_sexo <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vi_sexo,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_vi_sexo

traceplot(modelo_vi_sexo)

b0_vi_sexo = extract(modelo_vi_sexo,c("b0"))[[1]]
b1_vi_sexo = extract(modelo_vi_sexo,c("b1"))[[1]]
sigma_vi_sexo =extract(modelo_vi_sexo,c("sigma"))[[1]]

vi_sexo_df = data.frame(x = c(b0_vi_sexo, b1_vi_sexo), cual = rep(c("Masculino","Femenino"), each = length(b0_vi_sexo)))


set.seed(949)


masc_vi_sexo = rnorm(length(b0_vi_sexo), b0_vi_sexo, sigma_vi_sexo)
fem_vi_sexo = rnorm(length(b1_vi_sexo), b1_vi_sexo, sigma_vi_sexo)

vi_sexo_ppp = data.frame(x = c(masc_vi_sexo, fem_vi_sexo), cual = rep(c("Masculino","Femenino"), each = length(masc_vi_sexo)) )


#ECSF


lista_ecsf_sexo <- list(N = nrow(cerebros),
                  masc = ifelse(cerebros$sexo == "Femenino", 0, 1),
                  fem = ifelse(cerebros$sexo == "Femenino", 1, 0),
                  vhi = cerebros$lh_cortex_superiorfrontal_thickness
)



modelo_ecsf_sexo <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_ecsf_sexo,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

modelo_ecsf_sexo

traceplot(modelo_ecsf_sexo)

b0_ecsf_sexo = extract(modelo_ecsf_sexo,c("b0"))[[1]]
b1_ecsf_sexo = extract(modelo_ecsf_sexo,c("b1"))[[1]]
sigma_ecsf_sexo =extract(modelo_ecsf_sexo,c("sigma"))[[1]]


ecsf_sexo_df = data.frame(x = c(b0_ecsf_sexo, b1_ecsf_sexo), cual = rep(c("Masculino","Femenino"), each = length(b0_ecsf_sexo)))


set.seed(949)


masc_ecsf_sexo = rnorm(length(b0_ecsf_sexo), b0_ecsf_sexo, sigma_ecsf_sexo)
fem_ecsf_sexo = rnorm(length(b1_ecsf_sexo), b1_ecsf_sexo, sigma_ecsf_sexo)

ecsf_sexo_ppp = data.frame(x = c(masc_ecsf_sexo, fem_ecsf_sexo), cual = rep(c("Masculino","Femenino"), each = length(masc_ecsf_sexo)) )






#VCF



lista_vcf <- list(N = nrow(cerebros),
                  masc = ifelse(cerebros$sexo == "Femenino", 0, 1),
                  fem = ifelse(cerebros$sexo == "Femenino", 1, 0),
                  vhi = cerebros$lh_cortex_fusiform_volume
)



lista_vcf_sexo <- stan(
  file = "Modelo_vhi_sexo.stan",
  data = lista_vcf,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)


modelo_vcf_sexo

traceplot(modelo_vcf_sexo)


b0_vcf_sexo = extract(modelo_vcf_sexo,c("b0"))[[1]]
b1_vcf_sexo = extract(modelo_vcf_sexo,c("b1"))[[1]]
sigma_vcf_sexo =extract(modelo_vcf_sexo,c("sigma"))[[1]]

vcf_sexo_df = data.frame(x = c(b0_vcf_sexo, b1_vcf_sexo), cual = rep(c("Masculino","Femenino"), each = length(b0_vcf_sexo)))

set.seed(949)


masc_vcf_sexo = rnorm(length(b0_vcf_sexo), b0_vcf_sexo, sigma_vcf_sexo)
fem_vcf_sexo = rnorm(length(b1_vcf_sexo), b1_vcf_sexo, sigma_vcf_sexo)
vcf_sexo_ppp = data.frame(x = c(masc_vcf_sexo, fem_vcf_sexo), cual = rep(c("Masculino","Femenino"), each = length(masc_vcf_sexo)) )



#Graficos

morfo_sexo_ppp = rbind(vhi_sexo_ppp, vi_sexo_ppp, ecsf_sexo_ppp, vcf_sexo_ppp)
morfo_sexo_ppp$morfo = factor(rep(c("VHI", "VI", "ECSF", "VCF"), each = 10800), levels = c("VHI", "VI", "ECSF", "VCF"))


ggplot(morfo_sexo_ppp) + geom_density(aes(x = x, fill = cual), alpha = .5) + facet_wrap(~morfo, scales = "free") +
  scale_y_continuous(name = "Densidad") + theme(legend.position = "top", legend.title = element_blank()) + scale_x_continuous(name = "")

morfo_sexo_mean = rbind(vhi_sexo_df, vi_sexo_df, ecsf_sexo_df, vcf_sexo_df)
morfo_sexo_mean$morfo = factor(rep(c("VHI", "VI", "ECSF", "VCF"), each = 10800), levels = c("VHI", "VI", "ECSF", "VCF"))

ggplot(morfo_sexo_mean) + geom_density(aes(x = x, fill = cual), alpha = .5) + facet_wrap(~morfo, scales = "free") +
  scale_y_continuous(name = "Densidad") + theme(legend.position = "top", legend.title = element_blank()) + scale_x_continuous(name = "")

