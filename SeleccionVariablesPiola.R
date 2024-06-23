library(rstan)
library(readr)
library(dplyr)
library(loo)
library(kableExtra)
library(ggdist)
library(ggplot2)

cerebros <- read_csv("cerebros.csv")
cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"), lh_subcx_hippocampus_volume = lh_subcx_hippocampus_volume/1000, xh_general_etiv_volume = xh_general_etiv_volume/100000, lh_cortex_fusiform_volume = lh_cortex_fusiform_volume/1000, intensidad_campo = factor(ifelse(intensidad_campo == 1.5, "1.5T", "3T"), levels = c("3T", "1.5T")), sexo = factor(ifelse(sexo == "female", "Femenino", "Masculino"), levels = c("Masculino", "Femenino")), resonador_fab = factor(resonador_fab, levels = c("Siemens", "Philips", "GE")))



lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128]
)

modeloMulti5 <- stan(
  file = "RegLogMultivar3.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)


looM5 <- loo(modeloMulti5)


###############################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = scale(cerebros$lh_cortex_fusiform_volume)[1:128]
)

modeloMultivcf <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loocomp1 <- loo(modeloMultivcf)
###############################################################


lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = scale(cerebros$xh_general_etiv_volume)[1:128]
)

modeloMultivi <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loocomp2 <- loo(modeloMultivi)

###############################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = scale(cerebros$edad)[1:128]
)

modeloMultiedad <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loocomp3 <- loo(modeloMultiedad)
###############################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = (as.numeric(as.factor(cerebros$intensidad_campo))-1)
)

modeloMultiinte <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)
loocomp4 <- loo(modeloMultiinte)

###############################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte =  (as.numeric(as.factor(cerebros$sexo))-1)
)

modeloMultisexo <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loocomp5 <- loo(modeloMultisexo)

###############################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = modeloresonador$res1,
              axi = modeloresonador$res2
)


modeloMultires <- stan(
  file = "RegLogMultivarInterac2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loocomp6 <- loo(modeloMultires)

########################################################################

lista <- list(N = nrow(cerebros),
              diag = (as.numeric(as.factor(cerebros$diag)) - 1),
              age = scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128],
              vhi = scale(cerebros$lh_subcx_hippocampus_volume)[1:128],
              inte = scale(cerebros$lh_subcx_hippocampus_volume)[1:128]*scale(cerebros$lh_cortex_superiorfrontal_thickness)[1:128]
)

modeloMultiinterac <- stan(
  file = "RegLogMultivar2.stan",
  data = lista,
  chains = 4,
  warmup = 150,
  iter = 1500,
  control = list(adapt_delta = 0.99),
  seed = 1997
)

loointerac = loo(modeloMultiinterac)

loovhi = loo(modelo_VHI)
looecsf = loo(modelo_ecsf)

########################################################################







l1 = loo_compare(loocomp1,looM5)[2,1:2]
l2 = -loo_compare(loocomp2,looM5)[2,1:2]
l3 =loo_compare(loocomp3,looM5)[2,1:2]
l4 =-loo_compare(loocomp4,looM5)[2,1:2]
l5 =-loo_compare(loocomp5,looM5)[2,1:2]
l6 =-loo_compare(loocomp6,looM5)[2,1:2]
l7 =loo_compare(looecsf,looM5)[2,1:2]
l8 =loo_compare(loovhi,looM5)[2,1:2]
l9 =loo_compare(loointerac,looM5)[2,1:2]

i1 = c(l1[1]-l1[2]*1.96,l1[1]+l1[2]*1.96)
i2 = c(l2[1]-l2[2]*1.96,l2[1]+l2[2]*1.96)
i3 = c(l3[1]-l3[2]*1.96,l3[1]+l3[2]*1.96)
i4 = c(l4[1]-l4[2]*1.96,l4[1]+l4[2]*1.96)
i5 = c(l5[1]-l5[2]*1.96,l5[1]+l5[2]*1.96)
i6 = c(l6[1]-l6[2]*1.96,l6[1]+l6[2]*1.96)
i7 = c(l7[1]-l7[2]*1.96,l7[1]+l7[2]*1.96)
i8 = c(l8[1]-l8[2]*1.96,l8[1]+l8[2]*1.96)
i9 = c(l9[1]-l9[2]*1.96,l9[1]+l9[2]*1.96)

ic = data.frame(x = c(i1,i2,i3,i4,i5,i6,i7,i8,i9),y = factor(rep(c("Con VCF","Con VI","Con Edad","Con Intensidad","Con Sexo","Con Resonador","Sin VHI","Sin ECSF","Con Interacción"),each = 2), levels = c("Con Interacción","Sin ECSF","Sin VHI","Con Resonador","Con Sexo","Con Intensidad","Con Edad","Con VI","Con VCF")))


####################################################
ic1 = data.frame(x = c(mean(i1),mean(i2),mean(i3),mean(i4),mean(i5),mean(i6),mean(i7),mean(i8),mean(i9)),y = factor(rep(c("Con VCF","Con VI","Con Edad","Con Intensidad","Con Sexo","Con Resonador","Sin VHI","Sin ECSF","Con Interacción")), levels = c("Con Interacción","Sin ECSF","Sin VHI","Con Resonador","Con Sexo","Con Intensidad","Con Edad","Con VI","Con VCF")))


ic_comp =ggplot(ic) + geom_line(aes(x = x, y = y, group = y), color = "#ff0303") +  geom_vline(xintercept = 0, linetype = 2) +
  geom_point(data = ic1,aes(x = x, y = y, group = y) , color = "#050505") + labs(x = "Diferencia de ELPPD", y = "Modelos") + theme_minimal(base_size = 20)




cadenas = modeloMulti5 %>%  setNames(c("Intercepto", "VHI", "ECSF", rep("1",385))) %>% 
  traceplot(pars = c("b0", "b1", "b2")) + labs(colour = "Cadenas") + theme(text = element_text(size = 20))

b0.df = as.data.frame(extract(modeloMulti5, "b0"))
b1.df = as.data.frame(extract(modeloMulti5, "b1"))
b2.df = as.data.frame(extract(modeloMulti5, "b2"))

b0p = ggplot(b0.df) + geom_density(aes(x = b0), fill = "#38e28c", color = "#000000") + labs(y = "Densidad", x = expression(beta[0])) + theme_minimal(base_size = 20) 
b1p = ggplot(b1.df) + geom_density(aes(x = 1000*b1/sd(cerebros$lh_subcx_hippocampus_volume)), fill = "#38e28c", color = "#000000") + labs(y = "Densidad", x = expression(beta[VHI])) + theme_minimal(base_size = 20) 
b2p = ggplot(b2.df) + geom_density(aes(x = b2/sd(cerebros$lh_cortex_superiorfrontal_thickness)), fill = "#38e28c", color = "#000000") + labs(y = "Densidad", x = expression(beta[ECSF])) + theme_minimal(base_size = 20) + scale_x_continuous(breaks = seq(-13,-4,3))

posterior = ggarrange(b0p, b1p, b2p, ncol = 3)








esbozo <- data.frame(rstan::extract(modeloMulti5, c("b0", "b1", "b2")))
seq.vhi = (seq(1800,5600,10)-mean(cerebros$lh_subcx_hippocampus_volume))/sd(cerebros$lh_subcx_hippocampus_volume)
sample1 = esbozo[sample(1:nrow(esbozo), size = 500),]
data_sample1 = as.data.frame(matrix(nrow = 381*500, ncol = 3))
for (i in 1:500) {
  data_sample[((i-1)*(381)+1):((381)*i),] = data.frame(pi = c(rep(sample1$b0[i], length(seq.vhi)) + sample1$b1[i]*seq.vhi), grupo = rep(i,each = 381), edad = rep(seq(1.8,5.6,.01)))
}

exp(mean(1000*esbozo$b1/sd(cerebros$lh_subcx_hippocampus_volume)))

pps1 = ggplot(data.frame(x = c(0, 5), y = c(0, 1))) + stat_lineribbon(data = data_sample, aes(x = V3, y = exp(V1)/(1+exp(V1)), fill_ramp = after_stat(level)), fill = "#38e28c", alpha = .8) + 
  scale_x_continuous(breaks = seq(1.8,5.6,.5), limits = c(1.8,5.6)) + labs(y = expression(pi), x = expression(paste("VHI (", {cm^3}, ")"))) + theme_minimal(base_size = 20) + theme(legend.position = "none") + 
  geom_text(x = 4.3, y = 0.9, label = expression(paste(e^{hat(E)(beta[VHI])}==0.20)), size = 10)


seq.ecsf = (seq(1.8,3.2,.01)-mean(cerebros$lh_cortex_superiorfrontal_thickness))/sd(cerebros$lh_cortex_superiorfrontal_thickness)
sample2 = esbozo[sample(1:nrow(esbozo), size = 500),]
data_sample2 = as.data.frame(matrix(nrow = 141*500, ncol = 3))
for (i in 1:500) {
  data_sample2[((i-1)*(141)+1):((141)*i),] = data.frame(pi = c(rep(sample2$b0[i], length(seq.ecsf)) + sample2$b2[i]*seq.ecsf), grupo = rep(i,each = length(seq.ecsf)), edad = rep(seq(1.8,3.2,.01)))
}

exp(mean(esbozo$b2/sd(cerebros$lh_cortex_superiorfrontal_thickness))/10)


pps2 = ggplot(data.frame(x = c(0, 10), y = c(0, 10))) + stat_lineribbon(data = data_sample2, aes(x = V3, y = exp(V1)/(1+exp(V1)), fill_ramp = after_stat(level)), fill = "#38e28c", alpha = .8) + 
  scale_x_continuous(breaks = seq(1.8,3.2,length.out = 8), limits = c(1.8,3.2)) + labs(y = expression(pi), x = expression(paste("ECSF (", {mm}, ")"))) + theme_minimal(base_size = 20) + theme(legend.position = "none") + scale_y_continuous(limits = c(0,1)) +
  geom_text(x = 2.8, y = 0.9, label = expression(paste(e^{hat(E)(beta[ECSF])%.%0.1}==0.47)), size = 8)

set.seed(45)

sample3 = esbozo[sample(1:nrow(esbozo), size = 500),]
data_sample3 = as.data.frame(matrix(nrow = 128*500, ncol = 3))
df.ppc = as.data.frame(matrix(0,nrow = 500, ncol = 128))
for (i in 1:500) {
  df.ppc[i,] = (rep(sample3[i,1],128) + sample3[i, 2] * scale(cerebros$lh_subcx_hippocampus_volume)[1:128] + sample3[i, 3] * scale(cerebros$lh_cortex_superiorfrontal_thickness[1:128]))
  
}
n.ppc = as.data.frame(matrix(0,nrow = 500, ncol = 1))
for (i in 1:500) {
n.ppc[i,] = sum(rbinom(128,1,as.numeric(exp(df.ppc[1,])/(exp(df.ppc[1,])+1))))
}
n.ppc

data.frame(x = n.ppc)

ppc = ggplot(cerebros) + geom_bar(aes(x = diag), fill = "#5aeeac" , width = .5) + stat_summary(data = n.ppc, mapping = aes(x = 2, y = V1), fun.data = mean_sdl, color = "#047e46") +  stat_summary(data = n.ppc, mapping = aes(x = 1, y = 128-V1), fun.data = mean_sdl, color = "#047e46") + labs(x = "Diagnóstico", y = "Frecuencia absoluta") + theme_minimal(base_size = 20) +
  labs(caption = "Diagnóstico observado vs intervalo de valores predichos")  +  theme(plot.caption = element_text(size = 20,hjust = .5, color = "#555555"))

a = data.frame(x = c(median(ye0),median(ye1),median(ye2),median(ye3)))
a$cual = c("as", "S = Masculino\nI=1.5T\nR=GE", "S = Femenino\nI=3T\nR=GE","S = Femenino\nI=1.5T\nR=Philips")
plw = ggplot(a) + geom_bar(aes(x = cual, y = x), stat = "identity", fill = "#5aeeac") + labs(x = "Nivel", y = expression(hat(pi))) + geom_point(aes(x = cual, y = x), color = "#047e46")+ scale_y_continuous(limits = c(0, 1)) + geom_line(data = suma, aes(y = ye, x = re+1, group = re), color = "#047e46") + scale_x_discrete(labels = c(expression(beta[0]), expression(beta[0]+beta["S"]), expression(beta[0]+beta["I"]+beta["S"]), expression(beta[0]+beta["R"]+beta["I"]+beta["S"]))) + theme_minimal(base_size = 20)








save(ic_comp, ppc,cadenas,posterior, pps1, pps2, file = "ic_comp.RData")
