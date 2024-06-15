library(ggplot2)
library(readr)
library(tidyverse)
library(kableExtra)
library(ggpubr)


cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))

cerebros = cerebros %>% mutate(diag = ifelse(diag == "HC", "HC", "MCI&AD"))

#Intensidad
sum(cerebros$intensidad_campo == 1.5)
sum(cerebros$intensidad_campo == 3)

ggplot(cerebros) + geom_bar(aes(x = as.factor(intensidad_campo)), fill = "gold", color = "#000000", width = .5)

#Sexo
sum(cerebros$sexo == "female")
sum(cerebros$sexo == "male")

ggplot(cerebros) + geom_bar(aes(x = sexo), fill = "gold", color = "#000000", width = .5)


#Resonador
sum(cerebros$resonador_fab == "GE")
sum(cerebros$resonador_fab == "Philips")
sum(cerebros$resonador_fab == "Siemens")


ggplot(cerebros) + geom_bar(aes(x = resonador_fab), fill = "gold", color = "#000000", width = .5)

#Diag
sum(cerebros$diag == "HC")
sum(cerebros$diag == "MCI&AD")
ggplot(cerebros) + geom_bar(aes(x = diag), fill = "gold", color = "#000000", width = .5)


#Continuas
sumary = summary(cerebros[,c(4,7:10)])

texto1 = gsub("Min.   :", "", sumary[1,])
minimo = as.numeric(texto1)
texto2 = gsub("1st Qu.:", "", sumary[2,])
q1 = as.numeric(texto2)
texto3 = gsub("Median :", "", sumary[3,])
mediana = as.numeric(texto3)
texto4 = gsub("Mean   :", "", sumary[4,])
media = as.numeric(texto4)
texto5 = gsub("3rd Qu.:", "", sumary[5,])
q3 = as.numeric(texto5)
texto6 = gsub("Max.   :", "", sumary[6,])
maximo = as.numeric(texto6)

matrices =matrix(c(minimo, q1, mediana, media, q3, maximo), nrow = 6, ncol = 5, byrow = T)
rownames(matrices) = c("Mínimo", "Q1", "Mediana", "Media", "Q3", "Máximo")
kable(matrices, col.names = c("Edad", "VHI", "VI", "ECSF", "VCF"))

#Diag*Sexo
DS1 = sum(cerebros$diag == "HC" & cerebros$sexo == "male")
DS2 = sum(cerebros$diag == "MCI&AD" & cerebros$sexo == "male")
DS4 = sum(cerebros$diag == "HC" & cerebros$sexo == "female")
DS5 = sum(cerebros$diag == "MCI&AD" & cerebros$sexo == "female")

DS = matrix(c(DS1, DS2, DS4, DS5), nrow = 2, ncol = 2, byrow = T)
row.names(DS) = c("Masculino", "Femenino")
kable(DS, col.names = c("HC", "MCI&AD"))


#Diag*Resonador
DR1 = sum(cerebros$diag == "HC" & cerebros$resonador_fab == "Siemens")
DR2 = sum(cerebros$diag == "MCI&AD" & cerebros$resonador_fab == "Siemens")
DR4 = sum(cerebros$diag == "HC" & cerebros$resonador_fab == "Philips")
DR5 = sum(cerebros$diag == "MCI&AD" & cerebros$resonador_fab == "Philips")
DR7 = sum(cerebros$diag == "HC" & cerebros$resonador_fab == "GE")          
DR8 = sum(cerebros$diag == "MCI&AD" & cerebros$resonador_fab == "GE")         

          

DR = matrix(c(DR7, DR8, DR4, DR5,DR1, DR2), nrow = 3, ncol = 2, byrow = T)
row.names(DR) = c("GE", "Philips", "Siemens")
kable(DR, col.names = c("HC", "MCI&AD"))

#Diag*Intensida

DI1 = sum(cerebros$diag == "HC" & cerebros$intensidad_campo == 1.5)
DI2 = sum(cerebros$diag == "MCI&AD" & cerebros$intensidad_campo == 1.5)
DI4 = sum(cerebros$diag == "HC" & cerebros$intensidad_campo == 3)
DI5 = sum(cerebros$diag == "MCI&AD" & cerebros$intensidad_campo == 3)

DI = matrix(c(DI1, DI2, DI4, DI5), nrow = 2, ncol = 2, byrow = T)
row.names(DI) = c("1.5T", "3.0T")
kable(DI, col.names = c("HC", "MCI&AD"))


#Diag*Continuas
plot1 = ggplot(cerebros) + geom_boxplot(aes(x = diag, y = edad))
plot2 = ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_subcx_hippocampus_volume)) +
  scale_y_continuous(name = "VHI") + scale_x_continuous(name = "Diagnostico")
plot3 = ggplot(cerebros) + geom_boxplot(aes(x = diag, y = xh_general_etiv_volume))+
  scale_y_continuous(name = "VI") + scale_x_continuous(name = "Diagnostico")
plot4 = ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_cortex_superiorfrontal_thickness))+
  scale_y_continuous(name = "ECSF") + scale_x_continuous(name = "Diagnostico")
plot5 = ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_cortex_fusiform_volume))+
  scale_y_continuous(name = "VCF") + scale_x_continuous(name = "Diagnostico")


ggarrange(plot2, plot3, plot4, plot5)


#Sexo*Continuas
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_cortex_fusiform_volume))

#Edad*Continuas
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_point(aes(x = edad, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_cortex_fusiform_volume))

#Intensidad*Continuas
ggplot(cerebros) + geom_boxplot(aes(x = as.factor(intensidad_campo), y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_boxplot(aes(x = as.factor(intensidad_campo), y = xh_general_etiv_volume))
ggplot(cerebros) + geom_boxplot(aes(x = as.factor(intensidad_campo), y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_boxplot(aes(x = as.factor(intensidad_campo), y = lh_cortex_fusiform_volume))


#Resonador*Continuas
ggplot(cerebros) + geom_boxplot(aes(x = resonador_fab, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_boxplot(aes(x = resonador_fab, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_boxplot(aes(x = resonador_fab, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_boxplot(aes(x = resonador_fab, y = lh_cortex_fusiform_volume))
