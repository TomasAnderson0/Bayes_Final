library(ggplot2)
library(readr)
library(tidyverse)
library(kableExtra)


cerebros <- read_csv("cerebros.csv")
cerebros$diag = factor(cerebros$diag, levels = c("HC", "MCI", "AD"))



#Intensidad
I1 = sum(cerebros$intensidad_campo == 1.5)/128
I2 = sum(cerebros$intensidad_campo == 3)/128
I = c()

#Sexo
sum(cerebros$sexo == "female")/128
sum(cerebros$sexo == "male")/128

#Resonador
sum(cerebros$resonador_fab == "GE")/128
sum(cerebros$resonador_fab == "Philips")/128
sum(cerebros$resonador_fab == "Siemens")/128

#Diag
sum(cerebros$diag == "HC")/128
sum(cerebros$diag == "MCI")/128
sum(cerebros$diag == "AD")/128

#Categoricas
summary(cerebros[,c(4,7:10)])



#Diag*Sexo
DS1 = sum(cerebros$diag == "HC" & cerebros$sexo == "male")
DS2 = sum(cerebros$diag == "MCI" & cerebros$sexo == "male")
DS3 = sum(cerebros$diag == "AD" & cerebros$sexo == "male")
DS4 = sum(cerebros$diag == "HC" & cerebros$sexo == "female")
DS5 = sum(cerebros$diag == "MCI" & cerebros$sexo == "female")
DS6 = sum(cerebros$diag == "AD" & cerebros$sexo == "female")

DS = matrix(c(DS1, DS2, DS3, DS4, DS5, DS6), nrow = 2, ncol = 3, byrow = T)
row.names(DS) = c("Masculino", "Femenino")
kable(DS, col.names = c("HC", "MCI", "AD"))


#Diag*Intensidad
DI1 = sum(cerebros$diag == "HC" & cerebros$intensidad_campo == 1.5)
DI2 = sum(cerebros$diag == "MCI" & cerebros$intensidad_campo == 1.5)
DI3 = sum(cerebros$diag == "AD" & cerebros$intensidad_campo == 1.5)
DI4 = sum(cerebros$diag == "HC" & cerebros$intensidad_campo == 3)
DI5 = sum(cerebros$diag == "MCI" & cerebros$intensidad_campo == 3)
DI6 = sum(cerebros$diag == "AD" & cerebros$intensidad_campo == 3)

DI = matrix(c(DI1, DI2, DI3, DI4, DI5, DI6), nrow = 2, ncol = 3, byrow = T)
row.names(DI) = c("1.5T", "3.0T")
kable(DI, col.names = c("HC", "MCI", "AD"))



#Diag*Categoricas
ggplot(cerebros) + geom_boxplot(aes(x = diag, y = edad))
ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_boxplot(aes(x = diag, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_boxplot(aes(x = diag, y = lh_cortex_fusiform_volume))

#Sexo*Categoricas
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_boxplot(aes(x = sexo, y = lh_cortex_fusiform_volume))

#Edad*Categoricas
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_subcx_hippocampus_volume))
ggplot(cerebros) + geom_point(aes(x = edad, y = xh_general_etiv_volume))
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_cortex_superiorfrontal_thickness))
ggplot(cerebros) + geom_point(aes(x = edad, y = lh_cortex_fusiform_volume))


