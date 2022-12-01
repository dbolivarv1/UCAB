################################################
####              Prueba 2                  ####
####         Daniel Bolívar Véliz           ####
################################################

# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(ggpubr)

# Parte 1: Importar Datos ----------------------------------------------------------

# Importamos el archivo desde el directorio donde se encuentra


getwd()
setwd('C:/Users/dabv1/OneDrive/Documents/Coding/R/Data Science UCAB')

data_children <- read.csv("C:/Users/dabv1/OneDrive/Documents/Coding/R/Data Science UCAB/Prueba 2/share-of-children-underweight.csv")
data_c_e <- read.csv("C:/Users/dabv1/OneDrive/Documents/Coding/R/Data Science UCAB/Prueba 2/prevalence-of-severe-food-insecurity-by-region.csv")

regiones <- c('Latin America and Caribbean','East Asia and Pacific','Middle East and North Africa','North America')
regiones_2 <- c('Latin America and the Caribbean (FAO)','Eastern Asia (FAO)','Northern Africa (FAO)','Northern America (FAO)' )
years <- c('2015','2016','2017','2018','2019')



# Parte 2: Limpieza de datos -------------------------------------------------------

#data children

tabla_children <- data_children %>% 
  filter(Year %in% years & Entity %in% regiones) %>% 
  subset(select = - Code)

tabla_children <- rename(tabla_children, 'Porcentaje de niños con peso insuficiente' = Prevalence.of.underweight..weight.for.age....of.children.under.5., 
                'Region'=Entity, 'Periodo' = Year)

tabla_children$`Porcentaje de niños con peso insuficiente` <- round(tabla_children$`Porcentaje de niños con peso insuficiente`, digits = 1)

#Cambio de nombre de las variables

for(i in 1:nrow(tabla_children)){
  if(tabla_children$Region[i]=='Latin America and Caribbean'){
    tabla_children$Region[i] <- 'América Latina y el Caribe'
  } else if (tabla_children$Region[i] == 'East Asia and Pacific') {
      tabla_children$Region[i] <- 'Asia del Este y Pacífico'
  } else if (tabla_children$Region[i] == 'Middle East and North Africa'){
      tabla_children$Region[i] <- 'Medio Oriente y África del Norte'
  } else {
    tabla_children$Region[i] <- 'Norteamérica'
  }
}


#data food insecurity

# Se eligen las regiones y años a estudiar, se elimina la columna code, se renombra una columna a "PP" 

tabla_food <- data_c_e %>% 
  filter(Year %in% years & Entity %in% regiones_2) %>% 
  subset(select = - Code)

tabla_food <- tabla_food <- rename(tabla_food, 'Porcentaje de poblacion con inseguridad alimentaria' =Prevalence.of.severe.food.insecurity.in.the.total.population..percent...annual.value....00210400....Value...006121....percent,
                           'Region'=Entity, 'Periodo' = Year) 
  
#Cambio de nombre de las variables

for(i in 1:nrow(tabla_food)){
  if(tabla_food$Region[i]=='Latin America and the Caribbean (FAO)'){
    tabla_food$Region[i] <- 'América Latina y el Caribe'
  } else if (tabla_food$Region[i] == 'Eastern Asia (FAO)') {
    tabla_food$Region[i] <- 'Asia del Este y Pacífico'
  } else if (tabla_food$Region[i] == 'Northern Africa (FAO)'){
    tabla_food$Region[i] <- 'Medio Oriente y África del Norte'
  } else {
    tabla_food$Region[i] <- 'Norteamérica'
  }
}


# Union base de datos
df_data <- full_join(tabla_food, tabla_children)


### Graficos ---------


# Time Series Plot - Children Underweight---------------

p1 <- df_data %>% 
  ggplot(aes(x = Periodo,
           y = round(df_data$`Porcentaje de niños con peso insuficiente`, digits = 1),
           col = Region)) +
  scale_x_continuous(name = NULL)+
  scale_y_continuous(name= 'Niños con insuficiencia de peso (%)',breaks = seq(0,7,2), limits = c(0,7))+
  geom_line(size = 1 ,aes(linetype=Region))+
  geom_text(label= round(df_data$`Porcentaje de niños con peso insuficiente`, digits = 1),
            nudge_y = 0.3)+
  geom_point(size = 3)+
  ggtitle("Porcentaje de niños con insuficiencia alimentaria por región") +
  theme(plot.title = element_text(hjust = 0.5))



# Time Series Plot - Food Insecurity--------------

p2 <- df_data %>% 
  ggplot(aes(x = Periodo,
             y = round(df_data$`Porcentaje de poblacion con inseguridad alimentaria`, digits = 1),
             col = Region)) +
  scale_x_continuous(name = NULL)+
  scale_y_continuous(name= 'Poblacion con inseguridad alimentaria (%)',
                     breaks = seq(0,12,2), limits = c(0,11))+
  geom_line(size = 1 ,aes(linetype=Region))+
  geom_text(label= round(df_data$`Porcentaje de poblacion con inseguridad alimentaria`, 
                         digits = 1), nudge_y = 0.3)+
  geom_point(size = 3)+
  ggtitle("Porcentaje de población con inseguridad alimentaria severa") +
  theme(plot.title = element_text(hjust = 0.5))


# Graficas comparativas entre America Latina y Medio Oriente---------------

country <- c('América Latina y el Caribe', 'Asia del Este y Pacífico')

# (%) NINOS CON PESO INSUFICIENTE EN AMERICA LATINA, EL CARIBE,  ASIA DEL ESTE Y PACÍFICO
  
p3 <- df_data %>% 
  filter(Region %in% country) %>% 
  ggplot(aes(Periodo, `Porcentaje de niños con peso insuficiente`, color = Region))+
  scale_x_continuous(name = NULL)+
  scale_y_continuous(name= 'Niños con peso insuficiente (%)',
                     breaks = seq(0,7,2), limits = c(0,7))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  ggtitle("Porcentaje de niños con insuficiencia de peso") +
  theme(plot.title = element_text(hjust = 0.5))


# INSEGURIDAD ALIMENTICIA EN AMERICA LATINA, EL CARIBE,  ASIA DEL ESTE Y PACÍFICO

p4 <- df_data %>% 
  filter(Region %in% country) %>% 
  ggplot(aes(Periodo, `Porcentaje de poblacion con inseguridad alimentaria`, color = Region))+
  scale_x_continuous(name = NULL)+
  scale_y_continuous(name= 'Poblacion con inseguridad alimentaria (%)',
                     breaks = seq(0,12,2), limits = c(0,11))+
  geom_line(size = 1)+
  geom_point(size = 3)+
  ggtitle("Porcentaje de población con inseguridad alimentaria severa") +
  theme(plot.title = element_text(hjust = 0.5))

# COMPARACION DE AMBOS GRAFICOS

p5 <- ggpubr::ggarrange(p3,p4,
                  labels = "AUTO",
                  common.legend = T,
                  legend = "bottom",
                  align = "h",
                  ncol = 2)

