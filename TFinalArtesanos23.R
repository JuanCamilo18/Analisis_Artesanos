
##############################################################################
## ANÁLISIS EXPLORATORIO
## Este es un breve reporte del análisis exploratorio de los Artesanos 
# Inscritos en el Registro Nacional De Artesano - RNA por línea Artesanal y por
# Provincia [Gobierno Regional Piura]
##############################################################################

#### Configuraciones iniciales #############

rm(list = ls())

# Configurar el directorio de trabajo 
setwd("D:/Cursos22/Analitica2023/TFinalEP/Analisis_Artesanos")

#### MOdulos y datos ####
library(tidyverse)
library(readxl)


## Datos disponibles en:
# https://www.datosabiertos.gob.pe/dataset/informaci%C3%B3n-de-fallecidos-del-sistema-inform%C3%A1tico-nacional-de-defunciones-sinadef-ministerio

## Carguemos los datos Artesanos Inscritos en el Registro Nacional De Artesano
# - RNA por línea Artesanal y por Provincia [Gobierno Regional Piura]
df <- read.csv("artesanosrnagrp_0.csv", sep = ",")

colnames(df)

table(df$FECHA_CORTE)

# Eliminemos la primera columna 
#df <- df[, -1]


# Valores faltantes
colSums(is.na(df))

summary(df$UBIGEO)
table(df$UBIGEO)



#### Analisis descriptivo ####

## Estructura del dataframe 
str(df)


## Analicemos el numero de artesanos por provincia 
Num_Artesanos <- df %>% 
  group_by(PROVINCIA) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Num_Artesanos %>% 
  ggplot(mapping = aes(x = reorder(PROVINCIA, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 45, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Provincias",y= "Numero de artesanos")  +              
  #ylim(c(0,300)) +
  ggtitle("Diagrama de barras: Numero de artesanos por provincia") 



## Analicemos el numero de artesanos por distrito
Num_Art2 <- df %>% 
  group_by(DISTRITO) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Num_Art2 %>% 
  ggplot(mapping = aes(x = reorder(DISTRITO, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 90, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Distrito",y= "Numero de artesanos")  +              
  #ylim(c(0,300)) +
  ggtitle("Diagrama de barras: Numero de artesanos por distrito") 



## Analicemos la inscripcion de artesanos por clase solicitante
summary(df$SOLICITANTE)
table(df$SOLICITANTE)
unique(df$SOLICITANTE)


Art_sol <- df %>% 
  group_by(SOLICITANTE) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Art_sol %>% 
  ggplot(mapping = aes(x = reorder(SOLICITANTE, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 15, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Numero de artesanos",y= "Solicitante")  +              
  #ylim(c(0,300)) +
  ggtitle("Numero de artesanos segun tipo solicitante")


## Analicemos el numero de artesanos segun su estado 
unique(df$ESTADO)


## Analicemos el numero de artesanos segun tipo de constancia 
unique(df$TIPO_CONSTANCIA)


## Analicemos el numero de artesanos segun lineaartesanal
unique(df$LINEA_ARTESANAL)


## Analicemos el numero de artesanos segun sub lineaartesanal 
unique(df$SUBLINEA_ARTESANAL)




#########################################################################


### Paquetes necesarios
#Los paquetes requeridos para poder hacer mapas en R son, básicamente, los que se presentan a continuación.

library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

# Carguemos el mapa 
m_provincias <- read_sf("MProv/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
m_provincias



# Actualicemos df considerando solo los dptos del peru 
unique(df$PROVINCIA)
unique(m_provincias$NOMBPROV)
#df  <- df %>% filter(PROVINCIA %in% m_provincias$NOMBPROV)

df$PROVINCIA <- toupper(df$PROVINCIA)
unique(df$PROVINCIA)

# datos unidos

datos <- m_provincias %>% 
  left_join(df %>% count(PROVINCIA, name = "N_artes"),
            by =c("NOMBPROV"= "PROVINCIA")) %>% 
  mutate(N_artes = as.numeric(N_artes))


grafico1 <- ggplot(data = datos %>%
                  filter(NOMBDEP=="PIURA")) +
  geom_sf(aes(fill = N_artes), show.legend = T, colour = "white")+
  geom_label_repel(aes(label = NOMBPROV,
                       geometry = geometry), 
                   size = 2,
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   label.size = 1,
                   max.overlaps = Inf) +
  scale_fill_viridis_c(trans = "sqrt" ,  alpha = 0.8)+
  theme_void()

grafico1








