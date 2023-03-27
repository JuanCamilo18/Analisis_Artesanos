
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

Art_estado <- df %>% 
  group_by(ESTADO) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Art_estado %>% 
  ggplot(mapping = aes(x = reorder(ESTADO, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 15, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Numero de artesanos",y= "Estado")  +              
  #ylim(c(0,300)) +
  ggtitle("Numero de artesanos segun Estado")


## Analicemos el numero de artesanos segun tipo de constancia 
unique(df$TIPO_CONSTANCIA)

Art_const <- df %>% 
  group_by(TIPO_CONSTANCIA) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Art_const %>% 
  ggplot(mapping = aes(x = reorder(TIPO_CONSTANCIA, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 15, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Numero de artesanos",y= "Tipo de constancia")  +              
  #ylim(c(0,300)) +
  ggtitle("Numero de artesanos segun Tipo de constancia")


## Analicemos el numero de artesanos segun lineaartesanal
unique(df$LINEA_ARTESANAL)

Art_linart <- df %>% 
  group_by(LINEA_ARTESANAL) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Art_linart %>% 
  ggplot(mapping = aes(x = reorder(LINEA_ARTESANAL, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 15, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Numero de artesanos",y= "Línea artesanal")  +              
  #ylim(c(0,300)) +
  ggtitle("Numero de artesanos segun Línea artesanal")


## Analicemos el numero de artesanos segun sub lineaartesanal 
unique(df$SUBLINEA_ARTESANAL)

Art_sblinart <- df %>% 
  group_by(SUBLINEA_ARTESANAL) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))


Art_sblinart %>% 
  ggplot(mapping = aes(x = reorder(SUBLINEA_ARTESANAL, desc(N_Artesanos)), y = N_Artesanos)) +
  #geom_col() +
  geom_bar(stat = "identity", fill = "red", alpha = 0.6, width = 0.8)+
  theme(axis.text.x = element_text(angle = 15, face = "italic", colour = "blue"))+
  #coord_flip()+
  geom_text(aes(label = N_Artesanos), vjust = -0.3) +
  labs(x="Numero de artesanos",y= "Sublínea artesanal")  +              
  #ylim(c(0,300)) +
  ggtitle("Numero de artesanos segun Sublínea artesanal")



#########################################################################


### Paquetes necesarios
#Los paquetes requeridos para poder hacer mapas en R son, básicamente, los que se presentan a continuación.

library(sf)
library(purrr)
library(tidyverse)
library(ggplot2)
library(ggrepel)

## Carguemos el mapa por provincias
m_provincias <- read_sf("MProv/PROVINCIAS_inei_geogpsperu_suyopomalia.shp")
m_provincias

## Carguemos el mapa por distritos
m_distritos<- read_sf("MDist/DISTRITOS_inei_geogpsperu_suyopomalia.shp")
m_distritos


## Actualicemos el df considerando solo los dptos del peru 
unique(df$PROVINCIA)
unique(m_provincias$NOMBPROV)
#df  <- df %>% filter(PROVINCIA %in% m_provincias$NOMBPROV)
# ponemos en mayusculas df$PROVINCIA para luego unir con m_provincias$NOMBPROV
df$PROVINCIA <- toupper(df$PROVINCIA)
unique(df$PROVINCIA)

## datos unidos por provincias
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


### Datos por distrito
unique(df$DISTRITO)
unique(m_distritos$NOMDIST)
# # ponemos en mayusculas df$DISTRITO para luego unir con m_distritos$NOMDIST
df$PROVINCIA <- toupper(df$PROVINCIA)
unique(df$PROVINCIA)

## datos unidos por DISTRITO
datos_dist <- m_distritos %>% 
  left_join(df %>% count(DISTRITO, name = "N_artes"),
            by =c("NOMBDIST"= "DISTRITO")) %>% 
  mutate(N_artes = as.numeric(N_artes)) %>%
  filter(NOMBDEP=="PIURA")

grafico2 <- ggplot(data = datos_dist)  +
  geom_sf(aes(fill = N_artes), show.legend = T, colour = "white")+
  geom_label_repel(aes(label = NOMDIST,
                       geometry = geometry), 
                   size = 2,
                   stat = "sf_coordinates",
                   min.segment.length = 0,
                   label.size = 1,
                   max.overlaps = Inf) +
  scale_fill_viridis_c(trans = "sqrt" ,  alpha = 0.8)+
  theme_void()

grafico2



################################################################################33

# Paquetes 
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(fontawesome)












