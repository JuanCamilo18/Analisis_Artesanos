
##############################################################################
## ANÁLISIS EXPLORATORIO
## Este es un breve reporte del análisis exploratorio de los Artesanos 
# Inscritos en el Registro Nacional De Artesano - RNA por línea Artesanal y por
# Provincia [Gobierno Regional Piura]
##############################################################################

#### Configuraciones iniciales #############

rm(list = ls())

# Configurar el directorio de trabajo 
setwd("D:/Cursos22/Analitica2023/TFinalEP")

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


## Analicemos el numero de artesanos por provincia 
Num_Artesanos <- df %>% 
  group_by(PROVINCIA) %>% 
  summarize(N_Artesanos = n()) %>% 
  # ordenar el dataframe por la columna PRomedioMOnto
  arrange(desc(N_Artesanos))

Num_Artesanos %>% 
  ggplot(mapping = aes(x = PROVINCIA , y = N_Artesanos))+
  geom_col() +
  # geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))


## Analicemos el numero de artesanos por distrito


## Analicemos la inscripcion de artesanos por clase solicitante
summary(df$SOLICITANTE)
table(df$SOLICITANTE)
unique(df$SOLICITANTE)

## Analicemos el numero de artesanos segun su estado 
unique(df$ESTADO)


## Analicemos el numero de artesanos segun tipo de constancia 
unique(df$TIPO_CONSTANCIA)


## Analicemos el numero de artesanos segun lineaartesanal
unique(df$LINEA_ARTESANAL)


## Analicemos el numero de artesanos segun sub lineaartesanal 
unique(df$SUBLINEA_ARTESANAL)






