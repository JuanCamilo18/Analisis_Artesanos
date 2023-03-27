
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






