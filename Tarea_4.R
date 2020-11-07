setwd("~/Documents/GitHub")
library(lme4)
library(ggplot2)
library(readr)
Datos_tarea4 <- read_csv("Tarea_4/Datos_tarea4.csv")
Parsed with column specification:
  cols(
    A = col_double(),
    P = col_double(),
    L = col_double(),
    H1 = col_double(),
    H2 = col_double(),
    H3 = col_double(),
    H4 = col_double(),
    H5 = col_double(),
    D = col_double()
  )
View(Datos_tarea4)



# Análisis del modelo

# Tengo 60 poblaciones de P.pajaritus, cada una con 10 individuos muestreados, y quiero saber si para esas poblaciones hay un efecto del efecto latitudinal (L) y de la degradación del hábitat (H) sobre la diversidad genética de los pajaritos. 

require(ggplot2)
require(lme4)



D <- Datos_tarea4$D

L <- Datos_tarea4$L

P <- Datos_tarea4$P

H1 <- Datos_tarea4$H1
H2 <- Datos_tarea4$H2
H3 <- Datos_tarea4$H3
H4 <- Datos_tarea4$H4
H5 <- Datos_tarea4$H5

#Se realiza el siguiente modelo: 
  
M.1 <- lm(D ~ L + H1 + H2 + H3 + H4+ H5, data = Datos_tarea4)
S.1 <- summary(M.1)
S.1

plot(M.1, 1)
hist(mean(Datos_tarea4$D), M.1$residuals)

#Nuestros resultados indican varias cosas, que nuestro modelo explica  un 49% de la variación en la diversidad genética de los pájaros analizados,además podemos decir que en general hay variables incluidas en nuestro modelo que tienen efecto sobre la diversidad genética de los pajaritos.  Las variables L, H3 y H5 presentaron valores de t mayores, lo cual indica que cuanto más alto es el valor t mayor la divergencia entre los coeficientes del modelo y el coeficiente igual a cero.

#Es posible concluir que las variables H1, H2 y H4 tienen efecto sobre la diversidad genética de los pájaros en las diferentes poblaciones.