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

ggplot(Datos_tarea4$)

N <- 600

D <- Datos_tarea4$D

L <- Datos_tarea4$L

P <- Datos_tarea4$P

H1 <- Datos_tarea4$H1
H2 <- Datos_tarea4$H2
H3 <- Datos_tarea4$H3
H4 <- Datos_tarea4$H4
H5 <- Datos_tarea4$H5



#Planteamos un modelo donde observemos el mismo efecto de todos los pájaros en el estudio y que únicamente D depende del efecto latitudinal y de la degradación del hábitat 

M.1 <- lm(D ~ L + H1 + H2 + H3 + H4+ H5)
S.1 <- summary(M.1)
S.1

#Planteamos un modelo considerando la población y las demás variables que podrían afectar D

M.2 <- lm(D ~ P + L + H1 + H2 + H3 + H4+ H5)
S.2 <- summary(M.2)
S.2


# Por último planteamos un modelo mixto

M.3 <- lmer(D ~ L + H1 + H2 + H3 + H4+ H5 + (1|P))
S.3 <- summary(M.3)
S.3 

# crear vectores para guardar los resutados
# efecto estimado en 3 modelos
V.m1 <- NULL
V.m2 <- NULL
V.m3 <- NULL

# error estandar del efecto en 3 modelos
V.s1 <- NULL
V.s2 <- NULL
V.s3 <- NULL


V.m1 <- append(V.m1, S.1$coefficients[2,1])
V.s1 <- append(V.s1, S.1$coefficients[2,2])

V.m2 <- append(V.m2, S.2$coefficients[2,1])
V.s2 <- append(V.s2, S.2$coefficients[2,2])

V.m3 <- append(V.m3, S.3$coefficients[2,1])
V.s3 <- append(V.s3, S.3$coefficients[2, 2])



err.df <-
  data.frame(modelo = rep(rep(c("n.total", "n.poblaciones", "mixto"), each = N), 2), valor = c(V.m1, V.m2, V.m3, V.s1, V.s2, V.s3), par = rep(c("promedio", "se"), each = N * 3))

p <-
  ggplot(data = err.df, aes(x = valor, colour = modelo, fill = modelo)) + geom_histogram(
    bins = 50,
    position = "stack",
    alpha = 0.5,
    lwd = 0.2
  ) +
  theme_minimal(base_size = 16) + facet_wrap(~ par, scales = "free") +
  scale_colour_manual(values = c("#BDB76B", "#32CD32", "#00416A")) +
  scale_fill_manual(values = c("#BDB76B", "#32CD32", "#00416A"))

p
