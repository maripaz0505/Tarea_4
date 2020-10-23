setwd("~/Documents/GitHub")
library(lme4)
> library(readr)
> Datos_tarea4 <- read_csv("Tarea_4/Datos_tarea4.csv")
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
> View(Datos_tarea4)
> View(Datos_tarea4)


# Análisis del modelo

# Tengo 60 poblaciones de P.pajaritus, cada una con 10 individuos muestreados, y quiero saber si para esas poblaciones hay un efecto del efecto latitudinal (L) y de la degradación del hábitat (H) sobre la diversidad genética de los pajaritos. 

# Un mismo efecto para todos los pájaros (sin importar su población)
P.0 <- lm(D ~ L * H1 * H2 * H3 * H4 * H5, data = Datos_tarea4)
summary(P.0)
