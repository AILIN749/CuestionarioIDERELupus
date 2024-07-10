# Pruebas estadísticas de los  datos del cuestionario Idere 
# Ailín Malagón Silva
# Fecha: 09-07-2024
# Descripción : Este script ejecuta el análisis de varianza en los resultados de las puntuaciones del cuestionario idere como estado_rasgo 
# con el fin de analizar si existe alguna diferencia significativa entre los grupos de edades en terminos de las puntuciones obtenidas
# tomando como input filtrada_base_idere y dando como output pruebas_estadisticas_idere.

# Input Dir 
indir = "C:/Users/ailinmalagonsilva/proyectos/SaludMentalLupus/data"

#Output Dir
outdir = "C:/Users/ailinmalagonsilva/proyectos/SaludMentalLupus"

# Cargar input/data set
idere_pruebas<- read.csv("data/filtrada_base_idere.csv")

# Cargar librerías 
library(tidyverse)
#Histograma para ver si los datos siguen una distirbución normal 
#  Puntajes Estado 
hist(idere_pruebas$est_dep, main="Histograma de Puntuaciones de Depresión Estado", xlab="Puntuaciones de Depresión Estado", ylab="Frecuencia", col="blue")
# Puntajes Rasgo
hist(idere_pruebas$ras_dep, main="Histograma de Puntuaciones de Depresión Rasgo", xlab="Puntuaciones de Depresión Rasgo ", ylab="Frecuencia", col="red")

# Análisis de varianza ANOVA 

# Creación de intervalos  para obtener los  grupos de edades (variable discreta)

interval_ages <- idere_pruebas %>% 
  mutate(ages_groups = cut(age, breaks = c(17, 25, 35, 45, 55,68),
                         labels = c("17-25 años", "26-35 años", "36-45 años", "46-55 años"," 56-68 años"),
                         include.lowest = TRUE))
# Selección de los grupos de edades y sus respecxtivas puntuacfiones de cada participante 
# Puntuación del cuestionario como estado (variable continua)

data_estado <- interval_ages  %>% select(est_dep, ages_groups) %>% mutate(ages_groups = as.factor(ages_groups))

#Creación de modelo para análisis : regresión lineal del puntaje del cuestionario en función del grupo de edad.
modelo_est <-lm(est_dep ~ ages_groups, data = data_estado)
# Análisis anova de la regresión lineal creada anteriormente "modelo_est"
anova_est <- aov(modelo_est)
#Ver los resultados del estudio y evaluar si se rechaza o no la H0
summary(anova_est)

# Diferencias entre dos grupos y su significancia 
TukeyHSD(anova_est)

# Comporbar NORMALIDAD (Valores mayores a 0.05 quieres decir que siguen una distribución normal)
shapiro.test(anova_est$residuals)

#Comprobar homocedasticidad (Valores mayores a 0.05 indica que las varianzas son iguales)
bartlett.test(anova_est$residuals ~ ages_groups)

#Guardar mis varibales 
save(interval_ages, anova_est, data_estado, file = "data/pruebas_estadisticas_idere.RData")



