# Visualización de datos del cuestionario Idere para personas con tratamiento
# Ailín Malagón Silva
# Fecha: 08-07-2024
# Este script ejecuta los gráficos de box plot, gráfica de barras  para el cuestionario idere respecto al tipo de tratamiento de los pacientes
# con Lupus, utilizando como input basa_idere.csv y dando como output idere_tratamiento 
# Input Dir 
indir = "C:/Users/ailinmalagonsilva/proyectos/SaludMentalLupus/data"
#Output Dir
outdir = "C:/Users/ailinmalagonsilva/proyectos/SaludMentalLupus"
# Cargar input/data set
idere_tratamiento<- read.csv("data/Base_CSI_Res.csv")

# Librerias
library(ggplot2)
library(tidyverse)
library(patchwork)

#### Limpieza de datos ####

#Omitir sex___2 (Debido a que contiene la misma información que sex___1 pero presentada de manera distinta)
#Omitir  tambien los datos que correpsonde a afrontamiento y resiliencia para este df

idere_tratamiento <-  idere_tratamiento %>%
  select(-sex___2) %>% select(sex___1, calculated_age, age_group, lupus, est_dep, ras_dep, niv_dep_est, niv_dep_ras,treatment___1)


#Renombrar la columa sex_1 a Género y la columna edad 
colnames(idere_tratamiento)[colnames(idere_tratamiento) == "sex___1"] <- "gender"
colnames(idere_tratamiento)[colnames(idere_tratamiento) == "calculated_age"] <- "age"

#Convetir de la columna "gender" los valores de 0 & 1 a hombre y mujer y luego a factor 
idere_tratamiento <- idere_tratamiento %>%
  mutate(gender = case_when(
    gender == 1 ~ "Mujer",
    gender == 0 ~ "Hombre"
  ),
  gender = as.factor(gender)
  ) 
# quitar NA de las puntuaciones del cuestinario 
idere_tratamiento <-  idere_tratamiento %>%
  filter(!is.na(est_dep) & !is.na(ras_dep)) 

# Cambiar la columna  niv_dep_est & niv_dep_ras los valores de 1,2,3 a bajo,intermedio & alto
#Convertir ambas columnas a fcatores

idere_tratamiento <- idere_tratamiento %>%
  mutate( 
    niv_dep_est = case_when (
      niv_dep_est == 1 ~ "Bajo",
      niv_dep_est == 2 ~ "Intermedio",
      niv_dep_est == 3 ~ "Alto"
    ),
    niv_dep_ras = case_when(
      niv_dep_ras == 1 ~ "Bajo",
      niv_dep_ras == 2 ~ "Intermedio",
      niv_dep_ras == 3 ~ "Alto"
    ),
    niv_dep_est = factor(niv_dep_est, levels = c("Alto", "Intermedio", "Bajo")),
    niv_dep_ras = factor(niv_dep_ras, levels = c("Alto", "Intermedio", "Bajo"))
  )
# Convertir los 0 y 1 de la columna treatment 0 (sin tratamiento ) y 1 (con tratameinto)
idere_tratamiento <- idere_tratamiento %>%  mutate(treatment___1 = case_when(
  treatment___1 == 1 ~ "glucocorticoides",
  treatment___1 == 0 ~ "Sin glucocorticoides"
),
treatment___1 = as.factor(treatment___1)
) 
  
# Filtrado final de personas que no siguen las siguientes condiciones :  
# NA en edad | NA en lupus | Si son controles no se consideran tampoco 

idere_tratamiento <- idere_tratamiento %>%
  mutate(
    lupus = if_else(lupus == 1, lupus, NA_real_)
  ) %>%
  filter(!is.na(age) & !is.na(lupus) & !is.na(niv_dep_est) & ( age != 0 ) & ( age != 1643 ))


### Gráfico 1 ####

# Gráfico de box plot en distribución de personas que reciben tratameinto y las que no 


Paleta <- c("cadetblue","coral","mediumspringgreen")

ggplot(idere_tratamiento, aes(x = treatment___1, y = est_dep, fill = treatment___1 )) +
  geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(x = "Tratamiento", y = "Puntuación idere - estado", fill = "Medicamento") +
  ggtitle("Distribución de puntuaciones de cuestionario IDERE como estado") +
  theme_minimal() +  scale_fill_manual(values = Paleta)

### Gráfico 2 ####

#Gráfico de box plot para depresión como rasgo 

Paleta2 <- c("darkolivegreen3","darkorange","darkorchid3")

ggplot(idere_tratamiento, aes(x = treatment___1, y = ras_dep, fill = treatment___1 )) +
  geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(x = "Tratamiento", y = "Puntuación idere - rasgo", fill = "Medicamento") +
  ggtitle("Distribución de puntuaciones de cuestionario IDERE como rasgo") +
  theme_minimal() +  scale_fill_manual(values = Paleta2)

###Gráfico 3 ###

# Separar por grupos de personas con y sin tratamiento y por nivel de depreisón

conteo_tratamiento <- idere_tratamiento %>%
  group_by(treatment___1, niv_dep_est) %>%
  count() %>% ungroup()

#Plot 
ggplot(conteo_tratamiento, aes(x = niv_dep_est, y = n,  fill = treatment___1)) +
  geom_bar(color = "black", stat = "identity", position = "dodge") +
  labs( title = "Nivel de depresión como estado en pacientes con lupus",
        x = "Niveles de depresión",
        y = "Número de personas",
        fill = "Medicamento" 
  ) +
  theme_minimal() +
  scale_fill_manual(values = Paleta2)

# Guardar mis variables 
save(idere_tratamiento, conteo_tratamiento, file = "data/variables_idere_tratamiento.RData")






  
