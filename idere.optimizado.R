# Visualización de datos del cuestionario Idere
# Ailín Malagón Silva
# Fecha: 18-06-2024
# Este script ejecuta los gráficos de frecuencia,box plot, gráfica de barras  y dispesión para el cuestionario idere 
# del registro mexicano de pacientes con Lupus, utilizando como input basa_idere.csv y dando como output idere.optimizado
# Input Dir 
indir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/idere.optimizado/data"
#Output Dir
outdir = "C:/Users/ailinmalagonsilva/Desktop/viernes_bioinfo_UNAM/idere.optimizado"
# Cargar input/data set
idere_base<- read.csv("data/Base_idere.csv")

# instalación de paquetes
install.packages("ggplot2")
installed.packages("tidyverse")
install.packages("patchwork")

# Librerias
library(ggplot2)
library(tidyverse)
library(patchwork)

#### Limpieza de datos ####

#Omitir sex___2 (Debido a que contiene la misma información que sex___1 pero presentada de manera distinta) 
idere_base <- idere_base %>%
  select(-sex___2)

#Renombrar la columa sex_1 a Género y la columna edad 
colnames(idere_base)[colnames(idere_base) == "sex___1"] <- "gender"
colnames(idere_base)[colnames(idere_base) == "calculated_age"] <- "age"

#Convetir de la columna "gender" los valores de 0 & 1 a hombre y mujer y luego a factor 
idere_base <- idere_base %>%
  mutate(gender = case_when(
    gender == 1 ~ "Mujer",
    gender == 0 ~ "Hombre"
  ),
  gender = as.factor(gender)
  ) 
# quitar NA de las puntuaciones del cuestinario 
 idere_base <- idere_base %>%
  filter(!is.na(est_dep) & !is.na(ras_dep))

# Cambiar la columna  niv_dep_est & niv_dep_ras los valores de 1,2,3 a bajo,intermedio & alto
#Convertir ambas columnas a fcatores

idere_base <- idere_base %>%
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
 

# Filtrado final de personas que no siguen las siguientes condiciones :  
# NA en edad | NA en lupus | Si son controles no se consideran tampoco 

idere_filtered <- idere_base %>%
  mutate(
    lupus = if_else(lupus == 1, lupus, NA_real_)
  ) %>%
  filter(!is.na(age) & !is.na(lupus) & !is.na(niv_dep_est) & ( age != 0 ))
 
#### Gráfico 1 ####

#Gráficos de barras con frecuencias y porcentajes 

# Calcular frecuencias y porcentajes de la subescala estado
conteo_est <- idere_filtered  %>%
  group_by(niv_dep_est) %>%
  count() %>%
  ungroup() %>%
  mutate( porcentaje = `n`/ sum(`n`)* 100)
 
# Gráfico de barras subescala estado

Paleta <- c("cadetblue","coral","mediumspringgreen")

ggplot(conteo_est, aes(x = niv_dep_est, y = n,  fill = niv_dep_est)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje,1),"%"))) +
  labs( title = "Nivel de depresión como estado en pacientes con lupus",
        x = "Niveles de depresión",
        y = "Número de personas",
        fill = "Niveles de Depresión" 
  ) +
  theme_minimal() +
  scale_fill_manual(values = Paleta)

# Calcular  frecuencias y porcentajes de la subescala rasgo
conteo_ras <- idere_filtered  %>%
  group_by(niv_dep_ras) %>%
  count() %>%
  ungroup() %>%
  mutate( porcentaje = `n`/ sum(`n`)* 100)
# Gráfico de barras subescala rasgo
Paleta2 <- c("darkolivegreen3","darkorange","darkorchid3")

ggplot(conteo_ras, aes(x = niv_dep_ras, y = n,  fill = niv_dep_ras)) +
  geom_bar(color = "black", stat = "identity") +
  geom_text(aes(label = paste0(round(porcentaje,1),"%"))) +
  labs( title = "Nivel de depresión como rasgo en pacientes con lupus",
        x = "Niveles de depresión",
        y = "Número de personas",
        fill = "Niveles de Depresión" 
  ) +
  theme_minimal() +
  scale_fill_manual(values = Paleta2)

#### Gráfico 2 ####
#Gráficos de box plot para estado-depresión 

ggplot(idere_filtered, aes(x = gender, y = est_dep, fill = gender)) +
  geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(x = "Género", y = "Puntuación idere, estado", fill = "Género") +
  ggtitle("Distribución de puntuaciones de cuestionario IDERE como estado") +
  theme_minimal() +  scale_fill_manual(values = Paleta)

#Gráfico de box plot para rasgo-depresión 

ggplot(idere_filtered, aes(x = gender, y = ras_dep, fill = gender)) +
  geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(x = "Género", y = "Puntuación idere, rasgo", fill = "Género") +
  ggtitle("Distribución de puntuaciones de cuestionario IDERE como rasgo") +
  theme_minimal() +  scale_fill_manual(values = Paleta2)


# juntar los  2 gráficos en una sola imagen 

       #(box_est | box_rasgo) + plot_layout(ncol = 2)

### Gráfico 3 ####

#Gráficos de barras  X = rangos de edades | Y = puntuación de ambas subescalas ( estado / rasgo )

# Creación de rangos de edades 
age_range <- idere_filtered %>% 
     mutate(RangoEdad = cut(age, breaks = c(17, 25, 35, 45, 55,68),
     labels = c("17-25 años", "26-35 años", "36-45 años", "46-55 años"," 56-68 años"),
     include.lowest = TRUE))
# Cambio de formato ancho a largo
age_range_longer <- age_range %>%
  pivot_longer(cols = c(est_dep, ras_dep), 
               names_to = "Subescala", 
               values_to = "Puntuacion")

# Gráfico

Paleta3 <- c("cornflowerblue", "brown")

ggplot(age_range_longer,aes (x = RangoEdad, y = Puntuacion, fill = Subescala)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  labs( x ="Gruposde edades", y = "Puntuación del cuestionario") +
  ggtitle("Resultados de las subescalas estado y rasgo dependiendo de la edad") +
  theme_minimal()+
  scale_fill_manual(values = Paleta3)

#### Gráfico 4 ####

# Gráfico de dispersión para la puntuación de subescala estado
ggplot(idere_filtered, aes(x = age, y = est_dep)) +
  geom_point() +                               
  geom_smooth(method = "lm", se = FALSE, color ="cadetblue" ) +     
  labs(x = "Edad", y = "puntuación") + 
  theme_minimal()  
# Gráfico de dispersión para la puntuación de subescala rango
ggplot(idere_filtered, aes(x = age, y = ras_dep)) +
  geom_point() +                              
  geom_smooth(method = "lm", se = FALSE, color = "darkolivegreen1") +     
  labs(x = "Edad", y = "puntuación") +  
  theme_minimal() 

### Gráfico 5 ####

###Box plot de distribución de depresión como rasgo y  estado por grupos de distintas edades.###

 #Data_frame para cambiar nombres y convetir a factor 
 box_age_range <- age_range_longer %>%
  mutate(Subescala = case_when(
    Subescala == "est_dep" ~ "Estado",
    Subescala == "ras_dep" ~ "Rasgo"
  ),
  Subescala = as.factor(Subescala)
  ) 

# GRÁFICO  CON TODOS LOS GRUPOS EN UN SOLO GRÁFICO 

Paleta4 <- c("#76EEC6","#BCEE68","#FFEC8B","#FF7256", "#DDA0DD")

ggplot(box_age_range, aes(x = Subescala, y = Puntuacion, fill = RangoEdad)) +
  geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
  labs(x = "Subescala", y = "Puntuación", fill = "Grupos de Edades") +
  ggtitle("Distribución de puntaciones por Grupos de Edades") +
  theme_minimal() +
  scale_fill_manual(values = Paleta4) +
  facet_wrap(~ RangoEdad)
  
# GRÁFICOS POR SEPARADO DE CADA GRUPO DE EDAD 

#Crear una lista vacía para almacenar gráficos 
ages_plot_list <- list()

#Age_groups almacena del df box_age_range solo la columna RangoEdad tomando los valores  unicos que existen en esta columna

age_groups <- unique(box_age_range$RangoEdad)
#> print(age_groups)
#[1] 36-45 años  26-35 años  17-25 años   56-68 años 46-55 años 
# Levels: 17-25 años 26-35 años 36-45 años 46-55 años  56-68 años


## Generar un gráfico para cada grupo de edad, utilizaremos el vector creado "age_groups"
   # age_group toma un valor único de age_groups en cada iteración. 
   for (age_group in age_groups) {
   #filtra las filas del dataframe box_age_range donde el valor de la columna RangoEdad es igual al valor actual de age_group
   filtered_data <- box_age_range %>% filter(RangoEdad == age_group)
  
   # Crear el gráfico para el grupo de edad filtrado
   p <- ggplot(filtered_data, aes(x = Subescala, y = Puntuacion, fill = Subescala)) +
    geom_boxplot(color = "black", outlier.colour = "red", outlier.shape = 16) +
    labs(x = "Subescala", y = "Puntuación", fill = "Subescala") +
    ggtitle(paste("Distribución de puntuaciones para el grupo de edad", age_group)) +
    theme_minimal() +
    scale_fill_manual(values = Paleta4)
  
  # Añadir el gráfico a la lista
  ages_plot_list[[age_group]] <- p

  }



  # Mostrar los gráficos
for (age_group in age_groups) {
  print(ages_plot_list[[age_group]])
}



#guardar mis variables 

save(idere_base, idere_filtered, conteo_ras, conteo_est, age_range, age_range_longer,box_age_range, file = "data/data_idere_optimizada.RData")
write.csv(idere_filtered, file = "data/filtrada_base_idere.csv", row.names = FALSE)



                
                
