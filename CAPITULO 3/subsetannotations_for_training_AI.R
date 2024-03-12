#Script para seleccionar 10 fotos por arrecife y T1 de cada area para el entrenamiento del Robot 

##Data folder
Data <- "Data"

library(readr)
annotations <- read_csv(file.path(Data, "annotations_GN_verticla_horizontal.csv"))#read cover data


library(dplyr)
library(lubridate)
# Add a column named "DataSet" with the T1 for month 07/08 of 2019 and T2 for 12 2019
annotations <- annotations %>%
  mutate(DataSet = case_when(
    year(Date) == 2019 & month(Date) %in% c(8, 9) ~ "T1",
    year(Date) == 2019 & month(Date) == 12 ~ "T2",
    TRUE ~ "A"
  )) %>%
  select(Name,Date, DataSet, everything())

annotations <- annotations %>%
  filter(DataSet == "T1")

#seleccionar solo horizontal y vertical
annotations <- subset(annotations, `reef area`=="vertical"|`reef area`=="horizontal")

#eliminar aquellas fotos que tienen menos de 100 puntos
# Calcular la frecuencia de cada nombre en 'Name'
nombre_frecuencia <- table(annotations$Name)
# Filtrar los nombres que tienen menos de 100 filas
nombres_menos_de_100_filas <- names(nombre_frecuencia[nombre_frecuencia < 100])
# Crear un nuevo dataframe excluyendo los nombres con menos de 100 filas
annotations_filtrado <- annotations[!annotations$Name %in% nombres_menos_de_100_filas, ]


# Seleccionar 20 nombres distintos para cada "reef name"
nombres_seleccionados <- annotations_filtrado %>%
  distinct(`reef name`, Name) %>%
  group_by(`reef name`) %>%
  sample_n(17) %>%
  ungroup()

# Filtrar el dataframe original para obtener las filas correspondientes a los nombres seleccionados
resultado <- annotations_filtrado %>%
  semi_join(nombres_seleccionados, by = c("reef name", "Name"))

#resultado <- subset(resultado,`reef name`=="Derby")

write.csv(resultado, 'annotations_forAI.csv',row.names = F)

ff <- as.data.frame(table(resultado$Name))





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




# Cargar la librería dplyr si aún no está cargada
library(dplyr)

# Número de filas a seleccionar por combinación
filas_por_combinacion <- 12


# Dividir el dataframe en grupos según los factores de interés
grupos <- split(PQ.COVER_T1_T2, f = list(PQ.COVER_T1_T2$DataSet, PQ.COVER_T1_T2$`reef name`,PQ.COVER_T1_T2$`reef area`))

# Crear una función para seleccionar 37 filas aleatorias de cada grupo
seleccionar_muestras <- function(grupo) {
  # Verificar el número de filas en el grupo
  n_filas_grupo <- nrow(grupo)
  
  if (n_filas_grupo >= filas_por_combinacion) {
    return(sample_n(grupo, size = filas_por_combinacion))
  } else {
    # Si hay menos de 37 filas, seleccionar todas las filas del grupo
    return(grupo)
  }
}

# Aplicar la función a cada grupo y combinar los resultados
muestras_seleccionadas <- lapply(grupos, seleccionar_muestras)
df_muestras_seleccionadas <- do.call(rbind, muestras_seleccionadas)

# Verificar el dataframe resultante
head(df_muestras_seleccionadas)

table(df_muestras_seleccionadas$DataSet,df_muestras_seleccionadas$`reef name`,df_muestras_seleccionadas$`reef area`)


