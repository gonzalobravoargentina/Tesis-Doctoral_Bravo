#Script para seleccionar 10 fotos por arrecife y T1 de cada area

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
  sample_n(20) %>%
  ungroup()

# Filtrar el dataframe original para obtener las filas correspondientes a los nombres seleccionados
resultado <- annotations_filtrado %>%
  semi_join(nombres_seleccionados, by = c("reef name", "Name"))

resultado <- subset(resultado,`reef name`=="Derby")

write.csv(resultado, 'annotations_forAI.csv',row.names = F)

ff <- as.data.frame(table(resultado$Name))
