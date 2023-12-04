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



