#Script para obtener observaciones de iNaturalist y realizar estadisticas

# Cargar los paquetes requeridos
library(jsonlite)
library(dplyr)


#Los datos se pueden descargar de la pag iNaturalist (1) o desde el API(2)

# (1)leer los datos bajados del proyecto "biodiversidad-submarina-de-la-costa-argentina" o "taxonomia del mar argentino"
#Read data-------
##Data folder
Data <- "Data"
library(readr)
proyecto_obs  <- read_csv(file.path(Data, "observations-437676.csv"))

## (2) Bajar los datos desde la API del poryecto biodiversidad-submarina-de-la-costa-argentina solo funciona para 10000 observaciones
#library(rinat)
#proyecto <- get_inat_obs_project("biodiversidad-submarina-de-la-costa-argentina", type = "info", raw = FALSE)
#proyecto_obs <- get_inat_obs_project(proyecto$id, type = "observations",raw = TRUE)


#Para ver que usuario fue el primero en observar cada especies del proyecto , primero vamos a definir la lista de especies de interés ( en este caso aquellas que aparecen en el proyecto "biodiversidad-submarina-de-la-costa-argentina")
especies <- unique(proyecto_obs$scientific_name)
especies <- especies[!is.na(especies)]#elimina NA

# Crear una lista para almacenar los usuarios que subieron cada especie
usuarios_por_especie <- list()

# Recorrer cada especie y obtener el primer usuario en subirla (esto puede tardar dependiendo de la cabtidad de especies)
options(timeout = 4000000)
for (especie in especies) {
  url <- paste0("https://api.inaturalist.org/v1/observations?taxon_name=", URLencode(especie), "&order=asc&per_page=1")
  observation <- fromJSON(url)
  
  # Si hay observaciones para la especie, obtener el primer usuario que la subió
  if (observation$total_results > 0) {
    usuario <- observation$results$user$login
    usuarios_por_especie[[especie]] <- usuario
  }
}

#Crear un data frame con los resultados
resultados <- do.call(rbind, usuarios_por_especie)
resultados <- as.data.frame(resultados,make.names = T)

library(tibble)
resultados <- rownames_to_column(resultados, var = "RowNames")
colnames(resultados) <- c("scientific_name","Usuario")

#AGREGAR INFORMACION SOBRE TAXONOMICA de CADA TAXA
# Eliminar duplicados en spplist basado en scientific_name
spplist_unique <- proyecto_obs %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  select(
    taxon_id, taxon_kingdom_name, taxon_phylum_name, taxon_subphylum_name,
    taxon_superclass_name, taxon_class_name, taxon_subclass_name,
    taxon_superorder_name, taxon_order_name, taxon_suborder_name,
    taxon_superfamily_name, taxon_family_name, taxon_subfamily_name,
    taxon_supertribe_name, taxon_tribe_name, taxon_subtribe_name,
    taxon_genus_name, taxon_genushybrid_name, taxon_species_name,
    taxon_hybrid_name, taxon_subspecies_name, taxon_variety_name,
    taxon_form_name, species_guess
  )


# Realizar la operación de left join
resultados <- left_join(resultados, spplist_unique %>% select(scientific_name, taxon_kingdom_name, taxon_phylum_name, taxon_class_name, common_name), by = "scientific_name")


#Obtener las observaciones de un determindado usuario---------- 
#iNat username
user <- "gonzalobravopatagonia"

#Load required packages
library(httr)
library(rlist)
library(reactable)


#API set-up
x <- 1
resp <- GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=", as.character(x), "&hrank=species", sep = ""))
parsed <- content(resp, as = "parsed")

#Retreving data from the API
while(x < (parsed$total_results/500)+1){
  resp <- GET(paste("https://api.inaturalist.org/v1/observations/species_counts?user_id=", user, "&page=", as.character(x), "&hrank=species", sep = ""))
  parsed <- content(resp, as = "parsed")
  modJSON <- parsed$results %>%
    list.select(taxon$name, count, taxon$observations_count)
  if(x == 1){
    obs_usuario <- list.stack(modJSON)
  }
  if(x > 1){
    dataz <- list.stack(modJSON)
    obs_usuario <- rbind(obs_usuario, dataz)
  }
  x <- x+1
}

colnames(obs_usuario) <- c("scientific_name","Obsbyuser","ObsiNat")


#agregar a la tabla de resultados la cantidad de observaciones por el usuario especifico y la cantidad de obs totales que hay en iNat de cada especie
resultados <- left_join(resultados, obs_usuario, by = "scientific_name")


#Para obtener el data frame completo del usuario sin utilizar el API, bajamos de inaturalist el archivo csv
library(readr)
obs <- read_csv(file.path(Data,"observations-gonzalopatagonia.csv"))
obs <- read_csv(file.path(Data,"observations-gonzalopatagonia_2024.csv"))
#Sacar observaciones que no sean de Argentina
obs <- obs[obs$latitude <= -37, ]

#GRAFICOS -----
#Grafico de observaciones por año para el proyecto
proyecto_obs$year <- as.numeric(format(proyecto_obs$observed_on,"%Y"))
obs_byyear_proyecto = as.data.frame(table(proyecto_obs$year))
colnames(obs_byyear_proyecto)=c("Año","Observaciones") 


#Grafico de observaciones por año para usuario
obs$year <- as.numeric(format(obs$observed_on,"%Y"))
obs_byyear_usuario = as.data.frame(table(obs$year))
colnames(obs_byyear_usuario)=c("Año","Observaciones") 

#Para que ambos graficos compartan el mismo eje x:
# Crear un dataframe con todos los años deseados
años_deseados <- data.frame(Año = c(1980,1981,1982,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,
                                    2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,
                                    2014,2015,2016,2017,2018,2019,2020,2021,2022,2023))

# Unir el dataframe de años deseados con los datos originales
obs_byyear_usuario_completo <- merge(años_deseados, obs_byyear_usuario, by = "Año", all.x = TRUE)
obs_byyear_usuario_completo$Observaciones <- replace(obs_byyear_usuario_completo$Observaciones, is.na(obs_byyear_usuario_completo$Observaciones), 0)

# Ordenar el dataframe por el año
obs_byyear_usuario_completo <- obs_byyear_usuario_completo[order(obs_byyear_usuario_completo$Año), ]

# Obtener los valores únicos del eje x en ambos gráficos
valores_x <- unique(c(obs_byyear_usuario_completo$Año, obs_byyear_proyecto$Año))
valores_x <- sort(valores_x)

# Convertir la columna Año en ambos dataframes a tipo factor con los valores ordenados
obs_byyear_usuario_completo$Año <- factor(obs_byyear_usuario_completo$Año, levels = valores_x)
obs_byyear_proyecto$Año <- factor(obs_byyear_proyecto$Año, levels = valores_x)

# Crear los gráficos con los valores del eje x alineados
library(ggplot2)
grafico2 <- ggplot(obs_byyear_usuario_completo, aes(x = Año, y = Observaciones, group = 1)) +
  geom_line() +
  geom_point(data = subset(obs_byyear_usuario_completo, Observaciones != 0)) +
  xlab("Año") +
  ylab("Observaciones") +
  ggtitle("(B) Usuario Gonzalo Bravo") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Datos que deseas agregar
nuevas_filas <- data.frame(Año = c(1981, 1994, 1997, 1998, 1999, 2000, 2001, 2003),
                           Observaciones = rep(0, 8))

# Agregar nuevas filas
obs_byyear_proyecto <- rbind(obs_byyear_proyecto, nuevas_filas)

# Ordenar por año
obs_byyear_proyecto <- obs_byyear_proyecto[order(obs_byyear_proyecto$Año), ]

  grafico1 <- ggplot(obs_byyear_proyecto, aes(x = Año, y = Observaciones, group = 1)) +
  geom_line() +
  geom_point(data = subset(obs_byyear_proyecto, Observaciones != 0)) +
  xlab("Año") +
  ylab("Observaciones") +
  ggtitle(" (A) Proyecto Biodiversidad Submarina de la Costa Argentina") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Combinar gráficos
library(gridExtra)
grid.arrange(grafico1, grafico2, ncol = 1)


#MAPAS----
#crear mapa con numero de observaciones para un usuario
library(leaflet)
library(leaflet.extras)
# Crear el mapa
leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE, scaleControl = FALSE))%>%# add different provider tiles
  addProviderTiles("Esri.WorldGrayCanvas") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 2,
    fillOpacity = 0.5,
    stroke = T )
#%>% addSimpleGraticule(interval = 8)

leaflet(obs,options = leafletOptions(
  attributionControl=FALSE,zoomControl = FALSE))%>%# add different provider tiles
  addProviderTiles(
    "Esri.WorldGrayCanvas") %>%# Agregar los iconos de las localidades
  addCircleMarkers(
    data = obs, 
    ~longitude, 
    ~latitude,
    weight = 0.5,
    col = 'red',
    fillColor = 'red',
    radius = 4,
    fillOpacity = 0.5,
    stroke = T,clusterOptions = markerClusterOptions())





#Estadisticas----
especies <- unique(obs$taxon_species_name)
especies <- especies[!is.na(especies)]#elimina NA
especies <- as.data.frame(especies)
GBIF <- unique(obs$quality_grade)

# Filtrar y obtener las especies únicas con "research" en quality_grade
GBIF <- subset(obs, quality_grade == "research")$taxon_species_name



#Graficos de torta-----
library(ggplot2)
library(RColorBrewer)

# Calcular la frecuencia de los nombres
frecuencia <- table(proyecto_obs$taxon_phylum_name)

especies_por_phylum <- tapply(proyecto_obs$taxon_species_name, proyecto_obs$taxon_phylum_name, function(x) length(unique(x)))

# Crear un dataframe con los datos de frecuencia
df <- data.frame(Phylum = names(frecuencia), Frecuencia = frecuencia)

df <- data.frame(Phylum = names(especies_por_phylum), Frecuencia = especies_por_phylum)



# Ordenar el dataframe por frecuencia descendente
df <- df[order(df$Frecuencia.Freq, decreasing = TRUE), ]

#traducir al espanol 
df$Phylum <- c("Moluscos", "Cordados", "Equinodermos","Artrópodos", "Rodofitas", "Cnidarios", "Ocrofitas","Clorofitas", "Poríferos" , "Ctenóforos")
               
               , "Anélidos", "Briozoos", "Platelmintos", "Braquiópodos", "Nemertinos")



# Generar una paleta de colores gradualmente variados
colores <- colorRampPalette(colors = c("blue", "green", "orange", "red"))(length(df$Phylum))

# Crear el gráfico de torta con la paleta de colores personalizada
ggplot(df, aes(x = "", y = Frecuencia.Freq, fill = Phylum)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Gráfico de Torta - Frecuencia por Phylum") +
  scale_fill_manual(values = colores[1:length(df$Phylum)]) +  # Aplicar la paleta de colores personalizada
  theme_minimal() +
  theme(legend.position = "bottom")


library(plotly)
# Crear el gráfico de torta con plot_ly
p <- plot_ly(df, labels = df$Phylum, values = df$Frecuencia.Freq, type = "pie") %>%
  layout(title = "Gráfico de Torta - Frecuencia por Phylum")


library(ggplot2)

# Datos de ejemplo
df <- data.frame(
  Phylum = c("Moluscos", "Equinodermos", "Cordados", "Cnidarios", "Artrópodos","Poríferos","Rodofitas","Ocrofitas","Clorofitas","Anélidos","Briozoos"),
  Frecuencia = c(36.3, 18, 16, 13, 10,6,5,5,2,2,1)
)

# Colores en escala de grises
colores_grises <- c("#f7f7f7", "#cccccc", "#b2b2b2", "#999999", "#808080", "#666666", "#4d4d4d", "#333333", "#1a1a1a", "#000000", "#252525")

# Crear el gráfico de torta con plot_ly y personalizar colores
p <- plot_ly(df, labels = df$Phylum, values = df$Frecuencia, type = "pie") %>%
  layout(
    title = "Gráfico de Torta - Frecuencia por Phylum",
    colorway = colores_grises
  )

# Imprimir el gráfico
print(p)




