# Especifica la ubicación de tus archivos .csv
Data <- "Data/annotations_by_region"

#utilizamos el patrón '*.csv' para seleccionar todos los archivos .csv en la carpeta actual.
archivos_csv <- list.files(Data, pattern = "*.csv", full.names = TRUE)

# Crea un dataframe vacío para almacenar los datos
annotations <- data.frame()

# Itera sobre cada archivo y combínalos
for (archivo in archivos_csv) {
  datos <- read.csv(archivo)
  annotations <- rbind(annotations, datos)  # Combina los datos
}

annotations <- subset(annotations, Annotator=="gonzalopatagonia")

library(dplyr)
dfSummary <- annotations %>% group_by(Label) %>% 
  summarise(nPoints = n(), site = paste0(unique(site), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(annotations),1)) %>% 
  arrange(-nPoints) %>% 
  relocate(Label, nPoints, Percent, site)


library(formattable)
formattable(dfSummary, list(Percent=color_bar("steelblue")))

unique(annotations$Name)


#Created a csv to be imported to CoralNet
write.csv(annotations, 'annotations_training.csv',row.names = F)



#MASB (Dictyota) 1558-------------- OK
#MASG (Ulva) 676
#MASR 1478--------------------------OK
#MAFR 1791--------------------------OK 
#MAEFR 1462-------------------------OK
#MAFG 93
#MAECG 1016-------------------------OK
#MALCB 1395-------------------------OK
#MAENC 1587-------------------------OK
#MAEN 700
#MAENG 8
#MAAR 988---------------------------ok

#CNCAC (Corynactis) 1014------------OK
#CNTR 446
#CBW 347

##MOB 832
#BRY 
#SC 680

#SPMSI 812

#EURR 634
#ESS 368
#EBS 151

#ASC (Polyzoa) 379
#ASS 96
#AUC 294
#AUS 528





#Las grutas 4573
#Puerto Lobos 3698
#Punta Bs As 2070
#Puerto Piramide 1096
#Camarones 3825
#Comodoro 558
#Caleta Olivia 605
#Puerto deseado 
#ISDLE