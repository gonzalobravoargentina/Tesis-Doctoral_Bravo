library(vegan)
#library(devtools) for installing pairwiseAdonis packaged from GitHUb
#install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)
library(rich)
library(ggplot2)
library(cowplot)
library(doBy)
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)

# Suppress summaries info
options(dplyr.summarise.inform = FALSE)

#DATA
##Data folder
Data <- "Data"

#COVER DATA-------------
#We download from CORALNET two files, 1-species cover information and 2- metadata of photoquadrats:
# 1- metadata_HUMAN.csv
# 2- percent_cover_Human.csv

library(readr)
cover <- read_csv(file.path(Data, "percent_covers_HUMAN.csv"))#read cover data
cover <- cover[-nrow(cover), ]#delete last row that has the totals
metadata <- read_csv(file.path(Data,"metadata_HUMAN.csv"))#read metadata

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.COVER<- merge(metadata,cover, by.x = "Name", by.y ="Image name", all.x = F) 


#Remove original data frames from enviroment
rm(cover)
rm(metadata)


## Remove colums with taxa categories that were not used for the photoquadrats, this loops search colsum= 0 in all the data frame, and remove those columns. 
columnas_a_verificar <- colnames(PQ.COVER)[which(colnames(PQ.COVER) == "Points"):ncol(PQ.COVER)]
columnas_a_eliminar <- character(0) 

for (col in columnas_a_verificar) {
  if (sum(PQ.COVER[[col]]) == 0) {
    columnas_a_eliminar <- c(columnas_a_eliminar, col)
  }
}
if (length(columnas_a_eliminar) > 0) {
  PQ.COVER <- PQ.COVER[, !(colnames(PQ.COVER) %in% columnas_a_eliminar)]
}


# As we have 100 points in the photos, each column with values is expressed in percentage, however we want to remove the points of SHADOW, Unclear and some mobile invertebrates and recalculate %
suma_for_remove <- rowSums(PQ.COVER[, c("Arbacia dufresnii","Cosmasterias lurida","Cycethra verrucosa", "Fissurellidea patagonica","Odontaster penicillatus", "Pleurobranchaea maculata","Shadow","Unknown")], na.rm = TRUE)

PQ.COVER$Points <- PQ.COVER$Points - suma_for_remove #total points after removal 

#Eliminate the columns  "SHAD", "Unk","A.dufresni","C.lurida","C.verrucos","F.patagoni","O.penicill","P.maculata"
PQ.COVER <- PQ.COVER[, !(colnames(PQ.COVER) %in% c("Arbacia dufresnii","Cosmasterias lurida","Cycethra verrucosa", "Fissurellidea patagonica","Odontaster penicillatus", "Pleurobranchaea maculata","Shadow","Unknown"))]

# Calculate the new % using the total in column "Points"
for (col in colnames(PQ.COVER)[(which(colnames(PQ.COVER) == "Points") + 1):ncol(PQ.COVER)]) {
  PQ.COVER[, col] <- round((PQ.COVER[, col] / PQ.COVER$Points) * 100)
}

library(dplyr)
library(lubridate)
# Add a column named "DataSet" with the T1 for month 07/08 of 2019 and T2 for 12 2019
PQ.COVER <- PQ.COVER %>%
  mutate(DataSet = case_when(
    year(Date) == 2019 & month(Date) %in% c(8, 9) ~ "T1",
    year(Date) == 2019 & month(Date) == 12 ~ "T2",
    TRUE ~ "A"
  )) %>%
  select(Name,Date, DataSet, everything())


#For region comparison between PM and PP we will subset the data, take out deep reefs from PP and select T1 and T2
PQ.COVER_T1_T2 <- PQ.COVER %>%
  filter(!grepl("A", DataSet, ignore.case = TRUE))
PQ.COVER_T1_T2 <- PQ.COVER_T1_T2 %>%
  filter(!grepl("overhang", `reef area`, ignore.case = TRUE))


## Remove colums with taxa categories that were not used for the photoquadrats, this loops search colsum= 0 in all the data frame, and remove those columns. 
columnas_a_verificar <- colnames(PQ.COVER_T1_T2)[which(colnames(PQ.COVER_T1_T2) == "Points"):ncol(PQ.COVER_T1_T2)]
columnas_a_eliminar <- character(0) 

for (col in columnas_a_verificar) {
  if (sum(PQ.COVER_T1_T2[[col]]) == 0) {
    columnas_a_eliminar <- c(columnas_a_eliminar, col)
  }
}
if (length(columnas_a_eliminar) > 0) {
  PQ.COVER_T1_T2 <- PQ.COVER_T1_T2[, !(colnames(PQ.COVER_T1_T2) %in% columnas_a_eliminar)]
}

#clean environment
rm(col,columnas_a_eliminar,columnas_a_verificar,suma_for_remove)


#COVER DATA IA-----
##Data folder
Data <- "Data"
library(readr)
cover_AI <- read_csv(file.path(Data, "percent_covers_AI.csv"))#read cover data
metadata_AI <- read_csv(file.path(Data,"metadata_AI.csv"))#read metadata

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.COVER_AI<- merge(metadata_AI,cover_AI, by.x = "Name", by.y ="Image name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(cover_AI,metadata_AI)


## Remove colums with taxa categories that were not used for the photoquadrats, this loops search colsum= 0 in all the data frame, and remove those columns. 
columnas_a_verificar <- colnames(PQ.COVER_AI)[which(colnames(PQ.COVER_AI) == "Points"):ncol(PQ.COVER_AI)]
columnas_a_eliminar <- character(0) 

for (col in columnas_a_verificar) {
  if (sum(PQ.COVER_AI[[col]]) == 0) {
    columnas_a_eliminar <- c(columnas_a_eliminar, col)
  }
}
if (length(columnas_a_eliminar) > 0) {
  PQ.COVER_AI <- PQ.COVER_AI[, !(colnames(PQ.COVER_AI) %in% columnas_a_eliminar)]
}

names(PQ.COVER_AI)
# As we have 100 points in the photos, each column with values is expressed in percentage, however we want to remove the points of SHADOW, Unclear and some mobile invertebrates and recalculate %

#code for csv with full names
suma_for_remove <- rowSums(PQ.COVER_AI[, c("Arbacia dufresnii", "Fissurellidea patagonica","Odontaster penicillatus","Shadow")], na.rm = TRUE)
PQ.COVER_AI$Points <- PQ.COVER_AI$Points - suma_for_remove
PQ.COVER_AI <- PQ.COVER_AI[, !(colnames(PQ.COVER_AI) %in% c("Arbacia dufresnii", "Fissurellidea patagonica","Odontaster penicillatus","Shadow"))]

# Calculate the new % usung the total in column "Points"
for (col in colnames(PQ.COVER_AI)[(which(colnames(PQ.COVER_AI) == "Points") + 1):ncol(PQ.COVER_AI)]) {
  PQ.COVER_AI[, col] <- round((PQ.COVER_AI[, col] / PQ.COVER_AI$Points) * 100)
}


library(dplyr)
library(lubridate)
# Add a column named "DataSet" with the T1 for month 07/08 of 2019 and T2 for 12 2019
PQ.COVER_AI <- PQ.COVER_AI %>%
  mutate(DataSet = case_when(
    year(Date) == 2019 & month(Date) %in% c(8, 9) ~ "T1",
    year(Date) == 2019 & month(Date) == 12 ~ "T2",
    TRUE ~ "A"
  )) %>%
  select(Name,Date, DataSet, everything())


#For region comparison between PM and PP we will subset the data, take out deep reefs from PP and select T1 and T2
PQ.COVER_AI_T1_T2 <- PQ.COVER_AI %>%
  filter(!grepl("A", DataSet, ignore.case = TRUE))


dfRobot <- PQ.COVER_AI_T1_T2 %>% filter(`Annotation status`=='Unconfirmed') %>% 
  dplyr::select(-matches("^Annotation"), -Points) %>% 
  mutate(source='robot') 


## re arrange the fields
dfRobot <- dfRobot %>% relocate(Name, source,everything())

#clean environment
rm(col,columnas_a_eliminar,columnas_a_verificar,suma_for_remove)



#DENSITY DATA-----
library(readr)
density <- read_csv(file.path(Data, "density_HUMAN.csv"))#read density data
metadata <- read_csv(file.path(Data,"metadata_HUMAN.csv"))#read metadata

#not all the photos were analysed, so we discard those unconfirmed 
density <- subset(density,`Annotation status`=="confirmed")

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.DENSITY<- merge(metadata,density, by = "Name", all.x = F) 
#eliminated NA and add 0 
PQ.DENSITY[is.na(PQ.DENSITY)] <- 0

library(dplyr)
# Eliminar la columna "Notas" del data frame PQ.DENSITY
PQ.DENSITY <- PQ.DENSITY %>%
  select(-Notas)

library(dplyr)
library(lubridate)
# Add a column named "DataSet" with the T1 for month 07/08 of 2019 and T2 for 12 2019
PQ.DENSITY <- PQ.DENSITY %>%
  mutate(DataSet = case_when(
    year(Date) == 2019 & month(Date) %in% c(8, 9) ~ "T1",
    year(Date) == 2019 & month(Date) == 12 ~ "T2",
    TRUE ~ "A"
  )) %>%
  select(Name, Date, DataSet, everything())

#Remove original data frames from enviroment
rm(density)
rm(metadata)

#For region comparison between PM and PP we will subset the data, take out deep reefs from PP and select T1 and T2
PQ.DENSITY_T1_T2 <- PQ.DENSITY %>%
  filter(!grepl("A", DataSet, ignore.case = TRUE))
PQ.DENSITY_T1_T2 <- PQ.DENSITY_T1_T2 %>%
  filter(!grepl("overhang", `reef area`, ignore.case = TRUE))


#PRESENCE_ABSENCE DATA-----
library(dplyr)

#Merge PQ.COVER and PQ.DENSITY
PQ.PRESENCE_ABSENCE <- merge(PQ.COVER, PQ.DENSITY, by = "Name", all = FALSE)

# Identify columns with ".y"
cols_to_remove <- grep("\\.y$", colnames(PQ.PRESENCE_ABSENCE), value = TRUE)

# delete the columns with ".y"
PQ.PRESENCE_ABSENCE <- PQ.PRESENCE_ABSENCE[, !(colnames(PQ.PRESENCE_ABSENCE) %in% cols_to_remove)]

#change  ".x" to original names 
colnames(PQ.PRESENCE_ABSENCE) <- sub("\\.x$", "", colnames(PQ.PRESENCE_ABSENCE))

rm(cols_to_remove)

# Obtener las columnas después de "Points"
col_start <- which(colnames(PQ.PRESENCE_ABSENCE) == "Points") + 1
col_end <- ncol(PQ.PRESENCE_ABSENCE)

# Reemplazar números mayores que 0 por 1 en las columnas
PQ.PRESENCE_ABSENCE[, col_start:col_end][PQ.PRESENCE_ABSENCE[, col_start:col_end] > 0] <- 1

rm(col_start,col_end)


#For region comparison between PM and PP we will subset the data, select dataset T1 and T2 (-A) and keep only vertical and horizontal surfaces 
PQ.PRESENCE_ABSENCE_T1_T2 <- PQ.PRESENCE_ABSENCE %>%
  filter(!grepl("A", DataSet, ignore.case = TRUE))
PQ.PRESENCE_ABSENCE_T1_T2 <- PQ.PRESENCE_ABSENCE_T1_T2 %>%
  filter(!grepl("overhang", `reef area`, ignore.case = TRUE))


#Checklist---------
## make PQ.PRESENCE_ABSENCE_T1_T2 to long type
library(reshape2) 
PQ.PRESENCE_ABSENCE_T1_T2_long = melt(PQ.PRESENCE_ABSENCE_T1_T2, id.vars = 1:22, measure.vars = 23:ncol(PQ.PRESENCE_ABSENCE_T1_T2), na.rm = T)
#rename columns because the ontop command is not working 
colnames(PQ.PRESENCE_ABSENCE_T1_T2_long)[23] <- "TAXA"
colnames(PQ.PRESENCE_ABSENCE_T1_T2_long)[24] <- "Presence"
colnames(PQ.PRESENCE_ABSENCE_T1_T2_long)[7] <- "reefarea"

library(reshape2)
# Selecciona las columnas relevantes
df_selected <- PQ.PRESENCE_ABSENCE_T1_T2_long[, c("site", "TAXA", "Presence")]

# Utiliza dcast para crear la nueva tabla
checklist <- dcast(df_selected, TAXA ~ site, value.var = "Presence", fun.aggregate = sum)

# Rellena los valores NA con 0
checklist[is.na(checklist)] <- 0

#elimino aquellas especies que no estan presentes en ninguna de las dos zonas
checklist <- checklist[!(checklist$`Bahia Nueva` == 0 & checklist$`Bahia Piramides` == 0), ]


rm(df_selected)

#Presence - Absence by zone 
write.csv(checklist, file = "checklist_GolfoNuevo.csv")


#Especies no detectadas por IA----

# Obtener los nombres de las columnas de especies para cada dataframe
nombres_columnas_AI <- names(PQ.COVER_AI_T1_T2)[22:length(names(PQ.COVER_AI_T1_T2))]
nombres_columnas_T1_T2 <- names(PQ.COVER_T1_T2)[22:length(names(PQ.COVER_T1_T2))]

# Encontrar especies únicas en cada dataframe
especies_solo_en_AI <- setdiff(nombres_columnas_AI, nombres_columnas_T1_T2)
especies_solo_en_T1_T2 <- setdiff(nombres_columnas_T1_T2, nombres_columnas_AI)

# Verificar las especies únicas en cada dataframe
print("Especies únicas en PQ.COVER_AI_T1_T2:")
print(especies_solo_en_AI)

print("Especies únicas en PQ.COVER_T1_T2:")
print(especies_solo_en_T1_T2)


#Density plots-------
# Crea un dataframe con las columnas específicas que deseas visualizar
data_to_plot <- PQ.DENSITY_T1_T2[, c("site", "DataSet", "Arbacia dufresnii", "Tegula patagonica", "Cosmasterias lurida", "Trophon geversianus","Helcogrammoides cunninghami")]

data_to_plot$DataSet <- gsub("T1", "Agosto", data_to_plot$DataSet)
data_to_plot$DataSet <- gsub("T2", "Diciembre", data_to_plot$DataSet)

# Dibuja los boxplots
ggplot(data_to_plot, aes(x = site, y = `Arbacia dufresnii`)) +
  geom_boxplot(aes(fill = DataSet)) +
  labs(title = "Arbacia dufresnii", y = "Densidad",x="Zona",fill="Fecha") +
  theme_minimal()

ggplot(data_to_plot, aes(x = site, y = `Tegula patagonica`)) +
  geom_boxplot(aes(fill = DataSet)) +
  labs(title = "Tegula patagonica", y = "Densidad",x="Zona") +
  theme_minimal()

#GLM
arbacia = glm(`Tegula patagonica`~ site*DataSet,family = poisson, data = data_to_plot)
summary(arbacia)


#% COVER PLOTS---------

#Create long type dataframe 
PQ.COVER_T1_T2_long = melt(PQ.COVER_T1_T2, id.vars = 1:22, measure.vars = 23:ncol(PQ.COVER_T1_T2), na.rm = T)
#rename columns because the ontop command is not working 
colnames(PQ.COVER_T1_T2_long)[23] <- "TAXA"
colnames(PQ.COVER_T1_T2_long)[24] <- "cover"
colnames(PQ.COVER_T1_T2_long)[7] <- "reefarea"


#Calculate mean, SD, SE for cover data by factors 
Coverdata <- summaryBy(cover ~ TAXA + site + DataSet+reefarea,data=PQ.COVER_T1_T2_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})

# Dictyota dichotoma, macroalgas filamentosas rojas, macroalgas calcáreas incrustantes, Corynactis carnea, Substrato, macroalgas laminares rojas, Ulva y Aulacomya atra
# 
Coverdata2 <- subset(Coverdata,TAXA=="Dictyota dichotoma"|TAXA=="Macroalgae: Filamentous / filiform : red"|TAXA=="Macroalgae:Encrusting:Red:Calcareous:Crustose"|TAXA=="Corynactis carnea"|TAXA=="Substrate: Consolidated (hard)"|TAXA=="Macroalgae: Sheet-like / membraneous: red"|TAXA=="Ulva"|TAXA=="Aulacomya atra")

Coverdata2$TAXA <- gsub("\\([^)]+\\)", "", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub(" ", "", Coverdata2$TAXA)


Coverdata2$TAXA <- gsub("Macroalgae:Encrusting:Red:Calcareous:Crustose", "Calcáreas incrustantes", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Macroalgae:Filamentous/filiform:red", "Filamentosas Rojas", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Macroalgae:Sheet-like/membraneous:red", "Laminares rojas", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Substrate:Consolidated", "Sustrato", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Corynactiscarnea", "Corynactis carnea", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Aulacomyaatra", "Aulacomya atra", Coverdata2$TAXA)
Coverdata2$TAXA <- gsub("Dictyotadichotoma", "Dictyota dichotoma", Coverdata2$TAXA)

Coverdata2$DataSet <- gsub("T1", "Agosto", Coverdata2$DataSet)
Coverdata2$DataSet <- gsub("T2", "Diciembre", Coverdata2$DataSet)

# 6 x 15 
ggplot(Coverdata2,aes(x=reefarea,y=cover.mean,fill=DataSet)) + geom_bar(alpha=0.7,stat="identity",color="black",position=position_dodge()) + scale_color_grey() + geom_errorbar(aes(ymin=cover.mean-cover.SE, ymax=cover.mean+cover.SE), width=.2,position=position_dodge(.9)) + theme_bw() + scale_y_continuous(limits = c(0,100))+ labs(fill = "Fecha",x = "", y = "Cobertura (%)", title = "")+ theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + scale_fill_brewer(palette="Set2")+ facet_grid(site~TAXA)+theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))



#nMDS---------

#Horizontal data
horizontal.data <- subset(PQ.COVER_T1_T2,`reef area`=="horizontal")
#nMDS calculations (no transformation + Bray)
nMDShorizontal=metaMDS(horizontal.data[,-(1:22)],k=2,trymax=5,try = 5,distance ="bray",autotransform = FALSE)
NMDS1.horizontal <-nMDShorizontal$points[,1] 
NMDS2.horizontal <- nMDShorizontal$points[,2]
MDS.plot_horizontal<-cbind(horizontal.data[,-(1:22)], NMDS1.horizontal, NMDS2.horizontal,horizontal.data$DataSet,horizontal.data$site) 
#nMDS plot horizontal
nMDShorizontal.plot <- ggplot(MDS.plot_horizontal, aes(NMDS1.horizontal, NMDS2.horizontal, color=horizontal.data$site,shape=horizontal.data$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.horizontal)-0.5, y=min(NMDS2.horizontal)-0.5, label=paste('Stress =',round(nMDShorizontal$stress,3)))+ggtitle("    Fotocuadrantes superficie horizontal")+ scale_color_brewer(palette="Set2",name = "Zona", labels = c("Bahia Nueva", "Bahia Piramides"))

nMDShorizontal.plot_T <- ggplot(MDS.plot_horizontal, aes(NMDS1.horizontal, NMDS2.horizontal, color=horizontal.data$DataSet,shape=horizontal.data$DataSet))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.horizontal)-0.5, y=min(NMDS2.horizontal)-0.5, label=paste('Stress =',round(nMDShorizontal$stress,3)))+ggtitle("    Fotocuadrantes superficie horizontal - Fecha")+ scale_color_brewer(palette="Accent",name = "Fecha", labels = c("Agosto", "Diciembre"))

#Horizontal data AI
horizontal.data_AI <- subset(PQ.COVER_AI_T1_T2,`reef area`=="horizontal")
#nMDS calculations (no transformation + Bray)
nMDShorizontal_AI=metaMDS(horizontal.data_AI[,-(1:22)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.horizontal_AI <-nMDShorizontal_AI$points[,1] 
NMDS2.horizontal_AI <- nMDShorizontal_AI$points[,2]
MDS.plot_AI<-cbind(horizontal.data_AI[,-(1:22)], NMDS1.horizontal_AI, NMDS2.horizontal_AI,horizontal.data_AI$site) 
#nMDS plot horizontal AI
nMDShorizontal_AI.plot <- ggplot(MDS.plot_AI, aes(NMDS1.horizontal_AI, NMDS2.horizontal_AI, color=horizontal.data_AI$site,shape=horizontal.data_AI$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.horizontal_AI)-0.5, y=min(NMDS2.horizontal_AI)-0.5, label=paste('Stress =',round(nMDShorizontal_AI$stress,3)))+ggtitle("    Fotocuadrantes superficie horizontal_IA")+ scale_color_brewer(palette="Dark2",name = "Zona", labels = c("Bahia Nueva", "Bahia Piramides"))


#vertical data
vertical.data <- subset(PQ.COVER_T1_T2,`reef area`=="vertical")
#nMDS calculations (no transformation + Bray)
nMDSvertical=metaMDS(vertical.data[,-(1:22)],k=2,trymax=5,try =5,distance ="bray",autotransform = FALSE)

NMDS1.vertical <-nMDSvertical$points[,1] 
NMDS2.vertical <- nMDSvertical$points[,2]
MDS.plot_vertical<-cbind(vertical.data[,-(1:22)], NMDS1.vertical, NMDS2.vertical,vertical.data$site, vertical.data$DataSet) 
#nMDS plot vertical
nMDSvertical.plot <- ggplot(MDS.plot_vertical, aes(NMDS1.vertical, NMDS2.vertical, color=vertical.data$site,shape=vertical.data$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.vertical)-0.5, y=min(NMDS2.vertical)-0.5, label=paste('Stress =',round(nMDSvertical$stress,3)))+ggtitle("    Fotocuadrantes superficie vertical")+ scale_color_brewer(palette="Set2",name = "Reef Site", labels = c("Bahia Nueva", "Bahia Piramides")) 

nMDSvertical.plot_T <- ggplot(MDS.plot_vertical, aes(NMDS1.vertical, NMDS2.vertical, color=vertical.data$DataSet,shape=vertical.data$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.vertical)-0.5, y=min(NMDS2.vertical)-0.5, label=paste('Stress =',round(nMDSvertical$stress,3)))+ggtitle("    Fotocuadrantes superficie vertical - Fecha")+ scale_color_brewer(palette="Accent",name = "Fecha", labels = c("Agosto", "Diciembre")) 

#vertical data AI
vertical.data_AI <- subset(PQ.COVER_AI_T1_T2,`reef area`=="vertical")
#nMDS calculations (no transformation + Bray)
nMDSvertical_AI=metaMDS(vertical.data_AI[,-(1:22)],k=2,trymax=10,try = 10,distance ="bray",autotransform = FALSE)
NMDS1.vertical_AI <-nMDSvertical_AI$points[,1] 
NMDS2.vertical_AI <- nMDSvertical_AI$points[,2]
MDS.plot.vertical_AI<-cbind(vertical.data_AI[,-(1:22)], NMDS1.vertical_AI, NMDS2.vertical_AI,vertical.data_AI$site) 
#nMDS plot vertical AI
nMDSvertical_AI.plot <- ggplot(MDS.plot.vertical_AI, aes(NMDS1.vertical_AI, NMDS2.vertical_AI, color=vertical.data_AI$site,shape=vertical.data_AI$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "none",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.vertical_AI)-0.5, y=min(NMDS2.vertical_AI)-0.5, label=paste('Stress =',round(nMDSvertical_AI$stress,3)))+ggtitle("    Fotocuadrantes superficie vertical_IA")+ scale_color_brewer(palette="Dark2",name = "Reef Site", labels = c("Bahia Nueva", "Bahia Piramides")) 


# Plot legend
legend <- ggplot(MDS.plot_vertical, aes(NMDS1.vertical, NMDS2.vertical, color=vertical.data$site,shape=vertical.data$site))+geom_point(position=position_jitter(.1))+stat_ellipse(type='t',size =1) +theme_bw() + theme(legend.position = "bottom",axis.text.x = element_blank(),axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(),panel.background = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),plot.background = element_blank()) + annotate("text", x=max(NMDS1.vertical)-0.5, y=min(NMDS2.vertical)-0.5, label=paste('Stress =',round(nMDSvertical$stress,3)))+ggtitle("    Fotocuadrantes superficie vertical")+ scale_shape_manual(name = "Region", labels = c("Bahia Nueva", "Bahia Piramides"),values = c(16,17,15))+
  scale_color_brewer(palette="Dark2",name = "Region", labels = c("Bahia Nueva", "Bahia Piramides")) 


library(ggplot2)
library(cowplot)
#plot1
plot_grid(nMDShorizontal.plot,nMDShorizontal.plot_T,nMDSvertical.plot,nMDSvertical.plot_T,ncol = 2, align = "v")
#Create plot grid and save
legend_plot <- get_legend(legend)#take legend
nMDSbysurface <- plot_grid(nMDShorizontal.plot,nMDSvertical.plot,nMDShorizontal_AI.plot,nMDSvertical_AI.plot,ncol = 2, align = "v")
fig1 <- plot_grid(nMDSbysurface, legend_plot, ncol = 1, rel_heights = c(1,.09))



rm(nMDSbysurface,legend_plot,legend,nMDShorizontal.plot,nMDShorizontal_AI.plot,nMDSvertical.plot,nMDSvertical_AI.plot, NMDS1.vertical,NMDS1.vertical_AI,NMDS2.vertical,NMDS2.vertical_AI,NMDS1.horizontal,NMDS1.horizontal_AI,NMDS2.horizontal,NMDS2.horizontal_AI,fig1,nMDShorizontal,nMDShorizontal_AI,nMDSvertical,nMDSvertical_AI,MDS.plot,MDS.plot_AI, vertical.data,vertical.data_AI,horizontal.data,horizontal.data_AI,MDS.plot.vertical_AI)

#PERMANOVA with cover data----------------------
#Prior to PERMANOVA I used betadisper() to test for homogeneity of multivariate dispersion.  
#If PERMANOVA is significant but PERMDISP IS NOT, then you can infer that there is only a location effect. If both tests are significant, then there is a dispersion effect for sure and there might also be (not always) a location effect.
# Perform both tests (PERMANOVA and PERMDISP) will help to determine the nature of the difference between any pair of groups, whether it be due to location, spread, or a combination of the two.Function adonis studied the differences in the group means, but function betadisper studies the differences in group homogeneities.

#MODEL- Effects of area and time in benthic communities -->Factor= Site (Fixed), levels= Bahia Nueva , Bahia Piramides, Factor 2 time (fixed) , T1 y T2 
#As we have many samples we used the script "balancear muestras.R" to select 37 fotocuadras for each combination of factors

#df_muestras_seleccionadas es un dataframe con 37 fotocuandrantes por arrecife y tiempo de muestreo

#HORIZONTAL
#balanceado
df_muestras_seleccionadas_H <- subset(df_muestras_seleccionadas,`reef area`=="horizontal")
Cover.data_horizontal.bc <- vegdist(log(df_muestras_seleccionadas_H[,-(1:22)]+1),method = "bray")

#todas las muestras
df_muestras_seleccionadas_H <- subset(PQ.COVER_T1_T2,`reef area`=="horizontal")
Cover.data_horizontal.bc <- vegdist(log(df_muestras_seleccionadas_H[,-(1:22)]+1),method = "bray")

#PERMDISP sites
dispersion <- betadisper(Cover.data_horizontal.bc, df_muestras_seleccionadas_H$site)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#PERMDISP Dataset
dispersion <- betadisper(Cover.data_horizontal.bc, df_muestras_seleccionadas_H$DataSet)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#PERMANOVA un factor dataset
adonis2(log(df_muestras_seleccionadas_H[,-(1:22)]+1)~df_muestras_seleccionadas_H$DataSet,sim.method = "bray",permutations = 999)
#PERMANOVA un factor site
adonis2(df_muestras_seleccionadas_H[,-(1:22)]~df_muestras_seleccionadas_H$site,sim.method = "bray",permutations = 999)
#PERMANOVA dos factores dataset y site
adonis2(log(df_muestras_seleccionadas_H[,-(1:22)]+1)~df_muestras_seleccionadas_H$site*df_muestras_seleccionadas_H$DataSet,sim.method = "bray",permutations = 999)

#PERMANOVA tres factores, dataset y site y reef anidado #NESTED
#https://stats.stackexchange.com/questions/188519/adonis-in-vegan-order-of-variables-or-use-of-strata/238962#238962
adonis2(log(df_muestras_seleccionadas_H[,-(1:22)]+1) ~ df_muestras_seleccionadas_H$site / df_muestras_seleccionadas_H$`reef name`, strata = df_muestras_seleccionadas_H$`reef name`, data = df_muestras_seleccionadas_H)

#VERTICAL
##balanceado
df_muestras_seleccionadas_V <- subset(df_muestras_seleccionadas,`reef area`=="vertical")
Cover.data_horizontal.bc_V <- vegdist(log(df_muestras_seleccionadas_V[,-(1:22)]+1),method = "bray")

#todas las muestras
df_muestras_seleccionadas_V <- subset(PQ.COVER_T1_T2,`reef area`=="vertical")
Cover.data_horizontal.bc_V <- vegdist(log(df_muestras_seleccionadas_V[,-(1:22)]+1),method = "bray")
                                    
#PERMDISP sites
dispersion <- betadisper(Cover.data_horizontal.bc_V, df_muestras_seleccionadas_V$site)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse

#PERMDISP Dataset
dispersion <- betadisper(Cover.data_horizontal.bc_V, df_muestras_seleccionadas_V$DataSet)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse


#PERMANOVA un factor dataset
adonis2(log(df_muestras_seleccionadas_V[,-(1:22)]+1)~df_muestras_seleccionadas_V$DataSet,sim.method = "bray",permutations = 999)
#PERMANOVA un factor site
adonis2(df_muestras_seleccionadas_V[,-(1:22)]~df_muestras_seleccionadas_V$site,sim.method = "bray",permutations = 999)
#PERMANOVA dos factores dataset y site
adonis2(log(df_muestras_seleccionadas_V[,-(1:22)]+1)~df_muestras_seleccionadas_V$site*df_muestras_seleccionadas_V$DataSet,sim.method = "bray",permutations = 999)
#PERMANOVA tres factores, dataset y site y reef anidado #NESTED
#https://stats.stackexchange.com/questions/188519/adonis-in-vegan-order-of-variables-or-use-of-strata/238962#238962
adonis2(log(df_muestras_seleccionadas_V[,-(1:22)]+1) ~ df_muestras_seleccionadas_V$site / df_muestras_seleccionadas_V$`reef name`, strata = df_muestras_seleccionadas_V$`reef name`, data = df_muestras_seleccionadas_V)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#SPECIES ACUMULATION CURVES-------
#Species accumulation curves (SAC) are used to compare diversity properties of community data sets using different accumulator functions. The classic method is "random" which finds the mean SAC and its standard deviation from random permutations of the data, or subsampling without replacement (Gotelli & Colwell 2001). The "exact" method finds the expected SAC using sample-based rarefaction method that has been independently developed numerous times (Chiarucci et al. 2008) and it is often known as Mao Tau estimate (Colwell et al. 2012). The unconditional standard deviation for the exact SAC represents a moment-based estimation that is not conditioned on the empirical data set (sd for all samples > 0). The unconditional standard deviation is based on an estimation of the extrapolated number of species in the survey area (a.k.a. gamma diversity), as estimated by function specpool. The conditional standard deviation that was developed by Jari Oksanen (not published, sd=0 for all samples). Method "coleman" finds the expected SAC and its standard deviation following Coleman et al. (1982). All these methods are based on sampling sites without replacement. In contrast, the method = "rarefaction" finds the expected species richness and its standard deviation by sampling individuals instead of sites. It achieves this by applying function rarefy with number of individuals corresponding to average number of individuals per site.

library(vegan)
pool <- poolaccum(PQ.PRESENCE_ABSENCE_T1_T2[,-(1:22)])
plot(pool,display = "S")
plot(pool, display = c("S","chao"),xlab="Number of Photoquadrats",alpha=NA)

Curve1 <- specaccum(PQ.PRESENCE_ABSENCE_T1_T2[,-(1:22)])
plot(Curve1)

library(vegan)
pool.all <- poolaccum(PQ.PRESENCE_ABSENCE[,-(1:22)])
plot(pool.all,display = "S")
plot(pool.all, display = c("S","chao"),xlab="Number of Photoquadrats",alpha=NA)


pool <- with(PQ.PRESENCE_ABSENCE_T1_T2, specpool(PQ.PRESENCE_ABSENCE_T1_T2[,-(1:22)], PQ.PRESENCE_ABSENCE_T1_T2$`reef name`))


##Species richness by Gulf region
PQ.PRESENCE_ABSENCE_T1_T2 <- PQ.PRESENCE_ABSENCE_T1_T2 %>%
  filter(!grepl("overhang", `reef area`, ignore.case = TRUE))
PQ.PRESENCE_ABSENCE_T1_T2 <- PQ.PRESENCE_ABSENCE_T1_T2 %>%
  filter(!grepl("cavefloor", `reef area`, ignore.case = TRUE))

PP <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Piramides")
PM <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Nueva")

PP_spp<- specaccum(PP[,-(1:22)],"rarefaction")
PM_spp <- specaccum(PM[,-(1:22)],"rarefaction")

pool_PP <- poolaccum(PP[,-(1:22)])
pool_PM <- poolaccum(PM[,-(1:22)])


library(ggplot2)
library(reshape2)
chao_PP <- data.frame(summary(pool_PP)$chao,check.names = FALSE)
S_PP <- data.frame(summary(pool_PP)$S,check.names = FALSE)
PP.pool <- merge(chao_PP,S_PP,by="N", all.x = TRUE) 
PP.pool$Sites <- "Bahia Piramides"

S_PM <- data.frame(summary(pool_PM)$S,check.names = FALSE)
chao_PM <- data.frame(summary(pool_PM)$chao,check.names = FALSE)
PM.pool <- merge(chao_PM,S_PM,by="N", all.x = TRUE) 
PM.pool$Sites <- "Bahia Nueva"

PM.PP <- rbind(PP.pool,PM.pool)

#reodenar para que coicidan colores con MDS
PM.PP$Sites <- factor(PM.PP$Sites, levels = c("Bahia Piramides", "Bahia Nueva"))

p <- ggplot(PM.PP, aes(x=N,color =Sites)) + geom_line(aes(y = S)) + geom_line(aes(y = Chao), linetype="twodash") + 
  theme_bw() + theme(legend.position = "bottom",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_blank()) + labs(fill = "Zona",x = "Número de fotocuadrantes",y = "Número de Taxa",title = "")
p + guides(color = guide_legend(title = "Zona"))


specpool(PM[,-(1:22)])
specpool(PP[,-(1:22)])


#Richness Analysis-------------------------------------------
#Comparisons among Gulf regions

#Factors
site <- PQ.PRESENCE_ABSENCE_T1_T2$site
reefname <- PQ.PRESENCE_ABSENCE_T1_T2$`reef name`
reefarea <- PQ.PRESENCE_ABSENCE_T1_T2$`reef area`
time <- PQ.PRESENCE_ABSENCE_T1_T2$DataSet
presence_absence<-PQ.PRESENCE_ABSENCE_T1_T2[,-(1:22)]
sppnumber <- specnumber(presence_absence)
specpool(presence_absence)
#Data Frame with spp number per photoquadrat
spp <- data.frame(reefname,reefarea,site,time,sppnumber)

#All reefs and orientations from presence absence matrix 
totalrichness <- rich(PQ.PRESENCE_ABSENCE_T1_T2[,-(1:22)],nrandom=499,verbose=TRUE)


#Subset data by reef depth
Presence.absence.data.PM <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Nueva")
Presence.absence.data.PM <- Presence.absence.data.PM[,-(1:22)]
Presence.absence.data.PP <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Piramides")
Presence.absence.data.PP <- Presence.absence.data.PP[,-(1:22)]

#T1
Presence.absence.data.PM <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Nueva")
Presence.absence.data.PM_T1 <- subset(Presence.absence.data.PM, DataSet=="T1")
Presence.absence.data.PM_T1 <- Presence.absence.data.PM_T1[,-(1:22)]
Presence.absence.data.PP <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Piramides")
Presence.absence.data.PP_T1 <- subset(Presence.absence.data.PP, DataSet=="T1")
Presence.absence.data.PP_T1 <- Presence.absence.data.PP_T1[,-(1:22)]

#T2
Presence.absence.data.PM <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Nueva")
Presence.absence.data.PM_T2 <- subset(Presence.absence.data.PM, DataSet=="T2")
Presence.absence.data.PM_T2 <- Presence.absence.data.PM_T1[,-(1:22)]
Presence.absence.data.PP <- subset(PQ.PRESENCE_ABSENCE_T1_T2, site=="Bahia Piramides")
Presence.absence.data.PP_T2 <- subset(Presence.absence.data.PP, DataSet=="T2")
Presence.absence.data.PP_T2 <- Presence.absence.data.PP_T2[,-(1:22)]

#some richness data by site
specpool(Presence.absence.data.PM_T1)
specpool(Presence.absence.data.PM_T2)
specpool(Presence.absence.data.PP_T1)
specpool(Presence.absence.data.PP_T2)
specpool(Presence.absence.data.PM)
specpool(Presence.absence.data.PP)

#AI
coverdata_photoquadrats <- PQ.COVER_T1_T2[,-(1:22)]
specpool(coverdata_photoquadrats)
coverdata_photoquadrats <- coverdata_photoquadrats[-1837,]
specpool(coverdata_AI)

#Create a list with data from the 2 regions depth
list.data.spp.region<-list(Presence.absence.data.PM,Presence.absence.data.PP)
names(list.data.spp.region)<-c("Bahia Nueva","Bahia Piramides")

#Table of shared spp by region
shared(list.data.spp.region)


#Calculate richness statistics using "rich" for each region
rich.results.region <- lapply(list.data.spp.region, function (j) {
  richness.results <- rich(matrix=j, nrandom=499,verbose=TRUE)
})


#comparison using "c2cv" and "c2m"
#PM[1] vs PP[2]
c2cv(com1=rich.results.region[[1]]$matrix,com2=rich.results.region[[2]]$matrix,nrandom=999,verbose=F)
x<-rich(rich.results.region[[1]]$matrix,nrandom=50,verbose=TRUE)
y<-rich(rich.results.region[[2]]$matrix,nrandom=50,verbose=TRUE)
c2m(pop1=x$sumrow,pop2=y$sumrow,nrandom=999,verbose=F)# no difference in avg richness bu fotoquadrat


#GLM richness------
##Ver si existe alguna relacion entre la riqueza de especies y la region de GN. Como la riqueza de especies es un conteo, serıa mas apropiado utilizar un modelo lineal generalizado (GLM) con una distribucion de errores de tipo Poisson.

#Cayuela, L. (2010). Modelos lineales generalizados (GLM), 58–87.

#Riquezai = α + β · Profi + εi

library(vegan)
library(rich)
library(ggplot2)

PQ.PRESENCE_ABSENCE_T1_T2_H <- subset(PQ.PRESENCE_ABSENCE_T1_T2,`reef area`=="vertical")
PQ.PRESENCE_ABSENCE_T1_T2_H <- PQ.PRESENCE_ABSENCE_T1_T2

#Factors
region <- as.factor(PQ.PRESENCE_ABSENCE_T1_T2_H$site)
#replace depth factors levels by numbers
#levels(depth) <- as.numeric(c(25, 15,5))
reefname <- as.factor(PQ.PRESENCE_ABSENCE_T1_T2_H$`reef name`)
reefarea <- as.factor(PQ.PRESENCE_ABSENCE_T1_T2_H$`reef area`)
date <- as.factor(PQ.PRESENCE_ABSENCE_T1_T2_H$DataSet)
presence_absence<-PQ.PRESENCE_ABSENCE_T1_T2_H[,-(1:22)]
sppnumber <- as.numeric(specnumber(presence_absence))

#Data Frame with spp number per photoquadrat
spp <- data.frame(reefname,reefarea,region,date,sppnumber)

#boxplot
plot(spp$region, spp$sppnumber, xlab="Region", ylab="Richness") 


#histogramas
library(plyr)
mu <- ddply(spp,c("region","date"), summarise, grp.mean=mean(sppnumber))
head(mu)

p <-  ggplot(spp, aes(x=sppnumber))+ geom_histogram()+ facet_grid(region +date~.) + theme_bw()

p+geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
             linetype="dashed")



g1 = glm(sppnumber ~ region+date,family = poisson, data = spp)
g1
summary(g1)


require(MuMIn)
options(na.action = "na.fail") # change the default "na.omit" to prevent models
# from being fitted to different datasets in
# case of missing values.
globalmodel <- glm(sppnumber ~ region * date ,family = poisson, data = spp)
combinations <- dredge(globalmodel)
print(combinations)


#INDICATOR VALUE INDICES (inval)------
library(indicspecies)
inval.data <- subset(PQ.COVER_T1_T2,`reef area`=="horizontal")#1
inval.data <- subset(inval.data,site=="Bahia Nueva")#2
inval.data <- subset(inval.data,site=="Bahia Piramides")#2
inval.data <- subset(PQ.COVER_T1_T2,`reef area`=="vertical")#1
inval.data <- subset(PQ.COVER_T1_T2,`reef area`=="vertical")#1
wetpt = multipatt(inval.data[,-(1:22)], inval.data$DataSet,control = how(nperm=999)) 
summary(wetpt,indvalcomp=TRUE)
summary(wetpt)
summary(wetpt,alpha=1) 


#covergae
indvalori = multipatt(PQ.COVER_T1_T2[,-(1:22)], PQ.COVER_T1_T2$site,duleg = TRUE,control = how(nperm=999)) 
summary(indvalori,indvalcomp=TRUE) 


#Generating species combinations
wetcomb = combinespecies(PQ.COVER_T1_T2[,-(1:22)], max.order = 2)$XC 
dim(wetcomb)

indvalspcomb = multipatt(wetcomb, PQ.COVER_T1_T2$site, duleg = TRUE, control = how(nperm=999)) 
summary(indvalspcomb, indvalcomp = TRUE)


sc= indicators(X=PQ.COVER_T1_T2[,-(1:22)], cluster=PQ.COVER_T1_T2$site, group="Bahia Nueva",max.order = 3, verbose=TRUE,At=0.5, Bt=0.2)
print(sc, sqrtIVt = 0.6)
sc2=pruneindicators(sc, At=0.8, Bt=0.2, verbose=TRUE)
print(sc2)


#presence-absence data
phi <- multipatt(PQ.COVER_T1_T2[,-(1:22)], PQ.COVER_T1_T2$site, func = "r.g",control = how(nperm=999))
summary(phi)
round(head(phi$str),3)



#HUMAN vs ROBOT------
# Crear un vector con los nombres de dfRobot
nombres_robot <- dfRobot$Name

# Seleccionar las filas de PQ.COVER_T1_T2 cuyos nombres estén en nombres_robot
dfHuman <- PQ.COVER_T1_T2[PQ.COVER_T1_T2$Name %in% nombres_robot, ]

dfHuman$`Annotation status` <- "Confirmed"
dfHuman <- dfHuman %>% filter(`Annotation status`=='Confirmed') %>% 
  dplyr::select(-matches("^Annotation"), -Points) %>% 
  mutate(source='human') 

## re arrange the fields
dfHuman <- dfHuman %>% relocate(Name, source, everything())

## Create matching ComMat with common labels only
## the first 22 cols are id cols
## pick labels and common labels
labelsRobot <- colnames(dfRobot)[22:ncol(dfRobot)]
labelsHuman <- colnames(dfHuman)[22:ncol(dfHuman)]
labelsHumRob <- intersect(labelsHuman, labelsRobot)

## bind df by rows and select only common labels
dfHumRob <- bind_rows(dfHuman, dfRobot)
dfHumRob <- dfHumRob %>% dplyr::select(Name:`reef name`, contains(labelsHumRob))
dfHumRob[is.na(dfHumRob)] <- 0

# Eliminar la columna "Image ID" utilizando subset
#dfHumRob <- subset(dfHumRob, select = -c(`Image ID`))
df_HumanRobot <- dfHumRob %>%  pivot_longer(cols = 8:ncol(dfHumRob), names_to = 'Label', values_to = 'Cover')

rm(labelsHuman,labelsHumRob,labelsRobot,nombres_robot)

#Proportion of the labels in the set
df <- read_csv(file.path(Data, "annotations_HUMAN.csv"))

labelset <- read.csv(file.path(Data, "labelset.csv"))
names(labelset)[names(labelset) == "Name"] <- "Long.Name"

df <- left_join(df, labelset, by=c('Label'='Short.Code'))

dfSummary <- df %>% group_by(Long.Name) %>% 
  summarise(nPoints = n(), site = paste0(unique(site), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(df),1)) %>% 
  arrange(-nPoints) %>% 
  relocate(Long.Name, nPoints, Percent, site)

dfSummary <- left_join(dfSummary, labelset, by="Long.Name") %>% 
  relocate(Long.Name, nPoints, Percent, site)

library(formattable)
formattable(dfSummary, list(Percent=color_bar("steelblue")))


## Photoquadrats vs Robot
#This compares the classification of the 100 point grid labels from the photo quadrats to the output of the robot. We will use the same Bray-Curtis approach as we're interested in the composition of the community. If the robot is good enough, the BC distance must remain close to zero in the majority of the quadrats. We will use the labels that contribute in total up to 95% of all the records. The reason behind this is that with few rare labels used in the training of the robot, the classification accuracy of these labels are expected to be low, hence producing an artificially higher Bray-Curtis distance.

## get fotoquadrat human community data
CM_HumanRobot <- df_HumanRobot
humanLabels <- CM_HumanRobot %>% filter(source=="human") %>% 
  group_by(Label) %>% 
  summarise(n=sum(Cover)) %>% 
  arrange(-n) %>% 
  mutate(acum=cumsum(n), acumP = acum/sum(n))
humanLabels95 <- humanLabels$Label[humanLabels$acumP<=0.96]                                                  

## make community matrix from robot
robotLabels <- CM_HumanRobot %>% filter(source=="robot") %>% 
  group_by(Label) %>% 
  summarise(n=sum(Cover)) %>% 
  arrange(-n) %>% 
  mutate(acum=cumsum(n), acumP = acum/sum(n))
robotLabels95 <- robotLabels$Label[robotLabels$acumP<=0.96]                                                  

## intersect the set of both labels and use only 95% labels
labels95 <- intersect(humanLabels95, robotLabels95)
CM_HumanRobot95 <- CM_HumanRobot %>% filter(Label %in% labels95)

## make it wide
CM_HumanRobot95_wide <- CM_HumanRobot95 %>% pivot_wider(id_cols = Name:`reef name`, names_from = 'Label', values_from = 'Cover')
CM_HumanRobot95_wide[is.na(CM_HumanRobot95_wide)] <- 0
CM_Human95 <- CM_HumanRobot95_wide %>% filter(source=="human")
CM_Robot95 <- CM_HumanRobot95_wide %>% filter(source=="robot")


#Generate the BC table. For each photo (Name) build a two-row community matrix (row1: Human, row2: Robot), and calculate the BC distance.

## calculate BC index
BC_RH <- data.frame(Name=character(), BC=numeric())
nLabels <- ncol(CM_Human95)
for (i in 1:nrow(CM_Human95)){
  robotIndex <- which(CM_Robot95$Name==CM_Human95$Name[i])
  if (length(robotIndex) > 0 ){
    CM_one <- bind_rows(CM_Human95[i,8:nLabels], CM_Robot95[robotIndex,8:nLabels])
    CM_one[is.na(CM_one)] <- 0
    BC_RH <- bind_rows(BC_RH, 
                       data.frame(Name = CM_Human95$Name[i],
                                  BC = as.numeric(vegdist(CM_one, na.rm = T))))
  }else {
    print(paste0("NOT FOUND in Robot: ", CM_Visual$Name[i]))
  }
}

BC_RH <- merge(BC_RH, dfRobot, by.x = "Name", by.y = "Name", all.x = TRUE)

# Seleccionar las columnas de interés de dfRobot
BC_RH <- BC_RH[, c("Name", "Date", "DataSet", "region", "site", "reef name", "reef area", "understory", "Depth","BC")]


reefs <- as.data.frame(BC_RH$`reef name`)
unique(reefs)

# Realizar los cambios en la columna "reef name"
BC_RH <- BC_RH %>%
  mutate(`reef name` = gsub("Shallow_West", "PA_I", `reef name`)) %>%
  mutate(`reef name` = gsub("Middle_West", "PA_II", `reef name`)) %>%
  mutate(`reef name` = gsub("Dolar", "DO", `reef name`)) %>%
  mutate(`reef name` = gsub("Derby", "DE", `reef name`)) %>%
  mutate(`reef name` = gsub("Middle_East", "PA_III", `reef name`))

library(RColorBrewer)
library(ggplot2)
library(ggpubr)
palette(brewer.pal(8, "Set2"))

strataColor = c(horizontal="#f7fcb9", vertical="#addd8e")

pp <- ggplot(BC_RH, aes(`reef name`, BC, fill=`reef area`))
pp + geom_boxplot(width=0.5, aes(fill=`reef area`), colour="black", position=position_dodge2(preserve="single") , outlier.size = 0.3, lwd=0.2) + 
  scale_color_brewer(palette="Dark2")+
  ylim(0, 0.3) + 
  facet_grid(~DataSet) + 
  theme_pubclean() + 
  theme(legend.position = 'top')


# Crear el gráfico con el orden personalizado en el eje x
pp <- ggplot(BC_RH, aes(`reef name`, BC, fill = `reef area`))
pp + geom_boxplot(width = 0.5, aes(fill = `reef area`), colour = "black", 
                  position = position_dodge2(preserve = "single"), outlier.size = 0.3, lwd = 0.2) + 
  scale_color_brewer(palette = "Dark2") +
  ylim(0, 0.3) + 
  facet_grid(~DataSet) + 
  theme_pubclean() + 
  theme(legend.position = 'top') +
  scale_x_discrete(labels = c(
    PA_I = "PA I",
    PA_II = "PA II",
    PA_III = "PA III",
    DO = "DO",
    DE = "DE",
    PE_I = "PE I",
    PE_II = "PE II",
    PC_I = "PC I",
    PC_II = "PC II",
    PP_I = "PP I",
    PP_II = "PP II"
  ))


BC_RH_1 <- BC_RH %>% 
  group_by(DataSet,`reef name`,`reef area`) %>% 
  mutate(Quadrat=1:n()) 

table(BC_RH_1$DataSet, BC_RH_1$site, BC_RH_1$`reef name`,BC_RH_1$`reef area`)

# GLM BC -------
library(lme4)
model1 = glmer(BC ~DataSet + `reef area`+ DataSet*`reef area` + (1|`reef name`/Quadrat), data=BC_RH_1, family=binomial)
summary(model1)

model2 = glmer(BC ~DataSet + `reef name`+ DataSet*`reef name` + (1|`reef name`/`reef area`), data=BC_RH_1, family=binomial)
summary(model2)
model3 = glm(BC ~DataSet + `reef area`+ `reef name` , data=BC_RH_1, family=binomial)
summary(model3)

library(multcomp)
testsite = glht(model, linfct = mcp(DataSet = "Tukey"))
summary(testsite)
plot(confint(testsite))


BCmean <- round(mean(BC_RH_1$BC),3)
BCn <- nrow(BC_RH_1)
BCsd <- sd(BC_RH_1$BC)
BCmargin <- qt(0.975,df=BCn-1)*BCsd/sqrt(BCn)
BCupper <- round(BCmean + BCmargin, 3)
BClower <- round(BCmean - BCmargin, 3) 
#As demonstrated, the classifier is insensitive to the variability produced by DataSet and reef area, being equally efficient no matter the source of the quadrat. **In general, the difference between the classifier and the visual estimate of the community cover of a mean quadrat is `r BCmean` with a 95% confidence interval of [`r BClower` - `r BCupper`]**



#GLM Point vs Point------

#Proportion of the labels in the set
annotation.human <- read_csv(file.path(Data, "annotations_human.csv"))
annotation.robot <- read_csv(file.path(Data, "annotations_robot.csv"))

annotation.robot$`reef name`
# Unir los dataframes usando merge()
annotation.human.robot <- merge(annotation.robot, annotation.human, 
                by = c("Name","Date","site","reef name","reef area", "Row", "Column"), 
                suffixes = c(".robot", ".human"), 
                all = TRUE)
annotation.human.robot <- annotation.human.robot %>%
  select(Name, Date, site, `reef name`, `reef area`, Row, Column, Label.robot, Label.human)


combinaciones <- table(annotation.human.robot$Label.robot, annotation.human.robot$Label.human)


# Crear una variable binaria que indique si las anotaciones coinciden o no
annotation.human.robot$Coinciden <- ifelse(annotation.human.robot$Label.robot == annotation.human.robot$Label.human, 1, 0)

# Ajustar un modelo de regresión logística
modelo_glm <- glm(Coinciden ~ 1, family = binomial, data = annotation.human.robot)

# Obtener un resumen del modelo
summary(modelo_glm)

# Calcular la frecuencia de coincidencias y no coincidencias
frecuencia <- table(annotation.human.robot$Coinciden)

# Crear un gráfico de barras
barplot(frecuencia,
        xlab = "Coincidencia",
        ylab = "Frecuencia",
        col = c("skyblue", "salmon"),
        ylim = c(0, max(frecuencia) * 1.2),
        beside = TRUE)



#Bland-Altman plots-------------

#The Bland-Altman plot determines differences between the two estimations against the observer estimations, or reference sensu (Krouwer JS (2008) Why Bland-Altman plots should use X, not (Y + X)/2 when X is a reference method. Stat Med 27:778–780) 
library(ggplot2)
library(cowplot)
library(plotly)
library(ggpubr) 
library(gridExtra)
library(gridGraphics)
library(doBy)
library(reshape)
library(dplyr)
library(plotly)
library(ggpubr)




library(dplyr)
#combine the 2 dataframes 
blandaltman <- merge(dfRobot, dfHuman, 
                     by = c("Name", "Date", "DataSet", "site", "reef name", "reef area"), 
                     all = TRUE,
                     suffixes = c(".robot", ".human"))


#"Dictyota dichotoma"                           
#"Macroalgae: Filamentous / filiform : red"     
#"Macroalgae:Encrusting:Red:Calcareous:Crustose"
#"Corynactis carnea"                            
#"Substrate: Consolidated (hard)"               
#"Macroalgae: Sheet-like / membraneous: red"    
#"Ulva"                                         
#"Aulacomya atra"


SC <- blandaltman %>%
  select(Name, Date, DataSet, site, `reef name`, `reef area`, 
         `Corynactis carnea.human`, `Corynactis carnea.robot`)%>%
  mutate(diff = `Corynactis carnea.human` - `Corynactis carnea.robot`) %>%
  mutate(avg = (`Corynactis carnea.robot`+`Corynactis carnea.human`)/2)

#by reef area
SC2 <- SC %>% 
  group_by(`reef area`) %>% 
  summarise_at(vars(`Corynactis carnea.robot`,`Corynactis carnea.human`,diff,avg), list(mean, sd)) 
colnames(SC2) <- c("reef area","Corynactis carnea.R.mean","Corynactis carnea.H.mean","diff","avg","Corynactis carnea.R.SD","Corynactis carnea.H.SD","diff.SD","avg.SD")



g1 <- ggplot(SC, aes(x=avg, y=diff,colour=`reef area`,shape=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Differencia en % de cobertura entre humano e IA", x="% de cobertura promedio por humano",title = "Corynactis carnea")+theme(legend.position="none",plot.margin=unit(c(0,0,1,1), "cm"))

g2 <- ggplot(SC2, aes(x=avg, y=diff,colour=`reef area`)) + geom_point()+theme_bw() + labs(y="", x="% de cobertura promedio por humano (± DS)") + theme(axis.title.y =element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+geom_hline(yintercept=0)+theme(legend.position="none",plot.margin=unit(c(0,1,1,-0.1), "cm"))+geom_errorbar(aes(ymin =diff-diff.SD,ymax =diff+diff.SD),width = 0.2) + geom_errorbarh(aes(xmin =avg-avg.SD ,xmax = avg+avg.SD),height =-.2)

#set y limits and brecks
limits <- c(-40, 40)#see plot to set 
breaks <- seq(limits[1], limits[2], by=8)

# assign common axis to both plots
p1.common.y <- g1 + scale_y_continuous(limits=limits, breaks=breaks)
p2.common.y <- g2 + scale_y_continuous(limits=limits, breaks=breaks)

# At this point, they have the same axis, but the axis lengths are unequal, so ...
# build the plots 
p1.common.y <- ggplot_gtable(ggplot_build(p1.common.y))
p2.common.y <- ggplot_gtable(ggplot_build(p2.common.y))

# copy the plot height from p1 to p2
p2.common.y$heights <- p1.common.y$heights
#plot grid
grid1 <- grid.arrange(p1.common.y,p2.common.y,ncol=2,widths=c(1,1))

#add legend
legend <- ggplot(SC, aes(x=avg, y=diff,colour=`reef area`,shape=site))+geom_hline(yintercept=0) + geom_point()+ theme_bw()+labs(y="Differencia en % de cobertura entre humano e IA", x="",title = "Dictyota dichotoma")+theme(legend.position="bottom",legend.title = element_blank(),legend.box="vertical",plot.margin=unit(c(0,0,1,1), "cm"))
legend_plot <- get_legend(legend)#take legend

plot.Dictyotadichotoma <- plot_grid(grid1,legend_plot, ncol = 1, rel_heights = c(.25,1))


plot_grid(
  plot.Dictyotadichotoma,
  plot.MacroalgasFilamentosasrojas,
  plot.Macroalgascalcareasinscrustantes,
  plot.Corynactiscarnea,
  plot.SUBSTRATE,
  plot.Macroalgaslaminaresrojas,
  plot.ulva,
  plot.Aulacomyaatra, ncol=2)


#