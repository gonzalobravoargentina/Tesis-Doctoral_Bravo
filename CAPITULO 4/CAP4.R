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
# 1- metadata.csv
# 2- percent_cover.csv

library(readr)
cover <- read_csv(file.path(Data, "percent_covers_v2.csv"))#read cover data
cover <- cover[-nrow(cover), ]#delete last row that has the totals
metadata <- read_csv(file.path(Data,"metadata_v2.csv"))#read metadata

#Merge photoquadrat.metadata and photoquadrat.cover
PQ.COVER<- merge(metadata,cover, by.x = "Name", by.y ="Image name", all.x = F) 

#Remove original data frames from enviroment
rm(cover)
rm(metadata)

#take out overhanh surfaces
PQ.COVER <- subset(PQ.COVER,`reef area`!="overhang" )


PQ.COVER$site[PQ.COVER$site == "Ushuaia"] <- "Canal Beagle"
PQ.COVER$site[PQ.COVER$site == "Islas Becases"] <- "Canal Beagle"


#we will take out all the photos that were used for training the algorithm 
PQ.COVER <- subset(PQ.COVER,`Annotation status`=="Unconfirmed")


table(PQ.COVER$site)

#Remove those categories that have realy low covers
# Selecciona todas las columnas excepto las que deseas eliminar
#PQ.COVER <- PQ.COVER[, !names(PQ.COVER) %in% c("BRY", "ASS", "WPOT", "SHAD", "EOBSS","ESS","ESUR","MAFG")]

#ANNOTATIONS----------------

library(readr)
#The file annotations.csv is in a zip file, you must decompress before using (github does not allow heavy files)
annotations <- read_csv(file.path(Data, "annotations.csv"))
annotationsGB <- subset(annotations, Annotator=="gonzalopatagonia"|Annotator=="Imported")


library(dplyr)
dfSummary <- annotationsGB %>% group_by(Label) %>% 
  summarise(nPoints = n(), site = paste0(unique(site), collapse = ", ")) %>% 
  mutate(Percent = round(100*nPoints/nrow(annotationsGB),1)) %>% 
  arrange(-nPoints) %>% 
  relocate(Label, nPoints, Percent, site)

#Created a csv 
#write.csv(dfSummary, 'tableCATAMI.csv',row.names = F)

#PRESENCE_ABSENCE---------

PQ.PRESENCE_ABSENCE <- PQ.COVER
# Obtener las columnas después de "Points"
col_start <- which(colnames(PQ.PRESENCE_ABSENCE) == "Points") + 1
col_end <- ncol(PQ.PRESENCE_ABSENCE)

# Reemplazar números mayores que 0 por 1 en las columnas
PQ.PRESENCE_ABSENCE[, col_start:col_end][PQ.PRESENCE_ABSENCE[, col_start:col_end] > 0] <- 1

rm(col_start,col_end)


#CONFUSION MATRIX--------
## read confusion matrix from Coralnet output and calculate some stats using caret package

library(caret)
library(stringr)

#confusion matrix new algorithm (EfficientNet extractor)
library(readr)

cm <- read.csv(file.path(Data, "confusion_matrix_full_0.csv"), header=FALSE)

##get the Label. It is tricky as the first label has two parenthesis. The valid label is the last one in parenthesis
tblLabels <- str_split( cm[,1], "\\(")
tblLabels <- gsub("\\)", "", unlist(lapply(tblLabels, tail,1)))
nLabels <- nrow(cm) 
## modify here to select the first n lables
#nLabels = 7

tt <- as.table(as.matrix(cm[1:nLabels,2:(nLabels+1)]))
rownames(tt) <- tblLabels[1:nLabels]
colnames(tt) <- tblLabels[1:nLabels]

cmm =confusionMatrix(tt)
as.matrix(cmm)

round(100*cmm$overall, 2)

write.csv(cmm$byClass,"metricas.csv")

tbl <- cmm$table
tblDF <- as.data.frame.matrix(tbl)
library(formattable)
formattable(tblDF, list(area(row=TRUE)~color_tile("transparent", "steelblue")))

tbl <- round(100*prop.table(cmm$table, margin = 1),1)
formatted_table <- formattable(as.data.frame.matrix(tbl), list(area(row=TRUE)~color_tile("transparent", "coral")))

                            
#% COVER PLOTS---------
library(reshape2)
#Create long type dataframe 
PQ.COVER_long = melt(PQ.COVER, id.vars = 1:21, measure.vars = 22:ncol(PQ.COVER), na.rm = T)
#rename columns because the ontop command is not working 
colnames(PQ.COVER_long)[22] <- "CATAMI"
colnames(PQ.COVER_long)[23] <- "cover"
colnames(PQ.COVER_long)[6] <- "reef_area"


#Calculate mean, SD, SE for cover data by factors 
Coverdata <- summaryBy(cover ~ CATAMI + region + site + reef_area,data=PQ.COVER_long, FUN = function(x) { c(mean = mean(x),SD=sd(x),SE = sqrt(var(x)/length(x)))})

unique(Coverdata$CATAMI)
#select those more abundant
categorias_seleccionadas <- c("SC", "MAENRC", "MAFR","MAEFR", "MASB","MASR", "MAEN","MAAR","CNTR", "CNCAC","ASC","AUS","AUC","SPMSI","CBW")

# Seleccionar las filas con las categorías deseadas
Coverdata <- Coverdata[Coverdata$CATAMI %in% categorias_seleccionadas, ]

Coverdata$site[Coverdata$site == "Las Grutas"] <- "L. Gtas."
Coverdata$site[Coverdata$site == "Puerto Lobos"] <- "Pto. L."
Coverdata$site[Coverdata$site == "Punta Buenos Aires"] <- "P. B. A."
Coverdata$site[Coverdata$site == "Puerto Piramides"] <- "Pto. P."
Coverdata$site[Coverdata$site == "Camarones"] <- "Cms"
Coverdata$site[Coverdata$site == "Comodoro Rivadavia"] <- "C. R."
Coverdata$site[Coverdata$site == "Caleta Olivia"] <- "C. O."
Coverdata$site[Coverdata$site == "Puerto Deseado"] <- "Pto. D."
Coverdata$site[Coverdata$site == "Isla de los Estados"] <- "IDLE"
Coverdata$site[Coverdata$site == "Canal Beagle"] <- "Beagle"

# Define el orden personalizado de los sitios
orden_sitios <- c("L. Gtas.", "Pto. L.", "P. B. A.", "Pto. P.", 
                  "Cms", "C. R.", "C. O.", "Pto. D.", 
                  "IDLE", "Beagle")

# Convierte la columna "site" en un factor con el orden personalizado
Coverdata$site <- factor(Coverdata$site, levels = orden_sitios)

# Ahora, crea el gráfico con el orden personalizado
ggplot(Coverdata, aes(x = reef_area, y = cover.mean, fill = factor(reef_area))) +
  geom_bar(alpha = 0.7, stat = "identity", color = "black", position = position_dodge()) +
  scale_color_grey() +
  geom_errorbar(aes(ymin = cover.mean - cover.SE, ymax = cover.mean + cover.SE), width = 0.2, position = position_dodge(0.9)) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 100)) +
  labs(fill = "Inclinacion sustrato", x = "", y = "Cobertura (%)", title = "") +
  theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  scale_fill_brewer(palette = "Set2") +
  facet_grid(site ~ CATAMI) +
  theme(strip.text.x = element_text(size = 11), strip.text.y = element_text(size = 11)) +
  theme(axis.text.x = element_text(size = 9, angle = 45, hjust = 1), axis.text.y = element_text(size = 9), axis.title.y = element_text(size = 9))


#nMDS data---------

#Horizontal data
horizontal.data <- subset(PQ.COVER,`reef area`=="horizontal")

# Carga la biblioteca dplyr
#library(dplyr)
# Establece la cantidad deseada de filas por región (en este caso, 200)
#n_filas_por_region <- 160
# Realiza el muestreo aleatorio dentro de cada región
#horizontal.data <- horizontal.data %>% group_by(region) %>% sample_n(n_filas_por_region, replace = FALSE) %>% ungroup()


#nMDS calculations (transformation + Bray)
library(vegan)
nMDShorizontal=metaMDS(horizontal.data[,-(1:21)],k=2,trymax=5,try = 5,distance ="bray",autotransform = T)
NMDS1.horizontal <-nMDShorizontal$points[,1] 
NMDS2.horizontal <- nMDShorizontal$points[,2]
MDS.plot_horizontal<-cbind(horizontal.data[,-(1:22)], NMDS1.horizontal, NMDS2.horizontal,horizontal.data$site,horizontal.data$region) 

#vertical data
vertical.data <- subset(PQ.COVER,`reef area`=="vertical")

# Establece la cantidad deseada de filas por región (en este caso, 200)
#n_filas_por_region <- 27
# Realiza el muestreo aleatorio dentro de cada región
#vertical.data <- vertical.data %>%  group_by(region) %>%sample_n(n_filas_por_region, replace = FALSE) %>%ungroup()

#nMDS calculations (no transformation + Bray)
nMDSvertical=metaMDS(vertical.data[,-(1:21)],k=2,trymax=5,try =5,distance ="bray",autotransform = T)

NMDS1.vertical <-nMDSvertical$points[,1] 
NMDS2.vertical <- nMDSvertical$points[,2]
MDS.plot_vertical<-cbind(vertical.data[,-(1:22)], NMDS1.vertical, NMDS2.vertical,vertical.data$site, vertical.data$region) 

#nMDS plots----
table(horizontal.data$`reef area`)
#nMDS plot horizontal
nMDShorizontal.plot <- ggplot(MDS.plot_horizontal, aes(NMDS1.horizontal, NMDS2.horizontal, color=horizontal.data$site, shape=horizontal.data$site)) +
  geom_point(position=position_jitter(.1), alpha=0.8) +
  theme_bw() + 
  theme(legend.position = "top", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank()) +scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7,8,9,10)) +
  annotate("text", x = max(NMDS1.horizontal) - 0.5, y = min(NMDS2.horizontal) - 0.5, label = paste('Stress =', round(nMDShorizontal$stress, 3))) + ggtitle("Fotocuadrantes superficie horizontal") + 
  scale_color_brewer(palette = "Set2")


#nMDS plot vertical
nMDSvertical.plot <- ggplot(MDS.plot_vertical, aes(NMDS1.vertical, NMDS2.vertical, color=vertical.data$site, shape=vertical.data$site)) + geom_point(position=position_jitter(.1), alpha=0.8)+ theme_bw() +  theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank()) + scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7,8,9,10)) + annotate("text", x = max(NMDS1.vertical) - 0.5, y = min(NMDS2.vertical) - 0.5, label = paste('Stress =', round(nMDSvertical$stress, 3))) + ggtitle("Fotocuadrantes superficie vertical") + scale_color_brewer(palette = "Set2")

# Plot legend
legend <-  ggplot(MDS.plot_vertical, aes(NMDS1.vertical, NMDS2.vertical, color = vertical.data$region, shape = vertical.data$region)) +mgeom_point(position = position_jitter(.1), alpha = 0.8) + stat_ellipse(type = 't', size = 1) + theme_bw() +  theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank()) + scale_color_brewer(palette = "Set2", name = "Region") + scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7), name = "Region") + annotate("text", x = max(NMDS1.vertical) - 0.5, y = min(NMDS2.vertical) - 0.5, label = paste('Stress =', round(nMDSvertical$stress, 3))) + ggtitle("Fotocuadrantes superficie vertical") +guides(color = guide_legend(override.aes = list(alpha = 0.3)))


library(ggplot2)
library(cowplot)
#plot1
nMDSbysurface<- plot_grid(nMDShorizontal.plot,nMDSvertical.plot,ncol = 1, align = "v")
#Create plot grid and save
legend_plot <- get_legend(legend)#take legend
fig1 <- plot_grid(nMDSbysurface, legend_plot, ncol = 1, rel_heights = c(1,.09))


#nMDS plots avg-----------

#horizontal
colnames(MDS.plot_horizontal)[34] <- "region"
colnames(MDS.plot_horizontal)[33] <- "site"


# Calcular los puntos promedio por región
average_points_horizontal_region <- MDS.plot_horizontal %>%
  group_by(region) %>%
  summarize(NMDS1_avg = mean(NMDS1.horizontal), NMDS2_avg = mean(NMDS2.horizontal))

# Calcular los puntos promedio por sitio
average_points_horizontal_site <- MDS.plot_horizontal %>%
  group_by(site) %>%
  summarize(NMDS1_avg = mean(NMDS1.horizontal), NMDS2_avg = mean(NMDS2.horizontal))

# Crear el gráfico MDS para region
nMDShorizontal.plot <- ggplot(average_points_horizontal_region, aes(NMDS1_avg, NMDS2_avg)) +
  geom_point(data = average_points_horizontal_region, aes(NMDS1_avg, NMDS2_avg, color = region, shape = region), size = 3) +theme_bw() + theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank()) + scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7))  

# Crear el gráfico MDS para sitio
mdssitio_horizontal <-ggplot(average_points_horizontal_site, aes(NMDS1_avg, NMDS2_avg)) + geom_point(aes(color = site), size = 3) + geom_text(aes(label = site, x = NMDS1_avg + 0.07, y = NMDS2_avg + 0.04), size = 5) + theme_bw() +theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(),plot.title = element_text(size = 20)) + ggtitle("A- Fotocuadrantes superficie horizontal")


#vertical
colnames(MDS.plot_vertical)[34] <- "region"
colnames(MDS.plot_vertical)[33] <- "site"


# Calcular los puntos promedio por región
average_points_vertical_region <- MDS.plot_vertical %>%
  group_by(region) %>%
  summarize(NMDS1_avg = mean(NMDS1.vertical), NMDS2_avg = mean(NMDS2.vertical))

average_points_vertical_site <- MDS.plot_vertical %>%
  group_by(site) %>%
  summarize(NMDS1_avg = mean(NMDS1.vertical), NMDS2_avg = mean(NMDS2.vertical))

# Crear el gráfico MDS para region
ggplot(average_points_vertical_region, aes(NMDS1_avg, NMDS2_avg)) +
  geom_point(data = average_points_vertical_region, aes(NMDS1_avg, NMDS2_avg, color = region, shape = region), size = 3) + theme_bw() + theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(),) + scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7))  

# Crear el gráfico MDS para sitio
mdssitio_vertical <- ggplot(average_points_vertical_site, aes(NMDS1_avg, NMDS2_avg)) + 
geom_point(aes(color = site), size = 3) + geom_text(aes(label = site, x = NMDS1_avg + 0.07, y = NMDS2_avg + 0.04), size = 5) + theme_bw() + theme(legend.position = "none", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), plot.title = element_text(size = 20)) + 
  ggtitle("B- Fotocuadrantes superficie vertical")

# Plot legend
legend <-ggplot(average_points_vertical_site, aes(NMDS1_avg, NMDS2_avg)) +geom_point(aes(color = site), size = 3) +geom_text(aes(label = site), vjust = 1.5) + theme_bw() +theme(legend.position = "bottom", axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(),axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank()) + labs(color = "Sitio")  

library(ggplot2)
library(cowplot)
#plot1
nMDSbysites<- plot_grid(mdssitio_horizontal,mdssitio_vertical,ncol = 1, align = "v")
#Create plot grid and save
legend_plot <- get_legend(legend)#take legend
fig2 <- plot_grid(nMDSbysites, legend_plot, ncol = 1, rel_heights = c(1,.09))



#PERMANOVA with cover data----------------------
#Prior to PERMANOVA I used betadisper() to test for homogeneity of multivariate dispersion.  
#If PERMANOVA is significant but PERMDISP IS NOT, then you can infer that there is only a location effect. If both tests are significant, then there is a dispersion effect for sure and there might also be (not always) a location effect.
# Perform both tests (PERMANOVA and PERMDISP) will help to determine the nature of the difference between any pair of groups, whether it be due to location, spread, or a combination of the two.Function adonis studied the differences in the group means, but function betadisper studies the differences in group homogeneities.

#MODEL- Effects of area and time in benthic communities -->Factor= Site (Fixed), levels= Bahia Nueva , Bahia Piramides, Factor 2 time (fixed) , T1 y T2 
#As we have many samples we used the script "balancear muestras.R" to select 37 fotocuadras for each combination of factors

#df_muestras_seleccionadas es un dataframe con 37 fotocuandrantes por arrecife y tiempo de muestreo

#HORIZONTAL

#Horizontal data
horizontal.data <- subset(PQ.COVER,`reef area`=="horizontal")
#balanceado
Cover.data_horizontal.bc <- vegdist(log(horizontal.data[,-(1:21)]+1),method = "bray")

#PERMDISP sites
dispersion <- betadisper(Cover.data_horizontal.bc, horizontal.data$site)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse
anova(dispersion)
permutest(dispersion, pairwise = TRUE, permutations = 999)
(PERMDISP <- TukeyHSD(dispersion))

#PERMANOVA un factor site
adonis2(log(horizontal.data[,-(1:21)]+1)~horizontal.data$site,sim.method = "bray",permutations = 999)

#PARWISE PERMANOVA
library(pairwiseAdonis)
pairwise.adonis(horizontal.data[,-(1:21)],horizontal.data$site,sim.method ="bray")
pairwise.adonis(Cover.data_horizontal.bc,factors=horizontal.data$region,p.adjust.m='holm')

#VERTICAL
#vertical data
vertical.data <- subset(PQ.COVER,`reef area`=="vertical")
#balanceado
Cover.data_vertical.bc <- vegdist(log(vertical.data[,-(1:21)]+1),method = "bray")

#PERMDISP sites
dispersion <- betadisper(Cover.data_vertical.bc, vertical.data$region)
permutest(dispersion)
plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse
anova(dispersion)
permutest(dispersion, pairwise = TRUE, permutations = 999)
(PERMDISP <- TukeyHSD(dispersion))

#PERMANOVA un factor region
adonis2(log(vertical.data[,-(1:21)]+1)~vertical.data$region,sim.method = "bray",permutations = 999)

#PARWISE PERMANOVA
library(pairwiseAdonis)
pairwise.adonis(vertical.data[,-(1:21)],vertical.data$region,sim.method ="bray")




#INDICATOR VALUE INDICES (inval)------
library(indicspecies)

#coverage
indvalori = multipatt(PQ.COVER[,-(1:21)], PQ.COVER$site,duleg = TRUE,control = how(nperm=999)) 
summary(indvalori,indvalcomp=TRUE) 



#CLUSTER
PQ.COVER_region <- PQ.COVER[, c(3, 22:ncol(PQ.COVER))]

rownames(PQ.COVER_region) <- PQ.COVER_region$region
PQ.COVER_region <- PQ.COVER_region[, -1]  # Elimina la columna "region" si ya no la necesitas como columna

# calculate distance
keep.cols <- which(!names(wbInfo) %in% c("region", "site", + "capital", "iso3c"))  
wbDaisy <- daisy(x=wbInfo[, keep.cols]) 
wbH <- hclust(wbDaisy) > plot(wbH)



cluster<- kmeans(PQ.COVER[,-(1:21)], 3, nstart = 20)
cluster

table(cluster$cluster, PQ.COVER$region)



distance = dist(PQ.COVER_region)

mydata.hclust = hclust(distance)
plot(mydata.hclust)
plot(mydata.hclust,labels=mydata$,main='Default from hclust')
plot(mydata.hclust,hang=-1, labels=mydata$Company,main='Default from hclust')


#Richness Analysis-------------------------------------------


#Factors
site <- PQ.COVER$site
reefname <- PQ.COVER$`reef name`
reefarea <- PQ.COVER$`reef area`
region<- PQ.COVER$region
richness<-PQ.COVER[,-(1:21)]
sppnumber <- specnumber(richness)
specpool(richness)


#Data Frame with spp number per photoquadrat
spp <- data.frame(reefname,reefarea,site,region,sppnumber)

# Definir el orden personalizado de los sitios
orden_sitios <- c("Las Grutas", "Puerto Lobos", "Punta Buenos Aires",
                  "Puerto Piramides", "Camarones", "Comodoro Rivadavia",
                  "Caleta Olivia", "Puerto Deseado", "Isla de los Estados",
                  "Canal Beagle")

orden_sitios <- rev(c("Las Grutas", "Puerto Lobos", "Punta Buenos Aires",
                      "Puerto Piramides", "Camarones", "Comodoro Rivadavia",
                      "Caleta Olivia", "Puerto Deseado", "Isla de los Estados",
                      "Canal Beagle"))


# Aplicar el orden personalizado
spp$site <- factor(spp$site, levels = orden_sitios)

# Crear un boxplot
ggplot(spp, aes(x = site, y = sppnumber)) +
  geom_boxplot() + theme_bw()+
  labs(x = "Sitio", y = "Cantidad de categorias CATAMI") + theme(axis.text.x = element_text(size = 12))+ coord_flip()


ggplot(spp, aes(x = reefarea, y = sppnumber)) +
  geom_boxplot() + theme_bw()+
  labs(x = "Sitio", y = "Cantidad de categorias CATAMI") + theme(axis.text.x = element_text(size = 12))


#SOLO ANNOTACIONES DEL ROBOT CON > 80 
#seleccionar las notacions de robot con un machine confidence superior a 80
annotations_robot <- annotations[grepl("robot", annotations$Annotator) & annotations$`Machine confidence 1` > 80, c("Name","region", "site", "reef name", "reef area","Label")]

annotations_robot$site[annotations_robot$site == "Islote Moyano"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Puerto Cook"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Salvamento"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Basil Hall"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Roca Villa"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Hoppner"] <- "Isla de los Estados"
annotations_robot$site[annotations_robot$site == "Ushuaia"] <- "Canal Beagle"
annotations_robot$site[annotations_robot$site == "Tierra del Fuego"] <- "Canal Beagle"

# Calcular la cantidad de categorías distintas en la columna `Label` por foto (`Name`)
annotations_robot_richness <- aggregate(Label ~ Name + region + site + `reef name` + `reef area`, data = annotations_robot, FUN = function(x) length(unique(x)))


orden_sitios <- c("Las Grutas", "Puerto Lobos", "Punta Buenos Aires",
                  "Puerto Piramides", "Camarones", "Comodoro Rivadavia",
                  "Caleta Olivia", "Puerto Deseado", "Isla de los Estados",
                  "Canal Beagle")

# Aplicar el orden personalizado
annotations_robot_richness$site <- factor(annotations_robot_richness$site, levels = orden_sitios)

annotations_robot_richness <- subset(annotations_robot_richness, annotations_robot_richness$`reef area`=="horizontal")

# Crear un boxplot
ggplot(annotations_robot_richness , aes(x = site, y = Label)) +
  geom_boxplot() + theme_bw()+
  labs(x = "Sitio", y = "Cantidad de categorias CATAMI") 



#GLM richness------
##Ver si existe alguna relacion entre la riqueza de especies y la region de GN. Como la riqueza de especies es un conteo, serıa mas apropiado utilizar un modelo lineal generalizado (GLM) con una distribucion de errores de tipo Poisson.

#Cayuela, L. (2010). Modelos lineales generalizados (GLM), 58–87.

#Riquezai = α + β · Profi + εi

library(vegan)
library(rich)
library(ggplot2)

#Factors
site <- as.factor(PQ.COVER$site)
reefname <- as.factor(PQ.COVER$`reef name`)
reefarea <- as.factor(PQ.COVER$`reef area`)
region <- as.factor(PQ.COVER$region)
lat <- as.factor(PQ.COVER$Latitude)
presence_absence<-PQ.COVER[,-(1:21)]
sppnumber <- as.numeric(specnumber(presence_absence))

#Data Frame with spp number per photoquadrat
spp <- data.frame(site,reefarea,region,reefname,lat,sppnumber)

# Aplicar el orden personalizado
spp$site <- factor(spp$site, levels = orden_sitios)


#boxplot
plot(spp$site, spp$sppnumber, xlab="Site", ylab="Cantidad categorias CATAMI") 


#histogramas
library(plyr)
mu <- ddply(spp,c("site","reefarea"), summarise, grp.mean=mean(sppnumber))
head(mu)

p <-  ggplot(spp, aes(x=sppnumber))+ geom_histogram()+ facet_grid(site) + theme_bw()

p+geom_vline(data=mu, aes(xintercept=grp.mean, color="red"),
             linetype="dashed")


#MODELO un factor prof -----
g1 = glm(sppnumber ~ site,family = poisson, data = spp)
g1
summary(g1)


library(nlme) 
lme5 <- lme(sppnumber ~ lat, data = spp, random = ~ 1|factor(site)) 
summary(lme5)


glmer6 <- glmer(sppnumber ~ lat + (lat|site), data = spp, family=poisson)
summary(glmer6)
