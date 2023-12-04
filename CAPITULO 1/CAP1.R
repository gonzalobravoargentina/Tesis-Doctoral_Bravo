
#DATA---------------------------------------------------------
# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
## data sources
Data <- "Data"

# Compare data from RLS surrvey in Golfo Nuevo 

#read RLS data from GN and GSJ----
RLS_GN <- read.csv(file.path(Data,"Reef_Life_Survey_(RLS)#_Invertebrates-invertebrate_surveys.csv"))
#Calculate desnsity (ind.m2) of species in each block (each block is 50m x 1m band=50m2)
#  50m2-------RLS_GN$Total
#  1m2--------x
RLS_GN$Density <- RLS_GN$Total/50

#select only GN
RLS_GN <- subset(RLS_GN,SiteLat < -42.5)
RLS_GN$reef.name <- RLS_GN$Site

#Select only reefs from Piramides
RLS_GN <- subset(RLS_GN, reef.name=="Colombo" |reef.name=="Pafer"|reef.name=="PNU")

RLS_GN$study <- "RLS"


#Read cryptic fish data RLS-----
RLS_GN_Fish <- read.csv(file.path(Data,"Cryptobenthic_fish_abundance_RLS.csv"))
RLS_GN_Fish$Density <-RLS_GN_Fish$total/50

names(RLS_GN_Fish)
#select only GN
RLS_GN_Fish <- subset(RLS_GN_Fish,latitude < -42.5)
RLS_GN_Fish$reef.name <- RLS_GN_Fish$site_name

#Select only reefs from Piramides
RLS_GN_Fish<- subset(RLS_GN_Fish, reef.name=="Colombo" |reef.name=="Pafer"|reef.name=="PNU")
colnames(RLS_GN_Fish)[25] <- "Taxon"


library(leaflet)
map <- leaflet(RLS_GN) %>%
  addTiles() %>%
  addMarkers(~SiteLong, ~SiteLat, popup = ~paste("Latitud:", SiteLat, "<br>Longitud:", SiteLong))

library(doBy)
RLS_GN_Fish <- summaryBy(Density  ~ Taxon +reef.name,   data =RLS_GN_Fish, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })

RLS_GN_Fish <- summaryBy(Density.mean  ~ Taxon,   data =RLS_GN_Fish, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })

#Data Bravo et al 2020-----
Density.data.Bravo <- read.csv(file.path(Data,"Density_Data(Pardelas2019).csv"))
#transform count data to density 
Density.data.Bravo[,-(1:20)] <- Density.data.Bravo[,-(1:20)]/0.0625
#reshape data-frame 
library(reshape)
Density.data.Bravo <- melt(Density.data.Bravo,measure.vars=names(Density.data.Bravo[0,-(0:20)]), id.vars =(c("Name", "reef.name","reef.area")))
colnames(Density.data.Bravo)[5] <- "Density"
colnames(Density.data.Bravo)[4] <- "Taxon"

#Select only horizonta data for comparison with RLS
Density.data.Bravo <- subset(Density.data.Bravo, reef.area=="horizontal" | reef.area=="vertical")
Density.data.Bravo <- subset(Density.data.Bravo, reef.name=="Deep_East"|reef.name=="Deep_West" |reef.name=="Middle_West") 
Density.data.Bravo$study <- "Bravo"

#take out point on taxon names 
Density.data.Bravo$Taxon<-gsub(".", " ", Density.data.Bravo$Taxon, fixed=TRUE)

Density.data.Bravo <- summaryBy(Density  ~ Taxon + reef.name,   data =Density.data.Bravo, FUN = mean)

colnames(Density.data.Bravo)[3] <- "Density"
Density.data.Bravo$study <- "Bravo"

#merge both data frames 
Densitydata.RLSvsBRAVO <- merge(Density.data.Bravo, RLS_GN, by=c("Taxon","reef.name","Density","study"), all = TRUE)



#calculate mean density 
library(doBy)
Density.Data.comparison <- summaryBy(Density  ~ Taxon + study,   data =Densitydata.RLSvsBRAVO, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })


print(df_transformed)
Con este código, se generará un nuevo dataframe llamado "df_transformed" con la estructura que deseas, mostrando las medias y desviaciones estándar de las columnas "Density.mean" y "Density.SD" para los grupos "Bravo" y "RLS".








write.csv(Density.Data.comparison, 'densitydata.RLSvsBRAVO.csv',row.names = F)


mean(RLS_GN$Depth)




library(doBy)
names(RLS_GN$reef.name)
densitybyREEF.RLS <- summaryBy(Density  ~ Taxon + reef.name,   data =RLS_GN, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })


library(doBy)
names(Density.data.Bravo)
densitybyREEF.Bravo <- summaryBy(Density  ~ Taxon +reef.name,   data =Density.data.Bravo, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })
