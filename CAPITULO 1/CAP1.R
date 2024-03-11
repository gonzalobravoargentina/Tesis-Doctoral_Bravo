
#DATA----------
## data sources
Data <- "Data"

#Comparison data RLS (Reef Life Survey) with Bravo thesis in Golfo Nuevo----- 

#read RLS data from Golfo Nuevo and Golfo San jose----
RLS_GN <- read.csv(file.path(Data,"Reef_Life_Survey_(RLS)#_Invertebrates-invertebrate_surveys.csv"))

#Calculate desnsity (ind.m2) of species in each block (each block is 50m x 1m band=50m2)
RLS_GN$Density <- RLS_GN$Total/50

#select only Golfo Nuevo
RLS_GN <- subset(RLS_GN,SiteLat < -42.5)
RLS_GN$reef.name <- RLS_GN$Site

#Select only reefs from Piramides
RLS_GN <- subset(RLS_GN, reef.name=="Colombo" |reef.name=="Pafer"|reef.name=="PNU")

RLS_GN$study <- "RLS"


#Read cryptic fish data RLS-----
RLS_GN_Fish <- read.csv(file.path(Data,"Cryptobenthic_fish_abundance_RLS.csv"))
RLS_GN_Fish$Density <-RLS_GN_Fish$total/50


#select only GN
RLS_GN_Fish <- subset(RLS_GN_Fish,latitude < -42.5)
RLS_GN_Fish$reef.name <- RLS_GN_Fish$site_name

#Select only reefs from Piramides
RLS_GN_Fish<- subset(RLS_GN_Fish, reef.name=="Colombo" |reef.name=="Pafer"|reef.name=="PNU")
colnames(RLS_GN_Fish)[25] <- "Taxon"


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

#write.csv(Density.Data.comparison, 'densitydata.RLSvsBRAVO.csv',row.names = F)


library(doBy)
names(RLS_GN$reef.name)
densitybyREEF.RLS <- summaryBy(Density  ~ Taxon + reef.name,   data =RLS_GN, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })


library(doBy)
names(Density.data.Bravo)
densitybyREEF.Bravo <- summaryBy(Density  ~ Taxon +reef.name,   data =Density.data.Bravo, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Water Temperature DATA-------

#Data of temperature on all sites
tempdata <- read.csv(file.path(Data,"tempdata_allsites_v2.csv"))

# covert date to Date class
tempdata$Datetime<- as.Date(tempdata$Datetime,"%d/%m/%Y")

library(dplyr)
# Calcular el recuento de datos, y las fechas mínima y máxima para cada SITE
resumen <- tempdata %>%
  group_by(SITE) %>%
  summarize(Recuento = n(),
            Fecha_Minima = min(Datetime),
            Fecha_Maxima = max(Datetime))


# temperature every 1 hr, calculate mean temperature per day
library(doBy)
temperaturedaily <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata ,FUN = function(x) { c(mean = mean(x), Range=range(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

# covert date to Date class
temperaturedaily$Datetime<- as.Date(temperaturedaily$Datetime,"%d/%m/%Y")

library(ggplot2)
ggplot(temperaturedaily, aes(x = Datetime, y =Temp..mean)) + 
  geom_line(aes(color = SITE), size = 1) + labs(x="Date", y="Temperature (°C)") +theme_minimal()


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Plot USh, comodroro and Golfo Nuevo 

tempdata <- subset(tempdata,SITE=="USH"|SITE=="PC"|SITE=="Comodoro")
# temperature every 1 hr, calculate mena temperature per day

library(doBy)
temperaturedaily <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata ,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })


# covert date to Date class
temperaturedaily$Datetime<- as.Date(temperaturedaily$Datetime,"%d/%m/%Y")

library(ggplot2)
ggplot(temperaturedaily, aes(x = Datetime, y =Temp..mean)) + 
  geom_line(aes(color = SITE), size = 0.5) + labs(x="Fecha", y="Temperatura(°C)") + ggtitle("Temperatura del agua a 5 m de profundidad")+
  theme_minimal()

library(dplyr)
temperaturedaily <- temperaturedaily %>%
  mutate(SITE = recode(SITE,
                       "Comodoro" = "Comodoro Rivadavia",
                       "PC" = "Puerto Madryn",
                       "USH" = "Ushuaia"))

ggplot(temperaturedaily,aes(x =Datetime,y =Temp..mean,colour= SITE)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+scale_y_continuous(breaks = seq(4, 20, by = 1))+theme_minimal()+ ggtitle("Temperatura del agua a 5 m de profundidad")+ labs(x="Fecha", y="Temperatura(°C)")

ggplot(temperaturedaily, aes(x = Datetime, y = Temp..mean, colour = SITE)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-02-05"), linetype = "dashed", color = "red", size = 0.5, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", lim = as.Date(c("2018-07-01", "2019-06-30"))) +
  scale_y_continuous(breaks = seq(4, 20, by = 1)) +
  theme_minimal() +
  ggtitle("Temperatura del agua a 5 m de profundidad") +
  labs(x = "Fecha", y = "Temperatura (°C)")


pp <- ggplot(temperaturedaily,aes(x =Datetime,y =Temp..mean,colour= SITE)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b")+theme_minimal()+ ggtitle("Temperatura del agua a 5 m de profundidad")+ labs(x="Fecha", y="Temperatura(°C)")

library(plotly)
ggplotly(pp) %>% plotly::config(displayModeBar = F) 


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Get air temperature for the same period of time
airtempdata <- read.csv(file.path(Data,"air_temperatureMETEOBLU_2018.csv"))
airtempdata <- airtempdata[-(0:8),]
colnames(airtempdata) <- airtempdata[1,]
airtempdata <- airtempdata[-1,]
library(bReeze)
airtempdata$timestamp2 <- timestamp(timestamp=airtempdata$timestamp,"%Y%m%dT%H%M")
airtempdata$day <- format(airtempdata$timestamp2, "%m/%d/%Y")
# covert date to Date class
airtempdata$Datetime<- as.Date(airtempdata$day,"%m/%d/%Y")

airtempdata$SITE <- "Air temperature METEOBLUE"
airtempdata$Temp. <- as.numeric(airtempdata$`Puerto Pirámides Temperature [2 m elevation corrected]`)
airtempdata$Datetime
library(doBy)
airtempdatadaily <- summaryBy(Temp. ~ SITE + Datetime ,  data =airtempdata,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

#temperaturedata <- rbind(temperaturedaily, airtempdatadaily)
#temperaturedata$Datetime <- as.Date(temperaturedata$Datetime,"%d/%m/%Y")

airtempmonth  <- airtempdatadaily  %>% mutate(month = format(Datetime, "%m"))
airtempmonth <- airtempmonth %>% group_by(month) %>%  summarise(min = min(Temp..Range1), max = max(Temp..Range2))


ggplot(temperaturedaily,aes(x =Datetime,y =Temp..mean,colour= SITE)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b")+theme_minimal()+ ggtitle("5 m sea temperature") + geom_point(data = airtempdatadaily, aes(x = Datetime, y = Temp..mean))


tempdata_PC <- read.csv(file.path(Data,"tempdata_PC-Pcostero.csv"))

# covert date to Date class
tempdata_PC$datetransform<- as.Date(tempdata_PC$Datetime,"%d/%m/%Y")
ggplot(tempdata_PC, aes(x=datetransform,y=Temp., colour=SITE)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=SITE), alpha=0.3) +
  theme_bw()


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#TEMPERATURE data from loggers on 3 reefs
tempdata_3reefs <- read.csv(file.path(Data,"temperaturefiles_(april-sep)pardelas.csv"))

# temperature every 1 hr, calculate mean temperature per day
library(doBy)
temperaturedaily <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata_3reefs ,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

temperaturedailymaxmin <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata_3reefs ,FUN = function(x) { c(max=max(x), min=min(x),diference=max(x)-min(x))})

temperaturedailymaxmin2 <- summaryBy(Temp..max + Temp..min ~ SITE ,  data =temperaturedailymaxmin ,FUN = function(x) { c(max=max(x), min=min(x))})


temperaturedailydepth <- summaryBy(Temp..mean ~ SITE,  data =temperaturedaily ,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

# covert date to Date class
temperaturedaily$Datetime<- as.Date(temperaturedaily$Datetime,"%d/%m/%Y")
#plot
library(ggplot2)
temperatureplot1 <- ggplot(temperaturedaily, aes(x = Datetime, y =Temp..mean)) + geom_line(aes(color = SITE), size = 0.5) + labs(x="Month", y="Temperature (°C)", fill="Rocky reefs") + ggtitle("Water Sea temperature")+ 
  theme_bw() + theme(text = element_text(size=20))+scale_x_date(date_breaks = "1 month", date_labels = "%b/%Y")+ scale_colour_discrete(name="Rocky reefs",breaks=c("Alero20m", "PardelasMedio", "Pcostero"),labels=c("25 m", "15 m", "5 m"))

ggsave(filename = "temperatureplot1.png",plot = temperatureplot1,width=12,height = 8)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#read file with LUX and cloud data
ligthtempdata_pardelas <- read.csv(file.path(Data,"ligth_temperature_Pardelas2019.csv"))

#Descriptive Statistics
library(doBy)
summaryBy(Temp + Lux  ~ Site, data = ligthtempdata_pardelas, 
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# covert date to POSIXct class
library(lubridate)
ligthtempdata_pardelas$Datetimehr <- lubridate::mdy_hms(ligthtempdata_pardelas$Date.Time.GMT.03.00)

#select only one reef of each depth range (eliminate Deep_East)
ligthtempdata_pardelas <- subset(ligthtempdata_pardelas,Site!="Deep_East")

library(ggplot2)
ggplot(ligthtempdata_pardelas,aes(x=Datetimehr,y=Lux,colour= Site)) +
  geom_line() + scale_x_datetime(date_breaks = "24 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot Ligth and clouds
ggplot(ligthtempdata_pardelas, aes(x = Datetimehr)) + geom_line(aes(y = Lux, colour = Site))+ scale_x_datetime(date_breaks = "12 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ geom_line(aes(y =Cloud*100, colour = "Cloud cover"))+ scale_y_continuous(sec.axis = sec_axis(~./100, name = "Cloud cover [%]"))+ scale_color_discrete(name = "", labels = c("Cloud Cover", "25 m", "15 m"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ligthtempdata <- read.csv(file.path(Data,"Lux_reef-sections-PuntaEste.csv"))
names(ligthtempdata)
#Descriptive Statistics
library(doBy)
summaryBy(Temp + Lux  ~ Site, data = ligthtempdata, 
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# covert date to POSIXct class
library(lubridate)
ligthtempdata$Datetimehr <- lubridate::mdy_hms(ligthtempdata$Date.Time..GMT.03.00)

ligthtempdata <- subset(ligthtempdata,Reef.area!="cavefloor")

library(ggplot2)
ggplot(ligthtempdata,aes(x=Datetimehr,y=Lux,colour= Reef.area)) +
  geom_line() + scale_x_datetime(date_breaks = "12 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot Ligth and clouds
ggplot(ligthtempdata, aes(x = Datetimehr)) + geom_line(aes(y = Lux, colour = Reef.area))+ scale_x_datetime(date_breaks = "6 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ geom_line(aes(y =Cloud*100, colour = "Cloud cover"))+ scale_y_continuous(sec.axis = sec_axis(~./100, name = "Cloud cover [%]"))



