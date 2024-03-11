
#DATA----------
## data sources
Data <- "Data"

#Compare RLS (Reef Life Survey) data with data from this thesis for Golfo Nuevo----- 

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
RLS_GN_Fish <- read.csv(file.path(Data,"Reef_Life_Survey_(RLS)#_Cryptobenthic_fish_abundance_RLS.csv"))
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


#Data Thesis Bravo et al 2020-----
Density.data.Bravo <- read.csv(file.path(Data,"Density_Data_BRAVO.csv"))
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

#write.csv(Density.Data.comparison, 'Density_Data_BRAVO_RLS.csv',row.names = F)


library(doBy)
densitybyREEF.RLS <- summaryBy(Density  ~ Taxon + reef.name,   data =RLS_GN, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })

library(doBy)
densitybyREEF.Bravo <- summaryBy(Density  ~ Taxon +reef.name,   data =Density.data.Bravo, FUN = function(x) { c(mean = mean(x), SD=sd(x),SE = sqrt(var(x)/length(x))) })

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# Water Temperature DATA-------

#Data of temperature on all sites
tempdata <- read.csv(file.path(Data,"Water_Temperature_Data_HOBOLOGGERS.csv"))

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
#Plot USh, comodroro and Golfo Nuevo---- 

tempdata_USH_PC_COMODORO <- subset(tempdata,SITE=="USH"|SITE=="PC"|SITE=="Comodoro")
# temperature every 1 hr, calculate mena temperature per day

library(doBy)
temperaturedaily_USH_PC_COMODORO <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata_USH_PC_COMODORO ,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })


# covert date to Date class
temperaturedaily_USH_PC_COMODORO$Datetime<- as.Date(temperaturedaily_USH_PC_COMODORO$Datetime,"%d/%m/%Y")

library(ggplot2)
ggplot(temperaturedaily_USH_PC_COMODORO, aes(x = Datetime, y =Temp..mean)) + 
  geom_line(aes(color = SITE), size = 0.5) + labs(x="Fecha", y="Temperatura(°C)") + ggtitle("Temperatura del agua a 5 m de profundidad")+
  theme_minimal()

library(dplyr)
temperaturedaily_USH_PC_COMODORO <- temperaturedaily_USH_PC_COMODORO %>%
  mutate(SITE = recode(SITE,
                       "Comodoro" = "Comodoro Rivadavia",
                       "PC" = "Puerto Madryn",
                       "USH" = "Ushuaia"))

ggplot(temperaturedaily_USH_PC_COMODORO,aes(x =Datetime,y =Temp..mean,colour= SITE)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")+scale_y_continuous(breaks = seq(4, 20, by = 1))+theme_minimal()+ ggtitle("Temperatura del agua a 5 m de profundidad")+ labs(x="Fecha", y="Temperatura(°C)")

ggplot(temperaturedaily_USH_PC_COMODORO, aes(x = Datetime, y = Temp..mean, colour = SITE)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2019-02-05"), linetype = "dashed", color = "red", size = 0.5, alpha = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", lim = as.Date(c("2018-07-01", "2019-06-30"))) +
  scale_y_continuous(breaks = seq(4, 20, by = 1)) +
  theme_minimal() +
  ggtitle("Temperatura del agua a 5 m de profundidad") +
  labs(x = "Fecha", y = "Temperatura (°C)")


pp <- ggplot(temperaturedaily_USH_PC_COMODORO,aes(x =Datetime,y =Temp..mean,colour= SITE)) +
  geom_line() + scale_x_date(date_breaks = "1 month", date_labels = "%b")+theme_minimal()+ ggtitle("Temperatura del agua a 5 m de profundidad")+ labs(x="Fecha", y="Temperatura(°C)")

library(plotly)
ggplotly(pp) %>% plotly::config(displayModeBar = F) 


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Air temperature data-----
airtempdata <- read.csv(file.path(Data,"Air_Temperature_Data_METEOBLUE.csv"))
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


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Temperature Punta Cuevas vs Pardelas------
tempdata_PuntaCuevas_Pardelas <- subset(tempdata,SITE=="Pcostero"|SITE=="PC")

tempdata_PuntaCuevas_Pardelas <- tempdata_PuntaCuevas_Pardelas %>%
  mutate(SITE = recode(SITE,
                       "Pcostero" = "Punta Pardelas (Puerto Pirámides)",
                       "PC" = "Punta Cuevas (Puerto Madryn)"))

# covert date to Date class
tempdata_PuntaCuevas_Pardelas$datetransform<- as.Date(tempdata_PuntaCuevas_Pardelas$Datetime,"%d/%m/%Y")
ggplot(tempdata_PuntaCuevas_Pardelas, aes(x=datetransform,y=Temp., colour=SITE)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=SITE), alpha=0.3) +
  theme_bw()


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#Temperature data 3 reefs----
tempdata_3reefs <- subset(tempdata,SITE=="Pcostero"|SITE=="Alero20m"|SITE=="PardelasMedio")

# temperature every 1 hr, calculate mean temperature per day
library(doBy)
temperaturedaily_3reefs  <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata_3reefs ,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

temperaturedailymaxmin_3reefs <- summaryBy(Temp. ~ SITE + Datetime ,  data =tempdata_3reefs ,FUN = function(x) { c(max=max(x), min=min(x),diference=max(x)-min(x))})

temperaturedailymaxmin2 <- summaryBy(Temp..max + Temp..min ~ SITE ,  data =temperaturedailymaxmin_3reefs ,FUN = function(x) { c(max=max(x), min=min(x))})

temperaturedailydepth <- summaryBy(Temp..mean ~ SITE,  data =temperaturedaily_3reefs,FUN = function(x) { c(mean = mean(x), Range=range(x),diference=max(x)-min(x),SD=sd(x),SE = sqrt(var(x)/length(x))) })

# covert date to Date class
temperaturedaily_3reefs$Datetime<- as.Date(temperaturedaily_3reefs$Datetime,"%d/%m/%Y")

#plot
library(ggplot2)
temperatureplot1 <- ggplot(temperaturedaily_3reefs, aes(x = Datetime, y =Temp..mean)) + geom_line(aes(color = SITE), size = 0.5) + labs(x="Month", y="Temperature (°C)", fill="Rocky reefs") + ggtitle("Water Sea temperature")+ 
  theme_bw() + theme(text = element_text(size=20))+scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", lim = as.Date(c("2019-03-01", "2019-09-30")))+ scale_colour_discrete(name="Rocky reefs",breaks=c("Alero20m", "PardelasMedio", "Pcostero"),labels=c("25 m", "15 m", "5 m"))

#ggsave(filename = "temperatureplot1.png",plot = temperatureplot1,width=12,height = 8)

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#LUX and cloud data----

ligthtempdata_pardelas <- read.csv(file.path(Data,"Ligth_Temperature_Cloud_Data_HOBOLOGGERS.csv"))

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
ggplot(ligthtempdata_pardelas, aes(x = Datetimehr)) + geom_line(aes(y = Lux, colour = Site))+ scale_x_datetime(date_breaks = "12 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position = "top")+ geom_line(aes(y =Cloud*100, colour = "Cloud cover"))+ scale_y_continuous(sec.axis = sec_axis(~./100, name = "Cobertura de Nubes [%]"))+ scale_color_discrete(name = "", labels = c("Cobertura de Nubes (Modelo MeteoBlue)", "Arrecife profundidad de 16-25 m", "Arrecife profundidad de 8-15 m"))+labs(x = "Fecha y Hora", y = "Intensidad de Luz (Lux)")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ligthtempdata_inclination <- read.csv(file.path(Data,"Ligth_ReefInclination_Data_HOBOLOGGERS.csv"))

#Descriptive Statistics
library(doBy)
summaryBy(Temp + Lux  ~ Site, data = ligthtempdata_inclination, 
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

# covert date to POSIXct class
library(lubridate)
ligthtempdata_inclination$Datetimehr <- lubridate::mdy_hms(ligthtempdata_inclination$Date.Time..GMT.03.00)

ligthtempdata_inclination_cavefloor <- subset(ligthtempdata_inclination,Reef.area!="cavefloor")

library(ggplot2)
ggplot(ligthtempdata_inclination,aes(x=Datetimehr,y=Lux,colour= Reef.area)) +
  geom_line() + scale_x_datetime(date_breaks = "12 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plot Ligth and clouds
ggplot(ligthtempdata_inclination, aes(x = Datetimehr)) + geom_line(aes(y = Lux, colour = Reef.area))+ scale_x_datetime(date_breaks = "6 hour") +theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+ geom_line(aes(y =Cloud*100, colour = "Cloud cover"))+ scale_y_continuous(sec.axis = sec_axis(~./100, name = "Cloud cover [%]"))


# Water Velocity Data------
require(ggplot2)
require(RColorBrewer)
require(scales)

#Read data from tld current (CORMORANES)
currentDATA_corm<- read.csv(file.path(Data,"Water_Velocity_Data_Cormoranes_TILTCURRENTMETER.csv"))
currentDATA_corm$Velocity.N..m.s. <- currentDATA_corm$Velocity.N..cm.s./100
currentDATA_corm$Velocity.E..m.s. <- currentDATA_corm$Velocity.E..cm.s./100

temperatureDATA_corm <- read.csv(file.path(Data,"Water_Temperature_Data_Cormoranes_TILTCURRENTMETER.csv"))


#Transform time format 
currentDATA_corm$Datetime <- strptime(currentDATA_corm$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")
temperatureDATA_corm$Datetime <- strptime(temperatureDATA_corm$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")

#Merge current and temperature data
library(dplyr)
TILD.DATA_corm <- left_join(currentDATA_corm,temperatureDATA_corm, by="Datetime")

library(lubridate)
#Start 2021-08-08 14:52:17 End 2021-10-27 10:42:17
start <- as.POSIXct("2021-08-08 14:52:17")
end <- as.POSIXct("2021-10-27 10:42:17")
end-start #get time of deployment
int <- interval(start, end)

#Cut the track within the time interval
TILD.DATA_corm<- TILD.DATA_corm[TILD.DATA_corm$Datetime %within% int,]

#Read data from tld current (PUNTA ESTE)
currentDATA_pest<- read.csv(file.path(Data,"Water_Velocity_Data_PuntaEste_TILTCURRENTMETER.csv"))
currentDATA_pest$Velocity.N..m.s. <- currentDATA_pest$Velocity.N..cm.s./100
currentDATA_pest$Velocity.E..m.s. <- currentDATA_pest$Velocity.E..cm.s./100
temperatureDATA_pest <- read.csv(file.path(Data,"Water_Temperature_Data_PuntaEste_TILTCURRENTMETER.csv"))

#Transform time format 
currentDATA_pest$Datetime <- strptime(currentDATA_pest$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")
temperatureDATA_pest$Datetime <- strptime(temperatureDATA_pest$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")

#Merge current and temperature data
library(dplyr)
TILD.DATA_pest <- left_join(currentDATA_pest,temperatureDATA_pest, by="Datetime")

library(lubridate)
#Start 2021-10-15 11:36:09 End 2021-10-28 13:16:09
start <- as.POSIXct("2021-10-15 11:36:09")
end <- as.POSIXct("2021-10-28 13:16:09")
end-start #get time of deployment
int <- interval(start, end)

#Cut the track within the time interval
TILD.DATA_pest<- TILD.DATA_pest[TILD.DATA_pest$Datetime %within% int,]

plot.currentroses <- function(data,
                              spd,
                              dir,
                              spdres = 2,
                              dirres = 22.5,
                              spdmin = 2,
                              spdmax = 20,
                              spdseq = NULL,
                              palette = "YlGnBu",
                              countmax = NA,
                              debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")
    
  }  
  
  # create the plot
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned
                           ,y = (..count..)/sum(..count..)
                       ))+
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Speed (cm/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) + 
    scale_y_continuous(labels = percent) +
    ylab("Freq")+xlab("")
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}

p1 <- plot.currentroses(spd = TILD.DATA_corm$Speed..cm.s.,
                        dir = TILD.DATA_corm$Heading..degrees.,spdseq = c(0,10,20,30))+ theme_bw() 
Plot <- p1 + theme_bw()+ggtitle("Punta Cormoranes")

Plot1 <- p1 + theme_bw() +ggtitle("Punta Cormoranes") + theme(legend.position="none",axis.title.x=element_blank())

p2 <- plot.currentroses(spd = TILD.DATA_pest$Speed..cm.s.,
                        dir = TILD.DATA_pest$Heading..degrees.,spdseq = c(0,10,20,30))+ theme_bw() 

Plot2 <- p2 + theme_bw()+ggtitle("Punta Este") + theme(legend.position="none",axis.title.x=element_blank())


library(cowplot)
legend <- cowplot::get_legend(Plot)
plot_grid(Plot1,Plot2,legend,ncol = 3, align = "hv")


library(oce)
currentcm_corm <- as.cm(
  TILD.DATA_corm$Datetime,
  u = TILD.DATA_corm$Velocity.E..m.s.,
  v = TILD.DATA_corm$Velocity.N..m.s.,
  pressure = NULL,
  conductivity = NULL,
  temperature = TILD.DATA_corm$Temperature..C.,
  salinity = NULL,
  longitude = NA,
  latitude = NA,
  filename = "",
  debug = getOption("oceDebug")
)

summary(currentcm_corm)
plot(currentcm_corm,which=6)

currentcm_pest <- as.cm(
  TILD.DATA_pest$Datetime,
  u = TILD.DATA_pest$Velocity.E..m.s.,
  v = TILD.DATA_pest$Velocity.N..m.s.,
  pressure = NULL,
  conductivity = NULL,
  temperature = TILD.DATA_pest$Temperature..C.,
  salinity = NULL,
  longitude = NA,
  latitude = NA,
  filename = "",
  debug = getOption("oceDebug")
)

summary(currentcm_pest)
plot(currentcm_pest,which=1)#u
plot(currentcm_pest,which=2)#V
plot(currentcm_pest,which=6)#uv+ellipse+arrow
plot(currentcm_pest,which=9)#temperature

