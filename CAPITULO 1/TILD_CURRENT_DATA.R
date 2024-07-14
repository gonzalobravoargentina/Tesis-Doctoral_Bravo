# Water Velocity Data Doradillo------
require(ggplot2)
require(RColorBrewer)
require(scales)

Data <- "Data"


#Read data from tld current
currentDATA <- read.table("1912401_Dora_abril24_(0)_CR_burst_avg.txt", header=TRUE, sep=",")

#transfor to m/s
currentDATA$Velocity.N..m.s. <- currentDATA$Velocity.N..cm.s./100
currentDATA$Velocity.E..m.s. <- currentDATA$Velocity.E..cm.s./100

temperatureDATA <- read.table("1912401_Dora_abril24_(0)_T.txt", header=TRUE, sep=",")


#Transform time format for current data 
datetime_str <- paste(currentDATA$Date, currentDATA$Time)
datetime_posixlt <- strptime(datetime_str, format="%Y-%m-%d %H:%M:%S")
currentDATA$Datetime <- datetime_posixlt


#Transform time format for temperature data
temperatureDATA$Datetime <- strptime(temperatureDATA$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")


#Merge current and temperature data
library(dplyr)
TILD.DATA <- left_join(currentDATA,temperatureDATA, by="Datetime")


library(lubridate)
start <- as.POSIXct("2024-04-01 15:00:00")
end <- as.POSIXct("2024-05-01 15:00:00")
end-start #get time of deployment
int <- interval(start, end)

#Cut the track within the time interval
TILD.DATA<- TILD.DATA[TILD.DATA$Datetime %within% int,]



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
                           fill = spd.binned,
                           y = (..count..)/sum(..count..)
                       )) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Velocidad (cm/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) + 
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1), labels = percent) +
    ylab("Frecuencia")+xlab("")
  
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

p1 <- plot.currentroses(spd = TILD.DATA$Speed..cm.s.,
                        dir = TILD.DATA$Bearing..degrees.,spdseq = c(0,10,20,30))+ theme_bw() 
Plot <- p1 + theme_bw()+ggtitle("Doradillo")


library(oce)
currentcm <- as.cm(
  TILD.DATA$Datetime,
  u = TILD.DATA$Velocity.N..m.s.,
  v = TILD.DATA$Velocity.E..m.s.,
  pressure = NULL,
  conductivity = NULL,
  temperature = TILD.DATA$Temperature..C.,
  salinity = NULL,
  longitude = NA,
  latitude = NA,
  filename = "",
  debug = getOption("oceDebug")
)

summary(currentcm)
plot(currentcm,which=6,ylim(-0.4,0.4))


plot(currentcm,which=1)#u
plot(currentcm,which=2)#V
plot(currentcm,which=6,yaxs="i",ylim = c(-0.4, 0.4))#uv+ellipse+arrow
plot(currentcm,which=9)#temperature



# Water Velocity Data Cracker------
require(ggplot2)
require(RColorBrewer)
require(scales)

#Read data from tld current 
currentDATA_corm<- read.csv("1912402_Cracker_abril24_(0)_Current.csv")
currentDATA_corm$Velocity.N..m.s. <- currentDATA_corm$Velocity.N..cm.s./100
currentDATA_corm$Velocity.E..m.s. <- currentDATA_corm$Velocity.E..cm.s./100

temperatureDATA_corm <- read.csv("1912402_Cracker_abril24_(0)_Temperature.csv")


#Transform time format 
currentDATA_corm$Datetime <- strptime(currentDATA_corm$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")
temperatureDATA_corm$Datetime <- strptime(temperatureDATA_corm$ISO.8601.Time,"%Y-%m-%dT%H:%M:%S")

#Merge current and temperature data
library(dplyr)
TILD.DATA_corm <- left_join(currentDATA_corm,temperatureDATA_corm, by="Datetime")

library(lubridate)
start <- as.POSIXct("2024-04-01 15:00:00")
end <- as.POSIXct("2024-05-26 01:50:00")
end-start #get time of deployment
int <- interval(start, end)

#Cut the track within the time interval
TILD.DATA_corm<- TILD.DATA_corm[TILD.DATA_corm$Datetime %within% int,]


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
                           fill = spd.binned,
                           y = (..count..)/sum(..count..)
                       )) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = c("N","NNE","NE","ENE", "E", 
                                "ESE", "SE","SSE", 
                                "S","SSW", "SW","WSW", "W", 
                                "WNW","NW","NNW")) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Velocidad (cm/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank()) + 
    scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 0.3, 0.1), labels = percent) +
    ylab("Frecuencia")+xlab("")
  
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
Plot <- p1 + theme_bw()+ggtitle("Cracker")



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
plot(currentcm_corm,which=1)#u
plot(currentcm_corm,which=2)#v
plot(currentcm_corm,which=9)#temperature

plot(currentcm_pest,which=9)#temperature







