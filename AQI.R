library('lubridate')
library(ggplot2)




#setwd('C:/Users/yashr/Documents/Random Projects/Air-Quality-Index')


get_data <- function(city){
  if(identical(city, "santa barbara")){
    data <- read.csv("./lib/SantaBarbaraAQI.csv")
  }
  else{
    data <- read.csv("./lib/AQI.csv")
  }
  time_format <- "%m/%d/%Y %I %p"
  
  data$Date.Time <- as.POSIXlt(data$Date.Time, format=time_format)
  
  aqi_calc <- function(i_high, i_low, c_high, c_low, c){
    return( (i_high - i_low) / (c_high - c_low) * (c - c_low) + i_low )
  }
  
  pm10_to_aqi <- function(pm10){
    if(is.na(pm10)){
      return(NA)
    }
    if(pm10 >= 425){
      return(aqi_calc(400,301, 504, 425, pm10))
    }
    else if(pm10 >= 355){
      return(aqi_calc(300,201, 424, 355, pm10))
    }
    else if(pm10 >= 255){
      return(aqi_calc(200,151, 354, 255, pm10))
    }
    else if(pm10 >= 155){
      return(aqi_calc(150,101, 254, 155, pm10))
    }
    else if(pm10 >= 55){
      return(aqi_calc(100,51, 154, 55, pm10))
    }
    return(aqi_calc(50,0,54,0,pm10))
  }
  
  pm25_to_aqi <- function(pm25){
    if(is.na(pm25)){
      return(NA)
    }
    if(pm25 >= 250){
      return(aqi_calc(400,301, 350, 250, pm25))
    }
    else if(pm25 >= 150){
      return(aqi_calc(300,201, 249, 150, pm25))
    }
    else if(pm25 >= 55){
      return(aqi_calc(200,151, 149, 55, pm25))
    }
    else if(pm25 >= 35){
      return(aqi_calc(150,101, 54, 35, pm25))
    }
    else if(pm25 >= 12){
      return(aqi_calc(100,51, 34, 12, pm25))
    }
    return(aqi_calc(50,0,11,0,pm25))
  }
  
  data$AQI <- (unlist(lapply(data$PM10.ug.m3., pm25_to_aqi)) + unlist(lapply(data$PM10.ug.m3., pm10_to_aqi))) / 2
  
  return(data)
}

make_plot <- function(city){

  data <- get_data(city)
  
  
  
  fire_plot <- ggplot(data=data, aes(x=Date.Time, y=AQI)) + 
    
    
    #labeled color bars
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=301, ymax=330,fill='Hazardous'), color=NA, alpha=0.01) +#hazardous
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=201, ymax=300,fill="Very Unhealthy"), color=NA, alpha=0.01) +#very unhealthy
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=151, ymax=200,fill="Unhealthy"), color=NA, alpha=0.01) +#unhealthy
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=101, ymax=150,fill="Unhealthy for Sensitive Groups"), color=NA, alpha=0.01) +#USG
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=51, ymax=100,fill="Moderate"), color=NA, alpha=0.01) +#Moderate
    geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=0, ymax=50,fill="Good"), color=NA, alpha=0.01) +#Good
    scale_fill_manual('Air Quality',
  #                    values = c('black', 'purple','red','#FF4500','yellow','green'),
                                #'green', 'black','yellow','red','orange','purple'
                      values = c('green', '#696969','yellow','red','#FFA500','#8A2BE2'),
                      guide = guide_legend(override.aes = list(alpha = 1),),
                      breaks = c("Hazardous", "Very Unhealthy", "Unhealthy", "Unhealthy for Sensitive Groups","Moderate", "Good")) +
    
    
    #the actual graph
    geom_line(na.rm = TRUE, size = 1)+
    theme_minimal(base_size = 14) +
    labs(title="Goleta", 
         x="Date", y="AQI")
  
  
  fire_start <- as.POSIXlt("2017-12-04 18:28")
  fire_start_label <- as.POSIXlt("2017-12-04 10:28")
  
  outage_start <- as.POSIXlt("2017-12-04 21:00")
  outage_end <- as.POSIXlt("2017-12-05 02:00")
  
  outage2_start <- as.POSIXlt("2017-12-05 08:00")
  outage2_end <- as.POSIXlt("2017-12-05 12:00")
  
  
  
  get_closest_row <- function(datetime){
    data[which.min(abs(data$Date.Time - datetime)),]
  }
  
  fire_plot <- fire_plot +
    annotate("text", x=fire_start_label, y=get_closest_row(fire_start)$AQI, label="Fire Start", color="red")+
    #annotate("segment", x=fire_start, xend = fire_start, y=90, yend=get_closest_row(fire_start)$AQI, color="red")+
  
    annotate("rect", xmin = outage_start, xmax = outage_end, ymin = 0, ymax = 145,color="red",alpha = .2)+
    annotate("rect", xmin = outage2_start, xmax = outage2_end, ymin = 0, ymax = 145,color="red",alpha = .2)+
    annotate("text", x=outage_end, y=160, label="Power Outages", color="white")  
  
  
  
  fire_plot
}


