library('lubridate')
library(ggplot2)


setwd('C:/Users/yashr/Documents/Random Projects/Air-Quality-Index/lib')
data <- read.csv('AQI.csv')




time_format <- "%m/%d/%Y %I %p"

data$Date.Time <- as.POSIXlt(data$Date.Time, format=time_format)


message_plot <- ggplot(data=data, aes(x=Date.Time, y=PM10.ug.m3.)) + 
  #labeled color bars
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=301, ymax=500,fill='Hazardous'), color=NA, alpha=0.005) +#hazardous
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=201, ymax=300,fill="Very Unhealthy"), color=NA, alpha=0.005) +#very unhealthy
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=151, ymax=200,fill="Unhealthy"), color=NA, alpha=0.005) +#unhealthy
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=101, ymax=150,fill="Unhealthy for Sensitive Groups"), color=NA, alpha=0.005) +#USG
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=51, ymax=100,fill="Moderate"), color=NA, alpha=0.005) +#Moderate
  geom_rect(aes(xmin=min(data$Date.Time), xmax=max(data$Date.Time), ymin=0, ymax=50,fill="Good"), color=NA, alpha=0.005) +#Good
  scale_fill_manual('Highlight this',
#                    values = c('black', 'purple','red','#FF4500','yellow','green'),
 #                   values = c('green', 'black','yellow','red','#FF4500','purple'),
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  
  #the actual graph
  geom_path(na.rm = TRUE)+
  theme_minimal()


message_plot
