#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Air Quality During the Thomas Fires"),
  
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      
     sidebarPanel(
       verbatimTextOutput("details")
     ),
     
     
     
     
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(id="tabs",
          tabPanel("Goleta", plotOutput("GoletaPlot", click="plot_click")), 
          tabPanel("Santa Barbara", plotOutput("SBPlot", click="plot_click")),
          tabPanel("Santa Maria", plotOutput("MariaPlot", click="plot_click"))
        )
        
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

     
  output$GoletaPlot <- renderPlot({
      source("AQI.R")
    make_plot("Goleta")
  })
  
  output$SBPlot <- renderPlot({
    source("AQI.R")
    make_plot("Santa Barbara")
  })
  
  output$MariaPlot <- renderPlot({
    source("AQI.R")
    make_plot("Santa Maria")
  })
  
  
  output$details <- renderText({
    if(is.null(input$plot_click$x)){
      paste("N/A")
    }
    else{
    
      city<-input$tabs
      data <- get_data(city)
      
      datetime <- as.POSIXct(input$plot_click$x, origin = "1970-01-01")
      nearest_row <- data[which.min(abs(data$Date.Time - datetime)),]
      
      parse_hour <- function(hour){
        if(hour < 12){
          if(hour == 0){
            return("12AM")
          }
          return(paste(hour, "AM", sep=""))
        }
        if(hour == 12){
          return("12PM")
        }
        return(paste(hour-12, "PM", sep=""))
      }
      
      date_to_string <- function(date){
        paste(month(date), "/" ,mday(date) ," ", parse_hour(hour(date)), sep="")
      }
      
      
      
      paste("AQI:", round(nearest_row[,8]),
            "\nDate:",date_to_string(nearest_row[,1]), 
            "\n", 
            "\nPM10:", nearest_row[,3], 
            "\nPM2.5:", nearest_row[,4])
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

