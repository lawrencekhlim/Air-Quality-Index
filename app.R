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
         plotOutput("distPlot", hover=hoverOpts(id="plot_hover", delay=100, delayType = "debounce", nullOutside = FALSE ))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$distPlot <- renderPlot({
   # if(!exists("make_plot", mode = "function")){
      source("AQI.R")
   # }
    
    make_plot("goleta")
  })
  
  
  output$details <- renderText({
    if(is.null(input$plot_hover$x)){
      paste("N/A")
    }
    else{
    
      data <- get_data("goleta")
      datetime <- as.POSIXct(input$plot_hover$x, origin = "1970-01-01")
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
      
      
      
      paste("AQI:", nearest_row[,8],
            "\nDate:",date_to_string(nearest_row[,1]), 
            "\n", 
            "\nPM10:", nearest_row[,3], 
            "\nPM2.5:", nearest_row[,4])
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

