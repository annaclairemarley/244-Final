#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(shinythemes)
library(tmap)
library(leaflet)

ca_tidy<-read_sf(dsn = ".", layer = "county_tidy") # Read data
st_crs(ca_tidy) = 4326 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Where 2 Next?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
     sidebarPanel(
       checkboxGroupInput("attributes",
                          label = h3("Things I Look For:"),
                          c("Diversity" = "div", 
                            "Recreation" = "rec", 
                            "Entertainment" = "ent", 
                            "Coastline" = "coast"),
                          selected = "div")
       
       
     ),
     
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("mymap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

  
  
  output$mymap <- renderLeaflet({
    
      select_demo <- ca_tidy %>% 
        filter(attribute %in% input$attributes) 

    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data=select_demo, 
                 color = "red") %>% 
      addPolylines(data=ca_tidy$geometry,
                   weight = 2,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7
                   )  
      
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

