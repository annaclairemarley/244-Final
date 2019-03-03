library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(RColorBrewer)


tab_2<-read_sf(dsn = ".", layer = "tab_2") # Read data
st_crs(tab_2) = 4326 

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Where2Next"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        #Mountains or Beach
        radioButtons("geo", label = h3("Beach or Slopes"),
                     choices = list("Coast" = "coast", "Mountains" = "mountains", "Don't Care" = "na"), 
                     selected = "na"),
        
        #Urban or Rural
        radioButtons("metro", label = h3("Big City or Small Town"),
                     choices = list("City" = "city", "Rural" = "rural", "Don't Care" = "na"), 
                     selected = "na"),
        
        #Number of Bedrooms
        checkboxGroupInput("bedrm", label = h3("How Many Bed Rooms?"), 
                           choices = list("Studio" = "studio", "One Bed Room" = "one_bed", "Two Bed Room" = "two_bed", "Three Bed Room" = "three_bed"),
                           selected = "studio"),
        
        #Max Rent Budget
        sliderInput("rent", label = h3("Max Rent Budget"), min = 0, 
                    max = 4000, value = 1000)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$map <- renderLeaflet({
    
    dat <- tab_2
    
    #filter for coast/mountain
    if(input$geo != "na"){
      dat <- dat[which(dat$geo == input$geo),]
      
    }
    #filter for metro classification
    if(input$metro != "na"){
      dat <- dat[which(dat$metro == input$metro),]
      
    }
    #filter for bedrooms and rent max
    dat_rent<-dat %>%
      filter(bedroom %in% input$bedrm) %>% 
      filter(cost <= input$rent)
    
    #bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, Inf)
    bins<-seq(from = 700, to = 3000, by = 250)
    pal <- colorBin("Spectral", domain = tab_2$cost, bins = bins)
    
    #create map
    leaflet() %>% 
      addTiles() %>%
      fitBounds(lng1=-122, lat1=42, lng2=-117, lat2=32) %>% 
      addPolygons(data = dat_rent,
                  label = ~county,
                  fillColor = ~pal(men_rnt),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7) %>%
      addLegend(data = dat_rent,pal = pal, values = ~men_rnt, opacity = 0.7, title = "Average Rent $",
                position = "bottomright") %>% 
      addPolylines(data=tab_2,
                   weight = 1,
                   opacity = 1,
                   color = "white",
                   dashArray = "3",
                   fillOpacity = 0.7
                   
      ) 
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

