#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(tidyverse)
library(RColorBrewer)
library(plotly)
library(gt)


# Read in Data:
county_names <- read_csv("county_names.csv")
master_ranks <- read_csv("master_ranks.csv")
master_tidy2 <- read_csv("master_tidy2.csv")
master_tidy2$rank_name <- factor(master_tidy2$rank_name, c("diversity", "recreation", "nightlife", "entertainment", "health"))
values <- read_csv("values.csv")

#tab 2 data
tab_2<-read_sf(dsn = ".", layer = "tab_2") # Read data
st_crs(tab_2) = 4326 


# Create user interface (ui)
ui <- navbarPage(theme = shinytheme("darkly"),
  
  # Application title
  "Where 2 Next",
  
  # 3 panels all with separate inputs and outputs
  # Tab 1 - Summary 
  tabPanel("Summary",
           h1("Welcome to Where2Next", align = "center"),
           "We help you plan where in California you want to live based off factors that are important to you. Simply choose the attributes you want and see which counties support these on a California map. If you want to compare the metrics from two different counties, you can use our graph comparison feature. See how the two counties compare in their rankings and actual metrics.",
           br(),
           br(),
           img(src = "cityscape1.jpg",
               width = 500, height = 400), 
           align = "center",
           hr(h5(strong("Health"), align = "left"),
              p("Rank determined as a weighted sum of the following factors: 40% socioeconomic (education, employment, income, family and social support, and community safety), 30% health behaviors (tobacco use, diet and exercise, alcohol and drug use, and sexual activity), 20% clinical care (access to care and quality of care), and 10% physical environment (air and water quality, housing, and transit).", align = "left")),
           p(em("Data Source:"), a(href = "http://www.countyhealthrankings.org/rankings/data/CA", "California Health Rankings and Roadmaps."), "(2018). 2018 California Rankings Data [Data file and Summary Report].", align = "left"),
           
           hr(h5(strong("Recreation"), align = "left"),
          p("Number of recreation establishments per county normalized by the county’s entire area. This includes museums, zoos, amusement parks, and nature parks.", align = "left")),
        
           p(em("Data Source:"), a(href = "https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html", "United States Census Bureau"), "2016. County Business Patterns 2016 [Compete County data file].", align = "left"),
           
           hr(h5(strong("Nightlife"), align = "left"),
p("Number of nightlife establishments per county normalized by the county’s entire area. This includes bars, clubs, and liquor stores.
", align = "left")),
           
           p(em("Data Source:"), a(href = "https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html", "United States Census Bureau"), "2016. County Business Patterns 2016 [Compete County data file].", align = "left"),
           
           hr(h5(strong("Entertainment"), align = "left"), 
              p("Number of entertainment establishments per county normalized by the county’s entire area. This includes performing arts companies, theaters, and sports venues.", align = "left")),


  p(em("Data Source:"), a(href = "https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html", "United States Census Bureau"), "2016. County Business Patterns 2016 [Compete County data file].", align = "left"),
  
  hr(h5(strong("County Shapefiles"), align = "left")),
  
  p(em("Data Source:"), a(href = "https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html", "United States Census Bureau"), "2016. County Business Patterns 2016 [Compete County data file].", align = "left")

  ),
  
  # Tab 2 - Counties Results map based on selected inputs 
  tabPanel("Results Map",
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
           
           mainPanel(
             leafletOutput("map")
           )
  ),
           
          
  
  
  # Tab 3 - Graph and table comparing selected counties
  tabPanel("County Comparisions",
           fixedRow(
             column(12,
                    "",
                    fixedRow(
                      column(4,
                             selectInput("county1", "Select a County",
                                         choices = county_names$county_names,
                                         selected = "Alameda"),
                             selectInput("county2", "Select a Second County",
                                         choices = county_names$county_names,
                                         selected = "Amador"),
                             h3("Variable Comparison"),
                             h5(strong("Health")), 
                             ("Rank calculated from socioeconomic factors, behaviors, access to healthcare and physical environment."),
                             br(),
                             h5(strong("Recreation")), 
                             ("Includes museums, zoos, amusement parks, and nature parks."),
                             br(),
                             h5(strong("Nightlife")),
                             ("Includes bars, clubs, and liquor stores."),
                             br(),
                             h5(strong("Entertainment")),
                             "Includes performing arts companies, theaters, and sports venues.",
                             br(),
                             h5(strong("Diversity"))
                      ),
                      column(8,
                             h1("County Metric Rankings"),
                             plotOutput("comparison_graph"),
                             br(),
                             br(),
                             h1("County Metric Comparison"),
                             gt_output("comparison_table")
                      )
                    )
             )
           )))


# Define server logic required to draw a histogram
server <- function(input, output) {
  library(dplyr)
  library(kableExtra)
  library(plotly)

  output$ca_map <- renderPlot({
    county_outline
  })
  # Data frame for ploting:
 
  output$comparison_graph <- renderPlot({
    master_tidy2 %>% 
      filter(county == input$county1 | county == input$county2) %>% 
      ggplot(aes(x = rank_name, y = rank, group = county)) +
      geom_point(aes(color = county)) +
      geom_line(aes(color = county)) +
      scale_color_manual(values=c('steelblue3','indianred3')) +
      ylim(0,60) +
      scale_y_continuous(trans = "reverse", breaks = seq(0,60, by = 10)) +
      scale_x_discrete(expand = c(0.01,0.01)) +
      labs(
        x = "County Metrics",
        y = "Rank"
        ) +
      theme_bw() 
    })
  
  output$comparison_table <- render_gt({
    values %>% 
      filter(county == input$county1 | county == input$county2 | county == "CA Average") %>% 
      gt() %>% 
      cols_label(county = md("**County**"),
                 div_index = md("**Diversity**"),
                 rec_area = md("**Recreation**"),
                 night_area = md("**Nightlife**"),
                 ent_area = md("**Entertainment**"),
                 health_rank = md("**Health Rank**")) %>% 
      tab_spanner(label = md("Number of Establishments per Area"), 
                  columns = vars(div_index, rec_area, night_area, ent_area)) %>% 
      fmt_number(columns = vars(div_index, rec_area, night_area, ent_area),
                 decimals = 3) %>% 
      tab_style(
        style = cells_styles(
          text_style = "italic",
          text_color = "midnightblue"),
        locations = list(
          cells_data(columns = c(1), rows = c(3)),
          cells_data(columns = c(2), rows = c(3)),
          cells_data(columns = c(3), rows = c(3)),
          cells_data(columns = c(4), rows = c(3)),
          cells_data(columns = c(5), rows = c(3)),
          cells_data(columns = c(6), rows = c(3))
        )
      )
  })
  
  
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

