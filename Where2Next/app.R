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


# Read in Data:
county_names <- read_csv("county_names.csv")
master_ranks <- read_csv("master_ranks.csv")
values <- read_csv("values.csv")


# Create user interface (ui)
ui <- navbarPage(theme = shinytheme("darkly"),
  
  # Application title
  "Where 2 Next",
  
  # 3 panels all with separate inputs and outputs
  # Tab 1 - Summary 
  tabPanel("Summary",
           h1("Data Summary", align = "center"),
           img(src = "cityscape1.jpg",
               width = 500, height = 400), 
           align = "center",
           p("Explain/summarize the data here")),
  
  # Tab 2 - Counties Results map based on selected inputs 
  tabPanel("Results Map",
           sidebarPanel(
             radioButtons("location",
                          "Do You Prefer:",
                          choices = c("Ocean",
                                      "Mountains",
                                      "No Preference")),
             radioButtons("type",
                          "Do You Prefer:",
                          choices = c("Urban",
                                      "Rural",
                                      "No Preference")),
             sliderInput("rent",
                         "What's Your Rental Budget?",
                         min = 500,
                         max = 3000,
                         value =  c(1000,2000),
                         step = 500)
            
             ),
           
           mainPanel(
             plotOutput("ca_map")
           )
           
           ),
  
  
  # Tab 3 - Graph and table comparing selected counties
  tabPanel("County Comparisions",
           sidebarPanel(
             selectInput("county1", "Select a County",
                         choices = county_names$county_names,
                         selected = 1),
             selectInput("county2", "Select a Second County",
                         choices = county_names$county_names,
                         selected = 1)),
             "Variable Comparison", align = "left",
           mainPanel(
             h1("County Metric Rankings"),
             plotlyOutput("comparison_graph"),
             tableOutput("comparison_table")
           ))
           
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  library(dplyr)
  library(kableExtra)
  library(plotly)
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
  
  output$ca_map <- renderPlot({
    county_outline
  })
  # Data frame for ploting:
  observeEvent(input$county2, {
    plot_df <- master_ranks %>% 
      filter(county == input$county1 | county == input$county2)
    
    output$comparison_graph <- renderPlotly({
      plot_ly(plot_df, type = 'parcoords',
              line = list(color = ~county), 
              dimensions = list(
                list(range = c(58,1),
                     label = 'Diversity', values = ~div_rank),
                list(range = c(58,1),
                     label = 'Recreation', values = ~rec_rank),
                list(range = c(58,1),
                     constraintrange = c(5,6),
                     label = 'Night life', values = ~night_rank),
                list(range = c(58,1),
                     label = 'Entertainment', values = ~ent_rank),
                list(range = c(58,1),
                     label = 'Health', values = ~health_rank)
                
              ))
  }
    
  
      )
  })
  
  output$comparison_table <- function() {
    req(input$county1, input$county2)
    values %>% 
      filter(county == input$county1 | county == input$county2) %>% 
      knitr::kable("html", col.names = c("County", "Diversity", "Recreation",
                                         "Nightlife", "Entertainment", "Health Rank"),
                   align = "c", digits = 3) %>% 
      kable_styling(bootstrap_options = c("striped", "hovered", "boardered"), full_width = F) %>% 
      column_spec(1, bold = T) %>% 
      add_header_above(c(" " = 1, "Number of Establishments per Area" = 4, " " = 1))
  }
}

# Run the application 
shinyApp(ui = ui, server = server)

