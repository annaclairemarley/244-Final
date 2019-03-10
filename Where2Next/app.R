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
             gt_output("comparison_table")
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
        plot_df2 <- master_tidy2 %>% 
          filter(county == input$county1 | county == input$county2)
        
        output$comparison_graph <- renderPlot({
          ggplot(data = plot_df2, aes(x = rank_name, y = rank, group = county)) +
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
    
  
      
  })
  
  output$comparison_table <- render_gt({
    values %>% 
      filter(county == input$county1 | county == input$county2 | county == "CA Average") %>% 
      gt() %>% 
      tab_header(
        title = html("County Metrics Comparison")) %>% 
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
}

# Run the application 
shinyApp(ui = ui, server = server)

