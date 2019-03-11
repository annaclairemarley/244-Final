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
           hr(h5(strong("Health")),"Rank determined as a weighted sum of the following factors: 40% socioeconomic (education, employment, income, family and social support, and community safety), 30% health behaviors (tobacco use, diet and exercise, alcohol and drug use, and sexual activity), 20% clinical care (access to care and quality of care), and 10% physical environment (air and water quality, housing, and transit)."),
           
           br(),
           
           "Data Source: California Health Rankings and Roadmaps. (2018). 2018 California Rankings Data [Data file and Summary Report]. Retrieved from http://www.countyhealthrankings.org/rankings/data/CA.",
           
           hr(h5(strong("Recreation")),"Number of recreation establishments per county normalized by the county’s entire area. This includes museums, zoos, amusement parks, and nature parks."),
           
           br(),
          
            "Data Source: United States Census Bureau. 2016. County Business Patterns 2016 [Compete County data file]. Retrieved from https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html.",
           
           hr(h5(strong("Nightlife")),"Number of nightlife establishments per county normalized by the county’s entire area. This includes bars, clubs, and liquor stores.
"),
           br(),
           
           "Data Source: United States Census Bureau. 2016. County Business Patterns 2016 [Compete County data file]. Retrieved from https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html.",
           
           hr(h5(strong("Entertainment")),"Number of entertainment establishments per county normalized by the county’s entire area. This includes performing arts companies, theaters, and sports venues.")),
  
  br(),
  
  "Data Source: United States Census Bureau. 2016. County Business Patterns 2016 [Compete County data file]. Retrieved from https://www.census.gov/data/datasets/2016/econ/cbp/2016-cbp.html."
  
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
}

# Run the application 
shinyApp(ui = ui, server = server)

