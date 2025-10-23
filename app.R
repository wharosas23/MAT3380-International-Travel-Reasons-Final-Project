#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

### Introduction to Data Science
### Fall 2024
### Final Project -- Wendy Arenas Rosas

library(shiny)
library(tidyverse)
library(plotly)
library("rnaturalearth")
library("rnaturalearthdata")
# library(rnaturalearthhires)
international_trips <- read_csv("international-trips-for-personal-reasons.csv")
arrivals_reasons <- read_csv("international-arrivals-for-personal-vs-business-and-professional-reasons.csv")

### ================ Setting up and making the choropleth map=============================

# Creating a simple feature dataset with the polygon data for the boundaries 
# of all countries in the world
world <- ne_countries(scale = "medium", returnclass = "sf")

# Gets the map theme set up
my_map_theme<-function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}

# Renaming some of the variables from my data. In this case I am changing "Entity" to "Country"
# and "International tourists visiting for personal purposes" to "N_tourists"
names(international_trips) <- c("Country",
                                "Code",
                                "Year",
                                "N_tourists")

# Setting up specific cutpoints, colors, and labels that I'm going to use for my map
my_cutpoints <- c(0, 500000, 1000000, 5000000, 10000000)
my_labels <- c("0","500,000", "1 million", "5 million", "10 million")
my_colors <- c("#eff3ff", "#BCD2EE", "#6baed6", "#3182bd", "#27408B")

# Now making a function that will create the map for one of my data sets
travel_map <- function(yr){
  # Filtering to find the amount of international trips given yr as a user input
  # for a specific year
  personal_trips <- international_trips |>
    filter(Year == yr)
  
  # In order to join the two datas, I used "iso_a3_eh" from the world data, and "Code"
  # from the personal_trips data to match up
  world_IT <- world |>
    left_join(personal_trips, by = c("iso_a3_eh" = "Code"))
  
  # Making the map and adding interactivity
  temp_map <- world_IT |>
    mutate(text = paste("<b>",admin,"</b>\n", Year, "\n Arrivals: ", N_tourists))
  
  q <- ggplot(temp_map) +
    geom_sf(aes(fill =N_tourists, text = text), color="black") +
    my_map_theme() +
    scale_fill_stepsn(
      breaks = my_cutpoints,
      colors = my_colors,
      labels = my_labels,
      limits = c(min(my_cutpoints), max(my_cutpoints)),
      values = scales::rescale(my_cutpoints),
      guide = "none") +
    labs(fill = "") +
    theme(legend.position = "bottom") +
    theme(legend.text.position = "top")
  
  ggplotly(q, tooltip = "text")|>
    style(hoveron = "fills")
}

### ================ Setting up and making the choropleth map=============================

# Renaming some of the variables from the data
names(arrivals_reasons) <- c("country",
                             "abbr",
                             "year",
                             "IT_personal_visit",
                             "IT_professional_visit",
                             "region")

# Making a new data that only has the regions if both IT_personal_visit and IT_professional_visit don't have any values
# Note, I took the variable year out because it only had 2023 recorded which was unecessary to include
world_regions <- arrivals_reasons |>
  filter(is.na(IT_personal_visit) & is.na(IT_professional_visit))|>
  select(-year, -IT_personal_visit, -IT_professional_visit)

# Making another data set that contains both IT_personal_visit and IT_professional_visit, but not the country's region
international_visits <- arrivals_reasons |>
  filter(!is.na(IT_personal_visit) & !is.na(IT_professional_visit))|>
  select(-region)

# In internal_visits, there were some countries that didn't have anything in the 
# abbr column, so since it was a small amount, I just excluded them. For now, to 
# make it easy to join both international_visits and word_regions, I am joining 
# them by the variable country
international_visits <- international_visits|>
  select(-abbr) |>
  left_join(world_regions, by = c("country" = "country"))

# Now making a function that will display the plot
visiting_map <- function(yr){
  # Filtering based on the year input of the user
  temp <- international_visits|>
    filter(year == yr)
  
  # Making the plot and adding interactivity
  p <- ggplot(temp, aes(x = IT_professional_visit, y = IT_personal_visit,
                        text = paste("<b>",country,"</b>\n",
                                     year, 
                                     "\nTrips for business and professional reasons\n<b>",
                                     IT_professional_visit,"</b>\nTourists visiting for personal purposes\n<b>", 
                                     IT_personal_visit,"</b>")))+
    geom_point(aes(color = region)) +
    geom_abline(color = "gray", linetype = "dashed") +
    scale_x_log10(breaks = c(0, 100, 1000, 10000, 100000, 1000000, 10000000),
                  labels = c("0", "100", "1,000", "10,000", "100,000", "1 million", "10 million"),
                  minor_breaks = NULL) +
    scale_y_log10(breaks = c(0, 100, 1000, 10000, 100000, 1000000, 10000000),
                  labels = c("0", "100", "1,000", "10,000", "100,000", "1 million", "10 million"),
                  minor_breaks = NULL) +
    labs(color = "") +
    xlab("International trips for business and professional reasons") +
    ylab("International tourists visiting \nfor personal purposes") +
    theme(panel.background = element_rect(fill = "white")) +
    theme(panel.grid = element_line(color = "gray", linewidth = 0.1))
  
  ggplotly(p, tooltip = "text")
}

# ==================== Displaying the graphs with ShinyApp =========================================
# Define UI for application for map or plot
ui <- fluidPage(
  
  # Application title
  titlePanel("International Travel Reasons"),
  
  # Creating the tabs
  tabsetPanel(id = "tab",
              tabPanel(title = "Personal trips", 
                       value = "Personal",
                       sidebarPanel(
                         sliderInput("yr_p",
                                     "Select a year:",
                                     min = 1995,
                                     max = 2022,
                                     value = 2018,
                                     sep = "")
                       ),
                       mainPanel(
                         h3(textOutput("TitleTextMap")),
                         h5("Visits for personal reasons include holidays, leisure, and recreation; visiting friends and\n
                         relatives; education and training; health and medical care; religion/pilgrimages;\n
                         shopping; transit; and other miscellaneous reasons."),
                         plotlyOutput("map"),
                         img(src = "temp_map_legend.png", height="100%", width="100%", align = "center"),
                         h5("Graph made by Wendy Arenas Rosas"),
                         h5("Data Source:", tags$a(
                           href = "https://ourworldindata.org/grapher/international-trips-for-personal-reasons?time=2018",
                           "Our World In Data Original Map - International trips for personal reasons"))
                       )
              ),
              tabPanel(title = "Personal vs Business trips", value = "Business", 
                       sidebarPanel(
                         sliderInput("yr_b",
                                     "Select a year:",
                                     min = 1995,
                                     max = 2022,
                                     value = 2018,
                                     sep = "")
                       ),
                       mainPanel(
                         h3(textOutput("TitleTextPlot")),
                         h5("Visits for personal reasons include holidays, leisure, and recreation; visiting friends and relatives;\n
                         education and training; health and medical care; religion/pilgrimages; shopping; transit; and other\n
miscellaneous reasons."),
                         plotlyOutput("graph"),
                         h5("Graph made by Wendy Arenas Rosas"),
                         h5("Data Source:", tags$a(
                           href = "https://ourworldindata.org/grapher/international-arrivals-for-personal-vs-business-and-professional-reasons", 
                           "Our World In Data Original Map - International trips for personal vs. business and professional reasons"))
                       )
              )
              
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Server logic for the map graph
  output$TitleTextMap<-renderText(paste("International trips for personal reasons, ", input$yr_p))
  
  output$map <- renderPlotly({
    travel_map(input$yr_p)
  })
  
  # Server logic for the plot graph
  output$TitleTextPlot<-renderText(paste("International trips for personal vs. business and
professional reasons, ", input$yr_b))
  
  output$graph <- renderPlotly({
    visiting_map(input$yr_b)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
