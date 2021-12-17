# Load packages and data
library(shiny)
library(shinydashboard)
library(pacman)
library(plotly)
library(readr)
library(tidyverse)

p_load(rio)
    #data loading does not work, load from 02_build_dashboard.R
KGIdata <- rio::import(file = "data_processed/KGI.Rdata")%>%
    mutate(hover = paste0(country, "\nKGI:", KGI))

# Define UI ----------------------------------------------------------------
header <- dashboardHeader(title = "How globalized is the world really?",
                          titleWidth = 400)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About", tabName = "model", icon = icon("book"),
                 textOutput("text")),
        menuItem("Controls", tabName = "model", icon = icon("mouse"),
                 sliderInput("year", "Select year", 1990, 2020, 1990, step = 1, sep = ""),
                 checkboxInput("small", "Include small countries? (pop. < 100.000)", value = FALSE)),
        menuItem("Link to code", href = "https://youtu.be/dQw4w9WgXcQ", icon = icon("code"))
        )
    )

body <- dashboardBody(
    fluidRow(
       column(3,
              wellPanel(
                  h4("Most globalized countries"), tableOutput("ranking"))),
       column(9,
              plotlyOutput("world_map"))
    )
)
    
ui <- dashboardPage(header, 
                    sidebar, 
                    body
)
# Old code
#fluidRow(
 #   column(3,
  #         wellPanel(
   #            sliderInput("year", "Year", 1990, 2020, 2020, step = , sep = "")),
    #       wellPanel(
     #          h4("Include small countries?"),
      #         checkboxInput("small", "Countries with population less than 100.000"), value = FALSE),
       #    wellPanel(
        #       h4("Top 10 globalized countries"), tableOutput("ranking"))
#    ),
 #   column(9,plotlyOutput("world_map")))

# Define Server -----------------------------------------------------------
server <- function(input, output, session) {
    
    #small_filter <- reactive(filter(KGIdata$small=TRUE))
    #year_filter <- reactive(filter(year==input$year))
    
    # Need to write correct text
    output$text <- renderText({ 
        "The Kessler Globality Index..." 
    })
    
    observeEvent(input$year,{
        updateSliderInput(session, "year")
        updateCheckboxInput(session, "small")
    })
    
    # Need to implement filters for year and small countries
    output$ranking <- renderTable({
        KGIdata %>% 
            select(country, KGI) %>%
            arrange(desc(KGI)) %>% 
            head(10)
    })
    
    # Need to implement filter for year and small countries
    # Need to substitute the year slider below with a proper widget
    # Need to fix the positioning of the map (larger and less movable)
    output$world_map <- renderPlotly({
        # Define plotly map's properties, font, labels, and layout
        graph_properties <- list(
        scope = 'world',
        showland = TRUE,
        landcolor = toRGB("white"),
        color = toRGB("white"))
        
        font = list(
        family = "DM Sans",
        size = 15,
        color = "black")
        
        label = list(
        bgcolor = "#EEEEEE",
        bordercolor = "transparent",
        font = font)
        
        borders_layout <- list(color = toRGB("grey"), width = 0.5)
        
        map_layout <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'Mercator'))
        # Build actual plotly map
        world_map = plot_geo(KGIdata, 
                         locationmode = "world", 
                         frame = ~date) %>%
            add_trace(locations = ~iso3c,
                  z = ~KGI,
                  zmin = 0,
                  zmax = max(KGIdata$KGI),
                  color = ~KGI,
                  colorscale = "Paired",
                  text = ~hover,
                  hoverinfo = 'text') %>%
            layout(geo = map_layout,
                   font = list(family = "DM Sans")) %>%
            style(hoverlabel=label) %>%
            config(displayModeBar=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
