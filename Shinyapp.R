# Load packages and data
library(pacman)
p_load(rio, shiny, shinydashboard, plotly, readr, tidyverse)
    
#data loading does not work, load from 02_build_dashboard.R
KGIdata_original <- rio::import(file = "data_processed/KGI.Rdata") %>%
    mutate(hover = paste0(country, "\nKGI: ", KGI, "\nIndicators available: ", n_vars, "/7"))

# Define UI ----------------------------------------------------------------
header <- dashboardHeader(title = "How globalized is the world really?",
                          titleWidth = 400)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About", tabName = "model", icon = icon("book"),
                 textOutput("text")),
        menuItem("Controls", tabName = "model", icon = icon("mouse"),
                 sliderInput("year", "Select year", 1990, 2020, 1990, step = 1, sep = ""),
                 checkboxInput("small_include", "Include small countries? (pop. < 1 Mio.)", value = FALSE),
                 sliderInput("min_vars", "Min. Number of Indicators", 1, 7, 3)),
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

# Define Server -----------------------------------------------------------
server <- function(input, output, session) {
    
  # We have to create temp vars here to filter
  filtered_data <- reactive({
    year <- input$year
    small_include <- input$small_include
    min_vars <- input$min_vars
  
  # filtering
    KGIdata_filtered <- KGIdata_original %>% 
      filter(ifelse(small_include == T,
                      !is.na(small), 
                      small == F),
             date == year,
             n_vars >= min_vars) %>% 
      # just to make sure:
      as.data.frame() %>% 
      arrange(desc(KGI))
    KGIdata_filtered
    })  
  # This doesn't change, right? so no need to have it inside server function
      # Need to write correct text
    output$text <- renderText({ 
        "The Kessler Globality Index..." 
    })
    
 # Do we have to use the temp vars here too?
    observeEvent(input$year,{
        updateSliderInput(session, "year")
        updateCheckboxInput(session, "small_include")
        updateSliderInput(session, "min_vars")  
    })
    
    output$ranking <- renderTable({
        filtered_data() %>% 
            select(country, KGI) %>%
            arrange(desc(KGI)) %>% 
            head(10)
    })
      # could include lowest 10 as well? or make tabel scrollable
    
    # Could also quite easily construct the updated index from Schröder 2020 as alternative to switch between
    
    output$world_map <- renderPlotly({
        # Define plotly map's properties, font, labels, and layout
        graph_properties <- list(
        scope = 'world',
        showland = TRUE,
        landcolor = toRGB("lightgrey"),
        color = toRGB("lightgrey"))
        
        font = list(
        family = "DM Sans",
        size = 15,
        color = "black")
        
        label = list(
        bgcolor = "#EEEEEE",
        bordercolor = "gray",
        font = font)
        
        borders_layout <- list(color = toRGB("grey"), width = 0.5)
        
        map_layout <- list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = 'Mercator'))
        # Build actual plotly map
        world_map = plot_geo(filtered_data(), 
                         locationmode = "world", 
                         frame = ~date) %>%
            add_trace(locations = ~iso3c,
                  z = ~KGI,
                  zmin = 0,
                  zmax = 100,
                  color = ~KGI,
                  colorscale = "Inferno",
                  text = ~hover,
                  hoverinfo = 'text') %>%
            layout(geo = map_layout,
                   font = list(family = "DM Sans")) %>%
            style(hoverlabel=label) %>%
            config(displayModeBar=FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, 
         server = server)
