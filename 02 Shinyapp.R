# Load packages and data
library(pacman)
p_load(rio, shiny, shinydashboard, plotly, readr, tidyverse)
    
KGIdata_original <- rio::import(file = "data_processed/KGI.Rdata") %>%
    mutate(hover = paste0(country, "\nKGI: ", KGI_original, "\nIndicators available: ", n_vars_original, "/7"))

# Define UI ----------------------------------------------------------------
header <- dashboardHeader(title = "How globalized is the world?",
                          titleWidth = 400)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About", 
                 tabName = "model", 
                 icon = icon("book"),
                 menuSubItem("Method", 
                           href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard",
                           newtab = T),
                 menuSubItem("Sources", 
                           href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard",
                           newtab = T),
                 menuSubItem("Contributors",
                           href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard",
                           newtab = T)),
         menuItem("Controls", tabName = "model", icon = icon("mouse"),
                 sliderInput("year", "Select year", 1990, 2020, 2017, step = 1, sep = ""),
                 checkboxInput("small_include", 
                               "Include small countries? (pop. < 1 Mio.)", 
                               value = FALSE),
                 sliderInput("min_vars", 
                             "Min. Number of Indicators", 
                             1, 7, 3),
                 selectInput("version",
                             "KGI version:",
                             c("Kessler (2016)" = "KGI_original",
                               "Schröder (2020)" = "KGI_new")),
           # place this somewhere else:
                 tags$small(
                   "Note: 1. Since all indicators are computed on a per capita basis,",
                   " including small countries may produce an uniformative ranking.",
                   " 2. Data for 2020 should tread lightly due to the effects of the",
                   " COVID-19 pandemic."),
                  startExpanded = T
                 ),
        menuItem("Link to code", 
                 href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard",
                 icon = icon("code"),
                 newtab = T)
        ),
    collapsed = F
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
    
ui <- dashboardPage(skin = "red",
                    header, 
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
    version <- input$version
  
  # filtering
    KGIdata_filtered <- KGIdata_original %>% 
      mutate(KGI =  ifelse(version == "KGI_original",
                           KGI_original,
                           KGI_new),
             n_vars = ifelse(version == "KGI_original",
                             n_vars_original,
                             n_vars_new)) %>% 
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
        "The Kessler Globality Index (KGI) is a clear and effective measure of 
      globalization (see Kessler 2016; Schröder 2020). It comprises seven 
      indicators (per capita):\n
      -volume of foreign trade,\n
      -foreign direct investments (sum of inflows and outflows),\n
      -international meetings,\n 
      -international arrivals and departures at commercial airports,\n
      -international tourist arrivals and departures,\n
      -international incoming and outgoing telephone traffic in minutes,\n
      -estimated number of people with Internet access." 
    })
    

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
      # could include lowest 10 as well? or make table scrollable
    
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
