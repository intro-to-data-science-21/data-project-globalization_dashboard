# Load packages and data
library(pacman)
p_load(rio, shiny, shinydashboard, plotly, readr, tidyverse)
    
KGIdata_original <- rio::import(file = "data_processed/KGI.Rdata")

# Define UI ----------------------------------------------------------------
header <- dashboardHeader(title = "How globalized is the world? The Kessler Globality Index",
                          titleWidth = 750)

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("About", 
                 tabName = "model", 
                 icon = icon("book"),
                 
                 menuItem("Method", 
                           href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard/blob/main/README.md#kessler-globality-index-kgi",
                           newtab = F),
                 
                 menuItem("Full Report", 
                          href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard/blob/main/README.md",
                          newtab = F),
                 
                 menuItem("Sources", 
                             menuSubItem(text = "Kessler (2016)", 
                                         href = "https://link.springer.com/book/10.1007/978-3-658-02388-1",
                                         newtab = F),
                             menuSubItem(text = "Schröder (2020)", 
                                         href = "https://hertieschool-my.sharepoint.com/personal/204856_hertie-school_org/_layouts/15/onedrive.aspx?id=%2Fpersonal%2F204856%5Fhertie%2Dschool%5Forg%2FDocuments%2FJGU%2FPolitikwissenschaft%2FBachelorarbeit%2FBachelorarbeit%20Milan%20Schr%C3%B6der%2FGerechte%20Globalisierung%20%2D%20Zur%20Messung%20von%20Globalisierungsprozessen%20und%20ihrem%20Einfluss%20auf%20die%20Verteilungsgerechtigkeit%2Epdf&parent=%2Fpersonal%2F204856%5Fhertie%2Dschool%5Forg%2FDocuments%2FJGU%2FPolitikwissenschaft%2FBachelorarbeit%2FBachelorarbeit%20Milan%20Schr%C3%B6der",
                                         newtab = F)),
                 
                 menuItem("Contributors", 
                             menuSubItem(text = "Francesco Danovi, Università Bocconi",
                                         href = "https://it.linkedin.com/in/francesco-danovi-189152186",
                                         newtab = F),
                             menuSubItem(text = "Federico Mammana, Università Bocconi",
                                         href = "https://www.linkedin.com/in/federico-mammana/",
                                         newtab = F),
                             menuSubItem(text = "Milan Schröder, Hertie School",
                                         href = "https://www.linkedin.com/in/milan-schroeder/",
                                         newtab = F))),
        
         menuItem("Controls", tabName = "model", icon = icon("mouse"), startExpanded = T,
                 sliderInput("year", "Select year", 1990, 2020, 2017, step = 1, sep = "", animate = T),
                 checkboxInput("small_include", 
                               "Include small countries? (pop. < 1 Mio.)", 
                               value = FALSE),
                 
                 sliderInput("min_vars", 
                             "Min. Number of Indicators", 
                             1, 7, 3),
                 
                 selectInput("version",
                             "KGI version:",
                             c("Kessler (2016)" = "KGI_original",
                               "Schröder (2020)" = "KGI_new"))),
        
        menuItem("Link to Code", 
                 href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard",
                 icon = icon("code"),
                 newtab = F),
        menuItem("Download Data", 
                 icon = icon("save"),
                 menuItem(".csv", 
                          href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard/raw/main/data_processed/KGI.csv",
                          newtab = F),
                 menuItem(".xlsx", 
                          href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard/raw/main/data_processed/KGI.xlsx",
                          newtab = F),
                 menuItem(".Rdata", 
                          href = "https://github.com/intro-to-data-science-21/data-project-globalization_dashboard/raw/main/data_processed/KGI.Rdata",
                          newtab = F)),
    collapsed = F
    ),
  width = 300
)

body <- dashboardBody(
    fluidRow(
       column(3,
              wellPanel(
                  h4("Most globalized countries"), tableOutput("ranking"))),
       column(9,
              plotlyOutput("world_map"))),
    fluidRow(
      column(12, 
             verbatimTextOutput("description"))),
    fluidRow(
      column(12, 
             wellPanel(span("The Kessler Globality Index (KGI) is a clear and effective measure of the level of globalization (see Kessler 2016). 
                          It comprises seven indicators (per capita):"),
              tags$ul(
                tags$li("volume of international trade (WDI)"), 
                tags$li("foreign direct investments (sum of inflows and outflows, WDI)"), 
                tags$li("international meetings (UIA)"),
                tags$li("international arrivals and departures at commercial airports (ICAO)"), 
                tags$li("international incoming and outgoing telephone traffic in minutes (ITU)"), 
                tags$li("share of individuals using the internet (ITU)"))))),
    fluidRow(
      column(12,
             span("For detailled information consult our methods.",
                  align = "right"))),
    fluidRow(
      column(12,
             span("Note: Since all indicators are computed on a per capita basis, including small countries may produce an uniformative ranking.",
                  align = "right"))),
    fluidRow(
      column(12,
             span("Data for 2020 should tread lightly due to the effects of the COVID-19 pandemic.",
                  align = "right")))
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
                             n_vars_new),
             indicators_max = ifelse(version == "KGI_original",
                                     7,
                                     6),
             hover = paste0(country, "\nKGI: ", KGI, "\nIndicators available: ", n_vars, "/", indicators_max)) %>%
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
                  z = ~ KGI,
                  zmin = 0,
                  zmax = 100,
                  color = ~ KGI,
                  colorscale = "Inferno",
                  text = ~ hover,
                  hoverinfo = 'text') %>%
            layout(geo = map_layout,
                   font = list(family = "DM Sans")) %>%
            style(hoverlabel = label) %>%
            config(displayModeBar = FALSE)
    })

# output$description <- renderText({
#   ifelse(version == "KGI_original",
#            span("The Kessler Globality Index (KGI) is a clear and effective measure of the level of globalization (see Kessler 2016). 
#                           It comprises seven indicators (per capita):",
#                 tags$ul(
#                   tags$li("volume of international trade (WDI)"), 
#                   tags$li("foreign direct investments (sum of inflows and outflows, WDI)"), 
#                   tags$li("international meetings (UIA)"),
#                   tags$li("international arrivals and departures at commercial airports (ICAO)"), 
#                   tags$li("international incoming and outgoing telephone traffic in minutes (ITU)"), 
#                   tags$li("share of individuals using the internet (ITU)"))),
#   span("lol")
}

# Run the application 
shinyApp(ui = ui, 
         server = server)
