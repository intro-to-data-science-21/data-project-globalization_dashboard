# Load packages and data
library(shiny)
library(pacman)
library(plotly)
library(dplyr)
library(readr)
library(tidyverse)

p_load(rio)
KGIdata <- rio::import(file = "data_processed/KGI.Rdata")%>%
    mutate(hover = paste0(country, "\nKGI:", KGI))

# Define UI ----------------------------------------------------------------
ui <- fluidPage(
    titlePanel("A Globalization Dashboard (1990 - 2020)"),
    fluidRow(
        column(3,
               wellPanel(
                   # here we can add some other commands/filters/sliders/info,
                   # since the plot itself come swith a year slider
               )
        ),
        column(9,
               plotlyOutput("world_map")
               )
        )
    )

# Define Server -----------------------------------------------------------
server <- function(input, output) {

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
