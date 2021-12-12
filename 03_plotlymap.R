library(plotly)
library(dplyr)
library(readr)
library(pacman)
library(tidyverse)

p_load(rio)
KGIdata <- rio::import(file = "data_processed/KGI.Rdata") %>%
  mutate(hover = paste0(country, "\nKGI:", KGI))

graph_properties <- list(
  scope = 'world',
  showland = TRUE,
  landcolor = toRGB("white"),
  color = toRGB("white")
)

font = list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label = list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = font
)

borders_layout <- list(color = toRGB("grey"), width = 0.5)
map_layout <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

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
         title = "KGI in the world\n1990 - 2020",
         font = list(family = "DM Sans")) %>%
  style(hoverlabel=label) %>%
  config(displayModeBar=FALSE)

world_map
