# Libraries
library(readxl)
library(jsonlite)
library(rgeos)
library(showtext)
library(sysfonts)
library(stringr)
library(cowplot)
library(units)
library(geojsonsf)
library(sf)
library(broom)
library(here)
library(ggplot2)
library(dplyr)

# Cities
Barcelona <- geojson_sf(here("Data", "Bike Routes Barcelona.geojson"))
Berlin <- geojson_sf(here("Data", "Bike Routes Berlin.geojson"))
Chicago <- geojson_sf(here("Data", "Bike Routes Chicago.geojson"))
London <- geojson_sf(here("Data", "Bike Routes London.geojson"))
Melbourne <- geojson_sf(here("Data", "Bicycle Routes Melbourne.geojson"))
Milan <- geojson_sf(here("Data", "Bike Routes Milan.geojson"))
Nyc <- geojson_sf(here("Data", "Bicycle Routes NYC.geojson"))
Sydney <- geojson_sf(here("Data", "Bike Routes Sydney.geojson"))
Toronto <- geojson_sf(here("Data", "Bike Routes Toronto.geojson"))
Zurich <- geojson_sf(here("Data", "taz_mm.tbl_routennetz.json"))

# Apply filters
# Remove Staten Island in NYC
Nyc <- Nyc %>% filter(boro != "5")
# Only keep streets with bike lanes or bike paths in Zurich
Zurich <- Zurich %>%
  filter(veloweg == 1 | velostreifen %in% c("BOTH", "FT", "TF", "1"))

# Configure fonts and aesthetics
showtext_auto()
font_add_google("Quicksand")
font_add_google("Fira Sans")
font1 <- "Quicksand"
font2 <- "Fira Sans"

topmargin <- 1.75
fontsize <- 160
color1 <- "#1c4831"
color2 <- "#955020"
color3 <- "#586b44"
color4 <- "#572016"
color5 <- "#0e5047"
color6 <- "#934839"
color7 <- "#1d3b71"
color8 <- "#406c6d"
color9 <- "#082253"
color10 <- "#536B3A"
linesize <- 1.05

# Create city plots
BarcelonaPlot <- ggplot() +
  geom_sf(data = Barcelona, aes(geometry = geometry), size = linesize, color = color1) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

BerlinPlot <- ggplot() +
  geom_sf(data = Berlin, aes(geometry = geometry), size = linesize, color = color2) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

ChicagoPlot <- ggplot() +
  geom_sf(data = Chicago, aes(geometry = geometry), size = linesize, color = color3) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

LondonPlot <- ggplot() +
  geom_sf(data = London, aes(geometry = geometry), size = linesize, color = color4) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

MelbournePlot <- ggplot() +
  geom_sf(data = Melbourne, aes(geometry = geometry), size = linesize,color = color5) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

MilanPlot <- ggplot() +
  geom_sf(data = Milan, aes(geometry = geometry), size = linesize, color = color6) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

NycPlot <- ggplot() +
  geom_sf(data = Nyc, aes(geometry = geometry), size = linesize, color = color7) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

SydneyPlot <- ggplot() +
  geom_sf(data = Sydney, aes(geometry = geometry), size = linesize, color = color8) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

TorontoPlot <- ggplot() +
  geom_sf(data = Toronto, aes(geometry = geometry), size = linesize, color = color9) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

ZurichPlot <- ggplot() +
  geom_sf(data = Zurich, aes(geometry = geometry), size = linesize, color = color10) +
  theme_void() +
  theme(plot.margin = margin(topmargin, 1, 0, 1, unit = "cm"))

# Create grid plot of maps
GridPlot <- plot_grid(BarcelonaPlot, BerlinPlot, ChicagoPlot, LondonPlot, MelbournePlot,
                      MilanPlot, NycPlot, SydneyPlot, TorontoPlot, ZurichPlot,
                      nrow = 2, ncol = 5) +
  theme(plot.margin = margin(8, 0, 2, 0, unit = "cm"),
        plot.background = element_rect(fill = "#fdf9f5", color = "#fdf9f5"))


# Draw grid plot Add annotations: title, subtitle, city labels, credits
y_1st_row = 0.83
y_2nd_row = 0.38

ggdraw(GridPlot) +
  draw_label(label = "Bike Connections in Major Cities",
             x = 0.5, y = 0.95, size = 300,
             fontfamily = font1, fontface = "bold", color = "#102f2f") +
  draw_label(label = "All available bike lanes/paths according to each city's open data portal",
             x = 0.5, y = 0.9, size = 120,
             fontfamily = font1, color = "#102f2f") +
  draw_label(label = "Idea, code and non-Zurich data: @BlakeRobMills | Adaptation for Zurich: @rastrau, 2021-11-03",
             x = 0.5, y = 0.015, size = 100,
             fontfamily = font1, color="#102f2f") +

  draw_label(label = "BARCELONA",
             x = 0.1, y = y_1st_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color1) +
  draw_label(label = "BERLIN",
             x = 0.3, y = y_1st_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color2) +
  draw_label(label = "CHICAGO",
             x = 0.5, y = y_1st_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color3) +
  draw_label(label = "LONDON",
             x = 0.7, y = y_1st_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color4) +
  draw_label(label = "MELBOURNE",
             x = 0.9, y = y_1st_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color5) +

  draw_label(label = "MILAN",
             x = 0.1, y = y_2nd_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color6) +
  draw_label(label = "NEW YORK",
             x = 0.3, y = y_2nd_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color7) +
  draw_label(label = "SYDNEY",
             x = 0.5, y = y_2nd_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color8) +
  draw_label(label = "TORONTO",
             x = 0.7, y = y_2nd_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color9) +
  draw_label(label = "ZURICH",
             x = 0.9, y = y_2nd_row,
             size = fontsize, fontfamily = font1,
             fontface = "bold", color = color10)

# Save visualization to disk
ggsave(here("30day map challenge - day 2 - lines.png"), width = 29.7, height = 21)
