library(dplyr)
library(ggplot2)
library(sf)
library(tigris)
library(scales)
library(ggtext)

path <- "https://raw.githubusercontent.com/gurupratap-matharu/machine-learning-regression/refs/heads/master/dataset/housing.csv"
data <- read.csv(path)

options(tigris_use_cache = TRUE)
california <- counties(state = "CA", class = "sf")

data <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

ggplot() +
  geom_sf(data = california, fill = "gray", color = "black", alpha = 0.5) +
  geom_sf(data = data, color = "blue", fill = "white", shape = 21, size = 2, stroke = 0.5) +
  labs(
    title = "Mapa da Califórnia com Condados e Residências",
    subtitle = "Distribuição de Residências no Estado da Califórnia",
    caption = "<b>Fonte</b>: Dados de Localização de Residências",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0),
    legend.title = element_blank(),
    plot.caption = element_markdown(hjust = 0),
    plot.caption.position = "plot"
  )















