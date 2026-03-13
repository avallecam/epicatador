# Install if needed:
# install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr", "showtext"))

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(showtext)

# Load Google Font for clean professional look
font_add_google("Lato", "lato")
showtext_auto()

# Domicile countries
domicile_countries <- c(
  "Switzerland", "Pakistan", "Scotland", "New Caledonia", "India", "Germany",
  "South Korea", "Hong Kong", "England", "Philippines", "Somalia", "Bangladesh",
  "Tanzania", "Nigeria", "Egypt", "Benin", "Kenya", "Zambia", "Zimbabwe", "USA",
  "Belgium", "Uganda", "Ireland", "Singapore", "Colombia", "Canada", "Taiwan",
  "Indonesia", "Nepal", "Ghana"
)

# Normalise UK labels
domicile_countries <- gsub("Scotland|England", "United Kingdom", domicile_countries)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter countries
domicile_map <- world %>% filter(admin %in% domicile_countries)

# Compute centroids
domicile_centroids <- domicile_map %>% st_centroid()

# Title + Subtitle
title_text <- "5 GROUPS. 31 DOMICILES. ONE TRAINING."
subtitle_text <- "5 training groups. Sessions across two days. Accredited program."

# Generate the plot
ggplot() +
  geom_sf(data = world, fill = "#e5e5e5", color = "#cccccc", size = 0.2) +
  geom_sf(data = domicile_centroids, color = "#d60000", size = 3) +
  theme_minimal(base_family = "lato") +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(
      size = 24, face = "bold", hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 14, hjust = 0.5, color = "#555555",
      margin = margin(t = 10, b = 20)
    )
  ) +
  ggtitle(title_text, subtitle_text)