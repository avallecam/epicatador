# Install if needed:
# pak::pak(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr", "showtext"))

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(showtext)

# Load Google Font for clean professional look
font_add_google("Lato", "lato")
showtext_auto()

# LSHTM brand colours
lshtm_navy <- "#00205b"
lshtm_red  <- "#e4003b"

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

# Highlight domicile countries
domicile_map <- world %>% filter(admin %in% domicile_countries)

# Compute centroids
domicile_centroids <- domicile_map %>% st_centroid()

# Title + Subtitle
title_text    <- "5 GROUPS. 30 DOMICILES. ONE TRAINING."
subtitle_text <- "5 training groups. Sessions across two days. Accredited program."

# Generate the plot
ggplot() +
  # Ocean background via panel fill (set in theme)
  geom_sf(data = world,
          fill = "#dce3ea", color = "#b0bec5", linewidth = 0.2) +
  geom_sf(data = domicile_map,
          fill = lshtm_navy, color = "#b0bec5", linewidth = 0.2) +
  geom_sf(data = domicile_centroids,
          shape = 21, fill = lshtm_red, color = "white",
          size = 3, stroke = 0.8) +
  coord_sf(ylim = c(-58, 85), expand = FALSE) +
  theme_minimal(base_family = "lato") +
  theme(
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#a8cce0", color = NA),   # ocean
    plot.title = element_text(
      size = 22, face = "bold", hjust = 0.5, color = lshtm_navy,
      margin = margin(t = 16, b = 6)
    ),
    plot.subtitle = element_text(
      size = 13, hjust = 0.5, color = "#555555",
      margin = margin(b = 16)
    ),
    plot.margin = margin(10, 20, 10, 20)
  ) +
  ggtitle(title_text, subtitle_text)
