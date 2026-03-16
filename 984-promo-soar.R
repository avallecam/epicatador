# Install if needed:
# pak::pak(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "dplyr", "showtext", "cowplot"))

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(showtext)
library(cowplot)

# Epiverse fonts — Open Sans via Google Fonts
# Clash Display Bold must be installed locally (https://www.fontshare.com/fonts/clash-display)
# then add: font_add("clash", "ClashDisplay-Bold.otf")
font_add_google("Open Sans", "opensans")
showtext_auto()
font_title <- "opensans"   # swap to "clash" once Clash Display is installed
font_body  <- "opensans"

# Epiverse brand colours
ev_indigo <- "#071E2D"
ev_coral  <- "#F8494A"

# Domicile countries from last cohort
domicile_countries <- c(
  "Switzerland", "Pakistan", "Scotland", "New Caledonia", "India", "Germany",
  "South Korea", "Hong Kong", "Ethiopia", "England", "Philippines", "Somalia",
  "Bangladesh", "Tanzania", "Nigeria", "Egypt", "Benin", "Kenya", "Zambia",
  "Zimbabwe", "USA", "Italy", "Belgium", "Uganda", "Ireland", "Singapore",
  "Colombia", "Canada", "Taiwan", "Indonesia", "Nepal", "Ghana"
)

# Normalise UK labels
domicile_countries <- gsub("Scotland|England", "United Kingdom", domicile_countries)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Highlight domicile countries
domicile_map <- world %>% filter(admin %in% domicile_countries)

# Participant counts per domicile country
domicile_counts <- data.frame(
  admin = c("United Kingdom", "India", "United States of America", "Tanzania",
            "Nigeria", "Indonesia", "Bangladesh", "Canada", "Zambia",
            "Egypt", "Kenya", "Philippines"),
  n     = c(5, 5, 4, 4, 3, 3, 2, 2, 2, 2, 2, 2)
)

# Compute centroids and join counts (n = 1 for countries not in counts table)
domicile_centroids <- domicile_map %>%
  st_centroid() %>%
  left_join(domicile_counts, by = "admin") %>%
  mutate(n = replace(n, is.na(n), 1))

# Text
title_text    <- "OUTBREAK ANALYTICS IN R — JULY 2026"
subtitle_text <- "Last cohort: 55 participants from 31 countries · Apply now for the next edition"
caption_text  <- "LSHTM Short Course · Online · 6–31 July 2026 · Applications close 6 June 2026"


# LSHTM logo URL
lshtm_logo_url <- "https://www.lshtm.ac.uk/sites/default/files/LSHTM-logo-bw.jpg"

# Generate the plot
p <- ggplot() +
  # Base world map
  geom_sf(data = world,
          fill = "#dce3ea", color = "#b0bec5", linewidth = 0.2) +
  # Highlighted domiciles
  geom_sf(data = domicile_map,
          fill = ev_indigo, color = "#b0bec5", linewidth = 0.2) +
  # Centroid rings — size proportional to participant count
  geom_sf(data = domicile_centroids,
          aes(size = n), shape = 21,
          fill = alpha("white", 0.5), color = ev_coral, stroke = 1.5) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  # Bubble legend — bottom-right, near Australia
  annotate("point", x = c(81, 90, 99), y = c(-47, -47, -47),
           size = c(2, 4, 5), shape = 21,
           fill = alpha("white", 0.5), color = ev_coral, stroke = 1.5) +
  annotate("text", x = c(81, 90, 99), y = c(-51, -51, -51),
           label = c("1", "3", "5"), size = 5, fontface = "bold",
           color = ev_indigo, family = font_body) +
  annotate("text", x = 90, y = -57,
           label = "participants\nper country", size = 4.5,
           color = ev_indigo, family = font_body, lineheight = 0.9) +
  # Schedule card — semi-transparent indigo, Pacific Ocean
  annotate(
    "label", x = -130, y = -19,
    label = "5 learning groups\nAM & PM (UK time)\nto suit your time zone",
    fill = alpha(ev_indigo, 0.7), color = "white", label.size = NA,
    label.padding = unit(0.2, "lines"),
    fontface = "bold", family = font_body, size = 18, lineheight = 0.3
  ) +
  coord_sf(ylim = c(-58, 85), expand = FALSE) +
  theme_minimal(base_family = font_body) +
  theme(
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#e8edf2", color = NA),   # ocean — pale warm grey
    plot.title    = element_text(size = 104, face = "bold", hjust = 0.5, color = ev_indigo, family = font_title),
    plot.subtitle = element_text(size = 72,  hjust = 0.5, color = "#888888"),
    plot.caption  = element_text(size = 72,  hjust = 0.5, color = ev_coral),
    plot.margin = margin(10, 20, 10, 20)
  ) +
  ggtitle(title_text, subtitle_text) +
  labs(caption = caption_text)

# Overlay LSHTM logo in bottom-right white margin
p_final <- ggdraw(p) +
  draw_image(lshtm_logo_url, x = 0.82, y = 0.01, width = 0.15, height = 0.07)

ggsave("fig/promo-soar-2026.png", p_final, width = 11, height = 6.5, dpi = "retina")
