setwd("/home/lemur/RProjects/opoznienia_pociagow")

library(tidyverse)

stacje <- readRDS("lokalizacje_stacji.rds")
(m_id <- max(stacje$id))

stacje <- filter(stacje, !is.na(station_name))
(m_nrow <- nrow(stacje))


ggplot() +
  geom_polygon(data = map_data("world", region = "Poland"),
               aes(long, lat, group=group), color = "red", fill = "white") +
  geom_point(data = stacje, aes(long, lat), size = 0.1) +
  # geom_text(data = stacje, aes(long, lat, label = station_name  )) +
  coord_quickmap() +
  theme_minimal() +
  labs(title = paste0("Zebrano ", m_nrow, " lokalizacji stacji (z ", m_id, " zapytań)"),
       x = "", y = "")



ggplot() +
  geom_polygon(data = map_data("world", region = "Poland"),
               aes(long, lat, group=group), color = "red", fill = "white") +
  stat_density_2d(data = stacje,
                  aes(long, lat,
                      fill = ..level.., alpha = ..level..), geom = "polygon", h = 2,
                  show.legend = FALSE) +
  geom_point(data = stacje, aes(long, lat),
             size = 0.1, color = "black", alpha = 0.1) +
  scale_alpha_continuous(range = c(0.4, 0.8)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  coord_quickmap() +
  theme_minimal() +
  labs(title = paste0("Zebrano ", m_nrow, " lokalizacji stacji (z ", m_id, " zapytań)"),
       x = "", y = "")



library(ggmap)

google_key <- "AIzaSyCuwFglUiG87F2ndBpg3KHbw91ge9AMUoA"
register_google(key = google_key)

warszawa <- get_map("Warszawa")

ggmap(warszawa) +
  geom_point(data = stacje %>%
               filter(str_detect(station_name, "Warszawa")),
             aes(long, lat))

