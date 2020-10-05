setwd("~/RProjects/NMT")
rm(list = ls())

library(tidyverse)
library(rgdal)
library(raster)


# przygotoiwanie palety
paleta_rgb <- matrix(c(65,120,110,
                       90,160,60,
                       190,210,0,
                       255,250,120,
                       252,220,0,
                       245,190,0,
                       240,160,75,
                       230,130,70,
                       225,95,50,
                       210,65,45),
                     ncol = 3, byrow = TRUE)

paleta_rgb <- rgb(paleta_rgb/255)



theme_set(theme_minimal())

dane_plik <- "dane/malopolskie.txt"

dane_nmt <- read_delim(dane_plik, " ", col_names = c("x", "y", "z"), col_types = "ddd")


# przeliczenie wspołrzędnych
coordinates(dane_nmt) <- c("x", "y")
CRS_puwg1992 <- crs("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs ")
projection(dane_nmt) <- CRS_puwg1992 #dane są w układzie PUWG 1992
CRS_wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;") # układ WGS84 (współrzędne "normalne")
dane_nmt_wgs84 <- spTransform(dane_nmt, CRS_wgs84)

dane_nmt <- as.data.frame(dane_nmt_wgs84@coords) %>% mutate(z = dane_nmt_wgs84$z)


# zaokrąglenie danych = zmniejszenie dokładności i jednocześnie liczby punktów do narysowania
plot_data <- dane_nmt %>%
  mutate(x = round(x, 2),
         y = round(y, 2)) %>%
  group_by(x, y) %>%
  summarise(z = mean(z)) %>%
  ungroup()


plot_data %>%
  ggplot(aes(x, y, color = z)) +
  geom_point() +
  coord_map() +
  scale_color_gradientn(colors = paleta_rgb)


# dzielimy wysokość na 9 przedziałów, dziesiąty to poniżej 0.
przedzialy <- c(-1000, seq(0, round(max(full_df$z), -2), length.out=9))

plot_data %>%
  mutate(z_fct = cut(z, breaks = przedzialy)) %>%
  ggplot(aes(x, y, color = z_fct)) +
  geom_point() +
  coord_map() +
  scale_color_manual(values = paleta_rgb)


library(ggmap)
loc <- geocode("Kraków, Poland")
loc <- geocode("Warszawa, Poland")

dane_nmt %>%
  filter(y > as.numeric(loc[2]) - 0.25 & y < as.numeric(loc[2]) + 0.25) %>%
  filter(x > as.numeric(loc[1]) - 0.5 & x < as.numeric(loc[1]) + 0.5) %>%
  mutate(z_fct = cut(z, breaks = przedzialy)) %>%
  ggplot(aes(x, y, color = z_fct)) +
  geom_point() +
  geom_point(aes(x = as.numeric(loc[1]), y = as.numeric(loc[2])), size = 5, color = "red") +
  coord_map() +
  scale_color_manual(values = paleta_rgb)



mapa_gg <- get_map(c(mean(plot_data$x), mean(plot_data$y)), source = "google", maptype = "roadmap", zoom = 8)
ggmap(mapa_gg)


ggmap(mapa_gg, darken = 0.8) +
  geom_point(data = plot_data,
             aes(x, y, alpha = z, color = z)) +
  scale_alpha_continuous(range = c(0.01, 0.2)) +
  scale_color_gradientn(colors = paleta_rgb)
