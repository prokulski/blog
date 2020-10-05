setwd("~/RProjects/NMT")
rm(list = ls())

library(tidyverse)
library(rgdal)
library(raster)
library(fs)

prepare_data <- function(dane_nmt, rnd_fct = -3) {

  # punkty są z dokładnością do setek - nie ma co zaokrąglać do dziesiątek itp
  rnd_fct <- ifelse(rnd_fct > -2, -2, rnd_fct)
  # zaokrąglenie danych = zmniejszenie dokładności i jednocześnie liczby punktów do narysowania
  plot_data <- dane_nmt %>%
    mutate(x = round(x, rnd_fct),
           y = round(y, rnd_fct)) %>%
    group_by(x, y) %>%
    summarise(z = mean(z)) %>%
    ungroup()


  # przeliczenie wspołrzędnych
  coordinates(plot_data) <- c("x", "y")
  CRS_puwg1992 <- crs("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs ")
  projection(plot_data) <- CRS_puwg1992 #dane są w układzie PUWG 1992
  CRS_wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;") # układ WGS84 (współrzędne "normalne")
  dane_nmt_wgs84 <- spTransform(plot_data, CRS_wgs84)

  plot_data <- as.data.frame(dane_nmt_wgs84@coords) %>% mutate(z = dane_nmt_wgs84$z)

  return(plot_data)
}



pliki <- paste0("dane/", list.files("dane", pattern = "*.txt"))


full_df <- tibble()

for(i in 1:length(pliki)) {

  cat(paste0("Przerabiam plik ", pliki[[i]], "\n"))

  # wczytujemy plik
  temp_file <- read_delim(pliki[[i]], " ", col_names = c("x", "y", "z"), col_types = "ddd")

  # przekształcamy dane
  tmp_df <- prepare_data(temp_file, -2)

  # dołczamy do reszty danych
  full_df <- bind_rows(full_df, tmp_df)
}


# uśrednienie jeszcze raz - wygładzenie granic
full_df <- full_df %>%
  group_by(x, y) %>%
  summarise(z = mean(z)) %>%
  ungroup()

saveRDS(full_df, "nmt_polska_3.RDS")



#### rysowanie mapek ----


# przygotoiwanie palety
paleta_rgb <- matrix(c(65,120,110, # <0
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



full_df %>%
  ggplot(aes(x, y, color = z)) +
  geom_point() +
  coord_map() +
  scale_color_gradientn(colors = paleta_rgb)


# dzielimy wysokość na 9 przedziałów, dziesiąty to poniżej 0.
przedzialy <- c(-1000, seq(0, round(max(full_df$z), -2), length.out=9))

full_df %>%
  mutate(z_fct = cut(z, breaks = przedzialy)) %>%
  ggplot(aes(x, y, color = z_fct)) +
  geom_point() +
  coord_map() +
  scale_color_manual(values = paleta_rgb)
