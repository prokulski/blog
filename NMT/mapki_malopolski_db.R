setwd("~/RProjects/NMT")
rm(list = ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(raster))
library(broom)

# baza danych
dbname = "baza"
user = "lemur"
password = "pr0cma1l"
host = "localhost"


# wczytujemy mapę gmin
cat("Wczytuję mapę gmin.\n")
gminy_shp <- readOGR("../!mapy_shp/gminy.shp", "gminy")
gminy_shp <- spTransform(gminy_shp, CRS("+init=epsg:4326"))

# wybieramy powiaty z odpowiedniego województwa - żeby nie sprawdzać dla całego kraju
wojewodztwo <- gminy_shp[str_sub(gminy_shp$jpt_kod_je, 1, 2) == "12", ]

# wczytujemy tabelke z punktami
db_con <- src_postgres(dbname = dbname, host = host, user = user, password = password)
punkty_all <- db_con %>% tbl("nmt_polska")


# przygotoiwanie palety
paleta_rgb <- c("#41786E", "#5AA03C", "#BED200", "#FFFA78", "#FCDC00",
                "#F5BE00", "#F0A04B", "#E68246", "#E15F32", "#D2412D")


# średnia wysokość w gminie
mheight_gminy <- punkty_all %>%
  group_by(TERYT_gmn) %>%
  summarise(height_gmn = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  collect()

# mapa z shp na df
wojewodztwo_df_gminy <- tidy(wojewodztwo, region = "jpt_kod_je")

# łączymy mapę z danymi
wojewodztwo_df_gminy %>%
  left_join(mheight_gminy, by = c("id" = "TERYT_gmn")) %>%
  # rysyjemy
  ggplot() +
  geom_polygon(aes(long, lat, group=group, fill = height_gmn), color = "black") +
  coord_map() +
  scale_fill_gradientn(colours = paleta_rgb)




# średnia wysokość w powiecie
mheight_powiaty <- punkty_all %>%
  group_by(TERYT_pow) %>%
  summarise(height_pow = mean(height, na.rm = TRUE)) %>%
  ungroup() %>%
  collect()


# mapa średniej wysokości w powiatach z konturami gmin
wojewodztwo_df_gminy %>%
  # obciecie TERYT gminy do powiatu
  mutate(id = str_sub(id, 1, 4)) %>%
  left_join(mheight_powiaty, by = c("id" = "TERYT_pow")) %>%
  # rysyjemy (razem z konturami gmin)
  ggplot() +
  geom_polygon(aes(long, lat, group=group, fill = height_pow), color = "black") +
  coord_map() +
  scale_fill_gradientn(colours = paleta_rgb)


# mapa średniej wysokości w powiatach bez konturów gmin

# mapa powiatów (tylko po to, żeby pozbyć się konturów gmin)
powiaty_mapa <- readOGR("../!mapy_shp/powiaty.shp", layer = "powiaty")
powiaty_mapa <- spTransform(powiaty_mapa, CRS("+init=epsg:4326"))
powiaty_mapa <- tidy(powiaty_mapa, region = "jpt_kod_je") %>%
  filter(str_sub(id, 1, 2) == "12")

powiaty_mapa %>%
  left_join(mheight_powiaty, by = c("id" = "TERYT_pow")) %>%
  # rysyjemy (razem z konturami gmin)
  ggplot() +
  geom_polygon(aes(long, lat, group=group, fill = height_pow), color = "black") +
  coord_map() +
  scale_fill_gradientn(colours = paleta_rgb)




# dokładne punkty w powiecie
# tam gdzie najwyzej - Zakopane?
powiat_df_gminy <- gminy_shp[str_sub(gminy_shp$jpt_kod_je, 1, 4) == "1217", ]

# rysyjemy
ggplot() +
  geom_point(data = punkty_all %>% filter(TERYT_pow == "1217"),
             aes(long, lat, color = height)) +
  geom_polygon(data = powiat_df_gminy,
               aes(long, lat, group=group), color = "black", fill = NA) +
  coord_map() +
  scale_color_gradientn(colours = paleta_rgb)


dbDisconnect(db_con$con)
