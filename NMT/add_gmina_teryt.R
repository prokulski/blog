setwd("~/RProjects/NMT")
rm(list = ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(raster))


dane_plik <- "dane/malopolskie.txt"
woj_TERYT <- "12"

# definicja układu współrzednych
CRS_puwg1992 <- crs("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs ")
CRS_wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;") # układ WGS84 (współrzędne "normalne")

# wczytujemy mapę gmin
cat("Wczytuję mapę gmin.\n")
gminy_shp <- readOGR("../!mapy_shp/gminy.shp", "gminy")
gminy_shp <- spTransform(gminy_shp, CRS_wgs84)

# wybieramy powiaty z odpowiedniego województwa
wojewodztwo <- gminy_shp[substr(gminy_shp$jpt_kod_je, 1, 2) == woj_TERYT, ]

# kasujemy gminy_shp dla oszczednosci pamieci
rm(gminy_shp)



# wyczytujemy dane o wysokoąci punków w województwie
cat("Wczytuję plik z informacjami o wysokości punktów.\n")
dane_nmt <- read_delim(dane_plik, " ", col_names = c("x", "y", "z"), col_types = "ddd")


# funkcja sprawdza czy punkty z f_dataframe są w ramach f_shape i dodaje im kod TERYT gminy
add_TERYT_code <- function(f_dataframe, f_shape) {
  # wyimek przerabiamy na SHP
  coordinates(f_dataframe) <- c("x", "y")
  projection(f_dataframe) <- CRS_puwg1992
  f_dataframe <- spTransform(f_dataframe, CRS_wgs84)

  # macierz przypisania punktu do powiatu - to trochę trwa
  miejscowosci_mat <- gWithin(f_dataframe, f_shape, byid = TRUE)

  miejscowosci_mat <- t(miejscowosci_mat)
  # numer kolumny to numer powiatu, numer wiersza to numer punktu


  # bierzemy punkty z powrotem do tabelki
  punkty <- as.data.frame(f_dataframe@coords) %>% set_names(c("long", "lat"))
  punkty$height <- f_dataframe@data$z


  punkty$TERYT_gmn <- miejscowosci_mat %>%
    apply(1, function(x) min(which(x, arr.ind = TRUE), na.rm=TRUE)) %>%
    as.character(f_shape$jpt_kod_je)[.]

  return(punkty)
}


# dla całego województwa przygotowujemy dane
punkty_all <- tibble()

step_i <- 100 # paczki po 2000 punktów na raz wydaja sie byc optymalne
n_steps <- ceiling(nrow(dane_nmt)/step_i)

cat(sprintf("Mamy do zrobienia %d kroków pętli\n", n_steps))


for(i in 1:n_steps) {
  cat(sprintf("\tkrok = %d / %d (%.1f%%)\r", i, n_steps, i/n_steps))

  # bierzemy jedna paczkę punktów
  dane_nmt_small <- dane_nmt[ ((i-1)*step_i+1):(i*step_i), ] %>% na.omit()


  # dopisujemy kody TERYT gminy do punktów
  punkty <- add_TERYT_code(dane_nmt_small, wojewodztwo)
  # z TERYT gminy robimy TERYT województwa (pierwsze 2 znaki) i TERYT powiatu (pierwsze 4 znaki)
  punkty <- punkty %>%
    mutate(TERYT_woj = substr(TERYT_gmn, 1, 2),
           TERYT_pow = substr(TERYT_gmn, 1, 4))


  # łaczymy razem
  punkty_all <- bind_rows(punkty_all, punkty)

  # albo lepiej - dopisujemy do bazy
}

# zapisujemy wynikowe dane
saveRDS(punkty_all, "dane/malopolskie.rds")

# TERYT na faktory - dane będą mniejsze
punkty_all <- punkty_all %>%
  mutate(TERYT_woj = as.factor(TERYT_woj),
         TERYT_pow = as.factor(TERYT_pow),
         TERYT_gmn = as.factor(TERYT_gmn))

# zapisujemy wynikowe dane
saveRDS(punkty_all, "dane/malopolskie_factors.rds")


# jeden powiat
ggplot() +
  # punkty w gmnienie Słaboszów
  geom_point(data = punkty_all %>% filter(TERYT_gmn == "1208072"), aes(long, lat, color = height)) +
  # granice powiatu miechowskiego
  geom_polygon(data = wojewodztwo[substr(wojewodztwo$jpt_kod_je, 1, 4) == "1208", ],
               aes(long, lat, group=group), color = "black", fill = NA) +
  coord_map()


# całe województwo
ggplot() +
  geom_point(data = punkty_all, aes(long, lat, color = height)) +
  # granice powiatu miechowskiego
  geom_polygon(data = wojewodztwo, aes(long, lat, group=group), color = "black", fill = NA) +
  coord_map()




#### wersja z sp::over

# wczytujemy shp gmin, wydzielamy województow
gminy_shp <- readOGR("../!mapy_shp/gminy.shp", "gminy")
gminy_shp <- spTransform(gminy_shp, CRS_wgs84)
wojewodztwo <- gminy_shp[substr(gminy_shp$jpt_kod_je, 1, 2) == woj_TERYT, ]

# wczytujemy dane (i bierzemy 1% znich - żeby było szybciej)
dane_nmt <- read_delim(dane_plik, " ", col_names = c("x", "y", "z"), col_types = "ddd") %>%
sample_frac(0.01)

# dane puktowe transformujemy na shp
coordinates(dane_nmt) <- c("x", "y")
projection(dane_nmt) <- CRS_puwg1992
dane_nmt <- spTransform(dane_nmt, CRS_wgs84)

# to robi robotę :)
wynik <- over(dane_nmt, wojewodztwo)

dane_nmt$teryt <- wynik$jpt_kod_je

# przerabiamy na obiekt sf - łatwiej do rysowania
dane_nmt_sf <- st_as_sf(dane_nmt)

dane_nmt_sf


# rysujemy
ggplot(dane_nmt_sf, aes(color = teryt)) +
  geom_sf(show.legend = FALSE)



# rysujemy jeden powiat, z jego granicami
ggplot() +
  geom_sf(data = dane_nmt_sf %>%
            filter(str_sub(teryt, 1, 4) == "1216"),
          aes(color = teryt),
          show.legend = FALSE) +
  geom_sf(data = st_as_sf(wojewodztwo) %>%
            filter(str_sub(jpt_kod_je, 1, 4) == "1216"),
          fill = NA,
          color = "black")
