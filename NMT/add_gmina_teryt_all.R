setwd("~/RProjects/NMT")
rm(list = ls())

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(RPostgreSQL))

suppressPackageStartupMessages(library(rgdal))
suppressPackageStartupMessages(library(rgeos))
suppressPackageStartupMessages(library(raster))

# dostęp do bazy danych
dbname = "baza"
user = "lemur"
password = "pr0cma1l"
host = "localhost"


db_connector <- dbDriver("PostgreSQL")


# definicja układu współrzednych
CRS_puwg1992 <- crs("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +units=m +no_defs ")
CRS_wgs84 <- crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs;") # układ WGS84 (współrzędne "normalne")

# wczytujemy mapę gmin
cat("Wczytuję mapę gmin.\n")
gminy_shp <- readOGR("../!mapy_shp/gminy.shp", "gminy")
gminy_shp <- spTransform(gminy_shp, CRS_wgs84)

pliczki <- tribble(~plik, ~teryt,
                   "dolnoslaskie.txt", "02",
                   "kujawsko_pomorskie.txt", "04",
                   "lubelskie.txt", "06",
                   "lubuskie.txt", "08",
                   "lodzkie.txt", "10",
                   "malopolskie.txt", "12",
                   "mazowieckie.txt", "14",
                   "opolskie.txt", "16",
                   "podkarpackie.txt", "18",
                   "podlaskie.txt", "20",
                   "pomorskie.txt", "22",
                   "slaskie.txt", "24",
                   "swietokrzyskie.txt", "26",
                   "warminsko_mazurskie.txt", "28",
                   "wielkopolskie.txt", "30",
                   "zachodniopomorskie.txt", "32")


# funkcja sprawdza czy punkty z f_dataframe są w ramach f_shape i dodaje im kod TERYT gminy
add_TERYT_code <- function(f_dataframe, f_shape) {

  # wyimek przerabiamy na SHP
  coordinates(f_dataframe) <- c("long", "lat")
  projection(f_dataframe) <- CRS_puwg1992
  f_dataframe <- spTransform(f_dataframe, CRS_wgs84)

  # macierz przypisania punktu do powiatu - to trochę trwa
  miejscowosci_mat <- gWithin(f_dataframe, f_shape, byid = TRUE) %>% t()

  # teraz numer kolumny to numer powiatu, numer wiersza to numer punktu


  # bierzemy punkty z powrotem do tabelki
  punkty <- as.data.frame(f_dataframe@coords)

  # dodajemy ich wysokość
  punkty$height <- f_dataframe@data$height

  # na podstawie numeru kolumny z TRUE dla każdego wiersza wypełniamy wartość z kodem TERYT gminy
  punkty$TERYT_gmn <- miejscowosci_mat %>%
    apply(1, function(x) min(which(x, arr.ind = TRUE), na.rm=TRUE)) %>%
    as.character(f_shape$jpt_kod_je)[.]

  # zwracamy gotową paczkę danych
  return(punkty)
}

# wielkość paczki
# paczki po 2000 punktów na raz wydaja sie byc optymalne
step_i <- 100

# przy zapisie do bazy to jest zbędne
punkty_all <- tibble()

# dla kazdego wojewodztwa
for(woj_num in 1:nrow(pliczki)) {

  # jaki plik i jaki kog TERYT wojweództwa?
  dane_plik <- paste0("dane/", as.character(pliczki[woj_num, 1]))
  woj_TERYT <- as.character(pliczki[woj_num, 2])

  # wybieramy powiaty z odpowiedniego województwa - żeby nie sprawdzać dla całego kraju
  wojewodztwo <- gminy_shp[str_sub(gminy_shp$jpt_kod_je, 1, 2) == woj_TERYT, ]

  # wyczytujemy dane o wysokości punków w województwie
  cat(paste0("\n\n", woj_num, ": Wczytuję plik z informacjami o wysokości punktów dla '", dane_plik , "'.\n"))
  dane_nmt <- read_delim(dane_plik, " ", col_names = c("long", "lat", "height"), col_types = "ddd")

  # ile paczek będzie?
  n_steps <- ceiling(nrow(dane_nmt)/step_i)
  cat(sprintf("\tMamy do zrobienia %d kroków pętli dla pliku '%s'.\n", n_steps, dane_plik))

  # podpinamy się do bazy danych
  db_con <- dbConnect(db_connector,
                      dbname = dbname,
                      user = user, password = password,
                      host = host)

  # jedziemy plik z wysokościami, w paczkach
  for(i in 1:n_steps) {
    cat(sprintf("\t\tkrok = %d / %d (%.2f%%)\r", i, n_steps, 100*i/n_steps))

    # bierzemy jedna paczkę punktów
    dane_nmt_small <- dane_nmt[ ((i-1)*step_i+1):(i*step_i), ] %>% na.omit()


    # dopisujemy kody TERYT gminy do punktów
    punkty <- add_TERYT_code(dane_nmt_small, wojewodztwo)

    # z TERYT gminy robimy TERYT województwa (pierwsze 2 znaki) i TERYT powiatu (pierwsze 4 znaki)
    punkty <- punkty %>%
      mutate(TERYT_woj = str_sub(TERYT_gmn, 1, 2),
             TERYT_pow = str_sub(TERYT_gmn, 1, 4))

    # dopisujemy wynik do pełnej listy w bazie
    dbWriteTable(db_con, "nmt_polska", punkty,
                 append = TRUE, row.names = FALSE)

  }

  # odlaczamy od bazy
  dbDisconnect(db_con)
  cat(sprintf("\tPlik '%s' przepisany do bazy danych.\n", dane_plik))

}

