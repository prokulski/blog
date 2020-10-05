# zrodło danych http://ipa.lovethosetrains.com/ipa_17_18.7z
# pobranie: wget http://ipa.lovethosetrains.com/ipa_17_18.7z
# instalujemy p7zip: sudo apt-get install p7zip
# wypakowanie: 7zr e ipa_17_18.7z ipa_17_18/api/train/* -ojson

# shellowa scala: ~/spark/spark-2.3.2-bin-hadoop2.6/bin/spark-shell
setwd("~/RProjects/opoznienia_pociagow")

library(tidyverse)
library(lubridate)
library(sparklyr)

# podobieństwo tekstów
library(stringdist)


# dane o opóźnieniach mamy w Sparku
sc <- spark_connect(master = "local")
df <- spark_read_parquet(sc, "trains", "trains_parquet", memory = FALSE)


# ujednolicenie nazw stacji
stacje_df <- df %>%
  distinct(station_name) %>%
  collect()

stacje_df <- stacje_df %>%
  mutate(station_name = case_when(
    station_name == "Warszawa Zoo" ~ "Warszawa ZOO",
    station_name == "Warszawa Wschodnia" ~ "Warszawa Wschodnia Osobowa",
    station_name == "Kraków Główny" ~ "Kraków Główny Osobowy",
    station_name == "Tułowice Niemodlińskie" ~ "Tułowice",
    station_name == "Małaszewicze Przystanek" ~ "Małaszewicze",
    station_name == "Wolin Pomorski" ~ "Wolin",
    station_name == "Przylep" ~ "Zielona Góra Przylep",
    station_name == "Brest Centralny" ~ "Brest",
    station_name == "Forst Lausitz" ~ "Forst (Baršć)",
    TRUE ~ station_name))


stacje <- readRDS("lokalizacje_stacji.rds") %>% drop_na()

razem <- left_join(stacje_df, stacje, by = "station_name")


# szukamy najbardziej podobnych nazw stacji dla tych, których nie udało się złączyć joinem automatycznie
# to będzie nasz słownik pośredni
str_mat <- razem %>%
  filter(is.na(id)) %>%
  pull(station_name) %>%
  stringdistmatrix(stacje$station_name, method= "cosine")

rownames(str_mat) <- razem %>%
  filter(is.na(id)) %>%
  pull(station_name)

colnames(str_mat) <- stacje$station_name

str_mat_d <- str_mat %>%
  as.data.frame() %>%
  rownames_to_column("station_1") %>%
  gather(key = "station_2", value = "simmilarity", -station_1) %>%
  filter(!is.nan(simmilarity)) %>%
  group_by(station_1) %>%
  filter(simmilarity == min(simmilarity)) %>%
  ungroup()


# łączymy ze słownikiem pośrednim
polozenie_stacji <- left_join(razem,
                              str_mat_d %>% select(-simmilarity),
                              by = c("station_name" = "station_1")) %>%
  # jeśli nie ma nazwy stacji w słowniku pośrednim to bierzemy oryginalną (znalazła się w tabeli ze współrzędnymi)
  # w przeciwnym przypadku - nazwę ze słownika
  mutate(station_name = if_else(is.na(station_2), station_name, station_2)) %>%
  # potrzebujemy tylko unikalne nazwy
  select(station_name) %>%
  distinct() %>%
  # dodajemy info o wspołrzędnych
  left_join(stacje, by = "station_name") %>%
  # jeśli nazwa się powtarza (ale stacja ma inne id) to uśredniamy położenie
  group_by(station_name) %>%
  summarise(long = mean(long, na.rm = TRUE),
            lat = mean(lat, na.rm = TRUE)) %>%
  ungroup() %>%
  drop_na()


# kolejnosc stacji dla kazdego z pociagow:
trasy <- df %>%
  # jesli nie ma czasu przyjazdu to jest taki sam jak odjazdu - to dzieje sie na stacji poczatkowej
  mutate(arrival_time = if_else(is.na(arrival_time), departure_time, arrival_time)) %>%
  # dla kazdego pociagu w kazdym dniu
  group_by(train_name, schedule_date) %>%
  # ustawiamy kolejnosc wg czasu przyjazdu (wiec w efekcie wg kolejnosci stacji)
  arrange(train_name, arrival_time) %>%
  # nadajemy numerki kolejno
  mutate(n = row_number()) %>%
  ungroup() %>%
  # wybieramy unikalne wiersze z nazwa pociagu, kolejnym numerem i nazwa stacji
  distinct(train_name, n, station_name) %>%
  # sortujemy wg numerow (w ramach nazwy pociagu)
  arrange(train_name, n) %>%
  collect()

spark_disconnect_all()


# relacje pociagow:
relacje <- trasy %>%
  group_by(train_name) %>%
  filter(n == max(n) | n == min(n)) %>%
  mutate(direction = if_else(n == min(n), "from", "to")) %>%
  ungroup() %>%
  select(-n) %>%
  distinct(train_name, direction, .keep_all = TRUE) %>%
  spread(direction, station_name)


relacje_n <- relacje %>%
  count(from, to) %>%
  mutate(from = case_when(
    from == "Warszawa Zoo" ~ "Warszawa ZOO",
    from == "Warszawa Wschodnia" ~ "Warszawa Wschodnia Osobowa",
    from == "Kraków Główny" ~ "Kraków Główny Osobowy",
    from == "Tułowice Niemodlińskie" ~ "Tułowice",
    from == "Małaszewicze Przystanek" ~ "Małaszewicze",
    from == "Wolin Pomorski" ~ "Wolin",
    from == "Przylep" ~ "Zielona Góra Przylep",
    from == "Brest Centralny" ~ "Brest",
    from == "Forst Lausitz" ~ "Forst (Baršć)",
    TRUE ~ from),
    to = case_when(
      to == "Warszawa Zoo" ~ "Warszawa ZOO",
      to == "Warszawa Wschodnia" ~ "Warszawa Wschodnia Osobowa",
      to == "Kraków Główny" ~ "Kraków Główny Osobowy",
      to == "Tułowice Niemodlińskie" ~ "Tułowice",
      to == "Małaszewicze Przystanek" ~ "Małaszewicze",
      to == "Wolin Pomorski" ~ "Wolin",
      to == "Przylep" ~ "Zielona Góra Przylep",
      to == "Brest Centralny" ~ "Brest",
      to == "Forst Lausitz" ~ "Forst (Baršć)",
      TRUE ~ to))

polozenie_stacji <- left_join(polozenie_stacji,
                              str_mat_d %>% select(-simmilarity),
                              by = c("station_name" = "station_2")) %>%
  mutate(station_1 = if_else(is.na(station_1), station_name, station_1))


relacje_n_plot <- relacje_n %>%
  mutate(stationA = if_else(from < to, from, to),
         stationB = if_else(from < to, to, from)) %>%
  select(from = stationA, to = stationB, n) %>%
  group_by(from, to) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  left_join(polozenie_stacji, by = c("from" = "station_1")) %>%
  select(-from) %>%
  rename(from = station_name, from_lon = long, from_lat = lat) %>%
  left_join(polozenie_stacji, by = c("to" = "station_1")) %>%
  select(-to) %>%
  rename(to = station_name, to_lon = long, to_lat = lat) %>%
  drop_na() %>%
  filter(from_lon != to_lon) %>%
  filter(from_lat != to_lat)


saveRDS(relacje_n_plot, "relacje_n_plot.rds")


top_cities <- bind_rows(
  relacje_n_plot %>%
    group_by(from, from_lon, from_lat) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    set_names(c("city", "long", "lat", "n")),
  relacje_n_plot %>%
    group_by(to, to_lon, to_lat) %>%
    summarise(n = sum(n)) %>%
    ungroup() %>%
    set_names(c("city", "long", "lat", "n"))
) %>%
  top_n(100, n) %>%
  select(-n) %>%
  distinct()




relacje_n_plot %>%
  ggplot() +
  geom_polygon(data = map_data("world", region = "Poland"),
               aes(long, lat, group=group), color = "red", fill = "white") +
  geom_curve(aes(x = from_lon, y = from_lat,
                 xend = to_lon, yend = to_lat,
                 alpha = n, size = n), show.legend = FALSE) +
  geom_point(data = top_cities, aes(long, lat)) +
  ggrepel::geom_text_repel(data = top_cities, aes(long, lat, label = city ), size = 2) +
  scale_alpha_continuous(range = c(0.1, 0.6)) +
  scale_size_continuous(range = c(0.1, 0.6)) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Sieć połączeń kolejowych")

