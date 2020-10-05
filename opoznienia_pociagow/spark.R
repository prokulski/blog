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

df %>% head(10) %>% collect()
df %>% head(10) %>% collect() %>% glimpse()

count(df) %>% collect()




# ujednolicenie nazw stacji
stacje_df <- df %>%
  distinct(station_name) %>%
  collect()

stacje_df %>% filter(str_detect(station_name, "Kraków")) %>% as.data.frame()

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

stacje_df %>% filter(str_detect(station_name, "Kraków")) %>% as.data.frame()


stacje <- readRDS("lokalizacje_stacji.rds")

stacje <- stacje %>% drop_na()

razem <- left_join(stacje_df, stacje, by = "station_name")

razem %>% filter(str_detect(station_name, "Aleksandrów Kujawski")) %>% as.data.frame()


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

# czy jest Warszawa Wschodnia?
polozenie_stacji %>%
  filter(str_detect(station_name, "Warszawa")) %>%
  arrange(station_name) %>%
  as.data.frame()

# zobaczmy mapkę warszawskich stacji
polozenie_stacji %>%
  filter(str_detect(station_name, "Warszawa")) %>%
  ggplot() +
  geom_point(aes(long, lat)) +
  ggrepel::geom_text_repel(aes(long, lat, label = station_name))


# zobaczmy mapkę krakowskich stacji
polozenie_stacji %>%
  filter(str_detect(station_name, "Kraków")) %>%
  ggplot() +
  geom_point(aes(long, lat)) +
  ggrepel::geom_text_repel(aes(long, lat, label = station_name))




df_filltered <- df %>%
  filter(!is.na(arrival_delay)) %>%
  filter(arrival_delay <= mean(arrival_delay) + 2 * sd(arrival_delay)) %>%
  filter(arrival_delay >= mean(arrival_delay) - 2 * sd(arrival_delay))

# rozkład opoznien przyjazdow
df_filltered %>%
  dbplot_histogram(arrival_delay, binwidth = 5)


# opoznienie przyjazdu vs opoznienie odjazdu (czy opoznione pociagi stoja krocej na stacji?)
df_filltered %>%
  dbplot_raster(arrival_delay, departure_delay)


# srednie opoznienie pociagow wg dnia
mean_arrival_delay_df <- df_filltered %>%
  group_by(schedule_date) %>%
  summarise(mean_arrival_delay = mean(arrival_delay)) %>%
  ungroup() %>%
  collect()


ggplot(mean_arrival_delay_df) +
  geom_line(aes(schedule_date, mean_arrival_delay))

summary(mean_arrival_delay_df$mean_arrival_delay)

arrange(mean_arrival_delay_df, mean_arrival_delay)




# srednie opoznienie pociagow wg dnia i godziny
mean_arrival_delay_df <- df_filltered %>%
  # moment przyjazdu rozdzielamy na składowe czasu i daty
  mutate(ar_year = year(arrival_time),
         ar_month = month(arrival_time),
         ar_day = day(arrival_time),
         ar_hour = hour(arrival_time)) %>%
  # dla każdej z godzin liczymy średnie opóźnienia wszystkich pociągów na wszystkich stacjach
  group_by(ar_year, ar_month, ar_day, ar_hour) %>%
  summarise(mean_arrival_delay = mean(arrival_delay)) %>%
  ungroup() %>%
  # wynik wciągamy do pamięci - tego niżej nie zrobimy łatwo w Sparku
  collect() %>%
  # składamy datę w jedno
  mutate(ar_date = make_date(ar_year, ar_month, ar_day)) %>%
  # rozpoznajemy dzień tygodnia
  mutate(ar_wday = wday(ar_date, week_start = 1,
                        abbr = FALSE, label = TRUE, locale = "pl_PL.UTF8"))


mean_arrival_delay_df %>%
  group_by(ar_date) %>%
  summarise(mean_arrival_delay = mean(mean_arrival_delay)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(ar_date, mean_arrival_delay))



mean_arrival_delay_df %>%
  group_by(ar_wday, ar_hour) %>%
  summarise(mean_arrival_delay = mean(mean_arrival_delay)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(ar_wday, ar_hour, fill = mean_arrival_delay), color = "gray50") +
  scale_y_reverse() +
  scale_fill_gradient2(low = "green", mid = "white", high = "red", midpoint = 0)





df_filltered %>%
  group_by(train_id, train_name) %>%
  summarise(ma = mean(arrival_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  collect() %>%
  top_n(10, ma) %>%
  arrange(desc(ma))


df %>%
  filter(train_id == 2759) %>%
  collect()


df %>%
  group_by(station_name) %>%
  summarise(ma = mean(arrival_delay, na.rm = TRUE)) %>%
  ungroup() %>%
  collect() %>%
  top_n(10, ma) %>%
  arrange(desc(ma))



# kolejnosc stacji dla kazdego z pociagow:
trasy <- df %>%
  # jesli nie ma czasu przyjazdu to jest taki sam jak odjazdu - to dzieje sie na stacji poczatkowej
  mutate(arrival_time = if_else(is.na(arrival_time), departure_time, arrival_time)) %>%
  # dla kazdego pociagu w kazdym dniu
  group_by(train_id, schedule_date) %>%
  # ustawiamy kolejnosc wg czasu przyjazdu (wiec w efekcie wg kolejnosci stacji)
  arrange(train_id, arrival_time) %>%
  # nadajemy numerki kolejno
  mutate(n = row_number()) %>%
  ungroup() %>%
  # wybieramy unikalne wiersze z nazwa pociagu, kolejnym numerem i nazwa stacji
  distinct(train_id, n, station_name) %>%
  # sortujemy wg numerow (w ramach nazwy pociagu)
  arrange(train_id, n) %>%
  collect()


# relacje pociagow:
relacje <- trasy %>%
  group_by(train_id) %>%
  filter(n == max(n) | n == min(n)) %>%
  mutate(direction = if_else(n == min(n), "from", "to")) %>%
  ungroup() %>%
  select(-n) %>%
  distinct(train_id, direction, .keep_all = TRUE) %>%
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
  rename(from_lon = long, from_lat = lat) %>%
  left_join(polozenie_stacji, by = c("to" = "station_1")) %>%
  rename(to_lon = long, to_lat = lat) %>%
  drop_na()


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
  distinct() %>%
  top_n(40, n)




relacje_n_plot %>%
  ggplot() +
  geom_polygon(data = map_data("world", region = "Poland"),
               aes(long, lat, group=group), color = "red", fill = "white") +
  geom_curve(aes(x = from_lon, y = from_lat,
                 xend = to_lon, yend = to_lat,
                 alpha = n, size = n)) +
  geom_point(data = top_cities, aes(long, lat, size = n )) +
  geom_text(data = top_cities, aes(long, lat, label = city )) +
  scale_alpha_continuous(range = c(0.1, 0.6)) +
  scale_size_continuous(range = c(0.1, 0.6)) +
  coord_quickmap()


spark_disconnect_all()


library(igraph)


g <- graph_from_data_frame(relacje_n, directed = TRUE)
E(g)$width = relacje_n$n


plot(g,
     vertex.size = 3,
     vertex.label.cex = 0.8,
     edge.width = 5*E(g)$width/max(E(g)$width),
     edge.arrow.size = 0)


library(networkD3)

# przygotowanie interaktywnego grafu
D3_network_LM <- simpleNetwork(relacje_n,
                               Source = "from", Target = "to",
                               linkColour = "#cbcbcb", nodeColour = "#000000",
                               opacity = 0.6, zoom = TRUE,
                               fontSize = 15, fontFamily = "sans-serif",
                               height = 600, width = 700)


degree(g, mode = "total") %>%
  sort(decreasing = TRUE) %>%
  .[1:30] %>%
  as.data.frame() %>%
  rownames_to_column("stacja") %>%
  set_names(c("stacja", "stopien")) %>%
  arrange(stopien) %>%
  mutate(stacja = fct_inorder(stacja)) %>%
  ggplot() +
  geom_col(aes(stacja, stopien), fill = "lightgreen", color = "darkgreen") +
  geom_text(aes(stacja, stopien, label = stopien), color = "darkgreen", hjust = "right") +
  coord_flip()





# kolejnosc stacji dla kazdego z pociagow:
dane <- df %>%
  mutate(arrival_time = if_else(is.na(arrival_time), departure_time, arrival_time)) %>%
  group_by(schedule_id) %>%
  # ustawiamy kolejnosc wg czasu przyjazdu (wiec w efekcie wg kolejnosci stacji)
  arrange(arrival_time) %>%
  # nadajemy numerki kolejno
  mutate(n = row_number()) %>%
  ungroup() %>%
  select(train_name, n, station_name) %>%
  collect()
