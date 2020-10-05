library(tidyverse)
library(RMySQL)
library(lubridate)
library(gridExtra)
library(grid)
library(glue)

Sys.setlocale("LC_CTYPE", "pl_PL.UTF-8")

dbname = "test_db"
user = "lemur"
password = "pr0cma1l"
host = "localhost"

sterownik <- dbDriver("MySQL")
polaczenie <- dbConnect(sterownik, dbname = dbname, user = user, password = password, host = host)

dbSendQuery(polaczenie, "SET NAMES utf8;")
dbSendQuery(polaczenie, "SET CHARACTER SET utf8;")
dbSendQuery(polaczenie, "SET character_set_connection=utf8;")


dane <- dbGetQuery(polaczenie, 'SELECT "majonez" AS typ, majonez AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY majonez
UNION
SELECT "ketchup" AS typ, ketchup AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY ketchup
UNION
SELECT "ekstraklasa" AS typ, ekstraklasa AS nazwa, COUNT(*) AS liczba FROM regionalizmy GROUP BY ekstraklasa
UNION
SELECT "wszystkie" AS typ, "wszystkie" AS nazwa, COUNT(*) AS liczba FROM regionalizmy;
')

lokalizacje <- dbGetQuery(polaczenie, 'SELECT `timestamp`, `long`, lat FROM regionalizmy;') %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%
  mutate(data = as_date(timestamp),
         hour = hour(timestamp))

liga <- dbGetQuery(polaczenie, 'SELECT ekstraklasa, `long`, lat FROM regionalizmy;')
ketchup <- dbGetQuery(polaczenie, 'SELECT ketchup, `long`, lat FROM regionalizmy;')
majonez <- dbGetQuery(polaczenie, 'SELECT majonez, `long`, lat FROM regionalizmy;')

all_data <-  dbGetQuery(polaczenie, 'SELECT * FROM regionalizmy;')

dbDisconnect(polaczenie)


(n_glosow <- dane %>% filter(typ == "wszystkie") %>% pull(liczba))


p1 <- dane %>%
  filter(typ == "majonez") %>%
  arrange(liczba, nazwa) %>%
  mutate(nazwa = fct_inorder(nazwa)) %>%
  ggplot() +
  geom_col(aes(nazwa, liczba), fill = "lightgreen") +
  geom_text(aes(nazwa, liczba, label = liczba), hjust = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Najpopularniejszy majonez",
       x = "", y = "")


p2 <- dane %>%
  filter(typ == "ketchup") %>%
  arrange(liczba, nazwa) %>%
  mutate(nazwa = fct_inorder(nazwa)) %>%
  ggplot() +
  geom_col(aes(nazwa, liczba), fill = "lightgreen") +
  geom_text(aes(nazwa, liczba, label = liczba), hjust = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Najpopularniejszy ketchup",
       x = "", y = "")


p3 <- dane %>%
  filter(typ == "ekstraklasa", nazwa != "żadnej") %>%
  arrange(liczba, nazwa) %>%
  mutate(nazwa = fct_inorder(nazwa)) %>%
  ggplot() +
  geom_col(aes(nazwa, liczba), fill = "lightgreen") +
  geom_text(aes(nazwa, liczba, label = liczba), hjust = 1) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Najpopularniejsza drużyna Ekstraklasy",
       x = "", y = "")




poland_map <- map_data('world') %>% filter(region == "Poland")

p4 <- ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_point(data = lokalizacje %>%
               filter(long >= min(poland_map$long), long <= max(poland_map$long),
                      lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
             aes(long, lat), alpha = 0.5) +
  coord_quickmap() +
  theme_void() +
  labs(title = "Skąd pochodzą głosy?",
       x = "" , y = "")


p5 <- lokalizacje %>%
  mutate(time = floor_date(timestamp, unit = "15 minutes")) %>%
  count(time) %>%
  ggplot() +
  geom_col(aes(time, n)) +
  theme_minimal() +
  labs(title = "Liczba głosów w czasie",
       x = "", y = "")



grid.arrange(p1,p2,p3,p4,p5,
             layout_matrix = rbind(c(1,4,4),
                                   c(2,4,4),
                                   c(3,5,5)),
             bottom = glue("Dane zebrane z {n_glosow} głosów oddanych w ankiecie na stronie https://shiny.prokulski.science/regiony/"))




lokalizacje %>%
  count(data) %>%
  ggplot() +
  geom_col(aes(data, n)) +
  theme_minimal()


lokalizacje %>%
  count(data, hour) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(data, hour, fill = n)) +
  scale_y_reverse() +
  theme_minimal()


lokalizacje %>%
  count(data, hour) %>%
  ungroup() %>%
  group_by(data) %>%
  mutate(d_proc = 100*n/sum(n)) %>%
  ungroup() %>%
  mutate(d_proc_cut = cut(d_proc, breaks = seq(0, 100, 10))) %>%
  ggplot() +
  geom_tile(aes(data, hour, fill = d_proc_cut)) +
  scale_y_reverse() +
  theme_minimal()



ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_point(data = liga %>%
               filter(long >= min(poland_map$long), long <= max(poland_map$long),
                      lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
             aes(long, lat, color = ekstraklasa)) +
  coord_quickmap() +
  theme_minimal() +
  facet_wrap(~ekstraklasa)


ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_density_2d(data = liga %>%
                    filter(long >= min(poland_map$long), long <= max(poland_map$long),
                           lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
                  aes(long, lat, color = ..level..), bins = 5) +
  coord_quickmap() +
  theme_minimal() +
  facet_wrap(~ekstraklasa)



ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_point(data = liga %>%
               filter(ekstraklasa != "żadnej") %>%
               filter(long >= min(poland_map$long), long <= max(poland_map$long),
                      lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
             aes(long, lat, color = ekstraklasa), show.legend = FALSE) +
  coord_quickmap() +
  theme_void() +
  facet_wrap(~ekstraklasa)



ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_point(data = majonez %>%
               filter(long >= min(poland_map$long), long <= max(poland_map$long),
                      lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
             aes(long, lat, color = majonez), show.legend = FALSE) +
  coord_quickmap() +
  facet_wrap(~majonez) +
  labs(title = "Popularność majonezu w regionach", x = "", y = "") +
  theme_void()


ggplot() +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = "white", color = "black") +
  geom_point(data = ketchup %>%
               filter(long >= min(poland_map$long), long <= max(poland_map$long),
                      lat >= min(poland_map$lat), lat <= max(poland_map$lat)),
             aes(long, lat, color = ketchup), show.legend = FALSE) +
  coord_quickmap() +
  facet_wrap(~ketchup) +
  labs(title = "Popularność ketchupu w regionach", x = "", y = "") +
  theme_void()






all_data %>%
  filter(ekstraklasa != "żadnej", majonez != "ten, który jest") %>%
  count(ekstraklasa, majonez) %>%
  ggplot() +
  geom_tile(aes(ekstraklasa, majonez, fill = n), color = "gray50", show.legend = FALSE) +
  geom_text(aes(ekstraklasa, majonez, label = n)) +
  coord_flip() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_minimal() +
  labs(title = "Majonez a drużyna piłkarska", x = "", y = "")


all_data %>%
  filter(ekstraklasa != "żadnej", ketchup != "ten, który jest") %>%
  count(ekstraklasa, ketchup) %>%
  ggplot() +
  geom_tile(aes(ekstraklasa, ketchup, fill = n), color = "gray50", show.legend = FALSE) +
  geom_text(aes(ekstraklasa, ketchup, label = n)) +
  coord_flip() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_minimal() +
  labs(title = "Ketchup a drużyna piłkarska", x = "", y = "")

p6 <- all_data %>%
  count(majonez, ketchup) %>%
  ggplot() +
  geom_tile(aes(majonez, ketchup, fill = n), color = "gray50", show.legend = FALSE) +
  geom_text(aes(majonez, ketchup, label = n)) +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  theme_minimal() +
  labs(title = "Ketchup a majonez") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6))

p6


grid.arrange(p1,p2,p3,p4,p6,
             layout_matrix = rbind(c(4,4,5,5),
                                   c(4,4,5,5),
                                   c(1,2,3,3)),
             bottom = glue("Dane zebrane z {n_glosow} głosów oddanych w ankiecie na stronie https://shiny.prokulski.science/regiony/"))



library(kknn)

liga <- liga %>%
  filter(ekstraklasa != "żadnej") %>%
  mutate(ekstraklasa = if_else(ekstraklasa == "Jagiellonia Białysto", "Jagiellonia Białystok", ekstraklasa)) %>%
  filter(long >= min(poland_map$long), long <= max(poland_map$long),
         lat >= min(poland_map$lat), lat <= max(poland_map$lat)) %>%
  mutate(ekstraklasa = as.factor(ekstraklasa))

# siatka punktów w Polsce
poland_grid <- expand.grid(long = seq(min(poland_map$long), max(poland_map$long), 0.05),
                           lat = seq(min(poland_map$lat), max(poland_map$lat), 0.05))


model <- kknn(ekstraklasa~., liga, poland_grid, distance = 1, kernel = "gaussian")

poland_grid <- poland_grid %>%
  mutate(fit = fitted(model))


ggplot() +
  geom_point(data = poland_grid, aes(long, lat, color = fit), alpha = 0.6, size = 0.1, show.legend = FALSE) +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = NA, color = "black") +
  coord_quickmap() +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  facet_wrap(~fit) +
  labs(title = "Popularność drużyn Ekstraklasy na terenie Polski",
       x = "", y = "")


ggplot() +
  geom_jitter(data = poland_grid, aes(long, lat, color = fit), alpha = 0.6) +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = NA, color = "black") +
  coord_quickmap() +
  theme_minimal()




dither_map <- bind_cols(poland_grid %>% select(long, lat),
                        model$prob %>% as_data_frame()) %>%
  gather(key = "druzyna", val = "prob", -long, -lat) %>%
  filter(prob != 0)


ggplot() +
  geom_jitter(data = dither_map,
             aes(long, lat, color = druzyna, alpha = prob)) +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = NA, color = "black") +
  scale_alpha_continuous(range = c(0, 0.7)) +
  coord_quickmap() +
  theme_minimal()


ggplot() +
  geom_jitter(data = dither_map ,
              aes(long, lat, color = druzyna, alpha = prob)) +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = NA, color = "black") +
  scale_alpha_continuous(range = c(0, 0.7)) +
  coord_quickmap() +
  theme_minimal() +
  facet_wrap(~druzyna)



library(fpc)
dbs_data <- liga %>%
  filter(ekstraklasa == "Legia Warszawa") %>%
  select(long, lat) %>%
  filter(long >= min(poland_map$long), long <= max(poland_map$long),
         lat >= min(poland_map$lat), lat <= max(poland_map$lat))

min_samples <- max(round(0.1*nrow(dbs_data)), 1) # co najmniej 10% punktów tworzy klaster, ale o najmniej 1

epsilon <- 50 / 6371.0088 # kms_per_radian

dbs <- dbscan(dbs_data, eps = epsilon , MinPts = min_samples, method = "hybrid")

dbs_df <- dbs_data %>% mutate(cl = dbs$cluster)

dbs_df %>%
  ggplot() +
  geom_point(aes(long, lat, color = as.factor(cl))) +
  geom_polygon(data = poland_map, aes(long, lat, group=group), fill = NA, color = "black") +
  coord_quickmap() +
  theme_minimal()
