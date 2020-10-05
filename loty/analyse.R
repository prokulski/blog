library(tidyverse)
library(lubridate)

# poprawnione ręcznie dane
df <- read_csv2("dane/zapis.csv")

# położenie lotnisk z http://ourairports.com/
lotniska <- read_csv("dane/airports.csv")


# przygotowanie danych
loty <- df %>%
  # rozdzielenie tracy na kolejne lotniska
  mutate(trasa = str_split(trasa, "-")) %>%
  unnest(trasa) %>%
  select(data, kto, lotnisko_z=trasa) %>%
  # przesuwamy o jeden lotniska
  group_by(kto, data) %>%
  mutate(lotnisko_do = lead(lotnisko_z)) %>%
  filter(!is.na(lotnisko_do)) %>%
  filter(lotnisko_do != lotnisko_z) %>%
  ungroup() %>%
  # dodajemy położenie dla lotniska źródłowego i docelowego
  left_join(lotniska %>%
              select(ident, lat_z=latitude_deg, long_z=longitude_deg, miasto_z=municipality),
            by = c("lotnisko_z" = "ident")) %>%
  left_join(lotniska %>%
              select(ident, lat_do=latitude_deg, long_do=longitude_deg, miasto_do=municipality),
            by = c("lotnisko_do" = "ident")) %>%
  # wybieramy potrzebne kolumny
  select(kto, data, lotnisko_z, lotnisko_do, lat_z, long_z, miasto_z, lat_do, long_do, miasto_do) %>%
  # usuwamy niedopasowane lotniska - uwaga: tracimy część danych!
  na.omit() %>%
  # budujemy trasę przelotu
  mutate(lot = paste0(miasto_z, " - ", miasto_do)) %>%
  # poprawiamy format daty
  rowwise() %>%
  mutate(data2 = str_match(data, "(\\d{2}\\.\\d{2}\\.\\d{4})")[[1,1]] %>% dmy()) %>%
  ungroup() %>%
  mutate(kto = toupper(kto))


# najpopularniejsze trasy (3/4 wszystkich dla każdego z premierów)
loty %>%
  count(kto, lot) %>%
  group_by(kto) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(procent_lotow = 100*n/total) %>%
  arrange(kto, desc(procent_lotow)) %>%
  group_by(kto) %>%
  mutate(total = cumsum(procent_lotow)) %>%
  filter(total <= 75) %>%
  ungroup() %>%
  select(kto, lot, n, procent_lotow, total)


lotniska_pl <- lotniska %>% filter(iso_country == "PL") %>% pull(ident)

# gdzie latają po Polsce?
loty %>%
  filter(lotnisko_z %in% lotniska_pl & lotnisko_do %in% lotniska_pl) %>%
  count(kto, lat_z, long_z, miasto_z, lat_do, long_do, miasto_do) %>%
  group_by(kto) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_polygon(data = map_data("world") %>%
                 filter(region == "Poland"),
               aes(long, lat, group=group),
               color = "gray30", fill = "gray90") +
  geom_segment(aes(x = long_z, xend = long_do,
                   y = lat_z, yend = lat_do,
                   size = n),
               alpha = 0.25, color = "red",
               show.legend = FALSE) +
  geom_point(data = lotniska %>% filter(iso_country == "PL") %>%
               filter(ident %in% loty$lotnisko_z | ident %in% loty$lotnisko_do),
             aes(longitude_deg, latitude_deg),
             size = 1) +
  geom_text(data = lotniska %>% filter(iso_country == "PL") %>%
              filter(ident %in% loty$lotnisko_z | ident %in% loty$lotnisko_do),
            aes(longitude_deg, latitude_deg, label = municipality),
            size = 2, hjust = -0.1, vjust = 0) +
  xlim(13, 25) +
  ylim(49, 55) +
  coord_map() +
  scale_size_continuous(range=c(0.5, 3)) +
  facet_wrap(~kto) +
  theme_minimal() +
  theme(axis.text = element_blank(), panel.grid = element_blank()) +
  labs(x = "", y = "")


# gdzie latają po Europie?
loty %>%
  count(kto, lat_z, long_z, miasto_z, lat_do, long_do, miasto_do) %>%
  mutate(kto = toupper(kto)) %>%
  group_by(kto) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  borders("world", xlim = c(-10, 25), ylim = c(39, 70), fill = "gray90") +
  geom_segment(aes(x = long_z, xend = long_do,
                   y = lat_z, yend = lat_do,
                   size = n),
               alpha = 0.25, color = "red",
               show.legend = FALSE) +
  coord_map() +
  scale_size_continuous(range=c(0.5, 3)) +
  facet_wrap(~kto) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "", y = "")



# ile raz w miesiącu latają?
loty %>%
  mutate(data2 = floor_date(data2, "month")) %>%
  count(kto, data2) %>%
  ggplot() +
  geom_col(aes(data2, n, fill = kto), show.legend = FALSE) +
  geom_smooth(aes(data2, n, color = kto), show.legend = FALSE) +
  facet_wrap(~kto, scales = "free_x") +
  scale_x_date(date_labels = "%m/%Y") +
  theme_minimal() +
  labs(x = "", y = "Liczba lotów w miesiącu")


df2 <- read_csv2("dane/zapis_ok.csv") %>% mutate(data = dmy(data))


loty %>%
  mutate(wday = wday(data2, week_start = 1, label = TRUE)) %>%
  count(kto, wday, lot) %>%
  filter(n > 2) %>%
  ggplot() +
  geom_tile(aes(wday, lot, fill = n), color = "gray", show.legend = FALSE) +
  geom_text(aes(wday, lot, label = n)) +
  scale_fill_distiller(palette = "YlOrBr", direction = -1) +
  facet_wrap(~kto, scales = "free") +
  labs(x ="", y = "",
       title = "Najczęstsze trasy przelotów w poszczególnych dniach tygodnia") +
  theme_minimal()


left_join(loty, df2, by = c("kto" = "premier", "data2" = "data"))  %>%
  mutate(wday = wday(data2, week_start = 1, label = TRUE)) %>%
  filter(samolot == "CASA", kto == "PBS") %>%
  count(kto, wday, lot) %>%
  ggplot() +
  geom_tile(aes(wday, lot, fill = n), color = "gray", show.legend = FALSE) +
  geom_text(aes(wday, lot, label = n)) +
  scale_fill_distiller(palette = "YlOrBr", direction = -1) +
  labs(x ="", y = "",
       title = "Najczęstsze trasy przelotów w poszczególnych dniach tygodnia") +
  theme_minimal()



library(igraph)

loty_cnt <- loty %>% count(kto, miasto_z, miasto_do) %>% set_names(c("kto", "from", "to", "weight"))

loty_pbs <- loty_cnt %>% filter(kto == "PBS") %>% select(-kto)

loty_pbs_gpr <- graph_from_data_frame(loty_pbs, directed = TRUE)

E(loty_pbs_grp)$weight <- loty_pbs$weight

plot(loty_pbs_grp,
     vertex.size = 5,
     vertex.label.cex = 0.7,
     edge.width = scales::squish(E(loty_pbs_grp)$weight, range = c(1, 10)),
     edge.label = E(loty_pbs_grp)$weight,
     edge.arrow.size = 0.1,
     edge.arrow.width = 0.1,
     edge.curved = TRUE,
     layout = layout_with_fr)



