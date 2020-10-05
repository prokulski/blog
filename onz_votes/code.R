library(tidyverse)
library(lubridate)
library(widyr) # devtools::install_github("dgrtwo/widyr")

# United Nations voting data
library(unvotes)

# un_votes
#
# un_roll_calls
#
# un_roll_call_issues


#### SLOWNIKI ----

# kraje Unii Europejskiej
UE_countries_1 <- c("Austria", "Belgium", "Cyprus", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Italy", "Luxembourg", "Malta", "Netherlands", "Portugal", "Spain", "Sweden", "United Kingdom of Great Britain and Northern Ireland")
UE_countries_2 <- c(UE_countries_1, "Bulgaria", "Croatia", "Czech Republic", "Estonia", "Hungary", "Latvia", "Lithuania", "Romania", "Slovakia", "Slovenia")

# kraje NATO
NATO_countries_1 <- c("Belgium","Canada", "Denmark", "France", "Greece", "Iceland", "Italy", "Luxembourg", "Netherlands", "Norway", "Portugal", "Spain", "United Kingdom of Great Britain and Northern Ireland", "United States of America", "Turkey", "Federal Republic of Germany", "Germany")
NATO_countries_2 <- c(NATO_countries_1, "Czech Republic", "Hungary", "Bulgaria", "Estonia", "Latvia", "Lithuania","Romania", "Slovakia", "Slovenia", "Albania", "Croatia", "Montenegro")

# kraje Układu Warszawskiego
WarsawPact_countries <- c("Albania", "Bulgaria", "Czechoslovakia", "German Democratic Republic", "Hungary", "Romania", "Russian Federation")

# pozostałe kraje
other_countries <- c("Afghanistan", "Argentina", "Australia", "Brazil", "China", "Cuba", "Democratic People's Republic of Korea", "India", "Iran (Islamic Republic of)", "Iraq", "Israel", "Japan", "Kuwait", "Libya", "Madagascar", "Mexico", "Mongolia", "Morocco", "Mozambique", "New Zealand", "Pakistan", "Republic of Korea", "Saudi Arabia", "South Africa", "Switzerland", "Turkey", "Yugoslavia")


# wszystkie głosowania uzupełnione o datę
un_votes_date <- inner_join(un_votes %>%
                              select(-country_code),
                            un_roll_calls %>%
                              select(rcid, date),
                            by = "rcid")



#### POLSKA - daty historyczne ----

# PRL kończy się 4 czerwca 1989
# Polska jest członkiem NATO od 12 marca 1999
# Polska jest członkiem Unii Europejskiej od 1 maja 2004

# przypisujemy odpowiednie flagi dla powyższych okresów
un_votes_date <- un_votes_date %>%
  mutate(prl = date <= as_date("1989-06-04"),
         nato = date >= as_date("1999-03-12"),
         ue = date >= as_date("2004-05-01"))


# zobaczmy te okresy na osi czasu
un_votes_date %>%
  select(date:ue) %>%
  group_by(date) %>%
  distinct() %>%
  ungroup()  %>%
  gather("key", "val", -date) %>%
  mutate(key = factor(toupper(key), levels = rev(c("PRL", "NATO", "UE")))) %>%
  ggplot(aes(date, key)) +
  geom_point(aes(color = val)) +
  geom_vline(xintercept = as_date(c("1989-06-04", "1999-03-12", "2004-05-01"))) +
  annotate("label", x = as_date(c("1989-06-04", "1999-03-12", "2004-05-01")), y = c(3.3, 2.3, 1.3),
           label = c("Wybory 1989", "Polska w NATO", "Polska w UE"), color = "gray40", size = 3) +
  scale_color_manual(values = c("TRUE" = "#006837", "FALSE" = "#d73027"),
                     labels = c("TRUE" = "tak", "FALSE" = "nie")) +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", color = "")


# Wszystko się zgadza


#### POLSKA vs NATO ----

# jak głosowaliśmy w PRL - Układ Warszawski vs NATO?

# wybieramy kraje z NATO (przed przystąpieniem Polski) i Układu Warszawskiego
prl_nato <- un_votes_date %>%
  filter(!nato) %>%
  filter(country %in% c(NATO_countries_1, WarsawPact_countries, other_countries , "Poland")) %>%
  filter(country != "Germany") %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, sort = TRUE, upper = FALSE)


prl_nato <- bind_rows(
  prl_nato %>% filter(item1 == "Poland") %>% select(country = item2, correlation),
  prl_nato %>% filter(item2 == "Poland") %>% select(country = item1, correlation)
) %>%
  mutate(nato = case_when(
    country == "Russian Federation" ~ "Rosja",
    country %in% NATO_countries_1 ~ "NATO",
    country %in% WarsawPact_countries ~ "Układ Warszawski",
    TRUE ~ "inne"))

prl_nato %>%
  mutate(country = reorder(country, correlation)) %>%
  mutate(nato = factor(nato, levels = c("NATO", "Rosja", "Układ Warszawski", "inne"))) %>%
  ggplot(aes(country, correlation)) +
  geom_col(aes(fill = nato), color = "gray30", size = 0.1) +
  scale_fill_manual(values = c("NATO" = "#4575b4",
                               "Układ Warszawski" = "#fdae61",
                               "inne" = "#a6d96a",
                               "Rosja" = "#d73027")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie przed wstąpieniem Polski do NATO",
       x = "", y = "współczynnik korelacji", fill = "")


# a jak wstąpiliśmy do NATO?
polska_nato <- un_votes_date %>%
  filter(nato) %>%
  filter(country %in% c(NATO_countries_2, WarsawPact_countries, other_countries , "Poland")) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, sort = TRUE, upper = FALSE)


polska_nato <- bind_rows(
  polska_nato %>% filter(item1 == "Poland") %>% select(country = item2, correlation),
  polska_nato %>% filter(item2 == "Poland") %>% select(country = item1, correlation)
) %>%
  mutate(nato = case_when(
    country == "Russian Federation" ~ "Rosja",
    country %in% NATO_countries_1 ~ "NATO",
    country %in% WarsawPact_countries ~ "dawny Układ Warszawski",
    TRUE ~ "inne"))

polska_nato %>%
  mutate(country = reorder(country, correlation)) %>%
  mutate(nato = factor(nato, levels = c("NATO", "Rosja", "dawny Układ Warszawski", "inne"))) %>%
  ggplot(aes(country, correlation)) +
  geom_col(aes(fill = nato), color = "gray30", size = 0.1) +
  scale_fill_manual(values = c("NATO" = "#4575b4",
                               "dawny Układ Warszawski" = "#fdae61",
                               "inne" = "#a6d96a",
                               "Rosja" = "#d73027")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie po wstąpieniu Polski do NATO",
       x = "", y = "współczynnik korelacji", fill = "Czy państwo było w:")




# gdzie zaszła najwięjsza różnica?
nato_delta <- inner_join(prl_nato, polska_nato, by = c("country")) %>%
  mutate(delta = correlation.y - correlation.x) %>%
  select(country, delta) %>%
  arrange(delta)

nato_delta

nato_delta <- nato_delta %>%
  mutate(country = fct_inorder(country)) %>%
  gather("key", "value", -country)


ggplot() +
  geom_col(data = nato_delta %>% filter(key == "delta"),
           aes(country, value), fill = "lightgreen") +
  geom_point(data = nato_delta %>% filter(key != "delta"),
             aes(country, value, color = key)) +
  scale_color_manual(values = c("pl_nato" = "red", "prl_nato" = "blue")) +
  coord_flip()


# Polska w NATO - podobieństwo na mapie

polska_nato_2 <- un_votes_date %>%
  filter(nato) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, sort = TRUE, upper = FALSE)

plot_data <- bind_rows(
  polska_nato_2 %>% filter(item1 == "Poland") %>% select(country = item2, correlation),
  polska_nato_2 %>% filter(item2 == "Poland") %>% select(country = item1, correlation)
)

world_map <- map_data("world") %>%
  mutate(region = case_when(
    region == "Bolivia" ~ "Bolivia (Plurinational State of)",
    region == "Republic of Congo" ~ "Congo",
    region == "UK" ~ "United Kingdom of Great Britain and Northern Ireland",
    region == "Iran" ~ "Iran (Islamic Republic of)",
    region == "South Korea" ~ "Republic of Korea",
    region == "Laos" ~ "Lao People's Democratic Republic",
    region == "Moldova" ~ "Republic of Moldova",
    region == "Macedonia" ~ "The former Yugoslav Republic of Macedonia",
    region == "North Korea" ~ "Democratic People's Republic of Korea",
    region == "Russia" ~ "Russian Federation",
    region == "Syria" ~ "Syrian Arab Republic",
    region == "USA" ~ "United States of America",
    region == "Venezuela" ~ "Venezuela, Bolivarian Republic of",
    TRUE ~ region
  ))

plot_data <- left_join(world_map,
          plot_data,
          by = c("region"="country")) %>%
  mutate(correlation_cluster = cut(correlation, c(-2, -0.75, 0, 0.75, 2)))


plot_data %>%
  ggplot() +
  geom_polygon(aes(long, lat,
                   group=group, fill = correlation),
               color = "gray30", size = 0.1) +
  scale_fill_distiller(palette = "RdYlGn", na.value = "white") +
  coord_quickmap() +
  theme(panel.background = element_rect(fill = "#abd9e9"),
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie po wstąpieniu Polski do NATO",
       x = "", y = "", fill = "Współczynnik korelacji głosowań:")



# podobieństwo pogrupowane
plot_data %>%
  ggplot() +
  geom_polygon(aes(long, lat,
                   group = group, fill = correlation_cluster),
               color = "gray30", size = 0.1) +
  scale_fill_manual(values = c("(-0.75,0]" = "#1a9850",
                               "(0,0.75]" = "#fee08b",
                               "(0.75,2]" = "#d73027",
                               "NA" = "white"), na.value = "white") +
  coord_quickmap() +
  theme(panel.background = element_rect(fill = "#abd9e9"),
        axis.text = element_blank(),
        legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie po wstąpieniu Polski do NATO",
       x = "", y = "", fill = "Współczynnik korelacji głosowań:")



# jak zmieniało sie w czasie podobieństwo?

un_votes_session <- inner_join(un_votes %>%
                                 select(-country_code),
                               un_roll_calls %>%
                                 select(rcid, session, date),
                               by = "rcid")

un_votes_session %>%
  filter(country %in% c("United States of America", "Russian Federation", "Germany", "Federal Republic of Germany" ,"Poland")) %>%
  mutate(country = case_when(
    country == "United States of America" ~ "USA",
    country == "Russian Federation" ~ "Russia",
    country == "Federal Republic of Germany" ~ "Germany",
    TRUE ~ country)) %>%
  mutate(vote = as.numeric(vote)) %>%
  spread(country, vote) %>%
  na.omit() %>%
  group_by(session) %>%
  summarise(date = mean(date),
            cor_USA = cor(Poland, USA),
            cor_Germany = cor(Poland, Germany),
            cor_Russia = cor(Poland, Russia)) %>%
  ungroup() %>%
  ggplot() +
  geom_vline(xintercept = as_date(c("1989-06-04", "1999-03-12", "2004-05-01")), color = "gray40") +
  geom_hline(yintercept = c(-0.5, 0, 0.5), color = "gray40") +
  geom_line(aes(date, cor_USA), color = "blue", size = 2) +
  geom_line(aes(date, cor_Germany), color = "green", size = 2) +
  geom_line(aes(date, cor_Russia), color = "red", size = 2) +
  geom_vline(xintercept = as_date(c("1989-06-04", "1999-03-12", "2004-05-01")), color = "gray40") +
  annotate("label", x = as_date(rep("1973-12-02", 3)),  y = c(0.95, -0.4, -0.05),
           label = c("Rosja", "Niemcy", "USA"), color = c("red", "green", "blue")) +
  annotate("label", x = as_date(c("1989-06-04", "1999-03-12", "2004-05-01")), y = c(-0.5, -0.5, -0.5),
           label = c("Wybory 1989", "Polska w NATO", "Polska w UE"), color = "gray40", size = 3) +
  labs(title = "Podobieństwo głosów oddanych przez Polskę w głosowaniach w ONZ z wybranymi krajami",
       x = "", y = "Współczynnik korelacji")






# jak głosowaliśmy - Unia Europejska

# wybieramy kraje z NATO (przed przystąpieniem Polski) i Układu Warszawskiego
prl_ue <- un_votes_date %>%
  filter(!ue) %>%
  filter(country %in% c(UE_countries_2, "Poland", "Russian Federation")) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, sort = TRUE, upper = FALSE)


bind_rows(
  prl_ue %>% filter(item1 == "Poland") %>% select(country = item2, correlation),
  prl_ue %>% filter(item2 == "Poland") %>% select(country = item1, correlation)
) %>%
  mutate(ue = case_when(
    country == "Russian Federation" ~ "Rosja",
    country %in% UE_countries_1 ~ "stare UE",
    country %in% UE_countries_2 ~ "nowe UE",
    TRUE ~ "poza UE")) %>%
  mutate(country = reorder(country, correlation)) %>%
  mutate(ue = factor(ue, levels =  c("stare UE", "nowe UE", "poza UE", "Rosja"))) %>%
  ggplot(aes(country, correlation)) +
  geom_col(aes(fill = ue), color = "gray30", size = 0.1) +
  scale_fill_manual(values = c("stare UE" = "#4575b4",
                               "nowe UE" = "#abd9e9",
                               "poza UE" = "#fee090",
                               "Rosja" = "#d73027")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie przed wstąpieniem Polski do UE",
       x = "", y = "współczynnik korelacji", fill = "")



# a jak wstąpiliśmy do UE?
polska_ue <- un_votes_date %>%
  filter(ue) %>%
  filter(country %in% c(UE_countries_2, "Poland", "Russian Federation")) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote, sort = TRUE, upper = FALSE)


bind_rows(
  polska_ue %>% filter(item1 == "Poland") %>% select(country = item2, correlation),
  polska_ue %>% filter(item2 == "Poland") %>% select(country = item1, correlation)
) %>%
  mutate(ue = case_when(
    country == "Russian Federation" ~ "Rosja",
    country %in% UE_countries_1 ~ "stare UE",
    country %in% UE_countries_2 ~ "nowe UE",
    TRUE ~ "poza UE")) %>%
  mutate(country = reorder(country, correlation)) %>%
  mutate(ue = factor(ue, levels =  c("stare UE", "nowe UE", "poza UE", "Rosja"))) %>%
  ggplot(aes(country, correlation)) +
  geom_col(aes(fill = ue), color = "gray30", size = 0.1) +
  scale_fill_manual(values = c("stare UE" = "#4575b4",
                               "nowe UE" = "#abd9e9",
                               "poza UE" = "#fee090",
                               "Rosja" = "#d73027")) +
  coord_flip() +
  theme(legend.position = "bottom") +
  labs(title = "Podobieństwo Polski do innych państw w głosowaniach w ONZ",
       subtitle = "W okresie po wstąpieniu Polski do UE",
       x = "", y = "współczynnik korelacji", fill = "")




ue_votes <- un_votes_date %>%
  filter(ue) %>%
  filter(vote != "abstain") %>%
  filter(country %in% c(UE_countries_2, "Poland")) %>%
  mutate(country = if_else(country == "United Kingdom of Great Britain and Northern Ireland", "UK", country)) %>%
  mutate(vote = as.numeric(vote)) %>%
  pairwise_cor(country, rcid, vote)

ue_votes %>%
  mutate(item2 = factor(item2, levels = sort(unique(item2), decreasing = TRUE))) %>%
  ggplot() +
  geom_tile(aes(item1, item2, fill = correlation), color = "gray30", size = 0.1) +
  scale_fill_distiller(palette = "RdYlGn", na.value = "white") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "bottom") +
  labs(title = "Podobieństwo państw UE w głosowaniach w ONZ",
       subtitle = "W okresie po wstąpieniu Polski do UE",
       x = "", y = "", fill = "współczynnik korelacji")


# które państwa są najbardziej podobne do siebie?
ue_votes %>%
  group_by(item1) %>%
  filter(correlation == max(correlation)) %>%
  ungroup() %>%
  arrange(item1) %>%
  set_names(c("Państwo", "Najbardziej podobne państwo", "Współczynnik korelacji")) %>%
  knitr::kable()


# a gdyby tak podzielić jakos państwa na grupy?

# 6, bo bierzemy pierwastek z liczby państw zaokrąglony do góry
ue_votes$km <- kmeans(ue_votes$correlation, 6)$cluster

ue_votes_clusters <- ue_votes %>%
  group_by(item1) %>%
  count(km) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup() %>%
  group_by(item1) %>%
  top_n(1, n) %>%
  ungroup()

ue_votes_clusters %>%
  select(km, item1, p) %>%
  arrange(km, item1) %>%
  set_names(c("Grupa", "Państwo", "Dopasowanie do grupy")) %>%
  knitr::kable()


left_join(world_map,
           ue_votes_clusters %>%
             mutate(item1 = if_else(item1 == "UK", "United Kingdom of Great Britain and Northern Ireland", item1)),
           by = c("region" = "item1")) %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = as.factor(km)), color = "black", size = 0.1, show.legend = FALSE) +
  scale_fill_viridis_d(option = "D", na.value = "gray80") +
  coord_quickmap(xlim = c(-15,40), ylim = c(35, 71)) +
  theme(panel.background = element_rect(fill = "#abd9e9"),
        axis.text = element_blank()) +
  labs(title = "Podobieństwo krajów UE do siebie",
       subtitle = "Na podstawie głosowań w ONZ, w okresie po wstąpieniu Polski do UE",
       x = "", y = "")



