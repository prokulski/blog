library(tidyverse)
library(glue)
library(tidytext)
library(wordcloud)
library(widyr)
library(sf)

pl_stop_words <- read_lines("~/RProjects/!polimorfologik/polish_stopwords.txt")

poland_map <- read_sf("~/RProjects/!mapy_shp/wojewodztwa.shp") %>%
  select(jpt_nazwa_, geometry)

# https://bdl.stat.gov.pl/BDL/metadane/metryka/2137
poland_ludnosc <- read_csv2("~/RProjects/!daneGUS/Ludnosc_wg_plci_wieku_2017.csv",
                     col_types = "cccc-i---",
                     col_names = c("TERYT", "nazwa", "wiek", "plec", "liczba"),
                     skip = 1) %>%
  filter(wiek %in% c( "20-24", "25-29", "30-34", "35-39",
                      "40-44", "45-49", "50-54", "55-59",
                      "60-64")) %>%
  filter(str_to_lower(nazwa) %in% poland_map$jpt_nazwa_) %>%
  group_by(nazwa) %>%
  summarise(liczba = sum(liczba)) %>%
  ungroup() %>%
  mutate(nazwa = str_to_lower(nazwa))


# szukamy najnowszego pliku ze zgromadzonymi danymi
offers <- list.files("data/") %>%
  sort(decreasing = TRUE) %>%
  .[[1]] %>%
  paste0("data/", .) %>%
  # i wczytujemy go
  readRDS()

print(max(offers$i))
print(nrow(offers))



# funkcja rozbija tagi
make_tag_list <- function(x) {
  str_split(x, ", ") %>% unlist() %>% unique()
}


# czyszczenie danych
offers <- offers %>%
  # poprawa pl-literek
  mutate(localization = str_replace_all(localization, "&#243;", "ó"),
         category = str_replace_all(category, "&#243;", "ó"),
         company = str_replace_all(company, "&#243;", "ó"),
         company = str_replace_all(company, "&#211;", "Ó"),
         company = str_replace_all(company, "&amp;", "&"),
         body = str_replace_all(body, "&amp;", "&")) %>%
  # dodanie kolumny z wojewodztwem
  mutate(woj = str_split(localization, ", ") %>%
           # wojewodztwo to ostatni czlon w lokalizaji
           map_chr(.f = function(x) { x[[length(x)]] })) %>%
  # dodanie kolumny listą tagów z kategorii
  mutate(tag = map(category, make_tag_list)) %>%
  mutate_at(.vars = vars(company, category, woj), .funs = str_trim)


# Najpopularniejsze kategorie ogłoszeń
offers %>%
  unnest(tag) %>%
  count(tag) %>%
  mutate(p = 100*n/sum(n)) %>%
  mutate(tag = fct_reorder(tag, n)) %>%
  ggplot() +
  geom_col(aes(tag, p)) +
  coord_flip()


# tylko polskie oferty
# wojewodztwa, ktore mają małą literę na początku
offers_pl <- offers %>%
  filter(str_sub(woj, 1, 1) != str_to_upper(str_sub(woj, 1, 1)) & woj != "zagranica")



# najpopularniejsza kategoria wg województwa

offers_tag_woj <- offers_pl %>%
  select(woj, tag) %>%
  unnest(tag) %>%
  count(woj, tag)

offers_tag_woj %>%
  group_by(woj) %>%
  mutate(n = 100*n/sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(woj, tag, fill = n)) +
  geom_text(aes(woj, tag, label = round(n, 1))) +
  scale_fill_distiller(palette = "YlOrBr") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  labs(title = "Najpopularniejsza branża w ogłoszeniach Pracuj.pl w zależności od województwa",
       subtitle = paste0("Na podstawie ", nrow(offers), " ogłoszeń"),
       x = "", y = "")

# Praca w IT - jaki obszar wg województwa
offers_tag_woj %>%
  filter(str_detect(tag, "IT - ")) %>%
  ggplot() +
  geom_col(aes(woj, n, fill = tag),
           position = position_dodge()) +
  coord_flip()


# liczba ofert pracy wg województwa
offers_pl %>%
  select(woj) %>%
  count(woj) %>%
  left_join(poland_map, by = c("woj" = "jpt_nazwa_")) %>%
  mutate(center = st_centroid(geometry)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = n), color = "gray75", show.legend = FALSE) +
  geom_sf_label(aes(geometry = center, label = n), color = "black", fill = "white") +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = "Liczba ofert pracy w Pracuj.pl według województwa",
       x = "", y = "")


# liczba ofert w przeliczeniu na liczbę mieszkańców
offers_count <- offers_pl %>%
  select(woj) %>%
  count(woj)

n_pl_offers <- sum(offers_count$n)

offers_count %>%
  left_join(poland_map, by = c("woj" = "jpt_nazwa_")) %>%
  left_join(poland_ludnosc,  by = c("woj" = "nazwa")) %>%
  mutate(p = 10000* n/liczba) %>%
  mutate(center = st_centroid(geometry)) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = p), color = "gray75", show.legend = FALSE) +
  geom_sf_label(aes(geometry = center,
                    label = sprintf("%d (%.2f)", n, p)),
                color = "black", fill = "white", size = 3) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = paste0("Liczba ofert pracy w Pracuj.pl według województwa\n(z ", n_pl_offers, " zgromadzonych ofert)"),
       subtitle = "Opis na mapie: liczba ofert (liczba ofert na 10 tys. mieszkańców w wieku 20-64 lata)",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy",
       x = "", y = "")


# Liczba ofert pracy w Pracuj.pl według tagów
offers_pl %>%
  select(woj, tag) %>%
  unnest(tag) %>%
  count(woj, tag) %>%
  left_join(poland_map, by = c("woj" = "jpt_nazwa_")) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = n), color = "gray75", show.legend = FALSE) +
  scale_fill_viridis_c(option = "B") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = "Liczba ofert pracy w Pracuj.pl według województwa w podziale na branże",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy",
       x = "", y = "") +
  facet_wrap(~tag)



# najpopularniejsze kategorie wg województw
offers_pl %>%
  select(woj, tag) %>%
  unnest(tag) %>%
  count(woj, tag) %>%
  group_by(woj) %>%
  mutate(n = 100*n/sum(n)) %>%
  top_n(3, n) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(tag, n)) +
  coord_flip() +
  facet_wrap(~woj, scales = "free_y")


# j/w bez najpopularniejszych branż
offers_pl %>%
  select(woj, tag) %>%
  unnest(tag) %>%
  count(woj, tag) %>%
  group_by(woj) %>%
  mutate(n = 100*n/sum(n)) %>%
  ungroup() %>%
  filter(!tag %in% c("Sprzedaż", "Obsługa klienta", "Praca fizyczna")) %>%
  group_by(woj) %>%
  top_n(3, n) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(tag, n)) +
  coord_flip() +
  facet_wrap(~woj, scales = "free_y")



# popularność słów w opisach
words <- offers %>%
  unnest_tokens("word", body, token = "words")

words_clean <- words %>%
  # pl stop words
  filter(!word %in% pl_stop_words) %>%
  # angielskie stop words
  filter(!word %in% stop_words$word) %>%
  # liczby
  filter(is.na(as.numeric(word)))

# najpopularniejsze słowa w ogłoszeniach
words_clean %>%
  count(word) %>%
  top_n(50, n) %>%
  arrange(n) %>%
  mutate(word = fct_inorder(word)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()


# słowa związane z machine learning
ml_tags <- c("tensorflow", "pytorch", "nlp", "lstm", "cnn",  "spark", "hadoop", "hive", "python")

# słowa związane z IT
technologie <- c("java", "javascript", "apache",
                 "sql", ".net", "git", "sharepoint", "asp.net", "asp", "angular", "oracle", "html", "mvc",
                 "web", "bpm", "css", "php", "svn", "xml", "visual", "azure", "windows", "linux",
                 "erp", "uml", "api", "flask", "aws", ml_tags)


words %>%
  filter(word %in% ml_tags) %>%
  count(word) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()


words %>%
  filter(word %in% technologie) %>%
  count(word) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()

# ogłoszenia ze słowami związanymi z ML
ml_ids <- words %>% filter(word %in% ml_tags) %>% distinct(i) %>% pull(i)

# z jakimi innymi słowami występują słowa związane z ML
words_clean %>%
  filter(i %in% ml_ids, word %in% technologie) %>%
  count(word) %>%
  top_n(50, n) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()

# bi gramy
biwords <- offers %>%
  select(grade, tag, body) %>%
  unnest() %>%
  mutate(body = str_replace_all(body, "\\.", " ")) %>%
  unnest_tokens("word", body, token = "ngrams", n = 2) %>%
  # rozdzielenie bigramu na słowa
  separate(word, c("word1", "word2"), sep = " ") %>%
  # polskie stop words
  filter(!word1 %in% pl_stop_words) %>%
  filter(!word2 %in% pl_stop_words) %>%
  # angielskie stop words
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  # liczby
  filter(is.na(as.numeric(word1))) %>%
  filter(is.na(as.numeric(word2))) %>%
  # zwłączenie w bigram
  unite(word, word1, word2, sep = " ")

# śmieciowe bigramy
bi_stops <- c("danych osobowych","miejsce pracy","na przetwarzanie","prosimy o", "personal data",
              "sp z","z o.o","zgodę na", "potrzeb rekrutacji", "twoich danych", "rekrutacji informujemy",
              "na stanowisko","w zakresie","wyrażam zgodę", "prawo dostępu",
              "zgodnie z","moich danych","doświadczenie w","z dnia", "osobowych cofnięcie",
              "pracy w","z siedzibą","dane osobowe","dla potrzeb",
              "zawartych w","w celu","w przycisk","siedzibą w",
              "osoby zainteresowane","klikając w","osobowych zawartych","o ochronie",
              "ochronie danych","procesu rekrutacji","umowę o","o przesyłanie",
              "na podstawie","zainteresowane prosimy","przycisk aplikowania",
              "w tym","do pracy","przesyłanie aplikacji","of the",
              "aplikacji klikając","do realizacji",
              "prawo do","przetwarzanie moich","w związku","w pracy",
              "związku z", "przetwarzanie danych",
              "udział w","przetwarzanie danych" ,
              "realizacji procesu","w zespole","z ustawą","przetwarzanie przez"  ,
              "w oparciu","możliwość rozwoju","oparciu o","na rynku",
              "ms office","niezbędnych do","informujemy że","w polsce",
              "osobowych jest","dz u","dostępu do","ustawą z",
              "potrzeb niezbędnych","in the","danych oraz","swoich danych",
              "na potrzeby","klauzuli wyrażam","2016 r",
              "przyszłych rekrutacji","rozwoju zawodowego",
              "u z","email protected","z wybranymi",
              "o.o z","w warszawie","r o", "ochrony danych", "wybranymi kandydatami", "polska sp",
              "potrzeby przyszłych", "przetwarzania danych" ,
              "przycisk aplikuj" , "ograniczenia przetwarzania", "nr ref" ,"zakresie określonym",
              "przepisami ustawy", "pomocą przycisku",  "prowadzenia rekrutacji"   ,"jednocześnie wyrażam",
              "rekrutacji zgodnie", "ofercie pracy", "podanie danych", "opis stanowiska", "administratorem danych",
              "zakres obowiązków", "przycisku aplikuj", "uprzejmie informujemy", "zgłoszeniu rekrutacyjnym",
              "następującej klauzuli"
)

biwords_cnt <- biwords %>%
  filter(!word %in% bi_stops) %>%
  count(tag, word)


biwords_cnt %>%
  group_by(word) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  top_n(50, n) %>%
  mutate(word = fct_reorder(word, n)) %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip()


# chmura bigramów per kategorię
by(biwords_cnt,
   biwords_cnt$tag,
   function(x) {
     wordcloud(x$word, x$n,
               max.words = 100,
               scale = c(2.5, 0.2),
               colors = RColorBrewer::brewer.pal(12, "Paired"))
     text(0.1, 0.95, unique(x$tag), col="black", cex=1.3,  adj = c(0,0))
   }
)

# liczymy korelację liczby wystąpień poszczególnych bigramów pomiędzy kategoriami
biwords_cnt %>%
  pairwise_cor(tag, word, n, upper = FALSE, diag = TRUE) %>%
  arrange(desc(item2)) %>%
  mutate(item2 = fct_inorder(item2)) %>%
  ggplot() +
  geom_tile(aes(item1, item2, fill = correlation), color = "gray75") +
  geom_text(aes(item1, item2, label = sprintf("%.2f", correlation),
                color = if_else(correlation < 0.55, "white", "black")),
            show.legend = FALSE) +
  scale_fill_viridis_c(option = "B") +
  scale_color_manual(values = c("white" = "white", "black" = "black")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
        legend.position = "none") +
  labs(title = "Podobieństwo ofert pracy w zależności od branży",
       subtitle = "Na postawie ogłoszeń z Pracuj.pl. Podobieństwo liczone jako korelacja liczy wystąpień bi-gramów",
       caption = "(c) Łukasz Prokulski, fb.com/DaneAnalizy",
       x = "", y = "", fill = "Współczynnik korelacji")




# najpopulatniejsze bigramy dla IT
biwords_cnt %>%
  filter(str_detect(tag, "IT - ")) %>%
  group_by(tag) %>%
  mutate(p = 100*n/sum(n)) %>%
  top_n(20, p) %>%
  ungroup() %>%
  arrange(p) %>%
  mutate(word = fct_inorder(word)) %>%
  ggplot() +
  geom_col(aes(word, p, fill = tag), position = position_dodge()) +
  coord_flip()


fragment <- function(f_string, f_pattern, f_size = 30) {
  f_string <- str_to_lower(f_string)
  f_pattern <- str_to_lower(f_pattern)

  loc <- str_locate_all(f_string, f_pattern)

  pad_all <- vector("character", length(f_string))

  for(i in 1:length(f_string)) {
    pad_all[i] <- str_sub(f_string[i],
            loc[i] %>% unlist() %>% .[1] - f_size,
            loc[i] %>% unlist() %>% .[2] + f_size)
  }

  return(pad_all)
}



offers %>%
  select(body) %>%
  filter(str_detect(str_to_lower(body), "innowacyjne projekty")) %>%
  distinct() %>%
  mutate(pad = fragment(body, "innowacyjne projekty", 30))




# bierzemy po top 50 najczęściej występujących słów w tagu
words_pca <- words_clean %>%
  select(tag, word) %>%
  unnest(tag) %>%
  count(tag, word) %>%
  group_by(tag) %>%
  mutate(p = n/sum(n)) %>%
  top_n(50, p) %>%
  ungroup()

# szeroka tabela
words_clean_mat <- words_pca %>%
  select(-n) %>%
  spread(tag, p, fill = 0)

# pca
pca <- princomp(words_clean_mat[, 2:ncol(words_clean_mat)])

pca_df <- pca$scores %>%
  as_tibble() %>%
  select(Comp.1, Comp.2)

pca_df$kmeans_cluster <- kmeans(pca_df, 4)$cluster

pca_df %>%
  ggplot() +
  geom_point(aes(Comp.1, Comp.2, color = as.factor(kmeans_cluster)))

pca_df$word <- words_clean_mat$word

pca_df <- left_join(pca_df,
                    words_pca,
                    by = "word")

pca_df %>%
  group_by(kmeans_cluster, word) %>%
  summarise(n = sum(n)) %>%
  top_n(10, n) %>%
  filter(n > 1) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(word, n)) +
  coord_flip() +
  facet_wrap(~kmeans_cluster, scales = "free")


pca_df %>%
  count(tag, kmeans_cluster) %>%
  ggplot() +
  geom_col(aes(tag, n, fill = as.factor(kmeans_cluster)), position = position_fill()) +
  coord_flip()
