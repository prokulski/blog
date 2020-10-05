# Enklawa wydana jako Ove Logmansbo ale napisał ją Mróz

setwd("~/RProjects/AutorKsiazki")

library(tidyverse)
library(tidytext)
library(wordcloud)

rm(list=ls())

pl_stop_words <- read_lines("polish_stopwords.txt")


# lista książek
lista_ksiazek <- data_frame(
  autor = c("Miloszewski", "Miloszewski", "Miloszewski", "Miloszewski",
            "Mroz", "Mroz", "Mroz", "Mroz"),
  tytul = c("Bezcenny", "Gniew", "Uwiklanie", "Ziarno_prawdy",
            "Enklawa", "Kasacja", "Rewizja", "Zaginiecie")
)

liczba_ksiazek <- nrow(lista_ksiazek)

# nazwy plików
lista_ksiazek$plik <- paste0("ksiazki/", lista_ksiazek$autor, "-", lista_ksiazek$tytul, ".txt")

# poprawiamy nazwiska i tytuły, żeby ładniej wyglądało :)
lista_ksiazek$autor <- ifelse(lista_ksiazek$autor == "Mroz", "Remigiusz Mróz", "Zygmunt Miłoszewski")


# dwie książki oznaczamy jako nieznani autorzy
lista_ksiazek[1, "autor"] <- "nieznany1"
lista_ksiazek[5, "autor"] <- "nieznany2"

lista_ksiazek[3, "tytul"] <- "Uwikłanie"
lista_ksiazek[4, "tytul"] <- "Ziarno prawdy"
lista_ksiazek[8, "tytul"] <- "Zaginięcie"


# książki do jednej tabeli
ksiazki <- data_frame()
for(i in 1:nrow(lista_ksiazek)) {
  ksiazka <- read_lines(as.character(lista_ksiazek[i, "plik"]))
  ksiazka <- tbl_df(ksiazka) %>%
    rename(text = value) %>%
    mutate(tytul = as.character(lista_ksiazek[i, "tytul"]),
           autor = as.character(lista_ksiazek[i, "autor"]))

  ksiazki <- rbind(ksiazki, ksiazka)
}

rm(ksiazka, lista_ksiazek, i)

# usunięcie pustych linii
ksiazki <- filter(ksiazki, nchar(text) != 0)

# słowa
words <- ksiazki %>%
  unnest_tokens(words, text, token = "words") %>%
  filter(!words %in% pl_stop_words) %>%
  count(autor, tytul, words) %>%
  ungroup() %>%
  group_by(tytul) %>%
  mutate(proc = 100*n/sum(n)) %>%
  ungroup()

# chmurki słów
by(words,
   words$tytul,
   function(x) {
     wordcloud(x$words, x$n,
               max.words = 100,
               scale = c(2.0, 0.4),
               colors = RColorBrewer::brewer.pal(9, "Greens")[4:9])
     text(0.05, 0.95,
          paste0(unique(x$autor), " - \"", unique(x$tytul), "\""),
          col="darkred", cex=1,  adj=c(0,0))
   }
)

# najpopularniejsze 20 słów z książek
words %>%
  group_by(tytul) %>%
  top_n(20, wt = proc) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(words, proc, fill=tytul), stat="identity") +
  facet_wrap(~tytul, nrow = 1) +
  coord_flip()

words %>%
  group_by(tytul) %>%
  top_n(20, wt = proc) %>%
  ungroup() %>%
  group_by(words) %>%
  mutate(p = mean(proc)) %>%
  ungroup() %>%
  arrange(p) %>%
  mutate(words = factor(words, levels=unique(words))) %>%
  ggplot() +
  geom_point(aes(words, proc, color=tytul)) +
  coord_flip()

# słowa, które występują we wszystkich książkach
common_words <- words %>%
  group_by(words) %>%
  mutate(n_ksiazek = n()) %>%
  ungroup() %>%
  filter(n_ksiazek == liczba_ksiazek) %>%
  # średni udział słów na autora
  group_by(autor, words) %>%
  summarise(proc = mean(proc)) %>%
  ungroup()

# zmiana nazwisk, żeby kolumny się przyjemniej nazywało w kodzie
common_words$autor <- ifelse(common_words$autor == "Remigiusz Mróz", "mroz", common_words$autor)
common_words$autor <- ifelse(common_words$autor == "Zygmunt Miłoszewski", "miloszewski", common_words$autor)

# pivot
common_words <- common_words %>%
  spread(autor, proc)


# nieznani vs znani - do ktorego najblizej?

# metoda korelacji
library(corrgram)
corrgram(common_words, lower.panel = panel.shade, upper.panel = panel.cor)

# z korelacji widać, że największe współczynniki mają pary:
# nieznany1 - Miłoszewski
# książka napisana przez "nieznany1" to:
as.character(unique(ksiazki[ksiazki$autor == "nieznany1", "tytul"]))

# nieznany2 - Mróz
# książka napisana przez "nieznany2" to:
as.character(unique(ksiazki[ksiazki$autor == "nieznany2", "tytul"]))


# metoda najmniejszej odległości między średnimi udziałami
common_words %>%
  # liczymy bezwzględne odległości pomiędzy udziałem słów
  # w poszczególnych parach autorów - nieznani ze znanymi
  mutate(n1_miloszewski = abs(nieznany1 - miloszewski),
         n2_miloszewski = abs(nieznany2 - miloszewski),
         n1_mroz = abs(nieznany1 - mroz),
         n2_mroz = abs(nieznany2 - mroz)) %>%
  select(n1_miloszewski, n1_mroz, n2_miloszewski, n2_mroz) %>%
  # dla każdej z odległości liczymy podstawowe współczynniki statystyczne:
  # średnią, medianę i odcyhlenie standartowe
  map_df(.f = function(x) {
    data_frame(srednia = mean(x),
               mediana = median(x),
               odchylenie_std = sd(x))
  },
  .id = "para") %>%
  # unpivot tabeli na potrzeby wykresu
  gather(key="key", value="val", srednia, mediana, odchylenie_std) %>%
  # liczby są małe to je skalujemy
  mutate(val = 1000 * val) %>%
  # wykres
  ggplot() +
  geom_bar(aes(para, val), stat="identity",
           fill="lightgreen", color="darkgreen") +
  # liczby, żeby było łatwiej porównać wielkość słupków
  geom_text(aes(para, val, label=round(val, 2)), hjust = 1.1) +
  facet_wrap(~key, ncol=3) +
  coord_flip()

# interesują nas najmniejsze wartości - to oznacza najmniejszą różnicę
# z wykresu widać, że najmniejsze wartości dla każdego z czynników mają pary
# n1_miłoszewski
# n2_mróz
# czyli dokładnie tak samo jak wcześniej



# bigrams
biwords <- ksiazki %>%
  unnest_tokens(words, text, token = "ngrams", n=2) %>%
  separate(words, c("word1", "word2")) %>%
  filter(!word1 %in% pl_stop_words) %>%
  filter(!word2 %in% pl_stop_words) %>%
  mutate(words = paste(word1, word2, sep = " ")) %>%
  count(tytul, autor, words) %>%
  ungroup()

by(biwords,
   biwords$tytul,
   function(x) {
     wordcloud(x$words, x$n, max.words = 50, scale = c(1.4, 0.4))
     text(0.05, 0.95,
          paste0(unique(x$autor), " - \"", unique(x$tytul), "\""),
          col="darkred", cex=1,  adj=c(0,0))
   }
)


# tri-grams

triwords <- ksiazki %>%
  unnest_tokens(words, text, token = "ngrams", n=3) %>%
  separate(words, c("word1", "word2", "word3")) %>%
  #   filter(!word1 %in% pl_stop_words) %>%
  #   filter(!word2 %in% pl_stop_words) %>%
  #   filter(!word3 %in% pl_stop_words) %>%
  unite(words, word1, word2, word3, sep = " ") %>%
  count(tytul, autor, words) %>%
  ungroup()

by(triwords,
   triwords$tytul,
   function(x) {
     wordcloud(x$words, x$n, max.words = 50, scale = c(1.4, 0.4))
     text(0.05, 0.95,
          paste0(unique(x$autor), " - \"", unique(x$tytul), "\""),
          col="darkred", cex=1,  adj=c(0,0))
   }
)
