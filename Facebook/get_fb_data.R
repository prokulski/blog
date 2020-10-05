# setwd("~/RProjects/FB_sentiment/shiny")
rm(list = ls())

library(Rfacebook)
library(tidyverse)
library(lubridate)

theme_set(theme_minimal())

# slowniki i tokeny ----
load("/home/lemur/RProjects/!tokens/fb_oauth.rda")


#### FUNKCJE ----

getFPPosts <- function(page_name, n_posts = 50) {
  fb_page <- getPage(page = page_name, token = fb_oauth, n = n_posts, reactions = TRUE)

  # tylko potrzebne informacje
  fb_page <- fb_page %>%
    select(id, created_time, message, type, ends_with("_count")) %>%
    mutate(created_time = ymd_hms(created_time, tz = "Europe/Warsaw"))

  return(fb_page)
}



getPostDetails <- function(post_id, fb_page) {

  # liczba komentarzy do tego postu
  post_details <- fb_page[fb_page$id == post_id, ]

  czas <- format(post_details$created_time, "%Y-%m-%d %H:%M")
  message <- post_details$message
  likes <- post_details$likes_count
  comments <- post_details$comments_count

  return(list(time = czas, message = message, likes = likes, comments = comments))
}



getPostComments <- function(post_id, fb_page) {

  # liczba komentarzy do tego postu
  post_coments <- fb_page[fb_page$id == post_id, "comments_count"]

  # pobieramy komentarze z wybranego postu, max 100 sztuk
  fb_post <- getPost(post_id, token = fb_oauth, n = max(post_coments, 1000), comments = TRUE)

  # tabela z komciami - tylko potrzebne dane
  comments <- fb_post$comments %>%
    select(created_time, message, likes_count) %>%
    mutate(created_time = ymd_hms(created_time, tz = "Europe/Warsaw")) %>%
    arrange(created_time)

  return(comments)
}


#### DZIAŁANIE ----

page_name <- "Wojt.Gminy.Minsk.Mazowiecki"

fb_page_table <- getFPPosts(page_name, 300)

# bajbradziej komentowany post
post_id <- fb_page_table %>% filter(comments_count == max(comments_count)) %>% pull(id) %>% .[[1]]

post_details <- getPostDetails(post_id, fb_page_table)
post_details

comments_table <- getPostComments(post_id, fb_page_table)



# kiedy publikowane są posty?
fb_page_table %>%
  mutate(h = hour(created_time),
         d = wday(created_time, week_start = 1, label = TRUE, locale = "pl_PL.UTF8")) %>%
  count(d, h) %>%
  ggplot() +
  geom_tile(aes(d, h, fill = n), color = "gray60") +
  scale_y_reverse() +
  scale_fill_distiller(palette = "Reds", direction = 1)



fb_page_table %>%
  mutate(date = floor_date(created_time, unit = "months")) %>%
  group_by(date) %>%
  summarise(n = n(),
            l = sum(likes_count),
            c = sum(comments_count)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(date, n), color = "blue") + # posts
  geom_line(aes(date, l), color = "red") +  # likes
  geom_line(aes(date, c), color = "green")  # comments



fb_page_table %>%
  mutate(date = floor_date(created_time, unit = "weeks")) %>%
  group_by(date) %>%
  summarise(n = n(),
            l = sum(likes_count),
            c = sum(comments_count)) %>%
  ungroup() %>%
  ggplot() +
  geom_smooth(aes(date, l/n), color = "blue") + # likes/post
  geom_smooth(aes(date, c/n), color = "green")  # comments/post





###

stop_w <- c("krystyna", "anna", "janusz", "to", "jerzy", "zbigniew",
            "hanka", "irena", "jan",  "czesław",  "nie", "artur",
            "ale", "beata", "danuta", "eugeniusz", "krzysztof", "franciszek",
            "marek", "piotr", "pola", "tomasz", "andrzej", "wojciech",
            "joanna", "agnieszka", "daniel", "roman", "ewa", "grzegorz", "gustaw", "katarzyna",
            "się", "na", "10", "bo", "by", "maryla", "jeszcze", "aktorzy", "kalina",
            "panowie", "co", "do", "od", "maria", "mieczysław", "jest", "adolf", "cz", "edyta",
            "urszula", "jacek", "kabaret", "tadeusz", "tylko", "mi", "może", "pan", "pani",
            "tak", "agata", "violetta")

library(tidytext)
library(wordcloud)
library(glue)
source("~/RProjects/!Rmarkdown_templates/da_plot.R")

comments <- getPost("180340042008871_2128002970575892", token = fb_oauth)$comments
ludzie <- comments %>%
  select(message) %>%
  mutate(message = str_replace_all(message, "\\.", " ")) %>%
  unnest_tokens(word, message, token = "words") %>%
  mutate(word = case_when(
    word == "jackowska" ~ "kora",
    word == "kazik" ~ "staszewski",
    word == "muniek" ~ "staszczyk",
    TRUE ~ word)) %>%
  count(word, sort = T) %>%
  filter(!word %in% stop_w) %>%
  filter(nchar(word) != 1)



ludzie %>%
  as.data.frame() %>%
  head(60)

n_limit <- ludzie %>% arrange(desc(n)) %>% pull(n) %>% .[[50]]

ludzie <- ludzie %>% filter(n >= n_limit)

# wordcloud(ludzie$word, ludzie$n, max.words = 50)

n_comm <- nrow(comments)

ludzie %>%
  arrange(n, word) %>%
  mutate(word = fct_inorder(str_to_title(word))) %>%
  ggplot() +
  geom_col(aes(word, n), color = "gray40", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Kto Waszym zdaniem powinien znaleźć się w pierwszej dwudziestce\ngwiazd polskiej rozrywki na 100-lecie niepodległości Polski?",
       subtitle = glue("Zestawienie na podstawie {n_comm} komentarzy pod postem Karoliny Korwin-Piotrowskiej"),
       x = "", y = "Ile razy nazwisko padło w komentarzu?")

da_plot()

