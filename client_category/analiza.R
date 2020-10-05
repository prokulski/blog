setwd("~/RProjects/client_category")

library(data.table)
library(tidyverse)
library(lubridate)


df <- fread("data/train.csv", select = c(4,12), col.names = c("user_id", "timestamp"))

df <- df %>%
  mutate(timestamp = as_datetime(timestamp),
         date = as_date(timestamp),
         wday = wday(timestamp, week_start = 1, label = TRUE, abbr = FALSE, locale = "pl_PL.utf8"),
         hour = hour(timestamp))

# rozkład po dniu
df %>%
  count(date) %>%
  ggplot() +
  geom_area(aes(date, n))


# rozkład po godzinie i dniu tygodnia
df %>%
  count(wday, hour) %>%
  ggplot() +
  geom_tile(aes(wday, hour, fill = n)) +
  scale_y_reverse()


# rozkład po userze

df %>%
  count(user_id) %>%
  ggplot() +
  geom_density(aes(n)) +
  scale_x_log10()



###### przygotowanie danych pod model

# funkcja wybiera dane, które będą użyte do treowania modelu
select_data <- function(source_df) {

   source_df <- sample_frac(source_df, 0.1)
  users_more_than_once <- count(source_df, user_id) %>% filter(n > 1) %>% pull(user_id)
  df_more_than_once <- filter(source_df, user_id %in% users_more_than_once)

  return(df_more_than_once)
}


# funkcja skaluje szereg liczb, aby mieścił się w zakresie <0; 1>
scale_num <- function(x) {
  return((x-min(x))/(max(x) - min(x)))
}


# funkcja przygotowuje dane dla modelu - cechy ukryte itp
prepare_data <- function(source_df) {
  source_df %>%
    # łączna liczba wizyt na użytkownika
    group_by(user_id) %>%
    mutate(n_times = n()) %>%
    ungroup() %>%
    # cechy oparte na dacie
    mutate(dw = sprintf("w%d", wday(timestamp, week_start = 1)),       # dzień tygodnia
           h = hour(timestamp),                                        # godzina
           w = 1*(wday(timestamp, week_start = 1) %in% c(6, 7))) %>%   # czy weekend?
    # zliczamy liczbę wizyt w danym dniu tygodnia i o określonej godzinie dla każdego z użytkowników
    count(user_id, n_times, w, h, dw) %>%
    # przechodzimy na szeroką tabelę - takie nieco one-hot-encoder dla dni tygodnia i weekendów
    spread(dw, n, fill = 0) %>%
    # skalowanie godzin i łącznej liczby odwiedzin
    mutate_at(.vars = c("h", "n_times"),
              .funs = scale_num)
}



# wybieramy dane treningowe
df_train <- select_data(df)

nrow(df_train) / nrow(df)

ggplot() +
  geom_line(data = df_train %>% count(date), aes(date, n), color = "green") +
  geom_line(data = df %>% count(date), aes(date, n), color = "red")

left_join(df %>% count(date) %>% set_names(c("date", "n_all")),
          df_train %>% count(date) %>% set_names(c("date", "n_train")),
          by = "date") %>%
  mutate(ratio = 100*n_train/n_all) %>%
  ggplot() +
  geom_line(aes(date, ratio)) +
  labs(title = "Udział procentowy użytkowników powracajcych w ruch na stronie",
       x = "", y = "% użytkowników powracajcych")


##### przygotowanie modelu

# przygotowanie danych uczących model
wide_mat <- prepare_data(df_train)

# wybieramy unikalne wiersze do t-SNE
wide_mat_dist <- wide_mat %>%
  select(-user_id) %>%
  distinct()

# kmeans do określenia kategorii usera
km <- kmeans(wide_mat_dist, centers = 3)

# przypisujemy klasy do danych treningowych
wide_mat_dist$cluster <- as.factor(km$cluster)



### wtręt :)



# sprawdźmy czy tworzą się jakieś klastry
library(Rtsne)
tsne <- Rtsne(wide_mat_dist, initial_dims = ncol(wide_mat_dist))

tsne_df <- as.data.frame(tsne$Y)

ggplot(tsne_df, aes(V1, V2)) + geom_point()


tsne_df$cluster <- as.factor(km$cluster)

ggplot(tsne_df, aes(V1, V2, color = cluster)) + geom_point()

####





library(randomForest)

# trenujemy model
model <- randomForest(cluster ~ ., data = wide_mat_dist,
                      ntree = 100,
                      importance = TRUE)

# jakie cechy są najważniejsze?
varImpPlot(model)




# dopisujemy klasy do szerokiej tabeli z user_id
wide_mat$cluster <- predict(model, wide_mat)

# mając przypisanie user_id do klas możemy dodać klasy do danych źródłowych
df_train_clusters <- left_join(df_train,
                               select(wide_mat, user_id, cluster),
                               by = "user_id")




# teraz możemy sprawdzić jak rozkłada się ruch w poszczególnych klasach
# ruch dzien po dniu
df_train_clusters %>%
  filter(year(date) == 2017, month(date) == 4) %>%
  count(cluster, date) %>%
  group_by(cluster) %>%
  mutate(n_scaled = scale_num(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_area(aes(date, n_scaled, fill = cluster), alpha = 0.5, position = position_identity()) +
  geom_line(aes(date, n_scaled, color = cluster))



# ruch wg dni tygodnia
df_train_clusters %>%
  count(cluster, wday) %>%
  group_by(cluster) %>%
  mutate(n_scaled = scale_num(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(wday, n_scaled, fill = cluster)) +
  facet_wrap(~cluster, scales = "free_y")


# ruch wg godzin
df_train_clusters %>%
  count(cluster, hour) %>%
  group_by(cluster) %>%
  mutate(n_scaled = scale_num(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(hour, n_scaled, fill = cluster)) +
  facet_wrap(~cluster, scales = "free_y")



# ruch wg godzin i dni tygodnia
df_train_clusters %>%
  count(cluster, wday, hour) %>%
  group_by(cluster) %>%
  mutate(n_scaled = scale_num(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_tile(aes(wday, hour, fill = n_scaled)) +
  facet_wrap(~cluster) +
  scale_y_reverse()

