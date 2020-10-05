library(tidyverse)
library(lubridate)
library(jsonlite)

df <- fromJSON("dane/json/10207", flatten = TRUE)
df <- df$schedules %>%
  unnest() %>%
  mutate(schedule_date = as_date(schedule_date),
         arrival_time = as_datetime(arrival_time),
         departure_time = as_datetime(departure_time))

df %>%
  count(schedule_date) %>%
  ggplot() +
  geom_point(aes(schedule_date, n))

ggplot(df) +
  geom_density(aes(arrival_delay)) +
  facet_wrap(~station_name)

df %>%
  filter(departure_delay != 0) %>%
  ggplot() +
  geom_density(aes(departure_delay)) +
  facet_wrap(~station_name)


mean_times <- df %>%
  mutate(departure_time = make_datetime(year = 2018, month = 1, day = 1,
                                        hour = hour(departure_time),
                                        min = minute(departure_time)),
         arrival_time = make_datetime(year = 2018, month = 1, day = 1,
                                      hour = hour(arrival_time),
                                      min = minute(arrival_time))) %>%
  group_by(station_name) %>%
  summarise(m_departure_time = mean(departure_time),
            m_arrival_time = mean(arrival_time)) %>%
  ungroup()



left_join(df, mean_times, by = "station_name") %>%
  mutate(departure_time = make_datetime(year = 2018, month = 1, day = 1,
                                        hour = hour(departure_time),
                                        min = minute(departure_time)),
         arrival_time = make_datetime(year = 2018, month = 1, day = 1,
                                      hour = hour(arrival_time),
                                      min = minute(arrival_time))) %>%
  mutate(d_departure = departure_time - m_departure_time,
         d_arrival = arrival_time - m_arrival_time) %>%
  ggplot() +
  geom_density(aes(d_departure), fill = "red", alpha = 0.5) +
  geom_density(aes(d_arrival), fill = "green", alpha = 0.5) +
  facet_wrap(~station_name, scales = "free_y")




