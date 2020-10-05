setwd("~/RProjects/pilka_nozna")
# https://www.reddit.com/.../bg1el1/soccer_shots_dataset/ (już skasowany) zawierający link do arkusza z danymi zeskrapowanymi z https://understat.com/
# wymiray boiska http://www.sztucznatrawa.eu/art/1/projekty-boisk-52.html

library(tidyverse)
# library(googledrive)

# "1yeIl0rZldsmmRzdGUkG56QmX7UXI0PHQ" %>%
#   as_id() %>%
#   drive_get() %>%
#   drive_download(path = "xg_master.csv")


library(janitor)

library(igraph)


data <- read_csv("xg_master.csv") %>%
  clean_names() %>%
  mutate(player = stringi::stri_enc_tonative(player),
         assist_player = stringi::stri_enc_tonative(assist_player))



library(ggExtra)

# create a ggplot2 scatterplot

data %>% filter(result == "Goal") %>%
  ggplot( aes(x=x_coordinate, y=y_coordinate)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  # boisko
  scale_fill_viridis() +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 0.5, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0.2, xmax = 0.157, ymax = 0.8), fill = NA, color = "black") +
  geom_rect(aes(xmin = 1, ymin = 0.2, xmax = 1-0.157, ymax = 0.8), fill = NA, color = "black") +
  theme_void()


# add marginal histograms
ggExtra::ggMarginal(p, type = "histogram", color="grey")


# jakie mamy ligi i drużyny?
data %>%
  distinct(league, team) %>%
  group_by(league) %>%
  arrange(team) %>%
  mutate(n = row_number()) %>%
  ungroup() %>%
  spread(league, team) %>%
  select(-n) %>%
  as.data.frame


# skąd co pada?
data %>%
  ggplot() +
  geom_point(aes(x_coordinate, y_coordinate,
                 color = situation),
             alpha = 0.7, size = 0.5, show.legend = FALSE) +
  facet_grid(situation~result) +
# boisko
xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 0.5, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0.2, xmax = 0.157, ymax = 0.8), fill = NA, color = "black") +
  geom_rect(aes(xmin = 1, ymin = 0.2, xmax = 1-0.157, ymax = 0.8), fill = NA, color = "black") +
  theme_void()

data %>% count(result)

# jak padają gole?
data %>% filter(result == "Goal") %>% count(shot_type)

data %>% filter(result == "Goal") %>% count(situation)

data %>% filter(result == "Goal") %>% count(preceding_action)


# w której minucie padają bramki
data %>%
  filter(result == "Goal") %>%
  filter(minute <= 92) %>%
  count(minute) %>%
  ggplot() +
  geom_line(aes(minute, n), color = "green") +
  geom_smooth(aes(minute, n), color = "lightgreen", alpha = 0.5) +
  geom_vline(xintercept = c(45, 90))


# bloki po 5 minut, zależność od ligi
data %>%
  filter(result == "Goal") %>%
  mutate(minute = 5*( minute %/% 5) ) %>%
  count(minute, league) %>%
  group_by(league) %>%
  mutate(n = 100*n/sum(n)) %>%
  ungroup %>%
  ggplot() +
  geom_tile(aes(minute, league, fill = n)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  geom_vline(xintercept = c(45, 90))


# czy Neymar strzela lepiej czy gorzej w PSG po przejściu z Barcy?
data %>%
  filter(player == "Neymar") %>%
  filter(result == "Goal") %>%
  count(date, team) %>%
  ggplot() +
  geom_point(aes(date, n, color = team))


# skąd strzela
data %>%
  filter(player == "Neymar") %>%
  filter(result == "Goal") %>%
  ggplot() +
  geom_point(aes(x_coordinate, y_coordinate, color = team)) +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  # boisko
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 0.5, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0.2, xmax = 0.157, ymax = 0.8), fill = NA, color = "black") +
  geom_rect(aes(xmin = 1, ymin = 0.2, xmax = 1-0.157, ymax = 0.8), fill = NA, color = "black") +
  theme_void()



# top 12 strzelców
top_strikers <- data %>%
  filter(result == "Goal") %>%
  count(player, sort = T) %>%
  top_n(12, n) %>%
  pull(player)

# skąd strzelają?
data %>%
  filter(player %in% top_strikers) %>%
  filter(result == "Goal") %>%
  ggplot( aes(x=x_coordinate, y=y_coordinate)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  # boisko
  scale_fill_viridis() +
  facet_wrap(~player) +
  # boisko
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 1, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 0.5, ymax = 1), fill = NA, color = "black") +
  geom_rect(aes(xmin = 0, ymin = 0.2, xmax = 0.157, ymax = 0.8), fill = NA, color = "black") +
  geom_rect(aes(xmin = 1, ymin = 0.2, xmax = 1-0.157, ymax = 0.8), fill = NA, color = "black") +
  theme_void()


# Bramka Messiego prawie z rogu?
data %>%
  filter(player == "Lionel Messi", result == "Goal") %>%
  filter(y_coordinate == min(y_coordinate)) %>%
  t()

# https://www.google.com/search?q=Barcelona+-+Athletic+Club+2017-02-04
# https://youtu.be/VRDKncBV7eU?t=230



# z jakiej odległosci strzelają?
data %>%
  filter(player %in% top_strikers) %>%
  filter(result == "Goal") %>%
  mutate(distance = sqrt((x_coordinate-1)^2 + (y_coordinate-0.5)^2)) %>%
  group_by(player) %>%
  mutate(m_dist = mean(distance)) %>%
  ungroup() %>%
  arrange(m_dist) %>%
  mutate(player = fct_inorder(player)) %>%
  ggplot() +
  geom_boxplot(aes(player, distance, fill = player), color = "black",
               show.legend = FALSE) +
  coord_flip()

# najdalszy strzał
data %>%
  filter(result == "Goal") %>%
  mutate(distance = sqrt((x_coordinate-1)^2 + (y_coordinate-0.5)^2)) %>%
  filter(distance == max(distance)) %>%
  t()


# https://www.youtube.com/watch?v=EhMhqz0qvUo



# rozgrywki między drużynami

data_pairs <- data %>%
  select(home, away, league, home_goals, away_goals, match_id) %>%
  distinct(match_id, .keep_all = TRUE) %>%
  mutate(hG = if_else(home < away, home_goals, away_goals),
         aG = if_else(home < away, away_goals, home_goals),
         h = if_else(home < away, home, away),
         a = if_else(home < away, away, home)) %>%
  count(h, hG, a, aG) %>%
  group_by(h, a) %>%
  mutate(p = 100*n/sum(n)) %>%
  ungroup()



A_team = "Real Madrid"
B_team = "Barcelona"

data_sel <- data_pairs %>%
  filter(h %in% c(A_team, B_team),
       a %in% c(A_team, B_team))

home_team <- data_sel %>% pull(h) %>% unique()
away_team <- data_sel %>% pull(a) %>% unique()

data_sel %>%
  ggplot() +
  geom_tile(aes(hG, aG, fill = p), color = "black", show.legend = FALSE) +
  geom_text(aes(hG, aG, label = sprintf("%.f%%", p))) +
  scale_x_continuous(breaks = 0:10, limits = c(-1,11)) +
  scale_y_continuous(breaks = 0:10, limits = c(-1,11)) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(title = paste(home_team, "-", away_team),
       x = home_team, y = away_team)



sel_team = "Barcelona"

# bilans rozgrywek
data_pairs %>%
  filter(h == sel_team | a == sel_team) %>%
  mutate(sel_goals = if_else(h == sel_team, hG, aG),
         other_goals =  if_else(h == sel_team, aG, hG),
         other =  if_else(h == sel_team, a, h)) %>%
  select(other, sel_goals, other_goals, n) %>%
  gather("key", "val", -other, -n) %>%
  mutate(key = factor(key, levels = c("sel_goals", "other_goals"),
                      labels = c("Bramki strzelone", "Bramki stracone"))) %>%
  ggplot() +
  geom_tile(aes(other, val, fill = n), color = "black") +
  scale_y_continuous(breaks = 0:10) +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  facet_wrap(~key, ncol = 2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust = 1))


# srednie bramki
data_pairs %>%
  filter(h == sel_team | a == sel_team) %>%
  mutate(sel_goals = if_else(h == sel_team, hG, aG),
         other_goals =  if_else(h == sel_team, aG, hG),
         other =  if_else(h == sel_team, a, h)) %>%
  select(other, sel_goals, other_goals, n) %>%
  mutate(sel_goals = n * sel_goals,
         other_goals = n * other_goals) %>%
  group_by(other) %>%
  summarise(sel_goals = sum(sel_goals)/n(),
            other_goals = sum(other_goals)/n()) %>%
  ungroup() %>%
  mutate(delta = sel_goals - other_goals) %>%
  arrange(desc(delta)) %>%
  mutate(other = fct_inorder(other) %>% fct_rev()) %>%
  ggplot() +
  geom_col(aes(other, sel_goals), fill = "green") +
  geom_text(aes(other, sel_goals, label = sprintf("%.2f", sel_goals)), hjust = -0.1) +
  geom_col(aes(other, -other_goals), fill = "red") +
  geom_text(aes(other, -other_goals, label = sprintf("%.2f", other_goals)), hjust = 1.01) +
  geom_point(aes(other, delta), color = "black", size = 2) +
  # geom_text(aes(other, delta, label = sprintf("%.2f", delta)), hjust = -0.2) +
  coord_flip()




# kto komu podaje?
passes_graph <- data %>%
  filter(result == "Goal") %>%
  filter(player %in% top_strikers) %>%
  filter(!is.na(assist_player)) %>%
  # filter(player == "Robert Lewandowski") %>%
  count(assist_player, player) %>%
  # mutate_at(c("assist_player", "player"), iconv, to = "UTF8") %>%
  # filter(n >= 4) %>%
  select(from = assist_player, to = player, weight = n) %>%
  graph_from_data_frame(directed = TRUE)


plot(passes_graph,
     vertex.size = if_else(V(passes_graph)$name %in% top_strikers, 7, 3),
     # vertex.label.cex = 0.7,
     # vertex.size = V(passes_graph)$betweenness,
     vertex.label.cex = if_else(V(passes_graph)$name %in% top_strikers, 1.2, 0.1),
     vertex.label.color = "gray20",
     edge.arrow.size = 0,
     edge.arrow.width = 1.2,
     edge.width = E(passes_graph)$weight,
     edge.curved = TRUE)

# Lucas Moura - przezesł z PSG fo Tottenham


players_team <- bind_rows(data %>%
            filter(result == "Goal") %>%
            distinct(player, team),
          data %>%
            filter(result == "Goal") %>%
            distinct(assist_player, team) %>%
            select(player = assist_player)) %>%
  distinct() %>%
  drop_na() %>%
  count(player, team) %>%
  group_by(player) %>%
  top_n(1, n) %>%
  ungroup()


library(networkD3)

# Convert to object suitable for networkD3
passes_graph_d3 <- igraph_to_networkD3(passes_graph)

library(htmltools)

browsable(
  tagList(
    tags$head(
      tags$style('
        body{background-color: #FFFFFF !important}
        .nodetext{fill: #000000}
        .legend text{fill: #FF0000}
      ')
    ),
    # Create force directed network plot
    forceNetwork(Links = passes_graph_d3$links, Nodes = passes_graph_d3$nodes,
                 Source = 'source', Target = 'target',
                 NodeID = 'name', Group = 'name',
                 linkWidth = passes_graph_d3$links$value,
                 fontSize = 11,
                 opacityNoHover = 0.6,
                 zoom = TRUE)
  )
) %>%
  save_html("player_d3.html")
