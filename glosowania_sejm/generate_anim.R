library(tidyverse)

tsne_total <- readRDS("tsne_total.rds")

glosowania <- readRDS("final_glosowania.RDS")

klub_osoba <- glosowania %>% distinct(id_glosowania, klub, osoba)


tsne_total <- left_join(tsne_total, klub_osoba, by = c("id_glosowania" = "id_glosowania",
                                                       "osoba"))

rm(glosowania, klub_osoba)


ids <- sort(unique(tsne_total$id_glosowania))
j <- 0

for(id in ids) {
  j <- j + 1

  p <- tsne_total %>%
    filter(id_glosowania == id) %>%
    ggplot() +
    geom_point(aes(V1, V2, color = klub)) +
    scale_x_continuous(limits = c(-42, 42)) +
    scale_y_continuous(limits = c(-42, 42)) +
    theme_minimal()

  ggsave(filename =  sprintf("anim/%05d.png", j), plot = p,
         width = 12, height = 12, dpi = 90, units = "in", device = "png")
}

system('ffmpeg -r 5 -i anim/%05d.png -c:v libx264 -vf fps=25 -pix_fmt yuv420p glosowania_tsne.mp4')
