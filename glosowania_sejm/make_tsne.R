library(tidyverse)
library(lubridate)
library(widyr)
library(Rtsne)


glosowania <- readRDS("final_glosowania.RDS")


# przygotowanie tabeli z wartościami t-SNE dla wszystkich posłów po kolejnych głosowaniach
set.seed(2019)
nr_glosowan <- sort(unique(glosowania$id_glosowania))


j <- 0
l <- length(nr_glosowan)


for(i in nr_glosowan) {

  j <- j + 1
  cat(paste0("\r", j, " / ", l))

  df <-  glosowania %>%
    filter(id_glosowania <= i) %>%
    count(klub, osoba, glos) %>%
    spread(glos, n, fill = 0)

  df <- bind_cols(df, Rtsne(df[, 3:ncol(df)],
                            check_duplicates = FALSE,
                            pca = FALSE,
                            perplexity = 15,
                            theta = 0.25)$Y %>%
                    as_tibble() %>%
                    set_names(c("V1", "V2"))) %>%
    select(klub, osoba, V1, V2)

  p <- df %>%
    ggplot() +
    geom_point(aes(V1, V2, color = klub)) +
    scale_x_continuous(limits = c(-42, 42)) +
    scale_y_continuous(limits = c(-42, 42)) +
    theme_minimal()

  ggsave(filename =  sprintf("anim/%05d.png", j), plot = p,
         width = 12, height = 12, dpi = 90, units = "in", device = "png")
}

system('ffmpeg -r 5 -i anim/%05d.png -c:v libx264 -vf fps=25 -pix_fmt yuv420p glosowania_tsne.mp4')
