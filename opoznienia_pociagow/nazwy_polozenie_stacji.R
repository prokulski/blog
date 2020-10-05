setwd("/home/lemur/RProjects/opoznienia_pociagow")

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(logging))

# jesli trzeba tworzymy folder na logi
if(!dir.exists("logs")) dir.create("logs")

# konfiguracja logowania
logReset()
basicConfig()

# nazwa pliku do logowania - z data startu skryptu
LOG_FILENAME <- paste0("logs/scrapper_", str_replace_all(Sys.time(), "[-: ]", "_"), ".log")
addHandler(writeToFile, logger="scrapper", file=LOG_FILENAME)


# czy sa zapisane jakies dane?
if(file.exists("lokalizacje_stacji.rds")) {
  # tak - wczytujemy i jedziemy dalej
  stacje <- readRDS("lokalizacje_stacji.rds")
  m_id <- max(stacje$id)+1
} else {
  # nie - zaczynamy od zera
  stacje <- tibble()
  m_id <- 1
}

# zeby sie nie wywaliło przy timeoucie
safe_read_html <- safely(read_html)

# jedziemy po kolei wszystkie stacje
for(id in m_id:40000) {
  cat(paste0("\rID = ", id))

  base_url <- glue("https://www.bazakolejowa.pl/index.php?dzial=stacje&id={id}&ed=0&okno=polozenie")

  # proba wczytania strony
  page <- safe_read_html(base_url)

  # czy udalo sie?
  if(!is_null(page$result)) {

    script_js <- page$result %>% html_nodes("script") %>% html_text() %>% .[str_detect(., "mapInit")]

    if(length(script_js) != 0) {
      df <- str_match_all(script_js, "([\\d.]+), ([\\d.]+),.*'(.*)', ") %>% unlist() %>% .[2:4]
      df <- tibble(id = id, station_name = df[[3]], long = as.numeric(df[[1]]), lat = as.numeric(df[[2]]))

      stacje <- bind_rows(stacje, df)

      # loginfo(paste0("Zapisane ID = ", id), logger="scrapper")
    }
  } else {
    error_log <- paste0("ID = ", id, ": ", page$error)
    cat(paste0("\n", error_log, "\n"))
    # log zapisany do pliku
    logerror(error_log, logger="scrapper")

    # chyba zabijam serwer, po około 15 minutach się podnosi - czekajmy więc 20 minut
    Sys.sleep(20*60)
    logwarn("Ruszam dalej", logger="scrapper")

    # i jedziemy dalej
  }

  Sys.sleep(sample(seq(0.5, 1.5, 0.25), 1))

  if(id %% 10 == 0) saveRDS(stacje, "lokalizacje_stacji.rds")

}

saveRDS(stacje, "lokalizacje_stacji.rds")
