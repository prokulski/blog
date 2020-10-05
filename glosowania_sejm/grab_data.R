setwd("~/RProjects/glosowania_sejm/")

# lista posiedzen Sejmu
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=posglos&NrKadencji=8
# stąd bierzemy urle do głosowań
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=listaglos&IdDnia=1745
# konkretne głosowanie
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=glosowania&NrKadencji=8&NrPosiedzenia=84&NrGlosowania=67
# stąd bierzemy listę klubów, dla każdego klubu:
# http://sejm.gov.pl/Sejm8.nsf/agent.xsp?symbol=klubglos&IdGlosowania=51679&KodKlubu=Kukiz15

library(tidyverse)
library(lubridate)
library(rvest)

# library(RPostgreSQL)


# # pobranie danych z bazy
# dbname = "baza"
# user = "lemur"
# password = "pr0cma1l"
# host = "localhost"
#
# polaczenie <- src_postgres(dbname = dbname, host = host, user = user, password = password)



wez_liste_klubow <- function(glosowanie_url) {

  page <- read_html(glosowanie_url)

  kluby <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_node("tbody") %>%
    html_nodes("tr") %>%
    html_node("td") %>%
    html_node("a")

  return(tibble(
    klub = kluby %>% html_text(),
    link = kluby %>% html_attr("href") %>% paste0("http://sejm.gov.pl/Sejm8.nsf/", .)))
}

wez_glosowanie_klubu <- function(glosowanie_klubu_url) {

  page <- read_html(glosowanie_klubu_url)

  # wyniki konkretnego głosowania
  wyniki_glosowania <- page %>%
    html_node("div#contentBody") %>%
    html_node("table") %>%
    html_node("tbody") %>%
    html_nodes("td") %>%
    html_text() %>%
    str_squish()

  wyniki_glosowania_df <- tibble()

  for(i in seq(1, length(wyniki_glosowania), 3)) {
    wyniki_glosowania_df <- bind_rows(wyniki_glosowania_df,
                                      tibble(osoba = wyniki_glosowania[i+1],
                                             glos = wyniki_glosowania[i+2]))
  }

  wyniki_glosowania_df <- wyniki_glosowania_df %>%
    mutate(id_glosowania = as.numeric(str_match(glosowanie_klubu_url, "IdGlosowania=(\\d+)&KodKlubu=(.*)")[1,2]),
           klub = str_match(glosowanie_klubu_url, "IdGlosowania=(\\d+)&KodKlubu=(.*)")[1,3])

  return(wyniki_glosowania_df)

}


lista_glosowan_org <- readRDS("lista_glosowan.rds")

lista_glosowan <- lista_glosowan_org %>% filter(!pobrane)


glosowania_total <- readRDS("part_glosowania_total.RDS")


for(i in 1:nrow(lista_glosowan)) {

  glosowanie_kluby_url <- wez_liste_klubow(lista_glosowan$link[i])
  posiedzenie <- str_match(lista_glosowan$link[i], "NrPosiedzenia=(\\d+)&NrGlosowania=(\\d+)")
  data_posiedzenia <- lista_glosowan$data_posiedzenia[i]
  godzina_glosowania <- lista_glosowan$godzina[i]

  for(j in 1:nrow(glosowanie_kluby_url)) {

    cat(paste0(format(now(), "%H:%M:%S "),
               "Głosowanie ", i, " / ", nrow(lista_glosowan),
               ", klub ", j, " / ", nrow(glosowanie_kluby_url), ": ",
               glosowanie_kluby_url$link[j], "\n"))

    glosy_klubu <- wez_glosowanie_klubu(glosowanie_kluby_url$link[j]) %>%
      mutate(data_posiedzenia = data_posiedzenia,
             godzina_glosowania = godzina_glosowania,
             numer_posiedzenia = posiedzenie[1, 2],
             numer_glosowania = posiedzenie[1, 3])

    glosowania_total <- bind_rows(glosowania_total, glosy_klubu)

    Sys.sleep(runif(1, 2, 8)) # czekamy 2-8 sekund
  }

  lista_glosowan_org[lista_glosowan_org['link'] == lista_glosowan$link[i], "pobrane"] <- TRUE

  saveRDS(lista_glosowan_org, "lista_glosowan.rds")
  saveRDS(glosowania_total, "part_glosowania_total.RDS")

  # copy_to(polaczenie, glosowania_total, "glosowania", overwrite = TRUE, temporary = FALSE)
}

cat("\n\nLista głosów pobrana.\n")

saveRDS(glosowania_total, "glosowania_total.RDS")
