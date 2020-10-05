library(tidyverse)
library(pdftools)

# lista plików PDF
file_names <- c("dane/pbs.pdf", "dane/pdt.pdf", "dane/pek.pdf", "dane/pmm.pdf")

# tutaj będziemy trzymać pełne dane
complete = c()

# dla każdego kolejnego pliku:
for(file_name in file_names) {
  # wczytujemy PDFa
  pdf <- pdf_text(file_name)

  # miejsce na linie z aktualnie przerabianego pliku
  full <- c()

  # dla każdej ze stron w pliku
  for(i in 1:length(pdf)) {
    # dzielimy stronę na linie
    temp <- str_split(pdf[i], "\n")[[1]]
    # dodajemy linie z bierzącej strony do linii z całego pliku
    full <- c(full, temp)
  }

  # usuwamy puste linie
  full <- full[nchar(full) != 0]

  full <- str_squish(full)

  # więcej niż jedna spacja zostaje zamieniona na ;
  full <- gsub(pattern = " {2,}", ";", full)

  full_tab <- c()
  for(f in full) {
    f_tab <- f %>% str_replace_all(" (\\d+)-(\\d+)\\.", " \\2\\.") %>% str_match_all("(\\d+\\.) (.*) (\\d{2}\\.\\d{2}\\.\\d{4}) (.*)") %>% .[[1]] %>% .[3:5]
    full_tab <- c(full_tab,
                  paste0(f_tab[1], ";", f_tab[2], ";", f_tab[3], ";", file_name))
  }

  # dodajemy linie pliku do pełnych danych
  complete <- c(complete, full_tab)
}

complete <- c("samolot;data;trasa;premier", complete)
# zapisujemy pełne dane do pliku CSV
write_lines(complete, "dane/zapis_pdf.csv")

# poprawiamy ręcznie plik CSV np. w Excelu

dane <- read_csv2("dane/zapis_pdf.csv") %>%
  filter(!is.na(samolot)) %>%
  mutate(premier = str_replace_all(premier, "dane/|.pdf", "") %>%
           str_to_upper())

write_csv2(dane, "dane/zapis_ok.csv")
