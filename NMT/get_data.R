library(tidyverse)
library(rvest)

base_url <- "http://www.codgik.gov.pl/index.php/darmowe-dane/nmt-100.html"

data_urls <- read_html(base_url) %>% html_node("table") %>% html_nodes("a") %>% html_attr("href")

for(i in 1:length(data_urls)) {
  file_url <- data_urls[[i]]
  # sciagamy plik
  download.file(file_url, paste0("dane/", basename(file_url)))
  # rozpakowujemy go
  unzip(paste0("dane/", basename(file_url)), exdir = "dane/")
  Sys.sleep(1)
}

