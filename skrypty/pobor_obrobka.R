library(tidyverse)
library(lubridate)

# chyba alternatywa dla bulka
# https://ec.europa.eu/eurostat/databrowser/bulk?lang=en

# pobór danych
# pobór z bulka
linki <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Fmigr_asytpsm.tsv.gz"

# pobór z alternatywnego źródła (trzeba przerobić kod)
# linki <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/MIGR_ASYTPSM/?format=TSV&compressed=true"
dest <- "./dane/migracje.gz"

download.file(linki, dest, mode="wb")

rm(dest, linki)


# wstępna obróbka ---------------------------------------------------------

dane_raw <- read.table("./dane/migracje.gz", header = T) |> 
  mutate(across(2:18, ~as.numeric(.))) |> 
  #slice(1:100) |> 
  rename(nazwa = 1) |> 
  separate(col = nazwa, into = c(NA,"obywatelstwo","plec","wiek","kraj"), sep = ",")
  

dane <- dane_raw |> 
  pivot_longer(5:ncol(dane_raw)) |> 
  rename(data = name, liczba = value) |> 
  mutate(data = ymd(paste0(str_sub(data, 2,5), str_sub(data, 7,8), "01")))

save(dane, file = "./dane/dane.Rda")
