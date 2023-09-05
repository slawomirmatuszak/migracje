library(tidyverse)
library(lubridate)

# pobór danych
linki <- "https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/BulkDownloadListing?sort=1&file=data%2Fmigr_asytpsm.tsv.gz"
dest <- "./dane/migracje.gz"

download.file(linki, dest, mode="wb")

rm(dest, linki)


# wstępna obróbka ---------------------------------------------------------

dane_raw <- read.table("./dane/migracje.gz", header = T) |> 
  mutate(across(2:18, ~as.numeric(.))) |> 
  #slice(1:100) |> 
  rename(nazwa = 1) |> 
  separate(col = nazwa, into = c(NA,"obywatelstwo","plec","wiek","kraj"), sep = ",") |> 
  filter(obywatelstwo == "UA")
  

dane <- dane_raw |> 
  pivot_longer(5:ncol(dane_raw)) |> 
  rename(data = name, liczba = value) |> 
  mutate(data = ymd(paste0(str_sub(data, 2,5), str_sub(data, 7,8), "01")))

wiek_plec_pl <- dane |> 
  filter(kraj %in% c("PL", "DE", "CZ"), 
         data == max(data), 
         plec %in% c("M", "F"), 
         wiek %in% c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65")) |> 
  mutate(wiek = factor(wiek, levels = c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"))) |> 
  mutate(liczba = if_else(plec == "M", liczba*-1, liczba))

pop_range <- range(wiek_plec_pl$liczba)
#pop_range_seq <- seq(pop_range[1], pop_range[2], 5e4)
pop_range_breaks <- pretty(pop_range, n = 7)


ggplot(wiek_plec_pl, aes(wiek, liczba, fill = plec))+
  geom_col() +
  scale_y_continuous(breaks  = pop_range_breaks,
                     labels = abs(pop_range_breaks)/1000) +
  coord_flip()+
  facet_wrap(~kraj)+
  theme_bw()
