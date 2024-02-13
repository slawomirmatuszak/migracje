library(tidyverse)
library(haven)

CES_raw <- read_sav("./dane/refugees.sav")

# wyliczenia

calosc <- sum(uchodzcy_all$values)
panstwa_3 <- uchodzcy_all |> 
  filter(geo %in% c("CZ", "PL", "DE")) |> 
  ungroup() |> 
  summarise(liczba = sum(values)) |> 
  pull()

panstwa_3/calosc

wiek <- dane |> 
  filter(age %in% c("Y_LT18", "TOTAL"), 
         sex == "T")

grupy_wiekowe_all |> 
  group_by(sex) |> 
  summarise(values = sum(values))

dane |> 
  filter(geo != "EU27_2020", 
         sex %in% c("M", "F"), 
         age %in% "TOTAL") |> 
  group_by(sex) |> 
  summarise(values = sum(values)) 

test <- dane |> 
  filter(geo != "EU27_2020",
         #age %in% "TOTAL", 
         age %in% c("Y18-34", "Y35-64"), #, "Y_GE65"
         sex %in% c("M", "F")) |> 
  group_by(sex, geo) |> 
  summarise(values = sum(values)) |> 
  pivot_wider(names_from = sex, values_from = values) |> 
  mutate(wiekszosc = F>M, 
         odsetek = F/(F+M))
