library(tidyverse)
library(eurostat)
library(scales)

#test <- get_eurostat("MIGR_ASYTPSM")

nazwy_panstw <- data.frame(
  geo = c("AT", "BE", "BG", "CH", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI",
          "FR", "HR", "IE", "IS", "IT", "LI", "LT", "LU", "LV", "MT", "NL", "NO",
          "PL", "PT", "RO", "SE", "SI", "SK", "HU"),
  nazwa = c("Austria", "Belgia", "Bułgaria", "Szwajcaria", "Cypr", "Czechy", "Niemcy", "Dania",
            "Estonia", "Grecja", "Hiszpania", "Finlandia", "Francja", "Chorwacja", "Irlandia",
            "Islandia", "Włochy", "Liechtenstein", "Litwa", "Luksemburg", "Łotwa", "Malta",
            "Holandia", "Norwegia", "Polska", "Portugalia", "Rumunia", "Szwecja", "Słowenia",
            "Słowacja", "Węgry")
)


dane_raw <- get_eurostat("MIGR_ASYTPSM", 
                         filters = list(citizen = "UA"))#, 
                                        #age = "Y_LT14",
                                        #geo = "FR"))

dane <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(time == max(time))


# odsetek młodych mężczyzn ------------------------------------------------

odsetek_mlodych <- dane |> 
  filter(sex == "T" & age == "TOTAL" | sex == "M" & (age == "Y18-34"|age == "Y35-64")) |> 
  group_by(sex, geo) |> 
  summarise(values = sum(values, na.rm = T)) |> 
  pivot_wider(names_from = sex, values_from = values) |> 
  mutate(odsetek = M/T) |> 
  left_join(nazwy_panstw) |> 
  filter(!is.na(odsetek))


ggplot(odsetek_mlodych, aes(reorder(nazwa, odsetek), odsetek))+
  geom_col(fill = "steelblue")+
  scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_minimal()

# porównanie kilku państw i grup wiekowych --------------------------------

wiek_plec_pl <- dane |> 
  #filter(!is.na(liczba)) |> 
  filter(geo %in% c("PL", "DE", "CZ", "ES", "RO", "BG", "IT", "NL", "SK"), 
         sex %in% c("M", "F"), 
         age %in% c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65")) |>
  #group_by(kraj) |> 
  #filter(data == max(data)) |> 
  mutate(age = factor(age, levels = c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"))) |> 
  mutate(values = if_else(sex == "M", values*-1, values))

pop_range <- range(wiek_plec_pl$values)
#pop_range_seq <- seq(pop_range[1], pop_range[2], 5e4)
pop_range_breaks <- pretty(pop_range, n = 10)


ggplot(wiek_plec_pl, aes(age, values, fill = sex))+
  geom_col() +
  #scale_y_continuous(breaks  = pop_range_breaks,
  #                   labels = abs(pop_range_breaks)/1000) +
  scale_y_continuous(labels =  ~ number_format(scale = .001, suffix = " tys.")(abs(.x)))+
  coord_flip()+
  facet_wrap(~geo, scales = "free_x")+
  #facet_wrap(~geo)+
  theme_bw()


# polska i niemcy ---------------------------------------------------------

wiek_plec_pl |> 
  filter(geo %in% c("PL", "DE")) |> 
  ggplot(aes(age, values, fill = sex))+
  geom_col() +
  #scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " tys."))+
  scale_y_continuous(labels =  ~ number_format(scale = .001, suffix = " tys.")(abs(.x)))+
  #scale_y_continuous(breaks  = pop_range_breaks,
  #                   labels = abs(pop_range_breaks)/1000)+
  coord_flip()+
  #facet_wrap(~geo, scales = "free_x")+
  facet_wrap(~geo, ncol = 1)+
  theme_bw()

# liczba uchylantów -------------------------------------------------------

uchylanci <- dane |> 
  filter(sex == "M", 
         age %in% c("Y18-34", "Y35-64")) |> 
  group_by(geo) |> 
  summarise(values = sum(values))

ggplot(uchylanci, aes(reorder(geo, values), values))+
  geom_col(fill = "steelblue")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " tys."))+
  coord_flip()+
  theme_light()

# liczba niepełnoletnich --------------------------------------------------

dzieci <- dane |> 
  filter(sex == "T", 
         age %in% c("Y_LT14", "Y14-17")) |> 
  group_by(geo) |> 
  summarise(values = sum(values))

odsetek_dzieci <- dane |> 
  filter((sex == "T" & age == "TOTAL") | (sex == "T" & (age == "Y_LT14"|age == "Y14-17"))) |> 
  mutate(age = if_else(age == "TOTAL", age, "niepełnoletni")) |> 
  group_by(age, geo) |> 
  summarise(values = sum(values, na.rm = T)) |> 
  pivot_wider(names_from = age, values_from = values) |> 
  mutate(odsetek = `niepełnoletni`/TOTAL) |> 
  left_join(nazwy_panstw) |> 
  filter(!is.na(odsetek))

ggplot(odsetek_dzieci, aes(reorder(nazwa, odsetek), odsetek))+
  geom_col(fill = "steelblue")+
  scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_minimal()

# test francji

francja <- dane |> 
  filter(geo == "FR", sex == "T") |> 
  mutate(age = if_else(age == "TOTAL", age, "reszta")) |> 
  group_by(age) |> 
  summarise(values = sum(values))


# zmiana ukraińców w ciągu roku -------------------------------------------

zmiana1 <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(time == max(time), 
         age == "TOTAL", 
         sex == "T") |> 
  select(geo, gru_2023 = values)

# 2022-12-31
zmiana <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(time == "2022-12-01", 
         age == "TOTAL", 
         sex == "T") |> 
  select(geo, gru_2022 = values) |> 
  left_join(zmiana1) |> 
  mutate(zmiana = gru_2023 - gru_2022, 
         odsetek = -1+(gru_2023/gru_2022))

ggplot(zmiana, aes(reorder(geo, odsetek), odsetek))+
  geom_col(fill = "steelblue")+
  scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_minimal()

zmiana |> 
  filter(geo != "EU27_2020") |> 
ggplot(aes(reorder(geo, zmiana), zmiana))+
  geom_col(fill = "steelblue")+
  #scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_minimal()

# dynamika wybrane kraje --------------------------------------------------

dynamika <- dane_raw |> 
  filter(age == "TOTAL", 
         sex == "T", 
         geo %in% c("DE", "PL", "CZ")) # , "EU27_2020"

ggplot(dynamika, aes(time, values, col = geo))+
  geom_line(linewidth = 1.5) + 
  scale_y_continuous(limits = c(0, max(dynamika$values)),
                     labels = label_number(scale = 1e-6, suffix = " mln"))
