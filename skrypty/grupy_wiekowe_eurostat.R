library(tidyverse)
library(eurostat)
library(scales)
library(giscoR)

# funkcje -----------------------------------------------------------------

fun.zmiana.wieku <- function(x){
  
  out <- x
  out <- gsub("Y_LT14", "<14", out)
  out <- gsub("Y_GE65", ">64", out)
  out <- gsub("Y", "", out)
  out <- factor(out, levels = c("<14", "14-17", "18-34", "35-64", ">64"))
  return(out)
  
}


fun.interwaly <- function(x) {
  breaks <- c(0, 10000, 50000, 100000, 200000, 500000, 1000000, Inf)
  labels <- c("<10 tys.", "10-50 tys.", "50-100 tys.", "100-200 tys.", "200-500 tys.", "500 tys.-1 mln","> 1 mln")
  
  intervals <- cut(x, breaks = breaks, labels = labels, right = FALSE)
  return(intervals)
}

# pobór i wstępna obróbka -------------------------------------------------

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


# ogólna liczba w UE ------------------------------------------------------

EU27 <- dane_raw |> 
  filter(geo == "EU27_2020", 
         age == "TOTAL", 
         sex == "T", 
         time > "2022-11-01")

ggplot(EU27, aes(time, values))+
  geom_col()

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


# grupy wiekowe grudzien 2023.  ----------------------------------------------------

grupy_wiekowe_all <- dane |> 
  filter(geo != "EU27_2020", 
         sex %in% c("M", "F"), 
         age %in% c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65")) |> 
  group_by(sex, age) |> 
  summarise(values = sum(values)) |> 
  mutate(values = if_else(sex == "M", values*-1, values)) |> 
  mutate(age = fun.zmiana.wieku(age), 
         sex = gsub("F", "K", sex))

grupy_wiekowe_all |> 
  ggplot(aes(age, values, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = 1)(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(y = "liczba uchodźców", 
       x = "grupa wiekowa", 
       fill = "płeć")+
  theme_minimal()+
  theme(legend.position = "top")

ggsave(file = "./wykresy/grupy_wiekowe_gru_2023.jpg", units = "in", width = 6, height = 3.5)

# porównanie kilku państw i grup wiekowych --------------------------------

wiek_plec_pl <- dane |> 
  #filter(!is.na(liczba)) |> 
  filter(geo %in% c("PL", "DE", "CZ", "ES", "RO", "BG", "IT", "NL", "SK"), 
         sex %in% c("M", "F"), 
         age %in% c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65")) |>
  #group_by(kraj) |> 
  #filter(data == max(data)) |> 
  mutate(age = factor(age, levels = c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"))) |> 
  mutate(values = if_else(sex == "M", values*-1, values)) |> 
  left_join(nazwy_panstw) |> 
  mutate(age = fun.zmiana.wieku(age), 
         sex = gsub("F", "K", sex))

ggplot(wiek_plec_pl, aes(age, values, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = .001)(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  facet_wrap(~nazwa, scales = "free_x")+
  labs(y = "liczba uchodźców (w tys.)", 
       x = "grupa wiekowa", 
       fill = "płeć")+
  theme_minimal()+
  theme(legend.position = "top")

ggsave(file = "./wykresy/plec_9_panstw.jpg", units = "in", width = 6, height = 6)

# polska i niemcy ---------------------------------------------------------

wiek_plec_pl |> 
  filter(geo %in% c("PL", "DE")) |> 
  ggplot(aes(age, values, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = .001, suffix = " tys.")(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  facet_wrap(~nazwa, ncol = 1)+
  theme_bw()

# liczba uchylantów -------------------------------------------------------

uchylanci <- dane |> 
  filter(sex == "M", 
         age %in% c("Y18-34", "Y35-64")) |> 
  group_by(geo) |> 
  summarise(values = sum(values)) |> 
  left_join(nazwy_panstw)

ggplot(uchylanci, aes(reorder(nazwa, values), values))+
  geom_col(fill = "steelblue")+
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " tys."))+
  coord_flip()+
  theme_light()

uchylanci_2022 <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(sex == "M", 
         age %in% c("Y18-34", "Y35-64"), 
         time == "2022-12-01") |> 
  group_by(geo) |> 
  summarise(gru_2022 = sum(values)) |> 
  left_join(uchylanci) |> 
  mutate(roznica = values - gru_2022, 
         odsetek = -1+values/gru_2022)

uchylanci_2022 |> 
  ungroup() |> 
  summarise_if(is.numeric, sum)

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
  coord_flip()+
  theme_minimal()

zmiana |> 
  filter(geo != "EU27_2020") |> 
  ungroup() |> 
  summarise_if(is.numeric, sum)

# zmiana w ciągu roku - grupy wiekowe -------------------------------------
zmiana_wiek1 <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(time == max(time), 
         age != "TOTAL", 
         sex != "T") |> 
  select(geo, sex, age, gru_2023 = values)


zmiana_wiek <- dane_raw |> 
  group_by(geo) |> 
  filter(!is.na(values)) |> 
  filter(time == "2022-12-01", 
         age != "TOTAL", 
         sex != "T") |> 
  select(geo, sex, age, gru_2022 = values) |> 
  left_join(zmiana_wiek1) |> 
  mutate(zmiana = gru_2023 - gru_2022, 
         odsetek = -1+(gru_2023/gru_2022))

zmiana_wiek_calosc <- zmiana_wiek |> 
  group_by(sex, age) |> 
  summarise(gru_2023 = sum(gru_2023, na.rm = T),
            gru_2022 = sum(gru_2022, na.rm = T)) |> 
  mutate(zmiana = gru_2023 - gru_2022, 
         odsetek = -1+(gru_2023/gru_2022)) |> 
  filter(sex != "UNK" & age != "UNK") |> 
  filter(age != "Y_LT18") |> 
  mutate(age = factor(age, levels = c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"))) |> 
  mutate(values = if_else(sex == "M", odsetek*-1, odsetek)) |> 
  mutate(age = fun.zmiana.wieku(age), 
         sex = gsub("F", "K", sex))

# odsetek
zmiana_wiek_calosc |> 
  ggplot(aes(age, values, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = 100, suffix = "%")(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(y = "zmiana r/r", 
       x = "grupa wiekowa", 
       fill = "płeć")+
  theme_minimal()+
  theme(legend.position = "top")

ggsave(file = "./wykresy/zmiana_plec_rr.jpg", units = "in", width = 6, height = 3.5)

zmiana_wiek_calosc |> 
  mutate(zmiana = if_else(sex == "M", zmiana*-1, zmiana)) |> 
  ggplot(aes(age, zmiana, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = 1)(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(y = "liczba uchodźców", 
       x = "grupa wiekowa", 
       fill = "płeć")+
  theme_minimal()+
  theme(legend.position = "top")

ggsave(file = "./wykresy/zmiana_plec_rr_liczba.jpg", units = "in", width = 6, height = 3.5)


# dynamika wybrane kraje --------------------------------------------------

dynamika <- dane_raw |> 
  filter(age == "TOTAL", 
         sex == "T", 
         geo %in% c("DE", "PL", "CZ")) # , "EU27_2020"

ggplot(dynamika, aes(time, values, col = geo))+
  geom_line(linewidth = 1.5) + 
  scale_y_continuous(limits = c(0, max(dynamika$values)),
                     labels = label_number(scale = 1e-6, suffix = " mln"))


# mapa --------------------------------------------------------------------
library(sf)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)

# przygotowanie map

world <- ne_countries(scale = 'medium',
                      returnclass = 'sf')

mapa_europa <- world |>
  select(CNTR_CODE = sov_a3, geometry) |> 
  filter(CNTR_CODE %in% c("KOS", "BIH", "UKR", "RUS", "MDA", "BLR", "AZE", "GEO", "ARM", "KAZ")) |> 
  st_make_valid()


mapa <- get_eurostat_geospatial(
  resolution = "20",
  output_class = "sf",
  nuts_level = "0"
) |> 
  bind_rows(mapa_europa)

# temat dla map

temat <- list(theme(legend.position = c(0.8, 0.8),
                    plot.margin = margin(0, 0, 0, 0),
                    panel.border = element_blank(),
                    panel.spacing = unit(c(0, 0, 0, 0), "null"),
                    panel.background = element_rect(fill = "aliceblue", color = NA),
                    axis.text.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks = element_blank()) )



# # zmiana między 2022 i 2023 r.  -----------------------------------------

zmiana_mapa <- mapa |> 
  left_join(zmiana, by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))


ggplot(zmiana_mapa) +
  geom_sf(aes(fill = odsetek), color = "grey40")+
  scale_fill_gradient2(low = "red3", mid = "white", high = "darkgreen", midpoint = 0, labels = label_percent())+
  geom_richtext(
    data = subset(zmiana_mapa, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+ 
  temat

# można zmienić temat na void, wtedy jest idealnie dla worda
ggsave(file = "./wykresy/mapa_zmiana_po_roku.png", units = "in", width = 6.45, height = 5.48, bg="white")


# # liczba uchodźców wg państw --------------------------------------------

uchodzcy_all <- dane |> 
  filter(geo != "EU27_2020", 
         sex %in% c("T"), 
         age %in% c("TOTAL")) |> 
  mutate(etykieta = if_else(values > 1e6, paste(round(values/1e6, 2), "mln"), paste(round(values/1e3, 0), "tys.")))  |> 
  mutate(przedzial = fun.interwaly(values))

mapa_uchodzcy <- mapa |> 
  left_join(uchodzcy_all, by = c("CNTR_CODE" = "geo"))

ggplot(mapa_uchodzcy) +
  geom_sf(data = subset(mapa_uchodzcy, is.na(przedzial)), color = "grey40", fill = "grey50")+
  geom_sf(data = subset(mapa_uchodzcy, !is.na(przedzial)), aes(fill = przedzial), color = "grey40")+
  scale_fill_brewer(palette = "Greens", "", na.translate = FALSE) +
  geom_richtext(
    data = subset(mapa_uchodzcy, geo != "LI" & geo != "LU"),
    aes(label = etykieta, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+
  temat


ggsave(file = "./wykresy/mapa_uchodźcy_grudzien_2023.png", units = "in", width = 6.45, height = 5.48, bg="white")


# liczba uchodźców wg liczby ludności -------------------------------------

ludnosc_raw <- get_eurostat("tps00001")

ludnosc <- ludnosc_raw |> 
  group_by(geo) |> 
  filter(TIME_PERIOD == max(TIME_PERIOD), 
         geo %in% unique(dane$geo)) |> 
  select(geo, ludnosc = values) |> 
  left_join(uchodzcy_all) |> 
  filter(!is.na(values)) |> 
  mutate(per_100 = values*1e5/ludnosc, 
         etykieta = round(per_100), 
         etykieta = number_format(big.mark = " ")(etykieta))

mapa_ludnosc <- mapa |> 
  left_join(ludnosc, by = c("CNTR_CODE" = "geo"))

ggplot(mapa_ludnosc) +
  geom_sf(data = subset(mapa_ludnosc, is.na(per_100)), color = "grey40", fill = "grey50")+
  geom_sf(data = subset(mapa_ludnosc, !is.na(per_100)), aes(fill = per_100), color = "grey40")+
  scale_fill_distiller(palette = "Greens", direction = 1) +
  geom_richtext(
    data = subset(mapa_ludnosc, geo != "LI" & geo != "LU"),
    aes(label = etykieta, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+
  temat

ggsave(file = "./wykresy/mapa_uchodźcy_grudzien_100K.png", units = "in", width = 6.45, height = 5.48, bg="white")


# mapa uchylanci ----------------------------------------------------------

mapa_uchylanci <- mapa |> 
  left_join(odsetek_mlodych, by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))

ggplot(mapa_uchylanci) +
  geom_sf(aes(fill = odsetek), color = "grey40")+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = label_percent())+
  geom_richtext(
    data = subset(mapa_uchylanci, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    #fill = NA,
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+
  temat

ggsave(file = "./wykresy/mapa_uchylanci.png", units = "in", width = 6.45, height = 5.48, bg="white")


# uchylanci r/r -----------------------------------------------------------

mapa_uchylanci2 <- mapa |> 
  left_join(uchylanci_2022, by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))

ggplot(mapa_uchylanci2) +
  geom_sf(aes(fill = odsetek), color = "grey40")+
  scale_fill_distiller(palette = "Spectral", direction = -1, labels = label_percent())+
  geom_richtext(
    data = subset(mapa_uchylanci2, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    #fill = NA,
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+
  temat

ggsave(file = "./wykresy/mapa_uchylanci_zmiana.png", units = "in", width = 6.45, height = 5.48, bg="white")


# odsetek dzieci mapa -----------------------------------------------------

odsetek_dzieci_mapa <- mapa |> 
  left_join(subset(odsetek_dzieci, geo != "FR"), by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))

ggplot(odsetek_dzieci_mapa) +
  geom_sf(data = subset(odsetek_dzieci_mapa, is.na(odsetek)), color = "grey40", fill = "grey50")+
  geom_sf(data = subset(odsetek_dzieci_mapa, !is.na(odsetek)), aes(fill = odsetek), color = "grey40")+
  scale_fill_distiller(palette = "Greens", direction = 1, labels = label_percent()) +
  geom_richtext(
    data = subset(odsetek_dzieci_mapa, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    size =3,
    colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    alpha = 0.7,
    label.color = NA
  )+
  labs(x = NULL, 
       y = NULL, 
       fill = NULL)+
  theme_minimal()+
  coord_sf(
    xlim = c(2377294, 7453440),
    ylim = c(1313597, 5628510),
    crs = 3035
  )+
  temat

ggsave(file = "./wykresy/mapa_odsetek_niepelnoletnich.png", units = "in", width = 6.45, height = 5.48, bg="white")
