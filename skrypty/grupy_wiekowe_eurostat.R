library(tidyverse)
library(eurostat)
library(scales)
library(giscoR)

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
  mutate(values = if_else(sex == "M", values*-1, values)) |> 
  left_join(nazwy_panstw)

#pop_range <- range(wiek_plec_pl$values)
#pop_range_seq <- seq(pop_range[1], pop_range[2], 5e4)
#pop_range_breaks <- pretty(pop_range, n = 10)


ggplot(wiek_plec_pl, aes(age, values, fill = sex))+
  geom_col() +
  #scale_y_continuous(breaks  = pop_range_breaks,
  #                   labels = abs(pop_range_breaks)/1000) +
  scale_y_continuous(labels =  ~ number_format(scale = .001, suffix = " tys.")(abs(.x)))+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  facet_wrap(~nazwa, scales = "free_x")+
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
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  #facet_wrap(~geo, scales = "free_x")+
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
  #scale_y_continuous(labels = label_percent())+
  coord_flip()+
  theme_minimal()


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
  mutate(values = if_else(sex == "M", odsetek*-1, odsetek))

zmiana_wiek_calosc |> 
  ggplot(aes(age, values, fill = sex))+
  geom_col() +
  scale_y_continuous(labels =  ~ number_format(scale = 100, suffix = "%")(abs(.x)))+
  #scale_y_continuous(breaks  = pop_range_breaks,
  #                   labels = abs(pop_range_breaks)/1000)+
  scale_fill_brewer(palette = "Set1")+
  coord_flip()+
  labs(y = "odsetek", 
       x = "grupa wiekowa", 
       fill = "płeć")+
  #facet_wrap(~geo, scales = "free_x")+
  #facet_wrap(~geo, ncol = 1)+
  theme_minimal()+
  theme(legend.position = "top")

ggsave(file = "./wykresy/zmiana_plec_rr.jpg")

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
library(tmap)
library(ggrepel)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggtext)

#mapa_world_raw <- ne_download(scale = 110, type = "countries")

world <- ne_countries(scale = 'medium',
                      returnclass = 'sf')

mapa_europa <- world |>
  select(CNTR_CODE = sov_a3, geometry) |> 
  filter(CNTR_CODE %in% c("KOS", "BIH", "UKR", "RUS", "MDA", "BLR", "AZE", "GEO", "ARM", "KAZ")) |> 
  st_make_valid()

#ggplot(data = ukraina) +
#  geom_sf()

#mapa_europa <- mapa_world_raw |> 
#  select(CNTR_CODE = POSTAL, geometry) |> 
#  filter(CNTR_CODE %in% c("KO", "BiH")) |> #, "RUS", "UA", "BY"
#  st_make_valid()

#ukraina <- mapa_world_raw |> 
#  filter(sovereignt == "Ukraine")

mapa <- get_eurostat_geospatial(
  resolution = "20",
  output_class = "sf",
  nuts_level = "0"
) |> 
  bind_rows(mapa_europa)

zmiana_mapa <- mapa |> 
  left_join(zmiana, by = c("CNTR_CODE" = "geo")) |> 
  #mutate(procent = paste0(round(zmiana, 3)*100, "%"))
  mutate(procent = percent(odsetek, accuracy = 0.1)) #|> 
  #st_crop(st_bbox(c(xmin=2400000, ymin=1320000, xmax=7800000, ymax=5650000)))

tm_shape(zmiana_mapa,
         projection = "EPSG:3035",
         xlim = c(2400000, 7800000),
         ylim = c(1320000, 5650000)
) +
  tm_fill("odsetek") +
  tm_borders()+
  tm_text("procent", size = 0.8)



ggplot(zmiana_mapa) +
  # Base layer
  geom_sf(aes(fill = odsetek), color = "grey40")+
  scale_fill_gradient2(low = "red3", mid = "white", high = "darkgreen", midpoint = 0, labels = label_percent())+
  #geom_sf_text_repel(aes(label = procent))+
  #geom_sf_label(data = subset(zmiana_mapa, geo != "LI" & geo != "LU"), aes(label = procent), size = 2)+
  geom_richtext(
    data = subset(zmiana_mapa, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    #size = 14 / .pt,
    size =3,
    colour = "black",
    #label.colour = "black",
    label.padding = unit(c(0.05, 0.05, 0.05, 0.05), "lines"),
    fill = "white",
    #fill = NA,
    alpha = 0.7,
    label.color = NA,
    #label.r = unit(0, "lines")
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
  theme(legend.position = c(0.8, 0.8))

ggsave(file = "./wykresy/mapa_zmiana_po_roku6.jpg", units = "in", width = 7, height = 5.5)

mapa_uchylanci <- mapa |> 
  left_join(odsetek_mlodych, by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))

ggplot(mapa_uchylanci) +
  # Base layer
  geom_sf(aes(fill = odsetek), color = "grey40")+
  scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = label_percent())+
  #geom_sf_text(data = subset(mapa_uchylanci, geo != "LI" & geo != "LU"), aes(label = procent), size = 3)+
  geom_richtext(
    data = subset(mapa_uchylanci, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    #size = 14 / .pt,
    size =3,
    colour = "black",
    #label.colour = "black",
    label.padding = unit(c(0.10, 0.10, 0.10, 0.10), "lines"),
    fill = "white",
    #fill = NA,
    alpha = 0.7,
    label.color = NA,
    #label.r = unit(0, "lines")
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
  theme(legend.position = c(0.8, 0.8),
        plot.margin = margin(0, 0, 0, 0))

ggsave(file = "./wykresy/mapa_uchylanci10.jpg", units = "in", width = 7, height = 5.5)


# uchylanci r/r

mapa_uchylanci2 <- mapa |> 
  left_join(uchylanci_2022, by = c("CNTR_CODE" = "geo")) |> 
  mutate(procent = percent(odsetek, accuracy = 0.1))

ggplot(mapa_uchylanci2) +
  # Base layer
  geom_sf(aes(fill = odsetek), color = "grey40")+
  #scale_fill_distiller(palette = "YlOrRd", direction = 1, labels = label_percent())+ #palette = "Spectral", direction = -1
  scale_fill_distiller(palette = "Spectral", direction = -1, labels = label_percent())+
  #geom_sf_text(data = subset(mapa_uchylanci, geo != "LI" & geo != "LU"), aes(label = procent), size = 3)+
  geom_richtext(
    data = subset(mapa_uchylanci2, geo != "LI" & geo != "LU"),
    aes(label = procent, geometry = geometry), 
    stat = "sf_coordinates",
    #size = 14 / .pt,
    size =3,
    colour = "black",
    #label.colour = "black",
    label.padding = unit(c(0.10, 0.10, 0.10, 0.10), "lines"),
    fill = "white",
    #fill = NA,
    alpha = 0.7,
    label.color = NA,
    #label.r = unit(0, "lines")
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
  theme(legend.position = c(0.8, 0.8))
