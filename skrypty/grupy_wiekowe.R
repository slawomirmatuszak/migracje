library(tidyverse)

load("./dane/dane.Rda")

dane <- dane |> 
  filter(obywatelstwo == "UA")

wiek_plec_pl <- dane |> 
  filter(kraj %in% c("PL", "DE", "CZ", "ES", "RO", "BG"), 
         data == max(data), 
         plec %in% c("M", "F"), 
         wiek %in% c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65")) |> 
  mutate(wiek = factor(wiek, levels = c("Y_LT14", "Y14-17", "Y18-34", "Y35-64", "Y_GE65"))) |> 
  mutate(liczba = if_else(plec == "M", liczba*-1, liczba))

pop_range <- range(wiek_plec_pl$liczba)
#pop_range_seq <- seq(pop_range[1], pop_range[2], 5e4)
pop_range_breaks <- pretty(pop_range, n = 5)


ggplot(wiek_plec_pl, aes(wiek, liczba, fill = plec))+
  geom_col() +
  scale_y_continuous(breaks  = pop_range_breaks,
                     labels = abs(pop_range_breaks)/1000) +
  coord_flip()+
  facet_wrap(~kraj)+
  theme_bw()

# proporcje mężczyzn i kobiet
wiek_plec_pl |> 
  mutate(liczba = if_else(plec == "M", liczba*-1, liczba)) |> 
  group_by(kraj, wiek) |> 
  mutate(prop = round(liczba/sum(liczba),3)) |> 
  ggplot(aes(wiek, prop, fill = plec))+
  geom_col(position = "fill")+
  geom_text(aes(label = scales::percent(prop)), position = position_stack(.5))+
  coord_flip()+
  facet_wrap(~kraj)+
  theme_bw()


# liczba uchylantów w Polsce --------------------------------------

uchylanci <- wiek_plec_pl |> 
  filter(plec == "M", 
         wiek %in% c("Y18-34", "Y35-64")) |> 
  mutate(liczba = liczba*-1) |> 
  group_by(kraj) |> 
  summarise(liczba = sum(liczba))

