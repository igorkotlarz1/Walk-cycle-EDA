library(readr) #czytanie danych
library(ggplot2) #wizualizacja
library(dplyr) #agregacja
library(tidyr)
library(tidyverse)

#agregacja parametrow
#hist, box-plot, outliersi
#testy normalności
#cechy wskaujące różnice między grupami
#hipoteza stat. o wyższości jednej z grup nad drugą, weryfikacja

path_badani = "C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Badana_10_parametry_kroku.csv" 
path_kontrolna ="C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Kontrolna_10_parametry_kroku.csv"

#Wczytywanie danych
grupa_badana = read_delim(path_badani,delim = ";", escape_double = FALSE, trim_ws = TRUE)
grupa_kontrolna = read_delim(path_kontrolna, delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Dodanie etykiet kolumn dla Data Frame'ow
etykiety_kolumn = c("rodzaj_grupy", "id_uczestnika", "nazwa_testu", "cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L",
"dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", "tempo_kroku_R", "podw_podp", "pojed_podp")

colnames(grupa_badana) = etykiety_kolumn
colnames(grupa_kontrolna) = etykiety_kolumn

#Dodanie osobnej kolumny rodzaj_testu , aby skategoryzować dany rodzaj testu
grupa_badana$rodzaj_testu = case_when(
    startsWith(grupa_badana$nazwa_testu, "Las") ~ "Las",
    startsWith(grupa_badana$nazwa_testu, "Most") ~ "Most",
    startsWith(grupa_badana$nazwa_testu, "Stroop") ~ "Test Stroopa")

View(grupa_badana)
#View(grupa_kontrolna)

#Agregacja grupy badanej
aggr_badana = grupa_badana %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia =~mean(.x), mediana = ~median(.x),odchylenie = ~sd(.x))))

View(aggr_badana)

#WIZUALIZACJA DANYCH
n = length(grupa_badana$cykl_kroku_L)
bins = ceiling(log2(n)+1)


# Przekształcenie danych do formatu długiego
grupa_long <- grupa_badana %>%
  pivot_longer(
    cols = c(cykl_kroku_L, cykl_kroku_R, dl_fazy_podp_L, dl_fazy_podp_R,
             dl_fazy_przen_L, dl_fazy_przen_R, cykl_wykroku_L, cykl_wykroku_R,
             tempo_kroku_L, tempo_kroku_R, podw_podp, pojed_podp),
    names_to = "parametr",
    values_to = "wartosc"
  )

ggplot(grupa_long, aes(x = wartosc)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ parametr, scales = "free") +
  labs(title = "Histogramy parametrów", x = "Wartość", y = "Liczba") +
  theme_minimal()

#grupa_badana %>%
#    ggplot(aes(x = cykl_kroku_L)) + 
#    geom_histogram(binwidth = 0.05,bins =bins, fill = "steelblue", color = "white") +
#    geom_vline(aes(xintercept = median(cykl_kroku_L)),color = "red", linetype = "dashed", size = 1) +
#    labs(x = "Dlugosc cyklu (s)", y = "Liczba obserwacji", title = "Rozklad cyklu kroku lewej nogi")+
#    theme_minimal()

#grupa_badana %>%
#  ggplot(aes(x = cykl_kroku_R)) + 
#  geom_histogram(binwidth = 0.05,bins =bins, fill = "steelblue", color = "white") +
#  labs(x = "Dlugosc cyklu (s)", y = "Liczba obserwacji", title = "Rozklad cyklu kroku prawej nogi")+
#  theme_minimal()

#aggr_kontrolna = group_by(grupa_badana, nazwa_testu)

#View(aggr_kontrolna)

