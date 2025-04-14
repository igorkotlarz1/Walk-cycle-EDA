library(readr) #czytanie danych
library(ggplot2) #wizualizacja
library(gridExtra) #wiele wykresow jednoczesnie
library(dplyr) #agregacja
library(tidyr)
library(tidyverse) #workflow, pipeliney
#library(readxl)

path_badani = "C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Badana_10_parametry_kroku.csv"  #.xlsx
path_kontrolna ="C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Kontrolna_10_parametry_kroku.csv"

#WCZYTYWANIE DANYCH DO DATA FRAMEOW
grupa_badana = read_delim(path_badani,delim = ";", escape_double = FALSE, trim_ws = TRUE)
#grupa_badana = read_excel(path_badani)
grupa_kontrolna = read_delim(path_kontrolna, delim = ";", escape_double = FALSE, trim_ws = TRUE)

#UTWORZENIE ETYKIET DLA KAŻDEJ KOLUMNY
etykiety_kolumn = c("rodzaj_grupy", "id_uczestnika", "nazwa_testu", "cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R",
                    "dl_fazy_przen_L","dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", 
                    "tempo_kroku_R", "podw_podp", "pojed_podp")

colnames(grupa_badana) = etykiety_kolumn
colnames(grupa_kontrolna) = etykiety_kolumn

#grupa_badana = grupa_badana %>%
#  mutate(across(all_of(etykiety_kolumn[4:15]), as.numeric))

#DODANIE OSOBNEJ KOLUMNY rodzaj_testu, W CELACH KATEGORYZACJI DANYCH
grupa_badana$rodzaj_testu = case_when(
    startsWith(grupa_badana$nazwa_testu, "Las") ~ "Las",
    startsWith(grupa_badana$nazwa_testu, "Most") ~ "Most",
    startsWith(grupa_badana$nazwa_testu, "Stroop") ~ "Test Stroopa")

#AGREGACJA GRUPY BADANEJ - GRUPOWANIE PO RODZAJU TESTU
aggr_badana = grupa_badana %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd),
                   .names = "{.col}_{.fn}")) #zapis ktory tworzy etykiety nazwa_kolumny_funkcja_agregujaca

View(grupa_badana)
View(aggr_badana)

#===================================================================================================================

#PRZEKSZTAŁCENIE DANYCH DO FORMATU DŁUGIEGO W CELU WYKREŚLENIA WIELU WYKRESÓW JEDNOCZEŚNIE
grupa_long = grupa_badana %>%
  pivot_longer(
    cols = c(cykl_kroku_L, cykl_kroku_R, dl_fazy_podp_L, dl_fazy_podp_R, dl_fazy_przen_L, dl_fazy_przen_R, cykl_wykroku_L, cykl_wykroku_R,
          tempo_kroku_L, tempo_kroku_R, podw_podp, pojed_podp),
    names_to = "parametr",
    values_to = "wartosc")

#grupowanie etykiet parametrow w celu wizualizacji histogramow
grupa1 = c("cykl_kroku_L", "cykl_kroku_R", "cykl_wykroku_L", "cykl_wykroku_R")
grupa2 = c("dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L", "dl_fazy_przen_R")
grupa3 = c("podw_podp", "pojed_podp")
grupa4 = c("tempo_kroku_L", "tempo_kroku_R")

#BOX PLOTY PARAMETRÓW CHODU
{
#cykle
png("boxplot_cykle.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(2, 2, 2, 1), cex.main = 1.1)

for(parametr in grupa1)
{
  boxplot(grupa_badana[[parametr]], main = parametr, col = "lightblue")
  mediana = round(median(grupa_badana[[parametr]]),3)
  text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
}
dev.off()

#fazy 
png("boxplot_fazy.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(2, 2, 2, 1), cex.main = 1.1)

for(parametr in grupa2)
{
  boxplot(grupa_badana[[parametr]], main = parametr, col = "darkorange")
  mediana = round(median(grupa_badana[[parametr]]),3)
  text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
}
dev.off()

#podparcia
png("boxplot_podparcia.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(1,2), mar = c(2, 2, 2, 1), cex.main = 1.1)

for(parametr in grupa3)
{
  boxplot(grupa_badana[[parametr]], main = parametr, col = "limegreen")
  mediana = round(median(grupa_badana[[parametr]]),3)
  text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
}
dev.off()

#tempa
png("boxplot_tempa.png", width = 12, height = 8, units = "in", res = 300)
par(mfrow = c(1,2), mar = c(2, 2, 2, 1), cex.main = 1.1)

for(parametr in grupa4)
{
  boxplot(grupa_badana[[parametr]], main = parametr, col = "lightcoral")
  mediana = round(median(grupa_badana[[parametr]]),3)
  text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
}
dev.off()
}
#HISTOGRAMY ROZKLADU PARAMETRÓW CHODU
{
#cykle
p1 = grupa_long %>%
  filter(parametr %in% grupa1) %>%
  ggplot(aes(x = wartosc)) + 
  geom_histogram(aes(y = after_stat(density)),binwidth = 0.03, fill = "skyblue", color = "white") +
  geom_density(adjust = 2.0,fill = "skyblue",color = "darkblue", alpha = 0.35)+
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal() +
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0.25, NA))

  #print(p1)

#fazy 
p2 = grupa_long %>%
  filter(parametr %in% grupa2) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(aes(y = after_stat(density)),binwidth = 0.03, fill = "darkorange", color = "white") +
  geom_density(adjust = 2.5,fill = "orange",color =  "black", alpha = 0.35)+
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal()+
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0, 1.0))

  #print(p2)

#podparcia
p3 = grupa_long %>%
  filter(parametr %in% grupa3) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(aes(y = after_stat(density)),binwidth = 0.03, fill = "limegreen", color = "white") + #aes(y = after_stat(density))
  geom_density(adjust = 2.5,fill = "limegreen",color = "darkgreen", alpha = 0.35)+
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL, x = NULL, y = NULL) + #y = "Liczba obserwacji"
  theme_minimal()+
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0, NA))
  #print(p3)

#tempa
p4 = grupa_long %>%
  filter(parametr %in% grupa4) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(aes(y = after_stat(density)),binwidth = 3, fill = "#C62828", color = "white") +
  geom_density(adjust = 3.0,fill = "pink",color = "black", alpha = 0.35)+
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal()+
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(25, 100))

  #print(p4)

#SKRYPT DO AUTOMATYCZNEGO ZAPISYWANIA WYKRESÓW DO FORMATU .png 
histogramy = list(p1,p2,p3,p4) 
tytuly_hist = c("hist_cykle", "hist_fazy", "hist_podparcia","hist_tempa")

for(i in seq_along(histogramy))
{
  p = histogramy[[i]]
  tytul = tytuly_hist[i]
  nazwa_png = paste0(tytul,".png")
  nazwa_png_2 = paste0(tytul,"_norm",".png")
  
  png(nazwa_png_2, width = 1920, height = 1080, res = 200)
  print(p)
  dev.off()
}
  
#TWORZENIE ZBIORCZEGO WYKRESU WYNIKÓW AGREGACJI
aggr_long = aggr_badana %>%
  pivot_longer(
    cols = -rodzaj_testu,
    names_to = c("parametr", "statystyka"),
    names_sep = "_(?=[^_]+$)"
  )
}
#ZESTAWIENIE ŚREDNICH W ZALEŻNOŚCI OD RODZAJU TESTU
{
png("srednie_parametry_badani.png", width = 1920, height = 1080, res = 200)

ggplot(filter(aggr_long, statystyka == "srednia"), aes(x = rodzaj_testu, y = value, fill = rodzaj_testu)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(value, 3)),vjust = 2, size = 3, color = "black")+
  facet_wrap(~parametr, scales = "free_y") +
  labs(title = NULL, x = NULL, y = "Średnia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

#ZESTAWIENIE ODCHYLEŃ STANDARDOWYCH W ZALEŻNOŚCI OD RODZAJU TESTU

png("odchylenie_parametry_badani.png", width = 1920, height = 1080, res = 200)

ggplot(filter(aggr_long, statystyka == "odchylenie"), aes(x = rodzaj_testu, y = value, fill = rodzaj_testu)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(value, 3)),vjust = 1.5, size = 3, color = "black")+
  facet_wrap(~parametr, scales = "free_y") +
  labs(title = NULL, x = NULL, y = "Odchylenie standardowe") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()
}

