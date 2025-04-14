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

#WCZYTYWANIE DANYCH DO DATA FRAMEOW
grupa_badana = read_delim(path_badani,delim = ";", escape_double = FALSE, trim_ws = TRUE)
grupa_kontrolna = read_delim(path_kontrolna, delim = ";", escape_double = FALSE, trim_ws = TRUE)

#UTWORZENIE ETYKIET DLA KAŻDEJ KOLUMNY
etykiety_kolumn = c("rodzaj_grupy", "id_uczestnika", "nazwa_testu", "cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L",
"dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", "tempo_kroku_R", "podw_podp", "pojed_podp")

colnames(grupa_badana) = etykiety_kolumn
colnames(grupa_kontrolna) = etykiety_kolumn

#DODANIE OSOBNEJ KOLUMNY rodzaj_testu, W CELACH KATEGORYZACJI DANYCH
grupa_badana$rodzaj_testu = case_when(
    startsWith(grupa_badana$nazwa_testu, "Las") ~ "Las",
    startsWith(grupa_badana$nazwa_testu, "Most") ~ "Most",
    startsWith(grupa_badana$nazwa_testu, "Stroop") ~ "Test Stroopa")

View(grupa_badana)
#View(grupa_kontrolna)

#AGREGACJA GRUPY BADANEJ - GRUPOWANIE PO RODZAJU TESTU
aggr_badana = grupa_badana %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia =~mean(.x), mediana = ~median(.x),odchylenie = ~sd(.x))))

View(aggr_badana)

#WIZUALIZACJA DANYCH
n = length(grupa_badana$cykl_kroku_L)
bins = ceiling(log2(n)+1)

#PRZEKSZTAŁCENIE DANYCH DO FORMATU DŁUGIEGO W CELU WYKREŚLENIA WIELU WYKRESÓW JEDNOCZEŚNIE
grupa_long = grupa_badana %>%
  pivot_longer(
    cols = c(cykl_kroku_L, cykl_kroku_R, dl_fazy_podp_L, dl_fazy_podp_R,
             dl_fazy_przen_L, dl_fazy_przen_R, cykl_wykroku_L, cykl_wykroku_R,
             tempo_kroku_L, tempo_kroku_R, podw_podp, pojed_podp),
    names_to = "parametr",
    values_to = "wartosc"
  )

grupa1 = c("cykl_kroku_L", "cykl_kroku_R", "cykl_wykroku_L", "cykl_wykroku_R")
grupa2 = c("dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L", "dl_fazy_przen_R")
grupa3 = c("podw_podp", "pojed_podp")
grupa4 = c("tempo_kroku_L", "tempo_kroku_R")

#HISTOGRAMY ROZKLADU PARAMETRÓW CHODU
p1 = grupa_long %>%
  filter(parametr %in% grupa1) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(binwidth = 0.03, fill = "skyblue", color = "white") +
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL,  x = NULL, y = "Liczba obserwacji") +
  theme_minimal() +
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0.25, NA))

  #print(p1)
  
p2 = grupa_long %>%
  filter(parametr %in% grupa2) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(binwidth = 0.03, fill = "darkorange", color = "white") +
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL,  x = NULL, y = "Liczba obserwacji") +
  theme_minimal()+
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0, 1.0))

  #print(p2)
  
p3 = grupa_long %>%
  filter(parametr %in% grupa3) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(binwidth = 0.03, fill = "limegreen", color = "white") +
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL, x = NULL, y = "Liczba obserwacji") +
  theme_minimal()+
  theme(strip.text = element_text(size = 14))+
  coord_cartesian(xlim = c(0, NA))

  #print(p3)
  
p4 = grupa_long %>%
  filter(parametr %in% grupa4) %>%
  ggplot(aes(x = wartosc)) +
  geom_histogram(binwidth = 3, fill = "#C62828", color = "white") +
  facet_wrap(~ parametr, scales = "free") +
  labs(title = NULL,  x = NULL, y = "Liczba obserwacji") +
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
  
  png(nazwa_png, width = 1920, height = 1080, res = 200)
  print(p)
  dev.off()
  #ggsave(nazwa_png, plot = p, width = 10, height = 8, dpi = 300)
}
  
#TWORZENIE ZBIORCZEGO WYKRESU WYNIKÓW AGREGACJI
aggr_long = aggr_badana %>%
  pivot_longer(
    cols = -rodzaj_testu,
    names_to = c("parametr", "statystyka"),
    names_sep = "_(?=[^_]+$)"
  )

#ZESTAWIENIE ŚREDNICH W ZALEŻNOŚCI OD RODZAJU TESTU

#pdf("srednie_parametry_badani.pdf", width = 10, height = 8)
#png("srednie_parametry_badani.png", width = 1600, height = 1280, res =150)

ggplot(filter(aggr_long, statystyka == "srednia"), aes(x = rodzaj_testu, y = value, fill = rodzaj_testu)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(value, 3)),vjust = 2, size = 3, color = "black")+
  facet_wrap(~parametr, scales = "free_y") +
  labs(title = "Średnie parametrów w zależności od rodzaju testu", x = NULL, y = "Średnia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dev.off()

#ZESTAWIENIE ODCHYLEŃ STANDARDOWYCH W ZALEŻNOŚCI OD RODZAJU TESTU

#pdf("odchylenie_parametry_badani.pdf", width = 10, height = 8)
#png("odchylenie_parametry_badani.png", width = 1600, height = 1280, res =150)

ggplot(filter(aggr_long, statystyka == "odchylenie"), aes(x = rodzaj_testu, y = value, fill = rodzaj_testu)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = round(value, 3)),vjust = 1.5, size = 3, color = "black")+
  facet_wrap(~parametr, scales = "free_y") +
  labs(title = "Odchylenie standardowe od średniej parametrów w zależności od rodzaju testu", x = NULL, y = "Średnia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#dev.off()



#aggr_kontrolna = group_by(grupa_badana, nazwa_testu)

#View(aggr_kontrolna)

