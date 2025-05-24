library(readr) #czytanie danych
library(ggplot2) #wizualizacja
library(gridExtra) #wiele wykresow jednoczesnie
library(dplyr) #agregacja
library(tidyr)
library(tidyverse) #workflow, pipeliney

path_badani = "C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Badana_10_parametry_kroku.csv"  #.xlsx
path_kontrolna ="C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Kontrolna_10_parametry_kroku.csv"

#WCZYTYWANIE DANYCH DO DATA FRAMEOW
grupa_badana = read_delim(path_badani,delim = ";", escape_double = FALSE, trim_ws = TRUE)
grupa_kontrolna = read_delim(path_kontrolna, delim = ";", escape_double = FALSE, trim_ws = TRUE)

#UTWORZENIE ETYKIET DLA KAŻDEJ KOLUMNY
etykiety_kolumn = c("rodzaj_grupy", "id_uczestnika", "nazwa_testu", "cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R",
                    "dl_fazy_przen_L","dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", 
                    "tempo_kroku_R", "podw_podp", "pojed_podp")

colnames(grupa_badana) = etykiety_kolumn
colnames(grupa_kontrolna) = etykiety_kolumn

#DODANIE OSOBNEJ KOLUMNY rodzaj_testu, W CELACH KATEGORYZACJI DANYCH
grupa_badana$rodzaj_testu = case_when(
    startsWith(grupa_badana$nazwa_testu, "Las") ~ "Las",
    startsWith(grupa_badana$nazwa_testu, "Most") ~ "Most",
    startsWith(grupa_badana$nazwa_testu, "Stroop") ~ "Test Stroopa")

#pozbycie sie zbednych kolumn, 
grupa_badana  = grupa_badana %>% select(-rodzaj_grupy, -id_uczestnika, -nazwa_testu) 
#pozbycie się rekordów z wartościami ujemnymi parametrów
grupa_badana = grupa_badana[apply(grupa_badana[, -13], 1, function(row) all(row>0)), ] 

View(grupa_badana)

parametry_numeryczne =  c("cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R",
                          "dl_fazy_przen_L","dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", 
                          "tempo_kroku_R", "podw_podp", "pojed_podp")

drop_outliers = function(df, parametry)
{
  for(kolumna in parametry)
  {
    Q1 = quantile(df[[kolumna]], 0.25, na.rm = TRUE)
    Q3 = quantile(df[[kolumna]], 0.75, na.rm = TRUE)
    iqr = Q3 - Q1
    lower = Q1 - 1.0 * iqr
    upper = Q3 + 1.0 * iqr
    
    #grupa_badana = grupa_badana[grupa_badana[[kolumna]] >= lower & grupa_badana[[kolumna]] <= upper, ]
    df[[kolumna]][df[[kolumna]] < lower | df[[kolumna]] > upper] = NA
  }
  df = na.omit(df)
  return(df)
}

View(grupa_badana)


#TWORZENIE OSOBNYCH DATA FRAMEOW DLA KAZDEGO SCNEARIUSZU TESTU FILTRUJAC grupa_badana PO KOLUMNIE rodzaj_testu
badana_las = grupa_badana[grupa_badana$rodzaj_testu == "Las",]
badana_most = grupa_badana[grupa_badana$rodzaj_testu == "Most",]
badana_stroop = grupa_badana[grupa_badana$rodzaj_testu == "Test Stroopa",]

badana_las = drop_outliers(badana_las, parametry_numeryczne)
badana_most = drop_outliers(badana_most, parametry_numeryczne)
badana_stroop = drop_outliers(badana_stroop, parametry_numeryczne)
  
View(badana_las)
View(badana_most)
View(badana_stroop)

#==================================================================================================================
#AGREGACJA GRUPY BADANEJ - GRUPOWANIE PO RODZAJU TESTU
aggr_badana = grupa_badana %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))
                 
View(aggr_badana)

aggr_las = badana_las %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))
aggr_most = badana_most %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))
aggr_stroop = badana_las %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))

View(aggr_las)
#===================================================================================================================
#TESTY NORMALNOSCI
set.seed(1234)
alpha = 0.05 / 12

normality_test = function(df, alpha)
{
  testy = lapply(df, function(kolumna){
    if(is.numeric(kolumna))
    {
      wynik = shapiro.test(kolumna)
      return(wynik) #$p.value
    }
    else
      { return(NULL)}
  })
  
  testy = Filter(Negate(is.null), testy)
  
  result_df = data.frame(
    kolumna = names(testy),
    p_value = sapply(testy, function(x) x$p.value),
    normalnosc = sapply(testy, function(x) ifelse(x$p.value >= alpha, "normalny", "nienormalny"))
  )
  
  print(result_df)
}

ks_test = function(df, alpha)
{
  testy = lapply(df, function(kolumna){
    if(is.numeric(kolumna))
    {
      wynik = ks.test(kolumna, "pnorm", mean = mean(kolumna), sd = sd(kolumna))
      return(wynik) #$p.value
    }
    else
    { return(NULL)}
  })
  
  testy = Filter(Negate(is.null), testy)
  
  result_df = data.frame(
    kolumna = names(testy),
    p_value = sapply(testy, function(x) x$p.value),
    normalnosc = sapply(testy, function(x) ifelse(x$p.value >= alpha, "normalny", "nienormalny"))
  )
  
  print(result_df)
}

normality_test(badana_las, alpha)
normality_test(badana_most, alpha)
ks_test(badana_las, alpha)
ks_test(badana_most, alpha)

#========================================================
#BOX PLOTY PARAMETRÓW CHODU

#grupowanie etykiet parametrow w celu wizualizacji histogramow
grupa1 = c("cykl_kroku_L", "cykl_kroku_R", "cykl_wykroku_L", "cykl_wykroku_R")
grupa2 = c("dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L", "dl_fazy_przen_R")
grupa3 = c("podw_podp", "pojed_podp")
grupa4 = c("tempo_kroku_L", "tempo_kroku_R")

grupy_param = list(grupa1,grupa2,grupa3,grupa4)
tytuly = c("cykle", "fazy", "podparcia", "tempa")
kolory = c("lightblue", "darkorange", "limegreen", "lightcoral")
  
create_boxplots = function(parametry, df, nazwa, kolor )
{
  nazwa_pliku = paste0("bp_", nazwa,".png")
  png(nazwa_pliku, width = 12, height = 8, units = "in", res = 300)
  par(mfrow = c(2, 2), mar = c(2, 2, 2, 1), cex.main = 1.1)
  
  for(parametr in parametry)
  {
    boxplot(df[[parametr]], main = parametr, col = kolor)
    mediana = round(median(df[[parametr]]),3)
    text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
  }
  dev.off()
}
  
create_boxplots(grupa1, badana_las,"cykle", "lightblue")
create_boxplots(grupa2, badana_las,"fazy", "darkorange")
create_boxplots(grupa3, badana_las,"podparcia", "limegreen")
create_boxplots(grupa4, badana_las,"tempa", "lightcoral")

for(i in seq_along(grupy_param))
{
  create_boxplots(grupy_param[[i]], badana_las, paste0("badana_las_", tytuly[i]), kolory[i])
}
for(i in seq_along(grupy_param))
{
  create_boxplots(grupy_param[[i]], badana_most, paste0("badana_most_", tytuly[i]), kolory[i])
}

for(i in seq_along(grupy_param))
{
  create_boxplots(grupy_param[[i]], badana_stroop, paste0("badana_stroop_", tytuly[i]), kolory[i])
}
#========================================================

#PRZEKSZTAŁCENIE DANYCH DO FORMATU DŁUGIEGO W CELU WYKREŚLENIA WIELU WYKRESÓW JEDNOCZEŚNIE
grupa_long = grupa_badana %>%
  pivot_longer(
    cols = c(cykl_kroku_L, cykl_kroku_R, dl_fazy_podp_L, dl_fazy_podp_R, dl_fazy_przen_L, dl_fazy_przen_R, cykl_wykroku_L, cykl_wykroku_R,
             tempo_kroku_L, tempo_kroku_R, podw_podp, pojed_podp),
    names_to = "parametr",
    values_to = "wartosc")

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
#========================================================
