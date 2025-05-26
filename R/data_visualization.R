library(readr) #czytanie danych
library(ggplot2) #wizualizacja
library(gridExtra) #wiele wykresow jednoczesnie
library(dplyr) #agregacja
library(tidyr)
library(tidyverse) #workflow, pipeliney

library(nortest) #dodatkowe testy normalnosci

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

grupa_kontrolna$rodzaj_testu = case_when(
  startsWith(grupa_kontrolna$nazwa_testu, "Las") ~ "Las",
  startsWith(grupa_kontrolna$nazwa_testu, "Most") ~ "Most",
  startsWith(grupa_kontrolna$nazwa_testu, "Stroop") ~ "Test Stroopa")

#POZBYCIE SIE ZBEDNYCH KOLUMN, WARTOSCI UJEMNYCH
grupa_badana  = grupa_badana %>% select(-rodzaj_grupy, -id_uczestnika, -nazwa_testu) 
grupa_badana = grupa_badana[apply(grupa_badana[, -13], 1, function(row) all(row>0)), ] 

grupa_kontrolna = grupa_kontrolna %>% select(-rodzaj_grupy, -id_uczestnika, -nazwa_testu) 
grupa_kontrolna = grupa_kontrolna[apply(grupa_kontrolna[, -13], 1, function(row) all(row>0)), ] 

View(grupa_badana)
View(grupa_kontrolna)

#CZYSZCZENIE DATA FRAMEÓW Z OUTLIERÓW
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
    lower = Q1 - 1.5 * iqr
    upper = Q3 + 1.5 * iqr

    df[[kolumna]][df[[kolumna]] < lower | df[[kolumna]] > upper] = NA
  }
  df = na.omit(df)
  return(df)
}

View(grupa_badana)


#TWORZENIE OSOBNYCH DATA FRAMEOW DLA KAZDEGO SCNEARIUSZU TESTU FILTRUJAC PO KOLUMNIE rodzaj_testu
#GRUPA BADANA
badana_las = grupa_badana[grupa_badana$rodzaj_testu == "Las",]
badana_most = grupa_badana[grupa_badana$rodzaj_testu == "Most",]
badana_stroop = grupa_badana[grupa_badana$rodzaj_testu == "Test Stroopa",]

grupa_badana_clean = drop_outliers(grupa_badana, parametry_numeryczne)
badana_las_clean = drop_outliers(badana_las, parametry_numeryczne)
badana_most_clean = drop_outliers(badana_most, parametry_numeryczne)
badana_stroop_clean = drop_outliers(badana_stroop, parametry_numeryczne)
  
#GRUPA KONTROLNA
kontrolna_las = grupa_kontrolna[grupa_kontrolna$rodzaj_testu == "Las",]
kontrolna_most = grupa_kontrolna[grupa_kontrolna$rodzaj_testu == "Most",]
kontrolna_stroop = grupa_kontrolna[grupa_kontrolna$rodzaj_testu == "Test Stroopa",]

grupa_kontrolna_clean = drop_outliers(grupa_kontrolna, parametry_numeryczne)
kontrolna_las_clean = drop_outliers(kontrolna_las, parametry_numeryczne)
kontrolna_most_clean = drop_outliers(kontrolna_most, parametry_numeryczne)
kontrolna_stroop_clean = drop_outliers(kontrolna_stroop, parametry_numeryczne)

View(badana_las)
View(badana_most)
View(badana_stroop)

#===================================================================================================================
#AGREGACJA GRUPY BADANEJ - GRUPOWANIE PO RODZAJU TESTU
#BADANA
aggr_badana = grupa_badana_clean %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))

#KONTROLNA
aggr_kontrolna = grupa_kontrolna_clean %>%
  group_by(rodzaj_testu) %>%
  summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))  

View(aggr_badana)
View(aggr_kontrolna)

aggr_las = badana_las_clean %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))
aggr_most = badana_most_clean %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))
aggr_stroop = badana_las_clean %>% summarise(across(where(is.numeric),list(srednia = mean, mediana = median, odchylenie = sd, maksim = max, minim = min)))

View(aggr_las)
#===================================================================================================================
#TESTY NORMALNOSCI

set.seed(1234)
alpha = 0.05 / 12 #korekta Bonferroniego

shapiro_test = function(df, alpha, tytul)
{
  testy = lapply(df, function(kolumna){
    if(is.numeric(kolumna))
    {
      wynik = shapiro.test(kolumna)
      return(wynik)
    }
    else
      { return(NULL)}
  })
  
  testy = Filter(Negate(is.null), testy)
  
  result_df = data.frame(
    #kolumna = names(testy),
    W_stat = sapply(testy, function(x) round(x$statistic, 4)),
    p_value = sapply(testy, function(x) x$p.value),
    normalnosc = sapply(testy, function(x) ifelse(x$p.value >= alpha, "normalny", "nienormalny"))
  )
  
  print(paste0("Test Shapiro-Wilka grupa ", tytul))
  print(result_df)
}

ks_test = function(df, alpha, tytul)
{
  testy = lapply(df, function(kolumna){
    if(is.numeric(kolumna))
    {
      wynik = ks.test(kolumna, "pnorm", mean = mean(kolumna), sd = sd(kolumna))
      return(wynik)
    }
    else
    { return(NULL)}
  })
  
  testy = Filter(Negate(is.null), testy)
  
  result_df = data.frame(
    #kolumna = names(testy),
    p_value = sapply(testy, function(x) x$p.value),
    normalnosc = sapply(testy, function(x) ifelse(x$p.value >= alpha, "normalny", "nienormalny"))
  )
  
  print(paste0("Test KS grupa ", tytul))
  print(result_df)
}

#GRUPY BADANE
shapiro_test(grupa_badana_clean, alpha, "badana: wszystkie scenariusze razem")
shapiro_test(badana_las_clean, alpha, "badana: scenariusz LAS")
shapiro_test(badana_most_clean, alpha, "badana: scenariusz MOST")
shapiro_test(badana_stroop_clean, alpha, "badana: scenariusz TEST STROOPA")

ks_test(grupa_badana_clean, alpha, "badana: wszystkie scenariusze razem")
ks_test(badana_las_clean, alpha, "badana: scenariusz LAS")
ks_test(badana_most_clean, alpha, "badana: scenariusz MOST")
ks_test(badana_stroop_clean, alpha, "badana: scenariusz TEST STROOPA")

#GRUPY KONTROLNE
shapiro_test(grupa_kontrolna_clean, alpha, "kontrolna: wszystkie scenariusze razem")
shapiro_test(kontrolna_las_clean, alpha, "kontrolna: scenariusz LAS")
shapiro_test(kontrolna_most_clean, alpha, "kontrolna: scenariusz MOST")
shapiro_test(kontrolna_stroop_clean, alpha, "kontrolna: scenariusz TEST STROOPA")

ks_test(grupa_kontrolna_clean, alpha, "kontrolna: wszystkie scenariusze razem")
ks_test(kontrolna_las_clean, alpha, "kontrolna: scenariusz LAS")
ks_test(kontrolna_most_clean, alpha, "kontrolna: scenariusz MOST")
ks_test(kontrolna_stroop_clean, alpha, "kontrolna: scenariusz TEST STROOPA")

#TESTY ISTOTNOŚCI RÓŻNIC MIĘDZY GRUPAMI
mann_whitney_test = function(df1, df2, alpha, tytul)
{
  testy = lapply(names(df1), function(kolumna){
    x = df1[[kolumna]]
    y = df2[[kolumna]]
    
    if(is.numeric(x) && is.numeric(y))
    {
      wynik = wilcox.test(x, y, exact = FALSE, alternative = "less")
      return(list(kolumna = kolumna, wynik = wynik)) 
    }
    else 
    { return(NULL) }
  })
  testy = Filter(Negate(is.null), testy)
  
  result_df = data.frame(
    kolumna = sapply(testy, function(x) x$kolumna),
    p_value = sapply(testy, function(x) x$wynik$p.value),
    ist_roznicy = sapply(testy, function(x) ifelse(x$wynik$p.value < alpha, "istotna", "nieistotna"))
  )
  print(paste0("Test istotnosci Manna-Whitneya: ", tytul))
  print(result_df)
}

mann_whitney_test(grupa_badana_clean, grupa_kontrolna_clean, alpha, "wszystkie scenariusze razem")
mann_whitney_test(badana_las_clean, kontrolna_las_clean, alpha, "scenariusz LAS")
mann_whitney_test(badana_most_clean, kontrolna_most_clean, alpha, "scenariusz MOST")
mann_whitney_test(badana_stroop_clean, kontrolna_stroop_clean, alpha, "scenariusz TEST STROOPA")


#===================================================================================================================
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
  par(mfrow = c(2, 2), mar = c(2, 2, 2, 1),oma = c(0, 0, 2, 0), cex.main = 1.1)
  
  for(parametr in parametry)
  {
    boxplot(df[[parametr]], main = parametr, col = kolor)
    mediana = round(median(df[[parametr]]),3)
    text(1.2, mediana, labels = mediana, pos = 4, cex = 0.9, col = "black")
  }
  mtext(nazwa, outer = TRUE, cex = 1.5, font = 2)
  
  dev.off()
}

for(i in seq_along(grupy_param))
{
  create_boxplots(grupy_param[[i]], grupa_badana_clean, paste0("badana_", tytuly[i]), kolory[i])
  create_boxplots(grupy_param[[i]], grupa_kontrolna_clean, paste0("kontrolna_", tytuly[i]), kolory[i])
  #create_boxplots(grupy_param[[i]], badana_las_clean, paste0("b_las_", tytuly[i]), kolory[i])
  #create_boxplots(grupy_param[[i]], badana_most_clean, paste0("b_most_", tytuly[i]), kolory[i])
  #create_boxplots(grupy_param[[i]], badana_stroop_clean, paste0("b_stroop_", tytuly[i]), kolory[i])
}

#===================================================================================================================
#HISTOGRAMY ROZKLADU PARAMETRÓW CHODU

create_hist = function(df, parametry, kolor_fill, kolor_density, binw = 0.03, adj= 2.0, limits, nazwa_pliku) 
{
  df_long = df %>% 
    select(all_of(parametry)) %>%
    pivot_longer(cols= everything(), names_to = "parametr", values_to = "wartosc")
 
  wykres = df_long %>% 
    ggplot(aes(x = wartosc))+
    geom_histogram(aes(y = after_stat(density)),binwidth = binw, fill = kolor_fill, color = "white") +
    geom_density(adjust = adj, fill = kolor_fill, color = kolor_density, alpha = 0.35)+
    facet_wrap(~ parametr, scales = "free") +
    labs(title = NULL, x = NULL, y = NULL) +
    theme_minimal() +
    theme(strip.text = element_text(size = 14))+
    coord_cartesian(xlim = limits)
    
  #png(paste0(nazwa_pliku, ".png"), width = 1920, height = 1080, res = 200)
  print(wykres)
  #dev.off()
}

kolory_dens = c("darkblue", "black", "darkgreen", "black") 
bins = c(0.03, 0.03, 0.03, 3)
adjusts = c(2.0, 2.5, 2.5, 3.0)
limits = list(c(0.25,NA), c(0, 1.0), c(0, NA), c(25, 100))
 
for(i in seq_along(grupy_param))
{
  #create_hist(df = grupa_badana, parametry = grupy_param[[i]], kolor_fill = kolory[i], 
  # kolor_density = kolory_dens[i], binw = bins[i], adj = adjusts[i], limits = limits[[i]], nazwa_pliku = tytuly[i])
  
  #create_hist(df = badana_most, parametry = grupy_param[[i]], kolor_fill = kolory[i], 
  # kolor_density = kolory_dens[i], binw = bins[i], adj = adjusts[i], limits = limits[[i]], nazwa_pliku = tytuly[i])
  
  #create_hist(df = badana_las, parametry = grupy_param[[i]], kolor_fill = kolory[i], 
  # kolor_density = kolory_dens[i], binw = bins[i], adj = adjusts[i], limits = limits[[i]], nazwa_pliku = tytuly[i])
  
  create_hist(df = badana_stroop, parametry = grupy_param[[i]], kolor_fill = kolory[i], 
              kolor_density = kolory_dens[i], binw = bins[i], adj = adjusts[i], limits = limits[[i]], nazwa_pliku = tytuly[i])
}

create_hist(badana_las, grupa1,"skyblue", "darkblue", 0.03, "b_cykle")
create_hist(badana_las, grupa2,"darkorange", "black", 0.03, "b_fazy")
create_hist(badana_las, grupa3,"limegreen", "darkgreen", 0.03, "b_fazy")
create_hist(badana_las, grupa4,"#C62828", "black", 3, "b_fazy")

#===================================================================================================================
#TWORZENIE QQ-PLOTÓW W CELU WERYFIKACJI NORMALNOŚCI ROZKŁADU

create_qqplots = function(df, parametry, kolor, tytul) 
{
  #Przekształcenie do formatu długiego
  df_long = df %>%
    pivot_longer(
      cols = all_of(parametry),
      names_to = "parametr",
      values_to = "wartosc")
  
  wykres = df_long %>%
    ggplot(aes(sample = wartosc)) +
    stat_qq(color = kolor) +
    stat_qq_line(color = "black") +
    facet_wrap(~ parametr, scales = "free") +
    theme_minimal() +
    labs(title = tytul, x = NULL, y = NULL) + #paste("QQ-plots:", tytul) x = "Teoretyczne", y = "Empiryczne
    theme(strip.text = element_text(size = 14), plot.title = element_text(hjust = 0.5))
  
  #Zapis do pliku
  png(paste0("qq_", tytul, ".png"), width = 1920, height = 1080, res = 200)
  print(wykres)
  dev.off()
}

for(i in seq_along(grupy_param))
{
  #create_qqplots(grupa_badana_clean, grupy_param[[i]], kolory[i], paste0("badana_", tytuly[i]))
  create_qqplots(badana_most_clean, grupy_param[[i]], kolory[i], paste0("badana_most_", tytuly[i]))
  create_qqplots(badana_stroop_clean, grupy_param[[i]], kolory[i], paste0("badana_stroop_", tytuly[i]))
  #create_qqplots(grupa_kontrolna_clean, grupy_param[[i]], kolory[i], paste0("kontrolna_", tytuly[i]))
}


#===================================================================================================================
#TWORZENIE ZBIORCZEGO WYKRESU WYNIKÓW AGREGACJI

create_barplot = function (df, nazwa_stat, y_label, tytul)
{
  df_long = df %>%
    pivot_longer(
      cols = -rodzaj_testu,
      names_to = c("parametr", "statystyka"),
      names_sep = "_(?=[^_]+$)")
  
  wykres = ggplot(filter(df_long, statystyka == nazwa_stat), aes(x = rodzaj_testu, y = value, fill = rodzaj_testu)) +
    geom_bar(stat = "identity", color = "white") +
    geom_text(aes(label = round(value, 3)),vjust = 1.5, size = 3, color = "black")+
    facet_wrap(~parametr, scales = "free_y") +
    labs(title = tytul, x = NULL, y = y_label, ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  
  png(paste0(tytul, ".png"), width = 1920, height = 1080, res = 200)
  print(wykres)
  dev.off()
}

#ZESTAWIENIE ŚREDNICH W ZALEŻNOŚCI OD RODZAJU TESTU
create_barplot(aggr_badana, "srednia", "Średnia", "srednie_badana")
create_barplot(aggr_kontrolna, "srednia", "Średnia", "srednie_kontrolna")

#ZESTAWIENIE ŚREDNICH W ZALEŻNOŚCI OD RODZAJU TESTU
create_barplot(aggr_badana, "mediana", "Mediana", "mediany_badana")
create_barplot(aggr_kontrolna, "mediana", "Mediana", "mediany_kontrolna")


#ZESTAWIENIE ODCHYLEŃ STANDARDOWYCH W ZALEŻNOŚCI OD RODZAJU TESTU
create_barplot(aggr_badana, "odchylenie", "Odchylenie standardowe", "odchylenia_b")
create_barplot(aggr_kontrolna, "odchylenie", "Odchylenie standardowe", "odchylenia_k")

