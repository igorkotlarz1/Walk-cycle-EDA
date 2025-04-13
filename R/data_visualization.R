library(readr)

path_badani = "C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Badana_10_parametry_kroku.csv" 
path_kontrolna ="C:/Users/igor/Desktop/MS_projekt/P03_A/ASD_Kontrolna_10_parametry_kroku.csv"
  
grupa_badana = read_delim(path_badani,delim = ";", escape_double = FALSE, trim_ws = TRUE)
grupa_kontrolna = read_delim(path_kontrolna, delim = ";", escape_double = FALSE, trim_ws = TRUE)

etykiety_kolumn = c("rodzaj_grupy", "id_uczestnika", "nazwa_pliku", "cykl_kroku_L", "cykl_kroku_R", "dl_fazy_podp_L", "dl_fazy_podp_R", "dl_fazy_przen_L",
"dl_fazy_przen_R", "cykl_wykroku_L", "cykl_wykroku_R", "tempo_kroku_L", "tempo_kroku_R", "podw_podp", "pojed_podp")

colnames(grupa_badana) = etykiety_kolumn
colnames(grupa_kontrolna) = etykiety_kolumn

View(grupa_badana)
View(grupa_kontrolna)
