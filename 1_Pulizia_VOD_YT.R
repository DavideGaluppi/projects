# ---- libraries ----


library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyr)
require(pacman)
library(fuzzyjoin)
library(tidyr)
library(zoo)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())

# ---- WORK ----

##### PULIZIA DATASET #####

#### SET WD --> Datasets > VOD + YouTube

#### divido il dataset in VOD E YT ####
df_orig <- import('DATASET_TV_VOD.xlsx')
df_youtube <- df_orig %>% filter(Contenuto=='YOUTUBE')
df_vod <- df_orig %>% filter(Contenuto=='VOD')

#### da dataset FCP ESPERIMENTI per avere le due visioni di ogni partecipante ####

#### SET WD --> Datasets 

fcp_esperimenti <- import("FCP_esperimenti_20240416.xlsx", sheet = "REVISED 13.05")
contenuti_mostrati <- fcp_esperimenti %>% filter (`Lineare/VOD` == "VOD" & Device == "TV")

names(contenuti_mostrati)[names(contenuti_mostrati) == "ID partecipante"] <- "PARTECIPANTE"
contenuti_mostrati$PARTECIPANTE <- as.numeric(contenuti_mostrati$PARTECIPANTE)
contenuti_mostrati$PARTECIPANTE <- ifelse(contenuti_mostrati$PARTECIPANTE >= 400 & contenuti_mostrati$PARTECIPANTE <= 424, contenuti_mostrati$PARTECIPANTE + 600, contenuti_mostrati$PARTECIPANTE)

names(contenuti_mostrati)[names(contenuti_mostrati) == "Presenza partecipante"] <- "PRESENTE"
contenuti_mostrati$PRESENTE <- toupper(contenuti_mostrati$PRESENTE)
contenuti_mostrati = contenuti_mostrati %>% filter(PRESENTE == "SI")

contenuti_mostrati <- contenuti_mostrati[-85,]
contenuti_mostrati = contenuti_mostrati[,-c(1:9)]
contenuti_mostrati = contenuti_mostrati[,-c(11:38)]

names(contenuti_mostrati)[names(contenuti_mostrati) == "Primo contenuto"] <- "VISIONE1"
contenuti_mostrati$visione_vod <- ifelse(contenuti_mostrati$VISIONE1 == "VOD", "1", "2")
contenuti_mostrati$visione_yt <- ifelse(contenuti_mostrati$VISIONE1 == "YouTube", "1", "2")

contenuti_vod = contenuti_mostrati %>% select (PARTECIPANTE, "VOD piattaforma", "Video VOD", visione_vod)
contenuti_yt= contenuti_mostrati %>% select (PARTECIPANTE, "Video YT", visione_yt)

#### creo dataset pubblicita ####

## VOD ##
pubblicita_vod <- read_excel("Schema adv YT e VOD.xlsx", sheet = "Sheet1")
colnames(pubblicita_vod) <- as.character(unlist(pubblicita_vod[1,]))
pubblicita_vod <- pubblicita_vod[-1,]
pubblicita_vod <- pubblicita_vod[,-1]
pubblicita_vod$Categoria <- na.locf(pubblicita_vod$Categoria, na.rm = FALSE)
pubblicita_vod$Contenuto <- toupper(pubblicita_vod$Contenuto)
pubblicita_vod$Contenuto[pubblicita_vod$Contenuto == "TG LA7 (DATAROOM)"] <- 'TG LA7'

## YouTube ##
pubblicita_yt <- read_excel("Schema adv YT e VOD.xlsx", sheet = "YOUTUBE FINALE")
pubblicita_yt <- pubblicita_yt[-c(22,23,24),]
colnames(pubblicita_yt) <- as.character(unlist(pubblicita_yt[1,]))
pubblicita_yt <- pubblicita_yt[-1,]
pubblicita_yt = pubblicita_yt[,-c(1,7)]
pubblicita_yt$`Titolo video`[pubblicita_yt$`Titolo video` == "Da operaio a IMPRENDITORE partendo da zero (come ho fatto)"] <- "Da operaio a IMPRENDITORE"
pubblicita_yt$`Titolo video`[pubblicita_yt$`Titolo video` == "Ma è vero che siamo i migliori a RICICLARE? E se è così COME ABBIAMO FATTO?"] <- "Ma è vero che siamo i migliori a RICICLARE?"
pubblicita_yt$`Titolo video` <- toupper(pubblicita_yt$`Titolo video`)
pubblicita_yt$`Titolo video`[pubblicita_yt$`Titolo video` == "NOTE DI STILE, IL VIDEOPODCAST DI COSMOPOLITAN DA SANREMO 2024: GIULIA DE LELLIS"] <- "NOTE DI STILE"



### creo chiave ###
names(contenuti_vod)[names(contenuti_vod) == "PROGRAMMA"] <- "Contenuto"
names(contenuti_yt)[names(contenuti_yt) == "VIDEO"] <- "Titolo video"

names(contenuti_vod)[names(contenuti_vod) == "Video VOD"] <- "Contenuto"
names(contenuti_yt)[names(contenuti_yt) == "Video YT"] <- "Titolo video"


### unisco pubblicità ai programmi ###
# sistemo titoli video #
contenuti_yt$`Titolo video` <- toupper(contenuti_yt$`Titolo video`)
contenuti_yt$`Titolo video`[contenuti_yt$`Titolo video` == "NOTE DI STILE,  IL VIDEOPODCAST DI COSMOPOLITAN DA SANREMO 2024: GIULIA DE LELLIS"] <- "NOTE DI STILE"
# sistemo titoli vod #
contenuti_vod$Contenuto[contenuti_vod$Contenuto == "Alberto Angela - Stanotte"] <- "Alberto Angela - Stanotte a Parigi"
contenuti_vod$Contenuto <- toupper(contenuti_vod$Contenuto)
contenuti_vod$Contenuto[contenuti_vod$Contenuto == "TG LA7 (DATAROOM)"] <- 'TG LA7'


contenuti_vod <- fuzzy_left_join(
  contenuti_vod, pubblicita_vod,
  by = "Contenuto",
  match_fun = function(x, y) stringr::str_detect(x, y)
)

contenuti_vod = contenuti_vod[,-6]
names(contenuti_vod)[names(contenuti_vod) == "Contenuto.x"] <- "Programma"

contenuti_yt <- fuzzy_left_join(
  contenuti_yt, pubblicita_yt,
  by = "Titolo video",
  match_fun = function(x, y) stringr::str_detect(x, y)
)

contenuti_yt = contenuti_yt[,-4]
names(contenuti_yt)[names(contenuti_yt) == "Titolo video.x"] <- "Video"


#### unisco tutto ####

names(contenuti_vod)[names(contenuti_vod) == "PARTECIPANTE"] <- "ID"
names(contenuti_yt)[names(contenuti_yt) == "PARTECIPANTE"] <- "ID"

df_vod$ID = as.numeric(df_vod$ID)
df_vod <- left_join(df_vod, contenuti_vod, by = "ID")

df_youtube$ID = as.numeric(df_youtube$ID)
df_youtube <- left_join(df_youtube, contenuti_yt, by = "ID")

### sistemo i video ###

### avvicino le colonne di interesse ###
col_names <- colnames(df_vod)
last_eight <- tail(col_names, 8)
first_eight <- col_names[1:8]
remaining <- setdiff(col_names, c(first_eight, last_eight))
new_order <- c(first_eight, last_eight, remaining)
df_vod <- df_vod[, new_order]

col_names <- colnames(df_youtube)
last_six <- tail(col_names, 6)
first_eight <- col_names[1:8]
remaining <- setdiff(col_names, c(first_eight, last_six))
new_order <- c(first_eight, last_six, remaining)
df_youtube <- df_youtube[, new_order]

### tolgo righe inutili ###
df_vod <- df_vod %>% filter(Label != "SCREEN RECORDING 1")
df_youtube <- df_youtube %>% filter(Label != "YOUTUBE")

df_vod <- df_vod %>%
  group_by(ID, Start..ms.) %>%
  slice(1) %>%
  ungroup()

df_youtube <- df_youtube %>%
  group_by(ID, Start..ms.) %>%
  slice(1) %>%
  ungroup()


#### sostituisco valori non binning con il rispettivo contenuto ####

# # ID 1175 a mano perche manca ultimo F3... 
# prova <- df_vod %>% filter(ID == 1003)
# prova <- prova %>% 
#   mutate(x = Start..ms.[10] - Start..ms.[8]) 
# durata_orasi <- prova$x[1] # 15 secondi = 15000 ms 
# rm(prova)
# 
# la riga in cui riiniza 4 Hoteel è "BINNING_5S (INSTANCE 086)"
# 

df_vod[6638,8]<-"F3_ANNOTATIONS (INSTANCE 006)"


# Funzione per sostituire i valori --- YT
replace_values <- function(data) {
  non_binning_indices <- which(!grepl("BINNING", data$Label, ignore.case = TRUE))
  
  if(length(non_binning_indices) >= 6) {
    data$Label[non_binning_indices[1]] <- data$ADV_1[non_binning_indices[1]]
    data$Label[non_binning_indices[2]] <- data$ADV_2[non_binning_indices[2]]
    data$Label[non_binning_indices[3]] <- data$Video[non_binning_indices[3]]
    data$Label[non_binning_indices[4]] <- data$ADV_3[non_binning_indices[4]]
    data$Label[non_binning_indices[5]] <- data$ADV_4[non_binning_indices[5]]
    data$Label[non_binning_indices[6]] <- data$Video[non_binning_indices[6]]
  }
  return(data)
}
df_youtube <- df_youtube %>%
  group_by(ID) %>%
  arrange(ID) %>%
  group_modify(~ replace_values(.x)) %>%
  ungroup()

# Funzione per sostituire i valori --- VOD
replace_values <- function(data) {
  non_binning_indices <- which(!grepl("BINNING", data$Label, ignore.case = TRUE))
  
  if(length(non_binning_indices) >= 6) {
    data$Label[non_binning_indices[1]] <- data$ADV_1[non_binning_indices[1]]
    data$Label[non_binning_indices[2]] <- data$ADV_2[non_binning_indices[2]]
    data$Label[non_binning_indices[3]] <- data$Programma[non_binning_indices[3]]
    data$Label[non_binning_indices[4]] <- data$ADV_3[non_binning_indices[4]]
    data$Label[non_binning_indices[5]] <- data$ADV_4[non_binning_indices[5]]
    data$Label[non_binning_indices[6]] <- data$Programma[non_binning_indices[6]]
  }
  return(data)
}
df_vod <- df_vod %>%
  group_by(ID) %>%
  arrange(ID) %>%
  group_modify(~ replace_values(.x)) %>%
  ungroup()


### sostituisco ai valori binning il contenuto corrispondente ###

fill_binning_values <- function(data) {
  data <- data %>%
    mutate(
      New_Label = Label, 
      New_Label = ifelse(grepl("BINNING", New_Label, ignore.case = TRUE), NA, New_Label)
    ) %>%
    fill(New_Label, .direction = "down")
  
  return(data)
}

df_youtube <- df_youtube %>%
  group_by(ID) %>%
  arrange(ID) %>%
  group_modify(~ fill_binning_values(.x)) %>%
  ungroup()
df_youtube <- df_youtube %>%
  relocate(New_Label, .after = Label)

df_vod <- df_vod %>%
  group_by(ID) %>%
  arrange(ID) %>%
  group_modify(~ fill_binning_values(.x)) %>%
  ungroup()
df_vod <- df_vod %>%
  relocate(New_Label, .after = Label)

### tolgo colonne inutili ###

df_vod <- df_vod[,-c(5,6,7)]
df_youtube <- df_youtube[,-c(5,6,7)]




### merge dataset ### 

df_yt <- df_youtube
names(df_vod)[names(df_vod) == "visione_vod"] <- "VISIONE"
names(df_yt)[names(df_yt) == "visione_yt"] <- "VISIONE"

df_vod_yt <- bind_rows(df_vod, df_yt)
df_vod_yt <- df_vod_yt %>% arrange(ID,VISIONE)

df_vod_yt <- df_vod_yt %>%
  select(ID:Programma, "VOD piattaforma", Video, everything())

names(df_vod_yt)[names(df_vod_yt) == "Contenuto"] <- "VOD_YT"
names(df_vod_yt)[names(df_vod_yt) == "Label"] <- "Binning"
names(df_vod_yt)[names(df_vod_yt) == "New_Label"] <- "Label"
names(df_vod_yt)[names(df_vod_yt) == "Programma"] <- "Programma_VOD"
names(df_vod_yt)[names(df_vod_yt) == "VOD piattaforma"] <- "Piattaforma_VOD"
names(df_vod_yt)[names(df_vod_yt) == "Video"] <- "Video_YT"

df_vod_yt <- df_vod_yt %>% 
  select(ID:VISIONE, Piattaforma_VOD, Programma_VOD, Categoria, Video_YT, everything())


# ---- PArte A e B ----
colnames(df_vod_yt) <- gsub("mean.", "Value.", colnames(df_vod_yt), ignore.case = TRUE)
colnames(df_vod_yt) <- gsub("Average.", "Value.", colnames(df_vod_yt), ignore.case = TRUE)
colnames(df_vod_yt) <- gsub("Peaks.", "Value.Peaks.", colnames(df_vod_yt), ignore.case = TRUE)

df_vod_yt <- df_vod_yt %>%
  mutate(Pubblicita = ifelse(Label == ADV_1 | Label == ADV_2 | Label == ADV_3 | Label == ADV_4, 1, 0))

#### aggiungo pubblicita e creo visione A e B ####

df_vod_A <- df_vod_yt %>%
  group_by(ID,VOD_YT) %>%
  filter(Pubblicita==0) %>% 
  filter(VOD_YT == "VOD") %>% 
  mutate(
    diff_start_ms = Start..ms. - lag(Start..ms., default = first(Start..ms.)), # calcola la differenza
    temp_new_column = ifelse(diff_start_ms < 10000, 1, 0), # assegna 1 se la differenza è maggiore di 30.000, altrimenti 0
    ParteA = cummin(temp_new_column) # Usa cummin per propagare il primo zero a tutte le righe successive
  ) %>%
  select(-temp_new_column) # Rimuovi la colonna temporanea

df_yt_A <- df_vod_yt %>%
  group_by(ID,VOD_YT) %>%
  filter(Pubblicita==0) %>% 
  filter(VOD_YT == "YOUTUBE") %>% 
  mutate(
    diff_start_ms = Start..ms. - lag(Start..ms., default = first(Start..ms.)), # calcola la differenza
    temp_new_column = ifelse(diff_start_ms < 10000, 1, 0), # assegna 1 se la differenza è minore di 30.000, altrimenti 0
    ParteA = cummin(temp_new_column) # Usa cummin per propagare il primo zero a tutte le righe successive
  ) %>%
  select(-temp_new_column) 

df_A <- rbind(df_vod_A, df_yt_A)

df_vod_yt <- left_join(df_vod_yt,df_A[,c(1,10,16,33)],by=c("ID", "VISIONE", "Start..ms."))
df_vod_yt <- df_vod_yt%>%
  mutate(ParteA = ifelse(is.na(ParteA), 0, ParteA))

rm(df_A,df_vod_A,df_yt_A)

df_vod_yt <- df_vod_yt[-which(df_vod_yt$ID == 1273),] 
df_vod_yt <- df_vod_yt[-which(df_vod_yt$ID >= 1284),] 


# export(df_vod_yt, "DATASET_VOD_YOUTUBE.xlsx")

