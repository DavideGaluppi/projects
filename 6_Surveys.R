# ---- Libraries ----
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyr)
require(pacman)
library(corrplot)
library(writexl)
library(nlme)

# -------------------------------------------------------------------------


pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())

# ---- Work ----

#### SET WD --> Datasets > Surveys

# ---- carico file che mi serviranno ----
setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Surveys")
df_survey_1 <- import('FCP_Post_visione_1_def_June 7, 2024_09.20.xlsx')
df_survey_2 <- import('FCP_Post_visione_2_def_June 7, 2024_10.01.xlsx')
df_pre <- import("FCP_Pre_visione - RevL_June 7, 2024_10.02.xlsx")

#### SET WD --> Datasets > Lineare

setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
df_orig <- import('Metriche_Standardized.xlsx') 

# ---- Pulizia data survey ----
### pulizia df_survey 1 e 2 ###

df_survey_1$id_numerico <- gsub("[^0-9]", "", df_survey_1$Q2)
df_survey_2$id_numerico <- gsub("[^0-9]", "", df_survey_2$Q2)
df_pre$id_numerico <- gsub("[^0-9]", "", df_pre$Q1)

### partecipanti TV ###

df_survey_1 <- df_survey_1 %>%
  filter(id_numerico %in% df_orig$ID)
df_survey_2 <- df_survey_2 %>%
  filter(id_numerico %in% df_orig$ID)
df_pre <- df_pre %>%
  filter(id_numerico %in% df_orig$ID)

duplicated(df_survey_1$id_numerico) # 88, 250 e 264
df_survey_1 <- df_survey_1[-c(39,99,100),]

duplicated(df_survey_2$id_numerico) # 264 e 116 
df_survey_2 <- df_survey_2[-c(53,99),] 

which(duplicated(df_pre$id_numerico)) # 57 87 102 104 
df_pre<- df_pre[-c(57, 84, 99, 101),]

### tolgo colonne inutili ###

df_survey_1 <- df_survey_1[, -c(1:17)]
df_survey_2 <- df_survey_2[, -c(1:17)]
df_pre <- df_pre[, -c(1:17)]


### metto id per prima colonna ### 
df_survey_1 <- df_survey_1 %>%
  select(ncol(df_survey_1), everything())
df_survey_2 <- df_survey_2 %>%
  select(ncol(df_survey_2), everything())
df_pre <- df_pre %>%
  select(ncol(df_pre), everything())

### cancello colonna Q2 ###
df_survey_1 <- df_survey_1[, -2]
df_survey_2 <- df_survey_2[, -2]
df_pre <- df_pre[, -2]

df_survey_1 <- df_survey_1 %>%
  rename(
    Piacevolezza = Q1_1,
    Capacita_di_coinvolgimento = Q1_2,
    Interesse = Q1_3,
    Qualita_dei_contenuti = Q1_4,
    Qualita_della_produzione = Q1_5,
    Presenza_pubblicita = Q3,
    ID = id_numerico
  )

df_survey_2 <- df_survey_2 %>%
  rename(
    Piacevolezza = Q1_1,
    Capacita_di_coinvolgimento = Q1_2,
    Interesse = Q1_3,
    Qualita_dei_contenuti = Q1_4,
    Qualita_della_produzione = Q1_5,
    Presenza_pubblicita = Q3,
    ID = id_numerico
  )

df_survey_1 <- df_survey_1 %>% 
  mutate(Visione = 1)
df_survey_2 <- df_survey_2 %>% 
  mutate(Visione = 2)




df_pre_new <- df_pre


#correcting the year of birth question
df_pre_new$Q2 <- as.integer(str_extract( df_pre_new$Q2, '\\d{2}$'))
df_pre_new$Q2 <- case_when(df_pre_new$Q2 < 10 ~ df_pre_new$Q2 + 2000, df_pre_new$Q2 >= 10 ~ df_pre_new$Q2 +1900)
df_pre_new$Q2 <- year(Sys.Date()) - df_pre_new$Q2 

#renaming and rearranging
df_pre_new <- df_pre_new %>% 
  rename(Age = Q2, 
         Gender = Q3) %>% 
  arrange(id_numerico)

df_surveys <- rbind(df_survey_1, df_survey_2) 
df_surveys <- left_join(df_surveys, df_pre_new, join_by(ID == id_numerico))

df_surveys$ID <- as.integer(df_surveys$ID)

df_surveys <- df_surveys %>% 
  arrange(ID, Visione) %>% 
  select(ID, Age, Gender, Visione, everything())

df_surveys[,c(5:9)] <- lapply(df_surveys[,c(5:9)], as.integer) 

df_survey_final <- df_surveys %>% 
  select(-c(11, 12, 14, 22)) %>% 
  rename(
    Qualita_Prodotti = Valutaz1_1,
    Invasiva_Bilanciata_AD = Valutaz1_2,
    Creativita_AD = Valutaz1_3,
    Noiosa_Accattivante_AD = Valutaz1_4,
    Propensione_acquisto_prodotto = Valutaz1_5
  )


df_survey_final[,c(3:4,10,16, 23, 29, 35, 41, 47, 53, 59, 65)] <- lapply(df_survey_final[,c(3:4,10,16, 23, 29, 35, 41, 47, 53, 59, 65)], as.factor)
df_survey_final[,c(11:71)] <- lapply(df_survey_final[,c(11:71)], toupper)
df_survey_final[,c(1:2,5:9, 67:71)] <- lapply(df_survey_final[,c(1:2,5:9, 67:71)], as.integer)

df_orig[,c(10,11,13)] <- lapply(df_orig[,c(10,11,13)], as.integer)
df_orig[,c(2,3,6,7)] <- lapply(df_orig[,c(2,3,6,7)], as.character)
df_orig[,c(4,5,8,9,12)] <- lapply(df_orig[,c(4,5,8,9,12)], as.factor)
df_orig[,c(1,14:18)] <- lapply(df_orig[,c(1,14:18)], as.numeric)




#### Sistemazione dataset survey ####

# Identifica le colonne per ciascuna categoria

col_entries <- grep("^Alim2_|^Bev2_|^Integr1_|^Auto1_|^Super1_|^Tel1_|^Arred1_|^eComm1_|^cosm1_", names(df_survey_final), value = TRUE)


#creo il dataframe con le colonne che mi interessano, rinominando
survey_compact <- df_survey_final

survey_compact$Entries <- apply(survey_compact[col_entries], 1, 
                                function(row) paste(row, collapse = ','))
survey_compact$Entries <- str_replace_all(survey_compact$Entries, ',NA|NA,', '')

survey_compact <- survey_compact %>% 
  rename(TipoAlimenti = Alim1,
         Alimento_gia_visto = Alim3,
         Alimenti_lista= Alim4,
         TipoBevande = Bev1,
         Bevanda_gia_vista = Bev3,
         Bevanda_lista = Bev4,
         Integratori_gia_visto = Integr2,
         Integratori_lista = Integr3,
         Auto_gia_visto = Auto2,
         Auto_lista = Auto3,
         Supermercato_gia_visto = Super2,
         Supermercato_lista = Super3,
         Telcom_gia_visto = Tel2,
         Telcom_lista = Tel3,
         Arredamenti_gia_visto = Arred2,
         Arredamenti_lista = Arred3,
         eComm_gia_vista = eComm2,
         eComm_lista = eComm3,
         Cosmetici_gia_vista = Cosm2,
         Cosmetici_lista = Cosm3)

col_list <- grep("_lista", names(survey_compact), value = TRUE)
survey_compact$List <- apply(survey_compact[col_list], 1, 
                                function(row) paste(row, collapse = ','))
survey_compact$List <- str_replace_all(survey_compact$List, ',NA|NA,', '')

survey_final <- survey_compact %>% 
  select(c(1:10), Entries, List, c(67:73)) %>% 
  mutate(across(c(Entries, List), 
                ~ str_replace_all(., '\\\\|/|\\.*|-|,[:space:]|[:space:],', ''))) %>% 
  mutate(across(c(Entries, List), 
                ~ str_replace_all(., '^NA', ''))) 



# ---- PCA ----
# separo il dataframe su cui faro la PCA
df_pca <- survey_final %>% 
  select(c(1,4:9,13:17)) %>% 
  na.omit(.)


corrplot(cor(df_pca[,3:12])) #abbiamo dedotto che ci serviranno due PC e di tenere Propensione separato

# faccio PCA e varimax rotazione per asseghnare significato ai loadings
pca_result <- prcomp(df_pca[,c(3:11)], center = TRUE, scale. = TRUE, rank. = 2)

summary(pca_result)

varimax_results <- varimax(pca_result$rotation)

#salvo gli score relativi a cio che mi interessa
rotated_loadings <- varimax_results$loadings
rotated_scores <- as.data.frame(scale(df_pca[,c(3:11)]) %*% rotated_loadings)
df_pca <- df_pca[,c(1:2,12)] %>% 
  mutate(row_index = row_number())
rotated_scores <- rotated_scores %>% 
  mutate(row_index = row_number())

#creo dataframe con gli score ruotati, e scalo anche propensione acquisto per avere un senso
df_pca_final <- left_join(df_pca, rotated_scores, by = "row_index") %>% 
  select(-row_index) %>% 
  select(ID, Visione, PC1, PC2, Propensione_acquisto_prodotto) %>% 
  rename(PCA_contenuto = PC1, 
         PCA_AD = PC2) %>% 
  mutate(Propensione_acquisto_prodotto = scale(Propensione_acquisto_prodotto)[,1])

survey_final <- left_join(survey_final[,-c(5:9,13:17)], df_pca_final, by = c('ID', 'Visione'))



# ---- AGGIUNGO COLONNA PUBBLICITA MOSTRATE ----
df <- df_orig %>%
  mutate(Label = str_replace_all(Label, '00_', '')) %>% 
  mutate(Label = str_replace_all(Label, ',[:space:]|[:space:],', ',')) %>%
  mutate(Label = str_replace_all(Label, 'BARILLA', 'BARILLA,MULINO BIANCO,MACINE')) %>% 
  mutate(Label = str_replace_all(Label, 'FREGOLA', 'FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO')) %>% 
  mutate(Label = str_replace_all(Label, 'LETE', 'LETE,ACQUA LETE')) %>% 
  mutate(Label = str_replace_all(Label, 'ALFA ROMEO', 'ALFA ROMEO,TONALE'))
           

df_ad <- subset(df, Pubblicita == 1)   

pubblicita_nomi <- unique(df_ad$Label)

df_ad$ID <- as.numeric(df_ad$ID)  

df_ad <- df_ad %>%
  select(ID, Visione, Label) %>%
  distinct()

for (nome in pubblicita_nomi) {
  df_ad[[nome]] <- 0
}

cols_to_assign <- colnames(df_ad)[!colnames(df_ad) %in% c("ID", "Visione", "Label")]
for (col in cols_to_assign) {
  df_ad[[col]] <- ifelse(df_ad$Label == col, 1, df_ad[[col]])
}


df_ad <- df_ad %>%
  group_by(ID, Visione) %>%
  summarise(across(.cols = colnames(df_ad)[!colnames(df_ad) %in% c("ID", "Visione", "Label")], sum)) %>%
  arrange(ID) %>%
  ungroup() %>% 
  mutate('BARILLA' = `BARILLA,MULINO BIANCO,MACINE`,
         'MACINE' = `BARILLA,MULINO BIANCO,MACINE`,
         'FREGOLA' = `FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO`,
         'NUORO' = `FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO`,
         'PASTAIO' = `FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO`,
         'LETE' = `LETE,ACQUA LETE`,
         'ALFA ROMEO' = `ALFA ROMEO,TONALE`) %>% 
  rename('MULINO BIANCO' = 'BARILLA,MULINO BIANCO,MACINE',
         'IL PASTAIO DI NUORO' = 'FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO',
         'ACQUA LETE' = 'LETE,ACQUA LETE',
         'TONALE' = 'ALFA ROMEO,TONALE')

df_total_ad <- colSums(df_ad[3:22])

# Function to replace 1 with column name
replace_with_column_name <- function(row) {
  sapply(1:length(row), function(i) if (row[i] == 1) names(row)[i] else NA)
}

# Apply the function to each row
df_ad[,3:22] <- as.data.frame(t(apply(df_ad[,3:22], 1, replace_with_column_name)))




##############################################################################
#unisco
df_recall <- left_join(survey_final, df_ad, by = c('ID', 'Visione')) %>% 
  mutate(Entries = str_split(Entries, ','),
         List = str_split(List, ','))

# ---- Similarity score ----
# Function to compute similarity scores of each name in the list
compute_similarity <- function(entered_names, real_name) {
  # Initialize a matrix to store the similarity scores
  similarities <- matrix(0, nrow = length(entered_names[[1]]), ncol = 1)
  if(entered_names[[1]][1] == ''){
    similarities[1,1] <- NA
  }
  else  {# Loop over each entered name
    for (i in 1:length(entered_names[[1]])) {
      entered_name <- as.character(entered_names[[1]][i])
      # Compute the Jaro-Winkler distance
      similarities[i, 1] <- 1 - stringdist::stringdist(entered_name, real_name, method = "jw")
      }
    }
  return(similarities)
  }


## UNAIDED

replace_values_if_empty_entries <- function(df_recall) {
  # Loop su ogni riga del data frame
  for (i in 1:nrow(df_recall)) {
    # Se nella riga c'è una cella vuota in Entries
    if (df_recall$Entries[i] == "") {
      # Sostituisci i valori diversi da NA con 0 nelle colonne da 11 a 30
      df_recall[i, 11:30] <- lapply(df_recall[i, 11:30], function(x) ifelse(!is.na(x), 0, x))
    }
  }
  return(df_recall)
}

df_recall <- replace_values_if_empty_entries(df_recall)

extract_empty_entries <- function(df) {
  # Filtra le righe dove la colonna Entries è vuota
  filtered_df <- df[df$Entries == "", ]
  return(filtered_df)
}

df_empty_entries <- extract_empty_entries(df_recall)

remove_empty_entries <- function(df) {
  # Trova le righe dove la colonna Entries è vuota
  empty_entries_rows <- which(df$Entries == "")
  
  # Rimuovi le righe con Entries vuote dal dataset originale
  df <- df[-empty_entries_rows, ]
  
  return(df)
}

try <- df_recall
try <- remove_empty_entries(df_recall)


recall <- function(entered_names, real_name){
  if(any(is.na(compute_similarity(entered_names, real_name)))){
    recall <- NA
  }
  else if(any(compute_similarity(entered_names, real_name) > 0.8)){
    recall <- 1
  }
  else {
    recall <- 0
  }
  recall
}


col_ads <- colnames(df_ad[,-c(1:2)])

df_recall_entries <- try
for (i in 1:length(try$ID)) {
  for (j in col_ads) {
    df_recall_entries[i,j] <- recall(try$Entries[i], try[i,j])
  }
}

df_recall_entries <- rbind(df_recall_entries, df_empty_entries)
df_recall_entries <- df_recall_entries %>% 
  arrange(ID, Visione)

df_recall_entries <- df_recall_entries %>% 
  mutate(across(c(11:30), ~as.numeric(.))) %>% 
  select(c(1:5,11:30))


## AIDED

replace_values_if_empty_entries <- function(df_recall) {
  # Loop su ogni riga del data frame
  for (i in 1:nrow(df_recall)) {
    # Se nella riga c'è una cella vuota in Entries
    if (df_recall$List[i] == "") {
      # Sostituisci i valori diversi da NA con 0 nelle colonne da 11 a 30
      df_recall[i, 11:30] <- lapply(df_recall[i, 11:30], function(x) ifelse(!is.na(x), 0, x))
    }
  }
  return(df_recall)
}

df_recall <- replace_values_if_empty_entries(df_recall)

extract_empty_entries <- function(df) {
  # Filtra le righe dove la colonna Entries è vuota
  filtered_df <- df[df$List == "", ]
  return(filtered_df)
}

df_empty_entries <- extract_empty_entries(df_recall)

remove_empty_entries <- function(df) {
  # Trova le righe dove la colonna Entries è vuota
  empty_entries_rows <- which(df$List == "")
  
  # Rimuovi le righe con Entries vuote dal dataset originale
  df <- df[-empty_entries_rows, ]
  
  return(df)
}

try <- df_recall
try <- remove_empty_entries(df_recall)

recall <- function(entered_names, real_name){
  if(any(is.na(compute_similarity(entered_names, real_name)))){
    recall <- NA
  }
  else if(any(compute_similarity(entered_names, real_name) > 0.8)){
    recall <- 1
  }
  else {
    recall <- 0
  }
  recall
}


col_ads <- colnames(df_ad[,-c(1:2)])

df_recall_list <- try
for (i in 1:length(try$ID)) {
  for (j in col_ads) {
    df_recall_list[i,j] <- recall(try$List[i], try[i,j])
  }
}

df_recall_list <- rbind(df_recall_list, df_empty_entries)
df_recall_list <- df_recall_list %>% 
  arrange(ID, Visione)

df_recall_list <- df_recall_list %>% 
  mutate(across(c(11:30), ~as.numeric(.))) %>% 
  select(c(1:5,11:30))


df_piacevolezza <- df_recall[,c(1:5, 8:10)]



df_recall_entries <- df_recall_entries %>%
  mutate(`IL PASTAIO DI NUORO` = ifelse(rowSums(!is.na(select(., `IL PASTAIO DI NUORO`, `FREGOLA`, `NUORO`, `PASTAIO`))) == 0, NA, rowSums(select(., `IL PASTAIO DI NUORO`, `FREGOLA`, `NUORO`, `PASTAIO`), na.rm = TRUE))) %>% 
  mutate(`LETE` = ifelse(rowSums(!is.na(select(., `ACQUA LETE`, `LETE`))) == 0, NA, rowSums(select(., `ACQUA LETE`, `LETE`), na.rm = TRUE))) %>% 
  mutate(`ALFA ROMEO` = ifelse(rowSums(!is.na(select(., `ALFA ROMEO`, `TONALE`))) == 0, NA, rowSums(select(., `ALFA ROMEO`, `TONALE`), na.rm = TRUE))) %>% 
  mutate(`MULINO BIANCO` = ifelse(rowSums(!is.na(select(., `MULINO BIANCO`, `BARILLA`, `MACINE`))) == 0, NA, rowSums(select(., `MULINO BIANCO`, `BARILLA`, `MACINE`), na.rm = TRUE)))

df_recall_entries <- df_recall_entries %>%
  mutate(`IL PASTAIO DI NUORO` = replace(`IL PASTAIO DI NUORO`, `IL PASTAIO DI NUORO` == 2, 1)) %>% 
  select(-`ACQUA LETE`, -`FREGOLA`, -`NUORO`, - `PASTAIO`, -`TONALE`, -`BARILLA`, -`MACINE`)



df_recall_list <- df_recall_list %>%
  mutate(`IL PASTAIO DI NUORO` = ifelse(rowSums(!is.na(select(., `IL PASTAIO DI NUORO`, `FREGOLA`, `NUORO`, `PASTAIO`))) == 0, NA, rowSums(select(., `IL PASTAIO DI NUORO`, `FREGOLA`, `NUORO`, `PASTAIO`), na.rm = TRUE))) %>% 
  mutate(`LETE` = ifelse(rowSums(!is.na(select(., `ACQUA LETE`, `LETE`))) == 0, NA, rowSums(select(., `ACQUA LETE`, `LETE`), na.rm = TRUE))) %>% 
  mutate(`ALFA ROMEO` = ifelse(rowSums(!is.na(select(., `ALFA ROMEO`, `TONALE`))) == 0, NA, rowSums(select(., `ALFA ROMEO`, `TONALE`), na.rm = TRUE))) %>% 
  mutate(`MULINO BIANCO` = ifelse(rowSums(!is.na(select(., `MULINO BIANCO`, `BARILLA`, `MACINE`))) == 0, NA, rowSums(select(., `MULINO BIANCO`, `BARILLA`, `MACINE`), na.rm = TRUE)))

df_recall_list <- df_recall_list %>%
  mutate(`IL PASTAIO DI NUORO` = replace(`IL PASTAIO DI NUORO`, `IL PASTAIO DI NUORO` == 2, 1)) %>% 
  select(-`ACQUA LETE`, -`FREGOLA`, -`NUORO`, - `PASTAIO`, -`TONALE`, -`BARILLA`, -`MACINE`)



# setwd("~/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
## mi raccomando settate la directory in lineare

# export(df_recall_entries, 'Recall_Entries.xlsx')
# export(df_recall_list, 'Recall_List.xlsx')
# export(df_piacevolezza, 'Piacevolezza_Survey.xlsx')


### ULTIMA PUBBLICITÀ
df_orig$Posizione <- as.numeric(df_orig$Posizione)
df_ultima <- df_orig %>%
  group_by(ID, Visione) %>%
  mutate(Ultimo = if_else(Posizione == max(Posizione, na.rm = TRUE), 1, 0)) %>%
  filter(Ultimo == 1) %>% 
  select(ID, Visione, Label, Posizione, Ultimo) %>% 
  mutate(Label = str_replace(Label, "^00_", "")) %>%
  mutate(Label = str_replace(Label, "FREGOLA", "IL PASTAIO DI NUORO"),
         Label = str_replace(Label, "BARILLA", "MULINO BIANCO")) %>% 
  distinct()

df_ultima <- left_join(df_recall_entries, df_ultima, by = c("ID", "Visione"))

df_ultima <- df_ultima %>%
  rowwise() %>%
  mutate(ultima_ricordata = if_else(get(Label) == 1, 1, 0)) %>%
  ungroup()

df_ultima_1 <- df_ultima %>% 
  filter(Visione == 1)

df_ultima_2 <- df_ultima %>% 
  filter(Visione == 2)

ultima_ricordata_1 <- mean(df_ultima_1$ultima_ricordata)
ultima_ricordata_1
ultima_ricordata_2 <- mean(df_ultima_2$ultima_ricordata)
ultima_ricordata_2

improvement <- (ultima_ricordata_2-ultima_ricordata_1)/ultima_ricordata_1
improvement

success1 <- sum(df_ultima_1$ultima_ricordata)
success2 <- sum(df_ultima_2$ultima_ricordata)
total1 <- length(df_ultima_1$ultima_ricordata) # = total 2

table(success1, success2, total1, total1)

successes <- c(success1, success2)
totals <- c(total1, total1)
# Perform the two-proportion z-test
test_result <- prop.test(successes, totals)
# Print the results
print (test_result)

# ---- Summaries ----

summary_unaided <- df_recall_entries %>% 
  select(6:18) %>% 
  summarise(across(everything(), ~ mean(.x == 1, na.rm = TRUE) * 100))
summary_unaided <- as.data.frame(t(summary_unaided))
colnames(summary_unaided) <- "unaided"
summary_unaided


summary_aided <- df_recall_list %>% 
  select(6:18) %>% 
  summarise(across(everything(), ~ mean(.x == 1, na.rm = TRUE) * 100))
summary_aided <- as.data.frame(t(summary_aided))
colnames(summary_aided) <- "aided"
summary_aided


summary_recall <- cbind(summary_unaided, summary_aided)
summary_recall <- summary_recall %>%
  arrange(desc(unaided))
summary_recall


ggplot(summary_unaided, aes(x = reorder(row.names(summary_unaided), unaided), 
                            y = unaided)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Recall (Unaided) across Ads",
    x = "Ad",
    y = "Recall %"
  ) +
  geom_text(aes(label = round(unaided, 2)), vjust = -0.5, color = "navy", size = 5) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(0, 100) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12, colour = 'navy', angle = 45, vjust = 0.5),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12, colour = 'navy'),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )




ggplot(summary_aided, aes(x = reorder(row.names(summary_aided), aided), 
                            y = aided)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Recall (Aided) across Ads",
    x = "Ad",
    y = "Recall %"
  ) +
  geom_text(aes(label = round(aided, 2)), vjust = -0.5, color = "navy", size = 5) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(0, 100) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12, colour = 'navy', angle = 45, vjust = 0.5),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12, colour = 'navy'),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )



  
## ci concentriamo solo sugli unaided da adesso

## recall per partecipante e visione

calc_recall <- function(row) {
  sum_ones <- sum(row == 1, na.rm = TRUE)
  sum_zeros <- sum(row == 0, na.rm = TRUE)
  total <- sum_ones + sum_zeros
  if (total == 0) {
    return(0)  # Se non ci sono 1 o 0, ritorna NA
  } else {
    recall <- sum_ones / total
    return(recall)
  }
}

df_recall_entries$recall_unaided <- apply(df_recall_entries[, 6:18], 1, calc_recall)

df_recall_list$recall_aided <- apply(df_recall_list[, 6:18], 1, calc_recall)

df_recall_entries <- df_recall_entries %>% 
  select(ID, Age, Gender, Visione, recall_unaided)

df_recall_list <- df_recall_list %>% 
  select(ID, Age, Gender, Visione, recall_aided)

df_recall <- cbind(df_recall_entries, df_recall_list)
df_recall <- df_recall[,c(1:5,10)]

colonne_da_aggiungere <- survey_final[, c(8:10), drop = FALSE]

df_recall <- cbind(df_recall, colonne_da_aggiungere)

# write_xlsx(df_recall, "Recall_per_Partecipante.xlsx")








## RECALL PESATA

# setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
df_entries <- read_excel("Recall_Entries.xlsx")

df_summary <- as.data.frame(summary_unaided)
df_summary <- t(summary_unaided)

summary_vector <- as.vector(t(df_summary))
summary_vector <- summary_vector/100

common_columns <- intersect(names(df_entries), colnames(df_summary))

df_entries_adj <- df_entries %>%
  mutate(across(all_of(common_columns), ~ . / summary_vector[match(cur_column(), colnames(df_summary))]))


df_cont <- df_orig %>% 
  group_by(ID, Visione) %>% 
  select(ID, Visione, Contenuto) %>% 
  distinct()


df_recall_adj <- merge(df_cont, df_entries_adj, by = c("ID","Visione"))


# df_recall_adj <- df_recall_adj %>%
#   group_by(ID, Visione) %>%
#   distinct() %>%
#   mutate(recall_unaided = case_when(
#     Contenuto == 180 ~ recall_unaided * 1,
#     Contenuto == 120 ~ recall_unaided * 0.5,
#     Contenuto == 60 ~ recall_unaided * (1/3),
#     TRUE ~ recall_unaided  
#   )) %>% 
#   mutate(recall_aided = case_when(
#     Contenuto == 180 ~ recall_aided * 1,
#     Contenuto == 120 ~ recall_aided * 0.5,
#     Contenuto == 60 ~ recall_aided * (1/3),
#     TRUE ~ recall_aided
#   ))


df_recall_adj <- df_recall_adj %>%
  group_by(ID, Visione) %>%
  distinct() %>%
  mutate(across(common_columns, ~ case_when(
    Contenuto == 180 ~ . * 1,
    Contenuto == 120 ~ . * (2/3),
    Contenuto == 60 ~ . * (1/3),
    TRUE ~ .
  )))



# setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
# write_xlsx(df_recall_adj, "Recall_adj.xlsx")



## CONFRONTO TRA TOP BRAND PER RECALL E ATTENZIONE

top_att <- df_orig %>% 
  group_by(Label) %>% 
  filter(Pubblicita == 1) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention, na.rm = TRUE)) %>% 
  arrange(desc(Standardized_Value_Attention))
  
summary_recall









