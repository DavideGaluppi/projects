# ---- libraries ----
## install.packages("Hmisc")

library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyr)
require(pacman)
library(Hmisc)
library(robustbase)
library(corrplot)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())
#dev.off()

# ---- WORK ----

#### SET WD --> Datasets  > Lineare

#IMPORT DATASET AND FIRST CLEANING

df_orig <- import('DATASET_TV_LINEARE.xlsx')

df <- df_orig %>% 
  filter(!(Label == 'BINNING_5S')) %>% #tolgo le prime righe per id e Visione che contengono ripetizioni
  rename(Binning = Label) %>% 
  mutate(Label = ifelse(!grepl('^BINNING', Binning), Binning, NA)) %>% #creo colonna label con il contenuto
  fill(Label, .direction = 'down') %>% #riempio gli NA di label con il nome che c'è sopra
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, A, B, Binning, Label, everything())

df$ID <- as.numeric(df$ID)
colnames(df) <- gsub("mean.", "Value.", colnames(df), ignore.case = TRUE)
colnames(df) <- gsub("Average.", "Value.", colnames(df), ignore.case = TRUE)
colnames(df) <- gsub("Peaks.", "Value.Peaks.", colnames(df), ignore.case = TRUE)

df[df$ID == 216, "Visione"] <-  case_when(df[df$ID == 216, "Visione"] == 2 ~ 1, 
                                          is.na(df[df$ID == 216, "Visione"]) ~ 2)
df[df$ID == 187, "Visione"] <-  case_when(df[df$ID == 187, "Visione"] == 2 ~ 1, 
                                          is.na(df[df$ID == 187, "Visione"]) ~ 2)

## i partecipanti 173, 176, 218 hanno avuto problemi con EEG e quindi la riga è vuota, quindi li rimuovo
df <- df %>%  
  filter(grepl('BINNING', Binning)) %>% 
  filter(ID != 173) %>% 
  filter(ID != 176) %>% 
  filter(ID != 218)


df <- df[,c(1:14, 16:18, 22)]

columns <- colnames(df)[which(grepl('Cluster', colnames(df)) |
                                grepl('Asymmetry', colnames(df)))]



remove_all_NA_rows <- function(df, columns) {
  df %>%
    rowwise() %>%
    mutate(all_NA = all(is.na(c_across(all_of(columns))))) %>%
    filter(!all_NA) %>%
    select(-all_NA)  # Remove the helper column
}

df <- remove_all_NA_rows(df, columns)

df <- df %>% 
  group_by(ID, Visione, Label) %>% 
  mutate(Value.Peak.Amplitude = case_when(is.na(Value.Peak.Amplitude) ~ mean(Value.Peak.Amplitude, na.rm = TRUE),
                                          TRUE ~ Value.Peak.Amplitude)) %>% 
  mutate(Value.Peak.Amplitude = case_when(is.nan(Value.Peak.Amplitude) ~ 0,
                                          TRUE ~ Value.Peak.Amplitude)) %>% 
  ungroup()

# ---- ROBUST SCALING ---- 

df_Scaled <- df

# Function to apply robust scaling
robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  (x - med) / iqr
}

columns <- colnames(df_Scaled[,c(14:18)])

# Apply robust scaling to each column
df_Scaled <- df_Scaled %>% 
  group_by(ID, Visione) %>% 
  mutate(across(columns, ~ robust_scale(.))) %>% 
  ungroup()

# ---- Metriche ----

columns <- colnames(df_Scaled[, c(15:18)])

df_metric <- df_Scaled %>%
  mutate(across(columns,~ 10^(./10)) ) %>% 
  mutate(Value.Peak.Amplitude = case_when(is.nan(Value.Peak.Amplitude) ~ 0,
                                          TRUE ~ Value.Peak.Amplitude)) %>% 
  mutate(Value_Pleasantness = Value.Frontal.Asymmetry.Alpha,
         Value_Attention = Value.PSD.Electrode.Cluster.Value.Theta..dB.,
         Value_Engagement = Value.PSD.Electrode.Cluster.Value.Beta..dB./
           (Value.PSD.Electrode.Cluster.Value.Alpha..dB. + Value.PSD.Electrode.Cluster.Value.Theta..dB.)) %>% 
  select(!columns)

total_nan_count <- sum(sapply(df_metric[,c(15:17)], function(x) sum(is.nan(x))))
total_nan_count/(length(df_metric$ID)*3)

# ---- BOXPLOT ----

# Let's create a sample data frame for illustration purposes
set.seed(42)  # for reproducibility

# Select 5 random participants
random_participants <- sample(unique(df_metric$ID), 5)
selected_data <- df_metric %>% filter(ID %in% random_participants) %>% filter(Visione == 1)
selected_data$ID <- as.factor(selected_data$ID)
# Plotting the box plots
ggplot(selected_data, aes(x = ID, y = Value.Peak.Amplitude)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(selected_data, aes(x = ID, y = Value_Pleasantness)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(selected_data, aes(x = ID, y = Value_Attention)) +
  geom_boxplot(color = "navy", fill = "lightblue") +
  labs(title = 'Box Plot for 5 Random Participants, after robust scaling', 
       x = 'Participant ID', y = 'Attention') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

ggplot(selected_data, aes(x = ID, y = Value_Engagement)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(df_orig %>% filter(ID %in% random_participants) %>% filter(Visione == 1)
       , aes(x = as.factor(ID), y = Mean.PSD.Electrode.Cluster.Average.Theta..dB.)) +
  geom_boxplot(color = "navy", fill = "lightblue") +
  labs(title = 'Box Plot for 5 Random Participants, Raw Data', 
       x = 'Participant ID', y = 'Attention (Theta_Cluster)') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )



# ---- categorie ----
# Creare una funzione per categorizzare le etichette
categorize_label <- function(Label) {
  if (startsWith(Label, "NATURA MERAVIGLIA SENZA TEMPO") ||
      startsWith(Label, "SEGRETI FORESTE INCANTATE") ||
      startsWith(Label, "SHINDLER") ||
      startsWith(Label, "STANOTTE A PARIGI") ||
      startsWith(Label, "ULISSE") ||
      startsWith(Label, "UNA GIORNATA PARTICOLARE") ||
      startsWith(Label, "FREEDOM") ||
      startsWith(Label, "FUORILEGGE") ||
      startsWith(Label, "IN VIAGGIO CON BARBERO") ||
      startsWith(Label, "KILIMANGIARO")){
    return("Documentari")
  } else if (startsWith(Label, "CE POSTA PER TE") ||
             startsWith(Label, "C'È POSTA PER TE")||
             startsWith(Label, "4 RISTORANTI") ||
             startsWith(Label, "GRANDE FRATELLO") ||
             startsWith(Label, "STASERA TUTTO È POSSIBILE") ||
             startsWith(Label, "VIVARAI2") ||
             startsWith(Label, "AMICI") ||
             startsWith(Label, "MASTERCHEF") ||
             startsWith(Label, "X-FACTOR")) {
    return("Intrattenimento")
  } else if (startsWith(Label, "DOC") ||
             startsWith(Label, "DOMINA") ||
             startsWith(Label, "I FANTASTICI 5") ||
             startsWith(Label, "LOLITA LOBOSCO") ||
             startsWith(Label, "MARE FUORI") ||
             startsWith(Label, "MASTER AND COMMANDER") ||
             startsWith(Label, "NCIS") ||
             startsWith(Label, "SUCCESSION") ||
             startsWith(Label, "TERRA AMARA") ||
             startsWith(Label, "TUT IL DESTINO DI UN FARAONE") ||
             startsWith(Label, "THE EQUALIZER")) {
    return("Serie tv")
  } else if (startsWith(Label, "IL CAVALLO E LA TORRE") ||
             startsWith(Label, "IN ALTRE PAROLE") ||
             startsWith(Label, "LE IENE") ||
             startsWith(Label, "LE IENE INSIDE") ||
             startsWith(Label, "PETROLIO") ||
             startsWith(Label, "SKY CALCIO CLUB") ||
             startsWith(Label, "SKY TG 24 NUMERI") ||
             startsWith(Label, "SKY TG 24 WOW") ||
             startsWith(Label, "TG METANA DATAROOM") ||
             startsWith(Label, "PROPAGANDA LIVE") ||
             startsWith(Label, "BELVE") ||
             startsWith(Label, "DIRITTO E ROVESCIO") ||
             startsWith(Label, "OTTO E MEZZO") ||
             startsWith(Label, "QUARTA REPUBBLICA") ||
             startsWith(Label, "SPLENDIDA CORNICE") ||
             startsWith(Label, "SPLENDIDA_CORNICE")) {
    return("Talk show")
  } else {
    return(NA)
  }
}

# Applicare la funzione per creare la nuova colonna
df_metric$Categoria <- sapply(df_metric$Label, categorize_label)

# Inserire la nuova colonna nella posizione 8
df_metric <- df_metric[, c(1:9, ncol(df_metric), 10:(ncol(df_metric)-1))]

# Funzione per riempire i NA con il valore della riga precedente
riempi_na_con_precedente <- function(Categoria) {
  for (i in 2:length(Categoria)) {
    if (is.na(Categoria[i])) {
      Categoria[i] <- Categoria[i - 1]
    }
  }
  return(Categoria)
}

# Applica la funzione al dataframe
df_metric$Categoria <- riempi_na_con_precedente(df_metric$Categoria)

df_metric <- df_metric %>%
  mutate(tempo = lead(Start..ms.) - Start..ms.) %>%
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, A, B, Binning, Label, Categoria, Start..ms., tempo, everything())



# ---- COEFFICIENTI DI LE C.E. ----
col <- colnames(df_metric[,16:19])

df <- df_metric


df_C.E_mean_1 <- df %>%
  filter(grepl('^BINNING', Binning)) %>%
  filter(!grepl('^00|_B', Label)) %>%
  filter(Visione == 1) %>%
  group_by(ID, Visione) %>%
  summarise(Label = unique(Label),
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID)

df_C.E_mean_1 <- as.data.frame(lapply(df_C.E_mean_1, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))

colnames(df_C.E_mean_1) <- gsub("Value", "Mean", colnames(df_C.E_mean_1), ignore.case = TRUE)


df_C.E_mean_2 <- df %>%
  filter(grepl('^BINNING', Binning)) %>%
  filter(!grepl('^00|_B', Label)) %>%
  filter(Visione == 2) %>%
  group_by(ID, Visione) %>%
  summarise(Label = unique(Label),
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID) 

df_C.E_mean_2 <- as.data.frame(lapply(df_C.E_mean_2, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))


# Rinomina le colonne che contengono la parola "mean" in "sd"
colnames(df_C.E_mean_2) <- gsub("Value", "Mean_2", colnames(df_C.E_mean_2), ignore.case = TRUE)


# CALCOLO IL COEFFICIENT + TEST
col_mean_1 <- colnames(df_C.E_mean_1[,c(1, 4:7)])
col_mean_2 <- colnames(df_C.E_mean_2[,c(1, 4:7)])

LE_merged_C.E. <- merge(df_C.E_mean_2[,col_mean_2], df_C.E_mean_1[,col_mean_1], by = 'ID')

LE_merged_C.E.[,col_mean_1[-1]] <- LE_merged_C.E.[,col_mean_2[-1]]/LE_merged_C.E.[,col_mean_1[-1]]
LE_merged_C.E. <- LE_merged_C.E.[,col_mean_1] %>% 
  mutate(Visione = 2) %>% 
  mutate(Visione = as.factor(Visione))


colMeans(LE_merged_C.E.[,-c(1,6)], na.rm = TRUE)

t.test(df_C.E_mean_1$Mean_Pleasantness, df_C.E_mean_2$Mean_2_Pleasantness)
t.test(df_C.E_mean_1$Mean_Attention, df_C.E_mean_2$Mean_2_Attention)
t.test(df_C.E_mean_1$Mean_Engagement, df_C.E_mean_2$Mean_2_Engagement)


# ---- COEFFICIENTI DI LE PUBBLICITA ----

df <- df_metric


df_P_mean_1 <- df %>%
  filter(grepl('^BINNING', Binning)) %>%
  filter(grepl('^00', Label)) %>%
  filter(Visione == 1) %>%
  group_by(ID, Visione) %>%
  summarise(Label = NA,
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID) 

df_P_mean_1 <- as.data.frame(lapply(df_P_mean_1, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))

colnames(df_P_mean_1) <- gsub("Value", "Mean", colnames(df_P_mean_1), ignore.case = TRUE)


df_P_mean_2 <- df %>%
  filter(grepl('^BINNING', Binning)) %>%
  filter(grepl('^00', Label)) %>%
  filter(Visione == 2) %>%
  group_by(ID, Visione) %>%
  summarise(Label = NA,
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID)


df_P_mean_2 <- as.data.frame(lapply(df_P_mean_2, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))


# Rinomina le colonne che contengono la parola "mean" in "sd"
colnames(df_P_mean_2) <- gsub("Value", "Mean_2", colnames(df_P_mean_2), ignore.case = TRUE)


# CALCOLO IL COEFFICIENT + TEST
col_mean_1 <- colnames(df_P_mean_1[,c(1, 4:7)])
col_mean_2 <- colnames(df_P_mean_2[,c(1, 4:7)])

LE_merged_P <- merge(df_P_mean_2[,col_mean_2], df_P_mean_1[,col_mean_1], by = 'ID')

LE_merged_P[,col_mean_1[-1]] <- LE_merged_P[,col_mean_2[-1]]/LE_merged_P[,col_mean_1[-1]]
LE_merged_P <- LE_merged_P[,col_mean_1] %>% 
  mutate(Visione = 2) %>% 
  mutate(Visione = as.factor(Visione))

colMeans(LE_merged_P[,-c(1,6)], na.rm = TRUE)

t.test(df_P_mean_1$Mean_Pleasantness, df_P_mean_2$Mean_2_Pleasantness)
t.test(df_P_mean_1$Mean_Attention, df_P_mean_2$Mean_2_Attention)
t.test(df_P_mean_1$Mean_Engagement, df_P_mean_2$Mean_2_Engagement)

export(LE_merged_C.E., 'LE_merged_C.E..xlsx')
export(LE_merged_P, 'LE_merged_P.xlsx')

# ---- Aggiungo Colonna posizione ----

# Aggiungi la colonna "Posizione"
df_metric <- df_metric %>%
  mutate(Pubblicita = ifelse(startsWith(as.character(Label), "00_"), 1, 0)) %>% 
  group_by(Respondent.Name, Visione) %>%
  mutate(
    Posizione = ifelse(Pubblicita == 1, cumsum(Pubblicita * (lag(Label, default = "") != Label)), 0),
    Posizione = ifelse(Pubblicita == 0, 0, Posizione)
  ) %>%
  ungroup()

# Riordina le colonne per posizionare "pubblicità" e "Posizione" subito dopo "Label"
df_metric <- df_metric %>% 
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, Binning, Label, Pubblicita, Posizione, everything())

df_metric$Durata_pubblicita <- ifelse(startsWith(as.character(df_metric$Label), "00_"), 1, 0)


categorize_label <- function(Label) {
  if (endsWith(Label, "ALFA ROMEO") ||
      endsWith(Label, "AMAZON") ||
      endsWith(Label, "CHANEL") ||
      endsWith(Label, "COCA COLA") ||
      endsWith(Label, "COOP") ||
      endsWith(Label, "FREGOLA")){
    return(30)
  } else if (endsWith(Label, "BARILLA") ||
             endsWith(Label, "FASTWEB")||
             endsWith(Label, "IKEA") ||
             endsWith(Label, "LETE") ||
             endsWith(Label, "MULTICENTRUM") ||
             endsWith(Label, "MUTTI") ||
             endsWith(Label, "ORASI")){
    return(15)
  }
  else {
    return(0)
  }
}

# Applicare la funzione per creare la nuova colonna
df_metric$Durata_pubblicita <- sapply(df_metric$Label, categorize_label)

df_metric <- df_metric %>% 
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, Binning, Label, Pubblicita, Posizione, Durata_pubblicita, everything())

str(df_metric)
df_metric$Pubblicita <- as.factor(df_metric$Pubblicita)
levels <- c(1,2,3,4,5,6,7,8,9)
df_metric$Posizione <- factor(df_metric$Posizione, levels = levels, ordered = TRUE)
levels <- c(0, 15, 30)
df_metric$Durata_pubblicita <- factor(df_metric$Durata_pubblicita, levels = levels, ordered = TRUE)


df_metric[,c(1:5,7:11,14)] <- lapply(df_metric[,c(1:5,7:11,14)], as.factor)
df_metric[,c(17:20)] <- lapply(df_metric[,c(17:20)], as.numeric)

# ---- STANDARDIZZAZIONE MEAN 0, SIGMA 1 ----

standardization <- function(x) {
  value_cols <- grep("^Value", colnames(x), value = TRUE)
  col <- value_cols[-1]
  x_mean_baseline <- x %>% 
    filter(!grepl('^00|_B$', Label)) %>%
    group_by(ID, Visione) %>% 
    summarise(across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
    arrange(ID) 
  x_sd_baseline <- x %>% 
    filter(!grepl('^00|_B$', Label)) %>%
    group_by(ID, Visione) %>% 
    summarise(across(col, ~ sqrt(wtd.var(., tempo, na.rm = TRUE)))) %>%
    arrange(ID)
  colnames(x_sd_baseline) <- gsub("Value", "Sd", colnames(x_sd_baseline), ignore.case = TRUE)
  colnames(x_mean_baseline) <- gsub("Value", "Mean", colnames(x_mean_baseline), ignore.case = TRUE)
  
  
  all_mean_sd <- left_join(x_mean_baseline, x_sd_baseline, by = c('ID', 'Visione'))  
  all <- left_join(x, all_mean_sd, by = c('ID', 'Visione'))
  

  mean_cols <- grep("^Mean", colnames(all), value = TRUE)
  sd_cols <- grep("^Sd", colnames(all), value = TRUE)
  
  # Crea nuove colonne standardizzate
  for (i in seq_along(col)) {
    value_col <- col[i]
    mean_col <- mean_cols[i]
    sd_col <- sd_cols[i]
    
    all[,value_col] <- (all[,value_col] - all[,mean_col]) / all[,sd_col]
    new_col <- paste0('Standardized_', value_col)
    all <- all %>%
      select(-c(mean_col, sd_col))
    
  }
  all <- all %>% 
    rename_with(~ paste0("Standardized_", .), all_of(col))
  
  return(all)
}

df_stand <- standardization(df_metric)

prova <- subset(df_stand, ID == 100 & Visione == 2 & !grepl('^00|_B$', Label))

colMeans(prova[,c(19:22)], na.rm = TRUE)


df_final <- df_stand %>% 
  select(-c(A, B))


# ---- BOXPLOT ----

# Let's create a sample data frame for illustration purposes
set.seed(42)  # for reproducibility

# Select 5 random participants
random_participants <- sample(unique(df_final$ID), 5)
selected_data <- df_final %>% filter(ID %in% random_participants) %>% filter(Visione == 1) %>% filter(!grepl('^00|_B$', Label))
selected_data$ID <- as.factor(selected_data$ID)
# Plotting the box plots
ggplot(selected_data, aes(x = ID, y = Standardized_Value.Peak.Amplitude)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(selected_data, aes(x = ID, y = Standardized_Value_Pleasantness)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(selected_data, aes(x = ID, y = Standardized_Value_Attention)) +
  geom_boxplot(color = "navy", fill = "lightblue") +
  labs(title = 'Box Plot for 5 Random Participants, after standardization', 
       x = 'Participant ID', y = 'Attention') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

ggplot(selected_data, aes(x = ID, y = Standardized_Value_Engagement)) +
  geom_boxplot() +
  labs(title = 'Box Plot for 5 Random Participants', x = 'Participant ID', y = 'Value') +
  theme_minimal()

ggplot(df_orig %>% filter(ID %in% random_participants) %>% filter(Visione == 1)
       , aes(x = as.factor(ID), y = Mean.PSD.Electrode.Cluster.Average.Theta..dB.)) +
  geom_boxplot(color = "navy", fill = "lightblue") +
  labs(title = 'Box Plot for 5 Random Participants, Raw Data', 
       x = 'Participant ID', y = 'Attention (Theta_Cluster)') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )



# ---- export ----
# export(df_final, "Metriche_Standardized.xlsx")

