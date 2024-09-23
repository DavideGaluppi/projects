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
library(Hmisc)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())

# ---- WORK ----

#IMPORT

#### SET WD --> Datasets > VOD + YouTube

df_vod_yt <- import('DATASET_VOD_YOUTUBE.xlsx')
df_vod_yt$ID <- as.numeric(df_vod_yt$ID)

columns <- colnames(df_vod_yt)[which(grepl('Cluster', colnames(df_vod_yt)) |
                                grepl('Asymmetry', colnames(df_vod_yt)))]



remove_all_NA_rows <- function(df, columns) {
  df %>%
    rowwise() %>%
    mutate(all_NA = all(is.na(c_across(all_of(columns))))) %>%
    filter(!all_NA) %>%
    select(-all_NA)  # Remove the helper column
}

df_vod_yt <- remove_all_NA_rows(df_vod_yt, columns)

df_vod_yt <- df_vod_yt[, c(1:20, 22:24, 28, 31:32)]

df_vod_yt <- df_vod_yt %>% 
  group_by(ID, VISIONE, Label) %>% 
  mutate(Value.Peak.Amplitude = case_when(is.na(Value.Peak.Amplitude) ~ mean(Value.Peak.Amplitude, na.rm = TRUE),
                                          TRUE ~ Value.Peak.Amplitude)) %>% 
  mutate(Value.Peak.Amplitude = case_when(is.nan(Value.Peak.Amplitude) ~ 0,
                                          TRUE ~ Value.Peak.Amplitude)) %>% 
  ungroup()

# ---- ROBUST SCALING ---- 

df_Scaled <- df_vod_yt

# Function to apply robust scaling
robust_scale <- function(x) {
  med <- median(x, na.rm = TRUE)
  iqr <- IQR(x, na.rm = TRUE)
  (x - med) / iqr
}

columns <- colnames(df_Scaled[,c(20:24)])

# Apply robust scaling to each column
df_Scaled <- df_Scaled %>% 
  group_by(ID, VISIONE) %>% 
  mutate(across(columns, ~ robust_scale(.))) %>% 
  ungroup()

# ---- 
columns <- colnames(df_Scaled[, c(21:24)])

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
selected_data <- df_metric %>% filter(ID %in% random_participants) %>% filter(VISIONE == 1)
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

ggplot(df_vod_yt %>% filter(ID %in% random_participants) %>% filter(VISIONE == 1)
       , aes(x = as.factor(ID), y = Value.PSD.Electrode.Cluster.Value.Theta..dB.)) +
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

# ---- COEFFICIENTI DI LE C.E. ----
df <- df_metric %>%
  group_by(ID, VOD_YT) %>% 
  mutate(tempo = lead(Start..ms.) - Start..ms.) %>%
  select(ID, Respondent.Name, Study.Name, VOD_YT, VISIONE, Piattaforma_VOD, 
         Programma_VOD, Video_YT, Binning, Label, Categoria, Start..ms., tempo, 
         Pubblicita, ParteA, everything())



col <- colnames(df[,23:26])


df_C.E_mean_1 <- df %>%
  filter(Pubblicita == 0) %>%
  filter(ParteA == 1) %>%
  filter(VISIONE == 1) %>%
  group_by(ID, VISIONE) %>%
  summarise(Label = unique(Label),
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID)

df_C.E_mean_1 <- as.data.frame(lapply(df_C.E_mean_1, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))

colnames(df_C.E_mean_1) <- gsub("Value", "Mean", colnames(df_C.E_mean_1), ignore.case = TRUE)


df_C.E_mean_2 <- df %>%
  filter(Pubblicita == 0) %>%
  filter(ParteA == 1) %>%
  filter(VISIONE == 2) %>%
  group_by(ID, VISIONE) %>%
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

LE_merged_C.E.[,col_mean_1] <- LE_merged_C.E.[,col_mean_2]/LE_merged_C.E.[,col_mean_1]
LE_merged_C.E. <- LE_merged_C.E.[,col_mean_1] %>% 
  mutate(Visione = 2) %>% 
  mutate(Visione = as.factor(Visione))


colMeans(LE_merged_C.E.[,-c(1,6)], na.rm = TRUE)

t.test(df_C.E_mean_1$Mean_Pleasantness, df_C.E_mean_2$Mean_2_Pleasantness)
t.test(df_C.E_mean_1$Mean_Attention, df_C.E_mean_2$Mean_2_Attention)
t.test(df_C.E_mean_1$Mean_Engagement, df_C.E_mean_2$Mean_2_Engagement)


# ---- COEFFICIENTI DI LE PUBBLICITA ----
df <- df_metric %>%
  group_by(ID, VOD_YT) %>% 
  mutate(tempo = lead(Start..ms.) - Start..ms.) %>%
  select(ID, Respondent.Name, Study.Name, VOD_YT, VISIONE, Piattaforma_VOD, 
         Programma_VOD, Video_YT, Binning, Label, Categoria, Start..ms., tempo, 
         Pubblicita, ParteA, everything())



col <- colnames(df[,23:26])

df_P_mean_1 <- df %>%
  filter(Pubblicita == 1) %>%
  filter(VISIONE == 1) %>%
  group_by(ID, VISIONE, Label) %>%
  summarise(
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID) 

df_P_mean_1 <- as.data.frame(lapply(df_P_mean_1, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))

colnames(df_P_mean_1) <- gsub("Value", "Mean", colnames(df_P_mean_1), ignore.case = TRUE)


df_P_mean_2 <- df %>%
  filter(Pubblicita == 1) %>%
  filter(VISIONE == 2) %>%
  group_by(ID, VISIONE, Label) %>%
  summarise(
            across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
  arrange(ID)


df_P_mean_2 <- as.data.frame(lapply(df_P_mean_2, function(x) {
  x[is.nan(x)] <- NA
  return(x)
}))


# Rinomina le colonne che contengono la parola "mean" in "sd"
colnames(df_P_mean_2) <- gsub("Value", "Mean_2", colnames(df_P_mean_2), ignore.case = TRUE)


# CALCOLO IL COEFFICIENT + TEST
col_mean_1 <- colnames(df_P_mean_1[,c(1,3, 4:7)])
col_mean_2 <- colnames(df_P_mean_2[,c(1,3, 4:7)])

LE_merged_P <- merge(df_P_mean_2[,col_mean_2], df_P_mean_1[,col_mean_1], by = c('ID','Label'))

LE_merged_P[,col_mean_1[-c(1:2)]] <- LE_merged_P[,col_mean_2[-c(1:2)]]/LE_merged_P[,col_mean_1[-c(1:2)]]
LE_merged_P <- LE_merged_P[,col_mean_1] %>% 
  mutate(Visione = 2) %>% 
  mutate(Visione = as.factor(Visione))

colMeans(LE_merged_P[,-c(1,2,7)], na.rm = TRUE)

t.test(df_P_mean_1$Mean_Pleasantness, df_P_mean_2$Mean_2_Pleasantness)
t.test(df_P_mean_1$Mean_Attention, df_P_mean_2$Mean_2_Attention)
t.test(df_P_mean_1$Mean_Engagement, df_P_mean_2$Mean_2_Engagement)


# ---- STANDARDIZZAZIONE MEAN 0, SIGMA 1 ----
df <- df_metric %>%
  group_by(ID, VOD_YT) %>% 
  mutate(tempo = lead(Start..ms.) - Start..ms.) %>%
  select(ID, Respondent.Name, Study.Name, VOD_YT, VISIONE, Piattaforma_VOD, 
         Programma_VOD, Video_YT, Binning, Label, Categoria, Start..ms., tempo, 
         Pubblicita, ParteA, everything())

standardization <- function(x) {
  value_cols <- grep("^Value", colnames(x), value = TRUE)
  col <- value_cols[-1]
  x_mean_baseline <- x %>% 
    filter(Pubblicita == 0) %>%
    filter(ParteA == 1) %>%
    group_by(ID, VISIONE) %>% 
    summarise(across(col, ~ wtd.mean(., tempo, na.rm = TRUE))) %>%
    arrange(ID) 
  x_sd_baseline <- x %>% 
    filter(Pubblicita == 0) %>%
    filter(ParteA == 1) %>%
    group_by(ID, VISIONE) %>% 
    summarise(across(col, ~ sqrt(wtd.var(., tempo, na.rm = TRUE)))) %>%
    arrange(ID)
  colnames(x_sd_baseline) <- gsub("Value", "Sd", colnames(x_sd_baseline), ignore.case = TRUE)
  colnames(x_mean_baseline) <- gsub("Value", "Mean", colnames(x_mean_baseline), ignore.case = TRUE)
  
  
  all_mean_sd <- left_join(x_mean_baseline, x_sd_baseline, by = c('ID', 'VISIONE'))  
  all <- left_join(x, all_mean_sd, by = c('ID', 'VISIONE'))
  
  
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

df_stand <- standardization(df)

prova <- subset(df_stand, ID == 1175 & VISIONE == 2 & Pubblicita == 0 & ParteA == 1)

colMeans(prova[,c(23:26)], na.rm = TRUE)

df_final <- df_stand %>% 
  select(-c(16:19))



# ---- BOXPLOT ----

# Let's create a sample data frame for illustration purposes
set.seed(42)  # for reproducibility

# Select 5 random participants
random_participants <- sample(unique(df_final$ID), 5)
selected_data <- df_final %>% filter(ID %in% random_participants) %>% filter(VISIONE == 1) %>% filter(Pubblicita == 0)
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

ggplot(df_vod_yt %>% filter(ID %in% random_participants) %>% filter(VISIONE == 1)
       , aes(x = as.factor(ID), y = Value.PSD.Electrode.Cluster.Value.Theta..dB.)) +
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



# ---- EXPORTING ----

# export(df_final, "Metriche_Standardized_VOD_YT.xlsx")

