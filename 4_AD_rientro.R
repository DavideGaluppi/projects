library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(writexl)
library(purrr)

## install.packages("gridExtra")
library(gridExtra)


rm(list = ls())

#### SET WD --> Datasets > Lineare

# setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
df_metric <- read_excel("Metriche_Standardized.xlsx")

str(df_metric)
df_metric$Pubblicita <- as.factor(df_metric$Pubblicita)
df_metric$Posizione <- as.factor(df_metric$Posizione)
df_metric$Durata_pubblicita <- as.factor(df_metric$Durata_pubblicita)


df_ad_1 <- subset(df_metric, Pubblicita == 1 & Visione == 1) 
df_ad_2 <- subset(df_metric, Pubblicita == 1 & Visione == 2) 



# Grafico a barre della durata totale della pubblicità rispetto all'impatto sull'attenzione 
impatto_contenuto_1 <- aggregate(Standardized_Value_Attention ~ Contenuto, data = df_ad_1, FUN = mean)
a1 <- ggplot(impatto_contenuto_1, aes(x = Contenuto, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per durata totale della pubblicità (Visione 1)",
    x = "Durata totale della pubblicità (secondi)",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_contenuto_1)


impatto_contenuto_2 <- aggregate(Standardized_Value_Attention ~ Contenuto, data = df_ad_2, FUN = mean)
a2 <- ggplot(impatto_contenuto_2, aes(x = Contenuto, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per durata totale della pubblicità (Visione 2)",
    x = "Durata totale della pubblicità (secondi)",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_contenuto_2)


# Grafico a barre della durata della singola pubblicità rispetto all'impatto sull'attenzione
impatto_durata_1 <- aggregate(Standardized_Value_Attention ~ Durata_pubblicita, data = df_ad_1, FUN = mean)
a3 <- ggplot(impatto_durata_1, aes(x = Durata_pubblicita, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per durata della singola pubblicità (Visione 1)",
    x = "Durata della singola pubblicità (secondi)",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_durata_1)


impatto_durata_2 <- aggregate(Standardized_Value_Attention ~ Durata_pubblicita, data = df_ad_2, FUN = mean)
a4 <- ggplot(impatto_durata_2, aes(x = Durata_pubblicita, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per durata della singola pubblicità (Visione 2)",
    x = "Durata della singola pubblicità (secondi)",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_durata_2)


# Grafico a barre della posizione rispetto all'impatto sull'attenzione
impatto_posizione_1 <- aggregate(Standardized_Value_Attention ~ Posizione, data = df_ad_1, FUN = mean)
a5 <- ggplot(impatto_posizione_1, aes(x = Posizione, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per posizione (Visione 1)",
    x = "Posizione della pubblicità",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_posizione_1)


impatto_posizione_2 <- aggregate(Standardized_Value_Attention ~ Posizione, data = df_ad_2, FUN = mean)
a6 <- ggplot(impatto_posizione_2, aes(x = Posizione, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per posizione (Visione 2)",
    x = "Posizione della pubblicità",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_posizione_2)


# Grafico a barre della categoria rispetto all'impatto sull'attenzione
impatto_categoria_1 <- aggregate(Standardized_Value_Attention ~ Categoria, data = df_ad_1, FUN = mean)
a7 <- ggplot(impatto_categoria_1, aes(x = Categoria, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per categoria (Visione 1)",
    x = "Categoria del programma",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_categoria_1)


impatto_categoria_2 <- aggregate(Standardized_Value_Attention ~ Categoria, data = df_ad_2, FUN = mean)
a8 <- ggplot(impatto_categoria_2, aes(x = Categoria, y = Standardized_Value_Attention)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.7) +
  labs(
    title = "Per categoria (Visione 2)",
    x = "Categoria del programma",
    y = "Impatto sull'attenzione"
  ) +
  geom_text(aes(label = round(Standardized_Value_Attention, 3)), vjust = -0.5, color = "black", size = 3.5) +
  theme_minimal()
print(impatto_categoria_2)

grid.arrange(a1, a3, a5, a7, a2, a4, a6, a8, nrow = 2)


## Assessment of attention levels upon post-ad return to editorial content.
## considero i primi 15 secondi
## df_metric <- read_excel("df_metric.xlsx")

# df_metric <- df_metric %>%
#   arrange(ID, Visione, Start..ms.) %>%
#   mutate(transition = Pubblicita != lag(Pubblicita, default = first(Pubblicita))) %>%
#   ## filter(!is.na(df_metric$Standardized_Value_Attention)) %>%
#   select(ID, Respondent.Name, Study.Name, Contenuto, Visione, Binning, Label, transition, everything())
# 
# 
# ## creazione del pre-transizione --> 15 secondi
 


df_metric$Start..ms. <- as.numeric(df_metric$Start..ms.)
df_metric <- df_metric %>% 
  arrange(ID, Visione, Start..ms.)


## creazione del pre-transizione --> 15 secondi
df_metric <- df_metric %>%
  arrange(ID, Visione, Start..ms.) %>%
  mutate(transition = Pubblicita != lag(Pubblicita, default = first(Pubblicita))) %>%
  mutate(transition_group = cumsum(transition)) %>%
  group_by(transition_group) %>%
  mutate(transition = ifelse(row_number() <= 3 & transition_group != 0, TRUE, FALSE)) %>%
  ungroup() %>%
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, Binning, Label, transition, everything()) %>%
  select(-transition_group) 


df_metric <- df_metric %>%
  mutate(row_id = row_number())

transition_indices <- which(df_metric$transition)

get_previous_three_rows <- function(index) {
  start <- max(1, index - 3)  
  end <- index - 1
  df_metric[start:end, ]
}

previous_rows_list <- lapply(transition_indices, get_previous_three_rows)

df_transitions_prev <- bind_rows(previous_rows_list) %>%
  distinct() %>%  
  arrange(row_id) %>%  
  select(-row_id) 

df_transitions_prev <- df_transitions_prev %>% 
  filter(transition == FALSE)


# Filtra solo le righe dove avviene la transizione
df_transitions <- df_metric %>%
  filter(transition)
df_transitions <- df_transitions[,-22]

df_trans <- rbind(df_transitions, df_transitions_prev)
df_trans$ID <- as.numeric(df_trans$ID)
df_trans <- df_trans %>% 
  arrange(ID, Visione, Start..ms.)


df_trans <- df_trans %>%
  group_by(ID, Visione, Posizione) %>%
  mutate(attention_before = if_else(!transition, Standardized_Value_Attention, NA_real_),
         attention_after = if_else(transition, Standardized_Value_Attention, NA_real_) 
  )


## controlla anche che il rientro sia effettivamente il rientro...
table(df_transitions$Binning)
## appost


df_try <- df_trans %>%
  filter(transition == TRUE) %>% 
  arrange(ID, Visione, Start..ms.)

table(df_try$Posizione)
## tutte le entrate in pubblicità corrispondono alla prima posizione, qiuindi appost 👍 



## valuto inizialmente solo le righe in cui c'è l'ingresso in pubblicità
df_trans$entrata_pubblicita <- ifelse(df_trans$transition & substr(df_trans$Label, 1, 2) == "00" |
                                        grepl("_A$", df_trans$Label),1,0)
## entrata_pubblicita è uguale a 1 se equivale a un'entrata in pubblicità, 0 se a un ritorno al contenuto editoriale

df_trans <- df_trans %>%
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, Binning, Label, transition, Pubblicita, entrata_pubblicita, everything())

df_ad_starting <- df_trans %>%
  filter(entrata_pubblicita == 1) %>% 
  group_by(ID, Visione, Pubblicita) %>% 
  summarise(across(c(attention_before, attention_after), mean, na.rm = TRUE))

attenzione_pre <- df_ad_starting[,4]
attenzione_pre <- na.omit(attenzione_pre)

attenzione_post <- df_ad_starting[,5]
attenzione_post <- na.omit(attenzione_post)

df_entrata <- cbind(attenzione_pre, attenzione_post)
df_entrata$attention_change <- df_entrata$attention_after - df_entrata$attention_before
attenzione_media_ad <- mean(df_entrata$attention_change)
attenzione_min_ad <- min(df_entrata$attention_change)
attenzione_max_ad <- max(df_entrata$attention_change)


## ora controllo il livello di attenzione all'uscita dalla pubblicità
df_content_starting <- df_trans %>%
  filter(entrata_pubblicita == 0) %>% 
  group_by(ID, Visione, Pubblicita) %>% 
  summarise(across(c(attention_before, attention_after), mean, na.rm = TRUE))

attenzione_pre_cont <- df_content_starting[,4]
attenzione_pre_cont <- na.omit(attenzione_pre_cont)

attenzione_post_cont <- df_content_starting[,5]
attenzione_post_cont <- na.omit(attenzione_post_cont)

df_uscita <- cbind(attenzione_pre_cont, attenzione_post_cont)
df_uscita$attention_change <- df_uscita$attention_after - df_uscita$attention_before
attenzione_media_cont <- mean(df_uscita$attention_change)
attenzione_min_cont <- min(df_uscita$attention_change)
attenzione_max_cont <- max(df_uscita$attention_change)


xlim_range <- range(c(df_entrata$attention_change, df_uscita$attention_change), na.rm = TRUE)

p1 <- ggplot(df_entrata, aes(x = attention_change)) +
  geom_histogram(binwidth = 0.25, fill = "lightblue", color = "navy") + 
  geom_vline(aes(xintercept = attenzione_media_ad), color = "salmon", size = 2) + 
  labs(title = "Distribution of Attention at Advertisement Entry", x = "Attention", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Media:", round(attenzione_media_ad, 4), 
                                                   "\nMinimo:", round(attenzione_min_ad, 4), 
                                                   "\nMassimo:", round(attenzione_max_ad, 4)), 
           hjust = 1.1, vjust = 1.1, color = "navy", parse = FALSE, size = 8) +
  coord_cartesian(xlim = xlim_range) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 24, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 20, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 18, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 18, colour = 'navy'),
    plot.title = element_text(size = 26, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )


p2 <- ggplot(df_uscita, aes(x = attention_change)) +
  geom_histogram(binwidth = 0.25, fill = "lightblue", color = "navy") + 
  geom_vline(aes(xintercept = attenzione_media_cont), color = "salmon", size = 2) + 
  labs(title = "Distribution of Attention at Advertisement Exit", x = "Attention", y = "Frequency") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = paste("Media:", round(attenzione_media_cont, 4), 
                                                   "\nMinimo:", round(attenzione_min_cont, 4), 
                                                   "\nMassimo:", round(attenzione_max_cont, 4)), 
           hjust = 1.1, vjust = 1.1, color = "navy", parse = FALSE, size = 8) +
  coord_cartesian(xlim = xlim_range) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 24, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 20, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 18, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 18, colour = 'navy'),
    plot.title = element_text(size = 26, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )


plot <- grid.arrange(p1, p2, nrow = 2)

df_entrata$pos <- 'entrata'
df_uscita$pos <- 'uscita'
df_comparison <- rbind(df_entrata,df_uscita)
kruskal.test(attention_change ~ pos, data = df_comparison)


## ggsave("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Plot/Immagini_19_giugno/distribuzione_entrata_uscita_pubblicità.png", plot = plot, width = 10, height = 8)
