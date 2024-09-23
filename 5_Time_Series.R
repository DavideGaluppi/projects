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

#IMPORT DATASET AND FIRST CLEANING

#### SET WD --> Datasets > Lineare

df_orig <- import('Metriche_Standardized.xlsx')
attach(df_orig)
df <- df_orig %>% 
  group_by(ID, Visione, Pubblicita) %>% 
  mutate(tempo = row_number()) %>%
  ungroup() %>% 
  group_by(ID, Visione, Pubblicita) %>% 
  mutate(tmax = max(tempo)) %>% 
  ungroup()

df$Contenuto <- as.factor(df$Contenuto)

max_common_time <- df %>% 
  filter(!grepl('^00_|_B', Label)) %>% 
  group_by(Contenuto) %>% 
  summarise(tmax = min(tmax))


df_A <- df %>% 
  filter(!grepl('^00_|_B', Label)) %>% 
  group_by(Contenuto, tempo) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention))

df_P <- df %>% 
  filter(grepl('^00', Label)) %>% 
  group_by(Contenuto, tempo) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention)) 
  

df_B <- df %>% 
  filter(grepl('_B$', Label)) %>% 
  group_by(Contenuto, tempo) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention)) %>% 
  group_by(Contenuto) %>% 
  mutate(tempo = row_number()) %>% 
  ungroup()

  
ggplot(df_A, aes(x = tempo, 
                 y = Standardized_Value_Attention,
                 colour = Contenuto)) +
  geom_line(linewidth = 1.5) + 
  geom_point(size = 3)+
  labs(title = 'Average Attention values (Pre-Ads) for Protocols', 
       x = '', y = 'Attention') +
  ylim(-1.75,1.75) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 25, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 25, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_blank(),   # Increase x-axis text font size
    axis.text.y = element_text(size = 22, colour = 'navy'),
    plot.title = element_text(size = 28, face = "bold", colour = 'navy') # Increase title font size and make it bold
  ) +
  geom_line(aes(x = tempo, y = rep(0, 290)), color = 'black', linewidth = 1.5)


ggplot(df_P, aes(x = tempo, 
                 y = Standardized_Value_Attention,
                 colour = Contenuto)) +
  geom_line(linewidth = 2) + 
  geom_point(size = 3)+
  labs(title = 'Average Attention values (Ads) for Protocols', 
       x = '', y = 'Attention') +
  theme_minimal() +
  ylim(-2,2) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 25),
    legend.title = element_text(size = 25),
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 25, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 25, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_blank(),   # Increase x-axis text font size
    axis.text.y = element_text(size = 22, colour = 'navy'),
    plot.title = element_text(size = 28, face = "bold", colour = 'navy') # Increase title font size and make it bold
  ) +
  geom_line(aes(x = tempo, y = rep(0, 95)), color = 'black', linewidth = 1.5)


ggplot(df_B, aes(x = tempo, 
                 y = Standardized_Value_Attention,
                 colour = Contenuto)) +
  geom_line(linewidth = 2) + 
  geom_point(size = 3)+
  labs(title = '', 
       x = 'After-Ads', y = '') +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 18, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 18, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_blank(),   # Increase x-axis text font size
    axis.text.y = element_blank(),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  ) +
  geom_line(aes(x = tempo, y = rep(0, 231)), color = 'grey')
