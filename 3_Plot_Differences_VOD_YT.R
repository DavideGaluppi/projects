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
library(dunn.test)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())
#dev.off()

# ---- WORK ---- 

#### SET WD --> Datasets > VOD + YouTube

df_metric <- import("Metriche_Standardized_VOD_YT.xlsx")

# Rinomina le colonne
df_metric <- df_metric %>%
  dplyr::rename(
    Attention_Level = Standardized_Value_Attention,
    Engagement_Level = Standardized_Value_Engagement,
    Pleasantness = Standardized_Value_Pleasantness,
    Visione = VISIONE
  )
attach(df_metric)
#df_ad_1 <- subset(df_metric, Pubblicita == 1 & Visione == 1) 
#df_ad_2 <- subset(df_metric, Pubblicita == 1 & Visione == 2) 

df_ad<- subset(df_metric, Pubblicita == 1)

# ---- publicita ----

#ATTENTION
impatto_pub_att <- aggregate(Attention_Level ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_att, aes(x = reorder(Pubblicita, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.12, 0.12) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 28, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 28, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

anova_pubb_att <- kruskal.test(Attention_Level ~ Pubblicita, data = df_metric)
(anova_pubb_att)

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_metric$Attention_Level, df_metric$Pubblicita, p.adjust.method = "bonferroni")

#0.00012

#ENGAGEMENT

impatto_pub_eng <- aggregate(Engagement_Level ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_eng, aes(x = reorder(Pubblicita, -Engagement_Level), y = Engagement_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Engagement Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Engagement"
  ) +
  geom_text(aes(label = round(Engagement_Level, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.1, 0.1) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 28, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 28, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )


anova_pubb_eng <- kruskal.test(Engagement_Level ~ Pubblicita, data = df_metric)
(anova_pubb_eng)

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(Engagement_Level ~ Pubblicita, data = df_metric, p.adjust.method = "bonferroni")

#0.67 non significativo

#PLEASENTNESS

impatto_pub_pleas <- aggregate(Pleasantness ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_pleas, aes(x = reorder(Pubblicita, -Pleasantness), y = Pleasantness)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Pleasantness Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Pleasantness"
  ) +
  geom_text(aes(label = round(Pleasantness, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.1, 0.1) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 28, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 28, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )


anova_pubb_pleas <- kruskal.test(Pleasantness ~ Pubblicita, data = df_metric)
(anova_pubb_pleas)

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_metric$Pleasantness, df_metric$Pubblicita, p.adjust.method = "bonferroni")

#0.91 non significativo


# ---- VISIONE 1 VS 2 ----

df_ad$Visione <- factor(df_ad$Visione, levels = c('1', '2'), ordered = TRUE)
impatto_visioni <- aggregate(Attention_Level ~ Visione, data = df_ad, FUN = mean)
ggplot(impatto_visioni, aes(x = Visione, y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for different Vision",
    x = "Vision",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 5) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.13, 0.13) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12, colour = 'navy'),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

test_vision_att <- kruskal.test(Attention_Level ~ Visione, data = df_ad)
(test_vision_att)
#non significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Attention_Level, df_ad$Visione, p.adjust.method = "bonferroni")

#0.99 non significativo

####ENGAGEMENT

df_ad$Visione <- factor(df_ad$Visione, levels = c('1', '2'), ordered = TRUE)
impatto_visioni <- aggregate(Engagement_Level ~ Visione, data = df_ad, FUN = mean)
ggplot(impatto_visioni, aes(x = Visione, y = Engagement_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Engagement Mean for different Vision",
    x = "Vision",
    y = "Mean Engagement"
  ) +
  geom_text(aes(label = round(Engagement_Level, 3)), vjust = -0.5, color = "navy", size = 5) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.18, 0.18) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12, colour = 'navy'),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

test_vision_eng <- kruskal.test(Engagement_Level ~ Visione, data = df_ad)
(test_vision_eng)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Engagement_Level, df_ad$Visione, p.adjust.method = "bonferroni")

#significativo 0.0023, però le differenze fatte sulle medie non standardizzate 
#non vengono significative quidni consideriamo la visione 2 al pari della visione 1

####PLEASENTNESS

df_ad$Visione <- factor(df_ad$Visione, levels = c('1', '2'), ordered = TRUE)
impatto_visioni <- aggregate(Pleasantness ~ Visione, data = df_ad, FUN = mean)
ggplot(impatto_visioni, aes(x = Visione, y = Pleasantness)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Pleasantness Mean for different Vision",
    x = "Vision",
    y = "Mean Pleasentness"
  ) +
  geom_text(aes(label = round(Pleasantness, 3)), vjust = -0.5, color = "navy", size = 5) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.007, 0.007) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 14, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 14, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 12, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 12, colour = 'navy'),
    plot.title = element_text(size = 16, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

test_vision_pleas <- kruskal.test(Pleasantness ~ Visione, data = df_ad)
(test_vision_pleas)
#non significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Pleasantness, df_ad$Visione, p.adjust.method = "bonferroni")

#non significativo 0.94, ricordare la scalaaaaaa




# ---- YT vs VOD ----

#faccio sul totale (no visione 1 e visione2)

impatto_contenuto <- aggregate(Attention_Level ~ VOD_YT, data = df_ad, FUN = mean)
ggplot(impatto_contenuto, aes(x = VOD_YT, y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean VOD vs YOUTUBE",
    x = "",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.3, 0.3) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 28, colour = 'navy'),   # Increase x-axis text font size
    axis.text.y = element_text(size = 28, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )
test_contenuto <- kruskal.test(Attention_Level ~ VOD_YT, data = df_ad)
(test_contenuto)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Attention_Level, df_ad$VOD_YT, p.adjust.method = "bonferroni")

#2.2e-15 molto signifcativo, youtube ha attenzione maggiore!!!


# ---- CATEGORIA solo VOD----
#categoria sull'attenzione
df_ad.vod <- subset(df_ad, VOD_YT == 'VOD')

impatto_categoria <- aggregate(Attention_Level ~ Categoria, data = df_ad.vod, FUN = mean)
ggplot(impatto_categoria, aes(x = reorder(Categoria, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Categories (VOD)",
    x = "Categories",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.4, 0.4) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 24, colour = 'navy', angle = 45, vjust = 0.6),   # Increase x-axis text font size
    axis.text.y = element_text(size = 24, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

test_categoria <- kruskal.test(Attention_Level ~ Categoria, data = df_ad.vod)
(test_categoria)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
dunn.test(df_ad.vod$Attention_Level, df_ad.vod$Categoria,method = "bonferroni")

#                         Attualità/informazione  Documentari/cultura   Fiction/serie
# Documentari/cultura             0.2513                 -                   -            
#   Fiction/serie                 0.9913                 0.0017              -            
#   Intrattenimento               1.0000                 0.0021              1.0000      

# ---- piattaforma solo VOD----
#categoria sull'attenzione
df_ad.vod <- subset(df_ad, VOD_YT == 'VOD')

impatto_piattaforma <- aggregate(Attention_Level ~ Piattaforma_VOD, data = df_ad.vod, FUN = mean)
ggplot(impatto_piattaforma, aes(x = reorder(Piattaforma_VOD, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Platforms (VOD)",
    x = "Platforms",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 10) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.35, 0.35) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 24, colour = 'navy', angle = 45, vjust = 0.6),   # Increase x-axis text font size
    axis.text.y = element_text(size = 24, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

test_piattaforma <- kruskal.test(Attention_Level ~ Piattaforma_VOD, data = df_ad.vod)
(test_piattaforma)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
dunn.test(df_ad.vod$Attention_Level, df_ad.vod$Piattaforma_VOD, method = "bonferroni")

#               Mediaset Infinity   RaiPlay     Rivedila7
# RaiPlay             0.716             -           -        
# Rivedila7           1.000           0.488         -        
# SkyGO               4.6e-05         0.011       1.8e-05 
#skygo l'unica significativa e negativa!! le pubblicità su skygo abbassano l'attenzione?


# ---- Su label solo ----

impatto_label <- aggregate(Attention_Level ~ Label, data = df_ad, FUN = mean) %>% 
  arrange(Attention_Level)
ggplot(impatto_label, aes(x = reorder(Label, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Ads",
    x = "Ads Label",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -1.5, color = "navy", size = 7) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.7, 0.7) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 30, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 30, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 20, colour = 'navy', angle = 60, vjust = 0.6),   # Increase x-axis text font size
    axis.text.y = element_text(size = 20, colour = 'navy'),
    plot.title = element_text(size = 34, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )


test_label <- kruskal.test(Attention_Level ~ Label, data = df_ad)
(test_label)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
dunn.test(df_ad$Attention_Level, df_ad$Label, method = "bonferroni")


# Esegui il test t a coppie con correzione di Bonferroni
results <- dunn.test(df_ad$Attention_Level, df_ad$Label, method = "bonferroni")

# Estrai i p-value, corretti con Bonferroni
p_values <- as.data.frame(results$comparisons) %>% 
  rename('Comparison' = `results$comparisons`)
p_values$`p-value` = results$P.adjusted

p_values <- p_values %>% 
  filter(`p-value` <= 0.05)

as.matrix(p_values)

