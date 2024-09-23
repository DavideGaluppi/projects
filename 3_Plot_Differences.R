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

#### SET WD --> Datasets > Lineare

df_metric <- read_excel("Metriche_Standardized.xlsx")

# Rinomina le colonne
df_metric <- df_metric %>%
  dplyr::rename(
    Attention_Level = Standardized_Value_Attention,
    Engagement_Level = Standardized_Value_Engagement,
    Pleasantness = Standardized_Value_Pleasantness
  )
attach(df_metric)
#df_ad_1 <- subset(df_metric, Pubblicita == 1 & Visione == 1) 
#df_ad_2 <- subset(df_metric, Pubblicita == 1 & Visione == 2) 

df_ad<- subset(df_metric, Pubblicita == 1)

# ---- publicita ----

#ATTENTION
impatto_pub_att <- aggregate(Attention_Level ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_att, aes(x = reorder(Pubblicita, Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 15) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.05, 0.05) +
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
wilcox.test(Attention_Level ~ Pubblicita, data = df_metric, p.adjust.method = "bonferroni")

#0.012

#ENGAGEMENT

impatto_pub_eng <- aggregate(Engagement_Level ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_eng, aes(x = reorder(Pubblicita, -Engagement_Level), y = Engagement_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Engagement Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Engagement"
  ) +
  geom_text(aes(label = round(Engagement_Level, 3)), vjust = -0.5, color = "navy", size = 15) +  # aumenta la dimensione del testo
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

#0.016

#PLEASENTNESS

impatto_pub_pleas <- aggregate(Pleasantness ~ Pubblicita, data = df_metric, FUN = mean)
ggplot(impatto_pub_pleas, aes(x = reorder(Pubblicita, -Pleasantness), y = Pleasantness)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Pleasantness Mean for Ads vs Content",
    x = "Ad = 1, Content = 0",
    y = "Mean Pleasantness"
  ) +
  geom_text(aes(label = round(Pleasantness, 3)), vjust = -0.5, color = "navy", size = 15) +  # aumenta la dimensione del testo
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
wilcox.test(Pleasantness ~ Pubblicita, data = df_metric, p.adjust.method = "bonferroni")

#5.7e-05 molto significativo, le publicita erano più piacevoli


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
  ylim(-0.1, 0.1) +
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
summary(test_vision_att)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Attention_Level, df_ad$Visione, p.adjust.method = "bonferroni")

#significativo 0.0015, però le differenze fatte sulle medie non standardizzate 
#non vengono significative quidni consideriamo la visione 2 al pari della visione 1

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
  ylim(-0.15, 0.15) +
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
summary(test_vision_eng)
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
  ylim(-0.17, 0.17) +
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
summary(test_vision_pleas)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(df_ad$Pleasantness, df_ad$Visione, p.adjust.method = "bonferroni")

#significativo 0.0062, però le differenze fatte sulle medie non standardizzate 
#non vengono significative quidni consideriamo la visione 2 al pari della visione 1




# ---- 60 120 180 ----
               
#faccio sul totale (no visione 1 e visione2)

df_ad$Contenuto <- factor(df_ad$Contenuto, levels = c('60', '120', '180'), ordered = TRUE)
impatto_contenuto <- aggregate(Attention_Level ~ Contenuto, data = df_ad, FUN = mean)
ggplot(impatto_contenuto, aes(x = Contenuto, y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean per Total Durations of ADs",
    x = "Total Duration of Advertisements [s]",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 15) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.15, 0.15) +
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

test_contenuto <- kruskal.test(Attention_Level ~ Contenuto, data = df_ad)
(test_contenuto)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
dunn.test(df_ad$Attention_Level, df_ad$Contenuto,method = "bonferroni")

#       60        120   
# 120   0.2368    -     
# 180   0.0075    0.4023
#significativo solo tra 60 e 180


# ---- DURATA SINGOLA PUBBLICITA ---- 
# Grafico a barre della durata della singola pubblicità rispetto all'impatto sull'attenzione
df_ad$Durata_pubblicita <- factor(df_ad$Durata_pubblicita, levels = c('15', '30'), ordered = TRUE)
impatto_durata <- aggregate(Attention_Level ~ Durata_pubblicita, data = df_ad, FUN = mean)
ggplot(impatto_durata, aes(x = Durata_pubblicita, y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Duration of Advertisement",
    x = "Duration of Advertisement [s]",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 15) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.15, 0.15) +
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


test_contenuto_duarata <- kruskal.test(Attention_Level ~ Durata_pubblicita, data = df_ad)
(test_contenuto_duarata)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
wilcox.test(Attention_Level ~ Durata_pubblicita, data = df_ad, p.adjust.method = "bonferroni")

# significativo al 4.5e-05

# ---- CATEGORIA ----
#categoria sull'attenzione
impatto_categoria <- aggregate(Attention_Level ~ Categoria, data = df_ad, FUN = mean)
ggplot(impatto_categoria, aes(x = reorder(Categoria, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Categories",
    x = "Categories",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -1.1, color = "navy", size = 15) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.2, 0.2) +
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


test_categoria <- kruskal.test(Attention_Level ~ Categoria, data = df_ad)
(test_categoria)
#significativo. 

#vediamo con pairwise (correzioen di bonferroni per test multipli)
dunn.test(df_ad$Attention_Level, df_ad$Categoria, method = "bonferroni")

#                   Documentari   Intrattenimento   Serie tv
#Intrattenimento    0.13876       -                 -       
#  Serie tv         0.03052       1.00000           -       
#  Talk show        4.4e-09       0.00091           0.00420 

# ---- Su label ----
df_ad$Label <- as.factor(df_ad$Label)
impatto_label <- aggregate(Attention_Level ~ Label, data = df_ad, FUN = mean) %>% 
  arrange(Attention_Level)
ggplot(impatto_label, aes(x = reorder(Label, -Attention_Level), y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Ads",
    x = "Ads Label",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.9, color = "navy", size = 7) +  # aumenta la dimensione del testo
  theme_minimal()  +
  ylim(-0.35, 0.35) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 24, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 24, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 16, colour = 'navy', angle = 60),   # Increase x-axis text font size
    axis.text.y = element_text(size = 18, colour = 'navy'),
    plot.title = element_text(size = 28, face = "bold", colour = 'navy') # Increase title font size and make it bold
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

# 00_CHANEL - 00_ALFA ROMEO: p-value = 0.0092
# 00_FASTWEB - 00_ALFA ROMEO: p-value = 0.0028
# 00_FREGOLA - 00_ALFA ROMEO: p-value = 0.0000
# 00_COCA COLA - 00_AMAZON: p-value = 0.0460
# 00_IKEA - 00_AMAZON: p-value = 0.0350
# 00_LETE - 00_AMAZON: p-value = 0.0001
# 00_ORASI - 00_AMAZON: p-value = 0.0002
# 00_LETE - 00_CHANEL: p-value = 0.0003
# 00_ORASI - 00_CHANEL: p-value = 0.0008
# 00_FASTWEB - 00_COCA COLA: p-value = 0.0310
# 00_FREGOLA - 00_COCA COLA: p-value = 0.0002
# 00_FREGOLA - 00_COOP: p-value = 0.0140
# 00_IKEA - 00_FASTWEB: p-value = 0.0200
# 00_LETE - 00_FASTWEB: p-value = 0.0001
# 00_ORASI - 00_FASTWEB: p-value = 0.0003
# 00_LETE - 00_FREGOLA: p-value = 0.0000
# 00_ORASI - 00_FREGOLA: p-value = 0.0000

# ---- per posizione ----

df_ad$Posizione <- factor(df_ad$Posizione, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9'), ordered = TRUE)
impatto_posizione <- aggregate(Attention_Level ~ Posizione, data = df_ad, FUN = mean)
ggplot(impatto_posizione, aes(x = Posizione, y = Attention_Level)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  labs(
    title = "Attention Mean for Different Ad Position",
    x = "Ad Position",
    y = "Mean Attention"
  ) +
  geom_text(aes(label = round(Attention_Level, 3)), vjust = -0.5, color = "navy", size = 8) +  # aumenta la dimensione del testo
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




anova_pos <- kruskal.test(Attention_Level ~ Posizione, data = df_ad)
(anova_pos)
#significativo. pvalue:0.00957
dunn.test(df_ad$Attention_Level, df_ad$Posizione, method = "bonferroni")

results <- (dunn.test(df_ad$Attention_Level, df_ad$Posizione, method = "bonferroni"))

p_values <- as.data.frame(results$comparisons) %>% 
  rename('Comparison' = `results$comparisons`)
p_values$`p-value` = results$P.adjusted

p_values <- p_values %>% 
  filter(`p-value` <= 0.05)

as.matrix(p_values)
#       1     2     3     4     5     6     7     8    
# 2   1.000   -     -     -     -     -     -     -    
# 3   1.000 1.000   -     -     -     -     -     -    
# 4   1.000 1.000 1.000   -     -     -     -     -    
# 5   1.000 1.000 1.000 1.000   -     -     -     -    
# 6   1.000 1.000 1.000 1.000 1.000   -     -     -    
# 7   1.000 1.000 1.000 1.000 1.000 1.000   -     -    
# 8   1.000 1.000 1.000 0.617 1.000 0.478 1.000   -    
# 9   0.087 0.075 0.132 1.000 0.657 1.000 0.092 0.021

table(df_ad$Posizione)

#   1    2    3    4    5    6    7    8    9 
# 1089  937  906  619  649  458  307  238  140 



