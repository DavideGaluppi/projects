# ---- Libraries ----
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
require(pacman)
library(corrplot)
library(MASS)
library(writexl)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())

# ---- Work ----

#### SET WD --> Datasets > Lineare

setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/Lineare")
df_orig <- import('Metriche_Standardized.xlsx') 
df_pca <- import('Recall.xlsx')
df_recall <- import('Recall_adj.xlsx')

#### pulizia e creazione dataset pulito per LM ####

df_ad <- df_orig %>% 
  filter(Pubblicita == 1) %>% 
  mutate(Label = str_replace(Label, "^00_", "")) %>% 
  mutate(Label = str_replace(Label, "BARILLA", "MULINO BIANCO")) %>% 
  mutate(Label = str_replace(Label, "FREGOLA", "IL PASTAIO DI NUORO"))


df_recall$ID <- as.numeric(df_recall$ID)
df_recall <- df_recall %>%
  arrange(ID, Visione)

## il partecipante ha tutti NA perchè ha risposto che non c'erano pubblicità

df <- merge(df_ad, df_recall, by = c("ID", "Visione", "Contenuto"))


assign_recall <- function(row) {
  label_value <- row$Label
  corresponding_column <- as.character(label_value)
  row$recall <- row[[corresponding_column]]
  return(row)
}

dataset_with_recall <- df %>%
  rowwise() %>%
  mutate(recall = assign_recall(cur_data())) %>%
  ungroup()

df$recall <- dataset_with_recall$recall$recall

df <- df[, -c(23:36)]


df$Posizione[is.na(df$Posizione)] <- 0

str(df)
df$ID <- as.numeric(df$ID)
df$Visione <- as.factor(df$Visione)
df$Contenuto <- as.factor(df$Contenuto)
df$Durata_pubblicita <- as.factor(df$Durata_pubblicita)
df$Categoria <- as.factor(df$Categoria)
df$Has.Peaks <- as.factor(df$Has.Peaks)
df$Gender <- as.factor(df$Gender)
df$Pubblicita <- as.factor(df$Pubblicita)
df$Posizione <- as.factor(df$Posizione)


df <- df[, -c(12:16)]

df <- df %>% 
  group_by(ID, Visione, Label) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention, na.rm = TRUE),
            Standardized_Value_Pleasantness = mean(Standardized_Value_Pleasantness, na.rm = TRUE),
            Standardized_Value_Engagement = mean(Standardized_Value_Engagement, na.rm = TRUE),
            recall = unique(recall),
            Age = unique(Age),
            Gender = unique(Gender),
            Contenuto = unique(Contenuto),
            Posizione = unique(Posizione),
            Durata_pubblicita = unique(Durata_pubblicita),
            Categoria = unique(Categoria)
            ) %>% 
  arrange(ID, Visione) %>% 
  select(ID, Age, Gender, Visione, Label, Contenuto, everything())
  

df_pca <- df_pca[,c(1,2,8:10)]
try <- merge(df, df_pca, by = c("ID", "Visione"))

df_clean <- try

#### LM ####

detach("package:dplyr") # IMPORTANTE
library(nlme)
library(car)

corrplot(cor(sapply(df_clean[,-c(1,2,5,13)], as.numeric), use = "complete.obs"))

df_clean$Label = as.factor(df_clean$Label)

form = recall ~ Age + Gender + Categoria + Posizione + Label + 
  Standardized_Value_Pleasantness + Standardized_Value_Attention + Standardized_Value_Engagement + 
  PCA_contenuto + PCA_AD + Propensione_acquisto_prodotto 
## dalla formula tolgo Contenuto perché la variabile dipendente Recall è gia pesata sul Contenuto(60vs120vs180)
## non inserisco nemmeno Visione in quanto abbiamo già visto come non ci siano differenze fra Visione 1 e 2 in termini di Attenzione




## STEPWISE SELECTION

df_stepwise <- na.omit(df_clean)

model_step <- lm(formula = form, data = df_stepwise)
summary(model_step)

miminal_model <- lm(recall ~ 1, data = df_stepwise)
forward = stepAIC(miminal_model,direction="forward",scope=list(lower=miminal_model,upper=model_step))

backward = stepAIC(model_step,details = T, direction = "backward")

both = stepAIC(model_step,details = T, direction = "both")




### 0) LM con residui omoschedastici ###

model0 <- lm(recall ~ Age + Propensione_acquisto_prodotto + Standardized_Value_Engagement, data = df_stepwise)
summary(model0)
anova(model0)
AIC(model0) # 1934.931
vif(model0)


plot(fitted(model0), residuals(model0))
qqnorm(residuals(model0))
qqline(residuals(model0))
shapiro.test(residuals(model0))
par(mfrow = c(2,2))
plot(model0)
## sembra che i residui non siano normali ne omoschedastici



### 1) LM heteroscedasticity ###

# varPower
weights = varPower()
model1 <- gls(recall ~ Age + Propensione_acquisto_prodotto + Standardized_Value_Engagement, weights = weights, data = df_stepwise)
summary(model1) 
AIC(model1) # AIC 1963.511
anova(model1)



### 2) LINEAR MIXED MODELS ###

model2 <- lme(recall ~ Age + Propensione_acquisto_prodotto + Standardized_Value_Engagement, 
                random = ~1|ID, weights = weights, data = df_stepwise, 
                control = lmeControl(maxIter = 1000, msMaxIter = 1000, opt = "optim"))
summary(model2)
anova(model2)
AIC(model2) # 1887.108 LOWEST!


printCoefmat(summary(model2)$tTable, has.Pvalue = TRUE, P.values = TRUE)
plot(ranef(model2))



## scelta del modello migliore
AIC(model0, model1, model2)





