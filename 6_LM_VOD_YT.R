# ---- Libraries ----
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(stringr)
library(tidyr)
require(pacman)
library(corrplot)
library(MASS)
library(writexl)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

rm(list = ls())
graphics.off()

# ---- Work ----

#### SET WD --> Datasets > VOD + YouTube

setwd("/Users/davide/Library/CloudStorage/OneDrive-PolitecnicodiMilano/Project AFB/Complete dataset/VOD + YouTube")
df_orig <- import('Metriche_Standardized_VOD_YT.xlsx') 
df_pca <- import('Recall VOD_YT.xlsx')
df_recall <- import('Recall_adj VOD.xlsx')
df_premid <- import('pre_mid.xlsx')

df_orig$Label <- toupper(df_orig$Label)



assign_category <- function(video_title) {
  if (grepl("MINIONS|GRU", video_title, ignore.case = TRUE)) {
    return("Fiction/serie")
  } else if (grepl("FERRATA|PISTE|OPPENHEIMER|SECONDA GUERRA MONDIALE|UOMO BIONICO", video_title, ignore.case = TRUE)) {
    return("Documentari/cultura")
  } else if (grepl("TEDX|TRUCCO|CROSTATA|RISOTTO|SPORT|IMPRENDITORE|STILE|VOLVO|CICLOVIA|APPLE|DIFENSORE", video_title, ignore.case = TRUE)) {
    return("Intrattenimento")
  } else if (grepl("GUCCI|RICICLARE", video_title, ignore.case = TRUE)) {
    return("Attualità/informazione")
  } else {
    return(NA) # In caso non corrisponda a nessuna categoria
  }
}

df_orig <- df_orig %>%
  mutate(Categoria_yt = sapply(Video_YT, assign_category))

df_orig <- df_orig %>%
  mutate(Categoria = ifelse(!is.na(Categoria), Categoria, Categoria_yt))



df_ad <- df_orig %>% 
  filter(Pubblicita == 1) %>% 
  mutate(Label = str_replace(Label, "^00_", "")) %>% 
  mutate(Label = str_replace(Label, "BARILLA", "MULINO BIANCO")) %>% 
  mutate(Label = str_replace(Label, "FREGOLA", "IL PASTAIO DI NUORO"))

df_premid <- df_premid %>% 
  mutate(Label = str_replace(Label, "BARILLA,MULINO BIANCO,MACINE", "MULINO BIANCO")) %>% 
  mutate(Label = str_replace(Label, "FREGOLA,IL PASTAIO DI NUORO,NUORO,PASTAIO", "IL PASTAIO DI NUORO")) %>% 
  mutate(Label = str_replace(Label, "ALFA ROMEO,TONALE", "ALFA ROMEO")) %>% 
  mutate(Label = str_replace(Label, "LETE,ACQUA LETE", "LETE"))

df_ad <- left_join(df_ad, df_premid, by = c("ID", "VISIONE", "Label", "Standardized_Value_Attention"))
names(df_ad)[names(df_ad) == "VISIONE"] <- "Visione"

df_ad$Timing <- as.factor(df_ad$Timing)


df_recall$ID <- as.numeric(df_recall$ID)
df_recall <- df_recall %>%
  arrange(ID, Visione)

df <- left_join(df_ad, df_recall, by = c("ID", "Visione"))


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
df <- df[, -c(23,25,28:41)]

#### pulizia e creazione dataset pulito per LM ####


names(df)[names(df) == "VOD_YT.x"] <- "VOD_YT"
  

df_new <- left_join(df, df_pca[,c(1,4,8:10)], by = c("ID", "Visione"))

df_new$ID <- as.numeric(df_new$ID)
df_new$Visione <- as.factor(df_new$Visione)
df_new$Gender <- as.factor(df_new$Gender)
df_new$Categoria <- as.factor(df_new$Categoria)

df_new <- df_new[,-c(2,3, 6:9, 12:19)]


df_new <- df_new %>% 
  group_by(ID, Visione, Label) %>% 
  summarise(Standardized_Value_Attention = mean(Standardized_Value_Attention, na.rm = TRUE),
            Standardized_Value_Pleasantness = mean(Standardized_Value_Pleasantness, na.rm = TRUE),
            Standardized_Value_Engagement = mean(Standardized_Value_Engagement, na.rm = TRUE),
            VOD_YT = unique(VOD_YT),
            recall = unique(recall),
            Age = unique(Age),
            Gender = unique(Gender),
            Categoria = unique(Categoria),
            Timing = unique(Timing),
            PCA_contenuto = unique(PCA_contenuto),
            PCA_AD = unique(PCA_AD),
            Propensione_acquisto_prodotto = unique(Propensione_acquisto_prodotto)
  ) %>% 
  arrange(ID, Visione) 


df_vod <- df_new %>% 
  filter(VOD_YT == "VOD")
df_yt <- df_new %>% 
  filter(VOD_YT == "YOUTUBE")



#### LM YT ####

detach("package:dplyr") # IMPORTANTE
library(nlme)

form = formula(recall ~ Age + Gender + Categoria + VOD_YT + Timing +
                 Standardized_Value_Pleasantness + 
                 Standardized_Value_Attention + Standardized_Value_Engagement + PCA_contenuto + 
                 PCA_AD + Propensione_acquisto_prodotto)



## STEPWISE SELECTION

df_stepwise <- na.omit(df_yt)

model_step <- lm(formula = form, data = df_stepwise)
summary(model_step)

miminal_model <- lm(recall ~ 1, data = df_stepwise)
forward = stepAIC(miminal_model,direction="forward",scope=list(lower=miminal_model,upper=model_step))

backward = stepAIC(model_step,details = T, direction = "backward")

both = stepAIC(model_step,details = T, direction = "both")


###### YOUTUBE

### 0) LM homoscedastic

model0 <- lm(recall ~ Gender + Standardized_Value_Attention + Propensione_acquisto_prodotto, data = df_stepwise)
summary(model0)
anova(model0)
AIC(model0) # 850.6111
vif(model0) # tutti attorno a 1


plot(fitted(model0), residuals(model0))
qqnorm(residuals(model0))
qqline(residuals(model0))
shapiro.test(residuals(model0))
par(mfrow = c(2,2))
plot(model0)


### 1) LM heteroscedastic

df_stepwise$ID <- as.factor(df_stepwise$ID)

weights = varPower()
model1 <- gls(recall ~ Gender + Standardized_Value_Attention + Propensione_acquisto_prodotto, weights = weights, data = df_stepwise)
summary(model1) 
AIC(model1) # 864.0154


### 2) LMM

# str(df_yt)
# df_yt$Label <- as.factor(df_yt$Label)

# weights = varPower()
model2 <- lme(recall ~ Gender + Standardized_Value_Attention + Propensione_acquisto_prodotto, 
              random = ~1|ID, data = df_stepwise, na.action = na.omit)
summary(model2)
anova(model2)
AIC(model2) # 859.8949

AIC(model0,model1,model2)




## STEPWISE SELECTION

df_stepwise <- na.omit(df_vod)

model_step <- lm(formula = form, data = df_stepwise)
summary(model_step)

miminal_model <- lm(recall ~ 1, data = df_stepwise)
forward = stepAIC(miminal_model,direction="forward",scope=list(lower=miminal_model,upper=model_step))

backward = stepAIC(model_step,details = T, direction = "backward")

both = stepAIC(model_step,details = T, direction = "both")


###### VOD

### 0) LM homoscedastic
# df_vod$Label <- as.factor(df_vod$Label)
model0.1 <- lm(recall ~ Gender + PCA_contenuto, data = df_stepwise)
summary(model0.1)
anova(model0.1)
AIC(model0.1) # AIC 847.1796
vif(model0.1)

plot(fitted(model0.1), residuals(model0.1))
qqnorm(residuals(model0.1))
qqline(residuals(model0.1))
shapiro.test(residuals(model0.1))
par(mfrow = c(2,2))
plot(model0.1)

### 1) LM heteroscedastic

# df_vod$ID <- as.factor(df_vod$ID)
# str(df_vod)

weights = varPower()
model1.1 <- gls(recall ~ Gender + PCA_contenuto, weights = weights, data = df_stepwise, na.action = na.omit)
summary(model1.1) 
AIC(model1.1) # 860.4978



### 2) LMM

# str(df_vod)
# df_vod$Label <- as.factor(df_vod$Label)

# weights = varPower()
model2.1 <- lme(recall ~ Gender + PCA_contenuto, random = ~1|ID, data = df_stepwise, na.action = na.omit)
summary(model2.1)
anova(model2.1)
AIC(model2.1) # 847.8976

AIC(model0.1,model1.1,model2.1)





## VOD E YOUTUBE INSIEME

df_stepwise <- na.omit(df_new)

model_step <- lm(formula = form, data = df_stepwise)
summary(model_step)

miminal_model <- lm(recall ~ 1, data = df_stepwise)
forward = stepAIC(miminal_model,direction="forward",scope=list(lower=miminal_model,upper=model_step))

backward = stepAIC(model_step,details = T, direction = "backward")

both = stepAIC(model_step,details = T, direction = "both")



### 0) LM homoscedastic

model0.2 <- lm(recall ~ Age + Gender + VOD_YT + Standardized_Value_Attention + 
                 Propensione_acquisto_prodotto, data = df_stepwise)
summary(model0.2)
anova(model0.2)
AIC(model0.2) # AIC 1696.623
vif(model0.2)

plot(fitted(model0.2), residuals(model0.2))
qqnorm(residuals(model0.2))
qqline(residuals(model0.2))
shapiro.test(residuals(model0.2))
par(mfrow = c(2,2))
plot(model0.2)


### 1) LM heteroscedastic

weights = varPower()
model1.2 <- gls(recall ~ Age + Gender + VOD_YT + Standardized_Value_Attention + 
                  Propensione_acquisto_prodotto, weights = weights, data = df_stepwise, na.action = na.omit)
summary(model1.2) 
AIC(model1.2) # 1727.238



### 2) LMM

model2.2 <- lme(recall ~ Age + Gender + VOD_YT + Standardized_Value_Attention + 
                  Propensione_acquisto_prodotto, random = ~1|ID, data = df_stepwise, na.action = na.omit)
summary(model2.2)
anova(model2.2)
AIC(model2.2) # 1709.765



## scelta del modello migliore
AIC(model0.2,model1.2,model2.2)
