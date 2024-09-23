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
dev.off()

# ---- work ----

#### SET WD --> Datasets > VOD + YouTube

LE_merged_C.E. <- import('LE_merged_C.E..xlsx')
LE_merged_P <- import('LE_merged_P.xlsx')

# ---- plot ----
LE_merged_C.E.$Tipo <- 'Contenuto Editoriale'
LE_merged_P$Tipo <- 'Contenuto Pubblicitario'

attention_plot <- rbind(LE_merged_C.E.[,c(1, 4, 7)], LE_merged_P[,c(1,4, 7)])
attention_plot$Tipo <- as.factor(attention_plot$Tipo)

# Create the plot
p <- ggplot(attention_plot, aes(x = ID, y = Mean_Attention, color = Tipo)) +
  geom_point(size = 3) +
  geom_line(linewidth = 3) +
  
  labs(title = "Attention Ratio between Vision 2 and Vision 1",
       x = "Observation",
       y = "Value") +
  theme_minimal()  +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 24),
    panel.grid.major = element_blank(),   # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    axis.title.x = element_text(size = 26, colour = 'navy'),  # Increase x-axis label font size
    axis.title.y = element_text(size = 26, colour = 'navy'),  # Increase y-axis label font size
    axis.text.x = element_text(size = 22),   # Increase x-axis text font size
    axis.text.y = element_text(size = 22),
    plot.title = element_text(size = 28, face = "bold", colour = 'navy') # Increase title font size and make it bold
  )

print(p)

# ---- correlation ----
colnames(LE_merged_P)  <- gsub("Mean", "Mean_P", colnames(LE_merged_P), ignore.case = TRUE)

LE <- merge(LE_merged_C.E., LE_merged_P, by = 'ID')

LE <- LE %>% 
  select(-c(6,11))
LE <- as.data.frame(sapply(LE, as.numeric))

correlation <- LE %>% 
  select(c(1:5)) %>% 
  filter(ID == 1000)
for (i in 2:5) {
  col <- colnames(LE)[i]
  correlation[[col]] <- cor(x = LE[,i], y = LE[,c(i+4)], use = 'complete')
}

