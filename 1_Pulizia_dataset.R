# ---- Libraries ----
# Load necessary libraries
library(dplyr)  # for bind_rows

library(stringr)

library(tidyr)

require(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr)

library(readxl)



rm(list = ls())

# ---- WORK ----

#### SET WD --> Datasets > Exports_Omen

# ---- merge_duplicate_columns function ----
#Create function to merge the duplicate columns (the ones for each different EEG)
merge_duplicate_columns <- function(df) {
  # Get duplicate column names with '.1'
  duplicates <- grepl("\\.1|row_id", names(df)) #also get row id
  
  #if there are duplicates column, store them in a new df and remove the from original df
  if (any(duplicates == T)){
    dupl_df <- df[,duplicates]
    dupl_df <- as.data.frame(dupl_df)
    
    duplicates[length(duplicates)] <- FALSE
    df <- df[,!duplicates]
    
    colnames(dupl_df) <- gsub("\\.1$", "", colnames(dupl_df)) #make the duplicate column names match the original
    
    #for each column in original df, if the cell is NA, find the equivalent in the dupl_df and substitute
    for (col in colnames(df)){
      for (i in df$row_id){
        if (is.na(df[i, col])){
          if (any(names(dupl_df) == col)){
            df[i,col] <- dupl_df[i,col]  
          }
          
        }
      }
    }
  }
  
  return(df)
}

#---- reading and storing all the .csv files ----

# Get a list of all CSV files (including subdirectories)
file.list <- list.files(pattern = "TV_OLD\\.csv|TV\\.csv", recursive = TRUE)

# Initialize an empty list to store data frames
df.list <- list()

# Read each CSV file into a data frame and store it in the list
for (file in file.list) {
  df <- read.csv(file, header = FALSE)  # No automatic header
  
  # Specify the target value you're looking for
  target_value <- "Study Name"
  
  # Find the row index where the target value appears in the first column
  target_row_index <- which(df$V1 == target_value)[1]
  
  # Create a new data frame starting from the target row
  new_df <- df[target_row_index:nrow(df), ]
  
  # Set custom column names using the values from the target row
  colnames(new_df) <- as.character(new_df[1, ])
  
  # Remove the first row (used for column names)
  new_df <- new_df[-1, ]
  
  # Function to make column names unique
  make_unique <- function(names) {
    make.names(names, unique = TRUE)}
  # Apply the function to the column names
  colnames(new_df) <- make_unique(colnames(new_df))
  
  new_df$row_id <- 1:nrow(new_df) #create row id 
  
  new_df[new_df == ""] <- NA #substitute empty space with NA
  
  new_df <- merge_duplicate_columns(new_df) #apply function to merge dupl columns
  
  df.list[[file]] <- new_df #store the dataframe in a list, using as identifier the file name
  
}

# ---- Working on the combined dataframe of all the .csv files ----
# Combine the data frames into one
df_combined <- bind_rows(df.list, .id = "File") %>% # Use .id to identify the separate datasets
  arrange('Respondant.Name')

df_combined <- df_combined[,-c(length(df_combined))] #remove row_id column

df_combined$Respondent.Name <- as.character(df_combined$Respondent.Name)
df_combined$Respondent.Name <- toupper(df_combined$Respondent.Name)

df_combined <- df_combined %>%  
  filter(grepl('^S\\d|^\\d',Respondent.Name)) %>% #filter the subject in the correct form
  mutate(ID = as.numeric(gsub('\\D', "", Respondent.Name))) %>% # create a column with only subject number without S
  arrange(ID) %>% 
  mutate(Start..ms. = as.numeric(Start..ms.))

# ---- read the excel file used for notes to identify the correct Visione ----

#### SET WD --> Datasets 

visioni <- read_excel('FCP_esperimenti_20240416.xlsx', sheet = 'REVISED 13.05') %>% 
  select('ID partecipante', 'Primo contenuto', 'Secondo contenuto')
visioni <- na.omit(visioni)
visioni$`ID partecipante` <- as.integer(visioni$`ID partecipante`)
visioni[,2:3] <- lapply(visioni[,2:3], toupper)

# ---- combine visione df with data df ----
df_final <- left_join(df_combined, visioni, join_by('ID' == 'ID partecipante')) %>% 
  rename(A = 'Primo contenuto',
         B = 'Secondo contenuto') %>% #rename for easy access
  
  group_by(Respondent.Name, Study.Name) %>% 
  #if in the protocol in the study name is equal to A, the it's visione 1, else is visione 2
  mutate(Contenuto = toupper(str_extract(Study.Name, '\\d+|VOD|YouTube'))) %>% 
  mutate(Visione = case_when(Contenuto == A ~ 1, 
                             Contenuto == B ~ 2)) %>%
  
  select(-which(grepl('Ch8',colnames(df_combined)))) %>% #remove Ch8 columns
  
  select(ID, Respondent.Name, Study.Name, Contenuto, Visione, A, B, everything()) %>% #reorder columns
  
  select(-c('Gender', 'Age', 'Group', 'Type', 'Parent.Stimulus', 'Comment', 
            'Signal.Duration', 'Duration..ms.')) %>% #remove useless columns
  select(-File) %>% 
  
  arrange(ID, Visione, Start..ms.) %>% 
# ----Correcting the video label (for protocol 60 the names of the ads are different)----
  mutate(Label = toupper(Label))  %>% 
  mutate(Label = case_when(grepl('^FREGOLA',Label) ~ '00_FREGOLA', 
                           grepl('^CHANEL',Label) ~ '00_CHANEL',
                           grepl('^MULTICENTRUM',Label) ~ '00_MULTICENTRUM',
                           grepl('^IKEA ITALIA',Label) ~ '00_IKEA',
                           grepl('^LETE',Label) ~ '00_LETE',
                           grepl('^AMAZON',Label) ~ '00_AMAZON',
                           grepl('^MUTTI1',Label) ~ '00_MUTTI',
                           grepl('^COOP',Label) ~ '00_COOP',
                           grepl('^ORASI',Label) ~ '00_ORASI',
                           grepl('^ALFA TONALE',Label) ~ '00_ALFA ROMEO',
                           grepl('^BARILLA',Label) ~ '00_BARILLA',
                           grepl('^COCA COLA',Label) ~ '00_COCA COLA',
                           grepl('^PROPAGANDA LIVE A', Label) ~ 'PROPAGANDA LIVE_A',
                           .default = Label)) %>% 
  #correct an error with respondent 168 (for protocol 120 it has two data, but only the "B" is the complete one)
  filter(!(Respondent.Name == 'S168' & Study.Name == 'FCP_lineare_120_Smartphone')) %>% 
  mutate(Respondent.Name = ifelse(grepl('^S168B',Respondent.Name), 'S168', Respondent.Name)) %>% 
  # add S in case the respondent name starts with a number
  mutate(Respondent.Name = ifelse(grepl('^\\d',Respondent.Name), str_c('S', Respondent.Name), Respondent.Name))

#data type correction
df_final[,c(9:length(df_final))] <- lapply(df_final[,c(9:length(df_final))], as.numeric)
df_final[,c(1:8, 10)] <- lapply(df_final[,c(1:8, 10)], as.factor)
df_final[,c(11)] <- lapply(df_final[,c(11)], as.integer)

columns <- which(grepl('Cluster', colnames(df_final))|
                   grepl('Asymmetry', colnames(df_final)))

DF <- df_final[,c(1:13, columns)]
DF[,c(12:23)] <- lapply(DF[,c(12:23)], as.numeric)

DF_lineare <- DF %>% 
  filter(!(grepl('VOD|YOUTUBE', Contenuto)))
DF_VOD <- DF %>% 
  filter((grepl('VOD|YOUTUBE', Contenuto)))

# export(DF, 'DATASET_TV.xlsx')
# export(DF_lineare, 'DATASET_TV_LINEARE.xlsx')
# export(DF_VOD, 'DATASET_TV_VOD.xlsx')

