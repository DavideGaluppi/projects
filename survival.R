#libraries to install plyr rpart.plot caret gridExtra tidyverse rsample e1071 GGally data.table DT readr ggplot2 dplyr tidyr corrplot rms MASS e1071 ROCR gplots pROC rpart randomForest ggpubr
library(plyr)
library(rpart.plot)
library(caret) 
library(gridExtra) 
library(tidyverse) 
library(rsample) 
library(e1071) 
library(GGally) 
library(data.table) 
library(DT) 
library(readr) 
library(ggplot2) 
library(dplyr) 
library(tidyr) 
library(corrplot) 
library(rms) 
library(MASS) 
library(e1071) 
library(ROCR) 
library(gplots) 
library(pROC) 
library(rpart) 
library(randomForest) 
library(ggpubr)

#Churn <- read_csv("...TelcoChurn.csv")
#take a glimpse at the dataset
churn<-read_csv("TelcoChurn.csv")
View(churn)
## there are some columns where instead of having a binary response we have a third response equal to one of the existings (ex. "no phone services" equal to "no")
#identifying missing values and incomplete records
sapply(churn, function(x) sum(is.na(x))) 
#let's look at the records with missing values
churn[is.na(churn$TotalCharges),]
#clean the missing values
churn_clean <- churn[complete.cases(churn), ]
#check on the structure of the variable
View(churn_clean)
#adapt senior citizens
churn_clean$SeniorCitizen <- as.factor(mapvalues(churn_clean$SeniorCitizen, from=c("0","1"), to=c("No", "Yes")))
#adapt multiple lines
churn_clean$MultipleLines <- as.factor(mapvalues(churn_clean$MultipleLines, from=c("No phone service"), to=c("No")))
#lines 10-15 adaptation
churn_clean$OnlineSecurity <- as.factor(mapvalues(churn_clean$OnlineSecurity, from= c("No internet service"), to= c("No")))
churn_clean$InternetService <- as.factor(mapvalues(churn_clean$InternetService, from= c("Fiber optic"), to= c("Fiber")))
churn_clean$OnlineBackup <- as.factor(mapvalues(churn_clean$OnlineBackup, from= c("No internet service"), to= c("No")))
churn_clean$DeviceProtection <- as.factor(mapvalues(churn_clean$DeviceProtection, from= c("No internet service"), to= c("No")))
churn_clean$TechSupport <- as.factor(mapvalues(churn_clean$TechSupport, from= c("No internet service"), to= c("No")))
churn_clean$StreamingTV <- as.factor(mapvalues(churn_clean$StreamingTV, from= c("No internet service"), to= c("No")))
churn_clean$StreamingMovies <- as.factor(mapvalues(churn_clean$StreamingMovies, from= c("No internet service"), to= c("No")))

#erase customer ID (useless)
churn_clean$customerID <- NULL
#multicollinearity check
churn_clean %>% 
  dplyr::select (TotalCharges, MonthlyCharges, tenure) %>% 
  cor() %>% 
  corrplot.mixed(upper = "circle", tl.col = "black", number.cex = 0.7)
#as total charges correlates, in decision tree we apply total charges as output variable. Let's split training set and test set
set.seed(56) 
split_train_test <- createDataPartition(churn_clean$Churn,p=0.7,list=FALSE) 
dtrain<- churn_clean[split_train_test,] 
dtest<- churn_clean[-split_train_test,] 
#removing total charges from training dataset --> because it was collinear with MonthlyCharges and tenure 's variables
dtrain <- dtrain[,-19] 
dtest <- dtest[,-19]
#exploring the tree
tr_fit <- rpart(Churn ~., data = dtrain, method="class")
rpart.plot(tr_fit)

#Confusion matrix
tr_prob1 <- predict(tr_fit, dtest) 
tr_pred1 <- ifelse(tr_prob1[,2] > 0.5,"Yes","No")
## No means No Churn --> the customer should not churn
table(Predicted = tr_pred1, Actual = dtest$Churn)
## the model is more powerfull in computing NO instead of YES (in understanding well if the costumer is leaving instead of remaining)
#overall accuracy
tr_prob2 <- predict(tr_fit, dtrain)
tr_pred2 <- ifelse(tr_prob2[,2] > 0.5,"Yes","No")
tr_tab1 <- table(Predicted = tr_pred2, Actual = dtrain$Churn)
tr_tab2 <- table(Predicted = tr_pred1, Actual = dtest$Churn)
#Confusion matrix test set
confusionMatrix( as.factor(tr_pred1), as.factor(dtest$Churn), positive = "Yes" )
## confusion matrix shows us some metrics including Accuracy
#Confusion matrix training set
confusionMatrix(
    as.factor(tr_pred2), 
    as.factor(dtrain$Churn), 
    positive = "Yes"
    )


#random forest
churn_clean$Churn <- as.factor(mapvalues(churn_clean$Churn, from=c("1","0"), to=c(1, 0)))
dtest$Churn <- as.factor(mapvalues(dtest$Churn, from=c("Yes","No"), to=c(1, 0)))
dtrain$Churn <- as.factor(mapvalues(dtrain$Churn, from=c("Yes","No"), to=c(1, 0)))
#Set control parameters for random forest model selection
ctrl <- trainControl(method = "cv", number=5, classProbs = TRUE, summaryFunction = twoClassSummary)
#Exploratory random forest model selection
rf_fit1 <- train(Churn ~., data = dtrain, method = "rf", ntree = 75, tuneLength = 5, metric = "ROC", trControl = ctrl)
saveRDS(rf_fit1, "Churn.RDS")
rf_fit1 <- readRDS("Churn.RDS")
#Run optimal model
rf_fit2 <-randomForest(Churn ~., data = dtrain, ntree = 75, mtry = 2, importance = TRUE, proximity = TRUE)
#Display variable importance from random tree
varImpPlot(rf_fit2, sort=T, n.var = 10, 
           main = 'Top 10 important variables')
rf_pred1 <- predict(rf_fit2, dtest)
table(Predicted = rf_pred1, Actual = dtest$Churn)
plot(rf_fit2)
#Accuracy analysis
rf_pred2 <- predict(rf_fit2, dtrain)
rf_tab1 <- table(Predicted = rf_pred2, Actual = dtrain$Churn)
rf_tab2 <- table(Predicted = rf_pred1, Actual = dtest$Churn)
#training
confusionMatrix(
  as.factor(rf_pred2),
  as.factor(dtrain$Churn),
  positive = "1" 
)
rf_acc <- sum(diag(rf_tab2))/sum(rf_tab2)
rf_acc


#logistic regression
churn_clean$Churn <- as.factor(mapvalues(churn_clean$Churn, from=c("Yes","No"), to=c("1", "0")))
lr_fit <- glm(Churn ~., data = dtrain,
              family=binomial(link='logit'))
summary(lr_fit)
#confusion matrix for logistic regression
lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"1","0")
table(Predicted = lr_pred1, Actual = dtest$Churn)
#overall accuracy
lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"1","0")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$Churn)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$Churn)
# Train
confusionMatrix(as.factor(lr_pred2), as.factor(dtrain$Churn), positive = "1")
## looking at table and accuracy it seems that the second method (with logistic regression) predict better the results 

#data visualization - contract
p21 <- ggplot(churn_clean, aes(x = Contract, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = after_stat(count) -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by contract status")

p21
#data visualization - service
p22 <- ggplot(churn_clean, aes(x = InternetService, fill = Churn)) +
  geom_bar() +
  geom_text(aes(y = after_stat(count) -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3) +
  labs(title="Churn rate by internet service status")

p22
## having an internet service is worse than 
## customers with Adsl are more prone to churn respect to those who has the fiber
#data visualization for tenure
p23 <- ggplot(churn_clean, aes(x = tenure, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Months",
       title = "Churn rate by tenure")
p23
## show the probability of churn 
## the probability of churn increases with time goes on (the blue line goes smaller)


## SURVIVAL ANALYSIS

churn_clean$Churn_n <- ifelse(churn_clean$Churn == "Yes", 1, 0)

install.packages ("survival")
library(survival)

str (churn_clean)
surv <- Surv(time = churn_clean$tenure, event = churn_clean$Churn_n)
km_model <- survfit(surv ~ churn_clean$Contract, data = churn_clean)
autoplot(km_model, main = "Kaplan-Meier Survival Curve for Churn Rate", pval=TRUE, conf.int = TRUE)
summary(km_model)

