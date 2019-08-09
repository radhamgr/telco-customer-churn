library(tidyverse)

# setting the working directory
getwd()  
setwd("C:/Users/HP/Desktop")

# reading in the data
df <- read.csv("Telco Customer Churn.csv")

# dimensions of the data
dim(df)

# names of the data
colnames(df)

# taking a look at the data
str(df)
library(dplyr)
glimpse(df)

# changing character variables to factors
df <- df %>% mutate_if(is.character, as.factor)

# changing SeniorCitizen variable to factor
df$SeniorCitizen <- as.factor(df$SeniorCitizen)

# looking for missing values
colSums(is.na(df))
sum(is.na(df$TotalCharges))

# imputing with the median
df <- df %>% 
  mutate(TotalCharges = replace(TotalCharges,
                                is.na(TotalCharges),
                                median(TotalCharges, na.rm = T)))

# removing customerID; doesn't add any value to the model
df <- df %>% select(-customerID)

library(caTools)

# selecting random seed to reproduce results
set.seed(5)

split = sample.split(df$Churn, SplitRatio = 0.75)
train = subset(df, split==TRUE)
test = subset(df, split==FALSE)


# fitting the model
fit <- glm(Churn~., data=train, family=binomial)
# making predictions
churn.probs <- predict(fit, test, type="response")
head(churn.probs)

# Looking at the response encoding
contrasts(df$Churn)

# converting probabilities to "Yes" and "No" 
glm.pred = rep("No", length(churn.probs))
glm.pred[churn.probs > 0.5] = "Yes"
glm.pred <- as.factor(glm.pred)
cm=table(test$Churn,glm.pred)
library(caret)
confusionMatrix(cm)

library(ROCR)
# need to create prediction object from ROCR
pr <- prediction(churn.probs, test$Churn)

# plotting ROC curve
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# AUC value
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

