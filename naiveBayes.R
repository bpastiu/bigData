# Naive Bayes
library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readr,dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(rsample)
library(ISLR)
library(pROC)

Analysis <- read_csv("/Users/bogdanpastiu/Desktop/bigdata/dataset/breast_cancer_data2.csv")

set.seed(123)
split <- initial_split(Analysis, prop = 0.7, strata = "diagnosis")
train <- training(split)
test <- testing(split)
table(test$diagnosis)
table(train$diagnosis)

features <- setdiff(names(train), "diagnosis") 

x <- train[,features]  
y <- train$diagnosis

fitControl <- trainControl(
  method = "cv",
  number= 10
)

mod_nb <- train(
  x = x,   
  y = y,
  method = "nb",
  trControl = fitControl
)

mod_nb
confusionMatrix(mod_nb)

searchGrid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = c(0.5,1.0),
  adjust = c(0,1,2,3)
)

mod_nb_search <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = fitControl,
  tuneGrid = searchGrid
)

mod_nb_search
confusionMatrix(mod_nb_search)

pred <- predict(mod_nb_search, test)
predProb <- predict(mod_nb_search, test, type = "prob")
confusionMatrix(pred, test$diagnosis)

#furnizam 2 coloane pt curba ROC
dataset <- data.frame(
  actual.class <- test$diagnosis,   #cu valorile reale
  probability <- predProb[,1]      #cu probabilitati
)

#obtinerea valorilor ROC
roc.val <- roc(actual.class ~ probability, dataset)

# creem un data frame nou pentru a putea face grafic cu ggplot
adf <- data.frame(  
  specificity <- roc.val$specificities, 
  sensitivity <- roc.val$sensitivities)

ggplot(adf, aes(specificity, sensitivity))+ 
  geom_line(color = 'blue') +
  scale_x_reverse() + 
  theme(text = element_text(size=20))


searchOne <- expand.grid(
  usekernel = TRUE,
  fL = 0.5,
  adjust = 5
)


fitControlNone <- trainControl(
  method = "none"
)

ggplot(mod_nb_search)
ggplot(mod_nb_search, metric = "Accuracy")
