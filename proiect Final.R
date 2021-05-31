library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readr,dplyr)
library(ggplot2)
library(caret)
library(rsample)
library(ISLR)
library(rpart)
library(rpart.plot)


Analysis <- read_csv("breast_cancer_data2.csv")
# TODO data cleanup

Analysis$diagnosis <- as.factor(Analysis$diagnosis) # convert diagnosis from chr to fct

Analysis %>%
  ggplot(aes(mean_radius, mean_area, color=diagnosis, shape=diagnosis)) + geom_point() # graph displaying correclation between radius and area
 
Analysis %>%
  ggplot(aes(mean_perimeter, mean_texture, color=diagnosis, shape=diagnosis)) + geom_point() + geom_smooth() # graph displaying correclation between perimeter and texture

Analysis %>%
  ggplot(aes(mean_texture, mean_smoothness, color=diagnosis, shape=diagnosis)) + geom_point() + geom_boxplot() # graph displaying correclation between radius and perimeter

ggplot(Analysis) + 
  geom_boxplot(aes(x = diagnosis, y = mean_area, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for area corelation with diagnostic

ggplot(Analysis) + 
  geom_boxplot(aes(x = diagnosis, y = mean_smoothness, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for smoothness corelation with diagnostisc

ggplot(Analysis) + 
  geom_boxplot(aes(x = diagnosis, y = mean_radius, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for radius corelation with diagnostisc

ggplot(Analysis) + 
  geom_boxplot(aes(x = diagnosis, y = mean_texture, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for texture corelation with diagnostisc

ggplot(Analysis) + 
  geom_boxplot(aes(x = diagnosis, y = mean_perimeter, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for perimeter corelation with diagnostisc

by_diagnosis <- group_by(Analysis, diagnosis)
summarize(by_diagnosis, count = n()) # count occurences in the dataset

# model de regresie logistica, diagnosis in functie de mean_perimeter
mod_perimeter <- glm(data = Analysis, diagnosis ~ mean_perimeter, family = binomial) # cu cat mean_parameter e mai mic cu atata sansa e sa fie Benign si cu cat creste sansa e sa fie Malign
summary(mod_perimeter)

grid <- Analysis %>%
  data_grid(mean_perimeter = seq_range(mean_perimeter, 100)) %>%   # facem o prezicere punand un range de 100 de valori al lui mean_parameter din dataSet
  add_predictions(mod_perimeter, "prob_default", type = "response")

ggplot() + 
  geom_line(data = grid, aes(mean_perimeter, prob_default), color = "red", size = 2) # graficul care ne arata predictul

nd <- tribble(~mean_perimeter, 40, 120)  # predicted pentru 2 valori
predicted <- predict(mod_perimeter, newdata = nd, type = "response")
predicted

#aici am inserat eu test pentru texture @kiki
mod_texture <- glm(data = Analysis, diagnosis ~ mean_texture, family = binomial) # cu cat mean_parameter e mai mic cu atata sansa e sa fie Benign si cu cat creste sansa e sa fie Malign
summary(mod_texture)

# model de regreie logistica, diagnosis in functie de mean_smoothness 
mod_smoothness <- glm(data = Analysis, diagnosis ~ mean_smoothness, family = binomial) # cu cat mean_parameter e mai mic cu atata sansa e sa fie Benign si cu cat creste sansa e sa fie Malign
summary(mod_smoothness)

#de aici am inceput eu @Kiki

grid <- Analysis %>%
  data_grid(mean_smoothness = seq_range(mean_smoothness, 100)) %>%
  add_predictions(mod_smoothness, "prob_diagnosis", type="response")

ggplot() +
  geom_line(data = grid, aes(mean_smoothness, prob_diagnosis), color = "red", size = 2) 

nd <- tribble(~mean_smoothness, 0.05, 0.150)
predicted <- predict(mod_smoothness, newdata = nd, type = "response") 

# model de regreie logistica, diagnosis in functie de mean_area
mod_area <- glm(data = Analysis, diagnosis ~ mean_area, family = binomial) #
summary(mod_area)

grid <- Analysis %>%
  data_grid(mean_area = seq_range(mean_area, 100)) %>%
  add_predictions(mod_area, "prob_diagnosis", type="response")

ggplot() +
  geom_line(data = grid, aes(mean_area, prob_diagnosis), color = "red", size = 2) 

nd <- tribble(~mean_area, 500, 1000)
predicted <- predict(mod_area, newdata = nd, type = "response") 

# model de regreie logistica, diagnosis in functie de diagnosis
mod_radius <- glm(data = Analysis, diagnosis ~ mean_radius, family = binomial)
summary(mod_radius)

grid <- Analysis %>%
  data_grid(mean_radius = seq_range(mean_radius, 100)) %>%
  add_predictions(mod_radius, "prob_diagnosis", type="response")

ggplot() +
  geom_line(data = grid, aes(mean_radius, prob_diagnosis), color = "red", size = 2) 

nd <- tribble(~mean_radius, 10, 20)
predicted <- predict(mod_radius, newdata = nd, type = "response")



mod_all <- glm(data = Analysis, diagnosis ~ mean_perimeter + mean_texture + mean_smoothness, family = binomial)
summary(mod_all) #aici am inserat eu modd all pentru doar perimeter, texture si smoothness @kiki


mod_all <- glm(data = Analysis, diagnosis ~ mean_perimeter + mean_smoothness + mean_area + mean_radius + mean_texture, family = binomial)
summary(mod_all) #mod all pt toate

#scoatem mean_area, ca are cel mai mic P si facem un model nou @kiki

mod_all <- glm(data = Analysis, diagnosis ~ mean_perimeter + mean_smoothness + mean_radius + mean_texture, family = binomial)
summary(mod_all)

mod_all <- glm(data = Analysis, diagnosis ~ mean_smoothness + mean_texture, family = binomial)
summary(mod_all)

#facem un box plot pt cele mai bune variabile @kiki
ggplot(Analysis) + 
  geom_boxplot(aes(x = mean_smoothness, y = mean_texture, fill = diagnosis)) + theme(text = element_text(size=20)) # boxplot graph for area corelation with diagnostic

# perform clasification
set.seed(123)
split <- initial_split(Analysis, prop = 0.7, strata = "diagnosis")
train <- training(split)
test <- testing(split)

mod_area_train <- glm(data = train, diagnosis ~ mean_perimeter + mean_area, family = binomial)
summary(mod_area_train) #aici trebuie schimbat in texture si smoothness, am schimbat mai jos

mod_area_train <- glm(data = train, diagnosis ~ mean_texture + mean_smoothness, family = binomial)
summary(mod_area_train) #aici am introdus eu@kiki

pred_test <- predict(mod_area_train, newdata = test, type = "response")
table(pred_test > 0.5, test$diagnosis) 

# performa clasification analysis with caret
set.seed(123)
split <- initial_split(Analysis, prop = 0.7, strata = "diagnosis")
train <- training(split)
test <- testing(split)
table(test$diagnosis)
table(train$diagnosis)

features <- setdiff(names(Analysis),  "diagnosis")
x <- train[, features]
y <- train$diagnosis

fitControl <- trainControl(
  method = "cv",
  number = 10
)
modGLM_all <- train(
  x=x,
  y=y,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)
modGLM_all
confusionMatrix(modGLM_all)
pred_all = predict(modGLM_all, newdata = test, type = "raw")
confusionMatrix(pred_all, test$diagnosis)

summary(modGLM_all) #aici am mai adaugat eu @kiki

xNoMean_area <- x %>% select(-mean_area)
modGLM_NoMean_area <- train(
  x=xNoMean_area,
  y=y,
  method = "glm",
  family = "binomial",
  trControl = fitControl
)
confusionMatrix(modGLM_NoMean_area)
summary(modGLM_NoMean_area)
pred_noMean_area <- predict(modGLM_NoMean_area, test)
confusionMatrix(pred_noMean_area, test$diagnosis)








