library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readr,dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)
library(rsample)

Analysis <- read_csv("/Users/bogdanpastiu/Desktop/bigdata/dataset/breast_cancer_data2.csv")

# luam doar atribute numerice, pt fiecare din ele il consideram ca si metrica si luam valoarea atributului respectiv
# valorile pe axa x si facem cate un grafic pentru fiecare aribut
# metric va fi fiecare atribut (mean_area, mean_perimeter, mean_radius, mean_smoothness, mean_texture)
Analysis %>%
  select_if(is.numeric) %>%
  gather(metric, value) %>%
  ggplot(aes(value, fill=metric)) +
  geom_density(show.legend = TRUE) + 
  facet_wrap(~metric, scales = "free")

# convertim atributul diagnosis din character in factor
Analysis$diagnosis <- as.factor(Analysis$diagnosis) # convert diagnosis from chr to fct

# iau pacientii cu diagnosis M, selectez atributele numerice si facem corelatia lor si vom plott-ui aceasta corelatie cu corrplot
Analysis %>%
  filter(diagnosis == "M") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

# impartirea setului de date in set de antrenare si set de test
set.seed(123)
split <- initial_split(Analysis, prop = 0.7, strata = "diagnosis")
train <- training(split)
test <- testing(split)

# sa vedem impartirea facuta cu stratificarea diagnosis
table(train$diagnosis)
table(test$diagnosis)

# setul de features, toate atributele fara diagnosis y e diagnosis x sunt toate restu
# setul x-ilor si setul lui y
feature <- setdiff(names(train), "diagnosis")
x <- train[,features] 
y <- train$diagnosis

# metoda de validare: 10-folds Cross Validation
fitControl <- trainControl(
  method = "cv",
  number = 10
)

# invatarea modelului Naive Bayes
modNbSimpleCV <- train(
  x = x, 
  y = y,
  method = "nb",
  trControl = fitControl # 10 modele si pe cele 10 modele va da acuratetea
)

# matricea de confuzie
# in cadrul matricei de confuzie trebuie sa ne uitam pe diagonala, in cazul nostru aveam 60,7 B, true pozitive si 30,5 M true negative
modNbSimpleCV
confusionMatrix(modNbSimpleCV)

# construim un searchGrid, cautare si cu kernel true sau false. va returna 12 modele
searchGrid <- expand.grid(
  usekernel = c(TRUE,FALSE),
  fL = 0.5,
  adjust = seq(0, 5 , by = 1)
)

# invatarea modelului Naive Bayes
modNbCVSearch <- train(
  x = x, 
  y = y,
  method = "nb",
  trControl = fitControl, # 10 modele si pe cele 10 modele va da acuratetea
  tuneGrid = searchGrid # 120 de modele in total. 10 de la fitControl si inca 12 de la searchGrid si se inmultesc
)
modNbCVSearch
# la noi va fi usekernel true si cu 1 la ajustare, modelul 8 din tabel daca rulam bucata de mai sus
# output din consola: The final values used for the model were fL = 0.5, usekernel = TRUE and adjust = 1.

confusionMatrix(modNbCVSearch) # acuratete 0.9118

# listam primele 5 cele mai bune modele
modNbCVSearch$results %>%
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy)) # ruland asta, primul model e cel castigator cu usekernel true si adjust 1 cu acuratete de 0.91237

# predictie, facem predictia pe datele de test nu pe datele de train folosite anterior si ia modelul 1 cu acurateea cea mai buna
pred <- predict(modNbCVSearch, test)
predProb <- predict(modNbCVSearch, test, type = "prob")
confusionMatrix(pred, test$diagnosis) # acuratete de 0.9128

# curba ROC(k)
library(pROC)
dataset <- data.frame(
  actuall.class <- test$diagnosis,
  probability <- predProb[,1]
)
roc.val <- roc(actual.class ~ probability, dataset)
adf <- data.frame(
  specificity <- roc.val$specificities,
  sensitivity <- roc.val$sensitivities
)
ggplot(adf, aes(specificity, sensitivity)) + 
  geom_line(color = "blue") +
  scale_x_reverse() + 
  theme(text = element_text(size = 20))