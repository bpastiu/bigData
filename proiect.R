library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readr,dplyr)
library(ggplot2)
library(ISLR)
library(caret)
library(rsample)

Analysis <- read_csv("/Users/bogdanpastiu/Desktop/bigdata/dataset/breast_cancer_data.csv")
# TODO data cleanup

Analysis %>%
  ggplot(aes(mean_radius, mean_area, color=diagnosis)) + geom_point() # graph displaying correclation between radius and area
 
Analysis %>%
  ggplot(aes(mean_perimeter, mean_texture, color=diagnosis)) + geom_point() + geom_smooth() # graph displaying correclation between perimeter and texture

Analysis %>%
  ggplot(aes(mean_radius, mean_perimeter, color=diagnosis)) + geom_point() + geom_smooth() # graph displaying correclation between radius and perimeter

split=sample.split(Analysis$diagnosis,SplitRatio=0.65)
Analysis<-Analysis[-33]
training_set<-subset(Analysis,split==T)
View(training_set)
test_set<-subset(Analysis,split==F)
View(test_set)

bc_reg<-glm(data = Analysis, diagnosis ~ mean_radius + mean_area + mean_perimeter + mean_smoothness, family = binomial)
summary(bc_reg)



