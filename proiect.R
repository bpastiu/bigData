library(tidyverse)
library(modelr)
library(scatterplot3d)
library(readr,dplyr)
library(ggplot2)

Analysis <- read_csv("/Users/bogdanpastiu/Desktop/bigdata/dataset/breast_cancer_data.csv")
# TODO data cleanup

Analysis %>%
  ggplot(aes(mean_radius, mean_area, color=diagnosis)) + geom_point() # graph displaying correclation between radius and area
 
Analysis %>%
  ggplot(aes(mean_perimeter, mean_texture, color=diagnosis)) + geom_point() + geom_smooth() # graph displaying correclation between perimeter and texture

Analysis %>%
  ggplot(aes(mean_radius, mean_perimeter, color=diagnosis)) + geom_point() + geom_smooth() # graph displaying correclation between radius and perimeter



