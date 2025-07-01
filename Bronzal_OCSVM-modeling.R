#Install packages
pkgs <- sort(c('readr', 'ggplot2', 'dplyr'))
pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)){
  install.packages(pkgs_install)
}
library(readr)
library(ggplot2)
library(dplyr)
library(e1071)

#Read dataset
df <- read_csv("preprocessed_2013-2023.csv")

#truncate dataset for training
df1 <- df[1:1000,]
df1

#filter dataset to the 2 selected attributes
df2 <- df1 %>%
  select(`DATE`, `AVE. TEMP`, `AVE. HUM`)
df2

x <- subset(df2, select = 2) #AVE. TEMP as x variable
y <- df2$`AVE. TEMP` #AVE. HUM as y variable (dependent)

model <- svm(x, y, type='one-classification', kernel = 'linear') #training of model

print(model)
summary(model)

pred <- predict(model, subset(df, select = 4)) #model classifying the entire dataset

pred