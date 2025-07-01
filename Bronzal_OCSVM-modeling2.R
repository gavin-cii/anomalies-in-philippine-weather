pkgs <- sort(c('readr', 'ggplot2', 'dplyr', 'rgl', 'tidyverse', 'reshape2', 'sqldf'))
pkgs_install <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(pkgs_install)){
  install.packages(pkgs_install)
}

library(readr)
library(ggplot2)
library(dplyr)
library(rgl)
library(tidyverse)
library(reshape2)
library(sqldf)

require(e1071) # for svm()                                                                                                                                                          
require(rgl) # for 3d graphics
require(reshape2)
df2 <- read_csv("preprocessed_2013-2023.csv")
df1 <- df2[1:1000,]
t <- df1 %>%
  select(`DATE`, `AVE. TEMP`, `AVE. HUM`)

t[1:1000,]

x <- subset(t, select = 2) #AVE. TEMP as x variable
y <- t$`AVE. TEMP` #AVE. HUM as y variable (dependent)

svm_model <- svm(df2$`AVE. TEMP`, data = df2[1:1000,], type='one-classification', kernel='radial',scale=FALSE)

print(svm_model)
summary(svm_model)

pred <- predict(svm_model) #model classifying the entire dataset

result <- df2
result$anomaly <- pred
result

anomalies <- subset(result, result$anomaly=="TRUE")
anomalies

write.csv(anomalies, file = "OCSVM_anomalies.csv", row.names = TRUE)

ocsvm_plot <- ggplot(result, aes(`AVE. HUM`, `AVE. TEMP`, color = anomaly)) +
  geom_point() +
  scale_color_manual(values = c("skyblue", "lightcoral")) +
  geom_text(aes(`AVE. HUM`, `AVE. TEMP`, label = ifelse(anomaly == "TRUE", "", "")),  # Add labels only for outliers
            vjust = 1,  # Adjust vertical position of text (optional)
            hjust = 1,  # Adjust horizontal position of text (optional)
            inherit.aes = FALSE) +  # Inherit aesthetics except color
  theme_minimal() +
  labs(title = "OCSVM Weather Anomaly Detection")

ocsvm_plot