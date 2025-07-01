setwd("C:/Users/mj./Desktop/codes/R/KNN")
#install
install.packages("kknn")
install.packages("FNN")

library(kknn)
library(ggplot2)
library(FNN)

knn_anomaly_detection <- function(data, column1, column2, k = 5, threshold = 2) {
  tryCatch({
    model <- kknn::train.kknn(as.formula(paste(column2, "~", column1)), data, ks = k)
  }, error = function(e) {
    print("Error occurred during model training:")
    print(e)
    return(list(anomalies = NULL, anomaly_scores = NULL))
  })
  
  if (is.null(model)) {
    print("Error: Model could not be trained.")
    return(list(anomalies = NULL, anomaly_scores = NULL))
  }
  
  print(model)
  
  distances <- FNN::get.knnx(data[, c(column1, column2)], data[, c(column1, column2)], k = k)$nn.dist
  
  print("Distances:")
  print(head(distances))
  
  anomaly_score <- apply(distances, 1, mean, na.rm = TRUE)
  
  
  anomalies <- data[anomaly_score > threshold, ]

  return(list(anomalies = anomalies, anomaly_scores = anomaly_score))
}


data <- read.csv("data/preprocessed_2013-2023.csv")

print("Column names in the dataset:")
print(colnames(data))
column1 <- "AVE.TEMP"
column2 <- "AVE.HUM"

result <- knn_anomaly_detection(data, column1, column2, k = 10, threshold = 1)

# Graphs Anomalies
p <- ggplot(data, aes_string(x = column1, y = column2)) +
  geom_point(color = "blue") +
  geom_point(data = result$anomalies, aes_string(x = column1, y = column2), color = "red", size = 3, shape = 21, fill = "red") +
  labs(title = "K-NN Anomaly Detection", x = column1, y = column2)

print(p)

print("Detected Anomalies:")
print(result$anomalies)
print("Anomaly Scores:")
print(result$anomaly_scores)