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
  geom_point(aes(color = "Normal")) +
  geom_point(data = result$anomalies, aes_string(x = column1, y = column2, color = "'Anomaly'"), size = 3, shape = 21, fill = "red") +
  scale_color_manual(values = c("Normal" = "blue", "Anomaly" = "red"), labels = c("Anomaly", "Normal")) +
  labs(title = "K-NN Anomaly Detection", x = column1, y = column2) +
  theme(legend.title = element_blank())

print(p)

print("Detected Anomalies:")
print(result$anomalies)

#Anomaly Score
print("Anomaly Scores:")
print(result$anomaly_scores)

#Visual Inspection using Histogram
histogram <- ggplot() +
  geom_histogram(aes(x = result$anomaly_scores), bins = 20, fill = "lightblue", color = "black") +
  labs(title = "Histogram of Anomaly Scores", x = "Anomaly Score", y = "Frequency") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.5)) +
  theme_minimal()


library(gridExtra)
grid.arrange(p, histogram, nrow = 2)

#Visual Inspection using Density Plot
density_plot <- ggplot() +
  geom_density(aes(x = result$anomaly_scores), fill = "lightblue", color = "black") +
  labs(title = "Density Plot of Anomaly Scores", x = "Anomaly Score", y = "Density") +
  theme_minimal()

grid.arrange(p, histogram, density_plot, nrow = 3)

#Statistical Significance
mean_anomaly <- mean(result$anomaly_scores)
sd_anomaly <- sd(result$anomaly_scores)

z_scores <- (result$anomaly_scores - mean_anomaly) / sd_anomaly

#by using standard normal distribution, P value is calculated
p_values <- pnorm(-abs(z_scores)) * 2 

anomaly_data <- data.frame(Anomaly_Score = result$anomaly_scores, Z_Score = z_scores, P_Value = p_values)

cat("Summary Statistics:\n")
cat(paste("Mean Anomaly Score:", mean_anomaly, "\n"))
cat(paste("Standard Deviation of Anomaly Scores:", sd_anomaly, "\n"))

cat("\nAnomalies with Z-Scores and P-Values:\n")
print(anomaly_data)

p_value_plot <- ggplot(anomaly_data, aes(x = Anomaly_Score, y = P_Value)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Add a dashed line at p = 0.05 for significance level
  labs(title = "P-Values of Anomaly Scores", x = "Anomaly Score", y = "P-Value") +
  theme_minimal()

grid.arrange(p, histogram, density_plot, p_value_plot, nrow = 4)


p_value_z_score_plot <- ggplot(anomaly_data, aes(x = Z_Score, y = P_Value)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Add a dashed line at p = 0.05 for significance level
  labs(title = "P-Values vs Z-Scores", x = "Z-Score", y = "P-Value") +
  theme_minimal()

print(p_value_z_score_plot)
#Exporting Anomaly CSV
write.csv(result$anomalies, file = "knn_detected_anomalies.csv", row.names = FALSE)
print("Detected anomalies exported as csv.")
