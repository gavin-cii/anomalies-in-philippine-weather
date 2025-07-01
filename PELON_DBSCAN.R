# Load required libraries
library(dbscan)
library(ggplot2)
library(plotly)

# Load the dataset
df <- read.csv("D:\\Downloads\\preprocessed_2013-2023.csv")

### Preprocessing

# Select relevant columns
df2 <- df[, c("DATE", "AVE..HUM", "AVE..TEMP")]

# Convert DATE column to Date format
df2$DATE <- as.Date(df$DATE)

# Exclude DATE column for scaling
scaled_data <- scale(df2[, -1])  

### DBSCAN Clustering

# Find the optimal epsilon value
distances <- dist(scaled_data)  # Calculate distances
k_dist <- kNNdistplot(distances, k = 15)  # Plot k-distance graph

# Perform DBSCAN clustering
dbscan_result <- dbscan(scaled_data, eps = 0.3, minPts = 15)

# View anomalies
anomalies <- df[dbscan_result$cluster == 0, ]
print(anomalies)

# 2D Data Visualization
df2$cluster <- ifelse(dbscan_result$cluster == 0, "Anomaly", "Not Anomaly")

ggplot(df2, aes(x = AVE..HUM, y = AVE..TEMP, color = cluster)) +
  geom_point() +
  scale_color_manual(name = "Cluster", values = c("Anomaly" = "red", "Not Anomaly" = "blue")) +
  labs(title = "Weather Anomaly Detection using DBSCAN Clustering",
       x = "Average Humidity",
       y = "Average Temperature")


# 3D Data visualization
plot_ly(df2, x = ~AVE..HUM, y = ~AVE..TEMP, z = ~DATE, color = ~cluster, type = "scatter3d", mode = "markers",
        colors = c("Not Anomaly" = "blue", "Anomaly" = "red")) %>%
  layout(scene = list(xaxis = list(title = "Average Humidity"),
                      yaxis = list(title = "Average Temperature"),
                      zaxis = list(title = "Date")),
         title = "Weather Anomaly Detection using DBSCAN Clustering")



# Calculate and view silhouette score
silhouette_score <- silhouette(dbscan_result$cluster, dist(scaled_data))
print(silhouette_score)
    