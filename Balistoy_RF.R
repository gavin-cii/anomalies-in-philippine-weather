#Load required libraries
library(randomForest)
library(dplyr)
library(lubridate)  #For date transformations
library(plotly)

#Load the dataset
data <- read.csv("C:\\Users\\MSI\\Downloads\\Documents\\HisotricalWeather.csv")  # Replace with your actual CSV file path

#Print the structure of the original dataset
cat("Original Dataset Structure:\n")
str(data)
cat("\n")

#Clean the data by removing rows with missing values
data_cleaned <- na.omit(data)

#Scale the datase
numeric_cols <- sapply(data, is.numeric)
data[numeric_cols] <- scale(data[numeric_cols])

#Train the Random Forest model with proximity measure enabled
rf_model <- randomForest(data_cleaned[, -ncol(data_cleaned)], proximity = TRUE)

#Calculate anomaly scores using proximity measure
anomaly_scores <- apply(rf_model$proximity, 1, mean)

#Set a threshold for anomaly detection (99th percentile)
threshold <- quantile(anomaly_scores, 0.99)  # Adjust the quantile value as needed

#Identify anomalies using the threshold
anomalies <- data_cleaned[anomaly_scores > threshold, ]

#Print the structure of the cleaned dataset
cat("Cleaned Dataset Structure:\n")
str(data_cleaned)
cat("\n")

#Print anomalies detected
cat("Anomalies Detected:\n")
print(anomalies)
cat("\n")

#Transform the `DATE` column to year format and filter data starting from the anomaly date
anomaly_start_year <- year(anomalies$DATE[1])
data_filtered <- filter(data_cleaned, year(DATE) >= anomaly_start_year)

#Create a 2D plot of the dataset with anomalies highlighted 
fig <- plot_ly(data_filtered, x = ~AVE..TEMP, y = ~AVE..HUM,
               mode = "markers", 
               marker = list(size = 3, color = 'cornflowerblue'),
               name = "Normal",
               type = 'scatter', # 
               text = ~paste("Date: ", DATE), 
               hoverinfo = 'text')

fig <- add_trace(fig, data = anomalies, x = ~AVE..TEMP, y = ~AVE..HUM,
                 mode = "markers", 
                 marker = list(size = 4, color = 'red'),
                 name = "Anomaly",
                 type = 'scatter', 
                 text = ~paste("Date: ", DATE), 
                 hoverinfo = 'text')

fig <- layout(fig, title = "2D Plot of Dataset with Scaled Anomalies",
              xaxis = list(title = "Average Temperature"),
              yaxis = list(title = "Average Humidity"))


fig

#Create a 3D plot of the dataset with anomalies highlighted
fig <- plot_ly(data_filtered, x = ~AVE..TEMP, y = ~AVE..HUM, z = ~DATE, color = I("cornflowerblue"),
               type = "scatter3d", mode = "markers", marker = list(size = 2),
               name = "Normal")
fig <- add_trace(fig, data = anomalies, x = ~AVE..TEMP, y = ~AVE..HUM, z = ~DATE,
                 color = I("red2"), marker = list(size = 3), name = "Anomaly")
fig <- layout(fig, title = "3D Plot of Dataset with Anomalies",
              scene = list(xaxis = list(title = "Average Temperature"),
                           yaxis = list(title = "Average Humidity"),
                           zaxis = list(title = "Date")))
fig