# Load necessary libraries
library(ggplot2)

# Load the dataset
data <- read.csv("/home/imran/Downloads/Pregnant_Women_Participating.csv")  # Replace with your actual file path

# Ensure 'Average Participation' is numeric
data$Average.Participation <- as.numeric(data$Average.Participation)

# Categorize states into "High Participation" and "Low Participation"
threshold <- mean(data$Average.Participation, na.rm = TRUE)
data$Participation.Category <- ifelse(data$Average.Participation > threshold, 
                                      "High Participation", 
                                      "Low Participation")

# Create scatter plot with a trend line
ggplot(data, aes(x = Participation.Category, y = Average.Participation, color = Participation.Category)) +
  geom_jitter(width = 0.2, height = 0) +  # Add jittered points
  geom_smooth(method = "lm", aes(group = 1), color = "black", se = FALSE) +  # Add trend line
  labs(
    title = "Scatter Plot of Participation Categories with Trend Line",
    x = "Participation Category",
    y = "Average Participation"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("High Participation" = "red", "Low Participation" = "blue"))
