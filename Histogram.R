# Load the dataset
data <- read.csv("Pregnant_Women_Participating.csv")

# Ensure 'Average Participation' is numeric
data$Average.Participation <- as.numeric(data$Average.Participation)

# Extract the data for the histogram
y <- data$Average.Participation

# Create the histogram
h <- hist(y, breaks = 10, col = "lightblue", main = "Histogram with Normal Curve",
          xlab = "Average Participation", ylab = "Frequency")

# Calculate the mean and standard deviation
mn <- mean(y, na.rm = TRUE)
std_dev <- sd(y, na.rm = TRUE)

# Generate the normal curve data
x <- seq(min(y, na.rm = TRUE), max(y, na.rm = TRUE), length = 100)
y_norm <- dnorm(x, mean = mn, sd = std_dev)

# Scale the normal curve to match the histogram's scale
y_norm <- y_norm * diff(h$mids[1:2]) * length(y)

# Add the normal curve to the histogram
lines(x, y_norm, col = "blue", lwd = 2)
