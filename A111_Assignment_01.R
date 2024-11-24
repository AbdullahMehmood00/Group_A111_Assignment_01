library(readr)


#Load data
data <- read_csv("Pregnant_Women_Participating.csv")

#show dataset
data

# convert average participant to number
data$Average.Participation <- as.numeric(data$"Average Participation")

#Now i want to remove NA values if have any in dataset
average_participation <- na.omit(data$Average.Participation)

#NOw creating histogram
hist(average_participation, breaks = 15, probability = TRUE, col = "blue", main = "Histogram of Average Participation with Bell Curve", xlab="Average Participaion (Number of Pregnant Women)", ylab = "Frequency")

# density curve
lines(density(average_participation), col="red", lwd=2)

#Now we are working to find top 10 performers
#get top 10 performers 1 to 10 -> [1:10], if we need top 15 than use [1:15], but right now i need only 10 top performers
top_performers <- data[order(-data$Average.Participation), ][1:10, ]
#print top performers
top_performers


#Barplot show the top 10 states
barplot(height = top_performers$Average.Participation, names.arg = top_performers$"State Agency or Indian Tribal Organization",
        horiz = TRUE,
        las = 1,
        col = "skyblue",
        main = "Top 10 States/Organization by Average Participantion",
        xlab = "Average Participation (Number of preganat Women)",
        #ylab = top_performers$"State Agency or Indian Tribal Organization",
        cex.names = 0.8
        )

# Create a levels: Low, Medium, High
bins <- c(0, 5000, 20000, Inf)
#labels
labels <- c("Low", "Medium", "High")
data$Participation.Level <- cut(data$Average.Participation, breaks = bins, labels = labels, right = FALSE)

# Now create contingency table for State vs Participation Levels
contingency_table <- table(data$"State Agency or Indian Tribal Organization", data$Participation.Level)

# Create a levels: Low, Medium, High
bins <- c(0, 5000, 20000, Inf)

#labels
labels <- c("Low", "Medium", "High")
data$Participation.Level <- cut(data$Average.Participation, breaks = bins, labels = labels, right = FALSE)

# Now create contingency table for State vs Participation Levels
contingency_table <- table(data$"State Agency or Indian Tribal Organization", data$Participation.Level)

