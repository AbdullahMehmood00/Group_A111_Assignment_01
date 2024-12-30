library(readr)

#Load data
data <- read_csv("Pregnant_Women_Participating.csv")

#show dataset
data

# convert average participant to number
data$Average.Participation <- as.numeric(data$"Average Participation")

#Now i want to remove NA values if have any in dataset
average_participation <- na.omit(data$Average.Participation)
dev.new()
#NOw creating histogram
hist(average_participation, breaks = 15, probability = TRUE, col = "blue", main = "Histogram of Average Participation with Bell Curve", xlab="Average Participaion (Number of Pregnant Women)", ylab = "Frequency", names.arg= row.names(mtcars ))

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


# Perform the Chi-squared test
chi_squared_test <- chisq.test(contingency_table)


# Print the results
print(chi_squared_test)

### For practice
x<- mtcars$wt
y<- mtcars$mpg
mtcars

plot(x,y, main = "Mian", xlab="WT", ylab = "MPG", pch = 19, frame=T)
plot(x, 
    y, 
      main = "1974 Motor Trend data: MPG vs Weight", # chart title
     xlab = "Weight (1000s of lbs)", # x-axis label
     ylab = "MPG", # y-axis label
     pch = 19, # point shape (filled circle)
     frame = T, # surround chart with a frame
     )



model<- lm(y~x, data = mtcars)

abline(model, col="red")

abline(model, col=red)
goals<-c(1,2,5,3,2)
summary(goals)
min(goals)
max(goals)
mtcars

mean(mtcars$hp)
sd(mtcars$hp)
max(mtcars$hp)
summary(mtcars$hp)
dev.new()

hist(mtcars$mpg, xlab = "x", ylab = "y", main = "main", breaks = 20)

x<-seq(min(mtcars$mpg), max(mtcars$mpg), length=100)
y<-dnorm(x, mean = mean(mtcars$mpg), sd=sd(mtcars$mpg) * length(mtcars$mpg))
Box.size <-diff(h$mids[1:2])
y<-y*Box.size
lines(x,y,col="red")

t.test(mtcars$mpg ~ mtcars$am)

boxplot(mtcars$mpg ~ mtcars$am, xlab = "X Side", ylab="Y Side", main="MAIN")

pt <- table(mtcars$cyl,mtcars$am)
pt
chisq.test(pt)
percentages <- prop.table(pt, margin=2) * 100


barplot(percentages, col = c("red", "blue", "yellow"), xlab = "Manual 0 vs Automatic 1", ylab =
          "Percentage", main = "Stacked Bar Chart of Cylinders by Transmission Type", ylim = c(0,
                                                                                               100), legend.text = c("4", "6", "8"), args.legend = list(x = "topright"))


#for asssignmening names
names(df[7]) <- "myCol"


dev.new()
boxplot(mtcars$mpg ~ mtcars$am, xlab="xlab", ylab="y lab", main="Main")
cor(mtcars$mpg, mtcars$am, method = "pearson")


