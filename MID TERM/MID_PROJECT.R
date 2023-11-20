install.packages("readxl")
library(readxl)
setwd("E:/R Language/MID TERM")
data <- read_excel("DataSets.xlsx")

summary(data)

mode_gender <- as.character(names(which.max(table(data$gender))))
data$gender[is.na(data$gender)] <- mode_gender                          

missing_gender <- sum(is.na(data$gender))

if (missing_gender > 0) {
  print("There are", missing_gender, "missing values in the 'Gender' column")
} else {
  print("There are no missing values in the 'Gender' column")
}

mean_age <- mean(data$age, na.rm = TRUE)
data$age[is.na(data$age)] <- mean_age

median_pressure_high <- median(data$pressurehight, na.rm = TRUE)
data$pressurehight[is.na(data$pressurehight)] <- median_pressure_high

z_scores <- (data$impluse - mean(data$impluse)) / sd(data$impluse)
outliers <- abs(z_scores) > 3
outlier_values <- data$impluse[outliers]
print(outlier_values)


barplot(table(data$gender), main = "Gender", xlab = "Gender", ylab = "Count", col = "lightblue")

hist(data$pressurehight, main = "PressureHigh Histogram", xlab = "PressureHigh", ylab = "UpperBound", col = "lightblue")

boxplot(data$impluse, main = "Impulse Box Plot", ylab = "Impulse", col = "lightblue")




