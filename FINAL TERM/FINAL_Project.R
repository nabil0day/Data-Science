install.packages("e1071")
install.packages("caret")
install.packages("naivebayes")
library(e1071)
library(caret)
setwd("E:/R Language/Data-Science/FINAL TERM/")
data <- read.csv("Dataset.csv")
summary(data)
#################################################
data$Sex <- as.factor(data$Sex)
data$General_Health<-as.factor(data$General_Health)
data$Checkup<-as.factor(data$Checkup)
data$Exercise<-as.factor(data$Exercise)
data$Heart_Disease<-as.factor(data$Heart_Disease)
data$Skin_Cancer<-as.factor(data$Skin_Cancer)
data$Other_Cancer<-as.factor(data$Other_Cancer)
data$Depression<-as.factor(data$Depression)
data$Diabetes<-as.factor(data$Diabetes)
data$Arthritis<-as.factor(data$Arthritis)
data$Age_Category<-as.factor(data$Age_Category)
data$Smoking_History<-as.factor(data$Smoking_History)
levels(data$General_Health)
summary(data$General_Health)
########################################################

hist(data$Height_.cm.)
summary(data$Height_.cm.)
levels(data$Height_.cm.)




###############################################################

data$Height_.cm. <- as.numeric(as.character(data$Height_.cm.))
for (i in 1:length(data$Height_.cm.)) {
  if (is.na(data$Height_.cm.[i])) {
    data$Height_.cm.[i] <- "Missing"  
  } else if (data$Height_.cm.[i] < 141.0) {
    data$Height_.cm.[i] <- "Short"
  } else if (data$Height_.cm.[i] <= 191.0) {
    data$Height_.cm.[i] <- "Average"
  } else {
    data$Height_.cm.[i] <- "Tall"
  }
}


data$Height_.cm. <- factor(data$Height_.cm., levels = c("Missing", "Short", "Average", "Tall"))

summary(data$Height_.cm.)


###########################################################
summary(data$Weight_.kg.)
#hist(data$Weight_.kg.)
data$Weight_.kg. <- as.numeric(as.character(data$Weight_.kg.))

for (i in 1:length(data$Weight_.kg.)) {
  if (is.na(data$Weight_.kg.[i])) {
    data$Weight_.kg.[i] <- "Missing"
  } else if (data$Weight_.kg.[i] < 24.95) {
    data$Weight_.kg.[i] <- "Light Weight"
  } else if (data$Weight_.kg.[i] >= 24.95 & data$Weight_.kg.[i] < 81.65) {
    data$Weight_.kg.[i] <- "Medium Weight"
  } else if (data$Weight_.kg.[i] >= 81.65 & data$Weight_.kg.[i] < 95.25) {
    data$Weight_.kg.[i] <- "Medium Heavy Weight"
  } else if (data$Weight_.kg.[i] >= 95.25) {
    data$Weight_.kg.[i] <- "Heavy Weight"
  } else {
    data$Weight_.kg.[i] <- NA  
  }
}


data$Weight_.kg. <- factor(data$Weight_.kg., levels = c("Missing", "Light Weight", "Medium Weight", "Medium Heavy Weight", "Heavy Weight"))

summary(data$Weight_.kg.)
levels(data$Weight_.kg.)
##########################################################

summary(data$BMI)
data$BMI <- as.numeric(as.character(data$BMI))

for (i in 1:length(data$BMI)) {
  if (is.na(data$BMI[i])) {
    data$BMI[i] <- "Missing"
  } else if (data$BMI[i] < 18.5) {
    data$BMI[i] <- "Underweight"
  } else if (data$BMI[i] >= 18.5 & data$BMI[i] < 24.9) {
    data$BMI[i] <- "Normal Weight"
  } else if (data$BMI[i] >= 24.9 & data$BMI[i] < 29.9) {
    data$BMI[i] <- "Overweight"
  } else if (data$BMI[i] >= 29.9) {
    data$BMI[i] <- "Problem"
  } else {
    data$BMI[i] <- NA
  }
}


data$BMI <- factor(data$BMI, levels = c("Missing","Underweight", "Normal Weight", "Overweight", "Problem"))

summary(data$BMI)
levels(data$BMI)


############################################################
summary(data$Alcohol_Consumption)

data$Alcohol_Consumption <- as.numeric(as.character(data$Alcohol_Consumption))

for (i in 1:length(data$Alcohol_Consumption)) {
  if (is.na(data$Alcohol_Consumption[i])) {
    data$Alcohol_Consumption[i] <- "Missing"
  } else if (data$Alcohol_Consumption[i] == 0) {
    data$Alcohol_Consumption[i] <- "Non-Drinker"
  } else if (data$Alcohol_Consumption[i] > 0 & data$Alcohol_Consumption[i] <= 2) {
    data$Alcohol_Consumption[i] <- "Occasional Drinker"
  } else if (data$Alcohol_Consumption[i] > 2 & data$Alcohol_Consumption[i] <= 6) {
    data$Alcohol_Consumption[i] <- "Moderate Drinker"
  } else if (data$Alcohol_Consumption[i] > 6) {
    data$Alcohol_Consumption[i] <- "Heavy Drinker"
  } else {
    data$Alcohol_Consumption[i] <- NA
  }
}

data$Alcohol_Consumption <- factor(data$Alcohol_Consumption, levels = c("Missing","Non-Drinker", "Occasional Drinker", "Moderate Drinker", "Heavy Drinker"))


summary(data$Alcohol_Consumption)
levels(data$Alcohol_Consumption)

#######################################################################

summary(data$Fruit_Consumption)
data$Fruit_Consumption <- as.numeric(as.character(data$Fruit_Consumption))

for (i in 1:length(data$Fruit_Consumption)) {
  if (is.na(data$Fruit_Consumption[i])) {
    data$Fruit_Consumption[i] <- "Missing"
  } else if (data$Fruit_Consumption[i] == 0) {
    data$Fruit_Consumption[i] <- "Non-Consumer"
  } else if (data$Fruit_Consumption[i] > 0 & data$Fruit_Consumption[i] <= 15) {
    data$Fruit_Consumption[i] <- "Low Consumer"
  } else if (data$Fruit_Consumption[i] > 15 & data$Fruit_Consumption[i] <= 60) {
    data$Fruit_Consumption[i] <- "Moderate Consumer"
  } else if (data$Fruit_Consumption[i] > 60) {
    data$Fruit_Consumption[i] <- "High Consumer"
  } else {
    data$Fruit_Consumption[i] <- NA
  }
}


data$Fruit_Consumption <- factor(data$Fruit_Consumption, levels = c("Missing","Non-Consumer", "Low Consumer", "Moderate Consumer", "High Consumer"))

summary(data$Fruit_Consumption)
levels(data$Fruit_Consumption)


############################################################
summary(data$Green_Vegetables_Consumption)
data$Green_Vegetables_Consumption <- as.numeric(as.character(data$Green_Vegetables_Consumption))

for (i in 1:length(data$Green_Vegetables_Consumption)) {
  if (is.na(data$Green_Vegetables_Consumption[i])) {
    data$Green_Vegetables_Consumption[i] <- "Missing"
  } else if (data$Green_Vegetables_Consumption[i] == 0) {
    data$Green_Vegetables_Consumption[i] <- "Non-Consumer"
  } else if (data$Green_Vegetables_Consumption[i] > 0 & data$Green_Vegetables_Consumption[i] <= 5) {
    data$Green_Vegetables_Consumption[i] <- "Low Consumer"
  } else if (data$Green_Vegetables_Consumption[i] > 5 & data$Green_Vegetables_Consumption[i] <= 15) {
    data$Green_Vegetables_Consumption[i] <- "Moderate Consumer"
  } else if (data$Green_Vegetables_Consumption[i] > 15) {
    data$Green_Vegetables_Consumption[i] <- "High Consumer"
  } else {
    data$Green_Vegetables_Consumption[i] <- NA
  }
}

data$Green_Vegetables_Consumption <- factor(data$Green_Vegetables_Consumption, levels = c("Missing","Non-Consumer", "Low Consumer", "Moderate Consumer", "High Consumer"))

summary(data$Green_Vegetables_Consumption)
levels(data$Green_Vegetables_Consumption)


############################################################

summary(data$FriedPotato_Consumption)

data$FriedPotato_Consumption <- as.numeric(as.character(data$FriedPotato_Consumption))

for (i in 1:length(data$FriedPotato_Consumption)) {
  if (is.na(data$FriedPotato_Consumption[i])) {
    data$FriedPotato_Consumption[i] <- "Missing"
  } else if (data$FriedPotato_Consumption[i] == 0) {
    data$FriedPotato_Consumption[i] <- "Non-Consumer"
  } else if (data$FriedPotato_Consumption[i] > 0 & data$FriedPotato_Consumption[i] <= 3) {
    data$FriedPotato_Consumption[i] <- "Low Consumer"
  } else if (data$FriedPotato_Consumption[i] > 3 & data$FriedPotato_Consumption[i] <= 8) {
    data$FriedPotato_Consumption[i] <- "Moderate Consumer"
  } else if (data$FriedPotato_Consumption[i] > 8) {
    data$FriedPotato_Consumption[i] <- "High Consumer"
  } else {
    data$FriedPotato_Consumption[i] <- NA
  }
}

data$FriedPotato_Consumption <- factor(data$FriedPotato_Consumption, levels = c("Missing","Non-Consumer", "Low Consumer", "Moderate Consumer", "High Consumer"))

summary(data$FriedPotato_Consumption)
levels(data$FriedPotato_Consumption)

#############################################################

table(data$General_Health, data$Age_Category)





#############################################################

chisq_result_checkup <- chisq.test(table(data$General_Health, data$Checkup))
chisq_result_exercise <- chisq.test(table(data$General_Health, data$Exercise))
chisq_result_heart_disease <- chisq.test(table(data$General_Health, data$Heart_Disease))
chisq_result_skin_cancer <- chisq.test(table(data$General_Health, data$Skin_Cancer))
chisq_result_other_cancer <- chisq.test(table(data$General_Health, data$Other_Cancer))
chisq_result_depression <- chisq.test(table(data$General_Health, data$Depression))
chisq_result_diabetes <- chisq.test(table(data$General_Health, data$Diabetes))
chisq_result_arthritis <- chisq.test(table(data$General_Health, data$Arthritis))
chisq_result_sex <- chisq.test(table(data$General_Health, data$Sex))
chisq_result_age_category <- chisq.test(table(data$General_Health, data$Age_Category))
chisq_result_height_cm <- chisq.test(table(data$General_Health, data$Height_.cm.))
chisq_result_weight_kg <- chisq.test(table(data$General_Health, data$Weight_.kg.))
chisq_result_bmi <- chisq.test(table(data$General_Health, data$BMI))
chisq_result_smoking_history <- chisq.test(table(data$General_Health, data$Smoking_History))
chisq_result_alcohol_consumption <- chisq.test(table(data$General_Health, data$Alcohol_Consumption))
chisq_result_fruit_consumption <- chisq.test(table(data$General_Health, data$Fruit_Consumption))
chisq_result_green_vegetables_consumption <- chisq.test(table(data$General_Health, data$Green_Vegetables_Consumption))
chisq_result_friedpotato_consumption <- chisq.test(table(data$General_Health, data$FriedPotato_Consumption))

#########################################



print(chisq_result_checkup)
print(chisq_result_exercise)
print(chisq_result_heart_disease)
print(chisq_result_skin_cancer)
print(chisq_result_other_cancer)
print(chisq_result_depression)
print(chisq_result_diabetes)
print(chisq_result_arthritis)
print(chisq_result_sex)
print(chisq_result_age_category)
print(chisq_result_height_cm)
print(chisq_result_weight_kg)
print(chisq_result_bmi)
print(chisq_result_smoking_history)
print(chisq_result_alcohol_consumption)
print(chisq_result_fruit_consumption)a
print(chisq_result_green_vegetables_consumption)
print(chisq_result_friedpotato_consumption)



#############################################################

data <- subset(data, select = -Height_.cm.)
summary(data)
##############################################################

set.seed(123)
trainIndex <- createDataPartition(data$General_Health, p = 0.8, list = FALSE)
data_train <- data[trainIndex, ]
data_test <- data[-trainIndex, ]

naivebayes_model <- naiveBayes(General_Health ~ ., data = data_train)

predictions_test <- predict(naivebayes_model, newdata = data_test)

accuracy_test <- mean(predictions_test == data_test$General_Health)
cat("Accuracy on Test Set:", accuracy_test, "\n")

##############################################################

set.seed(123)
control <- trainControl(method = "cv", number = 10)
nb_model_cv <- train(General_Health ~ ., data = data, method = "naive_bayes", trControl = control)
accuracy_cv <- mean(nb_model_cv$results$Accuracy)
cat("Accuracy from 10-fold Cross-Validation:", accuracy_cv, "\n")

##############################################################

conf_matrix <- confusionMatrix(predictions_test, data_test$General_Health)
print(conf_matrix)

TP <- conf_matrix$table[2, 2]  
FP <- conf_matrix$table[1, 2]  
FN <- conf_matrix$table[2, 1]  

recall <- TP / (TP + FN)

precision <- TP / (TP + FP)

f_measure <- 2 * (precision * recall) / (precision + recall)

cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F-measure:", f_measure, "\n")


##############################################################
classes <- sapply(data, class)
print(classes)

#class(data$Exercise)



###########################################################

summary(data$Age_Category)
class(data$Age_Category)
levels(data$Age_Category)

levels(data$General_Health)
levels(data$Diabetes)

#############################################################


