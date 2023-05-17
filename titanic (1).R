
# Load required libraries
install.packages("titanic")
install.packages("caTools")
library(titanic)
library(e1071)
library(caTools)
library(klaR)


# Load the Titanic dataset
data("titanic_train")
titanic_train= as.data.frame(titanic_train)

# Prepare the dataset by selecting only the relevant columns
titanic_train = titanic_train[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare",'Embarked')]

# Check for missing values
sum(is.na(titanic_train))
sum(is.na(titanic_train$Age))
mean=round(mean(titanic_train$Age,na.rm = T),0)
titanic_train$Age=replace(titanic_train$Age,is.na(titanic_train$Age),mean)
# Convert the "Survived" and "Sex" columns to factors
titanic_train$Survived = as.factor(titanic_train$Survived)
titanic_train$Sex = as.factor(titanic_train$Sex)

# Split the dataset into training and testing sets
set.seed(123)
split = sample.split(titanic_train$Survived, SplitRatio = 0.77)
train = subset(titanic_train, split == TRUE)
test = subset(titanic_train, split == FALSE)
# Train the Naive Bayes model
model = naiveBayes(Survived ~ ., data = train)

# Make predictions on the testing set
predictions = predict(model, newdata = test)

# Calculate the accuracy of the model
accuracy = sum(predictions == test$Survived)/nrow(test)
accuracy*100




# Create a new data frame with a sample passenger's information
new_data = data.frame(Pclass = 1,
                       Sex = "female",
                       Age = 35,
                       SibSp = 1,
                       Parch = 2,
                       Fare = 120,Embarked='S')

# Convert the "Sex" column to a factor
new_data$Sex = factor(new_data$Sex)

# Make a prediction on the new data
prediction = predict(model, newdata = new_data)

# Print the prediction
prediction

