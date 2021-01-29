install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(caret)
library(e1071)
# Data
train_sal <- read.csv(file.choose())
View(train_sal)
str(train_sal)
train_sal[sapply(train_sal, is.character)] <- lapply(train_sal[sapply(train_sal, is.character)],as.factor)
train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

test_sal <- read.csv(file.choose())
View(test_sal)
str(test_sal)
test_sal[sapply(test_sal, is.character)] <- lapply(test_sal[sapply(test_sal, is.character)],as.factor)
test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)

ggplot(data=train_sal,aes(x=train_sal$Salary, y = train_sal$age,fill = train_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")



ggplot(data=train_sal,aes(x = train_sal$age, fill = train_sal$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')


plot(train_sal$workclass,train_sal$Salary)

Model <- naive_bayes(Salary ~ ., data = train_sal)
Model <- naive_bayes(Salary ~ ., data = train_sal, usekernel= T)
Model

plot(Model)

Model_pred <- predict(Model,test_sal)
mean(Model_pred==test_sal$Salary)
head(cbind(Model_pred,test_sal))


# Confusion Matrix - test data
confusionMatrix(Model_pred, test_sal$Salary)

pairs.panels(train_sal[-1])
