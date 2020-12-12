##### Classification using Naive Bayes --------------------
# Q : 1) Prepare a classification model using Naive Bayes for salary data 
# Data Description:
#age -- age of a person
#workclass	-- A work class is a grouping of work 
#education	-- Education of an individuals	
#maritalstatus -- Marital status of an individulas	
#occupation	 -- occupation of an individuals
#relationship -- 	
#race --  Race of an Individual
#sex --  Gender of an Individual
#capitalgain --  profit received from the sale of an investment	
#capitalloss	-- A decrease in the value of a capital asset
#hoursperweek -- number of hours work per week	
#native -- Native of an individual
#Salary -- salary of an individual

# Solution
# Loading required packages
install.packages("naivebayes")
library(naivebayes)
library(ggplot2)
library(psych)

# read the salary data 
SalaryData_Train <- read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\SVM-R\\SalaryData_Train.csv")
View(SalaryData_Train)

# converting the salary as class variable with two values - High and Low
sal <- ifelse(SalaryData_Train$Salary==" >50K","High","Low")
View(sal)
sal<-factor(sal)
salary_train <- data.frame(SalaryData_Train,sal)
View(salary_train)
salary_train <- salary_train[,-14]
View(salary_train)
#Visualization 
# Plot and ggplot 
ggplot(data=salary_train,aes(x=salary_train$sal, y = salary_train$age, fill = salary_train$sal)) +
  geom_boxplot() + ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$age, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# The salary of higher age people is higher than that of lower age
salary_train$workclass<-as.factor(salary_train$workclass)
plot(salary_train$workclass,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$workclass, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Self employed and govt.people earn more than any other workclass people
# Many people are working in private industries although the ratio between
# low and high is high
salary_train$education<-as.factor(salary_train$education)
plot(salary_train$education,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$education, fill =salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Bachelors,Professors, Masters and Doctorate people although less in number, but their salary is high
# HS-Gradepeople are high in numbers, but their salary range is low
salary_train$educationno<-as.factor(salary_train$educationno)
plot(salary_train$educationno,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$educationno, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# The higher degree in education earns more salary
salary_train$maritalstatus<-as.factor(salary_train$maritalstatus)
plot(salary_train$maritalstatus,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$maritalstatus, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Married-civ-spouse salary is comparatively high in proportion
salary_train$occupation<-as.factor(salary_train$occupation)
plot(salary_train$occupation,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$occupation, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Executive-Managerial category come under high salary category
salary_train$relationship<-as.factor(salary_train$relationship)
plot(salary_train$relationship,salary_train$sal)
ggplot(data= salary_train,aes(x = salary_train$relationship, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Husband by elation earns high salary
salary_train$race<-as.factor(salary_train$race)
plot(salary_train$race,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$race, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# White people in Us come under high category in earning high salary
salary_train$sex<-as.factor(salary_train$sex)
plot(salary_train$sex,salary_train$sal)
ggplot(data=salary_train,aes(x = salary_train$sex, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# Male people comparatively get high salary
salary_train$native<-as.factor(salary_train$native)
plot(salary_train$native,salary_train$sal)
ggplot(data= salary_train,aes(x = salary_train$native, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# United States people earn comparatively high salary
ggplot(data=salary_train,aes(x=salary_train$sal, y = salary_train$capitalgain, fill = salary_train$sal)) +
geom_boxplot() +
  ggtitle("Box Plot") 
ggplot(data=salary_train,aes(x = salary_train$capitalgain, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# There is a high variation in capital gain among low salary category people
ggplot(data=salary_train,aes(x=salary_train$sal, y = salary_train$capitalloss, fill = salary_train$sal)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$capitalloss, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# There is also high variation in capital loss as well more in low salary category
ggplot(data=salary_train,aes(x=salary_train$sal, y = salary_train$hoursperweek, fill = salary_train$sal)) +
  geom_boxplot() +
  ggtitle("Box Plot")
ggplot(data=salary_train,aes(x = salary_train$hoursperweek, fill = salary_train$sal)) +
  geom_density(alpha = 0.9, color = 'Violet')
# High salary earned people work for more time 

# read the test data
SalaryData_Test <- read.csv("C:\\Users\\IN102385\\OneDrive - Super-Max Personal Care Pvt. Ltd\\Assignment - Data Science -UL\\SVM-R\\SalaryData_Test.csv")
View(SalaryData_Test)
sal1 <- ifelse(SalaryData_Test$Salary==" >50K", "High","Low")
View(sal1)
sal1<-factor(sal1)
salary_test <- data.frame(SalaryData_Test,sal1)
salary_test <- salary_test[,-14]
View(salary_test)

# build the model1 with all fields
library(e1071)
library(caret)
sal_nb <- naiveBayes(salary_train$sal ~., data = salary_train)
sal_nb
predsal <- predict(sal_nb, salary_test)
confusionMatrix(predsal,salary_test$sal1)
# The model accuracy is 0.81
# Preparing another naive bayes model by using "cv" method
x= salary_train[,-14]
y = salary_train$sal
sal_nb1<-train(x,y,'nb',trControl=trainControl(method='cv',number=10))
predsal1 <- predict(sal_nb1, salary_test)
confusionMatrix(table(predsal1,salary_test$sal1))
#Plot Variable performance
X <- varImp(sal_nb1)
plot(X)
# Accurancy of the model2 is also 0.81
# Based on the importance, we have prepared another model as given below
colnames(salary_train)
sal_nb2 <- naiveBayes(salary_train$sal ~ age+educationno+relationship+maritalstatus+sex+capitalgain+occupation+capitalloss+hoursperweek+race, data = salary_train)
sal_nb2
predsal2 <- predict(sal_nb2, salary_test)
confusionMatrix(table(predsal2,salary_test$sal1))
# Accuracy of the third model is 0.79.

#Conclusion:
# The accuracy of the first model can be considered for prediction with accuracy of 0.81
# This is a useful model with no information rate 0.75
# P-value is also close to "0".


