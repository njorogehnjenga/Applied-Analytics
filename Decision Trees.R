# DECISION TREES ----

## Exploring and preparing the data ----
credit <- read.csv("C:\\Users\\Njenga\\Downloads\\Lecture 4-20211027\\Credit_Data.csv", stringsAsFactors = TRUE)
credit<- read.csv("https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/credit.csv",stringsAsFactors = TRUE)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
View(credit)
credit$default <- factor(credit$default, levels = c(1,2), labels = c("no","yes"))
summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

## Data preparation ----
### Splitting into train and test datasets ----
set.seed(12345)
credit_rand <- credit[order(runif(1000)),]

#order-rearrange a list of items in ascending or descending
#runif-generates a sequence of random numbers btn 0 and 1
#set.seed-generate random numbers in a predefined sequence

order(c(0.5,0.25,0.75,0.1))
# returns sequence 4 2 1 3(ascending)

summary(credit$amount)
summary(credit_rand$amount)

head(credit$amount)
head(credit_rand$amount)
# first values are different but summary stats are same, shuffle worked correctly
# if results do not match, run set.seed(214805)

credit_train <- credit_rand[1:900,]
credit_test <- credit_rand[901:1000,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

## Training the model ----
install.packages("C50")
library(C50)

credit_model <- C5.0(credit_train[-17],credit_train$default)
credit_model

summary(credit_model)

## Evaluating model perfomance ----
credit_pred <- predict(credit_model,credit_test)

library(gmodels)
CrossTable(credit_test$default,credit_pred, prop.chisq = FALSE, prop.c = FALSE, dnn = c("actual default","predicted default"))

### we can also use prediction probabilities
predicted_prob <- predict(credit_model,credit_test,type="prob")

### Other measures of perfomance ----
install.packages("caret")   #adds another function for confusion matrix

## Boosting accuracy of decision trees ----
credit_boost10 <- C5.0(credit_train[-17],credit_train$default,trials=10)
credit_boost10

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10,credit_test)
CrossTable(credit_test$default,credit_boost_pred10, prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE, dnn = c("actual default","predicted default"))

error_cost <- matrix(c(0,1,4,0),nrow = 2)
error_cost

credit_cost <- C5.0(credit_train[-17],credit_train$default,costs = error_cost)
credit_cost_pred <- predict(credit_cost,credit_test)
CrossTable(credit_test$default,credit_cost_pred,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn = c("actual default","predicted default"))


## RIPPER Algorithm ----
# RIPPER algo generate rules that match or exceed the performance of decision trees
mushrooms <- read.csv("C:\\Users\\Njenga\\Desktop\\Applied Analytics\\mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

mushrooms$veil.type <- NULL  #since veil type has 1 level we drop it 

table(mushrooms$type)  # split is 50/50 hence data is balanced

### Training model-1R classifier ----
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

### Evaluating model performance ----
summary(mushroom_1R)
# confusion matrix to see where rules went wrong

### Improving model performance ----
# we use jrip which is a java based implementation of ripper

mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip

### Evaluating model performance pt2 ----
# we use internal prediction probabilities to evaluate 
# TP-correctly classified as class of interest
# TN-correctly classified as not class of interest
# FP-incorrectly classified as class of interest
# FN-incorrectly classified as not class of interest
# accuracy ~ (TP+TN)/(TP+TN+FP+FN)

### Other measures of performance ----
#### Kappa statistic ----
# adjusts accuracy by accounting for possibility of correct prediction by chance alone
# <0.20 ~ poor agreement
# 0.20-0.40 ~ fair agreement
# 0.40-0.60 ~ moderate agreement
# 0.60-0.80 ~ good agreement
# 0.80-1.00 ~ very good agreement

install.packages("irr")
# kappa2()

#### Precision and recall ----
# Precision(+ve predicted value)-proportion of +ve examples that are truly positive
# Recall- measure of how complete the results are(shown by no of true positives over total +ves)

#library(caret)
# posPredValue(sms_results$predict_type, sms_results$actual_type,positive = "spam")
# for precision

# sensitivity()
# for recall

#### F-measure ----
# combines precision and recall into a single number
