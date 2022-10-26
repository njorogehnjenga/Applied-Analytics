# Tuning Stock models for better perfomance ----
## Parameter tuning using caret ----
### creating a simple tuned model ----
library(caret)
set.seed(300) # initialize random number generator
m <- train(default~.,data = credit, method="C5.0")
m
p<- predict(m,credit)
table(p,credit$default)
head(predict(m,credit))
head(predict(m,credit,type="prob")) # probabilities for each class

### Customizing the tuning process ----
ctrl <- trainControl(method = "cv", number = 10, selectionFunction = "oneSE")
grid <-expand.grid(.model="tree",.trials=c(1,5,10,15,20,25,30,35), .winnow= "FALsE")
grid

set.seed(300)
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid)
m

### Bagging ----
library(ipred)
set.seed(300)
mybag <- bagging(default~.,data = credit,nbagg=25)
credit_pred <- predict(mybag,credit)
table(credit_pred,credit$default)

library(caret)
set.seed(300)
ctrl <- trainControl(method="cv",number=10)
train(default~.,data = credit,method="treebag",trControl=ctrl)

### Training random forests ----
library(randomForest)
set.seed(300)
rf <- randomForest(default~.,data = credit)
rf

### Evaluating random forest perfomance ----
library(caret)
ctrl <- trainControl(method = "repeatedcv",number=10,repeats = 10)

grid_rf <- expand.grid(.mtry=c(2,4,8,16))

set.seed(300)
m_rf <- train(default~., data = credit, method="C5.0",metric="Kappa",trControl=ctrl,tuneGrid=grid_c50)