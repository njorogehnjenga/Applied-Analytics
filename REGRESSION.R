# Regression Analysis ----
## Importing the dataset ----
insurance <- read.csv("C:\\Users\\Njenga\\Desktop\\Applied Analytics\\insurance (1).csv",stringsAsFactors = TRUE)

## Exploring and preparing the data ----
str(insurance)

summary(insurance$charges)  # distribution
hist(insurance$charges)

table(insurance$region)

### Correlation matrix(relationships among features)
cor(insurance[c("age","bmi","children","charges")])

### scatterplot matrix(visualizing relationships)
pairs(insurance[c("age","bmi","children","charges")])

### enhanced scatterplot
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

# oval shape is CORRELATION ELLIPSE
# the more stretched it is the stronger the correlation
# curve is LOESS SMOOTH

## Training a model on the data ----
ins_model <- lm(charges~age+children+bmi+sex+region,data = insurance)
#or ins_model <- lm(charges~ ., data=insurance)

ins_model

## Evaluating model perfomance ----
summary(ins_model)

## Improving model perfomance ----
### Adding non-linear relationships
insurance$age2 <- insurance$age^2

### Transformation- converting numeric variable to binary indicator
insurance$bmi30 <- ifelse(insurance$bmi >=30,1,0)

### Model specification- adding interaction effects
# charges ~ bmi30*smoker

### Putting it all together
ins_model2 <- lm(charges~age+age2+children+bmi+sex+bmi30*smoker+region,data = insurance)

summary(ins_model2)
