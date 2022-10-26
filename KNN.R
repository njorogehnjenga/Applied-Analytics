# Applied Analytics ----

## Classification algorithms ----

### Knn classification ----

#### Example ----

##### Exploring and preparing the data ----
wbcd <- read.csv("C:\\Users\\Njenga\\Desktop\\Applied Analytics\\wisc_bc_data.csv")
str(wbcd)

# removing the first row
wbcd <- wbcd[-1]
table(wbcd$diagnosis)
wbcd$diagnosis <-factor(wbcd$diagnosis, levels = c("B","M"),labels = c("Benign","Malignant"))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

##### Normalizing ----
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalize(c(1,2,3,4,5))

wbcd_n <- as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)

# Creating train and test datasets
# Split the data
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

# Training the model
install.packages("class")
library(class)
wbcd_test_pred <- knn(train = wbcd_train , test = wbcd_test, cl = wbcd_train_labels, k=21)

# Evaluating Model Perfomance
install.packages("gmodels")
library(gmodels)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)

# Z-score standardization
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

# Divide into train and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)
