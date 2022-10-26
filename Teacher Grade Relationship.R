#R script for teacher pupil ratio and KCSE performance data analysis
#Research Qs 

#Step 1: set working directory
setwd("C:/Users/Njenga/Desktop/R Lab 2")

#Step 2: Examine the contents of the CSV file
file.show("Teacher Pupil Ratio(2).csv")
file.show("KCSE2.csv")


#Step 3: Load the dataset 
TPR<-read.csv ("Teacher Pupil Ratio(2).csv")
KCSE<- read.csv("KCSE2.csv")

# Step 4: Observe the structure of the dataset
str(TPR)
str(KCSE)

#Step 5: Save this as an R data object, so it can be read directly by R
save(TPR, file="TPR.RData")
save(KCSE, file="KCSE.RData")

# Step XX: recover this dataset in another R session
#load(file="TPR.RData")
#load(file="KCSE.RData")

# Step 6: Check for missing data
KCSE[!complete.cases(KCSE),]
TPR[!complete.cases(TPR),]

# Step 7: Remove those missing county assignment
KCSE<-subset(KCSE, County!="#N/A")

# Step 8: Remove any missing data
KCSE<-na.omit(KCSE)

# Step 9: Create a new variable totalgrade = meangrade*frequency
KCSE$Total.grade<-KCSE2$Mean.Grade*KCSE2$Frequency
str(KCSE)

# Step 10: Test accuracy of county data
unique(KCSE$County)
unique(TPR$COUNTY)

#Step 11:Drop Bondo County
TPR<-subset(TPR, COUNTY!="BONDO")
str(TPR)
unique(TPR$COUNTY)

# Step 11: Export data to excel/csv to generate quick pivots
write.csv(KCSE, "C:/Users/Njenga/Desktop/R Lab 2/KCSE.csv")
write.csv(TPR, "C:/Users/Njenga/Desktop/R Lab 2/TPR.csv")

# Step 12: Load condensed data ? called merged data
file.show("merged_data.csv")
merged_data<-read.csv ("merged_data.csv")
str(merged_data)
save(merged_data, file="merged_data.RData")

# Step 13: Draw scatterplot with round point shapes and line of best fit
attach(merged_data)
plot(mean_grade,avg_tpr, main="Scatterplot",xlab="County mean grade ", ylab="County Avg TPR ", pch=19) 
abline(lm(avg_tpr ~ mean_grade)) 

# Step 14: Do a regression
county.lm<-lm(avg_tpr ~ mean_grade, data= merged_data)
summary(county.lm)
plot(county.lm)

