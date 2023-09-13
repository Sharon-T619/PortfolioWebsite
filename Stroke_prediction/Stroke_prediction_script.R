## Importing Libraries
library(tidyverse)
library(caret)
library(randomForest)
library(gridExtra)

## Read in the Data
df_stroke <- read.csv("healthcare-dataset-stroke-data.csv")

## Describing Data
glimpse(df_stroke)
# Conclusion
# We have 5110 observations and 12 variables

## Convert character columns to factors- dataframe has columns of various data types
df_stroke$stroke <- factor(df_stroke$stroke, levels = c(0,1), labels = c("No", "Yes"))
df_stroke$gender <- as.factor(df_stroke$gender)
df_stroke$hypertension <- as.factor(df_stroke$hypertension)
df_stroke$heart_disease <- factor(df_stroke$heart_disease, levels = c(0,1), labels = c("No","Yes"))
df_stroke$ever_married <- as.factor(df_stroke$ever_married)
df_stroke$work_type <- as.factor(df_stroke$work_type)
df_stroke$Residence_type <- as.factor(df_stroke$Residence_type)
df_stroke$smoking_status <- as.factor(df_stroke$smoking_status)
df_stroke$bmi <- as.numeric(df_stroke$bmi)

glimpse(df_stroke)

# Perspective of data's statistical distribution
summary(df_stroke)

# Conclusion
#There is only one row for the 'Other' gender, so drop the row

## Drop the column with 'Other' (since there's only 1 row)
df_stroke = df_stroke[!df_stroke$gender == 'Other',]

##Check missing values
sum(is.na(df_stroke))

# Conclusion
# 201 missing values in bmi represents 5% of the total entries, replace missing values using mean value

## Imputing dataset _ replace missing values using mean bmi values
df_stroke$bmi[is.na(df_stroke$bmi)] <- mean(df_stroke$bmi,na.rm = TRUE)

# Reconfirm missing values in the dataset
##Check missing values
sum(is.na(df_stroke))

#### After missing values and data types have been properly configured, generate some graphs from the data to gain insights
## Plot features distribution 
#1. Gender
p1 <- ggplot(df_stroke, aes(x=" ", y=gender, fill=gender)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$gender)
pct=round((a/sum(a))*100)
pct

#2. Hypertension
p2 <- ggplot(df_stroke, aes(x=" ", y=hypertension, fill=hypertension)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$hypertension)
pct=round((a/sum(a))*100)
pct

#3. Heart_disease
p3 <- ggplot(df_stroke, aes(x=" ", y=heart_disease, fill=heart_disease)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$heart_disease)
pct=round((a/sum(a))*100)
pct

#4. Ever_married
p4 <- ggplot(df_stroke, aes(x=" ", y=ever_married, fill=ever_married)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$ever_married)
pct=round((a/sum(a))*100)
pct

grid.arrange(p1,p2,p3,p4, ncol =2)

#5. Residence_type
ggplot(df_stroke, aes(x=" ", y=Residence_type, fill=Residence_type)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$Residence_type)
pct=round((a/sum(a))*100)
pct

#6. Stroke
ggplot(df_stroke, aes(x=" ", y=stroke, fill=stroke)) + geom_bar(stat="identity", width=1) + coord_polar("y", start=0)
a=table(df_stroke$stroke)
pct=round((a/sum(a))*100)
pct

## Observations
# Approximately 10% of the people in the dataset have hypertension
# Around 5% of people in the dataset suffered from heart disease
# Almost an equal split for the feature 'Residence type' - 49% of the population comes from rural regions and 51% from urban 
# More than 65% are married and a higher % are working in the private sector

## Relation between the variables and the target variable (stroke possibility for individuals)
library(gridExtra)
p1 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = gender, fill = stroke))
p2 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = hypertension, fill = stroke))
p3 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = heart_disease, fill = stroke))
p4 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = ever_married, fill = stroke))

grid.arrange(p1, p2, p3, p4, ncol = 2)

p5 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = work_type, fill = stroke))
p6 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = Residence_type, fill = stroke))
p7 <- ggplot(data =  df_stroke) + geom_bar(mapping = aes(x = smoking_status, fill = stroke))

grid.arrange(p5, p6, p7, ncol = 1)

# Model Building and Prediction

## Split final dataset into training and test data
n_obs <- nrow(df_stroke)
split <- round(n_obs * 0.7)
train <- df_stroke[1:split,]

# Create test
test <- df_stroke[(split + 1): nrow(df_stroke),]
dim(train)
dim(test)

## Set.seed - select random seed and make te model reproducible
## RF classifier

# Modeling
set.seed(123)
rf_model <- randomForest(formula = stroke~., data = train)
rf_model

## Conclusion
# Out-of-Bag (OOB) estimate of error rate (7.13%)
# Number of trees 500
# Variables at each split= 3

## Print the confusion matrix - see how the classification model performed on the test data
confusionMatrix(predict(rf_model, test), test$stroke)

## Conclusion
# Accuracy is nearly 100% with a validation dataset
## Model was well trained on the training data












