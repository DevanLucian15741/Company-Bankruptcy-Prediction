#=======================Downloading Libraries and Packages=====================#
install.packages("corrplot")
install.packages("ggplot2")
install.packages("dplyr")
install.packages('MLmetrics')
install.packages('caret')
install.packages("recipes")
library(caret)
library(corrplot)
library(ggplot2)
library(dplyr)
library(MLmetrics)

# getwd()
# setwd('C:\\Users\\pattr\\Documents\\College\\Programming\\R')

#=======================Importing Dataset======================================#
Data <- read.csv("data.csv", fileEncoding = "UTF-8-BOM")

#=======================DATA VISUALIZATION=====================================#

# How Many Companies are bankrupt using piechart
Bankrupt = table(Data$Bankrupt.)
pie(Bankrupt,
    main = "Bankruptcy Rate",
    col = rainbow(length(Bankrupt)),
    labels = Bankrupt)

legend("topright",
       legend = c("Not Bankrupt", "Bankrupt"),
       fill=rainbow(length(Bankrupt)),
       cex = 0.8)

# Correlation Between Variables using Correlation Matrix

Correlation_Matrix <-cor(Data$Bankrupt., Data)
negative_threshold <- -0.2

head(round(Correlation_Matrix,2))

# Boxplot Visualization for detecting outliers in Dataset
for (i in 2:ncol(Data)){
  boxplot(Data[,i],
          xlab=colnames(Data)[i],
          col=i)
}
boxplot(Data,
        xlab=colnames(Data),
        col=i)
#=======================DATA PREPROCESSING=====================================#

# Find Missing Values in Dataset
sum(is.na(Data))

# Replace Categorized Binary Values with NO or Yes
Data$Bankrupt. <- factor(Data$Bankrupt., 
                         levels = c(0,1) ,
                         labels = c('NO' , 'YES'))

# Data Selection by Selecting Data based on Correlation Matrix
Dataset = subset(Data,
                 select = c("Bankrupt.", 
                            "ROA.C..before.interest.and.depreciation.before.interest",
                            "ROA.A..before.interest.and...after.tax",
                            "ROA.B..before.interest.and.depreciation.after.tax",
                            "Persistent.EPS.in.the.Last.Four.Seasons",
                            "Debt.ratio..",
                            "Net.Income.to.Total.Assets",
                            "Retained.Earnings.to.Total.Assets"))

colnames(Dataset)

# Replacing Outliers with Average of Selected Data
replace_outliers <- function(data, column) {
  if (is.numeric(data[[column]])) {
    Q1 <- quantile(data[[column]], 0.25)
    Q3 <- quantile(data[[column]], 0.75)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR
    
    # Replace outliers with the mean
    # data[[column]][data[[column]] < lower_bound | data[[column]] > upper_bound] <- mean(data[[column]], na.rm = TRUE)
    
    # Remove the outliers
    # removing gives better result
    data_filtered <- data[data[[column]] >= lower_bound & data[[column]] <= upper_bound, ]
  }
  return(data)
}

for (col in colnames(Dataset)) {
  Dataset <- replace_outliers(Dataset, col)
}

summary(Dataset)

#=======================Logistic Regression====================================#

# Splitting Dataset into Training and Testing Dataset
set.seed(69)
IndexDataset <- sample(2, nrow(Dataset), replace = T,prob = c(0.8,0.2))
TrainSet <- Dataset[IndexDataset==1,]
TestSet <- Dataset[IndexDataset==2,]

# Creating and Fitting Logistic Regression Model with Training Dataset
LogisticRegressionModel <- glm(Bankrupt. ~ 
                                 ROA.C..before.interest.and.depreciation.before.interest+
                                 ROA.A..before.interest.and...after.tax+
                                 ROA.B..before.interest.and.depreciation.after.tax+
                                 Persistent.EPS.in.the.Last.Four.Seasons+
                                 Debt.ratio..+
                                 Net.Income.to.Total.Assets+
                                 Retained.Earnings.to.Total.Assets,
                               data=TrainSet,
                               family = 'binomial')

summary(LogisticRegressionModel)

# Testing the Model with Test Dataset
Prediction<-as.vector(ifelse(predict(LogisticRegressionModel,
                                     TestSet)>0.2
                             ,"YES","NO"))

# Accuracy and F1-Score
print(c(paste0("Accuracy of LR with all features: ",mean(Prediction == TestSet$Bankrupt.)*100,"%")))
print(c(paste0("F1-score of LR with all features: ",F1_Score(Prediction,TestSet$Bankrupt.)*100,"%")))

# Confusion Matrix
Confusion_Matrix <-confusionMatrix(data=as.factor(Prediction),TestSet$Bankrupt.)
Confusion_Matrix

# rm(list=ls())
