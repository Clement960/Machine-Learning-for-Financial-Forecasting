#packages
install.packages("randomForest")
install.packages("plotly")
install.packages("tidyr")
install.packages("moments")
install.packages("caret")
install.packages("recipes")
install.packages("lattice")
install.packages('Rcpp')
install.packages("randomForest")
install.packages("carData")
install.packages("corrplot")
install.packages("rpart.plot")
install.packages("ROSE")
install.packages("e1071")
install.packages("naivebayes")
install.packages("ISLR")
install.packages("tree")
install.packages("party")
install.packages("sandwich")
install.packages("partykit")
install.packages("grid")
install.packages("libcoin")
install.packages("mvtnorm")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("DMwR")
library(nnet)
library(randomForest)
library(plotly)
library(factoextra)
library(FactoMineR)
library(libcoin)
library(mvtnorm)
library(grid)
library(partykit)
library(tree)
library(ISLR)
library(recipes)
library(lattice)
library(moments)
library(readr)
library(Rcpp)
library(tidyr)
library(data.table)
library(MASS)
library(ggplot2)
library(caret)
library(e1071)
library(magrittr)
library(naivebayes)
library(ROSE)
library(rpart)
library(randomForest)
library(carData)
library(corrplot)
library(rpart.plot)
library(party)
library(sandwich)
library(forecast)
library(readr)
library(tidyr)
library(carData)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(caret)
library(DMwR)

####################################################################################
################################ IMPORTATION AND FORMATTING ########################
####################################################################################

# Data Importation
Train <- read_delim("Train.csv", delim = ";", 
                    escape_double = FALSE, trim_ws = TRUE)
View(Train)

# Formatting variables 
Train$ID<-as.character(Train$ID)
Train$LIMIT_BAL<-as.numeric(Train$LIMIT_BAL)
Train$SEX<-as.factor(Train$SEX)
Train$EDUCATION<-as.factor(Train$EDUCATION)
Train$MARRIAGE<-as.factor(Train$MARRIAGE)
Train$AGE<-as.numeric(Train$AGE)
Train$CHILDREN<-as.numeric(Train$CHILDREN)
Train$PAY_1<-as.factor(Train$PAY_1)
Train$PAY_2<-as.factor(Train$PAY_2)
Train$PAY_3<-as.factor(Train$PAY_3)
Train$PAY_4<-as.factor(Train$PAY_4)
Train$PAY_5<-as.factor(Train$PAY_5)
Train$PAY_6<-as.factor(Train$PAY_6)
Train$BILL_AMT1<-as.numeric(Train$BILL_AMT1)
Train$BILL_AMT2<-as.numeric(Train$BILL_AMT2)
Train$BILL_AMT3<-as.numeric(Train$BILL_AMT3)
Train$BILL_AMT4<-as.numeric(Train$BILL_AMT4)
Train$BILL_AMT5<-as.numeric(Train$BILL_AMT5)
Train$BILL_AMT6<-as.numeric(Train$BILL_AMT6)
Train$PAY_AMT1<-as.numeric(Train$PAY_AMT1)
Train$PAY_AMT2<-as.numeric(Train$PAY_AMT2)
Train$PAY_AMT3<-as.numeric(Train$PAY_AMT3)
Train$PAY_AMT4<-as.numeric(Train$PAY_AMT4)
Train$PAY_AMT5<-as.numeric(Train$PAY_AMT5)
Train$PAY_AMT6<-as.numeric(Train$PAY_AMT6)
Train$default.payment.next.month<-as.factor(Train$default.payment.next.month)

#Remplace empty value

#preparation des données pour la moyenne.
table1 <- table(Train$SEX)
table2 <- table(Train$EDUCATION)
table3 <- table(Train$MARRIAGE)

#Delete id 
Train<-Train[,-1]

#Change Na by most reccurent categorical value
Train$SEX[is.na(Train$SEX)] <- names(table1)[which(table1==max(table1))]
Train$EDUCATION[is.na(Train$EDUCATION)] <- names(table2)[which(table2==max(table2))]
Train$MARRIAGE[is.na(Train$MARRIAGE)] <-names(table3)[which(table3==max(table3))]
Train$AGE[which(is.na(Train$AGE))]<-round(mean(Train$AGE,na.rm=TRUE))
Train$CHILDREN[which(is.na(Train$CHILDREN))]<-round(mean(Train$CHILDREN,na.rm=TRUE))

#Replace  0 in Marriage variable
Train$MARRIAGE[Train$MARRIAGE==0]<-names(table3)[which(table3==max(table3))]

View(Train)

####################################################################################
################################ Data Exploration ##################################
####################################################################################


######### UNIVARIATE #########



# See global statistical value
summary(Train)

# barplot of explained variable

length(Train$default.payment.next.month)
table <- table(Train$default.payment.next.month)
table

barplot(table, main="distribution of default payment next month", col=c("darkblue","orange"))


# Histogram for each variables

hist(Train$LIMIT_BAL,prob=FALSE,xlab= "Value", main = paste("Histogram of LIMIT_BAL"))

table1 <- table(Train$SEX)
table1
barplot(table1, main="distribution of SEX", col=c("darkblue","orange"))

table2 <- table(Train$EDUCATION)
table2
barplot(table2, main="distribution of EDUCATION", col=c("darkblue","orange", "red", "brown"))

table3 <- table(Train$MARRIAGE)
table3
barplot(table3, main="distribution of MARRIAGE", col=c("darkblue","orange", "red", "brown"))

hist(Train$AGE,prob=FALSE,xlab= "Value", main = paste("Histogram of AGE"))

hist(Train$CHILDREN,prob=FALSE,xlab= "Value", main = paste("Histogram of CHILDREN"))

table4 <- table(Train$PAY_1)
table4
barplot(table4, main="distribution of PAY_1", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey"))

table5 <- table(Train$PAY_2)
table5
barplot(table5, main="distribution of PAY_2", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey"))

table6 <- table(Train$PAY_3)
table6
barplot(table6, main="distribution of PAY_3", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey","purple"))

table7 <- table(Train$PAY_4)
table7
barplot(table7, main="distribution of PAY_4", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey","purple"))

table8 <- table(Train$PAY_5)
table8
barplot(table8, main="distribution of PAY_5", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey","purple"))

table9 <- table(Train$PAY_6)
table9
barplot(table9, main="distribution of PAY_6", col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey","purple"))

hist(Train$BILL_AMT1,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT1"))

hist(Train$BILL_AMT2,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT2"))

hist(Train$BILL_AMT3,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT3"))

hist(Train$BILL_AMT4,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT4"))

hist(Train$BILL_AMT5,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT5"))

hist(Train$BILL_AMT6,prob=FALSE,xlab= "Value", main = paste("Histogram of BILL_AMT6"))

hist(Train$PAY_AMT1,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT1"))

hist(Train$PAY_AMT2,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT2"))

hist(Train$PAY_AMT3,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT3"))

hist(Train$PAY_AMT4,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT4"))

hist(Train$PAY_AMT5,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT5"))

hist(Train$PAY_AMT6,prob=FALSE,xlab= "Value", breaks = 1000, main = paste("Histogram of PAY_AMT6"))

# Analysis of all the variables

#LIMIT_BAL
sd(Train$LIMIT_BAL, na.rm = FALSE)
kurtosis(Train$LIMIT_BAL)
skewness(Train$LIMIT_BAL)
boxplot(Train$LIMIT_BAL)


#SEX
sd(table1, na.rm = FALSE)
kurtosis(table1)
skewness(table1)
barplot(table1, main = "Barplot of SEX")
length(Train$SEX)
table1

#EDUCATION
sd(table2, na.rm = FALSE)
kurtosis(table2)
skewness(table2)
barplot(table2, col=c("darkblue","orange", "red", "brown","lightblue", "pink","yellow","black", "grey","purple"), main = "Barplot of EDUCATION", xlab = "1=graduate school, 2=university, 3=high school, 4=others" )
length(Train$EDUCATION)
length(table2)
length(Train$EDUCATION)

#MARRIAGE
sd(table3, na.rm = FALSE)
kurtosis(table3)
skewness(table3)
barplot(table3)

#AGE
sd(Train$AGE, na.rm = FALSE)
kurtosis(Train$AGE)
skewness(Train$AGE)
boxplot(Train$AGE)

#CHILDREN 
sd(Train$CHILDREN, na.rm = FALSE)
kurtosis(Train$CHILDREN)
skewness(Train$CHILDREN)
boxplot(Train$CHILDREN)

#PAY_AMT1
sd(Train$PAY_AMT1, na.rm = FALSE)
kurtosis(Train$PAY_AMT1)
skewness(Train$PAY_AMT1)
boxplot(Train$PAY_AMT1)

#PAY_AMT2
sd(Train$PAY_AMT2, na.rm = FALSE)
kurtosis(Train$PAY_AMT2)
skewness(Train$PAY_AMT2)

#PAY_AMT3
sd(Train$PAY_AMT3, na.rm = FALSE)
kurtosis(Train$PAY_AMT3)
skewness(Train$PAY_AMT3)
boxplot(Train$PAY_AMT3)

#PAY_AMT4
sd(Train$PAY_AMT4, na.rm = FALSE)
kurtosis(Train$PAY_AMT4)
skewness(Train$PAY_AMT4)
boxplot(Train$PAY_AMT4)

#PAY_AMT5
sd(Train$PAY_AMT5, na.rm = FALSE)
kurtosis(Train$PAY_AMT5)
skewness(Train$PAY_AMT5)
boxplot(Train$PAY_AMT5)

#PAY_AMT6
sd(Train$PAY_AMT6, na.rm = FALSE)
kurtosis(Train$PAY_AMT6)
skewness(Train$PAY_AMT6)
boxplot(Train$PAY_AMT6)

#BILL_AMT1
sd(Train$BILL_AMT1, na.rm = FALSE)
kurtosis(Train$BILL_AMT1)
skewness(Train$BILL_AMT1)
boxplot(Train$BILL_AMT1)

#BILL_AMT2
sd(Train$BILL_AMT2, na.rm = FALSE)
kurtosis(Train$BILL_AMT2)
skewness(Train$BILL_AMT2)
boxplot(Train$BILL_AMT2)

#BILL_AMT3
sd(Train$BILL_AMT3, na.rm = FALSE)
kurtosis(Train$BILL_AMT3)
skewness(Train$BILL_AMT3)
boxplot(Train$BILL_AMT3)

#BILL_AMT4
sd(Train$BILL_AMT4, na.rm = FALSE)
kurtosis(Train$BILL_AMT4)
skewness(Train$BILL_AMT4)
boxplot(Train$BILL_AMT4)

#BILL_AMT5
sd(Train$BILL_AMT5, na.rm = FALSE)
kurtosis(Train$BILL_AMT5)
skewness(Train$BILL_AMT5)
boxplot(Train$BILL_AMT5)

#BILL_AMT6
sd(Train$BILL_AMT6, na.rm = FALSE)
kurtosis(Train$BILL_AMT6)
skewness(Train$BILL_AMT6)
boxplot(Train$BILL_AMT6)
  


######### BIVARIATE #########



# ScatterPlots numerical variables
plot(Train$LIMIT_BAL,Train$AGE)
plot(Train$LIMIT_BAL,Train$BILL_AMT1)
plot(Train$LIMIT_BAL,Train$BILL_AMT2)
plot(Train$LIMIT_BAL,Train$BILL_AMT3)
plot(Train$LIMIT_BAL,Train$BILL_AMT4)
plot(Train$LIMIT_BAL,Train$BILL_AMT5)
plot(Train$LIMIT_BAL,Train$BILL_AMT6)
plot(Train$LIMIT_BAL,Train$PAY_AMT1)
plot(Train$LIMIT_BAL,Train$PAY_AMT2)
plot(Train$LIMIT_BAL,Train$PAY_AMT3)
plot(Train$LIMIT_BAL,Train$PAY_AMT4)
plot(Train$LIMIT_BAL,Train$PAY_AMT5)
plot(Train$LIMIT_BAL,Train$PAY_AMT6)

plot(Train$AGE,Train$BILL_AMT1)
plot(Train$AGE,Train$BILL_AMT2)
plot(Train$AGE,Train$BILL_AMT3)
plot(Train$AGE,Train$BILL_AMT4)
plot(Train$AGE,Train$BILL_AMT5)
plot(Train$AGE,Train$BILL_AMT6)
plot(Train$AGE,Train$PAY_AMT1)
plot(Train$AGE,Train$PAY_AMT2)
plot(Train$AGE,Train$PAY_AMT3)
plot(Train$AGE,Train$PAY_AMT4)
plot(Train$AGE,Train$PAY_AMT5)
plot(Train$AGE,Train$PAY_AMT6)

plot(Train$BILL_AMT1,Train$BILL_AMT2)
plot(Train$BILL_AMT1,Train$BILL_AMT3)
plot(Train$BILL_AMT1,Train$BILL_AMT4)
plot(Train$BILL_AMT1,Train$BILL_AMT5)
plot(Train$BILL_AMT1,Train$BILL_AMT6)
plot(Train$BILL_AMT1,Train$PAY_AMT1)
plot(Train$BILL_AMT1,Train$PAY_AMT2)
plot(Train$BILL_AMT1,Train$PAY_AMT3)
plot(Train$BILL_AMT1,Train$PAY_AMT4)
plot(Train$BILL_AMT1,Train$PAY_AMT5)
plot(Train$BILL_AMT1,Train$PAY_AMT6)

plot(Train$BILL_AMT2,Train$BILL_AMT3)
plot(Train$BILL_AMT2,Train$BILL_AMT4)
plot(Train$BILL_AMT2,Train$BILL_AMT5)
plot(Train$BILL_AMT2,Train$BILL_AMT6)
plot(Train$BILL_AMT2,Train$PAY_AMT1)
plot(Train$BILL_AMT2,Train$PAY_AMT2)
plot(Train$BILL_AMT2,Train$PAY_AMT3)
plot(Train$BILL_AMT2,Train$PAY_AMT4)
plot(Train$BILL_AMT2,Train$PAY_AMT5)
plot(Train$BILL_AMT2,Train$PAY_AMT6)

plot(Train$BILL_AMT3,Train$BILL_AMT4)
plot(Train$BILL_AMT3,Train$BILL_AMT5)
plot(Train$BILL_AMT3,Train$BILL_AMT6)
plot(Train$BILL_AMT3,Train$PAY_AMT1)
plot(Train$BILL_AMT3,Train$PAY_AMT2)
plot(Train$BILL_AMT3,Train$PAY_AMT3)
plot(Train$BILL_AMT3,Train$PAY_AMT4)
plot(Train$BILL_AMT3,Train$PAY_AMT5)
plot(Train$BILL_AMT3,Train$PAY_AMT6)

plot(Train$BILL_AMT4,Train$BILL_AMT5)
plot(Train$BILL_AMT4,Train$BILL_AMT6)
plot(Train$BILL_AMT4,Train$PAY_AMT1)
plot(Train$BILL_AMT4,Train$PAY_AMT2)
plot(Train$BILL_AMT4,Train$PAY_AMT3)
plot(Train$BILL_AMT4,Train$PAY_AMT4)
plot(Train$BILL_AMT4,Train$PAY_AMT5)
plot(Train$BILL_AMT4,Train$PAY_AMT6)

plot(Train$BILL_AMT5,Train$BILL_AMT6)
plot(Train$BILL_AMT5,Train$PAY_AMT1)
plot(Train$BILL_AMT5,Train$PAY_AMT2)
plot(Train$BILL_AMT5,Train$PAY_AMT3)
plot(Train$BILL_AMT5,Train$PAY_AMT4)
plot(Train$BILL_AMT5,Train$PAY_AMT5)
plot(Train$BILL_AMT5,Train$PAY_AMT6)

plot(Train$BILL_AMT6,Train$PAY_AMT1)
plot(Train$BILL_AMT6,Train$PAY_AMT2)
plot(Train$BILL_AMT6,Train$PAY_AMT3)
plot(Train$BILL_AMT6,Train$PAY_AMT4)
plot(Train$BILL_AMT6,Train$PAY_AMT5)
plot(Train$BILL_AMT6,Train$PAY_AMT6)

plot(Train$PAY_AMT1,Train$PAY_AMT2)
plot(Train$PAY_AMT1,Train$PAY_AMT3)
plot(Train$PAY_AMT1,Train$PAY_AMT4)
plot(Train$PAY_AMT1,Train$PAY_AMT5)
plot(Train$PAY_AMT1,Train$PAY_AMT6)

plot(Train$PAY_AMT2,Train$PAY_AMT3)
plot(Train$PAY_AMT2,Train$PAY_AMT4)
plot(Train$PAY_AMT2,Train$PAY_AMT5)
plot(Train$PAY_AMT2,Train$PAY_AMT6)

plot(Train$PAY_AMT3,Train$PAY_AMT4)
plot(Train$PAY_AMT3,Train$PAY_AMT5)
plot(Train$PAY_AMT3,Train$PAY_AMT6)

plot(Train$PAY_AMT4,Train$PAY_AMT5)
plot(Train$PAY_AMT4,Train$PAY_AMT6)

plot(Train$PAY_AMT5,Train$PAY_AMT6)

plot(Train$CHILDREN,Train$LIMIT_BAL)
plot(Train$CHILDREN,Train$AGE)
plot(Train$CHILDREN,Train$BILL_AMT1)
plot(Train$CHILDREN,Train$BILL_AMT2)
plot(Train$CHILDREN,Train$BILL_AMT3)
plot(Train$CHILDREN,Train$BILL_AMT4)
plot(Train$CHILDREN,Train$BILL_AMT5)
plot(Train$CHILDREN,Train$BILL_AMT6)
plot(Train$CHILDREN,Train$PAY_AMT1)
plot(Train$CHILDREN,Train$PAY_AMT2)
plot(Train$CHILDREN,Train$PAY_AMT3)
plot(Train$CHILDREN,Train$PAY_AMT4)
plot(Train$CHILDREN,Train$PAY_AMT5)
plot(Train$CHILDREN,Train$PAY_AMT6)

# Example of scatterplot with very low correlation: Amount Bill 1 and Age

cor(Train$BILL_AMT1, Train$AGE) # 0,04
plot(Train$BILL_AMT1, Train$AGE, 
     xlab="Amount Bill 1 ", ylab="Age ", pch=19)
abline(lm(Train$BILL_AMT1 ~ Train$AGE), col="red") # regression line 
lines(lowess(Train$BILL_AMT1,Train$BILL_AMT2), col="blue") # lowess line 

# Example of scatterplot with correlation 

cor(Train$BILL_AMT1, Train$BILL_AMT2) #0,72
plot(Train$BILL_AMT1, Train$BILL_AMT2,  
     xlab="Amount Bill 1 ", ylab="Amount Bill 2 ", pch=19)
abline(lm(Train$BILL_AMT1 ~ Train$BILL_AMT2), col="red") # regression line 
lines(lowess(Train$BILL_AMT1, Train$BILL_AMT2), col="blue") # lowess line 

# Example de scatterplot with low correlation

cor(Train$BILL_AMT1, Train$LIMIT_BAL) 
plot(Train$BILL_AMT1, Train$LIMIT_BAL, 
     xlab="Bill amount 1 ", ylab="Limit bal ", pch=19)
abline(lm(Train$BILL_AMT1 ~ Train$LIMIT_BAL), col="red") 
lines(lowess(Train$BILL_AMT1,Train$LIMIT_BAL), col="blue") # lowess line

# Correlations

Train2<-Train[,c(2,3,4,7,8,9,10,11,12,25,1,5,6,13,14,15,16,17,18,19,20,21,22,23,24)]
View(Train2)
Train_Cate<-Train2[,1:10]
View(Train_Cate)
Train_Num<-scale(Train2[,-1:-10])
View(Train_Num)

correlations <- cor(Train_Num)
corrplot(correlations, method="number") #All the Bill amounts are heavily correlated.

# Independence test

chisq.test(Train$SEX, Train$default.payment.next.month)         #Dependent
chisq.test(Train$EDUCATION, Train$default.payment.next.month)   #Dependent
chisq.test(Train$MARRIAGE, Train$default.payment.next.month)    #Dependent
chisq.test(Train$PAY_1, Train$default.payment.next.month)       #Dependent
chisq.test(Train$PAY_2, Train$default.payment.next.month)       #Dependent
chisq.test(Train$PAY_3, Train$default.payment.next.month)       #Dependent
chisq.test(Train$PAY_4, Train$default.payment.next.month)       #Dependent
chisq.test(Train$PAY_5, Train$default.payment.next.month)       #Dependent
chisq.test(Train$PAY_6, Train$default.payment.next.month)       #Dependent


# ANOVA

Anova <- aov(Train_Num[,1] ~ Train$default.payment.next.month)  #Independent
summary(Anova)
Anova <- aov(Train_Num[,2] ~ Train$default.payment.next.month)  #Independent
summary(Anova)
Anova <- aov(Train_Num[,3] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,4] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,5] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,6] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,7] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,8] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,9] ~ Train$default.payment.next.month)  #Dependent
summary(Anova)
Anova <- aov(Train_Num[,10] ~ Train$default.payment.next.month) #Independent
summary(Anova)
Anova <- aov(Train_Num[,11] ~ Train$default.payment.next.month) #Independent
summary(Anova)
Anova <- aov(Train_Num[,12] ~ Train$default.payment.next.month) #Independent
summary(Anova)
Anova <- aov(Train_Num[,13] ~ Train$default.payment.next.month) #Independent
summary(Anova)
Anova <- aov(Train_Num[,14] ~ Train$default.payment.next.month) #Independent
summary(Anova)
Anova <- aov(Train_Num[,15] ~ Train$default.payment.next.month) #Independent
summary(Anova)

# ACP
ACP = PCA(Train_Num)
summary(ACP) # Only on the numerical variables, thus is not explaining a lot of the variance. 

vector.pca<-Train_Num
pca<-prcomp(vector.pca,center=TRUE,scale=TRUE)
print(pca)
summary(pca)
plot(pca, type = "l", col= 'blue', main ="PCA")



########################################################################################
################################ Feature selection / extraction ########################
########################################################################################

######### Feature Selection #########  

#Based on previous analysis
#We select all the dependent variables (categorical and numerical) but we exclude the variables which are too correlated.
Train_Selection = Train
Train_Selection = Train[,-1]
Train_Selection = Train_Selection[,-4]
Train_Selection = Train_Selection[,-17:-22]
Train_Selection = Train_Selection[,-12:-16]

View(Train_Selection)


######### Feature Extraction #########  

###PCA 
pca<-prcomp(Train_Num,center=TRUE,scale=TRUE)
plot(pca,type="l", col=c("green"), main ="PCA")
#We see that the firsts 10 features catch 89,871% of de variance of the model. 


#New Features
Train_PCA<-data.frame(matrix(nrow = nrow(Train),ncol = 20))
colnames(Train_PCA) <- c("PC1","PC2","PC3", "PC4","PC5","PC6","PC7","PC8","PC9","PC10","SEX", "Education", "Marriage","PAY_1","PAY_2","PAY_3","PAY_4","PAY_5","PAY_6","default.payment.next.month")

for (a in 1:10)
{
  Train_PCA[,a]= pca$x[,a]
}
Train_PCA[,11:20]<-Train_Cate
summary(Train_PCA)
View(Train_PCA)






########################################################################################
################################ Modelling and experiments #############################
########################################################################################


#scale
Train_Num_Scale<-scale(Train_Num)

#Create a training set
set.seed(100)
train_ind <- createDataPartition(Train$default.payment.next.month, p=0.8, list=FALSE)
training <- Train[train_ind, ]
testing <- Train[-train_ind, ]

#Create a training set for Train_Selection
set.seed(100)
train_ind2 <- createDataPartition(Train_Selection$default.payment.next.month, p=0.8, list=FALSE)
training_Selec <- Train[train_ind, ]
testing_Selec <- Train[-train_ind, ]

#Create a training set for Train_PCA (extraction)
set.seed(100)
train_ind3 <- createDataPartition(Train_PCA$default.payment.next.month, p=0.8, list=FALSE)
training_PCA <- Train[train_ind, ]
testing_PCA <- Train[-train_ind, ]


########  Logistic Regression ######## 


LogReg <- multinom(default.payment.next.month~.,data=training)
PredLogReg <- predict(LogReg,testing)

# Computation of the accuracy, by comparing the predictions with the true value
Accuracy <- sum(PredLogReg==testing$default.payment.next.month)/nrow(testing)
Accuracy

# Computation of the confusion matrix
ConfusionMatrix_RegLog = table(PredLogReg,testing$default.payment.next.month)
ConfusionMatrix_RegLog

# Alternative way of computing the accuracy: from the confusion matrix
Accuracy2 <- (ConfusionMatrix_RegLog[1,1]+ConfusionMatrix_RegLog[2,2])/sum(ConfusionMatrix_RegLog)
Accuracy2

#cross valiation 
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train[Fold==i,]
  train <- Train[Fold!=i,]
  # build model
  reg <- multinom(default.payment.next.month~.,data=train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Multinom <- mean(Accuracy_vector)
Accuracy_Multinom


#--------------------------stepwise----------------------------
library(nnet)
library(MASS)
library(caret)

set.seed(100) #To always get the same training set and validation set
train_ind <- createDataPartition(Train$default.payment.next.month, p=0.8, list=FALSE)
testing<- Train[train_ind, ]
validation <- Train[-train_ind, ]


###Stepwise regression
reg<-glm(default.payment.next.month~.,family = binomial, data = testing)
myStepwise <- stepAIC(reg, scope=testing$default.payment.next.month ~., direction="both", steps=1000, trace=FALSE)

#Prediction, confusion matrix & accuracy of stepwise regression
pred_reg<-predict(myStepwise,validation)
pred_reg
confusionMatrix_reg = table(pred_reg>0.5,validation$default.payment.next.month)
confusionMatrix_reg


#A bit better than the logistic regression
Accuracy_reg <- (confusionMatrix_reg[1,1]+confusionMatrix_reg[2,2])/sum(confusionMatrix_reg)
Accuracy_reg




########  Logistic Regression with feature selection ######## 


LogReg <- multinom(default.payment.next.month~.,data=training_Selec)
PredLogReg <- predict(LogReg,testing_Selec)

# Computation of the accuracy, by comparing the predictions with the true value
Accuracy <- sum(PredLogReg==testing_Selec$default.payment.next.month)/nrow(testing_Selec)
Accuracy

# Computation of the confusion matrix
ConfusionMatrix_RegLog = table(PredLogReg,testing_Selec$default.payment.next.month)
ConfusionMatrix_RegLog

# Alternative way of computing the accuracy: from the confusion matrix
Accuracy2 <- (ConfusionMatrix_RegLog[1,1]+ConfusionMatrix_RegLog[2,2])/sum(ConfusionMatrix_RegLog)
Accuracy2

#cross valiation 
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_Selection[Fold==i,]
  train <- Train_Selection[Fold!=i,]
  # build model
  reg <- multinom(default.payment.next.month~.,data=train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Multinom_Selec <- mean(Accuracy_vector)
Accuracy_Multinom_Selec



########  Logistic Regression with feature extraction ######## 


LogReg <- multinom(default.payment.next.month~.,data=training_PCA)
PredLogReg <- predict(LogReg,testing_PCA)

# Computation of the accuracy, by comparing the predictions with the true value
Accuracy <- sum(PredLogReg==testing_PCA$default.payment.next.month)/nrow(testing_PCA)
Accuracy

# Computation of the confusion matrix
ConfusionMatrix_RegLog = table(PredLogReg,testing_PCA$default.payment.next.month)
ConfusionMatrix_RegLog

# Alternative way of computing the accuracy: from the confusion matrix
Accuracy2 <- (ConfusionMatrix_RegLog[1,1]+ConfusionMatrix_RegLog[2,2])/sum(ConfusionMatrix_RegLog)
Accuracy2

#cross valiation 
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_PCA[Fold==i,]
  train <- Train_PCA[Fold!=i,]
  # build model
  reg <- multinom(default.payment.next.month~.,data=train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Multinom_PCA <- mean(Accuracy_vector)
Accuracy_Multinom_PCA



#########  Naive Bayes ########## 


#Regression Bayes
RegBayes <- naiveBayes(as.factor(default.payment.next.month)~.,data = training)
summary(RegBayes)
print(RegBayes)

#Prediction Bayes
PredBayes = predict(reg_Naive, testing)
PredBayes

#Confusion matrix Bayes
confusionMatrix = table(PredBayes,testing$default.payment.next.month)
confusionMatrix

#Accuracy Bayes
Accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
Accuracy

#cross valiation Bayes
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train[Fold==i,]
  train <- Train[Fold!=i,]
  # build model
  reg <- naiveBayes(as.factor(default.payment.next.month)~.,data = train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Bayes <- mean(Accuracy_vector)
Accuracy_Bayes




######### Naive Bayes with feature selection ########## 


#Regression Bayes
RegBayes <- naiveBayes(as.factor(default.payment.next.month)~.,data = training_Selec)
summary(RegBayes)
print(RegBayes)

#Prediction Bayes
PredBayes = predict(reg_Naive, testing_Selec)
PredBayes

#Confusion matrix Bayes
confusionMatrix = table(PredBayes,testing_Selec$default.payment.next.month)
confusionMatrix

#Accuracy Bayes
Accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
Accuracy

#cross valiation Bayes
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_Selection[Fold==i,]
  train <- Train_Selection[Fold!=i,]
  # build model
  reg <- naiveBayes(as.factor(default.payment.next.month)~.,data = train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Bayes_Selec <- mean(Accuracy_vector)
Accuracy_Bayes_Selec



#########  Naive Bayes with feature extraction ########## 


#Regression Bayes
RegBayes <- naiveBayes(as.factor(default.payment.next.month)~.,data = training_PCA)
summary(RegBayes)
print(RegBayes)

#Prediction Bayes
PredBayes = predict(reg_Naive, testing_PCA)
PredBayes

#Confusion matrix Bayes
confusionMatrix = table(PredBayes,testing_PCA$default.payment.next.month)
confusionMatrix

#Accuracy Bayes
Accuracy <- (confusionMatrix[1,1]+confusionMatrix[2,2])/sum(confusionMatrix)
Accuracy

#cross valiation Bayes
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_PCA[Fold==i,]
  train <- Train_PCA[Fold!=i,]
  # build model
  reg <- naiveBayes(as.factor(default.payment.next.month)~.,data = train)
  pred <- predict(reg,validation)
  Accuracy_vector[i] <- sum(pred==validation$default.payment.next.month)/nrow(validation)
}
Accuracy_Bayes_PCA <- mean(Accuracy_vector)
Accuracy_Bayes_PCA


######## Decision tree ########

# setting up the training tree 
Tree <- rpart(default.payment.next.month~.,data=training,method="class", control=rpart.control(minsplit=5,cp=0))

# vizualization
plot(Tree, uniform=TRUE, branch=0.5, margin=0.1)
text(Tree, all=FALSE, use.n=TRUE)
plotcp(Tree)

# print of the best cp
print(Tree$cptable[which.min(Tree$cptable[,4]),1])

#trim
Tree_Opt <- prune(Tree,cp=Tree$cptable[which.min(Tree$cptable[,4]),1])

#new plot 
prp(Tree,extra=1)

#Prediction
PredTree<-predict(Tree_Opt,newdata=testing, type="class"  )

#Confusion Matrix
ConfusionMatrix<-table(testing$default.payment.next.month,PredTree)
print(ConfusionMatrix)

#accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross valiation Tree
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train[Fold==i,]
  train <- Train[Fold!=i,]
  # build model
  reg <- rpart(default.payment.next.month~.,data=train,method="class", control=rpart.control(minsplit=5,cp=0))
  reg2 <- prune(reg,cp=reg$cptable[which.min(reg$cptable[,4]),1])
  pred<-predict(reg2,newdata=validation, type="class"  )
  tab<-table(pred,validation$default.payment.next.month)
  acc=(tab[2,2]+tab[1,1])/sum(tab)
  Accuracy_vector[i] <- acc
}
Accuracy_Tree <- mean(Accuracy_vector)
Accuracy_Tree





######## Decision tree with feature selection ########


# setting up the training tree 
Tree <- rpart(default.payment.next.month~.,data=training_Selec,method="class", control=rpart.control(minsplit=5,cp=0))

# vizualization
plot(Tree, uniform=TRUE, branch=0.5, margin=0.1)
text(Tree, all=FALSE, use.n=TRUE)
plotcp(Tree)

# print of the best cp
print(Tree$cptable[which.min(Tree$cptable[,4]),1])

#trim
Tree_Opt <- prune(Tree,cp=Tree$cptable[which.min(Tree$cptable[,4]),1])

#new plot 
prp(Tree,extra=1)

#Prediction
PredTree<-predict(Tree_Opt,newdata=testing_Selec, type="class"  )

#Confusion Matrix
ConfusionMatrix<-table(testing_Selec$default.payment.next.month,PredTree)
print(ConfusionMatrix)

#accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross valiation Tree
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_Selection[Fold==i,]
  train <- Train_Selection[Fold!=i,]
  # build model
  reg <- rpart(default.payment.next.month~.,data=train,method="class", control=rpart.control(minsplit=5,cp=0))
  reg2 <- prune(reg,cp=reg$cptable[which.min(reg$cptable[,4]),1])
  pred<-predict(reg2,newdata=validation, type="class"  )
  tab<-table(pred,validation$default.payment.next.month)
  acc=(tab[2,2]+tab[1,1])/sum(tab)
  Accuracy_vector[i] <- acc
}
Accuracy_Tree_Selec <- mean(Accuracy_vector)
Accuracy_Tree_Selec



######## Decision tree with feature extraction ########

# setting up the training tree 
Tree <- rpart(default.payment.next.month~.,data=training_PCA,method="class", control=rpart.control(minsplit=5,cp=0))

# vizualization
plot(Tree, uniform=TRUE, branch=0.5, margin=0.1)
text(Tree, all=FALSE, use.n=TRUE)
plotcp(Tree)

# print of the best cp
print(Tree$cptable[which.min(Tree$cptable[,4]),1])

#trim
Tree_Opt <- prune(Tree,cp=Tree$cptable[which.min(Tree$cptable[,4]),1])

#new plot 
prp(Tree,extra=1)

#Prediction
PredTree<-predict(Tree_Opt,newdata=testing_PCA, type="class"  )

#Confusion Matrix
ConfusionMatrix<-table(testing_PCA$default.payment.next.month,PredTree)
print(ConfusionMatrix)

#accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross valiation Tree
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_PCA[Fold==i,]
  train <- Train_PCA[Fold!=i,]
  # build model
  reg <- rpart(default.payment.next.month~.,data=train,method="class", control=rpart.control(minsplit=5,cp=0))
  reg2 <- prune(reg,cp=reg$cptable[which.min(reg$cptable[,4]),1])
  pred<-predict(reg2,newdata=validation, type="class"  )
  tab<-table(pred,validation$default.payment.next.month)
  acc=(tab[2,2]+tab[1,1])/sum(tab)
  Accuracy_vector[i] <- acc
}
Accuracy_Tree_PCA <- mean(Accuracy_vector)
Accuracy_Tree_PCA

######## Neuronal Link ########


#Neurones with 1 layer
set.seed(100)
NL <- nnet(default.payment.next.month ~ ., data = training, skip = TRUE, size = 0)

#Prediction
PredNL <- predict(NL,testing)

#Confusion matrix
ConfusionMatrix <- table(PredNL,testing$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy

#Neurones with 3 layers
set.seed(100)
NL2 <- nnet(default.payment.next.month ~ ., data = training, skip = FALSE, size = 3, maxit = 1000)

#Prediciton
PredNL2 <- predict(NL2,testing)

#Confusion matrix
ConfusionMatrix <- table(PredNL2>0.5,testing$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train[Fold==i,]
  train_cv <- Train[Fold!=i,]
  # build model
  NN <- nnet(default.payment.next.month ~ ., data = train_cv, rang = 0.1, decay = 5e-4 , size = 3, maxit = 1000)
  pred <- predict(NN,newdata=validation_cv,type="class")
  tab<-table((pred),validation_cv$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_NL <- mean(Accuracy_vector)
Accuracy_NL



######## Neuronal Link with feature selection ########


#Neurones with 1 layer
set.seed(100)
NL <- nnet(default.payment.next.month ~ ., data = training_Selec, skip = TRUE, size = 0)

#Prediction
PredNL <- predict(NL,testing_Selec)

#Confusion matrix
ConfusionMatrix <- table(PredNL,testing_Selec$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy

#Neurones with 3 layers
set.seed(100)
NL2 <- nnet(default.payment.next.month ~ ., data = training_Selec, skip = FALSE, size = 3, maxit = 1000)

#Prediciton
PredNL2 <- predict(NL2,testing_Selec)

#Confusion matrix
ConfusionMatrix <- table(PredNL2>0.5,testing_Selec$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation <- Train_Selection[Fold==i,]
  train <- Train_Selection[Fold!=i,]
  # build model
  NN <- nnet(default.payment.next.month ~ ., data = train, rang = 0.1, decay = 5e-4 , size = 3, maxit = 1000)
  pred <- predict(NN,newdata=validation)
  tab<-table(pred,validation$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_NL_Selec <- mean(Accuracy_vector)
Accuracy_NL_Selec



######## Neuronal Link with feature extraction ########


#Neurones with 1 layer
set.seed(100)
NL <- nnet(default.payment.next.month ~ ., data = training_PCA, skip = TRUE, size = 0)

#Prediction
PredNL <- predict(NL,testing_PCA)

#Confusion matrix
ConfusionMatrix <- table(round(PredNL),testing_PCA$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy

#Neurones with 3 layers
set.seed(100)
NL2 <- nnet(default.payment.next.month ~ ., data = training_PCA, skip = FALSE, size = 3, maxit = 1000)

#Prediciton
PredNL2 <- predict(NL2,testing_PCA)

#Confusion matrix
ConfusionMatrix <- table(round(PredNL2),testing_PCA$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(confusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train_PCA[Fold==i,]
  train_cv <- Train_PCA[Fold!=i,]
  # build model
  NN <- nnet(default.payment.next.month ~ ., data = train_cv, rang = 0.1, decay = 5e-4 , size = 3, maxit = 1000)
  pred <- predict(NN,newdata=validation_cv,type="class")
  tab<-table(pred,validation_cv$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_NL_PCA <- mean(Accuracy_vector)
Accuracy_NL_PCA




######## Random Forest ########


RF <- randomForest(default.payment.next.month~.,data=training,ntree = 100, mtry = 2 )
print(RF)

#Prediction
PredRF <- predict(RF,testing)

#Confusion matrix
ConfusionMatrix <- table(round(PredRF),testing$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(ConfusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 20, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train[Fold==i,]
  train_cv <- Train[Fold!=i,]
  # build model
  randF <- randomForest(default.payment.next.month~.,data=train_cv,ntree = 100, mtry = 2 )
  pred <- predict(randF,validation_cv)
  cm <- table((pred),validation_cv$default.payment.next.month)
  AV=(cm[1,1]+cm[2,2])/sum(cm)
  Accuracy_vector[i] <- AV
}
Accuracy_RF <- mean(Accuracy_vector)
Accuracy_RF



######## Random Forest with feature selection ########


RF <- randomForest(default.payment.next.month~.,data=training_Selec,ntree = 100, mtry = 2 )
print(RF)

#Prediction
PredRF <- predict(RF,testing_Selec)

#Confusion matrix
ConfusionMatrix <- table(round(PredRF),testing_Selec$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(ConfusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 20, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train_Selection[Fold==i,]
  train_cv <- Train_Selection[Fold!=i,]
  # build model
  randF <- randomForest(default.payment.next.month~.,data=train_cv,ntree = 100, mtry = 2 )
  pred <- predict(randF,validation_cv)
  cm <- table((pred),validation_cv$default.payment.next.month)
  AV=(cm[1,1]+cm[2,2])/sum(cm)
  Accuracy_vector[i] <- AV
}
Accuracy_RF_Selec <- mean(Accuracy_vector)
Accuracy_RF_Selec




######## Random Forest with feature extraction ########


RF <- randomForest(default.payment.next.month~.,data=training_PCA,ntree = 100, mtry = 2 )
print(RF)

#Prediction
PredRF <- predict(RF,testing_PCA)

#Confusion matrix
ConfusionMatrix <- table(round(PredRF),testing_PCA$default.payment.next.month)
ConfusionMatrix

#Accuracy
Accuracy=(ConfusionMatrix[1,1]+ConfusionMatrix[2,2])/sum(ConfusionMatrix)
Accuracy


#cross validation 
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 20, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train_PCA[Fold==i,]
  train_cv <- Train_PCA[Fold!=i,]
  # build model
  randF <- randomForest(default.payment.next.month~.,data=train_cv,ntree = 100, mtry = 2 )
  pred <- predict(randF,validation_cv)
  cm <- table((pred),validation_cv$default.payment.next.month)
  AV=(cm[1,1]+cm[2,2])/sum(cm)
  Accuracy_vector[i] <- AV
}
Accuracy_RF_PCA <- mean(Accuracy_vector)
Accuracy_RF_PCA



######## Nearest Neighbour ########


install.packages("ISLR")
install.packages("ggplot2") 
install.packages("plyr")
install.packages("dplyr") 
install.packages("class")

library(ISLR) 
library(ggplot2) 
library(reshape2) 
library(plyr) 
library(dplyr) 
library(class)
library(caret)

#scale 
Train$LIMIT_BAL<-scale(Train$LIMIT_BAL)
Train$AGE<-scale(Train$AGE)
Train$CHILDREN<-scale(Train$CHILDREN)
Train$BILL_AMT1<-scale(Train$BILL_AMT1)
Train$BILL_AMT2<-scale(Train$BILL_AMT2)
Train$BILL_AMT3<-scale(Train$BILL_AMT3)
Train$BILL_AMT4<-scale(Train$BILL_AMT4)
Train$BILL_AMT5<-scale(Train$BILL_AMT5)
Train$BILL_AMT6<-scale(Train$BILL_AMT6)
Train$PAY_AMT1<-scale(Train$PAY_AMT1)
Train$PAY_AMT2<-scale(Train$PAY_AMT2)
Train$PAY_AMT3<-scale(Train$PAY_AMT3)
Train$PAY_AMT4<-scale(Train$PAY_AMT4)
Train$PAY_AMT5<-scale(Train$PAY_AMT5)
Train$PAY_AMT6<-scale(Train$PAY_AMT6)

set.seed(100)
train_ind <- createDataPartition(Train$default.payment.next.month, p=0.5, list=FALSE)
Trainset <- Train[train_ind, ]
validationset <- Train[-train_ind, ]

TraintargetT<-Train[train_ind, 25]
TraintargetV<-Train[-train_ind, 25]

##run knn function
pr <- knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=32)

##create confusion matrix
tab <- table(pr,TraintargetV)
tab

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#find the optimum value of k 
i=1                          # declaration to initiate for loop
k.optm=1                # declaration to initiate for loop
for (i in 1:100){ 
  knn.mod <-  knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=i)
  k.optm[i] <- 100 * sum(validationset[,25] == knn.mod)/NROW(validationset[,25])
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}
# the best K is 32
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")##


#cross validation 
set.seed(100)
Fold <- createFolds(Train$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train[Fold==i,]
  train_cv <- Train[Fold!=i,]
  target<-Train[Fold==i,25]
  # build model
  knearestN <- knn(train_cv[,-25],validation_cv[,-25],cl=train_cv[,25],k=32)
  tab <- table(knearestN,validation_cv$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_CV <- mean(Accuracy_vector)
Accuracy_CV





######## K nearest neighbours with feature selection ########


set.seed(100)
train_ind <- createDataPartition(Train$default.payment.next.month, p=0.5, list=FALSE)
Trainset <-training_Selec
validationset <- testing_Selec

TraintargetT<-Train[train_ind, 25]
TraintargetV<-Train[-train_ind, 25]

##run knn function
pr <- knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=32)

##create confusion matrix
tab <- table(pr,validationset$default.payment.next.month)
tab

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#find the optimum value of k 
i=1                          # declaration to initiate for loop
k.optm=1                # declaration to initiate for loop
for (i in 1:100){ 
  knn.mod <-  knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=i)
  k.optm[i] <- 100 * sum(validationset[,25] == knn.mod)/NROW(validationset[,25])
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}
# the best K is 32
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")##


#cross validation 
set.seed(100)
Fold <- createFolds(Train_Selection$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train[Fold==i,]
  train_cv <- Train[Fold!=i,]
  target<-Train[Fold==i,25]
  # build model
  knearestN <- knn(train_cv[,-25],validation_cv[,-25],cl=train_cv[,25],k=32)
  tab <- table(knearestN,validation_cv$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_CV <- mean(Accuracy_vector)
Accuracy_CV




######## K nearest neighbours with feature extraction ########


set.seed(100)
train_ind <- createDataPartition(Train$default.payment.next.month, p=0.5, list=FALSE)
Trainset <- training_PCA
validationset <- testing_PCA

TraintargetT<-Train[train_ind, 25]
TraintargetV<-Train[-train_ind, 25]

##run knn function
pr <- knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=32)

##create confusion matrix
tab <- table(pr,validationset$default.payment.next.month)
tab

##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

#find the optimum value of k 
i=1                          # declaration to initiate for loop
k.optm=1                # declaration to initiate for loop
for (i in 1:100){ 
  knn.mod <-  knn(Trainset[,-25],validationset[,-25],cl=Trainset[,25],k=i)
  k.optm[i] <- 100 * sum(validationset[,25] == knn.mod)/NROW(validationset[,25])
  k=i  
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy 
}
# the best K is 32
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")##


#cross validation 
set.seed(100)
Fold <- createFolds(Train_PCA$default.payment.next.month, k = 10, list=FALSE) # split our observations in 10 folds
Accuracy_vector<-rep(0,10)

for (i in 1:10){
  # built trainset and validation set
  validation_cv <- Train[Fold==i,]
  train_cv <- Train[Fold!=i,]
  target<-Train[Fold==i,25]
  # build model
  knearestN <- knn(train_cv[,-25],validation_cv[,-25],cl=train_cv[,25],k=32)
  tab <- table(knearestN,validation_cv$default.payment.next.month)
  ac<- (tab[1,1]+tab[2,2])/sum(tab)
  Accuracy_vector[i] <- ac
}
Accuracy_CV <- mean(Accuracy_vector)
Accuracy_CV

########################################################################################
################################ Predictions ###########################################
########################################################################################



TestStudent <- read_csv("TestStudent.csv")
View(TestStudent)

# Formatting variables 
TestStudent$ID<-as.character(TestStudent$ID)
TestStudent$LIMIT_BAL<-as.numeric(TestStudent$LIMIT_BAL)
TestStudent$SEX<-as.factor(TestStudent$SEX)
TestStudent$EDUCATION<-as.factor(TestStudent$EDUCATION)
TestStudent$MARRIAGE<-as.factor(TestStudent$MARRIAGE)
TestStudent$AGE<-as.numeric(TestStudent$AGE)
TestStudent$CHILDREN<-as.numeric(TestStudent$CHILDREN)
TestStudent$PAY_1<-as.factor(TestStudent$PAY_1)
TestStudent$PAY_2<-as.factor(TestStudent$PAY_2)
TestStudent$PAY_3<-as.factor(TestStudent$PAY_3)
TestStudent$PAY_4<-as.factor(TestStudent$PAY_4)
TestStudent$PAY_5<-as.factor(TestStudent$PAY_5)
TestStudent$PAY_6<-as.factor(TestStudent$PAY_6)
TestStudent$BILL_AMT1<-as.numeric(TestStudent$BILL_AMT1)
TestStudent$BILL_AMT2<-as.numeric(TestStudent$BILL_AMT2)
TestStudent$BILL_AMT3<-as.numeric(TestStudent$BILL_AMT3)
TestStudent$BILL_AMT4<-as.numeric(TestStudent$BILL_AMT4)
TestStudent$BILL_AMT5<-as.numeric(TestStudent$BILL_AMT5)
TestStudent$BILL_AMT6<-as.numeric(TestStudent$BILL_AMT6)
TestStudent$PAY_AMT1<-as.numeric(TestStudent$PAY_AMT1)
TestStudent$PAY_AMT2<-as.numeric(TestStudent$PAY_AMT2)
TestStudent$PAY_AMT3<-as.numeric(TestStudent$PAY_AMT3)
TestStudent$PAY_AMT4<-as.numeric(TestStudent$PAY_AMT4)
TestStudent$PAY_AMT5<-as.numeric(TestStudent$PAY_AMT5)
TestStudent$PAY_AMT6<-as.numeric(TestStudent$PAY_AMT6)

#Remplace empty value

#preparation des données pour la moyenne.
table1 <- table(TestStudent$SEX)
table2 <- table(TestStudent$EDUCATION)
table3 <- table(TestStudent$MARRIAGE)

#Delete id 
TestStudent<-TestStudent[,-1]

#Change Na by most reccurent categorical value
TestStudent$SEX[is.na(TestStudent$SEX)] <- names(table1)[which(table1==max(table1))]
TestStudent$EDUCATION[is.na(TestStudent$EDUCATION)] <- names(table2)[which(table2==max(table2))]
TestStudent$MARRIAGE[is.na(TestStudent$MARRIAGE)] <-names(table3)[which(table3==max(table3))]
TestStudent$AGE[which(is.na(TestStudent$AGE))]<-round(mean(TestStudent$AGE,na.rm=TRUE))
TestStudent$CHILDREN[which(is.na(TestStudent$CHILDREN))]<-round(mean(TestStudent$CHILDREN,na.rm=TRUE))

#Replace  0 in Marriage variable
TestStudent$MARRIAGE[TestStudent$MARRIAGE==0]<-names(table3)[which(table3==max(table3))]

View(TestStudent)



#Same levels
levels(TestStudent$PAY_3) = levels(train$PAY_3)
levels(TestStudent$PAY_5) = levels(train$PAY_5)


######## Random Forest ########
RdmForest <- randomForest(default.payment.next.month~.,data=Train,ntree = 100, mtry = 2 )
print(RdmForest)


#Prediction
PREDICTIONS <- predict(RdmForest,TestStudent)
table(PREDICTIONS)
write.csv(PREDICTIONS, file = "LLSMF2013-32-predictions.CSV", row.names = FALSE)



########################################################################################
################################ Sources ###############################################
########################################################################################

#https://www.lovelyanalytics.com/2017/08/29/random-forest-tutoriel-r/
#http://intelligency.org/ai_r.php
#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_Packages_R_for_Deep_Learning.pdf
#https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/
# https://towardsdatascience.com/beginners-guide-to-k-nearest-neighbors-in-r-from-zero-to-hero-d92cd4074bdb
#https://towardsdatascience.com/k-nearest-neighbors-algorithm-with-examples-in-r-simply-explained-knn-1f2c88da405c
#https://www.rdocumentation.org/


