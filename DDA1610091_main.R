## Loading the neccessary packages 

library(ggplot2)
library(caret)
library(car)
library(caTools)
library(ROCR)
library(MASS)

# CheckPoint 1:Data Understanding and Exploration

# Importing the data in to R Environment and storing it in german_credit variable
german_credit<-read.csv("german.csv",stringsAsFactors = F)

# Exploration of Data

# Structure of the dataset
str(german_credit)

# Summary of the dataset
summary(german_credit)

##univariate Analysis

##1.Credit History

str(factor(german_credit$Credit.history))

summary(factor(german_credit$Credit.history))

ggplot(german_credit, aes(x = Credit.history, fill = factor(Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label= ..count..), geom = "text", size = 4.5, position=position_dodge(width = 1),vjust = 1)

##2 Purpose of the Loan 

str(factor(german_credit$Purpose))

summary(factor(german_credit$Purpose))

ggplot(german_credit, aes(x = Purpose, fill =factor(Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label= ..count..), geom = "text", size = 4.5,colour="Black", position=position_dodge(width = 1),vjust = 1)


##3. Age in Years

# stucture of the Age variable
str(german_credit$Age.in.Years)

# summary of the Age variable
summary(german_credit$Age.in.Years)

#mean of the Age variable
mean(german_credit$Age.in.Years)

#median of the Age variable
median(german_credit$Age.in.Years)

#Variance of the Age variable
var(german_credit$Age.in.Years)

# Standard Deviance of the Age variable
sd(german_credit$Age.in.Years)

# Skewness of the Age Variable
library(moments)
skewness(german_credit$Age.in.Years)
#Skewness is positive which indicates distribution is positively skewed


#Kurtosis of the Age variable
kurtosis(german_credit$Age.in.Years)
#Kurtosis is greater than 3 which indicates the heavy tail

# percentile distribution of the Age Variable
quantile(german_credit$Age.in.Years,probs = seq(0,1,0.01))

##Histogram of the Age Variable to see the distribution 
ggplot(german_credit,aes(Age.in.Years))+geom_histogram( binwidth = 2, fill = "darkgreen")

## Box plot to see the outliers
boxplot(german_credit$Age.in.Years)

## 4 Housing

str(factor(german_credit$Housing.))

summary(factor(german_credit$Housing.))

ggplot(german_credit, aes(x =Housing., fill =factor(Default_status))) + geom_bar(position = "dodge") + stat_count(aes(label= ..count..), geom = "text", size = 4.5,colour="Black", position=position_dodge(width = 1),vjust = 1)



## 5 Credit Amount

##Structure of the credit amount
str(german_credit$Credit.amount)

##Summary of the Credit amount
summary(german_credit$Credit.amount)

##Mean of the Credit amount
mean(german_credit$Credit.amount)

##Median of the Credit amount
median(german_credit$Credit.amount)

##Variance of the Credit amount
var(german_credit$Credit.amount)

##Stanard Deviance of the Credit amount
sd(german_credit$Credit.amount)

##Skewness of the Credit amount
skewness(german_credit$Credit.amount)
#Skewness is positive which indicates distribution is positively skewed

##Kurtosis of the Credit amount
kurtosis(german_credit$Credit.amount)
#Kurtosis is greater than 3 which indicates the heavy tail

##Percentile distribution of the Credit amount
quantile(german_credit$Credit.amount,probs = seq(0,1,0.01))

##Histogram of the Credit amount to check the distribution
ggplot(german_credit,aes(x=Credit.amount))+geom_histogram(binwidth = 500,fill="brown")

##Boxplot of the Credit amount to check the outliers
boxplot(german_credit$Credit.amount)


##### Data Preparation and Transformation

##Structure of the dataset
str(german_credit)


## Check NA values first for all the variables in the data set.

sum(is.na(german_credit))

## There are no NA values in the dataset so now we proceed with 

## Convert all the character variables to factors
german_credit$Status.of.existing.checking.account<-as.factor(german_credit$Status.of.existing.checking.account)
german_credit$Credit.history<-as.factor(german_credit$Credit.history)
german_credit$Purpose<-as.factor(german_credit$Purpose)
german_credit$Savings.account.bonds<-as.factor(german_credit$Savings.account.bonds)
german_credit$Present.employment.since.<-as.factor(german_credit$Present.employment.since.)
german_credit$Personal.status.and.sex<-as.factor(german_credit$Personal.status.and.sex)
german_credit$Other.debtors...guarantors<-as.factor(german_credit$Other.debtors...guarantors)
german_credit$Property<-as.factor(german_credit$Property)
german_credit$Other.installment.plans<-as.factor(german_credit$Other.installment.plans)
german_credit$Housing.<-as.factor(german_credit$Housing.)
german_credit$Job_status<-as.factor(german_credit$Job_status)
german_credit$Telephone.<-as.factor(german_credit$Telephone.)
german_credit$foreign.worker<-as.factor(german_credit$foreign.worker)

## There are integer variables which should be factors
german_credit$Installment.rate.in.percentage.of.disposable.income<-as.factor(german_credit$Installment.rate.in.percentage.of.disposable.income)
german_credit$Number.of.existing.credits.at.this.bank.<-as.factor(german_credit$Number.of.existing.credits.at.this.bank.)
german_credit$Number.of.people.being.liable.to.provide.maintenance.for.<-as.factor(german_credit$Number.of.people.being.liable.to.provide.maintenance.for.)
german_credit$Default_status<-as.factor(german_credit$Default_status)
german_credit$Present.residence.since<-as.factor(german_credit$Present.residence.since)


##Check the structure of the dataset after converting all the variables
str(german_credit)


## checking for the outliers

## 1.Credit amount

boxplot(german_credit$Credit.amount)

quantile(german_credit$Credit.amount, probs = seq(0.8,1,0.01))

german_credit$Credit.amount[german_credit$Credit.amount>7687.95] <- 7687.95

german_credit$Credit.amount <- scale(german_credit$Credit.amount)

## 2.Age in Years

boxplot(german_credit$Age.in.Years)

quantile(german_credit$Age.in.Years, probs = seq(0.5,1,0.01))

german_credit$Age.in.Years[german_credit$Age.in.Years > 60.00] <-60.00

german_credit$Age.in.Years <- scale(german_credit$Age.in.Years)

## 3.Duration.in.month

summary(german_credit$Duration.in.month)

str(german_credit$Duration.in.month)

boxplot(german_credit$Duration.in.month)

quantile(german_credit$Duration.in.month, probs = seq(0.7,1, 0.01))

german_credit$Duration.in.month[german_credit$Duration.in.month > 42.00 ] <- 42.00

## Now we check the boxplot again to see if outliers have been removed

boxplot(german_credit$Duration.in.month)

## Now since the outliers have been removed we proceed with scaling

german_credit$Duration.in.month <- scale(german_credit$Duration.in.month)


# Create Dummy Variables

# Create dummy variable for Status of Existing Account
dummy_1<-data.frame(model.matrix(~Status.of.existing.checking.account,german_credit))[-1]

# Create dummy variable for Credit History
dummy_2<-data.frame(model.matrix(~Credit.history,german_credit))[-1]

# Create dummy variable for Purpose
dummy_3<-data.frame(model.matrix(~Purpose,german_credit))[-1]

# Create dummy variable for savings Account Bonds
dummy_4<-data.frame(model.matrix(~Savings.account.bonds,german_credit))[-1]

# Create dummy variable for Present Employment 
dummy_5<-data.frame(model.matrix(~Present.employment.since.,german_credit))[-1]

# Create dummy variable for Installment Rate
dummy_6<-data.frame(model.matrix(~Installment.rate.in.percentage.of.disposable.income,german_credit))[-1]

# Create dummy variable for Personal Status
dummy_7<-data.frame(model.matrix(~Personal.status.and.sex,german_credit))[-1]

# Create dummy variable for other debtors
dummy_8<-data.frame(model.matrix(~Other.debtors...guarantors,german_credit))[-1]

# Create dummy variable for Present Residence
dummy_9<-data.frame(model.matrix(~Present.residence.since,german_credit))[-1]

# Create dummy variable for Property
dummy_10<-data.frame(model.matrix(~Property,german_credit))[-1]

# Create dummy variable for Other Installment Plans
dummy_11<-data.frame(model.matrix(~Other.installment.plans,german_credit))[-1]

# Create dummy variable for Housing
dummy_12<-data.frame(model.matrix(~Housing.,german_credit))[-1]

# Create dummy variable for Number of Existing Credits at this bank
dummy_13<-data.frame(model.matrix(~Number.of.existing.credits.at.this.bank.-1,german_credit))[-1]

# Create dummy variable for Job Status
dummy_14<-data.frame(model.matrix(~Job_status,german_credit))[-1]

# Create dummy variable for Number of people being liable to provide maintenance
dummy_15<-data.frame(model.matrix(~Number.of.people.being.liable.to.provide.maintenance.for.-1,german_credit))[-1]

# Create dummy variable for Telephone
dummy_16<-data.frame(model.matrix(~Telephone.,data = german_credit))[-1]

# Create dummy variable for Foreign Worker
dummy_17<-data.frame(model.matrix(~foreign.worker,data = german_credit))[-1]

german_credit.numeric<-german_credit[c(2,5,13,21)]

str(german_credit)

## Binding all the dummy variables in to the german_credit dataframe
german_credit1<-cbind(german_credit.numeric,dummy_1,dummy_2,dummy_3,dummy_4,dummy_5,dummy_6,dummy_7,
                     dummy_8,dummy_9,dummy_10,dummy_11,dummy_12,dummy_13,dummy_14,dummy_15,dummy_16,dummy_17 )


##############Splitting the Dataset into train and test########

set.seed(100)
s=sample(1:nrow(german_credit1),0.7*nrow(german_credit1))
german_train = german_credit1[s,]
german_test = german_credit1[-s,]


##### Modelling
model_1 <- glm(Default_status~., family = binomial, data = german_train)
summary(model_1)


# Stepwise selection of variables
step<-stepAIC(model_1,direction = "both")

step

model_2<-glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Installment.rate.in.percentage.of.disposable.income2 + Installment.rate.in.percentage.of.disposable.income3 + 
               Installment.rate.in.percentage.of.disposable.income4 + Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143 + 
               Telephone.A192, family = binomial, data = german_train)
summary(model_2)
vif(model_2)

## Removing Installment.rate.in.percentage.of.disposable.income4
model_3<-glm(formula = Default_status ~ Duration.in.month + Credit.amount + 
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Installment.rate.in.percentage.of.disposable.income2 + Installment.rate.in.percentage.of.disposable.income3 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143 + 
               Telephone.A192, family = binomial, data = german_train)
summary(model_3)
vif(model_3)

## Removing Credit.amount 
model_4<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Installment.rate.in.percentage.of.disposable.income2 + Installment.rate.in.percentage.of.disposable.income3 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143 + 
               Telephone.A192, family = binomial, data = german_train)
summary(model_4)
vif(model_4)
## VIF values are less than the threshold value 
## We will remove values based on the insiginficance

## Removing Installment.rate.in.percentage.of.disposable.income2 
model_5<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
                Installment.rate.in.percentage.of.disposable.income3 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143 + 
               Telephone.A192, family = binomial, data = german_train)
summary(model_5)

## Removing Installment.rate.in.percentage.of.disposable.income3 
model_6<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143 + 
               Telephone.A192, family = binomial, data = german_train)
summary(model_6)

## Removing Telephone.A192 
model_7<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA41 + PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143  
               , family = binomial, data = german_train)
summary(model_7)

## Removing Purpose 41 
model_8<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
                PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Personal.status.and.sexA93 + 
               Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + PropertyA124 + Other.installment.plansA143  
             , family = binomial, data = german_train)
summary(model_8)

## Removing PropertyA124 
model_9<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA43 + PurposeA46 + Savings.account.bondsA64 + 
               Savings.account.bondsA65 + Present.employment.since.A74 + 
               Personal.status.and.sexA93 + Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + Other.installment.plansA143  
             , family = binomial, data = german_train)
summary(model_9)

## Removing  Savings.account.bondsA64 
model_10<-glm(formula = Default_status ~ Duration.in.month +  
               Status.of.existing.checking.accountA12 + Status.of.existing.checking.accountA13 + 
               Status.of.existing.checking.accountA14 + Credit.historyA34 + 
               PurposeA43 + PurposeA46 + Savings.account.bondsA65 + Present.employment.since.A74 + 
               Personal.status.and.sexA93 + Other.debtors...guarantorsA102 + Other.debtors...guarantorsA103 + 
               Present.residence.since2 + Other.installment.plansA143  
             , family = binomial, data = german_train)
summary(model_10)


## C-statistic
library(Hmisc)

german_train$predicted_prob = predict(model_10,  type = "response")
## C-statistic for train data
rcorr.cens(german_train$predicted_prob,german_train$Default_status) 

german_test$predicted_prob = predict(model_10, newdata = german_test,type = "response")
rcorr.cens(german_test$predicted_prob,german_test$Default_status)

#KS-statistic


model_score <- prediction(german_train$predicted_prob, german_train$Default_status)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf,"y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks <- max(ks_table)

ks

which(ks_table == ks)

##decile

which(ks_table == ks)/nrow(german_train)

## Maximum Ks statistics is in 4th decile(0.34) for train data

model_score_test <- prediction(german_test$predicted_prob,german_test$Default_status)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks_test<-max(ks_table_test)

which(ks_table_test == ks_test)

which(ks_table_test == ks_test)/nrow(german_test)

## Maximum Ks statistics is in 4th decile(0.343) for test data


# plottig ROC curve to decide the threshold level
plot(model_perf, colorize = T,print.cutoffs.at=seq(0,1, by = 0.1))

abline(0,1)
#confusion matrix 
## To minimize the false negatives i.e to have more sensitivity  I have set my threshold level as 0.3
library(caret)


## Confusion matrix for Train data
confusionMatrix(as.numeric(german_train$predicted_prob > 0.3),german_train$Default_status,positive = "1")
#Accuracy : 0.7343
#Sensitivity : 0.8026          
#Specificity : 0.7013 

## Confusion matrix for Test data
confusionMatrix(as.numeric(german_test$predicted_prob > 0.3),german_test$Default_status,positive = "1")
#Accuracy : 0.6833
#Sensitivity : 0.7500          
#Specificity : 0.6623  