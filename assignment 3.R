#Libraries
require(dplyr)
require(tidyr)
require(ggplot2)
require(bigmemory)
require(biganalytics)

library(dplyr)
library(tidyr)
library(ggplot2)
library(pastecs)
library(bigmemory)
library(biganalytics)

#Import Data from CSV
gcd <- read.csv("C:\\Users\\vincenttanms\\Documents\\DSCT Participants\\Assignment 3\\DSCT-Logistic-Regression-master\\german_credit_data.csv", header = TRUE, sep=";")

# Data Exploration

#Check for each variable how many have missing records
sapply(gcd, function(x) sum(is.na(x)))

#Check for each variable how many unique values are there
sapply(gcd, function(x) length(unique(x)))

# Summary of the Data
# view a summary of the raw data
head(gcd)

# looking at summary statistics for the entire data set
summary(gcd)

# looking at summary statistics for srch_adult_cnt and srch_children_cnt
summary(gcd[,c("srch_adults_cnt","srch_children_cnt")])

# using the pastecs package to generate descriptive statistics
stat.desc(gcd)

#Clean data

#Remove outliers
gcd <- subset(gcd, Personal != 7 )
gcd <- subset(gcd,Years.with.present.employer != 9)
gcd <- subset(gcd, Credit.asked.for..X.100. != 0)
gcd <- subset(gcd, Other.installment.plans != 4)

#Transform Catergorical data
#The contrast specifies how the levels of the factors will be coded into a family
# of numeric dummy variables
gcd$Status.of.account <- as.factor(gcd$Status.of.account)
levels(gcd$Status.of.account)

gcd$Credit.history <- as.factor(gcd$Credit.history)
levels(gcd$Credit.history)

gcd$purpose.of.loan <- as.factor(gcd$purpose.of.loan)
levels(gcd$purpose.of.loan)

gcd$Savings.account <- as.factor(gcd$Savings.account)
levels(gcd$Savings.account)

gcd$Years.with.present.employer <- as.factor(gcd$Years.with.present.employer)
levels(gcd$Years.with.present.employer)

gcd$Personal <- as.factor(gcd$Personal)
levels(gcd$Personal)

gcd$other.debtors.guarantors <- as.factor(gcd$other.debtors.guarantors)
levels(gcd$other.debtors.guarantors)

gcd$Assets <- as.factor(gcd$Assets)
levels(gcd$Assets)

gcd$Other.installment.plans <- as.factor(gcd$Other.installment.plans)
levels(gcd$Other.installment.plans)

gcd$Housing <- as.factor(gcd$Housing)
levels(gcd$Housing)

gcd$Job.Classification <- as.factor(gcd$Job.Classification)
levels(gcd$Job.Classification)


#The contrast specifies how the levels of the factors will be coded into a family
# of numeric dummy variables
contrasts(gcd$Status.of.account)
contrasts(gcd$Credit.history)
contrasts(gcd$purpose.of.loan)
contrasts(gcd$Savings.account)
contrasts(gcd$Years.with.present.employer)
contrasts(gcd$Personal)
contrasts(gcd$other.debtors.guarantors)
contrasts(gcd$Assets)
contrasts(gcd$Other.installment.plans)
contrasts(gcd$Housing)
contrasts(gcd$Job.Classification)

ngcd <- data.frame(gcd$Loan.Duration,
                   gcd$Credit.asked.for..X.100.,
                   gcd$Age,
                   gcd$No.of.dependents,
                   gcd$Credit.offered.,
                   gcd$Have.telephone,
                   gcd$Foreign.Worker)

#Transformation on Status.of.account
ngcd$Status.of.account.1[gcd$Status.of.account != 1] <- 1
ngcd$Status.of.account.1[gcd$Status.of.account == 1] <- 2

ngcd$Status.of.account.2[gcd$Status.of.account != 2] <- 1
ngcd$Status.of.account.2[gcd$Status.of.account == 2] <- 2

ngcd$Status.of.account.3[gcd$Status.of.account != 3] <- 1
ngcd$Status.of.account.3[gcd$Status.of.account == 3] <- 2

ngcd$Status.of.account.4[gcd$Status.of.account != 4] <- 1
ngcd$Status.of.account.4[gcd$Status.of.account == 4] <- 2

#Transformation on Credit.history
ngcd$Credit.history.0[gcd$Credit.history != 1] <- 1
ngcd$Credit.history.0[gcd$Credit.history == 1] <- 2

ngcd$Credit.history.1[gcd$Credit.history != 2] <- 1
ngcd$Credit.history.1[gcd$Credit.history == 2] <- 2

ngcd$Credit.history.2[gcd$Credit.history != 3] <- 1
ngcd$Credit.history.2[gcd$Credit.history == 3] <- 2

ngcd$Credit.history.3[gcd$Credit.history != 4] <- 1
ngcd$Credit.history.3[gcd$Credit.history == 4] <- 2

ngcd$Credit.history.4[gcd$Credit.history != 4] <- 1
ngcd$Credit.history.4[gcd$Credit.history == 4] <- 2

#Transformation on purpose.of.loan
ngcd$purpose.of.loan.0[gcd$purpose.of.loan != 0] <- 1
ngcd$purpose.of.loan.0[gcd$purpose.of.loan == 0] <- 2

ngcd$purpose.of.loan.1[gcd$purpose.of.loan != 1] <- 1
ngcd$purpose.of.loan.1[gcd$purpose.of.loan == 1] <- 2

ngcd$purpose.of.loan.2[gcd$purpose.of.loan != 2] <- 1
ngcd$purpose.of.loan.2[gcd$purpose.of.loan == 2] <- 2

ngcd$purpose.of.loan.3[gcd$purpose.of.loan != 3] <- 1
ngcd$purpose.of.loan.3[gcd$purpose.of.loan == 3] <- 2

ngcd$purpose.of.loan.4[gcd$purpose.of.loan != 4] <- 1
ngcd$purpose.of.loan.4[gcd$purpose.of.loan == 4] <- 2

ngcd$purpose.of.loan.5[gcd$purpose.of.loan != 5] <- 1
ngcd$purpose.of.loan.5[gcd$purpose.of.loan == 5] <- 2

ngcd$purpose.of.loan.6[gcd$purpose.of.loan != 6] <- 1
ngcd$purpose.of.loan.6[gcd$purpose.of.loan == 6] <- 2

ngcd$purpose.of.loan.8[gcd$purpose.of.loan != 8] <- 1
ngcd$purpose.of.loan.8[gcd$purpose.of.loan == 8] <- 2

ngcd$purpose.of.loan.9[gcd$purpose.of.loan != 9] <- 1
ngcd$purpose.of.loan.9[gcd$purpose.of.loan == 9] <- 2

ngcd$purpose.of.loan.10[gcd$purpose.of.loan != 10] <- 1
ngcd$purpose.of.loan.10[gcd$purpose.of.loan == 10] <- 2

#Transformation on Savings.account
ngcd$Savings.account.1[gcd$Savings.account != 1] <- 1
ngcd$Savings.account.1[gcd$Savings.account == 1] <- 2

ngcd$Savings.account.2[gcd$Savings.account != 2] <- 1
ngcd$Savings.account.2[gcd$Savings.account == 2] <- 2

ngcd$Savings.account.3[gcd$Savings.account != 3] <- 1
ngcd$Savings.account.3[gcd$Savings.account == 3] <- 2

ngcd$Savings.account.4[gcd$Savings.account != 4] <- 1
ngcd$Savings.account.4[gcd$Savings.account == 4] <- 2

ngcd$Savings.account.5[gcd$Savings.account != 5] <- 1
ngcd$Savings.account.5[gcd$Savings.account == 5] <- 2

#Transformation on Years.with.present.employer
ngcd$Years.with.present.employer.1[gcd$Years.with.present.employer != 1] <- 1
ngcd$Years.with.present.employer.1[gcd$Years.with.present.employer == 1] <- 2

ngcd$Years.with.present.employer.2[gcd$Years.with.present.employer != 2] <- 1
ngcd$Years.with.present.employer.2[gcd$Years.with.present.employer == 2] <- 2

ngcd$Years.with.present.employer.3[gcd$Years.with.present.employer != 3] <- 1
ngcd$Years.with.present.employer.3[gcd$Years.with.present.employer == 3] <- 2

ngcd$Years.with.present.employer.4[gcd$Years.with.present.employer != 4] <- 1
ngcd$Years.with.present.employer.4[gcd$Years.with.present.employer == 4] <- 2

ngcd$Years.with.present.employer.5[gcd$Years.with.present.employer != 5] <- 1
ngcd$Years.with.present.employer.5[gcd$Years.with.present.employer == 5] <- 2

#Transformation on Personal
gcd$sex[gcd$Personal == 1] <- 1
gcd$sex[gcd$Personal == 2] <- 2
gcd$sex[gcd$Personal == 3] <- 1
gcd$sex[gcd$Personal == 4] <- 1

gcd$status[gcd$Personal == 1] <- 2
gcd$status[gcd$Personal == 2]<-2
gcd$status[gcd$Personal == 3] <- 1
gcd$status[gcd$Personal == 4]<-2

#Transformation on other.debtors.guarantors
ngcd$other.debtors.guarantors.1[gcd$other.debtors.guarantors != 1] <- 1
ngcd$other.debtors.guarantors.1[gcd$other.debtors.guarantors == 1] <- 2

ngcd$other.debtors.guarantors.2[gcd$other.debtors.guarantors != 2] <- 1
ngcd$other.debtors.guarantors.2[gcd$other.debtors.guarantors == 2] <- 2

ngcd$other.debtors.guarantors.3[gcd$other.debtors.guarantors != 3] <- 1
ngcd$other.debtors.guarantors.3[gcd$other.debtors.guarantors == 3] <- 2

#Transformation on Assets
ngcd$Assets.1[gcd$Assets != 1] <- 1
ngcd$Assets.1[gcd$Assets == 1] <- 2

ngcd$Assets.2[gcd$Assets != 2] <- 1
ngcd$Assets.2[gcd$Assets == 2] <- 2

ngcd$Assets.3[gcd$Assets != 3] <- 1
ngcd$Assets.3[gcd$Assets == 3] <- 2

ngcd$Assets.4[gcd$Assets != 4] <- 1
ngcd$Assets.4[gcd$Assets == 4] <- 2

#Transformation on Other.installment.plans
ngcd$Other.installment.plans.1[gcd$Other.installment.plans != 1] <- 1
ngcd$Other.installment.plans.1[gcd$Other.installment.plans == 1] <- 2

ngcd$Other.installment.plans.2[gcd$Other.installment.plans != 2] <- 1
ngcd$Other.installment.plans.2[gcd$Other.installment.plans == 2] <- 2

ngcd$Other.installment.plans.3[gcd$Other.installment.plans != 3] <- 1
ngcd$Other.installment.plans.3[gcd$Other.installment.plans == 3] <- 2

#Transformation on Housing
ngcd$Housing.1[gcd$Housing != 1] <- 1
ngcd$Housing.1[gcd$Housing == 1] <- 2

ngcd$Housing.2[gcd$Housing != 2] <- 1
ngcd$Housing.2[gcd$Housing == 2] <- 2

ngcd$Housing.3[gcd$Housing != 3] <- 1
ngcd$Housing.3[gcd$Housing == 3] <- 2

#Transformation on Job.Classification
ngcd$Job.Classification.1[gcd$Job.Classification != 1] <- 1
ngcd$Job.Classification.1[gcd$Job.Classification == 1] <- 2

ngcd$Job.Classification.2[gcd$Job.Classification != 2] <- 1
ngcd$Job.Classification.2[gcd$Job.Classification == 2] <- 2

ngcd$Job.Classification.3[gcd$Job.Classification != 3] <- 1
ngcd$Job.Classification.3[gcd$Job.Classification == 3] <- 2

ngcd$Job.Classification.4[gcd$Job.Classification != 4] <- 1
ngcd$Job.Classification.4[gcd$Job.Classification == 4] <- 2



#Transformation on purpose.of.loan
gcd$purpose.of.loan.0[gcd$purpose.of.loan == 0] <- 1
gcd$purpose.of.loan.1[gcd$purpose.of.loan == 1] <- 1
gcd$purpose.of.loan.2[gcd$purpose.of.loan == 2] <- 2
gcd$purpose.of.loan.3[gcd$purpose.of.loan == 3] <- 2
gcd$purpose.of.loan.4[gcd$purpose.of.loan == 4] <- 2
gcd$purpose.of.loan.5[gcd$purpose.of.loan == 5] <- 2
gcd$purpose.of.loan.6[gcd$purpose.of.loan == 6] <- 2
gcd$purpose.of.loan.8[gcd$purpose.of.loan == 8] <- 3
gcd$purpose.of.loan.9[gcd$purpose.of.loan == 9] <- 3
gcd$purpose.of.loan.10[gcd$purpose.of.loan == 10] <- 4

purpose.of.loanMatrix <- model.matrix(~ purpose.of.loan -1, data = gcd)


gcd$Credit.offered.[gcd$Credit.offered. == 1] <- 0
gcd$Credit.offered.[gcd$Credit.offered. == 2] <- 1


# IMPORTNAT to scale first before doing cluster analysis
sgcd <- as.data.frame(scale(gcd))

#Check for each variable how many have missing records
sapply(sgcd, function(x) sum(is.na(x)))

# using standard k-means in R (scaled)
library(ggfortify)
km <- kmeans(sgcd, 2, iter.max = 25, nstart = 5, algorithm="Hartigan-Wong")
autoplot(km, data = sgcd)


#create a separate model training and testing set (typically 80/20)
train <- gcd[1:800,]
test <- gcd[801:1001,]

# create a logistic regression model with the training data
# target is Credit.offered., predictor variables are all values
model <- glm(Credit.offered. ~.,family=binomial(link='logit'),data=train)
# review the model
summary(model)


# odds ratios and 95% CI
exp(cbind(OR = coef(model), confint(model)))

# reviewing the model using chisq test across each predictor in sequence
# to determine the relative importance of each predictor
anova(model, test="Chisq")

# use the model to predict for cases 2 - 8 in the test set
fitted.results <- predict(model,newdata=subset(test,select=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)),type='response')
# the results of the model is a probability value and must be mapped to 1 or 0
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

# determine the mis-classification error
misClasificError <- mean(fitted.results != test$Credit.offered.)
# calculate the accuracy
print(paste('Accuracy',1-misClasificError))

library(ROCR)
pr<-prediction(fitted.results, test$Credit.offered.)
prf<-performance(pr,measure = "tpr", x.measure = "fpr")
plot(prf)

auc<- performance(pr, measure = "auc")
auc<-auc@y.values[[1]]
auc

#and then a lift chart
perf<-performance(pr, "lift","rpp")
plot(perf,main="Lift Curve",colorize=T)

