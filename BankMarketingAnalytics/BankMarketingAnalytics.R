# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Checkpoint 1 - Data preparation (no marks assigned for this step)                                                                                                #
#                You can use the code provided in the lectures to complete all data preparation steps                                                              #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Reusing code from - Data Understanding-Client Information.R

# ------------Bank Marketing Analysis---------------------#
# The standard process followed in analytics projects is:
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations

#-------------------------------------------------------
## Business Understanding:- Prospect Profiling
#-------------------------------------------------------
# Business objective is to achieve 80% of total responders at the minimum possible cost. 

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)

# Summary of dataset

summary(bank_data)

#-------------------------------------------------------

# Checking response rate of prospect customer

response <- 4640/(36548+4640)
response

# Checking missing values

sum(is.na(bank_data))

#-------------------------------------------------------

# Loading ggplot2 library
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e yes-no to 1-0

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check numeric value of response rate in each bucket 

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)


# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

# Checking structure of dataset

str(bank_data)

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

##--------------------------------------------------------  

# Checking structure of dataset 

str(bank_data)

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")



# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")


# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")

# Reusing code from - Data Understanding-Campaign Information.R

#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

# Reusing code from - Data Understanding-Economic Information.R

#-- social and economic context attributes

# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)

# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)

# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Checkpoint 2 - Build a logistic regression model without using the variable 'duration'                                                                           #
#                a) Perform variable selection using the usual methods                                                                                             #
#                b) Sort the data points in decreasing order of probability of response                                                                            #
#                c) Find the optimal probability cut-off and report the relevant evaluation metrics                                                                #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Checkpoint 2a) - Building Logistic Regression Model

# Required Packages

library(caret)
library(caTools)
library(dummies)

# Removing binning variables 
bank_data <- bank_data[, -21]

# Removing duration
#bank_data <- bank_data[, -10]

# Creating Unique ID
# prospectID
bank_data$prospectID <- seq(1, nrow(bank_data), 1)

#creating dummy variables

bank_data$response <- as.integer(bank_data$response)

k1 <- bank_data

bank_data <- dummy.data.frame(bank_data)

bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(1)

split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)

train <- bank_data[split_indices, ]

test <- bank_data[!split_indices, ]

nrow(train)/nrow(bank_data)

nrow(test)/nrow(bank_data)

### Model 1: Logistic Regression


library(MASS)

library(car)

# Removing "prospectID" and "duration" from input train data-set
logistic_1 <- glm(response ~ ., family = "binomial", data = train [, -c(45,62)])

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")

# stepAIC has removed some variables and only the following ones remain

logistic_2 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed + `previousMore than_3_times`, family = "binomial", data = train)


summary(logistic_2)
# AIC: 15982

# checking vif for logistic_2 
sort(vif(logistic_2), decreasing = TRUE)

#---------------------------------------------------------    
# removing `previousMore than_3_times` since the variable is not significant 

logistic_3 <- glm(formula = response ~ jobadmin. + jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)

summary(logistic_3)
# AIC: 15982

sort(vif(logistic_3), decreasing = TRUE)


# Removing "jobadmin." variable as not significant

logistic_4 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    maritaldivorced + educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)


summary(logistic_4)
# AIC: 15982
sort(vif(logistic_4), decreasing = TRUE)

# Removing "maritaldivorced" variable as not significant

logistic_5 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                                      educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)


summary(logistic_5)
# AIC: 15983

sort( vif(logistic_5), decreasing = TRUE)


# Removing "day_of_weekfri" as it is only insignificant variable left with p-value = 0.075743 (i.e. > 0.05)

logistic_6 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)



summary(logistic_6)
# AIC: 15984

sort( vif(logistic_6), decreasing = TRUE)

# Checking 'emp.var.rate' as it has high VIF =  139.505334

# Highly correlated with 'nr.employed' (VIF = 71.742576)
cor(train$emp.var.rate, train$nr.employed)
# [1] 0.9075266

# Removing "emp.var.rate"

logistic_7 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)


summary(logistic_7)
# AIC: 16084

sort( vif(logistic_7), decreasing = TRUE)
# VIF values are now in the range of 1 to 2.9


# Removing "monthoct" which is only insignificant variable now with p-value = 0.662958
logistic_8 <- glm(formula = response ~ jobretired + jobstudent + jobtechnician + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)


summary(logistic_8)
# AIC: 16082

sort( vif(logistic_8), decreasing = TRUE)

# Removing "jobtechnician" with p-value = 0.081816

logistic_9 <- glm(formula = response ~ jobretired + jobstudent + 
                    educationPrimary_Education + educationTertiary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.price.idx + cons.conf.idx + 
                    nr.employed , family = "binomial", data = train)

summary(logistic_9)
# AIC: 16083
sort( vif(logistic_9), decreasing = TRUE)

# Removing "educationTertiary_Education"

logistic_10 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.price.idx + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train)


summary(logistic_10)
# AIC: 16084
sort( vif(logistic_10), decreasing = TRUE)

# Removing "monthnov" 

logistic_11 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.price.idx + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train)

summary(logistic_11)
# AIC: 16087
sort( vif(logistic_11), decreasing = TRUE)

# Removing "cons.price.idx" which comparitively less signficant (i.e only **)

logistic_12 <- glm(formula = response ~ jobretired + jobstudent + 
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train)

summary(logistic_12)
# AIC: 16092
# There is no major change in AIC so, variable removal is fine

sort( vif(logistic_12), decreasing = TRUE)

# Removing "jobstudent" which comparitively less signficant (i.e only **)

logistic_13 <- glm(formula = response ~ jobretired + 
                     educationPrimary_Education + 
                     contactcellular + monthapr + monthjul + monthjun + monthmar + 
                     monthmay + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.conf.idx + 
                     nr.employed , family = "binomial", data = train)

summary(logistic_13)
# AIC: 16099
# There is no major change in AIC so, variable removal is fine

sort( vif(logistic_13), decreasing = TRUE)

# Here all variables are highly significant with ***
# finalizing logistic_13 as the best model as it better compared to logistic_11

#logistic_final <- logistic_11
logistic_final <- logistic_13
#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predicted_probability <- predict(logistic_final, newdata = test[, -c(61,62)], type = "response")
summary(predicted_probability)


## Basic Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predicted_probability >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

#---------------------------------------------------------    
# CheckPoint 2c) - Find the optimal probability cut-off and report the relevant evaluation metrics
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predicted_probability >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 7.6% for final model

predicted_response <- factor(ifelse(predicted_probability >= 0.076, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc
# Accuracy 
# 0.7209453
sens
# Sensitivity 
# 0.7076149

spec
# Specificity 
# 0.7226377 


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# CheckPoint 3 - Create a data frame with the variables prospect ID, actual response, predicted response, predicted probability of response,                       #
# duration of call in seconds, and cost of call                                                                                                                    #
# - While creating the data frame, calculate the cost of call for each prospect in a new column                                                                    #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#

library(dplyr)
library(tidyverse)

model_evaluation_df <- select(test,prospectID, response, duration) %>% 
                                                mutate(predicted_response,predicted_probability)


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# CheckPoint 2b) Sort the data points in decreasing order of probability of response                                                                               #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#

model_evaluation_df <- arrange(model_evaluation_df, desc(predicted_probability))

# Cost per call (INR) = 0.033*(duration_in_seconds) + 0.8
model_evaluation_df$cost_of_call <- 0.033*(model_evaluation_df$duration) + 0.8


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# CheckPoint 4 -  Find the number of top X% prospects you should target to meet the business objective                                                             #
# - Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)                                        #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#


summary(model_evaluation_df$response)
summary(model_evaluation_df$predicted_response)


response_rate <- table(test$response)[2]/(table(test$response)[1] + table(test$response)[2])

# sorting the probabilities in decreasing order 
model_evaluation_df <- model_evaluation_df[order(model_evaluation_df$predicted_probability, decreasing = T), ]

#Downloading the data 
write.csv(model_evaluation_df,"model_prediction_evaluation.csv")

summary(model_evaluation_df$response[1:6800])
summary(model_evaluation_df$predicted_response[1:6800])

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, duration, cost_of_call, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob, duration, cost_of_call))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%

    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE), totalCallduration=sum(duration, na.rm = TRUE), totalcost=sum(cost_of_call, na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)),
           CumDurationOfCall=cumsum(totalCallduration),
           CumCostOfCall=cumsum(totalcost))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 

model_evaluation_df$response <- as.factor(ifelse(model_evaluation_df$response=="yes",1,0))

LG = lift(model_evaluation_df$response, model_evaluation_df$predicted_probability, model_evaluation_df$duration, model_evaluation_df$cost_of_call, groups = 10)

# Let's say if you have spent 1Re for each customer
View(LG)

#   A tibble: 10 x 10
#   bucket total totalresp totalCallduration totalcost Cumresp      Gain  Cumlift CumDurationOfCall CumCostOfCall
#     <int> <int>     <dbl>             <dbl>     <dbl>   <dbl>     <dbl>    <dbl>             <dbl>         <dbl>
#  1      1  1236       624          348021.4 12473.507     624  44.82759 4.482759          348021.4      12473.51
#  2      2  1236       237          330162.3 11884.156     861  61.85345 3.092672          678183.7      24357.66
#  3      3  1235       109          335603.6 12062.917     970  69.68391 2.322797         1013787.3      36420.58
#  4      4  1236        87          325985.1 11746.308    1057  75.93391 1.898348         1339772.4      48166.89
#  5      5  1235        61          295562.3 10741.556    1118  80.31609 1.606322         1635334.7      58908.44
#  6      6  1236        67          317275.5 11458.891    1185  85.12931 1.418822         1952610.1      70367.33
#  7      7  1236        62          267780.0  9825.538    1247  89.58333 1.279762         2220390.1      80192.87
#  8      8  1235        59          298636.8 10843.014    1306  93.82184 1.172773         2519026.9      91035.89
#  9      9  1236        41          286157.3 10431.991    1347  96.76724 1.075192         2805184.2     101467.88
# 10     10  1235        45          311187.0 11257.172    1392 100.00000 1.000000         3116371.2     112725.05

# Business objective is to achieve 80% of total responders at the minimum possible cost

# Call duration and Cost incurred throught direct telemarketing 

#Cumulative Total Postive Responses 80.31% in 5th decile 1118 out of 1392
LG$Cumresp[5]
#[1] 1118


#Average Call Duration in Seconds
LG$CumDurationOfCall[5] / LG$Cumresp[5]
#[1] 1462.732


#Average Call Duration in Minutes
(LG$CumDurationOfCall[5] / LG$Cumresp[5]) / 60
#[1] 24.37887


#Average Call Cost in Rupees
LG$CumCostOfCall [5]/ LG$Cumresp[5]
#[1] 52.69092

#Cumulative Cost in Rupees to reach 80% of total predicted responders
LG$CumCostOfCall [5]
#[1] 58908.44

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# CheckPoint 5 -  Create a lift chart                                                                                                                              #
# - The x-axis contains the number of prospects contacted; the y-axis contains the ratio: response rate using the model/ response rate without using the model     #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Gain Chart 

plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")

# Lift Chart 

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Recommendations as Per model                                                                                                                                     #
# -----------------------------------------------------------------------------------------------------------------------------------------------------------------#
# 
# Optimal % of People to reach = 80% (1118 from 1392)
# Average Call Cost in Rupees = Rs.52.69
# Total Cost = Rs.58908.44
#
# The Cumulative Lift of 1.60 for top 5 deciles,
# means that when selecting 80% of the records based on the model, 
# one can expect 1.60 times the total number of targets (events) found by randomly 
# selecting 80%-of-records without a model. In terms of customer attrition (churn) model, 
# we can say we can cover 1.60 times the number of attritors by selecting only 80% of the
# customers based on the model as compared to 80% customer selection randomly.

