######################################################################################################################################################################################################
#                                                                                                                                                                                                    #
#   CredX Aquisition Analytics for Credit Card Customers                                                                                                                                             #
#                                                                                                                                                                                                    #
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
#                                                                                                                                                                                                    #
#   Analytical Problem Solving Approach using CRISP-DM framework                                                                                                                                                             #
#                                                                                                                                                                                                    #
#     1. Business Understanding    
#        A leading credit card provider CredX receives thousands of applicants every year and, experiencing increased credit loss over the last few years. 
#        The CEO wants to mitigate the credit risk using the best strategy of acquiring the right customers.
#
#        1.1 Business Understanding
#            CredX intends to mitigate their credit risk during acquisition by 'Finding The Right Customers'.
#        1.2 Goals of Data Analysis
#            1.2.1. Using past data of the bank's applicants identify the most important factors affecting credit risk
#            1.2.2. Create strategies to mitigate the acquisition risk for new applications, by identifying right customers using predictive modelling to differentiate Good Vs Bad customer
# 
#     2. Data Understanding
#        3.1 Demographic Data
#            Contains customer-level information like Age, Gender, Marital Status and Salary etc.
#        3.2 Credit Bureau Data
#            This is taken from the credit bureau, contains past Avg Credit Card Utilization, Outstanding balance and 30/60/90 DPDs in last 6/12 months etc
#
#     3. Data Preparation
#        3.1 Data Cleaning
#        3.2 Data Imputation
#        3.3 Feature Engineering
#            3.3.1 Derived Variables
#            3.3.1 Encoding / Dummy variables
#            3.3.1 Weight-of-Evidence (WoE)/Information Value (IV) computation
#
#     4. Exploratory Data Analysis
#        4.1 Univariate Analysis
#        4.2 Bi-variate 
#        4.3 Multi-variate Analysis
#
#     5. Feature selection
#        We will use Use Weight-of-Evidence(WoE) /Information Value(IV) for feature selection
#
#     4. Modeling Building       
#           This is a binary classification problem with highly unbalanced data. We will apply following combination of model types, sampling techniques & cross-validation
#
#        4.1 Demographic Data                 - Unbalanced Data  - Logistic Regression 
#        4.2 Demographic & Credit Bureau Data - Unbalanced Data  - Logistic Regression
#        4.3 Demographic & Credit Bureau Data - Under Sampling   - Logistic Regression - with Cross Validation
#        4.4 Demographic & Credit Bureau Data - Over  Sampling   - Logistic Regression - with Cross Validation 
#        4.5 Demographic & Credit Bureau Data - SMOTE Sampling   - Logistic Regression - with Cross Validation
#        4.6 Demographic & Credit Bureau Data - SMOTE Sampling   - Decision Trees      - with Cross Validation
#        4.7 Demographic & Credit Bureau Data - SMOTE Sampling   - Random Forest       - with Cross Validation
#
#     5. Models Evaluation using Metrics & Final Model Selection                                                                                                                                                                   #
#        5.1 Accuracy, Sensitivity & Specificity
#        5.2 F-Score (F1)
#        5.3 Area Under Curve (AUC)
#        5.4 KSStatistic
#        5.5 ROC Curve
#        5.6 Vintage Curve
#
#     6. Model Deployment                                                                                                                                                                            #
#     7. Application scorecard Building                                                                                                                                                              #
#     8. Calculate scores on Rejected Population Data                                                                                                                                                                  # 
#     9. Financial Benefit Analysis                                                                                                                                                                     #
#                                                                                                                                                                                                    #
######################################################################################################################################################################################################

#loading libraries
library(ggplot2)
library(dplyr)
library(outliers)
library(corrplot)
library(MASS)
library(caret)
library(ROSE)
library(car)
library(reshape2)
library(scales)
library(tidyr)
library(ROCR)
library(tibble)

#-------------------------------------------------------------------------------- Start - Common Functions -------------------------------------------------------------------------------------------

# For calculating Mode value of Categorical variables
ModeFunc <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to determine the outliers in a measure
checkForOutliersDetection <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
}

# For plotting correlation matrix
plot_correlationMatrix <- function (data, features) {
  
  melted_cor_matrix <- melt(round(cor(data [ names(data) 
                                             %in% 
                                               features],
                                      use="complete.obs"),2))
  
  ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value, label=value)) +  
    geom_tile()    + 
    geom_text()  + 
    xlab('')   + 
    ylab('') + 
    theme_minimal() + 
    theme(axis.text.x = element_text(size=10, 
                                     hjust=-0.08, 
                                     angle= -35 ))
}




#-------------------------------------------------------------------------------- End - Common Functions ---------------------------------------------------------------------------------------------

# Loading Data

# Loading Demographic Data
demographic_data.original <-read.csv("Demographic data.csv",
                            header = TRUE,
                            stringsAsFactors = FALSE)

# Loading Credit Bureau Data
creditbureau_data.original <- read.csv("Credit Bureau data.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data Cleaning 
#   1. Remove Duplicate Records
#   2. Separate Rejected Applications from Data Analysis & Model Building
#   3. Remove Invalid / Incorrect Values Records
#   4. Missing Values Treatment 
#   5. Outlier Treatment
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


## checking for Total row count
nrow(demographic_data.original)
#71295

nrow(creditbureau_data.original)
#71295

## Checking for Duplicate records 
length(unique(demographic_data.original$Application.ID))
#71292 - 3 Duplicate records exist

length(unique(creditbureau_data.original$Application.ID))
#71292 - 3 Duplicate records exist

# Remove duplicate records
demographic_data <- demographic_data.original[!duplicated(demographic_data.original$Application.ID),]
creditbureau_data <- creditbureau_data.original[!duplicated(creditbureau_data.original$Application.ID),]

summary(demographic_data)
summary(creditbureau_data)

# Checking NA Values
sapply(demographic_data, function(x) sum(is.na(x) | is.null(x)))
#                             Application.ID                                          Age                                      Gender 
#                                          0                                            0                                           0 
# Marital.Status..at.the.time.of.application.                            No.of.dependents                                      Income 
#                                          0                                            3                                           0 
#                                  Education                                   Profession                           Type.of.residence 
#                                          0                                            0                                           0 
#          No.of.months.in.current.residence              No.of.months.in.current.company                             Performance.Tag 
#                                          0                                            0                                        1425 
sapply(creditbureau_data, function(x) sum(is.na(x) | is.null(x)))
#                               Application.ID                    No.of.times.90.DPD.or.worse.in.last.6.months 
#                                            0                                                               0 
# No.of.times.60.DPD.or.worse.in.last.6.months                    No.of.times.30.DPD.or.worse.in.last.6.months 
#                                            0                                                               0 
# No.of.times.90.DPD.or.worse.in.last.12.months                   No.of.times.60.DPD.or.worse.in.last.12.months 
#                                            0                                                               0 
# No.of.times.30.DPD.or.worse.in.last.12.months                          Avgas.CC.Utilization.in.last.12.months 
#                                            0                                                            1058 
#          No.of.trades.opened.in.last.6.months                           No.of.trades.opened.in.last.12.months 
#                                            1                                                               0 
#       No.of.PL.trades.opened.in.last.6.months                        No.of.PL.trades.opened.in.last.12.months 
#                                            0                                                               0 
# No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 
#                                            0                                                               0 
#                    Presence.of.open.home.loan                                             Outstanding.Balance 
#                                           272                                                             272 
#                            Total.No.of.Trades                                      Presence.of.open.auto.loan 
#                                            0                                                               0 
#                               Performance.Tag 
#                                          1425 

# Making the Performance Tag as a factor variable as new feature
demographic_data$Performance <- as.factor(demographic_data$Performance.Tag)


# Validate Performance Tag across Application IDs in both data-sets
setdiff(dplyr::select(demographic_data, Application.ID, Performance.Tag),dplyr::select(creditbureau_data, Application.ID, Performance.Tag))
# [1] Application.ID  Performance.Tag
# <0 rows> (or 0-length row.names)

customer_master_data <- merge(demographic_data, creditbureau_data, by="Application.ID")
View(customer_master_data)

# Remove Performance.Tag.x and Performance.Tag.y
customer_master_data <- customer_master_data[,-12]
customer_master_data <- customer_master_data[,-30]

# ------------------------------------------------------------------- Remove Rejected Applications Records ------------------------------------------------------------------------------------------
# Check distribution of Classes and also NA values
summary(customer_master_data$Performance)
#    0     1  NA's 
#66920  2947  1425 

# Also classes (1 and 0) distribution, is highly unbalanced
((1425/71276)*100)
# [1] 1.9992

# The records with Performance = NA are treated as Applications rejected. 
# There are approximately 2% and we are ignoring them in the modeling.

# Separating 1425 records Performance = NA
rejected_records <- customer_master_data[which(is.na(customer_master_data$Performance)),]
nrow(rejected_records)
# [1] 1425

# Retaining records that have Performance = 1 or 0
customer_master_data <- customer_master_data[-which(is.na(customer_master_data$Performance)),]

# Data Quality Checks for Rejected Population
length(customer_master_data$Performance)
# [1] 69867
length(rejected_records$Application.ID)
# [1] 1425
length(unique(rejected_records$Application.ID))
# [1] 1425
# No Duplicate records in Rejected population
sort(unique(rejected_records$Age), decreasing = FALSE)
# No Records with Age <18
sum(is.na(rejected_records$No.of.months.in.current.company))
# [1] 0
sum(is.na(rejected_records$No.of.months.in.current.residence))
# [1] 0
sum(is.na(rejected_records$No.of.times.30.DPD.or.worse.in.last.6.months))
# [1] 0
sum(is.na(rejected_records$No.of.trades.opened.in.last.12.months))
# [1] 0
sum(is.na(rejected_records))
# [1] 1460
sum(is.na(rejected_records$Performance))
# [1] 1425
# ------------------------------------------------------------------- Removing Records with Invalid Data -------------------------------------------------------------------------------------------

# Demographic Data contains 65 records with Age < 18 which is an invalid value
length(which(customer_master_data$Age < 18))
# 65

length(which(customer_master_data$Age <= 0))
# 20

# 65 records exists with Age < 18, with only 1 as defaulter
dplyr::select(customer_master_data[which(customer_master_data$Age < 18),], Age, Application.ID, Performance)

# Removing records with Age < 18
customer_master_data <- customer_master_data[-which(customer_master_data$Age < 18),]
nrow(customer_master_data)
# [1] 69802

# ------------------------------------------------------------------- Missing Values - Data Imputation ---------------------------------------------------------------------------------------------
#
# Replacing missing values which are small in number, using simple and straight-forward techniques e.g. Mode etc
#
# No.of.trades.opened.in.last.6.months
#   - Only 1 missing value exist
#   - Make NA value as '0'

customer_master_data[which(is.na(customer_master_data$No.of.trades.opened.in.last.6.months)), 
                     "No.of.trades.opened.in.last.6.months"] <- 0
# Verify values
length(which(is.na(customer_master_data$No.of.trades.opened.in.last.6.months)))

# No.of.Dependents
#   - Only 3 missing values exist
#   - Make NA values for No.of.Dependents as '0'
customer_master_data [which(is.na(customer_master_data$No.of.dependents)), 
                      "No.of.dependents"] <- 0

# Verify values 
length(which(is.na(customer_master_data$No.of.dependents)))
#[1] 0

# Gender 
#   - Only 2 missing values
#   - Replacing with Mode value

summary(factor(customer_master_data$Gender))
#       0     F     M 
# 1     1 16490 53311

customer_master_data[-which(customer_master_data$Gender %in% c("F","M")), 
                     "Gender"] <- ModeFunc(customer_master_data$Gender)


# Marital Status
# Check for invalid i.e. NA values for Marital Status
nrow(customer_master_data[ -which(customer_master_data$Marital.Status..at.the.time.of.application. 
                                  %in% 
                                  c("Married","Single")),])


# Imputing with Mode i.e. "Married"
customer_master_data[-which( customer_master_data$Marital.Status..at.the.time.of.application. 
                             %in% 
                             c("Married","Single")), 
                     "Marital.Status..at.the.time.of.application."] <- ModeFunc(customer_master_data$Marital.Status..at.the.time.of.application.)

# Profession
#     - Only 12 values are missing
#     - Impute with Mode ()
summary(factor(customer_master_data$Profession))
#        SAL      SE SE_PROF 
# 12   39639   13915   16236

# Imputing with Mode Value i.e. "SAL"
customer_master_data[-which(customer_master_data$Profession 
                            %in% 
                              c('SAL','SE','SE_PROF')), 
                     "Profession"] <- ModeFunc(customer_master_data$Profession)

# Type of residence
#     - 8 values are missing
#     - Impute with Mode ()

summary(factor(customer_master_data$Type.of.residence))
#                 Company provided Living with Parents  Others            Owned              Rented 
# 8                1601             1767                 198               13986              52242 

# Imputing with Mode value i.e. "Rented"
customer_master_data[-which(customer_master_data$Type.of.residence 
                            %in% 
                              c('Company provided',
                                'Living with Parents',
                                'Others',
                                'Owned',
                                'Rented')), 
                     "Type.of.residence"] <- ModeFunc(customer_master_data$Type.of.residence)


# Number of months in current residence
length(which(customer_master_data$No.of.months.in.current.residence <= 0))
# 0

summary(customer_master_data$No.of.months.in.current.residence)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00    6.00   10.00   34.57   61.00  126.00

# Number of months in current company
# imputing age less than or equal to 0

length(which(customer_master_data$No.of.months.in.current.company <= 0))
#0

summary(customer_master_data$No.of.months.in.current.company)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.00   17.00   34.00   34.19   51.00  133.00 

# Education
# 118 records are with NA values - Need Imputation with WoE
nrow(customer_master_data[-which(customer_master_data$Education %in% c('Bachelor','Masters','Others','Phd','Professional')),])
#118


# Income 
# 106 records are with NA values - Need Imputation with WoE
length(which(customer_master_data$Income <= 0))
#106
customer_master_data$Income_imputed <- customer_master_data$Income
customer_master_data[which(customer_master_data$Income_imputed <=0), "Income_imputed"] <- NA

rejected_records$Income_imputed <- rejected_records$Income
rejected_records[which(rejected_records$Income_imputed <=0), "Income_imputed"] <- NA

View(customer_master_data)

# ------------------------------------------------------------------- Outlier treatment ------------------------------------------------------------------------------------------
# Note - Outliers removal is not required to perform on all measures, as it is not impacting any results
# ----
# Following are the variables with outliers.
#
# Outstanding.Balance
# Income
# Avgas.CC.Utilization.in.last.12.months
# Total.No.of.Trades
# No.of.trades.opened.in.last.12.months
# No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# No.of.PL.trades.opened.in.last.12.months
checkForOutliersDetection(customer_master_data, Outstanding.Balance) #function call
# Outliers identified: 0
summary(customer_master_data$Outstanding.Balance)
hist(customer_master_data$Outstanding.Balance, main = "Histogram of Outstanding.Balance")

# Income
checkForOutliersDetection(customer_master_data, Income) 
# Outliers identified: 0
hist(customer_master_data$Income, main = "Histogram of Income")

# Avgas.CC.Utilization.in.last.12.months
checkForOutliersDetection(customer_master_data, Avgas.CC.Utilization.in.last.12.months)
# Outliers identified: 3624
hist(customer_master_data$Avgas.CC.Utilization.in.last.12.months, main = "Histogram of Avgas.CC.Utilization.in.last.12.months")

outlier_range<-1.5*IQR(customer_master_data$Avgas.CC.Utilization.in.last.12.months, na.rm = T) #1843 outlier
upper_whisker=unname(quantile(customer_master_data$Avgas.CC.Utilization.in.last.12.months,0.95, na.rm = T))+outlier_range
lower_whisker=unname(quantile(customer_master_data$Avgas.CC.Utilization.in.last.12.months,0.05, na.rm = T))-outlier_range
customer_master_data2 <- customer_master_data[which((customer_master_data$Avgas.CC.Utilization.in.last.12.months>=upper_whisker | 
                                                       customer_master_data$Avgas.CC.Utilization.in.last.12.months<=lower_whisker)==FALSE),]
hist(customer_master_data2$Avgas.CC.Utilization.in.last.12.months, main = "Histogram of Avgas.CC.Utilization.in.last.12.months")

summary(customer_master_data$Performance)
#     0     1 
# 66856  2946
summary(customer_master_data2$Performance)
#     0     1 
# 65886  2898

# Total.No.of.Trades
checkForOutliersDetection(customer_master_data2, Total.No.of.Trades)
# Outliers identified: 6818
hist(customer_master_data2$Total.No.of.Trades, main = "Histogram of Total.No.of.Trades")
outlier_range<-1.5*IQR(customer_master_data2$Total.No.of.Trades, na.rm = T) #1843 outlier
upper_whisker=unname(quantile(customer_master_data2$Total.No.of.Trades,0.95, na.rm = T))+outlier_range
lower_whisker=unname(quantile(customer_master_data2$Total.No.of.Trades,0.05, na.rm = T))-outlier_range
customer_master_data3 <- customer_master_data2[which((customer_master_data2$Total.No.of.Trades>=upper_whisker | 
                                                        customer_master_data2$Total.No.of.Trades<=lower_whisker)==FALSE),]

summary(customer_master_data3$Performance)
#     0     1 
# 65779  2896

# Discarding the Outlier treatment as models are performing better with having Outliers
# It is not always required to remove outliers, as they carry on important patters & trends
# 
# customer_master_data <- customer_master_data3

# ------------------------------------------------------------------- Feature Engineering - Derived Variables-------------------------------------------------------------------------------------------

customer_master_data$Income_bin <- as.factor(cut(customer_master_data$Income,
                                                    breaks = c(-Inf,1,11,21,31,40,Inf),
                                                    labels=c("<0","1-10","11-20","21-30","31-40",">40"), 
                                                           ordered = TRUE))

customer_master_data$age_bin <- as.factor(cut(customer_master_data$Age,
                                    breaks=c(-Inf,35,46,55,Inf),
                                    labels=c("<35","35-45","46-55",">55"), ordered = TRUE,
                                    right = FALSE))

customer_master_data$current_residence_bin <- as.factor(cut(customer_master_data$No.of.months.in.current.residence,
                                                            breaks = c(-Inf,13,25,37,49,61,73,85,97,109,121,Inf),
                                                            labels=c("<12","13-24","25-36","37-48","49-60","61-72","73-84","85-96","97-108","109-120",">120"),
                                                            ordered = TRUE))

customer_master_data$current_company_bin <- as.factor(cut(customer_master_data$No.of.months.in.current.company, 
                                                        breaks = c(1,12, 24, 36, 48, 60, 72,84,96,108,120,136),
                                                            ordered = TRUE))



# ------------------------------------------------------------------- Feature Engineering - Weight Of Evidence(WoE)/ Information Value(IV) Analysis ------------------------------------------------------------------------------------- 
# Using WoE for both Variable Importance and also Missing Values

library(Information)
str(customer_master_data$Performance)

# The data contains status of customer performance through variable Performance Tag with value 1 representing Default and 0 for Non-Default. 
# We leverage R Information package for computing the Information Values (IV). But this package interprets 1 value for Good which is contradictory 
# to business case here as Performance Tag value. So, we need another variable Performance Tag for IV with values 1 and 0 replaced with 0 and 1 respectively.

customer_master_data$Performance.Tag_forIV <- ifelse(customer_master_data$Performance == 0, 1, 0)

IV <- create_infotables(data=customer_master_data, 
                        y="Performance.Tag_forIV",
                        bins = 10, 
                        parallel = FALSE)

IV_Value = data.frame(IV$Summary)

# Reference table for Variable Importance Analysis based on Information Value(IV)
# -------------------------------------------------------------------------------
# Information Value(IV)   Predictive Power
# -------------------------------------------------------------------------------
#                 <0.02 -> Useless for Prediction
#            0.02 - 0.1 -> Weak Predictor
#            0.1  - 0.3 -> Medium Predictor
#            0.3  - 0.5 -> Strong Predictor
#                  >0.5 -> Suspecious
# -------------------------------------------------------------------------------
# Printing values >=0.02
arrange(IV_Value [IV_Value$IV >=0.02, ], desc(IV))
#                                                           Variable IV
# 1                           Avgas.CC.Utilization.in.last.12.months 0.31015860
# 2                            No.of.trades.opened.in.last.12.months 0.29827712
# 3                         No.of.PL.trades.opened.in.last.12.months 0.29604052
# 4  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 0.29560438
# 5                                              Outstanding.Balance 0.24604217
# 6                     No.of.times.30.DPD.or.worse.in.last.6.months 0.24167952
# 7                                               Total.No.of.Trades 0.23713984
# 8                          No.of.PL.trades.opened.in.last.6.months 0.21973098
# 9                    No.of.times.90.DPD.or.worse.in.last.12.months 0.21393775
# 10                    No.of.times.60.DPD.or.worse.in.last.6.months 0.20592613
# 11  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 0.20523987
# 12                   No.of.times.30.DPD.or.worse.in.last.12.months 0.19848248
# 13                            No.of.trades.opened.in.last.6.months 0.18597050
# 14                   No.of.times.60.DPD.or.worse.in.last.12.months 0.18563797
# 15                    No.of.times.90.DPD.or.worse.in.last.6.months 0.16016368
# 16                               No.of.months.in.current.residence 0.07913990
# 17                                           current_residence_bin 0.06080075
# 18                                                          Income 0.04255551
# 19                                                      Income_bin 0.04007508
# 20                                 No.of.months.in.current.company 0.02175181

# ------------- Data Imputation based WoE Analysis Approach
# Two New columns are being added for every variable whose WoE analysis being done
# 1. Add WoE values for the variable as new feature (e.g. <variable>_WoE) and retain original feature/values in master data frame
# 2. For imputation to NA/Missing/Incorrect values BIN find other BIN/Bucket with nearest WoE value close enough, 
#    i.e. add new variable as <variable>_imputed
#    
#    2a) For a continuous variable - when nearest match found, use median value of matching bucket
#    2b) For a continuous variable - when nearest match NOT found, use median for whole continuous variable
#    2c) For a categorical variable - when nearest match found, use Mode value of matching bucket
#    2d) For a categorical variable - when nearest match NOT found, use Mode value of whole categorical variable

# 
# WoE Analysis for Education
Education_bin <- data.frame(IV$Tables$Education)
print(IV$Tables$Education)

# Education     N     Percent          WOE               IV
# 1                118 0.001690544 -0.004112913 0.00000002865124
# 2     Bachelor 17282 0.247593123 -0.016466018 0.00006766684551
# 3      Masters 23464 0.336160458 -0.008057761 0.00008957360028
# 4       Others   119 0.001704871 -0.491974160 0.00060871065855
# 5          Phd  4455 0.063825215  0.028288409 0.00065912947090
# 6 Professional 24362 0.349025788  0.017637051 0.00076682701447

plot_infotables(IV,"Education")

# Creating a column with WoE for Education
customer_master_data$Education_WoE <- ifelse(customer_master_data$Education=="Bachelor",
                                             IV$Tables$Education$WOE[2],
                                             ifelse(customer_master_data$Education=="Masters",
                                                    IV$Tables$Education$WOE[3],
                                                    ifelse(customer_master_data$Education=="Others",
                                                           IV$Tables$Education$WOE[4],
                                                           ifelse(customer_master_data$Education=="Phd",
                                                                  IV$Tables$Education$WOE[5],
                                                                  ifelse(customer_master_data$Education=="Professional",
                                                                         IV$Tables$Education$WOE[6],
                                                                         IV$Tables$Education$WOE[1])))))
unique(customer_master_data$Education_WoE)
# [1]  0.017649959  0.028258493 -0.008043166 -0.016495934 -0.004142828 -0.492004075
# Replace 'NA' (WoE = -0.004112913) values with  Masters (nearest WoE = -0.008057761)
customer_master_data$Education_imputed <- ifelse(customer_master_data$Education == "",
                                                 "Masters",
                                                 customer_master_data$Education)



# WoE Analysis for Income
Income.bin <- data.frame(IV$Tables$Income_imputed)

print(Income.bin)
#    Income_imputed    N     Percent         WOE          IV
# 1              NA  106 0.001518581  0.82915098 0.000726913
# 2           [1,5] 6222 0.089137847 -0.31412051 0.010901968
# 3          [6,10] 6508 0.093235151 -0.27545857 0.018939175
# 4         [11,16] 7916 0.113406493 -0.06639700 0.019454612
# 5         [17,21] 6795 0.097346781 -0.08141794 0.020124511
# 6         [22,26] 6821 0.097719263 -0.02551861 0.020188895
# 7         [27,31] 6807 0.097518696 -0.07956922 0.020829301
# 8         [32,36] 6820 0.097704937  0.15505200 0.023018462
# 9         [37,41] 6711 0.096143377  0.26716146 0.029100697
# 10        [42,48] 7780 0.111458124  0.17694798 0.032321235
# 11        [49,60] 7316 0.104810750  0.36098054 0.043933921

# # Replace '[-0.5,5]' (WoE = 0.30218631) values with median of [6,10] (WoE = -0.27545857)
# customer_master_data$Income_imputed <- ifelse(is.na(customer_master_data$Income_imputed),
#                                                            median(customer_master_data$Income_imputed, na.rm = TRUE),
#                                                            customer_master_data$Income_imputed)

# creating a Woe column for Income for master dataframe
customer_master_data$Income_imputed_WoE <- ifelse(is.na(customer_master_data$Income_imputed),
                                          IV$Tables$Income_imputed$WOE[1],
                                          ifelse(between(customer_master_data$Income_imputed,1,5),
                                                IV$Tables$Income_imputed$WOE[2],
                                                ifelse(between(customer_master_data$Income_imputed,6,10), 
                                                       IV$Tables$Income_imputed$WOE[3],
                                                       ifelse(between(customer_master_data$Income_imputed,11,16),
                                                              IV$Tables$Income_imputed$WOE[4],
                                                              ifelse(between(customer_master_data$Income_imputed,17,21),
                                                                     IV$Tables$Income_imputed$WOE[5],
                                                                     ifelse(between(customer_master_data$Income_imputed,22,26),
                                                                            IV$Tables$Income_imputed$WOE[6],
                                                                            ifelse(between(customer_master_data$Income_imputed,27,31),
                                                                                   IV$Tables$Income_imputed$WOE[7],
                                                                                   ifelse(between(customer_master_data$Income_imputed,32,36),
                                                                                          IV$Tables$Income_imputed$WOE[8],
                                                                                          ifelse(between(customer_master_data$Income_imputed,37,41),
                                                                                                 IV$Tables$Income_imputed$WOE[9],
                                                                                                 ifelse(between(customer_master_data$Income_imputed,42,48),
                                                                                                        IV$Tables$Income_imputed$WOE[10],
                                                                                                        IV$Tables$Income_imputed$WOE[11]))))))))))

unique(customer_master_data$Income_imputed_WoE)
# [1]  0.15505200 -0.06639700 -0.07956922  0.36098054  0.17694798 -0.31412051  0.26716146 -0.27545857 -0.08141794 -0.02551861  0.82915098

# creating a Woe column for Income for rejected records
rejected_records$Income_imputed_WoE <- ifelse(is.na(rejected_records$Income_imputed),
                                                  IV$Tables$Income_imputed$WOE[1],
                                                  ifelse(between(rejected_records$Income_imputed,1,5),
                                                         IV$Tables$Income_imputed$WOE[2],
                                                         ifelse(between(rejected_records$Income_imputed,6,10), 
                                                                IV$Tables$Income_imputed$WOE[3],
                                                                ifelse(between(rejected_records$Income_imputed,11,16),
                                                                       IV$Tables$Income_imputed$WOE[4],
                                                                       ifelse(between(rejected_records$Income_imputed,17,21),
                                                                              IV$Tables$Income_imputed$WOE[5],
                                                                              ifelse(between(rejected_records$Income_imputed,22,26),
                                                                                     IV$Tables$Income_imputed$WOE[6],
                                                                                     ifelse(between(rejected_records$Income_imputed,27,31),
                                                                                            IV$Tables$Income_imputed$WOE[7],
                                                                                            ifelse(between(rejected_records$Income_imputed,32,36),
                                                                                                   IV$Tables$Income_imputed$WOE[8],
                                                                                                   ifelse(between(rejected_records$Income_imputed,37,41),
                                                                                                          IV$Tables$Income_imputed$WOE[9],
                                                                                                          ifelse(between(rejected_records$Income_imputed,42,48),
                                                                                                                 IV$Tables$Income_imputed$WOE[10],
                                                                                                                 IV$Tables$Income_imputed$WOE[11]))))))))))

unique(rejected_records$Income_imputed_WoE)
# [1] -0.27545857 -0.02551861 -0.07956922 -0.08141794 -0.31412051  0.26716146  0.36098054 -0.06639700  0.15505200  0.17694798
# Replace NA with median for master dataframe
customer_master_data[which(is.na(customer_master_data$Income_imputed)), 
                     "Income_imputed"] <- median(customer_master_data$Income_imputed,
                                                 na.rm = TRUE)
# Replace NA with median for rejected records dataframe
rejected_records$Income_imputed <- rejected_records$Income
rejected_records[which(is.na(rejected_records$Income_imputed)), 
                 "Income_imputed"] <- median(rejected_records$Income_imputed,
                                             na.rm = TRUE)
plot_infotables(IV,"Income_imputed")


# WoE Analysis for Presence.of.open.home.loan
# Creating a WoE column for Presence.of.open.home.loan
Presence.of.open.home.loan.bin <- data.frame(IV$Tables$Presence.of.open.home.loan)
print(IV$Tables$Presence.of.open.home.loan)
# Presence.of.open.home.loan     N     Percent         WOE           IV
# 1                         NA   272 0.003896848  0.37444474 0.0004618241
# 2                      [0,0] 51485 0.737607450 -0.07385121 0.0046235462
# 3                      [1,1] 18043 0.258495702  0.23740686 0.0177092799

plot_infotables(IV,"Presence.of.open.home.loan")


customer_master_data$Presence.of.open.home.loan_WoE <- ifelse(is.na(customer_master_data$Presence.of.open.home.loan),
                                                              IV$Tables$Presence.of.open.home.loan$WOE[1],
                                                              ifelse(customer_master_data$Presence.of.open.home.loan==1,
                                                                     IV$Tables$Presence.of.open.home.loan$WOE[3],
                                                                     IV$Tables$Presence.of.open.home.loan$WOE[2]))
# Replace 'NA' (WoE = 0.37444474) values with 1 (nearest WoE = 0.23740686)
customer_master_data$Presence.of.open.home.loan_imputed <- ifelse(is.na(customer_master_data$Presence.of.open.home.loan),
                                                                  1,
                                                                  customer_master_data$Presence.of.open.home.loan)



# WoE Analysis for Outstanding.Balance
# Creating a WoE column for Outstanding.Balance
Outstanding.Balance.bin <- data.frame(IV$Tables$Outstanding.Balance)
print(IV$Tables$Outstanding.Balance)
# Outstanding.Balance    N     Percent        WOE           IV
# 1                   NA  272 0.003896848  0.3744447 0.0004618241
# 2             [0,6851] 6952 0.099598854  0.7700511 0.0426036519
# 3         [6852,25600] 6953 0.099613181  0.9115194 0.0983158956
# 4       [25604,386878] 6952 0.099598854  0.1379774 0.1000967231
# 5      [386879,585389] 6953 0.099613181 -0.2546806 0.1073661541
# 6      [585402,774188] 6953 0.099613181 -0.4512915 0.1324028254
# 7      [774194,972273] 6953 0.099613181 -0.4369463 0.1557145278
# 8     [972299,1357076] 6953 0.099613181 -0.4027584 0.1752039235
# 9    [1357118,2960907] 6952 0.099598854  0.3818808 0.1874410404
# 10   [2960909,3282409] 6953 0.099613181  0.8306478 0.2352665725
# 11   [3282457,5218801] 6954 0.099627507 -0.2961344 0.2452890592

plot_infotables(IV,"Outstanding.Balance")

customer_master_data$Outstanding.Balance_WoE <- ifelse(is.na(customer_master_data$Outstanding.Balance),
                                                       IV$Tables$Outstanding.Balance$WOE[1],
                                                       ifelse(customer_master_data$Outstanding.Balance <=6851,
                                                              IV$Tables$Outstanding.Balance$WOE[2],
                                                              ifelse(between(customer_master_data$Outstanding.Balance,6852,25590),
                                                                     IV$Tables$Outstanding.Balance$WOE[3],
                                                                     ifelse(between(customer_master_data$Outstanding.Balance,25600,386878),
                                                                            IV$Tables$Outstanding.Balance$WOE[4],
                                                                            ifelse(between(customer_master_data$Outstanding.Balance,386879,585389),
                                                                                   IV$Tables$Outstanding.Balance$WOE[5],
                                                                                  ifelse(between(customer_master_data$Outstanding.Balance,585402,774181),
                                                                                         IV$Tables$Outstanding.Balance$WOE[6],
                                                                                        ifelse(between(customer_master_data$Outstanding.Balance,774188,972265),
                                                                                               IV$Tables$Outstanding.Balance$WOE[7],
                                                                                               ifelse(between(customer_master_data$Outstanding.Balance,972273,1357072),
                                                                                                      IV$Tables$Outstanding.Balance$WOE[8],
                                                                                                      ifelse(between(customer_master_data$Outstanding.Balance,1357076,2960907),
                                                                                                             IV$Tables$Outstanding.Balance$WOE[9],
                                                                                                             ifelse(between(customer_master_data$Outstanding.Balance,2960909,3282409),
                                                                                                                    IV$Tables$Outstanding.Balance$WOE[10],
                                                                                                                    IV$Tables$Outstanding.Balance$WOE[11]))))))))))
unique(customer_master_data$Outstanding.Balance_WoE)
# [1] -0.2961643 -0.4513214 -0.4369762  0.1340639 -0.4027883  0.7700212 -0.2547105  0.8306179  0.3819991  0.9199346  0.3744148

# Calculate for rejected population
rejected_records$Outstanding.Balance_WoE <- ifelse(is.na(rejected_records$Outstanding.Balance),
                                                       IV$Tables$Outstanding.Balance$WOE[1],
                                                       ifelse(rejected_records$Outstanding.Balance <=6851,
                                                              IV$Tables$Outstanding.Balance$WOE[2],
                                                              ifelse(between(rejected_records$Outstanding.Balance,6852,25590),
                                                                     IV$Tables$Outstanding.Balance$WOE[3],
                                                                     ifelse(between(rejected_records$Outstanding.Balance,25600,386878),
                                                                            IV$Tables$Outstanding.Balance$WOE[4],
                                                                            ifelse(between(rejected_records$Outstanding.Balance,386879,585389),
                                                                                   IV$Tables$Outstanding.Balance$WOE[5],
                                                                                   ifelse(between(rejected_records$Outstanding.Balance,585402,774181),
                                                                                          IV$Tables$Outstanding.Balance$WOE[6],
                                                                                          ifelse(between(rejected_records$Outstanding.Balance,774188,972265),
                                                                                                 IV$Tables$Outstanding.Balance$WOE[7],
                                                                                                 ifelse(between(rejected_records$Outstanding.Balance,972273,1357072),
                                                                                                        IV$Tables$Outstanding.Balance$WOE[8],
                                                                                                        ifelse(between(rejected_records$Outstanding.Balance,1357076,2960907),
                                                                                                               IV$Tables$Outstanding.Balance$WOE[9],
                                                                                                               ifelse(between(rejected_records$Outstanding.Balance,2960909,3282409),
                                                                                                                      IV$Tables$Outstanding.Balance$WOE[10],
                                                                                                                      IV$Tables$Outstanding.Balance$WOE[11]))))))))))

unique(rejected_records$Outstanding.Balance_WoE)
# [1] -0.4027883 -0.4369762 -0.4513214  0.1340639 -0.2547105 -0.2961643  0.8306179  0.3819991  0.9199346

# Replace 'NA' (WoE = 0.3744447) values with median of [1357118,2960907] (WoE = 0.3818808)
customer_master_data$Outstanding.Balance_imputed <- ifelse(is.na(customer_master_data$Outstanding.Balance),
                                                           median(filter(customer_master_data, Outstanding.Balance >=1357076 & Outstanding.Balance<=2960907) [, "Outstanding.Balance"]),
                                                           customer_master_data$Outstanding.Balance)
# No 'NA' values for Outstanding.Balance in Rejected population
sum(is.na(rejected_records$Outstanding.Balance))
#[1] 0
rejected_records$Outstanding.Balance_imputed <- rejected_records$Outstanding.Balance

# WoE Analysis for Avgas.CC.Utilization.in.last.12.months

print(IV$Tables$Avgas.CC.Utilization.in.last.12.months)
# Avgas.CC.Utilization.in.last.12.months    N    Percent         WOE          IV
# 1                                      NA 1018 0.01458411 -0.11599767 0.000206996
# 2                                   [0,4] 5521 0.07909515  0.80182190 0.036016027
# 3                                   [5,6] 5463 0.07826423  0.80062757 0.071360890
# 4                                   [7,8] 6856 0.09822068  0.79320847 0.115034194
# 5                                  [9,11] 9587 0.13734563  0.67681490 0.161716538
# 6                                 [12,14] 6585 0.09433827  0.46706002 0.178421559
# 7                                 [15,21] 6851 0.09814905  0.07916613 0.179014873
# 8                                 [22,37] 7116 0.10194550 -0.47533086 0.207765296
# 9                                 [38,51] 6742 0.09658749 -0.58461156 0.251161250
# 10                                [52,71] 7016 0.10051288 -0.56326710 0.292660548
# 11                               [72,113] 7047 0.10095699 -0.38102610 0.310158603
plot_infotables(IV,"Avgas.CC.Utilization.in.last.12.months")


customer_master_data$Avgas.CC.Utilization.in.last.12.months_WoE <- ifelse(is.na(customer_master_data$Avgas.CC.Utilization.in.last.12.months),
                                                                          IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[1],
                                                                          ifelse(customer_master_data$Avgas.CC.Utilization.in.last.12.months<=4,
                                                                                 IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[2],
                                                                                 ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,5,6),
                                                                                        IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[3],
                                                                                        ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,7,8),
                                                                                               IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[4],
                                                                                               ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,9,11),
                                                                                                      IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[5],
                                                                                                      ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,12,14),
                                                                                                             IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[6],
                                                                                                             ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,15,21),
                                                                                                                    IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[7],
                                                                                                                    ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,22,37),
                                                                                                                           IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[8],
                                                                                                                           ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,38,51),
                                                                                                                                  IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[9],
                                                                                                                                  ifelse(between(customer_master_data$Avgas.CC.Utilization.in.last.12.months,52,71),
                                                                                                                                         IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[10],
                                                                                                                                         IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[11]))))))))))
unique(customer_master_data$Avgas.CC.Utilization.in.last.12.months_WoE)
# [1] -0.38102610  0.67681490 -0.47533086  0.07916613  0.80182190  0.80062757 -0.58461156  0.46706002 -0.56326710 -0.11599767  0.79320847

# Calculate for rejected population
rejected_records$Avgas.CC.Utilization.in.last.12.months_WoE <- ifelse(is.na(rejected_records$Avgas.CC.Utilization.in.last.12.months),
                                                                          IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[1],
                                                                          ifelse(rejected_records$Avgas.CC.Utilization.in.last.12.months<=4,
                                                                                 IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[2],
                                                                                 ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,5,6),
                                                                                        IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[3],
                                                                                        ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,7,8),
                                                                                               IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[4],
                                                                                               ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,9,11),
                                                                                                      IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[5],
                                                                                                      ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,12,14),
                                                                                                             IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[6],
                                                                                                             ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,15,21),
                                                                                                                    IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[7],
                                                                                                                    ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,22,37),
                                                                                                                           IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[8],
                                                                                                                           ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,38,51),
                                                                                                                                  IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[9],
                                                                                                                                  ifelse(between(rejected_records$Avgas.CC.Utilization.in.last.12.months,52,71),
                                                                                                                                         IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[10],
                                                                                                                                         IV$Tables$Avgas.CC.Utilization.in.last.12.months$WOE[11]))))))))))

unique(rejected_records$Avgas.CC.Utilization.in.last.12.months_WoE)
# [1] -0.56326710 -0.38102610 -0.58461156 -0.47533086  0.07916613  0.46706002 -0.11599767  0.67681490  0.80062757  0.79320847  0.80182190

# Replace 'NA' (WoE = -0.1159976) values with median of [72,113] (nearest WoE = -0.38102610)
customer_master_data$Avgas.CC.Utilization.in.last.12.months_imputed <- ifelse(is.na(customer_master_data$Avgas.CC.Utilization.in.last.12.months),
                                                                              median(filter(customer_master_data, Avgas.CC.Utilization.in.last.12.months >=72 & Avgas.CC.Utilization.in.last.12.months<=113) [, "Avgas.CC.Utilization.in.last.12.months"]),
                                                                              customer_master_data$Avgas.CC.Utilization.in.last.12.months)


# NA values exist for Avgas.CC.Utilization.in.last.12.months in rejected population
sum(is.na(rejected_records$Avgas.CC.Utilization.in.last.12.months))
# [1] 35
rejected_records$Avgas.CC.Utilization.in.last.12.months_imputed <- ifelse(is.na(rejected_records$Avgas.CC.Utilization.in.last.12.months),
                                                                              median(filter(rejected_records, Avgas.CC.Utilization.in.last.12.months >=72 & Avgas.CC.Utilization.in.last.12.months<=113) [, "Avgas.CC.Utilization.in.last.12.months"]),
                                                                          rejected_records$Avgas.CC.Utilization.in.last.12.months)

sum(is.na(rejected_records$Avgas.CC.Utilization.in.last.12.months_imputed))
# [1] 0

summary(customer_master_data)
summary(rejected_records)

# ------------------------------------------------------------------- Feature Engineering - Encoding/Dummy Variables ---------------------------------------------------------------------------------------------------
# Using No.of.dependents as numerical only
# customer_master_data$No.of.dependents <- as.factor(customer_master_data$No.of.dependents)
# customer_master_data$No.of.dependents <- as.numeric(customer_master_data$No.of.dependents)

# Gender
customer_master_data$Gender <- as.factor(customer_master_data$Gender)
levels(customer_master_data$Gender) <- c(1,0)

# Marital Status
customer_master_data$Marital.Status..at.the.time.of.application. <- as.factor(customer_master_data$Marital.Status..at.the.time.of.application.)
levels(customer_master_data$Marital.Status..at.the.time.of.application.) <- c(1,0)

# Type of Residence
customer_master_data$Type.of.residence <- as.factor(customer_master_data$Type.of.residence)

# One-Hot encoding for Education_imputed
customer_master_data$Education_imputed <- as.factor(customer_master_data$Education_imputed)
dummy_education <- data.frame(model.matrix(~Education_imputed,
                                           data=customer_master_data))
dummy_education <- dummy_education[,-1]
customer_master_data <- cbind(customer_master_data, dummy_education)

# One-Hot encoding for Profession
customer_master_data$Profession <- as.factor(customer_master_data$Profession)
dummy_profession <- data.frame(model.matrix(~Profession,
                                            data=customer_master_data))
dummy_profession <- dummy_profession[,-1]
customer_master_data <- cbind(customer_master_data, dummy_profession)

# One-Hot encoding for Residence Type
customer_master_data$Type.of.residence <- as.factor(customer_master_data$Type.of.residence)
dummy_residencetype <- data.frame(model.matrix(~Type.of.residence,
                                               data=customer_master_data))
dummy_residencetype <- dummy_residencetype[,-1]
customer_master_data <- cbind(customer_master_data, dummy_residencetype)

# Creating a .CSV file with WoE values
write.csv(customer_master_data,"customer_master_data_cleaned_WoE_feature_engineering.csv")


# ------------------------------------------------------------------- Exploratory Data Analysis (EDA) ---------------------------------------------------------------------------------------------------
graph_data <- customer_master_data

# Columns considered to convert into factor
toFactor_colname <- c("Gender","Marital.Status..at.the.time.of.application.","No.of.dependents","Education","Profession"
                      ,"Income_bin","age_bin","current_residence_bin","current_company_bin","Type.of.residence"
                      ,"Presence.of.open.auto.loan","Presence.of.open.home.loan","No.of.times.30.DPD.or.worse.in.last.12.months"
                      ,"No.of.times.30.DPD.or.worse.in.last.6.months","No.of.times.60.DPD.or.worse.in.last.12.months"
                      ,"No.of.times.60.DPD.or.worse.in.last.6.months","No.of.times.90.DPD.or.worse.in.last.12.months"
                      ,"No.of.times.90.DPD.or.worse.in.last.6.months")


graph_data[toFactor_colname] <- lapply(graph_data[toFactor_colname],factor)

graph_data$Performance <- as.factor(ifelse(graph_data$Performance==0,"Non-Defaulters","Defaulters"))

ggplot(graph_data,aes(x=Performance,fill=Performance)) +
  geom_bar() +  
  geom_text(stat = "count", aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))),vjust =-0.25)
# Only 4.2% defaulters and this is an unbalanced data

str(graph_data)

graph_data_categorical <- graph_data[,sapply(graph_data,is.factor)]
graph_data_continuous <- graph_data[,!sapply(graph_data,is.factor)]
graph_data_continuous <- graph_data_continuous[,-c(1,2)]

graph_data_categorical <- graph_data_categorical %>% dplyr::select(-Performance,Performance)

View(graph_data_categorical)
View(graph_data_continuous)

## Independent factor variables graph details
gather(graph_data_categorical, x, y, Gender:current_company_bin) %>%
  ggplot(aes(x = y, color = Performance, fill = Performance)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

# Independent continuous variable graph
# Histograms
# Excluding variables of Type - Original with NA/Missing Values, Dummy Variables & WoE Values
graph_data_continuous [, -c(1, 4, 11, 14, 15, 16, 17, 19, 21, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32) ] %>% 
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap( ~ key, scales = "free") + geom_histogram()

## boxplots
graph_data_continuous_1 <- graph_data_continuous %>% dplyr::select(-Outstanding.Balance,Outstanding.Balance)
new_col_names <- c("Age","Income","No.curr.resi","No.curr.comp",
                   "AvgCC.Util.12","trades_6","trades_12",
                   "PL_6","PL_12","inq_6_auto","In_12_auto",
                   "total_trade","Perf","Outstanding.balance")

colnames(graph_data_continuous_1) <- new_col_names

temp <- melt(graph_data_continuous_1[,1:13],id.vars = "Perf")
ggplot(data = temp, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Perf))

boxplot(graph_data_continuous_1$Outstanding.balance)



#---------------- Multivariate analysis -------------------

graph_data_multivariate <- dplyr::filter(graph_data,graph_data$Performance=="Defaulters")

ggplot(data = graph_data_multivariate, aes(x = age_bin,y=Performance, fill = Income_bin)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
           position = "dodge") + 
  geom_text(aes(y = round(prop.table(..count..) * 100 + 0.5,2), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 3) + 
  labs(x = 'Age group', y = 'Defaulters', fill = 'Income Group')

ggplot(data = graph_data_multivariate, aes(x = age_bin,y=Performance, fill = Gender)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
           position = "dodge") + 
  geom_text(aes(y = round(prop.table(..count..) * 100 + 0.5,2), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 3) + 
  labs(x = 'Age group', y = 'Defaulters', fill = 'Gender')

ggplot(data = graph_data_multivariate, aes(x = Income_bin,y=Performance, fill = Gender)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
           position = "dodge") + 
  geom_text(aes(y = round(prop.table(..count..) * 100 + 0.5,2), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 3) + 
  labs(x = 'Income group', y = 'Defaulters', fill = 'Gender')

ggplot(data = graph_data_multivariate, aes(x = No.of.dependents,y=Performance, fill = Income_bin)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
           position = "dodge") + 
  geom_text(aes(y = round(prop.table(..count..) * 100 + 0.5,2), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 3) + 
  labs(x = 'Number of Dependents', y = 'Defaulters', fill = 'Income group')

ggplot(data = graph_data_multivariate, aes(x = Type.of.residence,y=Performance, fill = Income_bin)) +
  geom_bar(aes(y = prop.table(..count..) * 100),
           position = "dodge") + 
  geom_text(aes(y = round(prop.table(..count..) * 100 + 0.5,2), 
                label = paste0(round(prop.table(..count..) * 100,2), '%')), 
            stat = 'count', 
            position = position_dodge(.9), 
            size = 3) + 
  labs(x = 'Type of Residence', y = 'Defaulters', fill = 'Income group')
# ---------------------------------------------------------------------------------------------------------------------------------------
# EDA Inferences
# ---------------------------------------------------------------------------------------------------------------------------------------
# Variable                                                        Importance        Conclusion
# ---------------------------------------------------------------------------------------------------------------------------------------
# Age                                                             - High            - Age group of 35-55 is significant
# Gender                                                          - Low             - Not significant feature
# Marital Status                                                  - Medium          - EDA also confirms Married Significant
# No of dependents                                                - Low             - Not significant feature
# Income                                                          - Low             - Not significant feature
# Education                                                       - Low             - Not significant feature
# Profession                                                      - High            - Salaried is significant with high frequency
# Type of residence                                               - High            - Rented is the most significant with high
# No of months in current residence                               - High            - < 12 months is high frequency
# No of months in current company                                 - Medium          - EDA also confirms <24 Months has significant default
# No of times 90 DPD or worse in last 6 months                    - Medium          - Higher the number has default effect
# No of times 60 DPD or worse in last 6 months                    - Medium          - Higher the number has default effect
# No of times 30 DPD or worse in last 6 months                    - Medium          - Higher the number has default effect
# No of times 90 DPD or worse in last 12 months                   - Medium          - Higher the number has default effect
# No of times 60 DPD or worse in last 12 months                   - Medium          - Higher the number has default effect
# No of times 30 DPD or worse in last 12 months                   - Low             - Not significant feature
# Avgas CC Utilization in last 12 months                          - High            - Most of the utilization are <20
# No of trades opened in last 6 months                            - Low             - Not significant feature
# No of trades opened in last 12 months                           - Low             - Not significant feature
# No of PL trades opened in last 6 months                         - Low             - Not significant feature
# No of PL trades opened in last 12 months                        - Low             - Not significant feature                       
# No of Inquiries in last 6 months (excluding home & auto loans)  - Low             - Not significant feature
# No of Inquiries in last 12 months (excluding home & auto loans) - Low             - Not significant feature
# Presence of open home loan                                      - Low             - Not significant feature
# Outstanding Balance                                             - Low             - Not significant feature
# Total No of Trades                                              - Low             - Not significant feature
# Presence of open auto loan                                      - Low             - Not significant feature
# ---------------------------------------------------------------------------------------------------------------------------------------
# End of EDA
# ------------------------------------------------------------------- Feature Selection - Based on WoE/IV and Correlation Analysis  ---------------------------------------------------------------------------------------------------

# Feature Selection based on WoE/IV = 0.02 to 0.5
arrange(IV_Value [IV_Value$IV >=0.02, ], desc(IV))[1]

#                                                           Variable
# 1                           Avgas.CC.Utilization.in.last.12.months
# 2                            No.of.trades.opened.in.last.12.months
# 3                         No.of.PL.trades.opened.in.last.12.months
# 4  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.
# 5                                              Outstanding.Balance
# 6                     No.of.times.30.DPD.or.worse.in.last.6.months
# 7                                               Total.No.of.Trades
# 8                          No.of.PL.trades.opened.in.last.6.months
# 9                    No.of.times.90.DPD.or.worse.in.last.12.months
# 10                    No.of.times.60.DPD.or.worse.in.last.6.months
# 11  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.
# 12                   No.of.times.30.DPD.or.worse.in.last.12.months
# 13                            No.of.trades.opened.in.last.6.months
# 14                   No.of.times.60.DPD.or.worse.in.last.12.months
# 15                    No.of.times.90.DPD.or.worse.in.last.6.months
# 16                               No.of.months.in.current.residence
# 17                                           current_residence_bin
# 18                                                  Income_imputed
# 19                                                          Income
# 20                                                      Income_bin
# 21                                 No.of.months.in.current.company

#---------------- Correlation analysis -------------------
# Correlation matrix
features_for_correlationMatrix <-
  c(
    "Income_imputed",
    "No.of.months.in.current.company",
    "No.of.months.in.current.residence",
    
    "Avgas.CC.Utilization.in.last.12.months_WoE",
    "Avgas.CC.Utilization.in.last.12.months_imputed",
    "Outstanding.Balance_WoE",
    "Outstanding.Balance_imputed",
    #"Presence.of.open.home.loan_WoE",
    #"Presence.of.open.home.loan_imputed",
    
    
    "Total.No.of.Trades",
    "No.of.trades.opened.in.last.6.months",
    "No.of.trades.opened.in.last.12.months",
    "No.of.PL.trades.opened.in.last.6.months",
    "No.of.PL.trades.opened.in.last.12.months",
    
    "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
    "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
    
    "No.of.times.30.DPD.or.worse.in.last.6.months",
    "No.of.times.60.DPD.or.worse.in.last.6.months",
    "No.of.times.90.DPD.or.worse.in.last.6.months",
    "No.of.times.30.DPD.or.worse.in.last.12.months",
    "No.of.times.60.DPD.or.worse.in.last.12.months",
    "No.of.times.90.DPD.or.worse.in.last.12.months"
  )
plot_correlationMatrix (customer_master_data, features_for_correlationMatrix)
# Correlation Analysis
# --------------------
# 1. All DPD columns are highly correlated (0.8 to 0.95), chosing one variable with high IV value range (0.02 to 0.5)
#    i.e. "No.of.times.30.DPD.or.worse.in.last.6.months"
# 2. All Trades related features are highly correlated 0.6 to 0.94, chosing only with high WoE value
#    i.e. No.of.trades.opened.in.last.12.months"
# 3. Feature "Outstanding.Balance_WoE" is correlated (0.65) with "Avgas.CC.Utilization.in.last.12.months_WoE" but considering 
#    based on business intution. Also considering "Outstanding.Balance_imputed"
# 4. Discarding both     "Presence.of.open.home.loan_WoE" and "Presence.of.open.home.loan_imputed" because they both are highly 
#    correlated (0.93 and 0.94 respectively) with "Outstanding.Balance_imputed"

# -------------------- Final Feature selection
# Following are list of features which are not having high correlation -0.5 to 0.5 with other featues

demographic_data_features <- c("Income_imputed",
                               "No.of.months.in.current.company",
                               "No.of.months.in.current.residence")

creditbureau_data_features <- c(    "Avgas.CC.Utilization.in.last.12.months_WoE",
                                    "Avgas.CC.Utilization.in.last.12.months_imputed",
                                    "Outstanding.Balance_WoE",
                                    "Outstanding.Balance_imputed",
                                    "No.of.times.30.DPD.or.worse.in.last.6.months",
                                    "No.of.trades.opened.in.last.12.months")

# ------------------------------------------------------------------- Model Building - Split Train (and Validation) & Test Datasets  --------------------------------------------------------------------------------------------------- 

# Not using scale technique as there is no gain/loss with model performance/accuracy metrics
# customer_master_data[, scale_col] <- sapply(customer_master_data[, scale_col], scale)

View(customer_master_data)


set.seed(100)
# Randomly divide the data into training and test sets (stratified by class)
index <- createDataPartition(customer_master_data$Performance, p = 0.7, list = FALSE)
train_data <- customer_master_data[index, ]
summary(train_data$Performance)
#     0     1 
# 46800  2063 
2063/(46800+2063)*100
# 4.222008% of Minority Class - 1

test_data  <- customer_master_data[-index, ]
summary(test_data$Performance)
# 0     1 
# 20056   883 
883/(20056+883)*100
# 4.217011% of Minority Class - 1

test_actual_default <- factor(ifelse(test_data$Performance ==1,"Yes","No"))

#-------------------------------------------------------------------- Model Evaluation - Common Functions ---------------------------------------------------------------------

# Function for Choosing the optimal probalility cutoff
perform_fn <- function(cutoff, test_data_prediction) 
{
  predicted_default <- factor(ifelse(test_data_prediction >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Function for calculating Optimal Cutoff
findOptimalCutOff <- function (test_data_prediction, thresholdStart, thresholdEnd) {
  # Summary of test probability
  summary(test_data_prediction)
  
  #s = seq(.03,.14,length=100)
  s = seq(thresholdStart, thresholdEnd,length=100)
  OUT = matrix(0,100,3)
  
  for(i in 1:100)
  {
    OUT[i,] = perform_fn(s[i], test_data_prediction)
  } 
  
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0.75,0.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
  
  cat("Optimal Cutoff = ", round(cutoff,3)[1])
  cat ("\n\n")
  
  return(round(cutoff,3)[1])
}

## Function for calculation of lift and cumulative gain
lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# For plotting the gain chart and to compute KS Statistic
GainLiftChart_KSStatistic <- function(model,data, value) {
  
  temp <- data
  temp$Predict <- predict(model,type=value,newdata=temp)
  
  LG = lift(temp$Performance, temp$Predict, groups = 10)
  # Gain Chart 
  plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
  
  # Lift Chart 
  plot(LG$bucket,LG$Cumlift,col="blue",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")
  write.csv(LG,"Lift-CumulativeGain-table.csv")
  
  # KS-Statistic
  if(value=="raw"){
    pred_object_test<- prediction(as.numeric(temp$Predict), as.numeric(temp$Performance))
  }else{
    pred_object_test<- prediction(temp$Predict,temp$Performance)
  }
  
  performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
  ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
    (attr(performance_measures_test, "x.values")[[1]])
  
  #LG$KS <- ks_table_test
  print(LG)
  max(ks_table_test)
  
}

evaluateClassificationModel <- function (test_pred, test_actual_default, cutOff) {  
  
  # Get optimal cut-off 
  #cutOff <- findOptimalCutOff(test_pred, thresholdStart, thresholdEnd)
  
  test_pred_default <- factor(ifelse(test_pred >= cutOff, "Yes", "No"))
  table(test_actual_default,test_pred_default)
  
  #install.packages("e1071")
  library(e1071)
  
  test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
  acc <- test_conf$overall[1]
  sens <- test_conf$byClass[1]
  spec <- test_conf$byClass[2]
  
  print(test_conf)
  
  precission_recall_f <- accuracy.meas(test_actual_default, test_pred_default, cutOff)
  
  # Using only F score 
  
  #print(cat("F Score = ", precission_recall_f$F))
  # precision: 0.042
  # recall: 1.000
  # F: 0.040
  
  # Precision can be seen as a measure of exactness or quality, whereas recall is a measure of completeness or quantity. 
  # In simple terms, high precision means that an algorithm returned substantially more relevant results than irrelevant ones, 
  # while high recall means that an algorithm returned most of the relevant results.
  
  roc_metrics <- roc.curve(test_actual_default, test_pred_default, plotit = T)
  
  
  metrics <- data.frame(Accuracy=acc, 
                        Sensitivity=sens, 
                        Specificity = spec,
                        F_score=precission_recall_f$F,
                        Threshold=precission_recall_f$threshold,
                        AUC=roc_metrics$auc,
                        False_positive_Rate=roc_metrics$false.positive.rate[2],
                        True_positive_Rate=roc_metrics$true.positive.rate[2])
  
  print(metrics)
  
  return(metrics)
}
# ------------------------------------------------------------------- Model Building (Unbalanced) -  Logistic Regression with Demographic Data ---------------------------------------------------------------------------------------------------
# Simple Logistic Regression model Using Demographic data 
# Using WoE Variables as derived features in addition original features (imputed for mising / incorrect values)
# As WoE is created based on Target Encoding, in general is not directly correlated with base variable

# Using WoE Variable (Note: results are same with Income_imputed as well instead of Income_imputed_WoE)
# Also Income_imputed becomes significant when both are used and they both are highly correlated

logistic_model_demographic_data_unbalanced <- glm(formula = Performance ~  No.of.months.in.current.residence + 
                                                                Income_imputed_WoE +
                                                                No.of.months.in.current.company, 
                                                 family = "binomial", 
                                                 data = train_data [, -1])
summary (logistic_model_demographic_data_unbalanced)
# AIC: 16983
sort(vif(logistic_model_demographic_data_unbalanced),decreasing = TRUE)
# Income_imputed_WoE No.of.months.in.current.residence   No.of.months.in.current.company 
# 1.017824                          1.016867                          1.016003 

test_pred = predict(logistic_model_demographic_data_unbalanced, type = "response", 
                    newdata = test_data)

summary(test_pred)
cutOff <- findOptimalCutOff(test_pred, .03,.14)
# Optimal Cutoff =  0.042

logistic_model_demographic_data_metrics <- evaluateClassificationModel(test_pred,
                            test_actual_default, 
                            cutOff)

rownames(logistic_model_demographic_data_metrics) <- "DemographicData - GLM - Unbalanced"
model_Metrics <- rbind(logistic_model_demographic_data_metrics)

# test_pred_default <- factor(ifelse(test_pred >= 0.040, "Yes", "No"))
# Accuracy    : 0.5373
# Sensitivity : 0.55606
# Specificity : 0.53650
# 
# F           : 0.040
#
# Area under the curve (AUC): 0.546

# ------------------------------------------------------------------- Model Building (Unbalanced) -  Logistic Regression with Demographic & Credit Bureau Data ---------------------------------------------------------------------------------------------------

# Using WoE Variables as derived features in addition original features (imputed for mising / incorrect values)
# As WoE is created based on Target Encoding, in general is not directly correlated with base variable

logistic_model_application_and_creditdata <- glm(formula = Performance ~ Income_imputed + 
                                                                         No.of.months.in.current.company +
                                                                         No.of.months.in.current.residence +
                                                                         Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                         Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                         Outstanding.Balance_WoE +
                                                                         Outstanding.Balance_imputed +
                                                                         No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                         No.of.trades.opened.in.last.12.months, 
                                                 family = "binomial", 
                                                 data = train_data [, -1])
summary (logistic_model_application_and_creditdata)
# AIC: 16374

logistic_model_application_and_creditdata_2 <- stepAIC(logistic_model_application_and_creditdata, direction="both")

summary(logistic_model_application_and_creditdata_2)
# AIC: 16372

# Removing Income_imputed as p-value = 0.142681
logistic_model_application_and_creditdata_3 <- glm(formula = Performance ~ 
                                                                           No.of.months.in.current.company +
                                                                           No.of.months.in.current.residence +
                                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                           Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                           Outstanding.Balance_WoE +
                                                                           Outstanding.Balance_imputed +
                                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                           No.of.trades.opened.in.last.12.months,
                                         family = "binomial", 
                                         data = train_data[, -1])

summary(logistic_model_application_and_creditdata_3)
# AIC: 16374
sort(vif(logistic_model_application_and_creditdata_3),decreasing = TRUE)

# Removing Outstanding.Balance_imputed due to high p-value = 0.564429
logistic_model_application_and_creditdata_4 <- glm(formula = Performance ~ 
                                                                           No.of.months.in.current.company +
                                                                           No.of.months.in.current.residence +
                                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                           Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                           Outstanding.Balance_WoE +
                                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                           No.of.trades.opened.in.last.12.months,
                                         family = "binomial", 
                                         data = train_data[, -1])

summary(logistic_model_application_and_creditdata_4)
# AIC: 16372
sort(vif(logistic_model_application_and_creditdata_4),decreasing = TRUE)

# Removing No.of.months.in.current.company as p-value = 0.08977
logistic_model_application_and_creditdata_5 <- glm(formula = Performance ~  
                                                                           No.of.months.in.current.residence +
                                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                           Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                           Outstanding.Balance_WoE +
                                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                           No.of.trades.opened.in.last.12.months,
                                         family = "binomial", 
                                         data = train_data[, -1])

summary(logistic_model_application_and_creditdata_5)
# AIC: 16368
sort(vif(logistic_model_application_and_creditdata_5),decreasing = TRUE)

# Removing No.of.months.in.current.residence due to high p-value = 0.099945
logistic_model_application_and_creditdata_6 <- glm(formula = Performance ~  
                                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                           Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                           Outstanding.Balance_WoE +
                                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                           No.of.trades.opened.in.last.12.months,
                                                   
                                         family = "binomial", data = train_data[, -1])
summary(logistic_model_application_and_creditdata_6)
# AIC: 16374
sort(vif(logistic_model_application_and_creditdata_6),decreasing = TRUE)

# Removing Avgas.CC.Utilization.in.last.12.months_imputed
# Avgas.CC.Utilization.in.last.12.months_WoE - VIF=2.690655, p-value=5.59e-13
# Avgas.CC.Utilization.in.last.12.months_imputed - VIF=1.909329 ,p-value=0.008180
cor(train_data$Avgas.CC.Utilization.in.last.12.months_imputed, train_data$Avgas.CC.Utilization.in.last.12.months_WoE)
# -0.7412454

logistic_model_application_and_creditdata_7 <- glm(formula = Performance ~  
                                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                           Outstanding.Balance_WoE +
                                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                           No.of.trades.opened.in.last.12.months,
                                                   family = "binomial", data = train_data[, -1])
summary(logistic_model_application_and_creditdata_7)
# AIC: 16378
sort(vif(logistic_model_application_and_creditdata_7),decreasing = TRUE)
# Outstanding.Balance_WoE   Avgas.CC.Utilization.in.last.12.months_WoE        No.of.trades.opened.in.last.12.months No.of.times.30.DPD.or.worse.in.last.6.months 
#                1.954735                                     1.776972                                     1.297773                                     1.292529 

logistic_model_application_and_creditdata_unbalanced <- logistic_model_application_and_creditdata_7

test_pred = predict(logistic_model_application_and_creditdata_unbalanced, type = "response", 
                    newdata = test_data)


summary(test_pred)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.01590 0.01915 0.03548 0.04245 0.06008 0.17121
cutOff <- findOptimalCutOff(test_pred, .01,.17)
# Optimal Cutoff =  0.049

logistic_model_application_and_creditdata_unbalanced_metrics <- evaluateClassificationModel(test_pred,
                            test_actual_default, 
                            cutOff)
rownames(logistic_model_application_and_creditdata_unbalanced_metrics) <- "FullData        - GLM - Unbalanced"
model_Metrics <- rbind(model_Metrics, logistic_model_application_and_creditdata_unbalanced_metrics)
# Optimal Cutoff =  0.049

# Accuracy    : 0.6334
# Sensitivity : 0.61721
# Specificity : 0.63407
#
# F           : 0.04046375
#
# Area under the curve (AUC): 0.6256443
#
#
# ---------------------------------------------- Cross Validation & Sampling with Demographic and Credit Bureau Data ------------------------------------------------------------------------
#
# -------------------------------- Logistic Regression - Using Under Sampling

t8 <- Sys.time()
model_glm_fullCustomerData_undersampling <- caret::train(Performance ~  
                                                                       Income_imputed + 
                                                                       No.of.months.in.current.company +
                                                                       No.of.months.in.current.residence +
                                                                       Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                       Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                       Outstanding.Balance_WoE +
                                                                       Outstanding.Balance_imputed +
                                                                       No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                       No.of.trades.opened.in.last.12.months,
                                                        data = train_data [, -1],
                                                        method = "glm",
                                                        family="binomial",
                                                        preProcess = c("scale", "center"),
                                                        tuneLength = 5,
                                                        trControl = trainControl(method = "cv", 
                                                                                 number = 5, 
                                                                                 verboseIter = TRUE,
                                                                                 sampling = "down"))

test_pred_fullCustomerData_glm_undersampling <- predict(model_glm_fullCustomerData_undersampling, 
                                                       type = "prob",
                                                       newdata = test_data)
t9 <- Sys.time()

t9-t8
# Time difference of 2.016878 secs

summary(test_pred_fullCustomerData_glm_undersampling[,2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2164  0.2935  0.4670  0.4590  0.6035  0.8538 
cutOff <- findOptimalCutOff(test_pred_fullCustomerData_glm_undersampling[,2], 0.20, 0.85)
# Optimal Cutoff =  0.541

model_glm_fullCustomerData_undersampling_metrics <- evaluateClassificationModel(test_pred_fullCustomerData_glm_undersampling[,2],
                            test_actual_default, 
                            cutOff)
rownames(model_glm_fullCustomerData_undersampling_metrics) <- "FullData        - GLM - Under-Sampling"
model_Metrics <- rbind(model_Metrics, model_glm_fullCustomerData_undersampling_metrics)

# Accuracy    : 0.6345
# Sensitivity : 0.62288
# Specificity : 0.06988
#
# F: 0.04046375
#
# Area under the curve (AUC): 0.6289243

# -------------------------------- Logistic Regression - Using Over Sampling

t8 <- Sys.time()
model_glm_fullCustomerData_oversampling <- caret::train(Performance ~  
                                                           Income_imputed + 
                                                           No.of.months.in.current.company +
                                                           No.of.months.in.current.residence +
                                                           Avgas.CC.Utilization.in.last.12.months_WoE +
                                                           Avgas.CC.Utilization.in.last.12.months_imputed +
                                                           Outstanding.Balance_WoE +
                                                           Outstanding.Balance_imputed +
                                                           No.of.times.30.DPD.or.worse.in.last.6.months +
                                                           No.of.trades.opened.in.last.12.months,
                                                         data = train_data [, -1],
                                                         method = "glm",
                                                         family="binomial",
                                                         preProcess = c("scale", "center"),
                                                         tuneLength = 5,
                                                         trControl = trainControl(method = "cv", 
                                                                                  number = 5, 
                                                                                  verboseIter = TRUE,
                                                                                  sampling = "up"))


test_pred_fullCustomerData_glm_oversampling <- predict(model_glm_fullCustomerData_oversampling, 
                                                        type = "prob",
                                                        newdata = test_data)
t9 <- Sys.time()

t9-t8
# Time difference of 7.105892 secs

summary(test_pred_fullCustomerData_glm_oversampling[,2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2224  0.3040  0.4597  0.4587  0.5897  0.8765  
cutOff <- findOptimalCutOff(test_pred_fullCustomerData_glm_oversampling[,2], 0.2, 0.87)
# Optimal Cutoff =  0.532

model_glm_fullCustomerData_oversampling_metrics <- evaluateClassificationModel(test_pred_fullCustomerData_glm_oversampling[,2],
                            test_actual_default, 
                            cutOff)
rownames(model_glm_fullCustomerData_oversampling_metrics) <- "FullData        - GLM - Over-Sampling"
model_Metrics <- rbind(model_Metrics, model_glm_fullCustomerData_oversampling_metrics)

# Accuracy    : 0.6264865
# Sensitivity : 0.6375991
# Specificity : 0.6259972
#
# F: 0.04046375
#
# Area under the curve (AUC): 0.6317982
#
# -------------------------------- Logistic Regression - Using SMOTE
t8 <- Sys.time()
model_glm_fullCustomerData_smote <- caret::train(Performance ~  
                                                          Income_imputed + 
                                                          No.of.months.in.current.company +
                                                          No.of.months.in.current.residence +
                                                          Avgas.CC.Utilization.in.last.12.months_WoE +
                                                          Avgas.CC.Utilization.in.last.12.months_imputed +
                                                          Outstanding.Balance_WoE +
                                                          Outstanding.Balance_imputed +
                                                          No.of.times.30.DPD.or.worse.in.last.6.months +
                                                          No.of.trades.opened.in.last.12.months,
                                                        data = train_data [, -1],
                                                        method = "glm",
                                                        family="binomial",
                                                        preProcess = c("scale", "center"),
                                                        tuneLength = 5,
                                                        trControl = trainControl(method = "cv", 
                                                                                 number = 5, 
                                                                                 verboseIter = TRUE,
                                                                                 sampling = "smote"))

test_pred_fullCustomerData_glm_smote <- predict(model_glm_fullCustomerData_smote, 
                                                       type = "prob",
                                                       newdata = test_data)
t9 <- Sys.time()

t9-t8
# Time difference of 10.60025 secs

summary(test_pred_fullCustomerData_glm_smote[,2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.1617  0.2449  0.3912  0.3948  0.5260  0.8312  
cutOff <- findOptimalCutOff(test_pred_fullCustomerData_glm_smote[,2], 0.16, 0.83)
# Optimal Cutoff =  0.458

model_glm_fullCustomerData_smotesampling_metrics <- evaluateClassificationModel(test_pred_fullCustomerData_glm_smote[,2],
                            test_actual_default, 
                            cutOff)
rownames(model_glm_fullCustomerData_smotesampling_metrics) <- "FullData        - GLM - SMOTE-Sampling"
model_Metrics <- rbind(model_Metrics, model_glm_fullCustomerData_smotesampling_metrics)

# Accuracy    : 0.6270118
# Sensitivity : 0.6398641
# Specificity : 0.626446
#
# F: 0.04046375
#
# Area under the curve (AUC): 0.6326885

# -------------------------------- Decision Tree - Using Under-Sampling
#           Accuracy Sensitivity Specificity    F_score Threshold       AUC False_positive_Rate True_positive_Rate
# Accuracy 0.05372749   0.9852775   0.0127144 0.04046375      0.05 0.5010041           0.9852775          0.9872856
# Discarding this Under-sampling options for Decision Tree

# -------------------------------- Decision Tree - Using Over-Sampling
#           Accuracy Sensitivity Specificity    F_score Threshold       AUC False_positive_Rate True_positive_Rate
# Accuracy 0.8581594   0.1313703   0.8901576 0.04046375      0.05 0.5107639           0.1098424          0.1313703
# Discarding this over-sampling options for Decision Tree
# -------------------------------- Decision Tree - Using SMOTE

t8 <- Sys.time()
model_rpart_fullCustomerData_smote <- caret::train(Performance ~  
                                                                 Income_imputed + 
                                                                 No.of.months.in.current.company +
                                                                 No.of.months.in.current.residence +
                                                                 #Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                 Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                 #Outstanding.Balance_WoE +
                                                                 Outstanding.Balance_imputed +
                                                                 No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                 No.of.trades.opened.in.last.12.months,
                                                          data = train_data [, -1],
                                                          method = "rpart",
                                                          #preProcess = c("scale", "center"),
                                                          #minsplit=30, minbucket = 15, cp=0.0001,
                                                          tuneLength = 5,
                                                          trControl = trainControl(method = "cv", 
                                                                                   number = 5, 
                                                                                   verboseIter = TRUE,
                                                                                   sampling = "smote"))
# Fitting cp = 0.000388 on full training set
t9 <- Sys.time()
t9-t8
# Time difference of 16.57429 secs

# # plot(model_rpart_fullCustomerData_smote)
# # prp(model_rpart_fullCustomerData_smote, box.palette = "Reds", tweak = 1.2)
# library(rpart.plot)
# rpart.plot(model_rpart_fullCustomerData_smote$finalModel)

test_pred_fullCustomerData_rpart_smote <- predict(model_rpart_fullCustomerData_smote, 
                                                         type = "prob",
                                                         newdata = test_data)

summary(test_pred_fullCustomerData_rpart_smote[,2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1083  0.1765  0.2533  0.2500  1.0000   
cutOff <- findOptimalCutOff(test_pred_fullCustomerData_rpart_smote[,2], 0.1, 0.25)
# Optimal Cutoff =  0.206

model_rpart_fullCustomerData_smotesampling_metrics <- evaluateClassificationModel(test_pred_fullCustomerData_rpart_smote[,2],
                            test_actual_default, 
                            cutOff)

rownames(model_rpart_fullCustomerData_smotesampling_metrics) <- "FullData        - RPART - SMOTE-Sampling"
model_Metrics <- rbind(model_Metrics, model_rpart_fullCustomerData_smotesampling_metrics)

# Accuracy    : 0.6015569
# Sensitivity : 0.599094
# Specificity : 0.6016653
#
# F: 0.04046375
#
# Area under the curve (AUC): 0.6003797

# -------------------------------- Random Forest - Using Under-Sampling
# Remove code snippet related to this model
#           Accuracy Sensitivity Specificity    F_score Threshold       AUC False_positive_Rate True_positive_Rate
# Accuracy 0.6067147   0.6070215   0.6067012 0.04046375     0.522 0.6068614           0.3932988          0.6070215
# Discarding this Under-sampling options for RF

# -------------------------------- Random Forest - Using Over-Sampling
# Remove code snippet related to this model
#           Accuracy Sensitivity Specificity    F_score Threshold       AUC False_positive_Rate True_positive_Rate
# Accuracy 0.5677444   0.5934315   0.5666135 0.04046375      0.05 0.5800225           0.4333865          0.5934315
# Also takes 50+ mins to build model. So, discarding RF Over-Sampling option
# Discarding this Over-sampling options for RF

# -------------------------------- Random Forest - Using SMOTE
t12 <- Sys.time()
model_rf_fullCustomerData_DemographicData_smote <- caret::train(Performance ~ 
                                                                  Income_imputed + 
                                                                  No.of.months.in.current.company +
                                                                  No.of.months.in.current.residence +
                                                                  Avgas.CC.Utilization.in.last.12.months_WoE +
                                                                  Avgas.CC.Utilization.in.last.12.months_imputed +
                                                                  Outstanding.Balance_WoE +
                                                                  Outstanding.Balance_imputed +
                                                                  No.of.times.30.DPD.or.worse.in.last.6.months +
                                                                  No.of.trades.opened.in.last.12.months,
                                                               data = train_data [, -1],
                                                               method = "rf",
                                                               ntree = 1000,
                                                               preProcess = c("scale", "center"),
                                                               trControl = trainControl(method = "cv", 
                                                                                        number = 5, 
                                                                                        verboseIter = TRUE,
                                                                                        sampling = "smote"))
t13 <- Sys.time()
t13-t12
# Time difference of 4.712344 mins
test_pred_fullCustomerData_rf_smote <- predict(model_rf_fullCustomerData_DemographicData_smote, 
                                              type = "prob",
                                              newdata = test_data)

summary(test_pred_fullCustomerData_rf_smote[,2])
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0010  0.0980  0.2240  0.2435  0.3650  0.8620 
cutOff <- findOptimalCutOff(test_pred_fullCustomerData_rf_smote[,2], .001,.86)
# Optimal Cutoff =  0.279

model_rf_fullCustomerData_smotesampling_metrics <- evaluateClassificationModel(test_pred_fullCustomerData_rf_smote[,2],
                                                                                test_actual_default, 
                                                                                cutOff)
rownames(model_rf_fullCustomerData_smotesampling_metrics) <- "FullData        - RF - SMOTE-Sampling"
model_Metrics <- rbind(model_Metrics, model_rf_fullCustomerData_smotesampling_metrics)

plot(varImp(object=model_rf_fullCustomerData_DemographicData_smote),main="Random Forest (SMOTE) - Variable Importance")

# Accuracy    : 0.6212
# Sensitivity : 0.62288
# Specificity : 0.62111
#
# F: 0.040
#
# Area under the curve (AUC): 0.622

#---------------------------------------------------------- Evaluation of Models for Final Model Selection ---------------------------------------------------------------------
# Evaluate various metrics across vall models built
# Analyze Accuracy, Sensitivity, Specificity, AUC, F-Score
model_Metrics
# Evaluating based on AUC, F-Score, Sensitivity, Specificity and Accuracy
#                                              Accuracy Sensitivity Specificity    F_score Threshold       AUC False_positive_Rate True_positive_Rate
# DemographicData - GLM   - Unbalanced        0.5392330   0.5537939   0.5385919 0.04046375     0.042 0.5461929           0.4614081          0.5537939
# FullData        - GLM   - Unbalanced        0.6333636   0.6172140   0.6340746 0.04046375     0.049 0.6256443           0.3659254          0.6172140
# FullData        - GLM   - Under-Sampling    0.6344620   0.6228766   0.6349721 0.04046375     0.541 0.6289243           0.3650279          0.6228766
# FullData        - GLM   - Over-Sampling     0.6223793   0.6387316   0.6216594 0.04046375     0.532 0.6301955           0.3783406          0.6387316
# FullData        - GLM   - SMOTE-Sampling    0.6271551   0.6387316   0.6266454 0.04046375     0.458 0.6326885           0.3733546          0.6387316
# FullData        - RF    - SMOTE-Sampling    0.6055208   0.6092865   0.6053550 0.04046375     0.279 0.6073208           0.3946450          0.6092865
# FullData        - RPART - SMOTE-Sampling    0.6110607   0.5922990   0.6118867 0.04046375     0.203 0.6020928           0.3881133          0.5922990
#
# Analyse Lift, Gain and KS-Statistic metrics
model_Metrics$KSStatistic [1] <- GainLiftChart_KSStatistic(logistic_model_demographic_data_unbalanced, test_data,  "response")
model_Metrics$Lift [1] <- 1.1
model_Metrics$Gain [1] <- 59.91
# KS-Statistic = 0.1065518
# A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
# 1      1  2094       131     131  14.83579 1.483579
# 2      2  2094       103     234  26.50057 1.325028
# 3      3  2094       105     339  38.39185 1.279728
# 4      4  2094        88     427  48.35787 1.208947
# 5      5  2094       102     529  59.90940 1.198188
# 6      6  2094        70     599  67.83692 1.130615
# 7      7  2094        88     687  77.80294 1.111471
# 8      8  2094        69     756  85.61721 1.070215
# 9      9  2094        59     815  92.29898 1.025544
# 10     10  2093        68     883 100.00000 1.000000

model_Metrics$KSStatistic [2] <- GainLiftChart_KSStatistic(logistic_model_application_and_creditdata_unbalanced, test_data,  "response")
model_Metrics$Lift [2] <- 1.49
model_Metrics$Gain [2] <- 74.97
# KS-Statistic = 0.2666422
# A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <dbl>   <dbl>     <dbl>    <dbl>
# 1      1  2094       181     181  20.49830 2.049830
# 2      2  2094       149     330  37.37259 1.868630
# 3      3  2094       135     465  52.66138 1.755379
# 4      4  2094       109     574  65.00566 1.625142
# 5      5  2094        88     662  74.97169 1.499434
# 6      6  2094        73     735  83.23896 1.387316
# 7      7  2094        48     783  88.67497 1.266785
# 8      8  2094        50     833  94.33749 1.179219
# 9      9  2094        32     865  97.96149 1.088461
# 10     10  2093        18     883 100.00000 1.000000
model_Metrics$KSStatistic [3] <- GainLiftChart_KSStatistic(model_glm_fullCustomerData_undersampling, test_data,  "raw")
model_Metrics$Lift [3] <- 1.43
model_Metrics$Gain [3] <- 71.80
# KS-Statistic = 0.2532234
# A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <int>   <int>     <dbl>    <dbl>
#   1      1  2094       124     124  14.04304 1.404304
# 2      2  2094       129     253  28.65232 1.432616
# 3      3  2094       148     401  45.41336 1.513779
# 4      4  2094       138     539  61.04190 1.526048
# 5      5  2094        95     634  71.80068 1.436014
# 6      6  2094        47     681  77.12344 1.285391
# 7      7  2094        50     731  82.78596 1.182657
# 8      8  2094        48     779  88.22197 1.102775
# 9      9  2094        50     829  93.88448 1.043161
# 10     10  2093        54     883 100.00000 1.000000
model_Metrics$KSStatistic [4] <- GainLiftChart_KSStatistic(model_glm_fullCustomerData_oversampling, test_data,  "raw")
model_Metrics$Lift [4] <- 1.44
model_Metrics$Gain [4] <- 72.23

# KS-Statistic = 0.2568917
# # A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <int>   <int>     <dbl>    <dbl>
#   1      1  2094       125     125  14.15629 1.415629
# 2      2  2094       133     258  29.21857 1.460929
# 3      3  2094       148     406  45.97961 1.532654
# 4      4  2094       140     546  61.83465 1.545866
# 5      5  2094        90     636  72.02718 1.440544
# 6      6  2094        46     682  77.23669 1.287278
# 7      7  2094        48     730  82.67271 1.181039
# 8      8  2094        48     778  88.10872 1.101359
# 9      9  2094        49     827  93.65798 1.040644
# 10     10  2093        56     883 100.00000 1.000000
model_Metrics$KSStatistic [5] <-GainLiftChart_KSStatistic(model_glm_fullCustomerData_smote, test_data, "raw")
model_Metrics$Lift [5] <- 1.27
model_Metrics$Gain [5] <- 63.75
# KS-Statistic = 0.2153007
# A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <int>   <int>     <dbl>    <dbl>
#   1      1  2094       132     132  14.94904 1.494904
# 2      2  2094       163     295  33.40883 1.670442
# 3      3  2094       153     448  50.73613 1.691204
# 4      4  2094        56     504  57.07814 1.426954
# 5      5  2094        59     563  63.75991 1.275198
# 6      6  2094        54     617  69.87542 1.164590
# 7      7  2094        69     686  77.68969 1.109853
# 8      8  2094        63     749  84.82446 1.060306
# 9      9  2094        69     818  92.63873 1.029319
# 10     10  2093        65     883 100.00000 1.000000

model_Metrics$KSStatistic [6] <-GainLiftChart_KSStatistic(model_rpart_fullCustomerData_smote, test_data, "raw")
model_Metrics$Lift [6] <- 1.08
model_Metrics$Gain [6] <- 54.36
# KS-Statistic = 0.1127819
# # A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain  Cumlift
# <int> <int>     <int>   <int>     <dbl>    <dbl>
#   1      1  2094       149     149  16.87429 1.687429
# 2      2  2094       108     257  29.10532 1.455266
# 3      3  2094        72     329  37.25934 1.241978
# 4      4  2094        78     407  46.09287 1.152322
# 5      5  2094        73     480  54.36014 1.087203
# 6      6  2094        88     568  64.32616 1.072103
# 7      7  2094        73     641  72.59343 1.037049
# 8      8  2094        78     719  81.42695 1.017837
# 9      9  2094        77     796  90.14723 1.001636
# 10     10  2093        87     883 100.00000 1.000000

model_Metrics$KSStatistic [7] <- GainLiftChart_KSStatistic(model_rf_fullCustomerData_DemographicData_smote, test_data, "raw")
model_Metrics$Lift [7] <- 1.03
model_Metrics$Gain [7] <- 51.64
# KS-Statistic = 0.0643504
0.0643504
# A tibble: 10 x 6
# bucket total totalresp Cumresp      Gain   Cumlift
# <int> <int>     <int>   <int>     <dbl>     <dbl>
#   1      1  2094       140     140  15.85504 1.5855040
# 2      2  2094        66     206  23.32956 1.1664779
# 3      3  2094        86     292  33.06908 1.1023028
# 4      4  2094        71     363  41.10985 1.0277463
# 5      5  2094        93     456  51.64213 1.0328426
# 6      6  2094        83     539  61.04190 1.0173650
# 7      7  2094        87     626  70.89468 1.0127811
# 8      8  2094        83     709  80.29445 1.0036806
# 9      9  2094        83     792  89.69422 0.9966025
# 10     10  2093        91     883 100.00000 1.0000000
# [1] 0.06415095

#------------------------------------------------------------------------ Final Model Selection ------------------------------------------------------------------------
# Top 2 Models Selected
# Note : 
# -----
#        1) Discarding Random Forest as it involves high computational resources,
#           and also not providing any better formance
#        2) Discarding GLM/Unbalanced with Full Data, as well because it is trained with unbalanced data

# FullData        - GLM   - SMOTE-Sampling
# FullData        - GLM   - Over-Sampling
#
# Discarding GLM/Unbalanced model though it has highest KS-Statistic value = 0.2666422 as it is based on Unabalanced data
# GLM/Over-Sampling model has better KS-Statistic value = 0.2568917 than GLM/SMOTE model KS-Statistic value = 0.2153007

#final_Model_For_Scorecard <- model_glm_fullCustomerData_oversampling
#final_Model_For_Scorecard$finalModel

final_Model_For_Scorecard <- logistic_model_application_and_creditdata_unbalanced
# Generalized Linear Model 
# 
# 48863 samples
# 9 predictor
# 2 classes: '0', '1' 
# 
# Pre-processing: scaled (9), centered (9) 
# Resampling: Cross-Validated (5 fold) 
# Summary of sample sizes: 39090, 39091, 39091, 39090, 39090 
# Addtional sampling using up-sampling prior to pre-processing
# 
# Resampling results:
#   
#   Accuracy   Kappa     
# 0.5782698  0.04827151

# Coefficients:
#   (Intercept)                                  Income_imputed  
# -0.00420                                        -0.01460  
# No.of.months.in.current.company               No.of.months.in.current.residence  
# -0.03083                                        -0.06005  
# Avgas.CC.Utilization.in.last.12.months_WoE  Avgas.CC.Utilization.in.last.12.months_imputed  
# -0.27858                                         0.11162  
# Outstanding.Balance_WoE                     Outstanding.Balance_imputed  
# -0.12984                                        -0.02812  
# No.of.times.30.DPD.or.worse.in.last.6.months           No.of.trades.opened.in.last.12.months  
# 0.22748                                         0.14213  
# 
# Degrees of Freedom: 93599 Total (i.e. Null);  93590 Residual
# Null Deviance:	    129800 
# Residual Deviance: 120700 	AIC: 120700


#---------------------------------------------------------- Application Scorecard Building  ---------------------------------------------------------------------

App_Scorecard <- function(model,testdataset){
  
  m <- model
  score_data <- testdataset
  score_data$bad <- predict(m,type="response",newdata = score_data[,-12])
  score_data$good <- (1- score_data$bad)
  score_data$odds <- score_data$good/score_data$bad
  score_data$logodds <- log(score_data$odds)

  points0 = 400
  odds0 = 10
  pdo = 20
  factor = pdo / log(2)
  offset = points0 - factor * log( odds0 )
  
  score_data$Score <- offset + factor * score_data$logodds
  
  return(score_data)
}

testdata_scorecard <- App_Scorecard(final_Model_For_Scorecard,test_data)
rejecteddata_scorecard <- App_Scorecard(final_Model_For_Scorecard,rejected_records)

#Optimal Cutoff =  0.049  - for the unbalanced model
points0 = 400
odds0 = 10
pdo = 20
factor = pdo / log(2)
offset = points0 - factor * log( odds0 )

cutoff_prob_from_model = .049
cutoff_logodd <- log((1-cutoff_prob_from_model)/cutoff_prob_from_model)
cutoff_score <- offset + factor * cutoff_logodd
cutoff_score
#419.33

## rejected data analysis
nrow(rejecteddata_scorecard[(rejecteddata_scorecard$Score >= cutoff_score),])
#55
boxplot(rejecteddata_scorecard$Score)

##  With this build we would have got 55 good customers who had been rejected.

## Full data analysis
fulldata <- customer_master_data
fulldata_scorecard <- App_Scorecard(final_Model_For_Scorecard,fulldata)
ggplot(fulldata_scorecard,aes(fulldata_scorecard$Score,fill=fulldata_scorecard$Performance))+ geom_histogram(binwidth = 10,colour="black")

fulldata_scorecard$predict_performance <- ifelse(fulldata_scorecard$bad>=0.049,1,0)
fulldata_scorecard$iswrong <- ifelse(fulldata_scorecard$predict_performance != fulldata_scorecard$Performance,1,0)

percent_of_wrongprediction <- (sum(fulldata_scorecard$iswrong)/nrow(fulldata_scorecard)) * 100
percent_of_wrongprediction

##-------------------  expected credit loss-------------------------S

#Expected loss(c1) = PD * EAD * LGD

#PD = Probability of default of each customer

#EAD = Exposure at default or oustanding

#LGD = Loss given default.

#Lets assume if recovery likelihood is 30% then LDG = 1  - 0.30 = 0.7

#Total loss expected if all customers are bad
fulldata_scorecard$expected_loss = fulldata_scorecard$bad * fulldata_scorecard$Outstanding.Balance_imputed * 0.7
# Calculated on Full data
# Total prospect loss =  2634047450
# (Prob of bad * Exposure at default * Loss given default)
# Expected loss by default customer from model 147718048
# 
# The loss amount of 147718048 can be straight away avoided by not giving loan to default customer prospects 
# However, by looking into the application score card, some customers of default category can be consider at medium risk because they fall in the boundary range. 
# This potential credit loss can be minimized by target those customer, which Credit Score falls within Good and Intermediate. 
# The verification / acquisition cost of Bad Customer can be minimized by this Model 

#Creating a dataframe for loss calculation
potential_credit_loss <- fulldata_scorecard[, c("Performance", "bad","Outstanding.Balance_imputed")]

#Subsetting for the defaulted customers
loss_default_customer <- potential_credit_loss[(potential_credit_loss$Performance == 1),]                                             

#Loss if the model is being used on the defaulted customer
loss_default_customer$loss_model <- as.integer(loss_default_customer$bad * loss_default_customer$Outstanding.Balance_imputed * 0.7)

#Calculating the total expected loss and the loss with the model.
total_expected_loss = sum(fulldata_scorecard$expected_loss)
total_extected_loss_default_cust <- sum(loss_default_customer$loss_model)

print(total_expected_loss)
print(total_extected_loss_default_cust)

#auto rejection rate

auto_rejection_rate <- sum(fulldata_scorecard$predict_performance)/nrow(fulldata_scorecard)
auto_approval_rate = 1 - auto_rejection_rate
auto_approval_rate
# Auto approval rate is 62.64%

# Rejected data analysis
#Number of good customers that is being rejected
nrow(rejecteddata_scorecard[(rejecteddata_scorecard$Score >= cutoff_score),])
#55

boxplot(rejecteddata_scorecard$Score)
# The histograms plots indicates that the number of defaulters decreases after Cut-off Score of 419
# Even though 419 is boundary value with Good and Bad Customers, we can suggest that the boundary range of customers fall between Good and Bad. 

rejecteddata_scorecard$expected_loss = rejecteddata_scorecard$bad * rejecteddata_scorecard$Outstanding.Balance_imputed * 0.7
rejecteddata_loss <- rejecteddata_scorecard[,c("Score","bad","Outstanding.Balance_imputed")]
rejecteddata_cutoff_score <- rejecteddata_loss[(rejecteddata_loss$Score >= 419),]

loss_by_rejected_good_customer <- sum(rejecteddata_cutoff_score$Outstanding.Balance_imputed * 0.7)

# Total prospect loss =  96026810
# (Loss because of the full rejected data)
# 
# Loss due of Rejection of Good customers is 43876837
# 
# The amount of 43876837 would have been gained on using the model because it was the loss by rejection the good customers

rejecteddata_expected_loss <- sum(rejecteddata_scorecard$expected_loss)
print(rejecteddata_expected_loss)
# 96026810
print(loss_by_rejected_good_customer)
# 43876837

