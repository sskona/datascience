######################################################################################################################################
#                                      HR ANALYTICS CASE STUDY 
######################################################################################################################################

# Problem Statement  -----------------------------------------------------------------------------------------------------------------
# HR analytics firm to understand what factors they should focus on, in order to curb 
# attrition. In other words, they want to know what changes they should make to their
# workplace, in order to get most of their employees to stay. Also, they want to know
# which of these variables is most important and needs to be addressed right away.


######################################################################################################################################
# Loading Libraries   ----------------------------------------------------------------------------------------------------------------

library(ggplot2)
library(gridExtra)
library(ggthemes)
library(cowplot)
library(lubridate)
library(stringr)
library(MASS)
library(caret)
library(car)
library(lmtest)
library(dplyr)
library(tidyr)
library(GGally)
library(caTools)

######################################################################################################################################
# Loading Data    ----------------------------------------------------------------------------------------------------------------

# Dataset 1: manager_survey_data.csv
manager_survey_data <- read.csv("manager_survey_data.csv")
str(manager_survey_data)

nrow(unique(manager_survey_data))         # 4410 unique values
sum (sapply(manager_survey_data, is.na))  # No NA values
sapply(manager_survey_data, function(x) length(which(x == "")))  # No blank value


# Dataset 2:  general_data.csv
general_data <- read.csv("general_data.csv")
str(general_data)

nrow(unique(general_data))                      # 4410 unique values
sum(sapply(general_data, is.na))                # 28 NA values. Need analysis
sapply(general_data, function(x) sum(is.na(x))) # 19 NA's in NumComapnies Worked and 9 in TotalWorkingYears
sapply(general_data, function(x) length(which(x==""))) # No blank value


# Dataset 3: employee_survey_data.csv
employee_survey_data <- read.csv("employee_survey_data.csv")
str(employee_survey_data)

nrow(unique(employee_survey_data))                        # 4410 unique values
sum (sapply(employee_survey_data, is.na))                 # 83 NA values. Need analysis
sapply(employee_survey_data , function(x) sum(is.na(x)))  
# 25 NA's in 'EnvironmentSatisficaton' 
# 20 NA's in 'JobSatisfaction' 
# 38 NA's in 'Worklifebalance'

sapply(employee_survey_data, function(x) length(which(x==""))) # No blank value


# Dataset 4: in_time.csv and out_time.csv
in_time   <- read.csv("in_time.csv")
out_time  <- read.csv("out_time.csv")

str(in_time)
str(out_time)

# Checking the structure of the two datasets.
( sum(colnames(in_time) == colnames (out_time)) == ncol(in_time)) & (ncol(in_time) == ncol(out_time))
# Both match

######################################################################################################################################
# Creating Employee Timing Dataframe    ----------------------------------------------------------------------------------------------

# Changing the column name "X" in in_time and out_time
colnames(in_time)[1]  <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

# Columns with all NA values should be a holiday
# Removing all columns in in_time and out_time which have only NA's
in_time  <-  in_time[, colSums(is.na(in_time)) < nrow(in_time)]
out_time <- out_time[, colSums(is.na(out_time)) < nrow(out_time)]

#There are 12 public holidays

# Converting in_time and out_time dataframes from Wide to Long Format
in_time   <- gather(in_time,  Date, time_in,  X2015.01.02:X2015.12.31)
out_time  <- gather(out_time, Date, time_out, X2015.01.02:X2015.12.31)

# Merging the two together
length(unique(in_time$EmployeeID))    # 4410, EmployeeID is key
length(unique(out_time$EmployeeID))   # 4410, EmployeeID is key 

setdiff(in_time$EmployeeID, out_time$EmployeeID)  # All Identical EmployeeID in both dataframes
setdiff(in_time$Date, out_time$Date)              # All Identical Dates in both dataframes

timing <- merge(in_time, out_time, by = c("EmployeeID","Date"))

# Formatting "timing" data in proper dates
timing$Date <- str_replace(timing$Date, "X","")
timing      <- timing %>% mutate(
  Date      = as.POSIXct(Date,     format = "%Y.%m.%d"),
  time_in   = as.POSIXct(time_in,  format = "%Y-%m-%d %H:%M:%S"),
  time_out  = as.POSIXct(time_out, format = "%Y-%m-%d %H:%M:%S"))

# Calculating Hours spend by the Employee in Office
timing$HoursinOffice <- round(as.numeric(timing$time_out - timing$time_in),digits = 2)

# Calculating Leaves taken by employee
# Here the assumption is if in and out times are 'NA' then employee is not present in office and on leave
timing$Leave         <- ifelse((is.na(timing$time_in) & is.na(timing$time_out)),1,0)

# Creating a new Dataframe which has
# avgHours - Average hours spent in office
# avgTimeIn and avgTimeOut - average Time In and Out Time of the Employee.
# Total leaves taken by the employee
AvgTiming <- timing %>% group_by(EmployeeID) %>% 
  summarise(avgHours   = round(mean(HoursinOffice, na.rm = T),digit = 3),
            avgTimeIn  = mean(time_in, na.rm = T),
            avgTimeOut = mean(time_out, na.rm = T),
            totLeaves  = sum(Leave, na.rm = T))

max(AvgTiming$totLeaves)
# Highest number of leaves taken an employee - 24


########################################################################################################################
# Creating Final Dataframe    ------------------------------------------------------------------------------------------

length(unique(general_data$EmployeeID))           # 4410, confirming EmployeeID is key 
length(unique(manager_survey_data$EmployeeID))    # 4410, confirming EmployeeID is key 
length(unique(employee_survey_data$EmployeeID))   # 4410, confirming EmployeeID is key 
length(unique(AvgTiming$EmployeeID))              # 4410, confirming EmployeeID is key 

setdiff(general_data$EmployeeID, manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets
employee <- merge(general_data, manager_survey_data, by = "EmployeeID")

setdiff(employee$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
employee <- merge( employee, employee_survey_data, by = "EmployeeID")

setdiff(employee$EmployeeID,AvgTiming$EmployeeID) # Identical EmployeeID across these datasets
employee <- merge( employee, AvgTiming, by = "EmployeeID")

sum(duplicated(employee)) #NO duplicates
str(employee) #Master file
View(employee)

#######################################################################################################################
# Checking employee    ----------------------------------------------------------------------------------

# No column has NA values more than 1%. No need to remove any columns.
sort(colMeans(is.na(employee)), decreasing = T)

# Columns with NA values are:
#   NumCompaniesWorked
#   TotalWorkingYears
#   EnvironmentSatisfaction
#   JobSatisfaction
#   WorkLifeBalance


# For "NumComapniesWorked" - consider NA as 0
sum(is.na(employee$NumCompaniesWorked)) # 19 NA's
employee$NumCompaniesWorked[is.na(employee$NumCompaniesWorked)] <- 0

# For "TotalWorkingYears" - consider NA as 0
sum(is.na(employee$TotalWorkingYears)) # 9 NA's
employee$TotalWorkingYears[is.na(employee$TotalWorkingYears)] <- 0

# For "EnvironmentSatisfaction" 
sum(is.na(employee$EnvironmentSatisfaction)) # 25 NA's
table(employee$EnvironmentSatisfaction)
#Keeping NA values as '3 - High' as per mode
employee$EnvironmentSatisfaction[is.na(employee$EnvironmentSatisfaction)] <- 3

# For "JobSatisfaction" 
sum(is.na(employee$JobSatisfaction)) # 20 NA's
table(employee$JobSatisfaction)
#Keeping NA as '4 - Very High' as per mode
employee$JobSatisfaction[is.na(employee$JobSatisfaction)] <- 4

# For "WorkLifeBalance"
sum(is.na(employee$WorkLifeBalance)) # 38 NA's
table(employee$WorkLifeBalance)
# Keeping NA as '3 - Better' as per mode
employee$WorkLifeBalance[is.na(employee$WorkLifeBalance)] <- 3

# Converting into factors
cols <- c("Education", 
          "JobLevel", 
          "JobInvolvement", 
          "PerformanceRating" ,
          "EnvironmentSatisfaction",
          "JobSatisfaction",
          "WorkLifeBalance",
          "StockOptionLevel")

employee[cols] <- lapply(employee[cols], factor)
str(employee)

# Removing columns having Zero-Varience i.e same values
employee <- employee[sapply(employee, function(x) length(unique(na.omit(x)))>1)]
# Columns removed - EmployeeCount, Over18, StandardHours

###########################################################################################################################
# Exploratory Data Analysis    --------------------------------------------------------------------------------------------

# Creating Bin for Age
df1<-employee %>% group_by(Age) %>% summarise(n())

employee$AgeBin <- 
  ifelse(employee$Age < 26,'< 26', 
         ifelse(employee$Age > 42, '> 42', '26 - 42'))

#If Attrition is Yes converting it to 1 and 0 incase of No
employee$Attrition <- ifelse(employee$Attrition == "Yes", 1, 0)
employee$Attrition <- as.character(employee$Attrition)

plot_grid(ggplot(employee, aes(x = AgeBin, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Age vs Attrition', x = 'Age'),
          ggplot(employee, aes(x = Gender, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Gender vs Attrition', x = 'Gender'),
          ggplot(employee, aes(x = MaritalStatus, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Marital Status vs Attrition', x = 'Marital Status'),
          ggplot(employee, aes(x = EducationField, fill = Attrition)) +geom_bar() + theme_classic() + labs(title = 'Education Field vs Attrition', x = 'Education Field') +coord_flip()
          + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
          ggplot(employee, aes(x = Education, fill = Attrition)) + geom_bar()+ theme_classic() + labs(title = 'Education vs Attrition', x = 'Education'),
          ncol = 2
)
#Age group 26-42 , Married Employees has highest attrition
#Education Levels 3 and 4 , Departments Life Sciences and Medical have attrition
#Gender has no effect


plot_grid(ggplot(employee, aes(x = BusinessTravel, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Business Travel vs Attrition', x = 'Business Travel'),
          ggplot(employee, aes(x = Department, fill = Attrition)) + geom_bar()+ theme_classic() + labs(title = 'Department vs Attrition', x = 'Department'),
          ggplot(employee, aes(x = JobLevel, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Job Level vs Attrition', x = 'Job Level'),
          ggplot(employee, aes(x = JobRole, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'Job Role vs Attrition', x = 'Job Role')+coord_flip(),
          ggplot(employee, aes(x = PerformanceRating, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'PerformanceRating vs Attrition', x = 'PerformanceRating')+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
          ncol = 2
)
#Significance : Employee Travel Rarely leave most , Job Levels 1 and 2 highest 

#Factors with no Impact
#Department has no effect, as attrition is there proportionately in all.
#Job Role has no clear indication on attrition
#Performance Rating has no clear indication on attrition


plot_grid(ggplot(employee, aes(x = WorkLifeBalance, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'WorkLifeBalance vs Attrition', x = 'WorkLifeBalance'),
          ggplot(employee, aes(x = JobSatisfaction, fill = Attrition)) + geom_bar()+ theme_classic() + labs(title = 'JobSatisfaction vs Attrition', x = 'JobSatisfaction'),
          ggplot(employee, aes(x = EnvironmentSatisfaction, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'EnvironmentSatisfaction vs Attrition', x = 'EnvironmentSatisfaction'),
          ggplot(employee, aes(x = JobInvolvement, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'JobInvolvement vs Attrition', x = 'JobInvolvement'),
          ggplot(employee, aes(x = StockOptionLevel, fill = Attrition)) + geom_bar() + theme_classic() + labs(title = 'StockOptionLevel vs Attrition', x = 'StockOptionLevel')+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
          ncol = 2
)
#Except Stock Option level, none of these factors have no logical explanation for attrition. 
#The higher values should have less attrition, but they are not


plot_grid(ggplot(employee, aes(x = TotalWorkingYears, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'TotalWorkingYears vs Attrition', x = 'TotalWorkingYears'),
          ggplot(employee, aes(x = MonthlyIncome, fill = Attrition)) + geom_histogram()+ theme_classic() + labs(title = 'MonthlyIncome vs Attrition', x = 'MonthlyIncome'),
          ggplot(employee, aes(x = NumCompaniesWorked, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'NumCompaniesWorked vs Attrition', x = 'NumCompaniesWorked'),
          ggplot(employee, aes(x = PercentSalaryHike, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'PercentSalaryHike vs Attrition', x = 'PercentSalaryHike'),
          ggplot(employee, aes(x = TrainingTimesLastYear, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'TrainingTimesLastYear vs Attrition', x = 'TrainingTimesLastYear'),
          ggplot(employee, aes(x = DistanceFromHome, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'DistanceFromHome vs Attrition', x = 'DistanceFromHome'),
          ncol = 2
)
#Significance : Total Working Years, Number of Companies worked before and Training taken last Year

#Factors with no Impact
#Salary Hike, Distance from Home, Monthly Income


plot_grid(ggplot(employee, aes(x = YearsAtCompany, colour = Attrition)) + geom_line(stat="count")+ theme_classic() + labs(title = 'YearsAtCompany vs Attrition', x = 'YearsAtCompany'),
          ggplot(employee, aes(x = YearsSinceLastPromotion, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'YearsSinceLastPromotion vs Attrition', x = 'YearsSinceLastPromotion'),
          ggplot(employee, aes(x = avgHours, fill = Attrition)) + geom_histogram() + theme_classic() + labs(title = 'Avg Working Hours vs Attrition', x = 'avgHours'),
          ggplot(employee, aes(x = totLeaves, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'Total Leaves vs Attrition', x = 'totLeaves'),
          ggplot(employee, aes(x = YearsWithCurrManager, colour = Attrition)) + geom_line(stat="count") + theme_classic() + labs(title = 'YearsWithCurrManager vs Attrition', x = 'YearsWithCurrManager')+ theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)),
          ncol = 2
) 
#Significance : Years at Company,Years Since last Promotion,Average Working Hours,xYears with Current Manager

#Factors with no Impact
#Number of Leaves


# Removing 'AgeBin' - as it was created only for plots
employee <- subset(employee, select = -AgeBin)


###########################################################################################################################
# Outlier Treatment    ----------------------------------------------------------------------------------------------------

# Function for checking Outliers
test_outlier <- function(df , col, glabel) {
  grid.arrange(ggplot(df, aes(x = col, fill = "red")) + geom_histogram() + ggtitle(paste("Frequency Distribution - ", glabel, sep = " ")) + labs(x = glabel, y = glabel) + guides(fill = FALSE),
               ggplot(df, aes(x = factor(0), col)) + geom_boxplot() + labs(x = "", y = glabel) + coord_flip(), nrow = 2)}


# 'DistanceFromHome'
test_outlier(employee, employee$DistanceFromHome, "Distance From Home")
# No outliers


# 'MonthlyIncome'
test_outlier(employee, employee$MonthlyIncome, "Monthly Income")
# Outliers present

length(boxplot.stats(employee$MonthlyIncome)$out) # 342 Outliers

out <- boxplot.stats(employee$MonthlyIncome)$out
max(employee[!employee$MonthlyIncome %in% out, ]$MonthlyIncome)

# Capped MonthlyIncome above 165550
employee[employee$MonthlyIncome %in% out, ]$MonthlyIncome <- 165550

test_outlier(employee, employee$MonthlyIncome, "Monthly Income")
# Outliers fixed


# 'PercentSalaryHike
test_outlier(employee, employee$PercentSalaryHike, "Percent Salary Hike")
# No Outliers


# 'YearsWithCurrManager'
test_outlier(employee, employee$YearsWithCurrManager, "Years With Current Manager")
# Outliers present

length(boxplot.stats(employee$YearsWithCurrManager)$out) # 42 Outliers
out <- boxplot.stats(employee$YearsWithCurrManager)$out
max(employee[!employee$YearsWithCurrManager %in% out, ]$YearsWithCurrManager)

# Capped YearsWithCUrrManager above 14
employee[employee$YearsWithCurrManager %in% out, ]$YearsWithCurrManager <- 14
test_outlier(employee, employee$YearsWithCurrManager, "Monthly Income")
# Outliers fixed

# 'YearsAtCompany'
test_outlier(employee, employee$YearsAtCompany, "Years At Company")
# Outliers present

length(boxplot.stats(employee$YearsAtCompany)$out) # 312 Outliers
out <- boxplot.stats(employee$YearsAtCompany)$out
max(employee[!employee$YearsAtCompany %in% out, ]$YearsAtCompany)

# Capped YearsAtCompany above 18
employee[employee$YearsAtCompany %in% out, ]$YearsAtCompany <- 18
test_outlier(employee, employee$YearsAtCompany, "Monthly Income")
# Outliers fixed

# 'avgHours'
test_outlier(employee, employee$avgHours, "Average Work Hours")
# Outliers present

length(boxplot.stats(employee$avgHours)$out) # 36 Outliers
out <- boxplot.stats(employee$avgHours)$out
max(employee[!employee$avgHours %in% out, ]$avgHours)

# Capped avgHours above 10.915
employee[employee$avgHours %in% out, ]$avgHours <- 10.915
test_outlier(employee, employee$avgHours, "Monthly Income")
# Outliers fixed

# Looking for Outliers in numerical tabular form.
View(sapply(employee[,c("DistanceFromHome",
                        "MonthlyIncome",
                        "NumCompaniesWorked",
                        "PercentSalaryHike",
                        "TotalWorkingYears",
                        "TrainingTimesLastYear",
                        "YearsAtCompany",
                        "YearsSinceLastPromotion")], 
            function(x) quantile(x, seq(0, 1, .01), na.rm = T)))


###################################################################################################
# Dummy Variables Creation    ---------------------------------------------------------------------
# creating dummy variables for factor attributes
employee_vector <- c("Gender",
                     "PerformanceRating",
                     "BusinessTravel",
                     "Department",
                     "MaritalStatus",
                     "StockOptionLevel",
                     "JobInvolvement",
                     "EnvironmentSatisfaction",
                     "JobSatisfaction",
                     "WorkLifeBalance",
                     "Education",
                     "JobLevel",
                     "EducationField",
                     "JobRole")
employee_fact <- subset(employee, select= c(which(names(employee) %in%  
                                                    employee_vector)))
dummies<- data.frame(sapply(employee_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =employee_fact))[,-1]))

#For Gender 0=female , 1 =female
#For Performance rating 3 as 0 , 4 as 1

#Combining the dummy variables created after removing the actual categorical columns
employee_1 <- cbind(employee[,-which(names(employee) %in%  
                                       employee_vector)], dummies)

View(employee_1)

str(employee_1)

#Removing avgTimein , avgTimeout as it wont be used in our analysis further as we have already created
#Average hours and leaves of an employee
employee_1 <- subset(employee_1 , select = -c(which(
  names(employee_1) %in% c("avgTimeIn" , "avgTimeOut"))))


# Correlation between numeric variables
ggpairs(employee_1[, c("DistanceFromHome","MonthlyIncome","NumCompaniesWorked","PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion")])


#"YearsAtCompany"  and "YearsSinceLastPromotion" are very much corelated .623
#"YearsAtCompany"  and "TotalWorkingYears" are very much corelated .584

# Feature standardisation
# Normalising continuous features 
scale_col <-
  c(
    "Age",
    "DistanceFromHome",
    "MonthlyIncome",
    "NumCompaniesWorked",
    "PercentSalaryHike",
    "TotalWorkingYears",
    "TrainingTimesLastYear",
    "YearsAtCompany",
    "YearsSinceLastPromotion",
    "YearsWithCurrManager",
    "avgHours",
    "totLeaves"
  )
employee_1[, scale_col] <- sapply(employee_1[, scale_col], scale)

View(employee_1)


# Checking attrition rate of prospect customer

Attrition <- sum(as.numeric(employee_1$Attrition))/nrow(employee_1)
Attrition # 16.12% Attrition rate. 


# converting target variable Attrition from character to factorwith levels 0/1 
employee_1$Attrition<-as.factor(employee_1$Attrition)


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_1$Attrition, SplitRatio = 0.7)

train = employee_1[indices,]

test = employee_1[!(indices),]


#Initial model
model_1 = glm(Attrition ~ ., data = train[,-1], family = "binomial")
summary(model_1) 
#AIC: 2142.4


# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)
#AIC: 2113.2

# Removing multicollinearity through VIF check
sort(vif(model_2),decreasing = TRUE)

# removing  EducationField.xMarketing as it has p value 0.152872

model_3<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
      NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
      YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
      BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
      Department.xResearch...Development + Department.xSales + 
      Education.x3 + Education.x4 + Education.x5 + 
      EducationField.xOther + JobLevel.x2 + JobRole.xLaboratory.Technician + 
      JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
      JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
      JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
      EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
      JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
      WorkLifeBalance.x4, family = "binomial", data = train[, -1])


summary(model_3) 
#AIC: 2113.2
sort(vif(model_3),decreasing = TRUE)

#Removing EducationField.xOther  as it has p value 0.135099 which is on higher side

model_4<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x3 + Education.x4 + Education.x5 + 
               JobLevel.x2 + JobRole.xLaboratory.Technician + 
               JobRole.xResearch.Director + JobRole.xResearch.Scientist + 
               JobRole.xSales.Executive + MaritalStatus.xSingle + StockOptionLevel.x1 + 
               JobInvolvement.x3 + EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_4) 
#2113.6
sort(vif(model_4),decreasing = TRUE)


# Removing Education.x3 as it has p value 0.085185

model_5<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
      NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
      YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
      BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
      Department.xResearch...Development + Department.xSales + 
      Education.x4 + Education.x5 + JobLevel.x2 + 
      JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
      JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
      MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x3 + 
      EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
      EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
      JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
      WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_5) 
#AIC: 2114.6
sort(vif(model_5),decreasing = TRUE)


# Removing  Education.x5  as it has p value 0.163560

model_6<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               Education.x4 + JobLevel.x2 + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_6) 
#AIC: 2114.7
sort(vif(model_6),decreasing = TRUE)


# Removing  Education.x4  as it has p value 0.180301

model_7<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobLevel.x2 + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + StockOptionLevel.x1 + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_7) 
#AIC: 2114.5
sort(vif(model_7),decreasing = TRUE)


# Removing  StockOptionLevel.x1  as it has p value 0.098056

model_8<-glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobLevel.x2 + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_8) 
#AIC: 2115.2
sort(vif(model_8),decreasing = TRUE)


# Removing  DistanceFromHome  as it has p value 0.081629

model_9<-glm(formula = Attrition ~ Age + MonthlyIncome + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobLevel.x2 + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_9) 
#AIC: 2116.3
sort(vif(model_9),decreasing = TRUE)


# Removing  MonthlyIncome   as it has p value 0.061874

model_10<-glm(formula = Attrition ~ Age + 
               NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
               YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
               BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
               Department.xResearch...Development + Department.xSales + 
               JobLevel.x2 + 
               JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
               JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
               MaritalStatus.xSingle + JobInvolvement.x3 + 
               EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
               EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
               JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
               WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_10) 
#AIC: 2117.9
sort(vif(model_10),decreasing = TRUE)


# Removing  JobInvolvement.x3 as it has p value 0.025411

model_11<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobLevel.x2 + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_11) 
#AIC: 2120.9
sort(vif(model_11),decreasing = TRUE)


# Removing  JobLevel.x2 as it has p value 0.024910

model_12<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_12) 
#AIC: 2123.9
sort(vif(model_12),decreasing = TRUE)

# Removing  JobRole.xLaboratory.Technician as it has p value 0.021357

model_13<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_13) 
#AIC: 2127.1
sort(vif(model_13),decreasing = TRUE)


# Removing  JobRole.xResearch.Scientist as it has p value 0.021364

model_14<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xResearch.Director + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_14) 
#AIC: 2130.3
sort(vif(model_14),decreasing = TRUE)


# Removing  JobRole.xResearch.Director as it has p value 0.002416

model_15<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                JobRole.xSales.Executive + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_15) 
#AIC: 2137
sort(vif(model_15),decreasing = TRUE)


# Removing  JobRole.xSales.Executive  as it has p value 0.006107

model_16<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3 + 
                WorkLifeBalance.x4, family = "binomial", data = train[, -1])

summary(model_16) 
#AIC: 2142.3
sort(vif(model_16),decreasing = TRUE)


# Removing  WorkLifeBalance.x4  as it has p value 0.001682

model_17<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x2 + WorkLifeBalance.x3
                , family = "binomial", data = train[, -1])

summary(model_17) 
#AIC: 2150.1
sort(vif(model_17),decreasing = TRUE)


# Removing  WorkLifeBalance.x2  as it has p value 0.009101

model_18<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + JobSatisfaction.x3 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])

summary(model_18) 
#AIC: 2154.9
sort(vif(model_18),decreasing = TRUE)


# Removing  JobSatisfaction.x3  as it has p value 0.001838

model_19<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + JobSatisfaction.x2 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])

summary(model_19) 
#AIC: 2162.5
sort(vif(model_19),decreasing = TRUE)


# Removing  JobSatisfaction.x2  as it has p value 0.023142

model_20<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])

summary(model_20) 
#AIC: 2165.8
sort(vif(model_20),decreasing = TRUE)

########################Correlation Plot for model_20

model20_corr <- subset(train[,-1], select = c(Age , 
                                     NumCompaniesWorked , TotalWorkingYears , TrainingTimesLastYear , 
                                     YearsSinceLastPromotion , YearsWithCurrManager , avgHours , 
                                     BusinessTravel.xTravel_Frequently , BusinessTravel.xTravel_Rarely , 
                                     Department.xResearch...Development , Department.xSales , 
                                     MaritalStatus.xSingle ,
                                     EnvironmentSatisfaction.x2 , EnvironmentSatisfaction.x3 , 
                                     EnvironmentSatisfaction.x4 , 
                                     JobSatisfaction.x4 , WorkLifeBalance.x3)) 
library(reshape2)
correlation_plot <- function(x){
corr <- cor(x) 

cormat <- round(cor(x), 2)


melted_cormat <- melt(cormat)

#Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 30, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank())
}

correlation_plot(model20_corr)
####################################################################

# Removing  BusinessTravel.xTravel_Rarely 
cor(train$BusinessTravel.xTravel_Frequently,train$BusinessTravel.xTravel_Rarely)
#-0.7499486 
#VIF=4.802828,p-value of BusinessTravel.xTravel_Rarely is higher (1.58e-05) than BusinessTravel.xTravel_Frequently
model_21<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + Department.xSales + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])

summary(model_21) 
#AIC: 2187.1
sort(vif(model_21),decreasing = TRUE)

# Removing  Department.xSales 
cor(train$Department.xResearch...Development,train$Department.xSales)
#-0.9063018 
#VIF= 3.797540,p-value of Department.xSales is higher than Department.xResearch...Development
model_22<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                Department.xResearch...Development + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])

summary(model_22) 
#AIC: 2214
sort(vif(model_22),decreasing = TRUE)

#Removing Department.xResearch...Development based on pvalue=0.186814
model_23<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4 + WorkLifeBalance.x3
              , family = "binomial", data = train[, -1])
#AIC: 2213.7
summary(model_23) 

sort(vif(model_23),decreasing = TRUE)

#Removing WorkLifeBalance.x3 based on pvalue=0.001717
model_24<-glm(formula = Attrition ~ Age + 
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_24) 
#AIC: 2221.5
sort(vif(model_24),decreasing = TRUE)

#Removing Age
cor(train$TotalWorkingYears,train$Age)
# 0.6792304 , these 2 are highly correlated , we will remove Age as it gives lesser AIC than when we remove TotalWorkingYears
model_25<-glm(formula = Attrition ~  
                NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_25) 
#AIC: 2237.9
sort(vif(model_25),decreasing = TRUE)

#Now all variables has low VIF values i.e VIF < 2

#Correlation Plot
model25_corr <- subset(train[,-1], select = c(NumCompaniesWorked , TotalWorkingYears , TrainingTimesLastYear , 
                                      YearsSinceLastPromotion , YearsWithCurrManager , avgHours , 
                                      BusinessTravel.xTravel_Frequently , 
                                      MaritalStatus.xSingle ,
                                      EnvironmentSatisfaction.x2 , EnvironmentSatisfaction.x3 , 
                                      EnvironmentSatisfaction.x4 , 
                                      JobSatisfaction.x4)) 
correlation_plot(model25_corr)

#Removing TrainingTimesLastYear based on pvalue=0.000215
model_26<-glm(formula = Attrition ~  
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_26) 
#AIC: 2250.1
sort(vif(model_26),decreasing = TRUE)


cor(train$YearsWithCurrManager,train$YearsSinceLastPromotion)
# 0.5071091
#Removing YearsWithCurrManager
model_27a<-glm(formula = Attrition ~  
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion  + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_27a) 
#AIC: 2281.2
sort(vif(model_27a),decreasing = TRUE)

#Removing YearsSinceLastPromotion
model_27b<-glm(formula = Attrition ~  
                NumCompaniesWorked + TotalWorkingYears + 
                 YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x2 + EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_27b) 
#AIC: 2288.1
sort(vif(model_27b),decreasing = TRUE)

#since AIC is lesser for model_27a we consider 27a as model 27
model_27 <- model_27a
summary(model_27)
#Accuracy =0.7218443 
#Sensitivity =0.7089202 
#Specificity =0.7243243 
#KS STatistics= 0.4332445

#Removing EnvironmentSatisfaction.x2 variable based on business understanding
model_27c<-glm(formula = Attrition ~  
                NumCompaniesWorked + TotalWorkingYears + 
                YearsSinceLastPromotion + YearsWithCurrManager + avgHours + 
                BusinessTravel.xTravel_Frequently + 
                MaritalStatus.xSingle +
                EnvironmentSatisfaction.x3 + 
                EnvironmentSatisfaction.x4 + 
                JobSatisfaction.x4
              , family = "binomial", data = train[, -1])

summary(model_27c) 
#AIC: 2284.8
#Accuracy =0.739229 
#Sensitivity = 0.741784 
#Specificity = 0.7387387 
#Ks Statistics = 0.4805228

#Based on EDA , EnvironmentSatisfaction is not a clear driving factor for attrition from bussiness point
#EnvironmentSatisfaction of values 3 and 4 has highest attrition rate , so we remove dummyvariable for 2 and build model

model_28a<-glm(formula = Attrition ~  
                 NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion  + avgHours +
                 BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle +
                 EnvironmentSatisfaction.x3 + 
                 EnvironmentSatisfaction.x4 + 
                 JobSatisfaction.x4
               , family = "binomial", data = train[, -1])

summary(model_28a) 
#AIC: 2313.3
#Accuracy =0.7248677 
#Sensitivity =0.7511737 
#Specificity =0.7198198 
#KS Statistics= 0.4709935

#Removing all the EnvironmentSatisfaction variables to further analyse the model
model_28b<-glm(formula = Attrition ~  
                 NumCompaniesWorked + TotalWorkingYears + 
                 YearsSinceLastPromotion  + avgHours + 
                 BusinessTravel.xTravel_Frequently + 
                 MaritalStatus.xSingle +
                 JobSatisfaction.x4
               , family = "binomial", data = train[, -1])

summary(model_28b) 
#AIC: 2342.9
#Accuracy = 0.7188209 
#Sensitivity = 0.7230047 
#Specificity = 0.718018 
#KS Statistics = 0.4410227


# With 10 significant variables in the model we will consider model_27c as the final model
# As these 10 variables are driving factors for attrition
#The Model explains critical driving factors for Employee Attrition  that match with insights as per Exploratory Data Analysis 
#Higher the values for Number of years with current Manager and also Total Working Years reduce Employee attrition
#A model without Environment Satisfaction and Job Satisfaction is also performing well, but considered in model from a Business Perspective

final_model<- model_27c

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])

summary(test_pred)

test$prob <- test_pred
View(test)

# Probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

#install.packages("e1071")
library(e1071)

test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Choosing the optimal probalility cutoff

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initialising a matrix of 100 X 3.

# Summary of test probability
summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.02)]
round(cutoff,2)

# Choosing cutoff value of 0.17 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.17, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc #0.739229
sens #0.741784 
spec #0.7387387

View(test)

### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#On testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

#Accuracy =0.739229 
#Sensitivity = 0.741784 
#Specificity = 0.7387387 
#Ks Statistics = 0.4805228

#Cut-Off 17% indicates 
#KSStatistic 48% is pretty good with much higher than standard expected 40% 
  
####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
#require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob, groups = 10) {
  if (is.factor(labels))
    labels  <- as.integer(as.character(labels))
  if (is.factor(predicted_prob))
    predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[, "bucket"] = ntile(-helper[, "predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels), funs(total = n(),
                                    totalresp = sum(., na.rm = TRUE))) %>%
    
    mutate(
      Cumresp = cumsum(totalresp),
      Gain = Cumresp / sum(totalresp) * 100,
      Cumlift = Gain / (bucket * (100 / groups))
    )
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
View(attrition_decile)

#The gain and the plots are mentioned in the  presentation

#Gain:
#76% Gain achieved with 4th decile
#83% Gain achieved with 5th decile
#Model is close to perfect model & far away from Random model

#Lift:
#1.9 Lift achieved with 4th decile
#1.67 lift achieved with 5th decile
#Model achieves 1.9 times more attrition than the Random model at 4th decile

#create residual plots
plot(model_27c)

#Recommendations:
#Factors for Employee Attrition and Suggestions for Retention

#Years since last promotion
#0 to 2 years is a cause of concern. 40% People leave within 1 Year after the promotion.
#Years with Current Manager 
#People who stay with same manager for longer than 3 Years have high retention
#Average working hours 
#People who are working  8 hours and more are leaving mostly
#Business Travel
#Employees who travel rarely leaves 50% of the time.
#Environment Satisfaction and Job Satisfaction 
#As per data available, these two surveys not have clear indication on the attrition
#However, as these critical factors and retained in model
#Number of Companies Work Before
#People who join this company as their second company leave the most.
#Marital Status
#Employees with Status Single leave most
#Total Working Years
#Retaining employees more than first 3 years reduces the attrition
