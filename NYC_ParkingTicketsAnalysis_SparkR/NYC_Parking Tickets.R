# SparkR Case Study - NYC Parking Tickets: An Exploratory Analysis

# For the scope of this analysis, # we wish to compare phenomenon related to parking
# tickets over three different years - 2015, 2016, 2017. All the analysis steps 
# mentioned below should be done for 3 different years. Each metric you derive 
# should be compared across the 3 years.


# 1. Initialize spark session
library(SparkR)
library(sparklyr)
library(stringr)
library(ggplot2)
library(gridExtra)

# Initialise the spark session
sparkR.session(master='local')

# 2. Create a Spark DataFrame and examine structure
# reading a CSV file from S3 bucket
parking_Tickets_NY_2015 <- SparkR::read.df("s3://pgdds3-skona/NYCParkingCaseStudy/Parking_Violations_Issued_-_Fiscal_Year_2015.csv", header=T, source = "csv")
parking_Tickets_NY_2016 <- SparkR::read.df("s3://pgdds3-skona/NYCParkingCaseStudy/Parking_Violations_Issued_-_Fiscal_Year_2016.csv", header=T, source = "csv")
parking_Tickets_NY_2017 <- SparkR::read.df("s3://pgdds3-skona/NYCParkingCaseStudy/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", header=T, source = "csv")

# Getting number of Partitions (Specific to our System)
getNumPartitions(parking_Tickets_NY_2015) #22 Partitions
getNumPartitions(parking_Tickets_NY_2016) #17 Partitions
getNumPartitions(parking_Tickets_NY_2017) #16 Partitions

### EXPLORING DATA

head(parking_Tickets_NY_2015)
head(parking_Tickets_NY_2016)
head(parking_Tickets_NY_2017)


# Examine the size
nrow(parking_Tickets_NY_2015)
# 11809233 Rows
ncol(parking_Tickets_NY_2015)
# 51 Columns

nrow(parking_Tickets_NY_2016)
# 10626899 Rows
ncol(parking_Tickets_NY_2016)
# 51 Columns

nrow(parking_Tickets_NY_2017)
# 10803028 Rows
ncol(parking_Tickets_NY_2017)
# 43 Columns

#removig duplicates
parking_Tickets_NY_2015 <- unique(parking_Tickets_NY_2015)
parking_Tickets_NY_2016 <- unique(parking_Tickets_NY_2016)
parking_Tickets_NY_2017 <- unique(parking_Tickets_NY_2017)

#to get the columns if there are different number of columns in the 3 dataframes
setdiff(names(parking_Tickets_NY_2015), names(parking_Tickets_NY_2016))
setdiff(names(parking_Tickets_NY_2015), names(parking_Tickets_NY_2017))
# "Latitude"           "Longitude"          "Community Board"    "Community Council " "Census Tract"      
#"BIN"                "BBL"                "NTA"   
setdiff(names(parking_Tickets_NY_2016), names(parking_Tickets_NY_2017))
# "Latitude"           "Longitude"          "Community Board"    "Community Council " "Census Tract"      
#"BIN"                "BBL"                "NTA"   

#Checked the values in these 8 columns of 2015 and 2016 data - All have 'NA' values
# collect(distinct(select(parking_Tickets_NY_2015,"Latitude"))) 
# collect(distinct(select(parking_Tickets_NY_2015,"Longitude"))) 
# collect(distinct(select(parking_Tickets_NY_2015,"Community Board")))
# collect(distinct(select(parking_Tickets_NY_2015,"Community Council ")))
# collect(distinct(select(parking_Tickets_NY_2015,"Census Tract")))
# collect(distinct(select(parking_Tickets_NY_2015,"BIN")))
# collect(distinct(select(parking_Tickets_NY_2015,"BBL")))
# collect(distinct(select(parking_Tickets_NY_2015,"NTA")))

#All these columns have only NA value

# collect(distinct(select(parking_Tickets_NY_2016,"Latitude")))
# collect(distinct(select(parking_Tickets_NY_2016,"Longitude"))) 
# collect(distinct(select(parking_Tickets_NY_2016,"Community Board")))
# collect(distinct(select(parking_Tickets_NY_2016,"Community Council ")))
# collect(distinct(select(parking_Tickets_NY_2016,"Census Tract")))
# collect(distinct(select(parking_Tickets_NY_2016,"BIN")))
# collect(distinct(select(parking_Tickets_NY_2016,"BBL")))
# collect(distinct(select(parking_Tickets_NY_2016,"NTA")))
#All these columns have only NA value

### CLEANING DATA
# Replacing " " in Column Name with "_" [underscore]
fixColumnNames <- function(x) str_replace_all(x, " ", "_")

colnames(parking_Tickets_NY_2015) <- fixColumnNames(colnames(parking_Tickets_NY_2015))
colnames(parking_Tickets_NY_2016) <- fixColumnNames(colnames(parking_Tickets_NY_2016))
colnames(parking_Tickets_NY_2017) <- fixColumnNames(colnames(parking_Tickets_NY_2017))

#Removing the columns from the dataframe
parking_Tickets_NY_2015$Latitude <- NULL
parking_Tickets_NY_2015$Longitude <- NULL
parking_Tickets_NY_2015$Community_Board <- NULL
parking_Tickets_NY_2015$Community_Council_ <- NULL
parking_Tickets_NY_2015$Census_Tract <- NULL
parking_Tickets_NY_2015$BIN <- NULL
parking_Tickets_NY_2015$BBL <- NULL
parking_Tickets_NY_2015$NTA <- NULL

parking_Tickets_NY_2016$Latitude <- NULL
parking_Tickets_NY_2016$Longitude <- NULL
parking_Tickets_NY_2016$Community_Board <- NULL
parking_Tickets_NY_2016$Community_Council_ <- NULL
parking_Tickets_NY_2016$Census_Tract <- NULL
parking_Tickets_NY_2016$BIN <- NULL
parking_Tickets_NY_2016$BBL <- NULL
parking_Tickets_NY_2016$NTA <- NULL

setdiff(names(parking_Tickets_NY_2015), names(parking_Tickets_NY_2016))
setdiff(names(parking_Tickets_NY_2015), names(parking_Tickets_NY_2017))
setdiff(names(parking_Tickets_NY_2016), names(parking_Tickets_NY_2017))
#So now the difference is 0 , and all 3 years dataframes has 43 columns


#with Column will add a new column and to_date will convert the date into proper format
parking_Tickets_NY_2015 <- parking_Tickets_NY_2015 %>% withColumn("Parsed_Issue_Date", to_date(parking_Tickets_NY_2015$Issue_Date,"MM/dd/yyyy"))
parking_Tickets_NY_2016 <- parking_Tickets_NY_2016 %>% withColumn("Parsed_Issue_Date", to_date(parking_Tickets_NY_2016$Issue_Date,"MM/dd/yyyy"))
parking_Tickets_NY_2017 <- parking_Tickets_NY_2017 %>% withColumn("Parsed_Issue_Date", to_date(parking_Tickets_NY_2017$Issue_Date,"MM/dd/yyyy"))

#Extracting year from issue date
parking_Tickets_NY_2015 <- parking_Tickets_NY_2015 %>% withColumn("YearofIssueDate", year(parking_Tickets_NY_2015$Parsed_Issue_Date))
parking_Tickets_NY_2016 <- parking_Tickets_NY_2016 %>% withColumn("YearofIssueDate", year(parking_Tickets_NY_2016$Parsed_Issue_Date))
parking_Tickets_NY_2017 <- parking_Tickets_NY_2017 %>% withColumn("YearofIssueDate", year(parking_Tickets_NY_2017$Parsed_Issue_Date))

str(parking_Tickets_NY_2015)
str(parking_Tickets_NY_2016)
str(parking_Tickets_NY_2017)

#extracting months
parking_Tickets_NY_2015 <- parking_Tickets_NY_2015 %>% withColumn("MonthofIssueDate", month(parking_Tickets_NY_2015$Parsed_Issue_Date))
parking_Tickets_NY_2016 <- parking_Tickets_NY_2016 %>% withColumn("MonthofIssueDate", month(parking_Tickets_NY_2016$Parsed_Issue_Date))
parking_Tickets_NY_2017 <- parking_Tickets_NY_2017 %>% withColumn("MonthofIssueDate", month(parking_Tickets_NY_2017$Parsed_Issue_Date))

#The data is of the fiscal years as per content section in https://www.kaggle.com/new-york-city/nyc-parking-tickets
#Subsetting data based on the fiscal year of issue date for that particular year
parking_Tickets_NY_2015 <- filter(parking_Tickets_NY_2015, ((parking_Tickets_NY_2015$YearofIssueDate==2014 & parking_Tickets_NY_2015$MonthofIssueDate %in% c(7,8,9,10,11,12)) | (parking_Tickets_NY_2015$YearofIssueDate==2015 & parking_Tickets_NY_2015$MonthofIssueDate %in% c(1,2,3,4,5,6))))
parking_Tickets_NY_2016 <- filter(parking_Tickets_NY_2016, ((parking_Tickets_NY_2016$YearofIssueDate==2015 & parking_Tickets_NY_2016$MonthofIssueDate %in% c(7,8,9,10,11,12)) | (parking_Tickets_NY_2016$YearofIssueDate==2016 & parking_Tickets_NY_2016$MonthofIssueDate %in% c(1,2,3,4,5,6))))
parking_Tickets_NY_2017 <- filter(parking_Tickets_NY_2017, ((parking_Tickets_NY_2017$YearofIssueDate==2016 & parking_Tickets_NY_2017$MonthofIssueDate %in% c(7,8,9,10,11,12)) | (parking_Tickets_NY_2017$YearofIssueDate==2017 & parking_Tickets_NY_2017$MonthofIssueDate %in% c(1,2,3,4,5,6))))

#Removing the invalid registrtion state of 99 from the dataset
parking_Tickets_NY_2015 <- filter(parking_Tickets_NY_2015, parking_Tickets_NY_2015$Registration_State!="99")
parking_Tickets_NY_2016 <- filter(parking_Tickets_NY_2016, parking_Tickets_NY_2016$Registration_State!="99")
parking_Tickets_NY_2017 <- filter(parking_Tickets_NY_2017, parking_Tickets_NY_2017$Registration_State!="99")

# Creating Temporary Views
createOrReplaceTempView(parking_Tickets_NY_2015, "df_2015")
createOrReplaceTempView(parking_Tickets_NY_2016, "df_2016")
createOrReplaceTempView(parking_Tickets_NY_2017, "df_2017")

######### Part 1 - Examine the data.

## Q1 Find total number of tickets for each year.
tot_ticket_2015 <- SparkR::sql("SELECT COUNT(DISTINCT Summons_Number) as Unique_Count, COUNT(Summons_Number) as Total_Count FROM df_2015")
head(tot_ticket_2015)
# 10558797  unique out of 10558798

# Find total number of tickets for each year.
tot_ticket_2016 <- SparkR::sql("SELECT COUNT(DISTINCT Summons_Number) as Unique_Count, COUNT(Summons_Number) as Total_Count FROM df_2016")
head(tot_ticket_2016)
# 10357238 unique out of 10357238

# Find total number of tickets for each year.
tot_ticket_2017 <- SparkR::sql("SELECT COUNT(DISTINCT Summons_Number) as Unique_Count, COUNT(Summons_Number) as Total_Count FROM df_2017")
head(tot_ticket_2017)
# 10504843 unique out of 10504843



## Q2 Find out how many unique states the cars which got parking tickets came from.
States_2015 <- SparkR::sql("SELECT COUNT(DISTINCT Registration_State) FROM df_2015")
head(States_2015)
# In 2015, Parking Tickets came out of 68 states. There are 50 states in USA, this means that
# outside cars were also fined for wrongful parking.

States_2016 <- SparkR::sql("SELECT COUNT(DISTINCT Registration_State) FROM df_2016")
head(States_2016)
# In 2016, Parking Tickets came out of 67 states. There are outside states as well in 2016.

States_2017 <- SparkR::sql("SELECT COUNT(DISTINCT Registration_State) FROM df_2017")
head(States_2017)
# In 2017, Parking Tickets came out of 66 states.



# Q3 Some parking tickets don't have addresses on them, which is cause for concern. Find out how many such tickets there are.
no_Address_2015 <- SparkR::sql("SELECT COUNT(Summons_Number) FROM df_2015 WHERE House_Number IS NULL AND Street_Name IS NULL")
head(no_Address_2015)
# Ans: 3470
# So, we are finding records which do not have both House Number and Street_Names. If either one of them is there, we have partial 
# address of that person. Therefore, we are taking NULL for both. 
# As per the SQL there are 3470 records like this in 2015.

no_Address_2016 <- SparkR::sql("SELECT COUNT(Summons_Number) FROM df_2016 WHERE House_Number IS NULL AND Street_Name IS NULL")
head(no_Address_2016)
# Ans: 2559
# For 2016 there are 2559 records with no House_Number and Street_Name

no_Address_2017 <- SparkR::sql("SELECT COUNT(Summons_Number) FROM df_2017 WHERE House_Number IS NULL AND Street_Name IS NULL")
head(no_Address_2017)
# Ans: 2184
# For 2017 there are 2184 records with no House_Number and Street_Name



######### Part 2 - AGGREGATION TASKS.

# Q1 How often does each violation code occur? (frequency of violation codes - find the top 5)

top5_Violation_2015 <- summarize(groupBy(parking_Tickets_NY_2015, parking_Tickets_NY_2015$Violation_Code),
                                 count = n(parking_Tickets_NY_2015$Violation_Code))
top5_Violation_2016 <- summarize(groupBy(parking_Tickets_NY_2016, parking_Tickets_NY_2016$Violation_Code),
                                 count = n(parking_Tickets_NY_2016$Violation_Code))
top5_Violation_2017 <- summarize(groupBy(parking_Tickets_NY_2017, parking_Tickets_NY_2017$Violation_Code),
                                 count = n(parking_Tickets_NY_2017$Violation_Code))

# Creating R Dataframes
top5_Violation_2015_df <- collect(top5_Violation_2015)
top5_Violation_2016_df <- collect(top5_Violation_2016)
top5_Violation_2017_df <- collect(top5_Violation_2017)

# 2015
head(dplyr::arrange(top5_Violation_2015_df, desc(top5_Violation_2015_df$count)),5)
# For 2015, here are top 5 violator codes
# Violation_Code   count
#             21 1592519
#             38 1397978
#             14  969177
#             36  824610
#             37  784747

# 2016
head(dplyr::arrange(top5_Violation_2016_df, desc(top5_Violation_2016_df$count)),5)
# For 2016, here are top 5 violator codes
# Violation_Code   count
#             21 1490775
#             36 1232910
#             38 1125950
#             14  857521
#             37  677448

# 2017
head(dplyr::arrange(top5_Violation_2017_df, desc(top5_Violation_2017_df$count)),5)
# For 2017, here are top 5 violator codes
# Violation_Code   count
#             21 1494775
#             36 1345192
#             38 1049457
#             14  877730
#             20  607510


# Creating Plots
plot_top5_violation_2015 <- top5_Violation_2015_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Code, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Code") + ylab("Frequency")+
  ggtitle("Top 5 Violation code - 2015")


plot_top5_violation_2016 <- top5_Violation_2016_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Code, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Code") + ylab("Frequency")+
  ggtitle("Top 5 Violation code - 2016")

plot_top5_violation_2017 <- top5_Violation_2017_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Code, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Code") + ylab("Frequency")+
  ggtitle("Top 5 Violation code - 2017")

grid.arrange(plot_top5_violation_2015, plot_top5_violation_2016, plot_top5_violation_2017, ncol = 1, nrow = 3)



# Q2 How often does each vehicle body type get a parking ticket? How about the vehicle make? (find the top 5 for both)
#Vehicle Body
top5_Vehicle_Body_2015 <- parking_Tickets_NY_2015 %>%
  groupBy(parking_Tickets_NY_2015$Vehicle_Body_Type) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2015$Vehicle_Body_Type))

top5_Vehicle_Body_2015_df <- collect(top5_Vehicle_Body_2015)


top5_Vehicle_Body_2016 <- parking_Tickets_NY_2016 %>%
  groupBy(parking_Tickets_NY_2016$Vehicle_Body_Type) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2016$Vehicle_Body_Type))
top5_Vehicle_Body_2016_df <- collect(top5_Vehicle_Body_2016)


top5_Vehicle_Body_2017 <- parking_Tickets_NY_2017 %>%
  groupBy(parking_Tickets_NY_2017$Vehicle_Body_Type) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2017$Vehicle_Body_Type))
top5_Vehicle_Body_2017_df <- collect(top5_Vehicle_Body_2017)


plot_top5_Vehicle_Body_2015 <- top5_Vehicle_Body_2015_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Body_Type, -count) %>%
  ggplot(aes(Vehicle_Body_Type, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Body Type") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Body Type - 2015")


plot_top5_Vehicle_Body_2016 <- top5_Vehicle_Body_2016_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Body_Type, -count) %>%
  ggplot(aes(Vehicle_Body_Type, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Body Type") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Body Type - 2016")

plot_top5_Vehicle_Body_2017 <- top5_Vehicle_Body_2017_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Body_Type, -count) %>%
  ggplot(aes(Vehicle_Body_Type, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Body Type") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Body Type - 2017")

grid.arrange(plot_top5_Vehicle_Body_2015, plot_top5_Vehicle_Body_2016, plot_top5_Vehicle_Body_2017, ncol = 1, nrow = 3)


#Vehicle Make
top5_Vehicle_Make_2015 <- parking_Tickets_NY_2015 %>%
  groupBy(parking_Tickets_NY_2015$Vehicle_Make) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2015$Vehicle_Make))

top5_Vehicle_Make_2015_df <- collect(top5_Vehicle_Make_2015)
# Vehicle_Make   count
#         FORD  1417303
#         TOYOT 1123523
#         HONDA 1018049
#         NISSA  837569
#         CHEVR  836389


top5_Vehicle_Make_2016 <- parking_Tickets_NY_2016 %>%
  groupBy(parking_Tickets_NY_2016$Vehicle_Make) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2016$Vehicle_Make))
top5_Vehicle_Make_2016_df <- collect(top5_Vehicle_Make_2016)
# Vehicle_Make   count
#        FORD 1324774
#        TOYOT 1154790
#        HONDA 1014074
#        NISSA  834833
#        CHEVR  759663

top5_Vehicle_Make_2017 <- parking_Tickets_NY_2017 %>%
  groupBy(parking_Tickets_NY_2017$Vehicle_Make) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2017$Vehicle_Make))
top5_Vehicle_Make_2017_df <- collect(top5_Vehicle_Make_2017)
# Vehicle_Make   count
#        FORD 1280958
#        TOYOT 1211451
#        HONDA 1079238
#        NISSA  918590
#        CHEVR  714655

plot_top5_Vehicle_Make_2015 <- top5_Vehicle_Make_2015_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Make, -count) %>%
  ggplot(aes(Vehicle_Make, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Make") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Make - 2015")


plot_top5_Vehicle_Make_2016 <- top5_Vehicle_Make_2016_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Make, -count) %>%
  ggplot(aes(Vehicle_Make, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Make") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Make - 2016")

plot_top5_Vehicle_Make_2017 <- top5_Vehicle_Make_2017_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Vehicle_Make, -count) %>%
  ggplot(aes(Vehicle_Make, count)) +
  geom_bar(stat = "identity") +
  xlab("Vehicle Make") + ylab("Frequency")+
  ggtitle("Top 5 Vehicle Make - 2017")

grid.arrange(plot_top5_Vehicle_Make_2015, plot_top5_Vehicle_Make_2016, plot_top5_Vehicle_Make_2017, ncol = 1, nrow = 3)

# Q3 A precinct is a police station that has a certain zone of the city under its command. 
# Find the (5 highest) frequencies of:

# Violation Precinct frequency
#  Q3.1 - Violating Precincts (this is the precinct of the zone where the violation occurred)

# Violation Precinct frequency
Violation_Precinct_2015 <-  parking_Tickets_NY_2015 %>%
  groupBy(parking_Tickets_NY_2015$Violation_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2015$Violation_Precinct))
top5_Violation_Precinct_2015_df <- collect(Violation_Precinct_2015)
# Violation_Precinct   count
#                  0 1633006
#                 19  559716
#                 18  400887
#                 14  384596
#                  1  307808

Violation_Precinct_2016 <-  parking_Tickets_NY_2016 %>%
  groupBy(parking_Tickets_NY_2016$Violation_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2016$Violation_Precinct))
top5_Violation_Precinct_2016_df <- collect(Violation_Precinct_2016)
#Violation_Precinct   count
#                 0 1868655
#                 19  554465
#                 18  331704
#                 14  324467
#                  1  303850

Violation_Precinct_2017 <-  parking_Tickets_NY_2017 %>%
  groupBy(parking_Tickets_NY_2017$Violation_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2017$Violation_Precinct))
top5_Violation_Precinct_2017_df <- collect(Violation_Precinct_2017)
#Violation_Precinct   count
#                 0 2072400
#                 19  535671
#                 14  352450
#                  1  331810
#                 18  306920


plot_top5_Violation_Precinct_2015 <- top5_Violation_Precinct_2015_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Precinct, -count) %>%
  ggplot(aes(Violation_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Violation Precinct - 2015")


plot_top5_Violation_Precinct_2016 <- top5_Violation_Precinct_2016_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Precinct, -count) %>%
  ggplot(aes(Violation_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Violation Precinct - 2016")

plot_top5_Violation_Precinct_2017 <- top5_Violation_Precinct_2017_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Violation_Precinct, -count) %>%
  ggplot(aes(Violation_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Violation Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Violation Precinct - 2017")

grid.arrange(plot_top5_Violation_Precinct_2015, plot_top5_Violation_Precinct_2016, plot_top5_Violation_Precinct_2017, ncol = 1, nrow = 3)


# Q3.2 - Issuing Precincts (this is the precinct that issued the ticket)

# Issuer Precinct frequency

Issuer_Precinct_2015 <-  parking_Tickets_NY_2015 %>%
  groupBy(parking_Tickets_NY_2015$Issuer_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2015$Issuer_Precinct))
top5_Issuer_Precinct_2015_df <- collect(Issuer_Precinct_2015)

top5_Issuer_Precinct_2015_df
# Issuer_Precinct   count
#                0 1834343
#               19  544946
#               18  391501
#               14  369725
#                1  298594

Issuer_Precinct_2016 <-  parking_Tickets_NY_2016 %>%
  groupBy(parking_Tickets_NY_2016$Issuer_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2016$Issuer_Precinct))
top5_Issuer_Precinct_2016_df <- collect(Issuer_Precinct_2016)

top5_Issuer_Precinct_2016_df
# Issuer_Precinct   count
#               0 2140274
#              19  540569
#              18  323132
#              14  315311
#               1  295013

Issuer_Precinct_2017 <-  parking_Tickets_NY_2017 %>%
  groupBy(parking_Tickets_NY_2017$Issuer_Precinct) %>%
  SparkR::summarize(count = SparkR::count(parking_Tickets_NY_2017$Issuer_Precinct))
top5_Issuer_Precinct_2017_df <- collect(Issuer_Precinct_2017)

top5_Issuer_Precinct_2017_df
# Issuer_Precinct   count
#               0 2388479
#              19  521513
#              14  344977
#               1  321170
#              18  296553

plot_top5_Issuer_Precinct_2015 <- top5_Issuer_Precinct_2015_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Issuer_Precinct, -count) %>%
  ggplot(aes(Issuer_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Issuer Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Issuer Precinct - 2015")


plot_top5_Issuer_Precinct_2016 <- top5_Issuer_Precinct_2016_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Issuer_Precinct, -count) %>%
  ggplot(aes(Issuer_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Issuer Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Issuer Precinct - 2016")

plot_top5_Issuer_Precinct_2017 <- top5_Issuer_Precinct_2017_df %>%
  dplyr::top_n(n = 5) %>%
  dplyr::arrange(Issuer_Precinct, -count) %>%
  ggplot(aes(Issuer_Precinct, count)) +
  geom_bar(stat = "identity") +
  xlab("Issuer Precinct") + ylab("Frequency")+
  ggtitle("Top 5 Issuer Precinct - 2017")

grid.arrange(plot_top5_Issuer_Precinct_2015, plot_top5_Issuer_Precinct_2016, plot_top5_Issuer_Precinct_2017, ncol = 1, nrow = 3)



#  Q4 - Find the violation code frequency across 3 precincts which have issued the most number of tickets - 

Violation_Frequency_across_Precincts_2015 <- SparkR::sql("SELECT Issuer_Precinct, Violation_Code, COUNT(Violation_Code) as count from df_2015 GROUP BY Issuer_Precinct, Violation_Code")
Violation_Frequency_across_Precincts_2016 <- SparkR::sql("SELECT Issuer_Precinct, Violation_Code, COUNT(Violation_Code) as count from df_2016 GROUP BY Issuer_Precinct, Violation_Code")
Violation_Frequency_across_Precincts_2017 <- SparkR::sql("SELECT Issuer_Precinct, Violation_Code, COUNT(Violation_Code) as count from df_2017 GROUP BY Issuer_Precinct, Violation_Code")

# Creating R Dataframes
df_violation_freq_2015 <- collect(Violation_Frequency_across_Precincts_2015)
df_violation_freq_2016 <- collect(Violation_Frequency_across_Precincts_2016)
df_violation_freq_2017 <- collect(Violation_Frequency_across_Precincts_2017)

# Top 3 Precincts for 2015
df_violation_freq_2015 %>% 
  dplyr::group_by(Issuer_Precinct) %>%
  dplyr::summarize(count = sum(count)) %>% 
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 Issuer_Precinct are 0, 19, 18 
#Issuer_Precinct    count
# 0               1643552
# 19               535880
# 18               383977

# Top 3 Precincts for 2016
df_violation_freq_2016 %>% 
  dplyr::group_by(Issuer_Precinct) %>%
  dplyr::summarize(count = sum(count)) %>% 
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 Issuer_Precinct are 0, 19, 18

#Issuer_Precinct    count
# 0               2061872
# 19               531651
# 18               316730

# Top 3 Precincts for 2017
df_violation_freq_2017 %>% 
  dplyr::group_by(Issuer_Precinct) %>%
  dplyr::summarize(count = sum(count)) %>% 
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 Issuer_Precinct are 0, 19, 14
#Issuer_Precinct    count
# 0               2248357
# 19               514061
# 14               340003

# Plotting the graphs
# Plotting the frequency of Violation Codes in the Top 3 Issuer Precinct in 2015 
p2015 <-df_violation_freq_2015 %>%
  dplyr::filter(Issuer_Precinct %in% c("0", "19", "18")) %>%
  dplyr::filter(count > 5000) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2015") +
  facet_grid(.~Issuer_Precinct)

# Plotting the frequency of Violation Codes in the Top 3 Issuer Precinct in 2016
p2016 <-df_violation_freq_2016 %>%
  dplyr::filter(Issuer_Precinct %in% c("0", "19", "18")) %>%
  dplyr::filter(count > 5000) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2016") +
  facet_grid(.~Issuer_Precinct)

# Plotting the frequency of Violation Codes in the Top 3 Issuer Precinct in 2017
p2017<- df_violation_freq_2017 %>%
  dplyr::filter(Issuer_Precinct %in% c("0", "19", "14")) %>%
  dplyr::filter(count > 5000) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2017") +
  facet_grid(.~Issuer_Precinct)

grid.arrange(p2015, p2016, p2017, ncol = 1, nrow = 3,
             top = "Violation Code Frequency for Top 3 Precincts Issued Most Tickets")

# OBSERVATION 
# Assuming 0 as a valid Precinct 
# Across the top 3 precincts, the most given Violation Codes are not the same.
# And it seems like the trend in each precinct remains the same for 2015 and 2016, but it changes for 2017



# Q5 -   You'd want to find out the properties of parking violations across different times of the day:
# Q5.1   The Violation Time field is specified in a strange format. Find a way to make this into a time attribute that you can use to divide into groups.
#        Find a way to deal with missing values, if any.
#        Divide 24 hours into 6 equal discrete bins of time. The intervals you choose are at your discretion. 

# For 2015
# Violation Time format - 0752A, 1240P, 1243P, 0232P, 1239P, 0617P, 0741P, 0425A, 0437A, 0839A
parking_Tickets_NY_2015_Violation_Times <- SparkR::sql("SELECT Violation_Code, Violation_Time,
                                                       CASE WHEN (substr(Violation_Time, 1, 4) = 'P') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 < 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 + 12
                                                       WHEN (substr(Violation_Time, 1, 4) = 'A') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 > 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 - 12
                                                       ELSE cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100
                                                       END as hour_of_the_day
                                                       FROM df_2015")
head(parking_Tickets_NY_2015_Violation_Times)
# Violation_Code Violation_Time hour_of_the_day
#             47          0143P            1.43
#             14          0719A            7.19
#             17          0455P            4.55
#             21          0948A            9.48
#             14          0951A            9.51
#             37          0226P            2.26

createOrReplaceTempView(parking_Tickets_NY_2015_Violation_Times, "df_2015_violation_time_interval")

parking_Tickets_NY_2015_Violation_TimeIntervals <- SparkR::sql("SELECT Violation_Code, hour_of_the_day,
                                                               CASE WHEN (hour_of_the_day>0 AND hour_of_the_day <= 4) THEN 'MidNight-4AM'
                                                               WHEN (hour_of_the_day >4 AND hour_of_the_day <= 8) THEN 'Early-Morning'
                                                               WHEN (hour_of_the_day >8 AND hour_of_the_day <= 12) THEN 'Morning'
                                                               WHEN (hour_of_the_day >12 AND hour_of_the_day <= 16) THEN 'After-Noon'
                                                               WHEN (hour_of_the_day >16 AND hour_of_the_day <= 20) THEN 'Evening'
                                                               ELSE 'Night'
                                                               END as violation_interval
                                                               FROM df_2015_violation_time_interval")

head(parking_Tickets_NY_2015_Violation_TimeIntervals)
# Violation_Code hour_of_the_day violation_interval
#             47            1.43       MidNight-4AM
#             14            7.19      Early-Morning
#             17            4.55      Early-Morning
#             21            9.48            Morning
#             14            9.51            Morning
#             37            2.26       MidNight-4AM

# For 2016
# Violation Time format - 0752A, 1240P, 1243P, 0232P, 1239P, 0617P, 0741P, 0425A, 0437A, 0839A
parking_Tickets_NY_2016_Violation_Times <- SparkR::sql("SELECT Violation_Code, Violation_Time,
                                                       CASE WHEN (substr(Violation_Time, 1, 4) = 'P') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 < 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 + 12
                                                       WHEN (substr(Violation_Time, 1, 4) = 'A') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 > 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 - 12
                                                       ELSE cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100
                                                       END as hour_of_the_day
                                                       FROM df_2016")
head(parking_Tickets_NY_2016_Violation_Times)
# Violation_Code Violation_Time hour_of_the_day
#             40          1240P           12.40
#             71          0914A            9.14
#             71          1129A           11.29
#             21          1136A           11.36
#             74          0919A            9.19
#             67          0810A            8.10

createOrReplaceTempView(parking_Tickets_NY_2016_Violation_Times, "df_2016_violation_time_interval")

parking_Tickets_NY_2016_Violation_TimeIntervals <- SparkR::sql("SELECT Violation_Code, hour_of_the_day,
                                                               CASE WHEN (hour_of_the_day>0 AND hour_of_the_day <= 4) THEN 'MidNight-4AM'
                                                               WHEN (hour_of_the_day >4 AND hour_of_the_day <= 8) THEN 'Early-Morning'
                                                               WHEN (hour_of_the_day >8 AND hour_of_the_day <= 12) THEN 'Morning'
                                                               WHEN (hour_of_the_day >12 AND hour_of_the_day <= 16) THEN 'After-Noon'
                                                               WHEN (hour_of_the_day >16 AND hour_of_the_day <= 20) THEN 'Evening'
                                                               ELSE 'Night'
                                                               END as violation_interval
                                                               FROM df_2016_violation_time_interval")

head(parking_Tickets_NY_2016_Violation_TimeIntervals)
# Violation_Code hour_of_the_day violation_interval
#             40           12.40         After-Noon
#             71            9.14            Morning
#             71           11.29            Morning
#             21           11.36            Morning
#             74            9.19            Morning
#             67            8.10            Morning

# For 2017
# Violation Time format - 0752A, 1240P, 1243P, 0232P, 1239P, 0617P, 0741P, 0425A, 0437A, 0839A
parking_Tickets_NY_2017_Violation_Times <- SparkR::sql("SELECT Violation_Code, Violation_Time,
                                                       CASE WHEN (substr(Violation_Time, 1, 4) = 'P') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 < 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 + 12
                                                       WHEN (substr(Violation_Time, 1, 4) = 'A') AND (cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 > 12) THEN cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100 - 12
                                                       ELSE cast(substr(Violation_Time, 1, 4) AS DOUBLE)/100
                                                       END as hour_of_the_day
                                                       FROM df_2017")
head(parking_Tickets_NY_2017_Violation_Times)
# Violation_Code Violation_Time hour_of_the_day
#             14          0435P            4.35
#             14          1016P           10.16
#             14          0940A            9.40
#             21          0838A            8.38
#             18          0805A            8.05
#             66          0543P            5.43

createOrReplaceTempView(parking_Tickets_NY_2017_Violation_Times, "df_2017_violation_time_interval")

parking_Tickets_NY_2017_Violation_TimeIntervals <- SparkR::sql("SELECT Violation_Code, hour_of_the_day,
                                                               CASE WHEN (hour_of_the_day>0 AND hour_of_the_day <= 4) THEN 'MidNight-4AM'
                                                               WHEN (hour_of_the_day >4 AND hour_of_the_day <= 8) THEN 'Early-Morning'
                                                               WHEN (hour_of_the_day >8 AND hour_of_the_day <= 12) THEN 'Morning'
                                                               WHEN (hour_of_the_day >12 AND hour_of_the_day <= 16) THEN 'After-Noon'
                                                               WHEN (hour_of_the_day >16 AND hour_of_the_day <= 20) THEN 'Evening'
                                                               ELSE 'Night'
                                                               END as violation_interval
                                                               FROM df_2017_violation_time_interval")

head(parking_Tickets_NY_2017_Violation_TimeIntervals)
# Violation_Code hour_of_the_day violation_interval
#             14            4.35      Early-Morning
#             14           10.16            Morning
#             14            9.40            Morning
#             21            8.38            Morning
#             18            8.05            Morning
#             66            5.43      Early-Morning

# Q5.2 - For each of these groups, find the 3 most commonly occurring violations

# Creating new temporary view for storing the previous data
createOrReplaceTempView(parking_Tickets_NY_2015_Violation_TimeIntervals, "df_2015_violation_count")
createOrReplaceTempView(parking_Tickets_NY_2016_Violation_TimeIntervals, "df_2016_violation_count")
createOrReplaceTempView(parking_Tickets_NY_2017_Violation_TimeIntervals, "df_2017_violation_count")

# Writing the SQLs for the three years
top_violations_per_group_2015 <- SparkR::sql("SELECT violation_interval, Violation_Code, COUNT(Violation_Code) AS count 
                                             FROM df_2015_violation_count 
                                             GROUP BY violation_interval, Violation_Code")

top_violations_per_group_2016 <- SparkR::sql("SELECT violation_interval, Violation_Code, COUNT(Violation_Code) AS count 
                                             FROM df_2016_violation_count 
                                             GROUP BY violation_interval, Violation_Code")

top_violations_per_group_2017 <- SparkR::sql("SELECT violation_interval, Violation_Code, COUNT(Violation_Code) AS count 
                                             FROM df_2017_violation_count 
                                             GROUP BY violation_interval, Violation_Code")

# Converting the Spark dataframe to R Dataframe
df_top_interval_2015 <- collect(top_violations_per_group_2015)
df_top_interval_2016 <- collect(top_violations_per_group_2016)
df_top_interval_2017 <- collect(top_violations_per_group_2017)

#Plotting the Graphs
plot_violation_2015 <- df_top_interval_2015 %>%
  dplyr::group_by(violation_interval) %>%
  dplyr::top_n(n = 3, wt = count) %>%
  dplyr::arrange(violation_interval, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2015") +
  scale_y_log10() +
  facet_grid(.~violation_interval, scales = "free")

plot_violation_2016 <- df_top_interval_2016 %>%
  dplyr::group_by(violation_interval) %>%
  dplyr::top_n(n = 3, wt = count) %>%
  dplyr::arrange(violation_interval, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2016") +
  scale_y_log10() +
  facet_grid(.~violation_interval, scales = "free")

plot_violation_2017 <- df_top_interval_2017 %>%
  dplyr::group_by(violation_interval) %>%
  dplyr::top_n(n = 3, wt = count) %>%
  dplyr::arrange(violation_interval, -count) %>%
  ggplot(aes(Violation_Code, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Code", y = "Count", title = "Frequency in 2017") +
  scale_y_log10() +
  facet_grid(.~violation_interval, scales = "free")

grid.arrange(plot_violation_2015, plot_violation_2016, plot_violation_2017, 
             ncol = 1, nrow = 3, top = "Violation Code Frequency for Top3 most commonly occurring violations - Time Slots of The Day")

# Q5.3 - Now, try another direction. For the 3 most commonly occurring violation codes, 
#        find the most common times of day (in terms of the bins from the previous part)

# 2015
# Finding the most commonly occuring violation codes.
df_top_interval_2015 %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 are 21 > 38 > 14
# Violation_Code   count
# 21             1592519
# 38             1397978
# 14              969177

# 2016
# Finding the most commonly occuring violation codes.
df_top_interval_2016 %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 are 21 > 36 > 38
# Violation_Code   count
# 21             1490775
# 36             1232910
# 38             1125950

# 2017
# Finding the most commonly occuring violation codes.
df_top_interval_2017 %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::arrange(desc(count)) %>%
  dplyr::top_n(n = 3)
# Top 3 are 21 > 36 > 38
# Violation_Code   count
# 21             1494775
# 36             1345192
# 38             1049457

# Plotting Graphs
plot_vc_2015 <- df_top_interval_2015 %>%
  dplyr::filter(Violation_Code %in% c("21","38","14")) %>%
  ggplot(aes(violation_interval, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Interval", y = "Count", title = "Frequency of Violation Interval in 2015") +
  facet_grid(.~Violation_Code, scales = "free")

plot_vc_2016 <- df_top_interval_2016 %>%
  dplyr::filter(Violation_Code %in% c("21","38","14")) %>%
  ggplot(aes(violation_interval, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Interval", y = "Count", title = "Frequency of Violation Interval in 2016") +
  facet_grid(.~Violation_Code, scales = "free")

plot_vc_2017 <- df_top_interval_2017 %>%
  dplyr::filter(Violation_Code %in% c("21","38","14")) %>%
  ggplot(aes(violation_interval, count)) +
  geom_bar(stat = "identity") +
  labs(x = "Violation Interval", y = "Count", title = "Frequency of Violation Interval in 2017") +
  facet_grid(.~Violation_Code, scales = "free")

grid.arrange(plot_vc_2015, plot_vc_2016, plot_vc_2017, 
             ncol = 1, nrow = 3, 
             top = "Violation Interval Frequency for Top 3 Violation Codes")





# Q6 - Let's try and find some seasonality in this data

# First, divide the year into some number of seasons, and 
# Reference Link - https://www.nyc.com/visitor_guide/weather_facts.75835/

# 2015
parking_Tickets_NY_2015_Seasons <- SparkR::sql("SELECT 
                                               CASE WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=12 OR CAST(substr(Issue_Date, 1,2) AS INT)=1 OR CAST(substr(Issue_Date, 1,2) AS INT)=2)  THEN 'Winter'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=3 OR CAST(substr(Issue_Date, 1,2) AS INT)=4 OR CAST(substr(Issue_Date, 1,2) AS INT)=5) THEN 'Spring'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=6 OR CAST(substr(Issue_Date, 1,2) AS INT)=7 OR CAST(substr(Issue_Date, 1,2) AS INT)=8) THEN 'Summer'
                                               ELSE 'Fall'
                                               END as season_of_year,
                                               Violation_Code
                                               FROM df_2015")
head(parking_Tickets_NY_2015_Seasons)  
# season_of_year Violation_Code
#           Fall             21
#         Spring             14
#         Summer             46
#         Spring             19
#         Spring             19
#           Fall             21

# 2016
parking_Tickets_NY_2016_Seasons <- SparkR::sql("SELECT 
                                               CASE WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=12 OR CAST(substr(Issue_Date, 1,2) AS INT)=1 OR CAST(substr(Issue_Date, 1,2) AS INT)=2)  THEN 'Winter'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=3 OR CAST(substr(Issue_Date, 1,2) AS INT)=4 OR CAST(substr(Issue_Date, 1,2) AS INT)=5) THEN 'Spring'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=6 OR CAST(substr(Issue_Date, 1,2) AS INT)=7 OR CAST(substr(Issue_Date, 1,2) AS INT)=8) THEN 'Summer'
                                               ELSE 'Fall'
                                               END as season_of_year,
                                               Violation_Code
                                               FROM df_2016")
head(parking_Tickets_NY_2016_Seasons)
# season_of_year Violation_Code
#1         Summer             21
#2         Summer             21
#3         Summer             21
#4         Summer             21
#5         Summer             21
#6         Summer             75

# 2017
parking_Tickets_NY_2017_Seasons <- SparkR::sql("SELECT 
                                               CASE WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=12 OR CAST(substr(Issue_Date, 1,2) AS INT)=1 OR CAST(substr(Issue_Date, 1,2) AS INT)=2)  THEN 'Winter'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=3 OR CAST(substr(Issue_Date, 1,2) AS INT)=4 OR CAST(substr(Issue_Date, 1,2) AS INT)=5) THEN 'Spring'
                                               WHEN (CAST(substr(Issue_Date, 1,2) AS INT)=6 OR CAST(substr(Issue_Date, 1,2) AS INT)=7 OR CAST(substr(Issue_Date, 1,2) AS INT)=8) THEN 'Summer'
                                               ELSE 'Fall'
                                               END as season_of_year,
                                               Violation_Code
                                               FROM df_2017")
head(parking_Tickets_NY_2017_Seasons)
# season_of_year Violation_Code
#         Summer              7
#         Summer              7
#         Summer              5
#         Summer             47
#           Fall             69
#         Summer              7

# Creating Temporary Views
createOrReplaceTempView(parking_Tickets_NY_2015_Seasons, "df_seasons_2015")
createOrReplaceTempView(parking_Tickets_NY_2016_Seasons, "df_seasons_2016")
createOrReplaceTempView(parking_Tickets_NY_2017_Seasons, "df_seasons_2017")

# Q6.1 - find frequencies of tickets for each season.

# 2015
parking_Tickets_Frequency_NY_2015_Seasons <- SparkR::sql("SELECT season_of_year, Violation_Code, COUNT(Violation_Code) count
                                                         FROM df_seasons_2015
                                                         GROUP BY season_of_year, Violation_Code")

# 2016
parking_Tickets_Frequency_NY_2016_Seasons <- SparkR::sql("SELECT season_of_year, Violation_Code, COUNT(Violation_Code) count
                                                         FROM df_seasons_2016
                                                         GROUP BY season_of_year, Violation_Code")

# 2017
parking_Tickets_Frequency_NY_2017_Seasons <- SparkR::sql("SELECT season_of_year, Violation_Code, COUNT(Violation_Code) count
                                                         FROM df_seasons_2017
                                                         GROUP BY season_of_year, Violation_Code")
# Creating R Dataframe
df_parking_Tickets_Frequency_NY_2015_Seasons <- collect(parking_Tickets_Frequency_NY_2015_Seasons)
df_parking_Tickets_Frequency_NY_2016_Seasons <- collect(parking_Tickets_Frequency_NY_2016_Seasons)
df_parking_Tickets_Frequency_NY_2017_Seasons <- collect(parking_Tickets_Frequency_NY_2017_Seasons)


# Plotting Frequency of Violation Codes in different Seasons of the Year 2015
df_parking_Tickets_Frequency_NY_2015_Seasons %>%
  ggplot(aes(Violation_Code,count)) +
  geom_bar(stat ="identity") +
  labs(x = "Violation Code", y ="Frequency", title = "Frequency of Violation Codes in different Seasons of the Year 2015") +
  facet_grid(season_of_year~.)

# Plotting Frequency of Violation Codes in different Seasons of the Year 2016
df_parking_Tickets_Frequency_NY_2016_Seasons %>%
  ggplot(aes(Violation_Code,count)) +
  geom_bar(stat ="identity") +
  labs(x = "Violation Code", y ="Frequency", title = "Frequency of Violation Codes in different Seasons of the Year 2016") +
  facet_grid(season_of_year~.)

# Plotting Frequency of Violation Codes in different Seasons of the Year 2017
df_parking_Tickets_Frequency_NY_2017_Seasons %>%
  ggplot(aes(Violation_Code,count)) +
  geom_bar(stat ="identity") +
  labs(x = "Violation Code", y ="Frequency", title = "Frequency of Violation Codes in different Seasons of the Year 2017") +
  facet_grid(season_of_year~.)


# Q6.2 - Then, find the 3 most common violations for each of these season

# 2015
df_parking_Tickets_Frequency_NY_2015_Seasons %>% 
  dplyr::group_by(season_of_year) %>%
  dplyr::top_n(n = 3, wt = Violation_Code) %>%
  dplyr::arrange(season_of_year, desc(count))

#season_of_year Violation_Code  count
# Fall           98             11853
# Fall           99              2140
# Fall           97               175
# Spring         98             11350
# Spring         99              1015
# Spring         97                48
# Summer         98              9971
# Summer         99              1931
# Summer         97               177
# Winter         98             10362
# Winter         99              1035
# Winter         97                37


# 2016
df_parking_Tickets_Frequency_NY_2016_Seasons %>% 
  dplyr::group_by(season_of_year) %>%
  dplyr::top_n(n = 3, wt = Violation_Code) %>%
  dplyr::arrange(season_of_year, desc(count))

# season_of_year Violation_Code  count
# Fall           98             11934
# Fall           99               759
# Fall           97                23
# Spring         98             10341
# Spring         99               881
# Spring         97                32
# Summer         98              7694
# Summer         99               754
# Summer         97                45
# Winter         98             11807
# Winter         99               832
# Winter         97                25


# 2017
df_parking_Tickets_Frequency_NY_2017_Seasons %>% 
  dplyr::group_by(season_of_year) %>%
  dplyr::top_n(n = 3, wt = Violation_Code) %>%
  dplyr::arrange(season_of_year, desc(count))

# season_of_year Violation_Code  count
# Fall           98             12757
# Fall           99               676
# Fall           97                25
# Spring         98             12995
# Spring         99               757
# Spring         97                28
# Summer         98              9012
# Summer         99               747
# Summer         97                23
# Winter         98             12657
# Winter         99               670
# Winter         97                19



# Q7  - The fines collected from all the parking violation constitute a revenue source for the NYC police department. Let's take an example of estimating that for the 3 most commonly occurring codes.
# Q7.1 - Find total occurrences of the 3 most common violation codes

# 3 most violation codes for 2015
df_violation_2015 <- df_parking_Tickets_Frequency_NY_2015_Seasons %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::top_n(n = 3) %>%
  dplyr::arrange(desc(count))
df_violation_2015
# Violation_Code   count
#             21 1464127
#             38 1304009
#             14  905715


# 3 most violation codes for 2016
df_violation_2016 <- df_parking_Tickets_Frequency_NY_2016_Seasons %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::top_n(n = 3) %>%
  dplyr::arrange(desc(count))
df_violation_2016
# Violation_Code   count
#             21 1490775
#             36 1232910
#             38 1125950


# 3 most violation codes for 2017
df_violation_2017 <- df_parking_Tickets_Frequency_NY_2017_Seasons %>% 
  dplyr::group_by(Violation_Code) %>%
  dplyr::summarise(count = sum(count)) %>%
  dplyr::top_n(n = 3) %>%
  dplyr::arrange(desc(count))
df_violation_2017
# Violation_Code   count
#             21 1494775
#             36 1345192
#             38 1049457


# Q7.2 - Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines. 
#        They're divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. 
#         For simplicity, take an average of the two.
# Reference Link - http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page
# Violation Code - Avg Charge
#             14   115
#             21    55
#             36    50
#             38    50
NYCparking_violationcode_fines <- data.frame(Violation_Code=c(21,38, 14, 36), Fine_Amount=c(55, 50, 115, 50))


# Total Fine collected for 2015
fine_2015 <- merge(df_violation_2015, NYCparking_violationcode_fines)
fine_2015$total <- fine_2015$count * fine_2015$Fine_Amount

sum(fine_2015$total)
# Total Fine for 2015 = USD 268,942, 800


# Total Fine collected for 2016
fine_2016 <- merge(df_violation_2016, NYCparking_violationcode_fines)
fine_2016$total <- fine_2016$count * fine_2016$Fine_Amount

sum(fine_2016$total)
# Total Fine for 2016 = USD 199,935,625


# Total Fine collected for 2017
fine_2017 <- merge(df_violation_2017, NYCparking_violationcode_fines)
fine_2017$total <- fine_2017$count * fine_2017$Fine_Amount

sum(fine_2017$total)
# Total Fine for 2017 = USD 201,945,075


# Q7.3 - What can you intuitively infer from these findings?
# Violation Code 21 - 

# 24.65% of Tickets from Violation Code 21
round((fine_2015[which.max(fine_2015$Fine_Amount), 2] / sum(fine_2015$count)*100),2)


# 52.27% Revenue generated for NYC police department
round((fine_2015[which.max(fine_2015$Fine_Amount), 3] / sum(fine_2015$Fine_Amount))*100,2)


# 38.73% of Tickets from Violation Code 21
round((fine_2016[which.max(fine_2016$Fine_Amount), 2] / sum(fine_2016$count)*100),2)


# 35.48% Revenue generated for NYC police department
round((fine_2016[which.max(fine_2016$Fine_Amount), 3] / sum(fine_2016$Fine_Amount))*100,2)

# 38.43% of Tickets from Violation Code 21
round((fine_2017[which.max(fine_2017$Fine_Amount), 2] / sum(fine_2017$count)*100),2)


# 35.48% Revenue generated for NYC police department
round((fine_2017[which.max(fine_2017$Fine_Amount), 3] / sum(fine_2017$Fine_Amount))*100,2)

