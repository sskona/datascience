#############################################################################################################################################################################
#                     GlobalMart - Retail-Giant Sales Forecasting
#############################################################################################################################################################################
#
# Business Understanding & Objective
# ----------------------------------
# The store caters to 7 different market segments and in 3 major categories. The objective is to finalize the plan for 
# next 6 months by forecasting the sales and the demand for next 6 months, that would help to manage the revenue and 
# inventory accordingly.
#
# Find out 2 most profitable (and consistent) segment from these 21 and forecast the sales and demand for these segments.
# 
# Data Understanding & Preparation
# --------------------------------
# The data is at Order Date level and per transaction. The data needs to be aggregated to at month level per each of 21 segments
# for all Sales, Quantity and Profit.
#
# Model building
# ---------------
# Use Classical decomposition and auto ARIMA for forecasting.
# Smoothen the data before performing classical decomposition
#
# Model evaluation
# ----------------
# Forcast Sales and Demand for next 6 months
# Evaluate Model using MAPE with last 6 months values
#
#### Loading libraries and getting data####

library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(ggthemes, warn.conflicts = FALSE)
library(gridExtra, warn.conflicts = FALSE)
library(graphics, warn.conflicts = FALSE)
library(forecast, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tseries, warn.conflicts = FALSE)


#Loading Data
globalmart <- read.csv("Global Superstore.csv",
                       header = TRUE, stringsAsFactors = FALSE)
dim(globalmart)
# 51290 observations and 24 variables
str(globalmart)
View(globalmart)

######################################################################
### Check for Data Discrepancies

summary(globalmart)

# Checking missing/NA  valueS
sapply(globalmart, function(x) sum(is.na(x)))
# Only Postal Code field has 41296 NA values

# Remove duplicate values (if any) in the dataset
nrow(unique(globalmart))  #51290
# There are no duplicates

# Checking NULL values
sum(is.null(globalmart))
is.null(globalmart)
# There are no NULL values

# Checking blank values
sapply(globalmart, function(x) length(which(x == "")))
# There are no blank values

# Converting into factors
cols <- c("Ship.Mode",
          "Segment",
          "City",
          "State",
          "Country",
          "Market",
          "Region",
          "Category",
          "Sub.Category",
          "Order.Priority")
globalmart[cols] <- lapply(globalmart[cols], factor)
str(globalmart)

####segment the whole dataset into the 21 subsets based on the market and the customer segment level.####
#7 different market segments and 3 major categories(consumer , corporate and home office)
levels(globalmart$Market)
#"Africa" "APAC"   "Canada" "EMEA"   "EU"     "LATAM"  "US" 
levels(globalmart$Segment)
#"Consumer"    "Corporate"   "Home Office"

######################################################################
### Exploring Data

# Plot - Profits in Various Markets
globalmart %>%
  group_by(Market) %>%
  summarise(total_profit = sum(Profit)) %>%
  ggplot(aes(x = Market, y = sort(total_profit, decreasing = TRUE), fill=Market)) +
  geom_col() +
  theme_light() +
  labs(title = "Profits in Various Markets", x = "Market", y = "Total Profits")

# Plot - Profits in Various Regions
globalmart %>%
  group_by(Market, Region) %>%
  summarise(total_profit = sum(Profit)) %>%
  ggplot(aes(x = Region, y = sort(total_profit, decreasing = TRUE),fill=Region)) +
  geom_col() +
  facet_wrap(~Market, scales = "free") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Profits in Various Regions", x = "Region", y = "Total Profit")

# Plot - Segment wise Profits
globalmart %>%
  group_by(Segment) %>%
  summarise(total_profit = sum(Profit)) %>%
  ggplot(aes(Segment, total_profit , fill=Segment)) +
  geom_col() +
  theme_light() +
  labs(title = "Segment wise Profits", x = "Segment", y = "Total Profits")


# Convert Date columns to Date format and create Combined segment-market field
globalmart <- globalmart %>%
  mutate(MarketSegment = paste(Market, Segment, sep = "-"),
         Order.Date = as.Date(Order.Date, format = "%d-%m-%Y"),
         Ship.Date = as.Date(Ship.Date, format = "%d-%m-%Y"),
         order_year = format(Order.Date, "%Y"),
         OrderMonth = format(Order.Date, "%Y-%m"),
         ShipMonth = format(Ship.Date, "%Y-%m"))

# Verify that 21 Segments are created
unique(globalmart$MarketSegment)
length(unique(globalmart$MarketSegment))

# Finding the most profitable segment-market  
globalmart %>%
  group_by(MarketSegment) %>%
  summarise(total_profit = sum(Profit)) %>%
  top_n(10) %>%
  ggplot(aes(x = MarketSegment, sort(total_profit, decreasing = TRUE),fill=MarketSegment)) +
  geom_col() +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Top 5 Segment-Market", x = "Segment-Market", y = "Profit")
# APAC-Consumer, APAC-Corporate, APAC-Home office, EU-Consumer and EU-Corporate
# are the Top 5 most profitable pairs


# Finding whether the above segment-market is consistent throughout all the years from 2011 to 2014
globalmart %>%
  group_by(order_year, MarketSegment) %>%
  summarise(total_profit = sum(Profit)) %>%
  top_n(10) %>%
  ggplot(aes(x = MarketSegment, sort(total_profit, decreasing = TRUE) , fill=MarketSegment)) +
  geom_col() +
  facet_grid(order_year~.) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Top 5 Segment-Market", x = "Segment-Market", y = "Profit")

# APAC-Consumer, APAC-Corporate, APAC-Home office, EU-Consumer and
# EU-Corporate have been most consistent in Top 6 most profitable pairs



###########################################################

# Aggregate the 3 attributes - Sales, Quantity & Profit
# Over the Order Date to arrive at monthly values for these attributes

globalmart_aggregated <- globalmart %>%
  group_by(MarketSegment, OrderMonth) %>%
  summarise(Sales = sum(Sales),
            Profit = sum(Profit),
            Quantity = sum(Quantity))

head(globalmart_aggregated)

# Calculate 'Coefficient of Variation' for each Markset Segment
CoeffcientOfVariation <- function(x) sd(x) / abs(mean(x))

globalmart_marketsegments_top10 <- globalmart %>%
  group_by(MarketSegment) %>%
  summarise(CV = CoeffcientOfVariation(Profit),
            Profit = sum(Profit)) %>%
  arrange(desc(Profit), CV) %>%
  top_n(10)

globalmart_marketsegments_top10

#---------------------------------
#   MarketSegment      CV   Profit
#---------------------------------
#  1 APAC-Consumer     4.21 222818
#  2 EU-Consumer       4.72 188688
#  3 US-Consumer       9.39 134119
#  4 APAC-Corporate    4.23 129737
#  5 EU-Corporate      4.78 123394
#  6 LATAM-Consumer    5.44 120633
#  7 US-Corporate      7.62  91979
#  8 APAC-Home Office  4.63  83445
#  9 EU-Home Office    4.92  60748
# 10 US-Home Office    6.28  60299
#---------------------------------

# Market-Segment 'APAC-Consumer' has highest Profit and Lowest CV

# Choosing 'EU-Consumer' following two reasons
# Reason 1. 2nd Most Profitable Market-Segment because Profit of 188687.71 is much higher than 129737.23 of APAC-Corporate
# Reason 2. CV 4.776482 of 'EU-Consumer' is not very much higher than 4.231301 of 'APAC-Corporate'

globalmart_aggregated_MarketSegment1 <- globalmart_aggregated %>%
  filter(MarketSegment == globalmart_marketsegments_top10[1, 1])

globalmart_aggregated_MarketSegment1

globalmart_aggregated_MarketSegment2 <- globalmart_aggregated %>%
  filter(MarketSegment == globalmart_marketsegments_top10[2, 1])

globalmart_aggregated_MarketSegment2

# Verify the Sequence of Order Months is retained in the data-sets
# Comparing existing sequence of Year-Month with Sorted Order 
# A Total of 48 Months Sales
(sum (globalmart_aggregated_MarketSegment1$OrderMonth == globalmart_aggregated_MarketSegment1 [order(globalmart_aggregated_MarketSegment1$OrderMonth), 2]) == 48)

# A Total of 48 Months Sales
(sum (globalmart_aggregated_MarketSegment2$OrderMonth == globalmart_aggregated_MarketSegment2 [order(globalmart_aggregated_MarketSegment2$OrderMonth), 2]) == 48)

###############################################  
# CREATING TIME SERIES OBJECTS
# for Sales and Quantity for both the Market Segments


# MARKET SEGMENT 1

# Arranging the DF in ascending OrderMonth.
globalmart_aggregated_MarketSegment1 <- globalmart_aggregated_MarketSegment1 %>% arrange(OrderMonth)

# Time Series object for Sales of Market Segment 1
Sales_timeSeries_MarketSegment1 <- ts(globalmart_aggregated_MarketSegment1$Sales)
plot(Sales_timeSeries_MarketSegment1)
abline(reg = lm(Sales_timeSeries_MarketSegment1~time(Sales_timeSeries_MarketSegment1)))
# It appears to be a incremental trend 


# Time Series object for Quantity of Market Segment 1
Quantity_timeSeries_MarketSegment1 <- ts(globalmart_aggregated_MarketSegment1$Quantity)
plot(Quantity_timeSeries_MarketSegment1)
abline(reg = lm(Quantity_timeSeries_MarketSegment1~time(Quantity_timeSeries_MarketSegment1)))
# It appears to be an incremental trend


# MARKET SEGMENT 2

# Arranging the DF in ascending OrderMonth.
globalmart_aggregated_MarketSegment2 <- globalmart_aggregated_MarketSegment2 %>% arrange(OrderMonth)

# Time Series object for Sales of Market Segment 2
Sales_timeSeries_MarketSegment2 <- ts(globalmart_aggregated_MarketSegment2$Sales)
plot(Sales_timeSeries_MarketSegment2)
abline(reg = lm(Sales_timeSeries_MarketSegment2~time(Sales_timeSeries_MarketSegment2)))
# It appears to be a incremental trend 


# Time Series object for Quantity of Market Segment 2
Quantity_timeSeries_MarketSegment2 <- ts(globalmart_aggregated_MarketSegment2$Quantity)
plot(Quantity_timeSeries_MarketSegment2)
abline(reg = lm(Quantity_timeSeries_MarketSegment2~time(Quantity_timeSeries_MarketSegment2)))
# It appears to be an incremental trend


#########################################  Smoothen the data ######################################
smoothing <- function (x, y) {
  col <- length(x) - y
  
  plot(x)
  
  #Smoothing the series - Moving Average Smoothing
  
  w <-y
  
  # convolution - Moving Average
  smoothedseries <- stats::filter(x, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(x)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  #Plot the smoothed time series
  
  
  lines(smoothedseries, col="blue", lwd=2)
  
  #timevals_in <- x[1:col]
  smootheddf <- as.data.frame(cbind(seq(1:length(x)), as.vector(smoothedseries)))
  
  return(smootheddf)
}

################################### Classical decomposition ########################################
# All 4 Time Series have 48 months data
timevals_in <- seq(1:42)
out_timevals <- seq(43:48)

#--- Market Segment 1 - Classical Decomposition - Sales_smoothTS_MarketSegment1 ------

Sales_smoothTS_MarketSegment1 <- smoothing(Sales_timeSeries_MarketSegment1, 1)
colnames(Sales_smoothTS_MarketSegment1) <- c('Month', 'Sales')
View(Sales_smoothTS_MarketSegment1)

timevals <- c(1:nrow(Sales_smoothTS_MarketSegment1))

in_data_Sales_smoothTS_MarketSegment1 <- Sales_smoothTS_MarketSegment1 [1:42,]
out_data_Sales_smoothTS_MarketSegment1 <- Sales_smoothTS_MarketSegment1 [43:48,]


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Sales_MarketSegment1 <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) * cos(0.5*Month)
                                 + Month, data=in_data_Sales_smoothTS_MarketSegment1)

global_pred_Sales_MarketSegment1 <- predict(lmfit_Sales_MarketSegment1, Month=timevals_in)
summary(global_pred_Sales_MarketSegment1)
lines(timevals_in, global_pred_Sales_MarketSegment1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_Sales_MarketSegment1 <- in_data_Sales_smoothTS_MarketSegment1$Sales - global_pred_Sales_MarketSegment1
plot(local_pred_Sales_MarketSegment1, col='red', type = "l")

# ACF plot shows values are stationary
acf(local_pred_Sales_MarketSegment1)
acf(local_pred_Sales_MarketSegment1, type="partial")
armafit_Sales_MarketSegment1 <- auto.arima(local_pred_Sales_MarketSegment1)

tsdiag(armafit_Sales_MarketSegment1)
# sigma^2 estimated as 5436309:  log likelihood=-385.28
# AIC=772.55   AICc=772.65   BIC=774.29
armafit_Sales_MarketSegment1

# Series: local_pred_Sales_MarketSegment1 
# ARIMA(0,0,0) with zero mean 

# sigma^2 estimated as 5436309:  log likelihood=-385.28
# AIC=772.55   AICc=772.65   BIC=774.29

# Check if the residual series is white noise

resi_Sales_MarketSegment1 <- local_pred_Sales_MarketSegment1-fitted(armafit_Sales_MarketSegment1)
plot(resi_Sales_MarketSegment1)

adf.test(resi_Sales_MarketSegment1,alternative = "stationary")
#p-value =0.01
kpss.test(resi_Sales_MarketSegment1)
#p-value =0.1
#The series is stationary which indicates it is white noise

# Evaluation the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- out_data_Sales_smoothTS_MarketSegment1$Month

global_pred_out_Sales_MarketSegment1 <- predict(lmfit_Sales_MarketSegment1,data.frame(Month = out_data_Sales_smoothTS_MarketSegment1$Month))

#Compare prediction with the actual values, using MAPE

MAPE_Sales_MarketSegment1 <- accuracy(global_pred_out_Sales_MarketSegment1, out_data_Sales_smoothTS_MarketSegment1[,2])[5]
MAPE_Sales_MarketSegment1
#27.17518

# Plotting forecasted values along with actual values
class_dec_pred <- c(ts(global_pred_Sales_MarketSegment1),ts(global_pred_out_Sales_MarketSegment1))
plot(Sales_timeSeries_MarketSegment1, col = "black")
lines(class_dec_pred, col = "red")

# Future prediction for 49 to 54 periods along with periods 1 to 48
global_futurepred_Sales_MarketSegment1 <- predict(lmfit_Sales_MarketSegment1,data.frame(Month = ts(seq(43:54))))
local_futurepred_Sales_MarketSegment1 <- predict(armafit_Sales_MarketSegment1, n.ahead = 12)

futurepred_Sales_MarketSegment1 <- global_futurepred_Sales_MarketSegment1 + as.data.frame(local_futurepred_Sales_MarketSegment1)[,2]

par(mfrow = c(2,1))
class_dec_futurepred <- c(ts(global_pred_Sales_MarketSegment1),ts(futurepred_Sales_MarketSegment1))
plot(Sales_timeSeries_MarketSegment1, col = "black")
title(main = "Model Evaluation and Forecast for Sales in APAC-Consumer")
lines(class_dec_futurepred, col = "red")
plot(ts(futurepred_Sales_MarketSegment1, start = 43, end = 54), type="l", lty=2, lwd=3)
title(main = "Sales Forecast upto 54 Months ")
par(mfrow = c(1,1))

#--- Market Segment 1 - Classical Decomposition - Quantity_smoothTS_MarketSegment1 -----

Quantity_smoothTS_MarketSegment1 <- smoothing(Quantity_timeSeries_MarketSegment1, 1)
colnames(Quantity_smoothTS_MarketSegment1) <- c('Month', 'Quantity')
View(Quantity_smoothTS_MarketSegment1)

timevals <- c(1:nrow(Quantity_smoothTS_MarketSegment1))

in_data_Quantity_smoothTS_MarketSegment1 <- Quantity_smoothTS_MarketSegment1 [1:42,]
out_data_Quantity_smoothTS_MarketSegment1 <- Quantity_smoothTS_MarketSegment1 [43:48,]


# Fit a multiplicative model with trend and seasonality to the data
# Seasonality modeled using a sinusoid function

lmfit_Quantity_MarketSegment1 <- lm(Quantity ~ sin(0.5*Month) * poly(Month,1) * cos(0.5*Month)
                                    + Month, data=in_data_Quantity_smoothTS_MarketSegment1)

global_pred_Quantity_MarketSegment1 <- predict(lmfit_Quantity_MarketSegment1, Month=timevals_in)
summary(global_pred_Quantity_MarketSegment1)
lines(timevals_in, global_pred_Quantity_MarketSegment1, col='red', lwd=2)

#Verify locally predictable series
#Modeling as an ARMA series

local_pred_Quantity_MarketSegment1 <- in_data_Quantity_smoothTS_MarketSegment1$Quantity - global_pred_Quantity_MarketSegment1
plot(local_pred_Quantity_MarketSegment1, col='red', type = "l")
acf(local_pred_Quantity_MarketSegment1)
acf(local_pred_Quantity_MarketSegment1, type="partial")
armafit_Quantity_MarketSegment1 <- auto.arima(local_pred_Quantity_MarketSegment1)

# Series: local_pred_Quantity_MarketSegment1 
# ARIMA(3,0,2) with zero mean 

# Coefficients:
#  ar1      ar2      ar3     ma1     ma2
# -0.2624  -0.3570  -0.4485  0.8661  0.7862
# s.e.   0.2083   0.1625   0.1667  0.2126  0.2064

# sigma^2 estimated as 732.8:  log likelihood=-197.1
# AIC=406.21   AICc=408.61   BIC=416.63

tsdiag(armafit_Quantity_MarketSegment1)
#sigma^2 estimated as 732.8:  log likelihood=-197.1
#AIC=406.21   AICc=408.61   BIC=416.63
armafit_Quantity_MarketSegment1

#Check if the residual series is white noise

resi_Quantity_MarketSegment1 <- local_pred_Quantity_MarketSegment1-fitted(armafit_Quantity_MarketSegment1)
plot(resi_Quantity_MarketSegment1)

adf.test(resi_Quantity_MarketSegment1,alternative = "stationary")
#p-value=0.0334
kpss.test(resi_Quantity_MarketSegment1)
#p-value=0.1
#The series is stationary which indicates it is white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

global_pred_out_Quantity_MarketSegment1 <- predict(lmfit_Quantity_MarketSegment1,data.frame(Month = out_data_Quantity_smoothTS_MarketSegment1$Month))

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_Quantity_MarketSegment1 <- accuracy(global_pred_out_Quantity_MarketSegment1, out_data_Quantity_smoothTS_MarketSegment1[,2])[5]
MAPE_Quantity_MarketSegment1
#18.29203

#Plot the forecasted values along with original values
class_dec_pred <- c(ts(global_pred_Quantity_MarketSegment1),ts(global_pred_out_Quantity_MarketSegment1))
plot(Quantity_timeSeries_MarketSegment1, col = "black")
lines(class_dec_pred, col = "red")  

# Future prediction for 49 to 54 periods along with periods 1 to 48
global_futurepred_Quantity_MarketSegment1 <- predict(lmfit_Quantity_MarketSegment1,data.frame(Month = ts(seq(43:54))))
local_futurepred_Sales_MarketSegment1 <- predict(armafit_Quantity_MarketSegment1, n.ahead = 12)


futurepred_Quantity_MarketSegment1 <- global_futurepred_Quantity_MarketSegment1 + as.data.frame(local_futurepred_Sales_MarketSegment1)[,2]

par(mfrow = c(2,1))
class_dec_Quantityfuturepred <- c(ts(global_pred_Quantity_MarketSegment1), ts(futurepred_Quantity_MarketSegment1))
plot(Quantity_timeSeries_MarketSegment1, col = "black")
title(main = "Model Evaluation and Forecast for Quantity in APAC-Consumer")
lines(class_dec_Quantityfuturepred, col = "red")
plot(ts(class_dec_Quantityfuturepred, start = 43, end = 54), type="l", lty=2, lwd=3)
title(main = "Quantity Forecast upto 54 Months")
par(mfrow = c(1,1))


#----- Market Segment 2 - Classical Decomposition - Sales_smoothTS_MarketSegment2 -----  

Sales_smoothTS_MarketSegment2    <- smoothing(Sales_timeSeries_MarketSegment2, 1)
colnames(Sales_smoothTS_MarketSegment2) <- c('Month', 'Sales')
View(Sales_smoothTS_MarketSegment2)

timevals <- c(1:nrow(Sales_smoothTS_MarketSegment2))

in_data_Sales_smoothTS_MarketSegment2 <- Sales_smoothTS_MarketSegment2 [1:42,]
out_data_Sales_smoothTS_MarketSegment2 <- Sales_smoothTS_MarketSegment2 [43:48,]


#Fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Sales_MarketSegment2 <- lm(Sales ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month)  * poly(Month,3)
                                 + Month, data=in_data_Sales_smoothTS_MarketSegment2)

global_pred_Sales_MarketSegment2 <- predict(lmfit_Sales_MarketSegment2, Month=timevals_in)
summary(global_pred_Sales_MarketSegment2)
lines(timevals_in, global_pred_Sales_MarketSegment2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_Sales_MarketSegment2 <- in_data_Sales_smoothTS_MarketSegment2$Sales - global_pred_Sales_MarketSegment2
plot(local_pred_Sales_MarketSegment2, col='red', type = "l")
acf(local_pred_Sales_MarketSegment2)
acf(local_pred_Sales_MarketSegment2, type="partial")
armafit_Sales_MarketSegment2 <- auto.arima(local_pred_Sales_MarketSegment2)

tsdiag(armafit_Sales_MarketSegment2)

# Series: local_pred_Sales_MarketSegment2 
# ARIMA(0,0,1) with zero mean 

# Coefficients:
#  ma1
# 0.4646
# s.e.  0.1384

# sigma^2 estimated as 10059208:  log likelihood=-397.81
# AIC=799.63   AICc=799.94   BIC=803.11

armafit_Sales_MarketSegment2

# Check if the residual series is white noise

resi_Sales_MarketSegment2 <- local_pred_Sales_MarketSegment2-fitted(armafit_Sales_MarketSegment2)
plot(resi_Sales_MarketSegment2)

adf.test(resi_Sales_MarketSegment2,alternative = "stationary")
#p-value = 0.01
kpss.test(resi_Sales_MarketSegment2)
#p-value = 0.1
#This indicates it is stationary i.e. white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- out_data_Sales_smoothTS_MarketSegment2$Month

global_pred_out_Sales_MarketSegment2 <- predict(lmfit_Sales_MarketSegment2,data.frame(Month = out_data_Sales_smoothTS_MarketSegment2$Month))

fcast <- global_pred_out_Sales_MarketSegment2

#Compare prediction with the actual values, using MAPE

MAPE_Sales_MarketSegment2 <- accuracy(fcast,out_data_Sales_smoothTS_MarketSegment2[,2])[5]
MAPE_Sales_MarketSegment2
#6.195639

#lot the predictions along with original values, to

class_dec_pred <- c(ts(global_pred_Sales_MarketSegment2),ts(global_pred_out_Sales_MarketSegment2))
plot(Sales_timeSeries_MarketSegment2, col = "black")
lines(class_dec_pred, col = "red")  

# Future prediction for 49 to 54 periods along with periods 1 to 48
global_futurepred_Sales_MarketSegment2 <- predict(lmfit_Sales_MarketSegment2,data.frame(Month = ts(seq(43:54))))
local_futurepred_Sales_MarketSegment2 <- predict(armafit_Sales_MarketSegment2, n.ahead = 12)

futurepred_Sales_MarketSegment2 <- global_futurepred_Sales_MarketSegment2 + as.data.frame(local_futurepred_Sales_MarketSegment2)[,2]

par(mfrow = c(2,1))
class_dec_futurepredSales <- c(ts(global_pred_Sales_MarketSegment2),ts(futurepred_Sales_MarketSegment2))
plot(Sales_timeSeries_MarketSegment2, col = "black")
title(main = "Model Evaluation and Forecast for Quantity in EU-Consumer")
lines(class_dec_futurepredSales, col = "red")
plot(ts(futurepred_Sales_MarketSegment2, start = 43, end = 54), type="l", lty=2, lwd=3)
title(main = "Quantity Forecast Upto 54 Months")
par(mfrow = c(1,1))

#----- Market Segment 2 - Classical Decomposition - Quantity_smoothTS_MarketSegment2 -----

Quantity_smoothTS_MarketSegment2 <- smoothing(Quantity_timeSeries_MarketSegment2, 1)
colnames(Quantity_smoothTS_MarketSegment2) <- c('Month', 'Quantity')
View(Quantity_smoothTS_MarketSegment2)

timevals <- c(1:nrow(Quantity_smoothTS_MarketSegment2))

in_data_Quantity_smoothTS_MarketSegment2 <- Quantity_smoothTS_MarketSegment2 [1:42,]
out_data_Quantity_smoothTS_MarketSegment2 <- Quantity_smoothTS_MarketSegment2 [43:48,]


#Fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Quantity_MarketSegment2 <- lm(Quantity ~ sin(0.6*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3)
                                    + Month, data=in_data_Quantity_smoothTS_MarketSegment2)

global_pred_Quantity_MarketSegment2 <- predict(lmfit_Quantity_MarketSegment2, Month=timevals_in)
summary(global_pred_Quantity_MarketSegment2)
lines(timevals_in, global_pred_Quantity_MarketSegment2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_Quantity_MarketSegment2 <- in_data_Quantity_smoothTS_MarketSegment2$Quantity - global_pred_Quantity_MarketSegment2
plot(local_pred_Quantity_MarketSegment2, col='red', type = "l")
acf(local_pred_Quantity_MarketSegment2)
acf(local_pred_Quantity_MarketSegment2, type="partial")
armafit_Quantity_MarketSegment2 <- auto.arima(local_pred_Quantity_MarketSegment2)

tsdiag(armafit_Quantity_MarketSegment2)
# Series: local_pred_Quantity_MarketSegment2 
# ARIMA(0,0,0) with zero mean

# sigma^2 estimated as 1013:  log likelihood=-204.93
# AIC=411.86   AICc=411.96   BIC=413.6
armafit_Quantity_MarketSegment2

# Check if the residual series is white noise

resi_Quantity_MarketSegment2 <- local_pred_Quantity_MarketSegment2-fitted(armafit_Quantity_MarketSegment2)
plot(resi_Quantity_MarketSegment2)

adf.test(resi_Quantity_MarketSegment2,alternative = "stationary")
#p-value = 0.01
kpss.test(resi_Quantity_MarketSegment2)
# p-value = 0.1
#This indicates it is stationary series i.e. white noise

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

global_pred_out_Quantity_MarketSegment2 <- predict(lmfit_Quantity_MarketSegment2,data.frame(Month = out_data_Quantity_smoothTS_MarketSegment2$Month))

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_Quantity_MarketSegment2 <- accuracy(global_pred_out_Quantity_MarketSegment2, out_data_Quantity_smoothTS_MarketSegment2[,2])[5]
MAPE_Quantity_MarketSegment2
#10.01007

# Plot future forecasts along with original values and fitted values for trained data

class_dec_pred <- c(ts(global_pred_Quantity_MarketSegment2),ts(global_pred_out_Quantity_MarketSegment2))
plot(Quantity_timeSeries_MarketSegment2, col = "black")
lines(class_dec_pred, col = "red")  

# Future prediction for 49 to 54 periods along with periods 1 to 48
global_futurepred_Quantity_MarketSegment2 <- predict(lmfit_Quantity_MarketSegment2,data.frame(Month = ts(seq(43:54))))
local_futurepred_Sales_MarketSegment2 <- predict(armafit_Quantity_MarketSegment2, n.ahead = 12)


futurepred_Quantity_MarketSegment2 <- global_futurepred_Quantity_MarketSegment2 + as.data.frame(local_futurepred_Sales_MarketSegment2)[,2]

par(mfrow = c(2,1))
class_dec_Quantityfuturepred2 <- c(ts(global_pred_Quantity_MarketSegment2), ts(futurepred_Quantity_MarketSegment2))
plot(Quantity_timeSeries_MarketSegment2, col = "black")
title(main = "Model Evaluation and Forecast for Quantity in EU-Consumer")
lines(class_dec_Quantityfuturepred2, col = "red")
plot(ts(class_dec_Quantityfuturepred2, start = 43, end = 54), type="l", lty=2, lwd=3)
title(main = "Quantity Forecast Upto 54 Months")
par(mfrow = c(1,1))

###################################################  Auto ARIMA ######################################

forecastWithAutoARIMAModel <- function (timeSeries, numberOfForecasts) {
  par(mfrow = c(3,1))
  numberOfFittedValues <- length(timeSeries) - numberOfForecasts
  totalOfPeriods <- length(timeSeries)
  
  cat("\nTotal No. Of Months      = ", totalOfPeriods)
  cat("\nFitted Months            = ", numberOfFittedValues)
  cat("\nForecasted Months        = ", numberOfForecasts)
  
  # Auto ARIMA - Sales for Market Segment 1
  autoarima_MarketSegment <- auto.arima(timeSeries[1:numberOfFittedValues])
  
  cat ("\n\n--------- Auto ARIMA Model ---------\n\n")
  print(autoarima_MarketSegment)
  
  tsdiag(autoarima_MarketSegment)
  plot(autoarima_MarketSegment$x, col="black")
  title(main = "Time Series and Auto ARIMA model")
  lines(fitted(autoarima_MarketSegment), col="red")
  
  #Again, let's check if the residual series is white noise
  resi_auto_arima_Sales_MarketSegment1 <- timeSeries[1:numberOfFittedValues] - fitted(autoarima_MarketSegment[1:numberOfFittedValues])
  
  cat ("\n\n--------- White Noise Testing -------\n\n")
  
  # Augmented Dickey-Fuller Test
  print(adf.test(resi_auto_arima_Sales_MarketSegment1,alternative = "stationary"))
  
  # KPSS Test for Level Stationarity
  print(kpss.test(resi_auto_arima_Sales_MarketSegment1))
  
  #outdata <- timeSeries[37:42]
  
  # Evaluate the model using MAPE
  fcast_auto_arima <- predict(autoarima_MarketSegment, n.ahead = numberOfForecasts)
  
  MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,timeSeries[(numberOfFittedValues+1):totalOfPeriods])[5]
  
  cat ("\n\n--------- Accuracy using MAPE -------\n\n")
  cat("MAPE = ", MAPE_auto_arima)
  cat ("\n\n")
  
  #Lastly, let's plot the predictions along with original values, to
  #get a visual feel of the fit
  
  auto_arima_pred <- c(fitted(autoarima_MarketSegment),ts(fcast_auto_arima$pred))
  
  plot(timeSeries, col = "black")
  title(main = "Evaluation of model with last 6 months data")
  lines(auto_arima_pred, col = "red")
  
  fcast_auto_arima2 <- predict(autoarima_MarketSegment, n.ahead = 12)
  
  cat ("\n\n--------- Predicting 6 more periods (Months 49 to 54) beyond test data  -------\n\n")
  
  print(fcast_auto_arima2$pred)
  
  lines(c(fitted(autoarima_MarketSegment), fcast_auto_arima2$pred), col = "blue")
  
  # Plot for showing predictions for Test data and future periods
  plot(fcast_auto_arima2$pred, type="l", lty=2, lwd=3)
  title(main = "Last 6 months and 6 months Future Forecast")
  par(mfrow = c(1,1))
}

# ADF Test p-value = 0.01, KPSS Test p-value = 0.1
# Both tests confirm Residuals are Stationary
# MAPE =  27.68952
forecastWithAutoARIMAModel(Sales_timeSeries_MarketSegment1, 6)

# ADF Test p-value = 0.01, KPSS Test p-value = 0.1
# Both tests confirm Residuals are Stationary
# MAPE =  26.24458
forecastWithAutoARIMAModel(Quantity_timeSeries_MarketSegment1, 6)

# ADF Test p-value = 0.01, KPSS Test p-value = 0.1
# Both tests confirm Residuals are Stationary
# MAPE =  28.9226
forecastWithAutoARIMAModel(Sales_timeSeries_MarketSegment2, 6)

# ADF Test p-value = 0.04521, KPSS Test p-value = 0.1
# Both tests confirm Residuals are Stationary
# MAPE =  30.13319
forecastWithAutoARIMAModel(Quantity_timeSeries_MarketSegment2, 6)

# Recommendations
# Classical Decompositions Methods are performing well

#


