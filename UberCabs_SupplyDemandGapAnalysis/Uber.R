# Load Libraries
#---------------

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Business Objectives
# -------------------
#     Analysis is to 
#       1. identify the root cause of the problem (i.e. cancellation and non-availability of cars)
#       2. possible hypotheses of the problem(s)
#       3. recommend ways to improve them
#
# Read the data file
uber_Requests <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE, header = TRUE)

# Data Understanding
# ------------------
# Only the trips to and from the airport are being considered.
# Following attributes associated with each request made by a customer:
#
#       1. Request id: A unique identifier of the request
#       2. Time of request: The date and time at which the customer made the trip request
#       3. Drop-off time: The drop-off date and time, in case the trip was completed 
#       4. Pick-up point: The point from which the request was made
#       5. Driver id: The unique identification number of the driver
#       6. Status of the request: The final status of the trip, that can be either completed, cancelled by 
#          the driver or no cars available
 

# Data Cleaning 
# -------------
# 
# Date Time formats
#     columns         - Request.timestamp, Drop.timestamp
#     Current formats - DD/MM/YYYY HH:MM and DD-MM-YYYY HH:MM:SS
#     Cleaning        - Replace / with -
#                     - Unified format as DD-MM-YYYY HH:MM:SS
#     
# NA Values
#     columns         - Driver.id, Drop.timestamp
#     Cleaning        - Not Applicable, as they represent Cancelled or Non-availability of car
#
#     columns         - Status, Pickup.point &  Request.timestamp
#     Cleaning        - Assumption that Rows that NA values in any of these columns not required for analysis
# 
# Use new variables
# :SS value will be :00 where ever it missing
#
# Pickup Request Date Time 

# Remove duplicate rows
uber_Requests <- unique(uber_Requests)

# Remove Rows if any of columns Status, Pickup.point &  Request.timestamp have NA values 
uber_Requests <-  uber_Requests [which(!is.na(uber_Requests$Pickup.point)), ]
uber_Requests <-  uber_Requests [which(!is.na(uber_Requests$Status)), ]
uber_Requests <-  uber_Requests [which(!is.na(uber_Requests$Request.timestamp)), ]

# Cleaning  'Request timestap' for Unified Date Time
uber_Requests$Request_timestamp <- as.POSIXct(
                                        str_replace_all(uber_Requests$Request.timestamp, '/', '-'),
                                          format = "%d-%m-%Y %H:%M"
                                    )
# Cleaning  'Drop timestamp' for Unified Date Time
uber_Requests$Dropoff_timestap  <- as.POSIXct(
                                        str_replace_all(uber_Requests$Drop.timestamp, '/', '-'), 
                                        format = "%d-%m-%Y %H:%M"
                                    )

# Derived Metrics
# ---------------
# Analyze date time level requests data at aggregated level of 1 hour window. 
# i.e. Request made at 1 AM and 1.45AM are analyzed at 1 AM window period

uber_Requests$Requesthour <- as.numeric( 
                                format( uber_Requests$Request_timestamp, 
                                        "%H"
                                      )
                              )

# This variable helps to aggregate requests both cancelled & non-availabilty cars as 'Trip Not Completed'
uber_Requests$Status2 <- ifelse(
                          uber_Requests$Status == 'Trip Completed', 
                          'Completed', 
                          'Not Completed'
                      )


# For Time Slots 
# Dividing Time slots of the day as follows
# Evening       - 4PM - 8PM
# Late Evening  - 8PM - MidNight
# Late Night    - MidNight - 3AM
# Early Morning - 4AM - 6AM
# Morning       - 6AM - Noon
# After Noon    - Noon - 4PM

uber_Requests$TimeSlot <- ifelse(uber_Requests$Requesthour >= 20, 
                        'Late Evening', 
                        ifelse(uber_Requests$Requesthour >= 16, 
                               "Evening",
                               ifelse(uber_Requests$Requesthour > 12, 
                                      "After Noon", 
                                      ifelse(uber_Requests$Requesthour >= 6, 
                                             "Morning",
                                             ifelse(uber_Requests$Requesthour > 4, 
                                                    "Early Morning", 
                                                    "Late Night"
                                                   )
                                             )
                                      )
                               )
                        )

# Ordered Factor variable
uber_Requests$TimeSlot <- factor( uber_Requests$TimeSlot, 
                        ordered = TRUE, 
                        levels = c( "Late Night", 
                                    "Early Morning", 
                                    "Morning", 
                                    "After Noon", 
                                    "Evening", 
                                    "Late Evening"
                                    )
                        )

# For calculating counts and diplaying plots
frequency_counts <- geom_text(
                                stat='count',
                                  aes(label=..count..),
                                    vjust=-0.5
                              )

# For calculating % counts and diplaying plots
frequency_counts_percentages <- geom_text (aes(y = (
                                                      (..count..)/sum(..count..
                                                   )
                                              ), 
                                            label = scales::percent((..count..)/sum(..count..))),
                                                stat = "count",position = position_stack(vjust=-10) , vjust = -10)

#ggplot(uber, aes(x = uber_Requests$Request_time)) + geom_bar ()

# Part 1
# Q1 - Create plot to visualise the frequency of requests that get cancelled or show 'no cars available'

      # Frequency Analysis on Trip Status
      ggplot(uber_Requests, aes(uber_Requests$Status, fill = uber_Requests$Status)) + 
              geom_histogram(stat = "count", bins = length(unique(uber_Requests$Status))) + 
                    labs(x="Trip Status", y="Number of Requests")	 + guides(fill=guide_legend("Trip Status")) + 
                    frequency_counts +
                    ggtitle("Frequency of Requests - Trip Status Wise", subtitle = "Only 42% of Requests are Completed")

      # Frequency Analysis on Pickup Point 
      ggplot(uber_Requests, aes(uber_Requests$Pickup.point, fill = uber_Requests$Pickup.point)) + 
              geom_histogram(
                               stat = "count", 
                              bins = length(unique(uber_Requests$Pickup.point))) + 
                              labs(x="Pickup Point", y="Number of Requests")	 + 
                              guides(fill=guide_legend ("Pickup Point")) + 
              frequency_counts +
              ggtitle("Frequency of Requests - Pickup Point Wise", subtitle = "City Pickup Requests are 4% higher than Airport Pickup")

      # Frequency Analysis on Request Hour
      ggplot(uber_Requests, aes(uber_Requests$Requesthour, fill = "")) + 
          geom_histogram(stat = "count", bins = length(unique(uber_Requests$Requesthour))) + 
              labs(x="Request Hour", y="Number of Requests")	 + guides(fill=guide_legend("Requests")) +     
                  frequency_counts +
                            ggtitle("Frequency of Requests - Hour Wise")

      # Frequency Analysis on Time Slots
      ggplot(uber_Requests, aes(uber_Requests$TimeSlot, fill = uber_Requests$TimeSlot)) + 
            geom_histogram(stat = "count", bins = length(unique(uber_Requests$TimeSlot))) + 
                labs(x="Time Slots", y="Number of Requests")	 + guides(fill=guide_legend("Time Slots")) +     
                    frequency_counts +
                            ggtitle("Frequency of Requests - Time Slots Wise", subtitle = "Requests - Highest in Morning Slot")

# Add + geom_text(stat = 'count', aes(label = ..count..))


# Q2 - Identify the most problematic types of requests (city to airport / airport to city etc.) 

      ggplot(uber_Requests,aes(uber_Requests$Pickup.point)) + 
        geom_histogram(stat="count",aes(fill=uber_Requests$Status)) + 
            labs(x="Pickup Point", y="Number of Requests")	 + guides(fill=guide_legend("Trip Status")) + 
                    ggtitle("Frequency of Requests - Pickup Points Vs Trip Status", subtitle = "\nCity Pickup - Cancellations are very High\n\nAirport Pickup - Non-availability of cars is Extremely High\n")
        
# Q3 - The time slots (early mornings, late evenings etc.)         

      # Hourly plot
      #ggplot(uber,aes(uber_Requests$Requesthour)) + geom_histogram (stat="count",aes(fill=uber_Requests$Status)) + 
            #ggtitle("Requests - Time Slots", subtitle = "")
      
      ggplot(uber_Requests,aes(uber_Requests$TimeSlot)) + geom_histogram (stat="count",aes(fill=uber_Requests$Status)) + 
            labs(x="Time Slots", y="Number of Requests")	 + guides(fill=guide_legend("Trip Status")) + 
                    ggtitle("Frequency of Requests - Time Slots Vs Trip Status", subtitle = "\nCancellations - Very High in Morning\n\nNon-availability of cars - Very High in Evening and Late Evening\n")      
      

# Part -2 - Supply-Demand Gap Analysis
# Find out the gap between supply and demand and show the same using plots.      
# Q4 : Find the time slots when the highest gap exists
      
      #Supply-Demand Gap Analysis Plot
      ggplot(uber_Requests,aes(uber_Requests$Requesthour)) + 
        geom_line (stat="count", size = 1.0) + geom_text(aes(x = 5, y = 470, label = "Demand")) +
        geom_line (stat="count", aes(color=uber_Requests$Status2, width = 3.0), size = 1.0) + 
                    geom_text(aes(x = 5, y = 280, label = "Gap")) + 
                        geom_text(aes(x = 5, y = 200, label = "Supply")) +
        
        labs(x="Request Hour", y="Number of Requests")	 + guides(color = guide_legend("Trip Status")) + 
        ggtitle("Requests - Supply-Demand Gap Analysis", 
        subtitle = "Gap increases in proprotionate with Demand. Supply is significantly Low.\n\nDemand = Total Requests\nSupply = Total Trips Completed\nGap    = Total Requests Cancelled & Cars Not Available\n\n")
        
      
      
# Q5 : Find the types of requests (city-airport or airport-city) for which the gap is the most severe in the identified time slots
      
      # Analyze only Requests Not Completed
      uber_Requests_Notcompleted <- filter(uber_Requests, Status2 == 'Not Completed')
      
      # Analyze Requests Completed - Pickup Point Wise
      ggplot(uber_Requests_Notcompleted,aes(uber_Requests_Notcompleted$TimeSlot)) + 
        geom_bar(stat="count",aes(fill=uber_Requests_Notcompleted$Pickup.point)) + 
        labs(x="Time Slots", y="Number of Requests Not Completed")	 + guides(fill=guide_legend("Pickup Point")) + 
        ggtitle("Requests Not Completed  - Time Slots Vs Pickup Points", 
                    subtitle = "\nCity Pikcup - Morning Time Slot - Very High\nAirport Pickup - Evening & Late Evening Times Slots - Very High\n")
      
# Part 3
# Address specific problenms of 'Cancellations' & 'Non-Availability of Cars"      
# Q6: What do you think is the reason for this issue for the supply-demand gap?
      
      # Analyze Requests - Cancelled
      uber_Requests_cancelled <- filter(uber_Requests_Notcompleted, Status == 'Cancelled')
      
      ggplot(uber_Requests_cancelled,aes(uber_Requests_cancelled$TimeSlot)) + 
          geom_bar(stat="count",aes(fill=uber_Requests_cancelled$Pickup.point)) + 
            labs(x="Request Hour", y="Number of Requests - Cancelled")	 + guides(fill=guide_legend("Pickup Point")) + 
                ggtitle("Requests Cancelled - Hours Vs Pickup Points", subtitle = "City Pickup - Extreamly High Cancellations in Morning Slot\n")
      
      
      # Analyze Requests - Cars Not Available
      uber_Requests_CarsNotAvailable <- filter(uber_Requests_Notcompleted, Status == 'No Cars Available')
      
      ggplot(uber_Requests_CarsNotAvailable,aes(uber_Requests_CarsNotAvailable$TimeSlot)) + 
          geom_bar(stat="count",aes(fill=uber_Requests_CarsNotAvailable$Pickup.point)) + 
            labs(x="Request Hour", y="Number of Requests - No Cars Available")	 + guides(fill=guide_legend("Pickup Point")) + 
                ggtitle("No Cars Available - Hours Vs Pickup Points", subtitle = "Airport Pickup - Extreamly High Demand not met for Evening and Late Evening Slots")
      
# What do you think is the reason for this issue for the supply-demand gap? Write the answer in less than 100 words.      
# Hypothesis - Root Causes for Business Loss and Supply-Demand-Gap
      
# 58% of Overall of demand (3914/6745) not met due to less Supply.
# 79% of Gap (i.e.3102/3914) occurs in time slots - Morning, Evening & Late Evening       
# Following are 3 pressing problems needs immediate rectification
#      
# 1. Non-Availability of Cars for Airport Pikcup in Evening & Late Evening Time slots
# 2. Cancellations of City Pickup in Morning Time Slot (6AM - 10 AM)
#     57% (720/1264) of City Pickup Cancellations from Morning time slots
#     92% (702/763) of Morning Slot Cancellations are for City Pickups
# 3. Non-Availability of Cars - City Pickup - Morning Time Slot (6AM - 10 AM)
#     Z% of City Pickup are also not met due to non-availability of cars      
      
# Possible Reasons
# 1. City Pickup Cancellations (6AM - 10AM)
#    - Extremely Low Demand during After Noon hours (Noon - 4PM) for Airport Pickup to get a return City Pickup
# 2. Airport Picup Non-Availability of Cars
#    - Extremely High Demand in Evening and Late Evening (6PM - 10PM) which twice to that City Pickup Cancellations problem
#    - As cars do not reach Aiport due to City Pickup Cancellations (6AM - 10AM), this also contribute this problem
#      
# Recommendations
#       
# 1. City Pickup Cancellations (6AM - 10AM)
#     - Add premium charge for customers who book cab at this hour, which helps to compensate drivers After noon Hours (Noon - 4PM)
#
# 2. Airport Picup Non-Availability of Cars
#     - Deploy more cabs at Evening and Late Evenings hours 
#     - Add premium charge for customers who book cab at Evening and Late Evening hours
#      
# 3. Non-Availability of Cars - City Pickup - Morning Time Slot (6AM - 10 AM)
      
      
      
# --- END