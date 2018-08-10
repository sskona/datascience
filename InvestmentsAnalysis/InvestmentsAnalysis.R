library(dplyr)
library(tidyr)
library(stringr)

#Load Data : Importing companies and rounds2 data
  raw.companies<-read.delim("companies.txt",header = TRUE,stringsAsFactors = FALSE,sep = '\t',quote = "\"",na.strings = c(""," ","NA"))
  companies <- raw.companies
  str(companies)

  raw.rounds2<-read.csv("rounds2.csv",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","NA"))
  rounds2 <- raw.rounds2
  str(rounds2)

########################################################    
#CHECKPOINT 1: Data Cleaning 1 
  
  # Questions
  
  # Making the permalink and company_permalink lower case for consistency
  companies$permalink<-tolower(companies$permalink)
  rounds2$company_permalink<-tolower(rounds2$company_permalink)
  
  # Q1. How many unique companies are present in companies?
  length(unique(companies$permalink))
  #Answer: 66368
  
  # Q2. How many unique companies are present in rounds2?
  length(unique(rounds2$company_permalink))
  #Answer: 66368
  
  # Q3. In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
  #Answer:  permalink

  # Q4. Are there any companies in the rounds2 file which are not  present in companies ? Answer Y/N.
  length(which(!companies$permalink %in% rounds2$company_permalink))
  #Answer:  N

  # Merge the two data frames so that all variables (columns) in the companies 
  # frame are added to the rounds2 data frame. Name the merged frame master_frame.
  
  # Q5. How many observations are present in master_frame ?
  
  master_frame <- merge(rounds2, companies, by.x = "company_permalink", by.y = "permalink")
  length(master_frame$company_permalink)
  #Answer: 114949


  
########################################################    
#CHECKPOINT 2: Funding Type Analysis
  
  # Questions
  
  # Q1.Average funding amount of venture type
  master_frame %>% 
                    filter(funding_round_type=="venture") %>% 
                        summarise(avg_venture=mean(raised_amount_usd,na.rm=TRUE))
  #Answer: 11748949
  
  # Q2.Average funding amount of angel type
  master_frame %>% 
                    filter(funding_round_type=="angel") %>% 
                        summarise(avg_angel=mean(raised_amount_usd,na.rm=TRUE))
  #Answer: 958694.5
  
  # Q3.Average funding amount of seed type
  master_frame %>%
                  filter(funding_round_type == "seed") %>%
                      summarise(avg_seed = mean(raised_amount_usd, na.rm = TRUE))
  #Answer: 719818
  
  # Q4.Average funding amount of private equity type
  master_frame %>%
                   filter(funding_round_type == "private_equity") %>%
                      summarise(avg_pvt_equity = mean(raised_amount_usd, na.rm = TRUE))
  #Answer: 73308593
  
  # Q5.Considering that Spark Funds wants to invest between 5 to 15 million USD per 
  #investment round, which investment type is the most suitable for them?
  
    (master_frame %>% 
            group_by(funding_round_type) %>% 
                    summarise(mean = mean(raised_amount_usd, na.rm = TRUE)) %>%
                          filter(mean >= 5000000 & mean <= 15000000))[1,1]
  
  #Answer:  Venture
  

########################################################    
#CHECKPOINT 3: Country Analysis
  
  #Filter the chosen investment type and create a dataframe on that
  #Removing records with NULL company codes as they would not add value to the analysis
  master_frame_venture <- master_frame %>% filter(funding_round_type=="venture" & !is.na(country_code))
  
  #Finding top9 countries  
  top9 <- master_frame_venture %>% 
    group_by(country_code) %>%
    summarise(sum = sum(raised_amount_usd, na.rm = TRUE)) %>%
    arrange(desc(sum)) %>%
    head(n=9)
  
  # Questions
  
  # Q1. Top English speaking country
  top9[1, 1]
  # Ans: USA
  
  # Q2. Second English speaking country
  top9[3, 1]
  # Ans: GBR
  
  # Q33. Third English speaking country
  top9[4, 1]
  # Ans: IND  
  

########################################################    
#CHECKPOINT 4: Sector Analysis 1
  
  #Importing and cleaning mapping.csv
  raw.mapping <- read.csv("mapping.csv",header = TRUE,check.names = FALSE,stringsAsFactors = FALSE)
  mapping <- raw.mapping
  
  #Converting from Wide to Long form.
  mapping <- mapping %>% 
                  gather(sector, value, 2:10) %>% 
                      filter(value == "1") %>%
                            arrange(category_list)  

  #Removing the "Blanks" row and the "Value" column which was created in the previous step.
  #And renaming the columns
  mapping <- mapping[-1,-3]
  colnames(mapping) <- c("primary_sector","main_sector")
  
  #Fixing the spelling issues
  mapping$primary_sector <- gsub("^0","Na",mapping$primary_sector)
  mapping$primary_sector <- gsub("0","na",mapping$primary_sector)
  
  #Converting back NA to 0 in Enterprise 2.na
  mapping$primary_sector <- str_replace(mapping$primary_sector, "Enterprise 2.na","Enterprise 2.0")
  str(mapping)
  
  #Extracting PRIMARY Sector from Category List in master_frame
  master_frame_venture <- separate(master_frame_venture,category_list,into=c("primary_sector","remaining_sector"),sep="\\|",extra = "merge", fill = "right")
  
  #Merging master_frame and mapping
  venture_funding_sectors <- merge(master_frame_venture, mapping, by="primary_sector")
  
  
########################################################    
#CHECKPOINT 5: Sector Analysis 2
  
  #Investments between 5 to 15 million USD for Funding Type = venture
  final.data <- venture_funding_sectors %>% 
                  filter(raised_amount_usd>=5000000 & raised_amount_usd<=15000000)
  
  #Data frame for USA, GBR, IND
  D1 <- final.data %>% filter(country_code == "USA")
  D2 <- final.data %>% filter(country_code == "GBR")
  D3 <- final.data %>% filter(country_code == "IND")
  
  #Adding "Total no of investment" (count_sector_wise) 
  #and "Total amount invested" (sum_sector_wise) to D1, D2, D3
  D1 <- D1 %>% 
    group_by(main_sector) %>% 
    mutate(count_sector_wise = n(),
           sum_sector_wise = sum(raised_amount_usd, na.rm = TRUE))
  
  D2 <- D2 %>% 
    group_by(main_sector) %>% 
    mutate(count_sector_wise = n(),
           sum_sector_wise = sum(raised_amount_usd, na.rm = TRUE))
  
  D3 <-D3 %>% 
    group_by(main_sector) %>% 
    mutate(count_sector_wise = n(),
           sum_sector_wise = sum(raised_amount_usd, na.rm = TRUE))

  # Questions  
  
  # Q1. Total number of investments (count)
  
  #USA - 12063
  nrow(D1)
  #GBR - 621  
  nrow(D2)
  #IND - 328  
  nrow(D3)
  
  # Q2. Total amount of investment (USD)
  
  #USA - 107757097294
  sum(D1$raised_amount_usd)
  #GBR - 5379078691  
  sum(D2$raised_amount_usd)
  #IND - 2949543602  
  sum(D3$raised_amount_usd)

  #Calculating top sectors for D1, D2, D3
  D1_Topsectors <- D1 %>% 
                          select(main_sector, count_sector_wise) %>% 
                            distinct() %>%
                                    arrange(desc(count_sector_wise))
  
  D2_Topsectors <- D2 %>% 
                          select(main_sector, count_sector_wise) %>% 
                            distinct() %>% 
                                    arrange(desc(count_sector_wise))
  
  D3_Topsectors <- D3 %>% 
                          select(main_sector, count_sector_wise) %>% 
                            distinct() %>% 
                                    arrange(desc(count_sector_wise))
  
    
  # Q3. Top sector (based on count of investments)
  #Answer: Others for D1, D2, D3
  D1_Topsectors [1, 1]
  D2_Topsectors [1, 1]
  D3_Topsectors [1, 1]
  
  # Q4. Second-best sector (based on count of investments)
  #Answer: "Social, Finance, Analytics, Advertising" for D1, D2, D3
  D1_Topsectors [2, 1]
  D2_Topsectors [2, 1]
  D3_Topsectors [2, 1]
  
  # Q5. Third-best sector (based on count of investments)
  
  #Answers: 
  #"Cleantech / Semiconductors" for D1, D2
  D1_Topsectors [3, 1]
  D2_Topsectors [3, 1]
  #"News, Search and Messaging" for D3
  D3_Topsectors [3, 1]
  
  # Q6. Number of investments in the top sector (refer to point 3)
  
  #USA - 2950
  D1_Topsectors [1, 2]
  #GBR - 147  
  D2_Topsectors [1, 2]
  #IND - 110
  D3_Topsectors [1, 2]
  
  # Q7. Number of investments in the second-best sector (refer to point 4)

  #USA - 2714
  D1_Topsectors [2, 2]
  #GBR - 133
  D2_Topsectors [2, 2]
  #IND - 60
  D3_Topsectors [2, 2]
  
  # Q8. Number of investments in the third-best sector (refer to point 5)
  
  #USA - 2350
  D1_Topsectors [3, 2]
  #GBR - 130   
  D2_Topsectors [3, 2]
  #IND - 52  
  D3_Topsectors [3, 2]
  
  # Q9. For the top sector count-wise (point 3), which company received the highest investment?

  #USA - Virtustream
  (D1 %>% 
      filter(main_sector == D1_Topsectors[1,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]
  
  #GBR - Electric Cloud
    (D2 %>% 
      filter(main_sector == D2_Topsectors[1,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]

  #IND - FirstCry.com
    (D3 %>% 
      filter(main_sector == D3_Topsectors[1,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]
  
  # Q10. For the second-best sector count-wise (point 4), which company received the highest investment?
  
  
  #USA - SST Inc. (Formerly ShotSpotter)
  (D1 %>% 
      filter(main_sector == D1_Topsectors[2,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]
  
  
  #GBR - Celltick Technologies
  (D2 %>% 
      filter(main_sector == D2_Topsectors[2,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]
  
  #IND - Manthan Systems
  (D3 %>% 
      filter(main_sector == D3_Topsectors[2,1]) %>% 
      group_by(main_sector, name) %>% 
      summarise(total = sum(raised_amount_usd)) %>% 
      arrange(desc(total))) [1, 2]
  