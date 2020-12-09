# Author: Daan Straatman
# Date: December 11, 2020

# Install and activate package required to read file
library(readr)

# Load in the dataset from Inside Airbnb, scraped on December 4th 2017
Listing.full <- read_csv("Data Science & Marketing Analytics/Data Science & Marketing Analytics/Master Thesis/Data/listings.04-12-2017.gz")

# Load in the dataset from Inside Airbnb, scraped on December 6th 2018
Reviews <-read.csv("~/Data Science & Marketing Analytics/Data Science & Marketing Analytics/Master Thesis/Data/reviews.06-12-2018.csv")

#### MERGE DATASETS AND TRANSFORM DATA ####

# Install required packagage and convert to dates
library(plyr)
Reviews$date <- as.Date(Reviews$date, format = "%Y-%m-%d")

# Keep rows with reviews from 2017-12-04 to 2018-12-04 (365 days ahead)
Reviews <- Reviews[(Reviews$date >= as.Date("2017-12-04") & Reviews$date <= as.Date("2018-12-04")),]

# Aggregate the number of reviews for each unique listing ID
Reviews <- aggregate(data=Reviews, Reviews$date ~ Reviews$listing_id, function(x) length(unique(x)))

# Rename the columns to match between datasets
colnames(Reviews) <- c("id", "Calculated_reviews")

# Merge datasets, adding the number of calculated reviews as extra column
Listing.full <- merge(Listing.full, Reviews, by = "id", all = T)

# Remove listings that are added after the 4th of December 2017
removeNA <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Remove rows without information, based on missing values of Scrape ID
Listing.full <- removeNA(Listing.full, "scrape_id")

# Assign a value of zero to listings without any review
Listing.full$Calculated_reviews <- ifelse(is.na(Listing.full$Calculated_reviews), 0, Listing.full$Calculated_reviews)

# Reload the original review dataset
Reviews <-read.csv("~/Data Science & Marketing Analytics/Data Science & Marketing Analytics/Master Thesis/Data/reviews.06-12-2018.csv")

# Identify active listings (at least one review one year prior or after 2017-12-04)
Reviews <- Reviews[(Reviews$date >= as.Date("2016-12-04") & Reviews$date <= as.Date("2018-12-04")),]

# Aggregate the data 
Reviews <- aggregate(data=Reviews, Reviews$date ~ Reviews$listing_id, function(x) length(unique(x)))

# Rename the first column to merge and add active reviews to the dataset
colnames(Reviews) <- c("id", "active_reviews")

# Merge data and again remove rows without any information
Listing.full <- merge(Listing.full, Reviews, by = "id", all = T)
Listing.full <- removeNA(Listing.full, "scrape_id")

# Transform observations without any review in two years from NA to zero
Listing.full$active_reviews <- ifelse(is.na(Listing.full$active_reviews), 0, Listing.full$active_reviews)

# Only keep observations with a review in two years in the dataset
Listing.full <- Listing.full[Listing.full$active_reviews > 0,]

# Identify highly available listings, defined as 60 days available or more, 
# in order to use comparable listings
Listing.full <- Listing.full[Listing.full$availability_365 >= 60,] 

# Apply the review rate of 50% include minimum nights and calculate the occupancy rate
Listing.full$Calculated_reviews <- Listing.full$Calculated_reviews/0.5

# Multiply calculated reviews with minimum nights of 3.4 or higher
# Calculate occupancy rate and weigh by one year
Listing.full$occupancy_rate <- ifelse(Listing.full$minimum_nights >= 3.4, 
                                      (Listing.full$minimum_nights * Listing.full$Calculated_reviews)/365,
                                      (3.4 * Listing.full$Calculated_reviews)/365)

# Cap the maximum occupancy rate at 1 by censoring the dependent variable
Listing.full$occupancy_rate <- ifelse(Listing.full$occupancy_rate > 1, 1, Listing.full$occupancy_rate)

# Obtain the description by merging name, summary and space into one text
Listing.full$description <- paste(Listing.full$name, Listing.full$summary,Listing.full$space)

# Identify the five possible safety features by making a binary variable
library(stringr)
Listing.full$smoke_detector <- ifelse(str_detect(Listing.full$amenities, "Smoke detector"), 1, 0)
Listing.full$first_aid_kit <- ifelse(str_detect(Listing.full$amenities, "First aid kit"), 1, 0)
Listing.full$carbon_monoxide_detector <- ifelse(str_detect(Listing.full$amenities, "Carbon monoxide detector"), 1, 0)
Listing.full$Fire_extinguisher <- ifelse(str_detect(Listing.full$amenities, "Fire extinguisher"), 1, 0)
Listing.full$Lock_on_bedroom_door <- ifelse(str_detect(Listing.full$amenities, "Lock on bedroom door"), 1, 0)

# Obtain the number of safety features by calculating the sum
Listing.full$safety_features <- Listing.full$smoke_detector + Listing.full$first_aid_kit + 
  Listing.full$carbon_monoxide_detector + Listing.full$Fire_extinguisher + Listing.full$Lock_on_bedroom_door

# Obtain the number of amenities based on minimum word lenght
Listing.full$amenities <- ifelse(nchar(Listing.full$amenities)>2, str_count(Listing.full$amenities, ",")+1, 0)

# Remove uninformative columns 
Listing <- Listing.full[,-c(1:7,9:20,21:24,28,30,31:36,38,39,41:50,62,63,70:76,78,79,81:89,91,95:98,100:104)]

# Write function to transform 'N/A' to real NA values
make.true.NA <- function(x) if(is.character(x)){
  is.na(x) <- x=="N/A"; x} else {x}
Listing[] <- lapply(Listing, make.true.NA)

# Transform columns to correct data type 
Listing$host_response_time <- as.factor(Listing$host_response_time)
Listing$neighbourhood_cleansed <- as.factor(Listing$neighbourhood_cleansed)
Listing$property_type <- as.factor(Listing$property_type)
Listing$room_type <- as.factor(Listing$room_type)
Listing$bed_type <- as.factor(Listing$bed_type)
Listing$cancellation_policy <- as.factor(Listing$cancellation_policy)

# Transform host response rate from % to a rate between zero and one
Listing$host_response_rate <- as.numeric(sub("%", "", Listing$host_response_rate))/100

# Remove dots, commas and $ for prices
Listing$price <- as.numeric(gsub("\\.", "", gsub("\\,", "", gsub("\\$", "", Listing$price))))/100
Listing$security_deposit <- as.numeric(gsub("\\.", "", gsub("\\,", "", gsub("\\$", "", Listing$security_deposit))))/100
Listing$cleaning_fee <- as.numeric(gsub("\\.", "", gsub("\\,", "", gsub("\\$", "", Listing$cleaning_fee))))/100
Listing$extra_people <- as.numeric(gsub("\\.", "", gsub("\\,", "", gsub("\\$", "", Listing$extra_people))))/100

# Transform missing values for security deposit and cleaning fee to zero
Listing$security_deposit <- ifelse(is.na(Listing$security_deposit), 0, Listing$security_deposit)
Listing$cleaning_fee <- ifelse(is.na(Listing$cleaning_fee), 0, Listing$cleaning_fee)

# Transform super strict policy to strict
Listing$cancellation_policy <- mapvalues(Listing$cancellation_policy, from = c("super_strict_60"), to = c("strict"))

# Transform categories within property type and bed type that are underrepresented by less 
# than 1 percent (36 observations) to 'Other'
Listing$property_type <- mapvalues(Listing$property_type, from = c("Boutique hotel", "Bungalow",
                                                                   "Cabin", "Chalet", "Condominium", "Earth House", "Guest suite", "Guesthouse",
                                                                   "Hostel", "Serviced apartment", "Vacation home", "Villa"), 
                                   to = c("Other", "Other", "Other","Other", "Other", "Other","Other", 
                                          "Other", "Other","Other", "Other", "Other"))

Listing$bed_type <- mapvalues(Listing$bed_type, from = c("Airbed", "Couch", "Futon"), to = c("Other", "Other", "Other"))

# Add binary variable for having a host description of at least 3 words.
Listing$host_about <- ifelse(sapply(strsplit(Listing$host_about, " "), length) >= 3, Listing$host_about, NA)
Listing$host_has_descr <- ifelse(is.na(Listing$host_about), FALSE, TRUE)

# Check the number of missing values of each variable
for (i in 1:33){
  print(sum(is.na(Listing[,i])))
}

# Remove square feet due to 3467 missing observations
Listing <- Listing[,-17]

# Change some column names for clarity
colnames(Listing)[5] <- "superhost"
colnames(Listing)[6] <- "host_id_verified"
colnames(Listing)[7] <- "neighborhood"
colnames(Listing)[8] <- "exact_location"
colnames(Listing)[25] <- "review_score"
colnames(Listing)[28] <- "guest_picture"
colnames(Listing)[29] <- "guest_phone_verification"



#### WORD ANALYSIS ####

# Load required packages
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(SnowballC)
library(qdap)
library(cld2)
attach(Listing)

# Pre-process text for description
Listing$description <- gsub(" NA", " ", Listing$description, ignore.case = TRUE)         #Remove NA due to merging
Listing$description <- gsub("sink", "drainage", Listing$description, ignore.case = TRUE) #Sink is likely a noun instead of negative adjective
Listing$description <- gsub("bunk", "bed", Listing$description, ignore.case = TRUE)      #Bunk is likely a noun instead of negative adjective
Listing$description <- as.character(Listing$description)  %>%
  tolower() %>%
  {mgsub(emoticon[,2],emoticon[,1],.)} %>%
  {gsub("\\n", " ", .)} %>%                        # Remove \n (newline)     
  {gsub("[?!]+",".",.)} %>%                        # Remove ? and ! (replace by single .)
  {gsub("[\\[\\*\\]]*"," ",.)} %>%                 # Remove [ and ] * (replace by single space)
  {gsub("(\"| |\\$)-+\\.-+"," number ", .)} %>%    # Find numbers
  {gsub("(-+:)*-+ *am"," timeam", .)} %>%          # Find time AM
  {gsub("(-+:)*-+ *pm"," timepm", .)} %>%          # Find time PM
  {gsub("-+:-+","time", .)} %>%                    # Find general time
  {gsub("( |\\$)--+"," number ", .)} %>%           # Find remaining numbers
  {gsub("-"," ", .)} %>%                           # Remove all -
  {gsub("\"+"," ", .)} %>%                         # Remove all "
  {gsub(";+"," ", .)} %>%                          # Remove excess ;
  {gsub("\\.+","\\. ", .)} %>%                     # Remove excess .
  {gsub(" +"," ", .)} %>%                          # Remove excess spaces
  {gsub("\\. \\.","\\. ", .)}                      # Remove space between periods

# Keep a-z, 0-9, remove any other characters
Listing$description <- gsub("[^0-9A-Za-z///' ]", "", Listing$description, ignore.case = TRUE)

# Detect languages and transform languages other than English to NA
Listing$host_about <- ifelse(detect_language(Listing$host_about) == "en", Listing$host_about, NA)
Listing$description <- ifelse(detect_language(Listing$description) == "en", Listing$description, NA)

# Create sentiment scores per full text and add the polarity scores
pol <- polarity(Listing[,"description"])$all
Listing$polarity <- pol[,"polarity"]

# Scale polarity scores and label as persuasive or informative
Listing$polarity.temp <- Listing$polarity
Listing[,34] <- scale(Listing[,34], scale = TRUE, center = TRUE)
Listing$style <- ifelse(Listing$polarity.temp > 0, "Persuasive", "Informative")
Listing <- Listing[,-34]

# Split text based on "Persuasive"/"Informative" 
persuasive_descr <- Listing %>% filter(style == "Persuasive")
informative_descr <- Listing %>% filter(style == "Informative")

# Obtain word frequencies in informative texts
all_informative_words <- Listing %>% filter(style == "Informative")
all_informative_words <- all_informative_words %>%
  unnest_tokens(word,description) %>%
  anti_join(stop_words, by = "word")
all_informative_words%>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(25, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Word Frequencies: informative description")

# Obtain word frequencies in persuasive texts
all_persuasive_words <- Listing  %>% filter(style == "Persuasive")
all_persuasive_words <- (all_persuasive_words) %>% unnest_tokens(word,description) %>% anti_join(stop_words, by = "word")
all_persuasive_words%>%
  count(word, sort=TRUE) %>%
  mutate(word = reorder(word,n)) %>%
  top_n(25, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() +
  labs(x = NULL, y = "Number of occurences") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Word Frequencies: persuasive descriptions ")

# Obtain difference of frequency of words in persuasive vs informative texts
all_sentence <- full_join(all_persuasive_words %>% count(word, sort=TRUE),
                                all_informative_words %>% count(word, sort=TRUE),
                                by="word")
all_sentence[is.na(all_sentence$n.x), "n.x"] <- 0
all_sentence[is.na(all_sentence$n.y), "n.y"] <- 0

# Normalize the counts by total number of words in each group and calculate ratio
all_sentence$n.x  <- all_sentence$n.x/sum(all_sentence$n.x)
all_sentence$n.y  <- all_sentence$n.y/sum(all_sentence$n.y)
all_sentence$diff <- all_sentence$n.x-all_sentence$n.y

# Obtain graph with specific informative words
all_sentence%>%
  mutate(word= reorder(word, -diff)) %>%           
  top_n(-20, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (Persuasive-Informative)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific Informative Words")

# Obtain graph with specific persuasive words
all_sentence%>%
  mutate(word = reorder(word,diff)) %>%           
  top_n(20, diff) %>%
  ggplot(aes(word,diff)) +  
  geom_col() +
  labs(x = NULL, y = "Difference in word frequency (Persuasive-Informative)") +
  coord_flip() +
  theme(text = element_text(size = 17)) +
  ggtitle("Specific Persuasive words")