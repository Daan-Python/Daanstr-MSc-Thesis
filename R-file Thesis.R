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


#### ELASTIC NET REGRESSION ####

# Load required packages
library(caret)
library(glmnet)
library(tidyverse)
library(glmnetUtils)
library(data.table)
library(MLmetrics)

# Generate training and test dataset

# Create temporary dataset including all control variables without missing observations
Listing.temp <- na.omit(Listing[,c(4,7:15,17:23,26,28:30,32)])

# Generate training and test dataset based
set.seed(53)
sample.size <- floor(0.8 * nrow(Listing.temp))
train.listing <- sample(seq_len(nrow(Listing.temp)), size = sample.size)
train <- Listing.temp[train.listing,]
test <- Listing.temp[-train.listing,]

# Scale training data
y_train <- as.vector(train$occupancy_rate)
x_train <- model.matrix(train$occupancy_rate~ ., data = train, 
                        contrasts.arg = lapply(train[,c(2,4,5,10)], contrasts, contrasts = FALSE))
x_train <- scale(x_train, scale = TRUE, center = TRUE)

# Remove intercept
x_train <- x_train[,-1] 

# Scale test data
y_test <- as.vector(test$occupancy_rate)
x_test <- model.matrix(test$occupancy_rate~ ., data = test, 
                       contrasts.arg = lapply(test[,c(2,4,5,10)], contrasts, contrasts = FALSE))
x_test <- scale(x_test, scale = TRUE, center = TRUE) 

# Remove intercept
x_test <- x_test[,-1] 

# Cross validate the best Lambda, based on subset of different Alpha's
set.seed(100)
cv.elastic <- cva.glmnet(x_train, y_train, nfolds = 10,
                         alpha = seq(0, 1, by = 0.01))

# Get the best model parameters through following function
get_model_params <- function(cv.elastic) {
  alpha <- cv.elastic$alpha
  lambdaMin <- sapply(cv.elastic$modlist, `[[`, "lambda.min")
  lambda.1se <- sapply(cv.elastic$modlist, `[[`, "lambda.1se")
  MSE <- sapply(cv.elastic$modlist, function(mod) {min(mod$cvm)})
  best <- which.min(MSE)
  data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
             lambda.1se = lambda.1se[best], MSE = MSE[best], RMSE = sqrt(MSE[best]))
}

best.params <- get_model_params(cv.elastic) 
best.params       

# Create plot with cross validated MSE for each alpha
minlossplot(cv.elastic, cv.type = "min", xlab = "Alpha", cex.lab = 1.2, cex.axis = 1.2)

# Create plot with best cross validated lambda, given the best alpha
plot(cv.elastic$modlist[[71]])

# Estimate the model with the conservative estimate of lambda
cv.elastic.best <- glmnet(x_train, y_train, alpha = best.params$alpha, 
                          family = "gaussian",lambda = best.params$lambda.1se)

# Obtain output table of the elastic net regression
coef(cv.elastic.best)
round(cv.elastic.best$beta, digits = 4)

# Assess how model performs on test data
pred <- predict(cv.elastic.best, x_test)
RMSE(pred, y_test)

# Add significant factor variables as dummies to final dataset
Listing$nb_Bos_en_Lommer <- ifelse(Listing$neighborhood == "Bos en Lommer", TRUE, FALSE)
Listing$nb_Centrum_Oost <- ifelse(Listing$neighborhood == "Centrum-Oost", TRUE, FALSE)
Listing$nb_Centrum_West <- ifelse(Listing$neighborhood == "Centrum-West", TRUE, FALSE)
Listing$nb_Gaasperdam_Driemond <- ifelse(Listing$neighborhood == "Gaasperdam - Driemond", TRUE, FALSE)
Listing$nb_Ijburg_Zeeburgereiland <- ifelse(Listing$neighborhood == "IJburg - Zeeburgereiland", TRUE, FALSE)
Listing$nb_Oostelijk_Havengebied_Indische_Buurt <- ifelse(Listing$neighborhood == "Oostelijk Havengebied - Indische Buurt", TRUE, FALSE)
Listing$pt_Other <- ifelse(Listing$property_type == "Other", TRUE, FALSE)
Listing$rt_Entire_Home_Apt <- ifelse(Listing$room_type == "Entire home/apt", TRUE, FALSE)
Listing$rt_Private_Room <- ifelse(Listing$room_type == "Private room", TRUE, FALSE)

# Drop all control variables that are insignificant according 
Listing <- Listing[,-c(7,9:13,15,20:23,28,29,32)]


#### SENTIMENT ANALYSIS ####

# Load required packages
library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(syuzhet)
library(SnowballC)
library(qdap)
library(cld2)

# Pre-process text of description and host about
options(stringsAsFactors = FALSE)
Listing$description <- gsub(" NA", " ", Listing$description, ignore.case = TRUE)            # Remove NA due to merging
Listing$description <- gsub("sink", "drainage", Listing$description, ignore.case = TRUE)    # Sink is likely a noun instead of negative adjective
Listing$description <- gsub("bunk", "bed", Listing$description, ignore.case = TRUE)         # Bunk is likely a noun instead of negative adjective
Listing$description <- tolower(Listing$description)                                         # Transform all text to lower case
Listing$description <- gsub("[^0-9a-z,.:;' ]", "", Listing$description, ignore.case = TRUE) # Keep a-z, 0-9, ",", ".", ":", ";" and "'", remove any other characters
Listing$description <- gsub(":", ",", Listing$description, ignore.case = TRUE)              # Replace : by ,
Listing$description <- gsub(";", ",", Listing$description, ignore.case = TRUE)              # Replace ; by ,
Listing$description <- gsub("\\.", ",", Listing$description, ignore.case = TRUE)            # Replace . by ,
Listing$description <- gsub("\\,+","\\, ", Listing$description, ignore.case = TRUE)         # Remove excess ,
Listing$description <- gsub(" +", " ", Listing$description, ignore.case = TRUE)             # Remove excess spaces

Listing$host_about <- gsub(" NA", " ", Listing$host_about, ignore.case = TRUE)              # Remove NA due to merging
Listing$host_about <- tolower(Listing$host_about)                                           # Transform all text to lower case
Listing$host_about <- gsub("[^0-9a-z,.:;' ]", "", Listing$host_about, ignore.case = TRUE)   # Keep a-z, 0-9, ",", ".", ":", ";" and "'", remove any other characters
Listing$host_about <- gsub(":", ",", Listing$host_about, ignore.case = TRUE)                # Replace : by ,
Listing$host_about <- gsub(";", ",", Listing$host_about, ignore.case = TRUE)                # Replace ; by ,
Listing$host_about <- gsub("\\.", ",", Listing$host_about, ignore.case = TRUE)              # Replace . by ,
Listing$host_about <- gsub("\\,+","\\, ", Listing$host_about, ignore.case = TRUE)           # Remove excess ,
Listing$host_about <- gsub(" +", " ", Listing$host_about, ignore.case = TRUE)               # Remove excess spaces

# Detect languages and transform languages other than English to NA
Listing$host_about <- ifelse(detect_language(Listing$host_about) == "en", Listing$host_about, NA)
Listing$description <- ifelse(detect_language(Listing$description) == "en", Listing$description, NA)

# Calculate and add the polarity score of each individual listing for description and host about
Listing$d <- polarity(Listing$description)
Listing$pol_descr <- Listing$d$all$polarity

Listing$h <- polarity(Listing$host_about)
Listing$pol_host <- Listing$h$all$polarity

# Plot polarity scores for description and host about
ggplot() +
  geom_density(data = Listing, aes(x = pol_descr, fill = "g"), alpha = 0.3) +
  geom_density(data = Listing, aes(x = pol_host, fill = "r"), alpha = 0.3) +
  scale_fill_manual(name = "Text", values = c("g" = "green", "r" = "red"), labels = c("g" = "Description", "r" = "Host About")) +
  labs(x = "Polarity Score", y = "Density") +
  theme(text = element_text(size = 11))

# Transform "NaN" to NA through a function
make.NA <- function(x) if(is.numeric(x)){
  is.na(x) <- x=="NaN"; x} else {x}
Listing[] <- lapply(Listing, make.NA)

# Calculate number of identified positive and negative words used in each textual description and host about
Listing$pos_words_description <- Listing$d$all$pos.words
Listing$pos_words_description <- as.character(Listing$pos_words_description)
Listing$pos_words_description <- ifelse(nchar(Listing$pos_words_description)>2, str_count(Listing$pos_words_description, ",")+1, 0)
Listing$neg_words_description <- Listing$d$all$neg.words
Listing$neg_words_description <- as.character(Listing$neg_words_description)
Listing$neg_words_description <- ifelse(nchar(Listing$neg_words_description)>2, str_count(Listing$neg_words_description, ",")+1, 0)

Listing$pos_words_host <- Listing$h$all$pos.words
Listing$pos_words_host <- as.character(Listing$pos_words_host)
Listing$pos_words_host <- ifelse(nchar(Listing$pos_words_host)>2, str_count(Listing$pos_words_host, ",")+1, 0)
Listing$neg_words_host <- Listing$h$all$neg.words
Listing$neg_words_host <- as.character(Listing$neg_words_host)
Listing$neg_words_host <- ifelse(nchar(Listing$neg_words_host)>2, str_count(Listing$neg_words_host, ",")+1, 0)

# Standardize polarity scores
Listing[,c(29,31)] <- scale(Listing[,c(29,31)], scale = TRUE, center = TRUE)

# Label polarity scores persuasive or informative based the scaled polarity values
Listing$description_intention <- ifelse(Listing$pol_descr > 0, "Persuasive", "Informative")
Listing$description_intention <- as.factor(Listing$description_intention)
Listing$host_about_intention <- ifelse(Listing$pol_host > 0, "Persuasive", "Informative")
Listing$host_about_intention <- as.factor(Listing$host_about_intention)

# Transform missing values to NA instead of zero words
Listing$pos_words_description <- ifelse(is.na(Listing$description_intention), NA, Listing$pos_words_description)
Listing$neg_words_description <- ifelse(is.na(Listing$description_intention), NA, Listing$neg_words_description)
Listing$pos_words_host <- ifelse(is.na(Listing$host_about_intention), NA, Listing$pos_words_host)
Listing$neg_words_host <- ifelse(is.na(Listing$host_about_intention), NA, Listing$neg_words_host)

# Descriptive statistics
aggregate(Listing$pos_words_description ~ Listing$description_intention, FUN = mean, data = Listing)
aggregate(Listing$neg_words_description ~ Listing$description_intention, FUN = mean, data = Listing)
aggregate(Listing$pos_words_host ~ Listing$host_about_intention, FUN = mean, data = Listing)
aggregate(Listing$neg_words_host ~ Listing$host_about_intention, FUN = mean, data = Listing)

# Remove variables that are no longer needed
Listing <- Listing[,c(3:27,36,37)]


#### TOBIT REGRESSION ####

# Load required packages
library(AER)
library(car)

# Transform missing values in description and host about to 'No Text' category
Listing$host_about_intention <- addNA(Listing$host_about_intention)
Listing$host_about_intention <- ifelse(Listing$host_about_intention == "NA", "No Text", Listing$host_about_intention)
Listing$host_about_intention <- mapvalues(Listing$host_about_intention, from = c("1", "2", "3"), to = c("Informative", "Persuasive", "No Text"))
Listing$host_about_intention <- as.factor(Listing$host_about_intention)

Listing$description_intention <- addNA(Listing$description_intention)
Listing$description_intention <- ifelse(Listing$description_intention == "NA", "No Text", Listing$description_intention)
Listing$description_intention <- mapvalues(Listing$description_intention, from = c("1", "2", "3"), to = c("Informative", "Persuasive", "No Text"))
Listing$description_intention <- as.factor(Listing$description_intention)

# Change the order of the columns for the sake of output tables
Listing <- Listing[,c(3,11,1,4,26,27,7,16,14,12,2,5,6,8,9,10,13,17:25,15)]

# Estimate final model through tobit regression
Tobit <- tobit(Listing$occupancy_rate ~ ., left = -Inf, right = 1, data = Listing)
summary(Tobit)

# Create a function to calculate Mcfadden's pseudo R2
PseudoR2 <- function(obj){1 - as.vector(logLik(obj)/logLik(update(obj, . ~ 1)))}
PseudoR2(Tobit)

# Check for multicollinearity
vif(Tobit) # Note that there were no issues

