# This clear all the objects in your Global environment
rm(list = ls())

# clears the console area
cat("\014")

# Sets R random number to start at the same sequence
set.seed(1234)

# Write your Code Here

# Pre processing

DATASET_FILENAME  <- "natality_50k.csv"          # Name of input dataset file
DATASET_FILENAME_B <- "natality_10k.csv"
OUTPUT_FIELD      <- "weight_pounds"          # Field name of the output class to predict


natal<-read.csv(DATASET_FILENAME_B)
natalB<-read.csv(DATASET_FILENAME)

#Preprocessing the fields
natalB$X <-NULL
natalB$child_race <-NULL
natalB$mother_race<-NULL
natalB$born_alive_alive <-NULL #Post Birth fields to be removed
natalB$born_alive_dead <-NULL  #Post Birth fields to be removed
natalB$born_dead <-NULL #Post Birth fields to be removed
natalB$father_race <-NULL
natalB$record_weight <-NULL
natalB$source_year <- NULL



#natalB$cigarette_use<-NULL
#natalB$cigarettes_per_day <-NULL
#natalB$alcohol_use <-NULL
#natalB$drinks_per_week <-NULL


names(natalB)


#Converting the true falso to 1 and 0
natalB[,c("cigarette_use")] <- as.integer(as.logical(natalB$cigarette_use))
natalB[,c("alcohol_use")] <- as.integer(as.logical(natalB$alcohol_use))
natalB[,c("is_male")] <- as.integer(as.logical(natalB$is_male))

natalB<-natalB[complete.cases(natalB), ]

#pre-processing the data averaging the fields for the missing data

#Replacing na with the average mean of the column
natalB$drinks_per_week = ifelse(is.na(natalB$drinks_per_week), round(ave(natalB$drinks_per_week, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$drinks_per_week)
natalB$cigarette_use = ifelse(is.na(natalB$cigarette_use), round(ave(natalB$cigarette_use, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$cigarette_use)
natalB$alcohol_use = ifelse(is.na(natalB$alcohol_use), round(ave(natalB$alcohol_use, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$alcohol_use)
natalB$cigarettes_per_day[is.na(natalB$cigarettes_per_day)] = 0
natalB$ever_born = ifelse(is.na(natalB$ever_born), round(ave(natalB$ever_born, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$ever_born)
natalB$weight_gain_pounds = ifelse(is.na(natalB$weight_gain_pounds), round(ave(natalB$weight_gain_pounds, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$weight_gain_pounds)
natalB$gestation_weeks = ifelse(is.na(natalB$gestation_weeks), round(ave(natalB$gestation_weeks, FUN = function(x) mean(x, na.rm = TRUE ))), natalB$gestation_weeks)

natalB<-natalB[complete.cases(natalB), ]






#Correlation matrix
numeric_natal<-natalB
numeric_natal.cor = cor(numeric_natal)

write.csv(natalB,"Natality_Preprocessed")


#Now Go to LinearReg.R to do the Linear Regression Save this file as it is used in that class
