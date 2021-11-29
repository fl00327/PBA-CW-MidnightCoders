#Pre-Processing
#Midnight Coders (J) 2021



library(dplyr)



clearup_dataset <- function(csvName){
  
  clean_year<-read.csv(csvName)
  
  #remove the fields that are added post-birth, excluding weight_pounds
  
  clean_year$apgar_1min <- NULL
  clean_year$apgar_5min <- NULL
  
  #Remove fields which can be deduced to have no relevance on the weight of the child
  clean_year$mother_residence_state <-NULL
  clean_year$state <- NULL
  clean_year$day <- NULL
  clean_year$wday <- NULL
  clean_year$month <- NULL
  clean_year$mother_married <- NULL
  clean_year$mother_birth_state <- NULL
  
  #Handling the time since last menstrual cycle is difficult and unweildy.
  clean_year$lmp <- NULL
  
  #remove duplicate fields
  clean_year$year <- NULL
  
  #Now filter the data
  #clean_year<- filter(clean_year, weight_pounds < 99, mother_age <99, father_age < 99, !is.na(cigarettes_per_day), cigarettes_per_day != 99)
  clean_year<- filter(clean_year,
                      weight_pounds < 99,
                      mother_age <99,
                      father_age < 99,
                      weight_pounds > 0,
                      mother_age > 0,
                      plurality > 0,
                      weight_pounds < 99,
                      mother_age < 99,
                      plurality < 99,
                      !is.na(is_male),
                      !is.na(plurality),
                      !is.na(mother_age),
                      !is.na(gestation_weeks),
                      !is.na(weight_pounds))
  #clean_year<- filter(clean_year, father_age < 99)
  
  
  clean_year <- clean_year[sample(nrow(clean_year), 1000000),]
  glimpse(clean_year)
  
  return(clean_year)
}



# clean_year_2004<- clearup_dataset("2004.csv")
# clean_year_2005<- clearup_dataset("2005.csv")
# clean_year_2006<- clearup_dataset("2006.csv")
# clean_year_2007<- clearup_dataset("2007.csv")
# clean_year_2008<- clearup_dataset("2008.csv")



clean_data = rbind(clean_year_2004,clean_year_2005,clean_year_2006,clean_year_2007,clean_year_2008)



#Write entire dataframe to a file [LARGE]
write.csv(clean_data, "C:/Users/Bradley/alldata_5m.csv")



#Take sample of dataset at given intervals, essentially downsampling large dataset to produce more managable sizes.
write.csv(clean_data[sample(nrow(clean_data), 100000),], "C:/Users/Bradley/alldata_100k.csv")
write.csv(clean_data[sample(nrow(clean_data), 500000),], "C:/Users/Bradley/alldata_500k.csv")
write.csv(clean_data[sample(nrow(clean_data), 50000),], "C:/Users/Bradley/alldata_50k.csv")
write.csv(clean_data[sample(nrow(clean_data), 10000),], "C:/Users/Bradley/alldata_10k.csv")
write.csv(clean_data[sample(nrow(clean_data), 1000),], "C:/Users/Bradley/alldata_1k.csv")
write.csv(clean_data[sample(nrow(clean_data), 1000000),], "C:/Users/Bradley/alldata_1m.csv")