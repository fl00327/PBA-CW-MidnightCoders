# ***************************************
#      Midnight Coders COM3018 Coursework
#       Ultrasound Testing R File
#             November 2021
#
#        MARS to predict:
#             weight_pounds
#
#         Predictor Fields:
#             is_male
#            mother_age
#             pluarlity
#          gestation_weeks
#
# ***************************************

#Libraries 
library(caret)
library(ggplot2)


# ******************************************************************************
# GetModelAccuracy(): Function to return the given model's accuracy
# INPUT: model -> The model to find the accuracy for
#        data -> The data         
# OUTPUT: The accuracy of the model
# ******************************************************************************
GetModelAccuracy <- function(model, data) {
  
  #Predict the outcome on a test set that the model has not yet seen.
  predictions <- predict(model, data)
  
  #Create a new dataframe containing the true weight_pounds and guessed weight_pouunds for each child in test dataset.
  results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=predictions))
  
  #Map a true/false value for each child if it is truely underweight (< 5.5 pounds)
  results$true_underweight <- ifelse(results$true_value < 5.5, TRUE, FALSE)
  #Map a true/false value for each child if it is predicted to be underweight (< 5.5 pounds)
  results$predicted_underweight <- ifelse(results$predicted_value < 5.5, TRUE, FALSE)
  
  #Calculate if the model prediction and true value are the same
  results$correctly_predicted <-ifelse(results$predicted_underweight == results$true_underweight, TRUE, FALSE)
  
  #Count the number it guessed correctly and incorrectly
  total_correct <- length(which(results$correctly_predicted==TRUE))
  total_incorrect <- length(which(results$correctly_predicted==FALSE))
  
  print("Total Correctly Predicted:")
  print(total_correct)
  print("Total Incorrectly Predicted:")
  print(total_incorrect)
  print("Accuracy Percentage:")
  print(total_correct/(length(results$correctly_predicted)))
  
  return(total_correct/(length(results$correctly_predicted)))
}
  
#Remove all values in environment to save memory and provide a clear workspace.
rm(list = ls())

#For reproducability on submission, seed the random generator.
set.seed(12345)

#Load in file
df<-read.csv("./data/natality_1k.csv")

#Remove all the fields we do not want to use
df$X <-NULL
df$child_race <-NULL
df$mother_race<-NULL
df$cigarette_use <-NULL
df$cigarettes_per_day <-NULL
df$alcohol_use <-NULL
df$drinks_per_week <-NULL
df$weight_gain_pounds <-NULL
df$born_alive_alive <-NULL
df$born_alive_dead <-NULL
df$born_dead <-NULL
df$ever_born <-NULL
df$father_race <-NULL
df$father_age <-NULL
df$record_weight <-NULL
df$year <-NULL
df$wday <-NULL
df$state <-NULL
df$apgar_1min <-NULL
df$apgar_5min <-NULL
df$mother_residence_state <-NULL
df$lmp <-NULL
df$mother_married <-NULL
df$mother_birth_state <-NULL
df$day <-NULL
df$month <-NULL
df$source_year <-NULL


#Filter the data to ensure the values are correct
df <- dplyr::filter(df,
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
                    !is.na(weight_pounds)
)

#If plurality is greater than 1, set value to 1 otherwise put in value 0
#This is because you can work out if an expectant mother has more than one child inside her without an ultrasound.
df$plurality_simplified <-ifelse(df$plurality > 1, 1, 0)

#Map the falues of "true" and "false" to numerical values.
df$is_male <- factor(df$is_male)
df$is_male <- as.numeric(df$is_male)

#Preprocess the data
#Normalising the data proved to reduce accuract for linear regression, therefore remove scalinging and centering.
preProcValues <- preProcess(df)

#Create a split of the dataset, ensuring the distribution of weight_pounds remains the same for both subsets.
trainIndex <- createDataPartition(df$weight_pounds, p=0.7, list=FALSE, times=1)
dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

#Set parameters, such as cross validation. This samples 10 blocks of data 5 times, building a model. Then averages the results.
control <- trainControl(method='repeatedcv', number=100, repeats=1)

#Train a new model with the best parameters hard-coded in, removing the need to try every possible combination again.
fit1 <- train(weight_pounds ~ is_male+plurality+mother_age+gestation_weeks, #Predict weight_pounds using all other fields in dataframe
              data=dfTrain, #Training dataset - 70% of original dataset
              method='earth', #Multivariate adaptive regression splines (MARS)
              # nprune=5, #Hard code in best value from fit0
              # degree=2, #Hard code in best value from fit0
              trControl=control) #Set repeated cross validation parameters


#Train a new model with the best parameters hard-coded in, removing the need to try every possible combination again.
fit2 <- train(weight_pounds ~ plurality_simplified+mother_age+gestation_weeks, #Predict weight_pounds using all other fields in dataframe
              data=dfTrain, #Training dataset - 70% of original dataset
              method='earth', #Multivariate adaptive regression splines (MARS)
              # nprune=8, #Hard code in best value from fit0
              # degree=1, #Hard code in best value from fit0
              trControl=control) #Set repeated cross validation parameters

#Get the model accuracy of the models with and without the ultrasound
UltraAcc <- GetModelAccuracy(fit1,dfTest)
NoUltraAcc <- GetModelAccuracy(fit2,dfTest)

modelResults <- data.frame(Model = c("Ultrasound Data", "No Ultrasound Data"),
                           Accuracy = c(UltraAcc, NoUltraAcc)
)


ggplot(data=modelResults, aes(x=Model, y=Accuracy, fill=Model)) +
  geom_bar(stat="identity") +
  coord_cartesian(ylim = c(0.9, 0.95))

