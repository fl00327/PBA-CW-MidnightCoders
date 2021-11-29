# ***************************************
#   Midnight Coders COM3018 Coursework
#         Support Vector Machine R File
#             November 2021
#
#       Support Vector Machine to predict:
#             weight_pounds
#
#         Predictor Fields:
#             is_male
#            mother_age
#             pluarlity
#          gestation_weeks
#
# ***************************************

# ******************************************************************************
# main(): The entry point of the code
# INPUT: None
# OUTPUT: None
# ******************************************************************************
main <- function(){

  #Libraries 
  library(ggplot2)
  
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
                      !is.na(weight_pounds),
  )
  
  #Map the values of "true" and "false" to numerical values.
  df$is_male <- factor(df$is_male)
  df$is_male <- as.numeric(df$is_male)
  
  #Map weight_pounds to a true-false underweight field for classification
  df$underweight <- ifelse(df$weight_pounds < 5.5, TRUE, FALSE)
  
  #Remove the weight_pounds field as it is no longer needed.
  df$weight_pounds <- NULL
  df$underweight <- factor(df$underweight)
  
  #Normalising the data proved to reduce accuracy for linear regression, therefore remove scaling and centering.  
  preProcValues <- preProcess(df)
  
  #Create a split of the dataset (70:30 Train:Test), ensuring the distribution of weight_pounds remains the same for both subsets.
  trainIndex <- createDataPartition(df$underweight, p=0.7, list=FALSE, times=1)
  dfTrain <- df[trainIndex,]
  dfTest <- df[-trainIndex,]
  
  svmTuneGrid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
  
  #Set parameters, such as cross validation. This samples 10 blocks of data 5 times, building a model. Then averages the results.
  control <- trainControl(method='repeatedcv', number=100, repeats=1)
  
  #Train a variety of models using the hyperparameter tuning to decide on the best value.
  fit0 <- train(underweight ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='svmLinear',      #Support Vector Machine ML Model
                tuneGrid = svmTuneGrid, #Hyperparameter Tuning, changing c through 0-5 at various intervals
                trControl=control)   #Set repeated cross validation parameters
  
  #See what parameters were best 
  print(fit0)
  
  fit1 <- train(underweight ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='svmLinear',      #Support Vector Machine ML Model
                trControl=control)   #Set repeated cross validation parameters
  
  
  #Predict the outcome on a test set that the model has not yet seen. 
  predictions <- predict(fit1, dfTest)
}

main()
