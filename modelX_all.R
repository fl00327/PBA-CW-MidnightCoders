# ***************************************
#      Midnight Coders COM3018 Coursework
#        All Model Tests R File
#             November 2021
#
#        Model to predict:
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
  library(caret)
  library(ggplot2)
  
  #Remove all values in environment to save memory and provide a clear workspace.
  rm(list = ls())
  
  #For reproducability on submission, seed the random generator.
  set.seed(12345)
  
  #Regression to predict
  # weight_pounds
  
  #Predictor Fields
  # is_male
  # mother_age
  # pluarlity
  # gestation_weeks
  
  
  #Load in file
  df<-read.csv("./data/natality_10k.csv")
  
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
  
  #Preprocess the data
  #Normalising the data proved to reduce accuracy for linear regression, therefore remove scaling and centering. 
  preProcValues <- preProcess(df)
  
  #Create a split of the dataset (70:30 Train:Test), ensuring the distribution of weight_pounds remains the same for both subsets.
  trainIndex <- createDataPartition(df$weight_pounds, p=0.7, list=FALSE, times=1)
  dfTrain <- df[trainIndex,]
  dfTest <- df[-trainIndex,]
  
  #Set parameters, such as cross validation. This samples 10 blocks of data 1 time(s), building a model. Then averages the results.
  control <- trainControl(method='repeatedcv', number=100, repeats=1)
  
  #Build LM Model
  
  lmModel <- train(weight_pounds ~ ., #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,      #Training dataset - 70% of original dataset
                method='lm',       #Multiple Linear Regression
                trControl=control  # Set repeated cross validation parameters
  )
  
  
  #Build RF Model
  
  rfModel <- train(weight_pounds ~ ., #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,      #Training dataset - 70% of original dataset
                method='rf',       #Multiple Linear Regression
                trControl=control  #Set repeated cross validation parameters
  )
  
  
  #Build NN Model
  
  nnModel <- train(weight_pounds ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='mlpML',
                layer1=10,
                layer2=5,#Multivariate adaptive regression splines (MARS)
                #tuneGrid = neuralnetGrid, #Hyperparameter Tuning, changing perceptrons in each layer
                trControl=control)   #Set repeated cross validation parameters
  
  #Build MARS Model
  
  #Train a new model with the best parameters hard-coded in, removing the need to try every possible combination again.
  marsModel <- train(weight_pounds ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='earth',      #Multivariate adaptive regression splines (MARS)
                #nprune=5,            #Hard code in best value from fit0       
                #degree=2,            #Hard code in best value from fit0
                trControl=control)   #Set repeated cross validation parameters
  
  #Get LM Stats
   
  LM_predictions <- predict(lmModel, dfTest)
  
  #Create a new dataframe containing the true weight_pounds and guessed weight_pouunds for each child in test dataset.
  LM_results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=LM_predictions))
  
  #Map a true/false value for each child if it is truely underweight (< 5.5 pounds)
  LM_results$true_underweight <- ifelse(LM_results$true_value < 5.5, TRUE, FALSE)
  #Map a true/false value for each child if it is predicted to be underweight (< 5.5 pounds)
  LM_results$predicted_underweight <- ifelse(LM_results$predicted_value < 5.5, TRUE, FALSE)
  
  #Calculate if the model prediction and true value are the same
  LM_results$correctly_predicted <-ifelse(LM_results$predicted_underweight == LM_results$true_underweight, TRUE, FALSE)
  
  #Count the number it guessed correctly and incorrectly
  LM_total_correct <- length(which(LM_results$correctly_predicted==TRUE))
  LM_total_incorrect <- length(which(LM_results$correctly_predicted==FALSE))
  
  print("Total Correctly Predicted:")
  print(LM_total_correct)
  print("Total Incorrectly Predicted:")
  print(LM_total_incorrect)
  print("Accuracy Percentage:")
  print(LM_total_correct/(length(LM_results$correctly_predicted)))
  
  LM_accuracy <-LM_total_correct/(length(LM_results$correctly_predicted))
  
  #NN Model Accuracy
  
  NN_predictions <- predict(nnModel, dfTest)
  
  #Create a new dataframe containing the true weight_pounds and guessed weight_pouunds for each child in test dataset.
  NN_results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=NN_predictions))
  
  #Map a true/false value for each child if it is truely underweight (< 5.5 pounds)
  NN_results$true_underweight <- ifelse(NN_results$true_value < 5.5, TRUE, FALSE)
  #Map a true/false value for each child if it is predicted to be underweight (< 5.5 pounds)
  NN_results$predicted_underweight <- ifelse(NN_results$predicted_value < 5.5, TRUE, FALSE)
  
  #Calculate if the model prediction and true value are the same
  NN_results$correctly_predicted <-ifelse(NN_results$predicted_underweight == NN_results$true_underweight, TRUE, FALSE)
  
  #Count the number it guessed correctly and incorrectly
  NN_total_correct <- length(which(NN_results$correctly_predicted==TRUE))
  NN_total_incorrect <- length(which(NN_results$correctly_predicted==FALSE))
  
  print("Total Correctly Predicted:")
  print(NN_total_correct)
  print("Total Incorrectly Predicted:")
  print(NN_total_incorrect)
  print("Accuracy Percentage:")
  print(NN_total_correct/(length(NN_results$correctly_predicted)))
  
  NN_accuracy <-NN_total_correct/(length(NN_results$correctly_predicted))
  
  #RF Model
  
  RF_predictions <- predict(rfModel, dfTest)
  
  #Create a new dataframe containing the true weight_pounds and guessed weight_pouunds for each child in test dataset.
  RF_results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=RF_predictions))
  
  #Map a true/false value for each child if it is truely underweight (< 5.5 pounds)
  RF_results$true_underweight <- ifelse(RF_results$true_value < 5.5, TRUE, FALSE)
  #Map a true/false value for each child if it is predicted to be underweight (< 5.5 pounds)
  RF_results$predicted_underweight <- ifelse(RF_results$predicted_value < 5.5, TRUE, FALSE)
  
  #Calculate if the model prediction and true value are the same
  RF_results$correctly_predicted <-ifelse(RF_results$predicted_underweight == RF_results$true_underweight, TRUE, FALSE)
  
  #Count the number it guessed correctly and incorrectly
  RF_total_correct <- length(which(RF_results$correctly_predicted==TRUE))
  RF_total_incorrect <- length(which(RF_results$correctly_predicted==FALSE))
  
  print("Total Correctly Predicted:")
  print(RF_total_correct)
  print("Total Incorrectly Predicted:")
  print(RF_total_incorrect)
  print("Accuracy Percentage:")
  print(RF_total_correct/(length(RF_results$correctly_predicted)))
  
  RF_accuracy <-RF_total_correct/(length(RF_results$correctly_predicted))
  
  #MARS Model
  
  MARS_predictions <- predict(marsModel, dfTest)
  
  #Create a new dataframe containing the true weight_pounds and guessed weight_pouunds for each child in test dataset.
  MARS_results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=MARS_predictions))
  
  #Map a true/false value for each child if it is truely underweight (< 5.5 pounds)
  MARS_results$true_underweight <- ifelse(MARS_results$true_value < 5.5, TRUE, FALSE)
  #Map a true/false value for each child if it is predicted to be underweight (< 5.5 pounds)
  MARS_results$predicted_underweight <- ifelse(MARS_results$predicted_value < 5.5, TRUE, FALSE)
  
  #Calculate if the model prediction and true value are the same
  MARS_results$correctly_predicted <-ifelse(MARS_results$predicted_underweight == MARS_results$true_underweight, TRUE, FALSE)
  
  #Count the number it guessed correctly and incorrectly
  MARS_total_correct <- length(which(MARS_results$correctly_predicted==TRUE))
  MARS_total_incorrect <- length(which(MARS_results$correctly_predicted==FALSE))
  
  print("Total Correctly Predicted:")
  print(MARS_total_correct)
  print("Total Incorrectly Predicted:")
  print(MARS_total_incorrect)
  print("Accuracy Percentage:")
  print(MARS_total_correct/(length(MARS_results$correctly_predicted)))
  
  MARS_accuracy <-MARS_total_correct/(length(MARS_results$correctly_predicted))
}

main()
