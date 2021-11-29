# ***************************************
#      Midnight Coders COM3018 Coursework
#   Multilayer Perceptron model Model R File
#             November 2021
#
#   Multilayer Perceptron model to predict:
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
  
  #Normalising the data proved to reduce accuracy for linear regression, therefore remove scaling and centering.  
  preProcValues <- preProcess(df)
  
  #Create a split of the dataset (70:30 Train:Test), ensuring the distribution of weight_pounds remains the same for both subsets.
  trainIndex <- createDataPartition(df$weight_pounds, p=0.7, list=FALSE, times=1)
  dfTrain <- df[trainIndex,]
  dfTest <- df[-trainIndex,]
  
  
  #Set parameters, such as cross validation. This samples 10 blocks of data 5 times, building a model. Then averages the results.
  control <- trainControl(method='repeatedcv', number=100, repeats=1)
  
  neuralnetGrid <-  expand.grid(layer1 = seq(0, 100, by = 10), 
                                layer2 = seq(0, 100, by = 10),
                                layer3 = seq(0, 100, by = 10)
                                
  )
  
  #Train a variety of models using the hyperparameter tuning to decide on the best value.
  fit0 <- train(weight_pounds ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='mlpML',      #Multilayer Perceptron model
                tuneGrid = neuralnetGrid, #Hyperparameter Tuning, changing nprune and degree
                trControl=control)   #Set repeated cross validation parameters
  
  #See what parameters were best 
  print(fit0)
  
  #Train a new model with the best parameters hard-coded in, removing the need to try every possible combination again.
  fit1 <- train(weight_pounds ~ .,   #Predict weight_pounds using all other fields in dataframe
                data=dfTrain,        #Training dataset - 70% of original dataset
                method='mlpML',      #Multilayer Perceptron model
                layer1=10,            #Hard code in best value from fit0       
                layer2=5,            #Hard code in best value from fit0
                trControl=control)   #Set repeated cross validation parameters
  
  #Predict the outcome on a test set that the model has not yet seen. 
  predictions <- predict(fit1, dfTest)
  
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
  
  
  #To prevent overplotting, sample the dataset if its too large and plot a random sample. 
  #Note this is NOT stratified sampling of correctly/incorrectly guessed values, it is random sampling.
  if (nrow(results) > 500) {
    results_to_plot <- results[sample(nrow(results), 500),]
  } else {
    #if less than 500 results to plot, plot them all.
    results_to_plot <- results
  }
  
  #Plot a graph displaying values (potentially sampled) with colour coding to indicate if guessed correctly or not.
  ggplot(results_to_plot, aes(x = true_value, y = predicted_value, color=factor(correctly_predicted, levels = c(TRUE, FALSE)))) +
    geom_point( alpha = 0.3) +
    coord_cartesian(xlim =c(2, 10), ylim = c(2, 10)) +
    scale_color_manual(values = c("#00BFC4", "#F8766D")) +
    labs(
      x = "True Birth Weight (pounds)",
      y = "Predicted Birth Weight (pounds)",
      colour = "Correctly Guessed")
  
      #Add line to graph to show what a 'perfect' model would predict.
      #geom_abline(intercept = 0, slope = 1, color="red", alpha=0.4, linetype="dashed") +
      
      #Add a regression line to output of model. Useful for comparison to above line to compare perfect vs actual performance.
      #geom_smooth(method = "lm", se=FALSE)
}

main()