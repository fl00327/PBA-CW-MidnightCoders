# This clear all the objects in your Global environment
rm(list = ls())

# clears the console area
cat("\014")

# Sets R random number to start at the same sequence
set.seed(1234)


myLibraries<-c("scatterplot3d", "caret")


library(pacman)

pacman::p_load(char=myLibraries, install = TRUE, character.only = TRUE)

#Load additional R script files provide for this lab 
source("lab2functions.R")

natalB<-read.csv("Natality_Preprocessed")


natalB$X <-NULL

X<-split(natalB, natalB$is_male)
Male<-X$`1`
Female<-X$`0`
# Randomise the entire dataset

natalB<-natalB[order(runif(nrow(natalB))),]
Male<-Male[order(runif(nrow(Male))),]
Female<-Female[order(runif(nrow(Female))),]



names(natalB)

#Splitting the natalB dataset that includes both of the genders 
training_records<-round(nrow(natalB)*(70/100))
training_data<-natalB[1:training_records,]
testing_data = natalB[-(1:training_records),]


#Visualization will take 10 seconds
pairs(training_data[,c("weight_pounds","gestation_weeks","mother_age")])

NscatterPlotError<-function(datasetTrain, datasetTest, outputName,predictorName) {
  #Creates a "formula" and the trains model on TRAIN dataset
  formular<-paste(outputName,"~",predictorName)
  linearModel<-lm(formula=formular,data=datasetTrain)
  
  # Extract predictor (input) values from TEST dataset into a data frame
  predictorInput<-subset(datasetTest,select=predictorName)
  
  # Get predictions from the model using the TEST dataset
  y_predicted<-predict(linearModel, predictorInput)
  
  #Extract the expected response (output) values from TEST dataset
  # into a dataframe
  y_actual<-datasetTest[,outputName]
  
  # Calculate the metrics using functions in lab2functions.R
  RMSE<-round(Nrmse(actual=y_actual,predicted=y_predicted),digits=2)
  mae<-round(Nmae(actual=y_actual,predicted=y_predicted),digits=2)
  r2<-round(Nr2(linearModel),digits=2)
  
  error<-y_actual-y_predicted
  
  #Create a data frame, so that we can sort these 
  # the input predictor (x)
  # the expected value (actual_y)
  #  and the residuals in the model
  results<-data.frame(predictorInput,y_actual,error)
  
  
  #order from lowest to highest the valexpected values
  #for the ease of visualization
  
  results<-results[order(y_actual),]
  
  plot(results[,predictorName],
       results$y_actual,
       pch=4,
       ylab=outputName,
       xlab=predictorName,
       main="Linear Regression Errors",
       sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  
  
  #Plot the linear model as a straight line
  abline(linearModel,col = "blue", lwd=3)
  
  # Plot vertical lines from the actual points to the predicted value,
  # highlighting the error magnitude
  
  suppressWarnings(arrows(results[,predictorName],
                          results$y_actual,
                          results[,predictorName],
                          results$y_actual-results$error,
                          length=0.05,angle=90,code=3,col="red"))
  
  return(list(
    MAE=mae,
    RMSE=RMSE,
    r2=r2))
}

results<-NscatterPlotError(datasetTrain=training_data, datasetTest=testing_data,
                           outputName="weight_pounds", predictorName="gestation_weeks")



# Try it with a different predictor mother_age
results<-NscatterPlotError(datasetTrain=training_data, datasetTest=testing_data,
                           outputName="weight_pounds", predictorName="mother_age")


#Create and plot a linear model for all predictors

plotAllErrors<-function(datasetTrain, datasetTest, outputName){
  
  print("RESULTS")
  
  y<- data.frame(matrix(ncol = 4, nrow = 0))
  x<-c("Field","r2","RMSE","MAE")
  colnames(y) <- x
  
  for(i in 1:ncol(datasetTest)){
    
    xname<-names(datasetTest) [i]
    
    if(xname!="weight_pounds"){
      results<-NscatterPlotError(datasetTrain=datasetTrain, datasetTest=datasetTest,
                                 outputName=outputName, predictorName=xname)
      print(paste("Field=",xname,
                  "r2",results$r2,
                  "RMSE",results$RMSE,
                  "MAE",results$MAE))
      
      res<-data.frame(Field=xname,r2=results$r2,RMSE=results$RMSE,MAE=results$MAE)
      y<-rbind(y,res)
    }
  }
  print(y)
}

plotAllErrors(datasetTrain=training_data, datasetTest=testing_data,outputName="weight_pounds")


#Linear model with 2 or more predictors
linearModelTransform2Inputs<-lm(weight_pounds~gestation_weeks+weight_gain_pounds,data=training_data)
r2<-round(Nr2(linearModelTransform2Inputs),digits=2)
print(paste("r^2 with gestation_weeks + weight_gain_pounds added=",r2))


#Linear model with 2 or more predictors
linearModelTransform2Inputs<-lm(weight_pounds~mother_age+gestation_weeks,data=training_data)
r2<-round(Nr2(linearModelTransform2Inputs),digits=2)
print(paste("r^2 with mother_age + cigarette_use=",r2))



x<-natalB[,"gestation_weeks"]
y<-natalB[,"weight_pounds"]
z<-natalB[,"mother_age"]
library(scatterplot3d)
scatterplot3d(x,y,z, pch=16, highlight.3d = TRUE)



NscatterPlotMultiLinearRegression(datasetTrain = training_data,
                                  datasetTest = testing_data,
                                  outputName = "weight_pounds",
                                  predictorName1 = "gestation_weeks",
                                  predictorName2 = "cigarette_use")

NscatterPlotMultiLinearRegression(datasetTrain = training_data,
                                  datasetTest = testing_data,
                                  outputName = "weight_pounds",
                                  predictorName1 = "gestation_weeks",
                                  predictorName2 = "mother_age")



#"." means all variables"
linearModelTransformAllInputs<-lm(weight_pounds~.,data=training_data)


#This calculates the r squared value - a measure of fit 
r2<-round(Nr2(linearModelTransformAllInputs),digits=2) 
print(paste("Calculated r^2 with all variables added=",r2))


# Use caret library to determine scaled "importance"
importance<-as.data.frame(caret::varImp(linearModelTransformAllInputs, scale = TRUE))

#Removes puntuation characters from field names as these cause issues 
row.names(importance)<-gsub("[[:punct:][:blank:]]+", "", row.names(importance))

# Plot the % importance ordered from lowest to highest
barplot(t(importance[order(importance$Overall),,drop=FALSE]))


nonlinearModel<-lm(weight_pounds~polym(gestation_weeks,weight_gain_pounds,mother_age,father_age, 
                                       ever_born, degree = 7)+is_male+plurality, data = training_data)

#Wait 20 Seconds

r2<-round(Nr2(nonlinearModel),digits=2) 
print(paste("Non Linear Regression: r^2 with lstat+age+rm=",r2))



NscatterPlotNonLinearRegression(datasetTrain = training_data,
                                datasetTest = testing_data,
                                outputName = "weight_pounds",
                                predictorName = "gestation_weeks",
                                polyOrder = 5)

NscatterPlotNonLinearRegression(datasetTrain = training_data,
                                datasetTest = testing_data,
                                outputName = "weight_pounds",
                                predictorName = "weight_gain_pounds",
                                polyOrder = 7)



# Linear model  classification

threshold<-mean(training_data$weight_pounds)

NscatterPlotLinearDecisions(dataset=training_data, 
                            outputName = "weight_pounds",
                            predictorName = "gestation_weeks",
                            threshold = threshold)

?glm() # Is used to fit generalized linear models, 


logisticModel<-NscatterPlotLogisticRegression(datasetTrain = training_data,
                                              outputName = "weight_pounds",
                                              predictorName = "gestation_weeks",
                                              threshold = threshold)


scaled_training_data<-Nrescaleentireframe(training_data)
predictedProbs<-as.vector(predict(logisticModel,scaled_training_data,type = "response"))


predictedClass<-predictedProbs>0.5
actualClass<-scaled_training_data$medv>0.5


#please note sometimes the predicted/actual are swapped and sometimes the 
# definition of Negative and Positive are swapped, make sure they are not swapped in your project
confusion<-table(predictedClass,actualClass)
print(confusion)


print(NcalcConfusion(confusion))

