DATASET_FILENAME  <- "Natality_Preprocessed"          #Name of input dataset file
OUTPUT_FIELD      <- "weight_pounds"             # Field name of the output class to predict

# These are the data preparation values

HOLDOUT           <- 70                   # % split to create TRAIN dataset

# Cutoff values - you can experiment with these

CUTOFF_OUTLIER    <- 0.99                 # Confidence p-value for outlier detection
# negative = analyse but do not replace outliers
CUTOFF_DISCRETE   <- 5                    # Number of empty bins to determine discrete
CUTOFF_REDUNDANT  <- 0.95                 # Linear correlation coefficient cut-off

# Indicates the type of each field

TYPE_DISCRETE     <- "DISCRETE"           # field is discrete (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is not encoded

MAX_LITERALS      <- 55                    # Maximum numner of 1-hot-encoding fields

# These are the supervised model constants

PDF_FILENAME      <- "tree.pdf"           # Name of PDF with graphical tree diagram
RULES_FILENAME    <- "rules.txt"          # Name of text file with rules saved
RESULTS_FILENAME  <- "results.csv"        # Name of the CSV results file
NODE_LEVEL        <- 1                    # The number is the node level of the tree to print
BOOST             <- 20                   # Number of boosting iterations. 1=single model
FOREST_SIZE       <- 1000                 # Number of trees in the forest
SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage

BASICNN_HIDDEN    <- 5                    # 10 hidden layer neurons
BASICNN_EPOCHS    <- 100                  # Maximum number of training epocs

# See https://cran.r-project.org/web/packages/h2o/h2o.pdf

DEEP_HIDDEN       <- c(5,5)               # Number of neurons in each layer
DEEP_STOPPING     <- 2                    # Number of times no improvement before stop
DEEP_TOLERANCE    <- 0.01                 # Error threshold
DEEP_ACTIVATION   <- "TanhWithDropout"    # Non-linear activation function
DEEP_REPRODUCABLE <- TRUE                 # Set to TRUE to test training is same for each run



MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "formattable",
               "stats",
               "caret",
               "PerformanceAnalytics",
               "stringr",
               "partykit",
               "C50",
               "randomForest",
               "keras",
               "h2o")

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

library(keras)

#Load additional R script files provide for this lab
source("4labFunctions.R")
source("lab4DataPrepNew.R")

randomForest<-function(train,test){
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  
  train_inputs<-train[-positionClassOutput]
  
  train_expected<-train[,positionClassOutput]
  myTitle<-"Preprocessed Dataset. Random Forest= 1000 trees"
  
  rf<-randomForest::randomForest(x=train_inputs,
                                 factor(train_expected),
                                 ntree=FOREST_SIZE,
                                 mtry=sqrt(ncol(train_inputs)))
  
  print(summary(rf))
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = rf,
                                   testDataset = test,
                                   title=myTitle)
  
  
  return(measures)
}

fullDT<-function(train,test,boost=1,plot=TRUE){
  positionClassOutput<-which(names(train)==OUTPUT_FIELD)
  # train data: dataframe with the input fields
  train_inputs<-train[-positionClassOutput]
  # train data: vector with the expedcted output
  train_expected<-train[,positionClassOutput]
  # ************************************************
  # Create a standard Decision Tree using the C5.0 algorithm
  # Uses library C50
  # Outputs the tree in the format of rules
  myTitle<-"Preprocessed Dataset. DT C5.0"
  if (boost>1)
    myTitle<-paste(myTitle,"BOOSTED=",boost)
  print(myTitle)
  tree<-C50::C5.0(x=train_inputs,
                  factor(train_expected),
                  rules=TRUE,
                  trials=BOOST)
  print(summary(tree))
  # Use the created decision tree with the test dataset
  # to determine best classification threshold & calculate metrics
  measures<-getTreeClassifications(myTree = tree,
                                   testDataset = test,
                                   title=myTitle)
  
  if (plot==TRUE){
    
    print(summary(tree))
    # Get importance of the input fields
    importance<-C50::C5imp(tree, metric = "usage")
    names(importance)<-"Strength"
    importance<-importance[order(importance$Strength, decreasing = TRUE),,drop=FALSE]
    
    print(formattable::formattable(importance))
    
    # Plot the importance fields
    barplot(t(importance),las=2,
            border = 0, cex.names =0.7,
            main=myTitle)
    
    dftreerules<-NDT5RuleOutput(tree)
    print(formattable::formattable(dftreerules))
    
    
    print("Plot decision tree to file called tree.pdf")
    Global_train_inputs<<-train_inputs
    Global_train_expected<<-train_expected
    # :: is used to specify a function within the named package to avoid confusion
    tree<-C50::C5.0(x=Global_train_inputs,
                    factor(Global_train_expected),
                    trials=boost)
    # ::: is used to directly access a member of a package that is internal
    graphtree<-C50:::as.party.C5.0(tree)
    # The plot is large - so print to a big PDF file
    pdf(PDF_FILENAME, width=100, height=50, paper="special", onefile=F)
    # The number is the node level of the tree to print
    plot(graphtree[NODE_LEVEL])
    #This closes the PDF file
    dev.off()
  }
  return(measures)
}

simpleDT<-function(train,test,plot=TRUE){
  positionClassOutput<-which(names(train) == OUTPUT_FIELD)
  x<-train[-positionClassOutput]
  y<-factor(train[,positionClassOutput])
  
  tree <- C50::C5.0(x, y, trials=1, rules=TRUE)
  measures<-getTreeClassifications(tree, test, title = "Orginal Dataset. DT C5.0")
  return(measures)
}

getTreeClassifications<-function(myTree,testDataset,title,classLabel=1, plot=TRUE){
  positionClassOutput<-which(names(testDataset) == OUTPUT_FIELD)
  x<-testDataset[-positionClassOutput]
  test_inputs<-data.frame(x)
  testPredictedClassProbs<-predict(myTree, test_inputs, type="prob")
  
  # Get the column index with the class label
  classIndex<-which(as.numeric(colnames(testPredictedClassProbs)) == classLabel)
  
  # Get the probabilities for classifying the good loans
  test_predictedProbs<-testPredictedClassProbs[,classIndex]
  
  #test data: vector with just the expected output class
  test_expected<-testDataset[,positionClassOutput]
  
  measures<-NdetermineThreshold(test_predictedProbs,test_expected,plot=plot,title=title)
  return(measures)
}

mlpNeural<-function(train,test, plot = TRUE){
  myTitle<-paste("Preprocessed Dataset. MLP. Hidden=",BASICNN_HIDDEN,sep="")
  print(myTitle)
  
  mlp_classifier<-N_MLP_TrainClassifier(train=train, fieldNameOutput=OUTPUT_FIELD,
                                        hidden=BASICNN_HIDDEN,
                                        plot=plot)
  
  measures<-N_evaluate_MLP(test=test, fieldNameOutput=OUTPUT_FIELD,
                           mlp_classifier=mlp_classifier,
                           plot=plot,
                           myTitle=myTitle)
  return(measures)
}

metricsToRealWorld<-function(dataset, measures, natural){
  badloan<-0
  goodloan<-0
  positionClassOutput=which(names(dataset) == OUTPUT_FIELD)
  x<-dataset[positionClassOutput]
  inputs<-data.frame(x)
  for(field in 1:(nrow(inputs))){
    
    if (inputs[field,1] == 0) {
      badloan<-badloan + 1
    }else {
      goodloan<-goodloan + 1
    }
  }
  
  classBalance<-badloan/goodloan
  print(classBalance)
  print(paste("Class balance, bad:good=", round(classBalance, digits = 2)))
}

deepNeural<-function(train, test, plot = TRUE){
  myTitle<-"Preprocessed Dataset. Deep NN"
  N_DEEP_Initialise()
  deep_classifier<-N_DEEP_TrainClassifier(train=train,
                                          fieldNameOutput=OUTPUT_FIELD,
                                          hidden=DEEP_HIDDEN,
                                          stopping_rounds=DEEP_STOPPING,
                                          stopping_tolerance=DEEP_TOLERANCE,
                                          activation=DEEP_ACTIVATION,
                                          reproducible=DEEP_REPRODUCABLE)
  
  # Evaluate the deep NN as we have done previously
  measures<-N_EVALUATE_DeepNeural(test=test,
                                  fieldNameOutput=OUTPUT_FIELD,
                                  deep=deep_classifier,
                                  plot=plot,
                                  myTitle = myTitle)
  
  if (plot==TRUE){
    # ************************************************
    # TELL ME SOMETHING INTERESTING...
    summary(deep_classifier)
    plot(deep_classifier) # plots the scoring history
    # variable importance from the deep neural network
    importance = as.data.frame(h2o::h2o.varimp(deep_classifier))
    row.names(importance)<-importance$variable
    importanceScaled<-subset(importance, select=scaled_importance)*100
    colnames(importanceScaled)<-"Strength"
    
    barplot(t(importanceScaled),las=2, border = 0,
            cex.names =0.7,
            main=myTitle)
    print(formattable::formattable(data.frame(importanceScaled)))
  }
  # ************************************************
  return(measures)
  
}




main<-function(){
  
  loans <- NreadDataset(DATASET_FILENAME)
  original<-NConvertClass(loans)
  original<-NPREPROCESSING_splitdataset(original)
  
  measures<-simpleDT(original$train, original$test)
  allResults<-data.frame(DT_raw=unlist(measures))
  
  measures<-fullDT(original$train, original$test)
  allResults<-cbind(allResults, data.frame(DT_preprocess=unlist(measures)))
  
  #NprintMeasures(measures, title = "Original Dataset. DT C5.0")
  
  dataset<-NPREPROCESSING_dataset(loans, scaleFlag = TRUE)
  
  splitData<-NPREPROCESSING_splitdataset(dataset)
  
  #naturalClassBalance<-2/100
  #measures<-metricsToRealWorld(dataset = dataset, measures = measures, natural = naturalClassBalance)
  
  
  
  measures<-randomForest(train=splitData$train, test = splitData$test)
  allResults<-cbind(allResults, data.frame(RandomForest=unlist(measures)))
  
  
  measures<-mlpNeural(train=splitData$train, test=splitData$test)
  allResults<-cbind(allResults, data.frame(MLP=unlist(measures)))
  
  measures<-deepNeural(train=splitData$train, test=splitData$test)
  allResults<-cbind(allResults, data.frame(Deep_Neural=unlist(measures)))
  
  #Swapping columns to rows, rows to column
  allResults<-data.frame(t(allResults))
  #Sorting with highest to lowest MCC
  allResults<-allResults[order(allResults$MCC, decreasing = TRUE),]
  
  #Printing out results and formatting them and storing them into a csv file
  print(formattable::formattable(allResults))
  write.csv(allResults,file = RESULTS_FILENAME)
  
  
} 



gc() # garbage collection to automatically release memory

# clear plots and other graphics
if(!is.null(dev.list())) dev.off()
graphics.off()

# This clears all warning messages
assign("last.warning", NULL, envir = baseenv())

# clears the console area
cat("\014")

print("START Supervised Machine Learning")


set.seed(123)

# ************************************************
main()

print("end")


loans <- read.csv(DATASET_FILENAME)


original<-NConvertClass(loans)
loans<-loans[,-(1)]
original<-NPREPROCESSING_splitdataset(original)

measures<-simpleDT(original$train, original$test)
allResults<-data.frame(DT_raw=unlist(measures))

measures<-fullDT(original$train, original$test)
allResults<-cbind(allResults, data.frame(DT_preprocess=unlist(measures)))

#NprintMeasures(measures, title = "Original Dataset. DT C5.0")

dataset<-NPREPROCESSING_dataset(loans, scaleFlag = TRUE)

splitData<-NPREPROCESSING_splitdataset(loans)

#naturalClassBalance<-2/100
#measures<-metricsToRealWorld(dataset = dataset, measures = measures, natural = naturalClassBalance)



measures<-randomForest(train=splitData$train, test = splitData$test)
allResults<-cbind(allResults, data.frame(RandomForest=unlist(measures)))


measures<-mlpNeural(train=splitData$train, test=splitData$test)
allResults<-cbind(allResults, data.frame(MLP=unlist(measures)))

measures<-deepNeural(train=splitData$train, test=splitData$test)
allResults<-cbind(allResults, data.frame(Deep_Neural=unlist(measures)))

#Swapping columns to rows, rows to column
allResults<-data.frame(t(allResults))
#Sorting with highest to lowest MCC
allResults<-allResults[order(allResults$MCC, decreasing = TRUE),]

#Printing out results and formatting them and storing them into a csv file
print(formattable::formattable(allResults))
write.csv(allResults,file = RESULTS_FILENAME)


