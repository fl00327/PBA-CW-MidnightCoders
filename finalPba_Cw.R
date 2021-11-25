#DNN Model
#Midnight Coders
library(caret)
library(nnet)
library(ggplot2)

rm(list = ls())
#Use

# weight_pounds,
# is_male
# mother_age
# pluarlity
# gestation_weeks

#Load in file
df<-read.csv("Natality_Preprocessed.csv")

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
#REMOVE THIS AFTER

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

#Back Up
df1<-df
#factor and normalise the data
# df$weight_pounds <- with(df, (weight_pounds - min(weight_pounds)) / (max(weight_pounds) - min(weight_pounds)))
# df$mother_age <- with(df, (mother_age - min(mother_age)) / (max(mother_age) - min(mother_age)))
# df$gestation_weeks<- with(df, (gestation_weeks - min(gestation_weeks)) / (max(gestation_weeks) - min(gestation_weeks)))

#df$is_male <- factor(df$is_male)
#df$is_male <- as.numeric(df$is_male)

#preProcValues <- preProcess(df, method = c("center", "scale"))
preProcValues <- preProcess(df)
#Create a split of the dataset, ensuring the distribution of weight_pounds remains the same for both subsets.
set.seed(123)
trainIndex <- createDataPartition(df$weight_pounds, p=0.7, list=FALSE, times=1)

dfTrain <- df[trainIndex,]
dfTest <- df[-trainIndex,]

# dfTrain <- predict(preProcValues, dfTrain_old)
# dfTest <- predict(preProcValues, dfTest_old)

#plot(0:nrow(df),df$weight_pounds )

#Set parameters, such as cross validation. This samples 10 blocks of data 10 times, building a model. Then averages the results.
control <- trainControl(method='repeatedcv', number=100, repeats=1)

fit1 <- train(weight_pounds ~ ., data=dfTrain, method='lm',
              trControl=control)

r2<-round(Nr2(fit1),digits=2) 
print(paste("Non Linear Regression: r^2 with lstat+age+rm=",r2))

# predict the outcome on a test set
predictions <- predict(fit1, dfTest)

results <- do.call(rbind, Map(data.frame, true_value=dfTest$weight_pounds, predicted_value=predictions))

results$true_underweight <- ifelse(results$true_value < 5.5, 1, 0)
results$predicted_underweight <- ifelse(results$predicted_value < 5.5, 1, 0)

results$correctly_predicted <-ifelse(results$predicted_underweight == results$true_underweight, 1, 0)

total_correct <- length(which(results$correctly_predicted==1))
total_incorrect <- length(which(results$correctly_predicted==0))

print(total_correct)
print(total_incorrect)

print(total_correct/(length(results$correctly_predicted)))

ggplot(data = data.frame(x = dfTest$weight_pounds, y = predict(fit1, dfTest)), aes(x = dfTest$weight_pounds, y = predict(fit1, dfTest))) +
  geom_point(color="black", alpha = 0.3) +
  geom_smooth(method = "lm")

varimp_mars <- varImp(fit1)
plot(varimp_mars, main="Variable Important")
