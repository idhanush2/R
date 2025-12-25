library(dplyr) 
library(ggplot2)
library(caret)
head(storms)

ggplot(storms, aes(category)) + geom_bar()

#cleaning the dataset

storm <- storms %>% select(-c('name')) #select all but name
storm <- na.omit(storm) #remove na
head(storm)

dummy <- dummyVars(category ~ ., data = storm)

dummies <- as.data.frame(predict(dummy, newdata = storm))

head(dummies)

nzv <- nearZeroVar(dummies) #find and remove zero variance predictors

length(nzv) 

storm.pca <- prcomp(dummies)
summary(storm.pca) #pca with cumulative proportions

screeplot(storm.pca, type = 'l') + title(xlab = 'PCs') # visualize the scree plot

#separating target variable in pca

target <- storm %>% dplyr::select(category)

#create the components
preProc <- preProcess(dummies, method = 'pca', pcaComp = 2)
storm.pc <- predict(preProc, dummies)
#put back the target column
storm.pc$category <- storm$category
head(storm.pc)

#base model
library(e1071)
storm_dummies <- dummies
storm_dummies$category <- storm$category
train_control = trainControl(method = 'cv', number = 5)
svm_storm <- train(category ~., data = storm_dummies, method = 'svmLinear', trControl = train_control)
svm_storm

#fit the model
svm_storm2 <-train(category ~., data = storm.pc, method = 'svmLinear', trControl = train_control)
svm_storm2
