## 16 December 2014 
## Stefanie N. Kairs
## Practical Machine Learning - Course Project
library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(Hmisc)
library(plyr)
library(rpart)
library(randomForest)
set.seed(32979)

##Get data files
data_directory <- "data"

fileURL1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
dataFile1 <- "pml-training.csv"
dataPath1 <- paste(data_directory, dataFile1, sep="/")

if (!file.exists(dataPath1)){
    download.file(fileURL1, destfile=dataPath1, method="curl")
   }

fileURL2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
dataFile2 <- "pml-testing.csv"
dataPath2 <- paste(data_directory, dataFile2, sep="/")

if (!file.exists(dataPath2)){
    download.file(fileURL2, destfile=dataPath2, method="curl")
}

##Import data
trainPML <- read.csv(file=dataPath1, row.names=NULL, na.strings=c("NA","#DIV/0!"), as.is=TRUE)
trainPML <- trainPML[,-1]

##Clean Data
trainPML$user_name <- factor(trainPML$user_name)
trainPML$cvtd_timestamp <- strptime(trainPML$cvtd_timestamp[1], format="%m/%d/%Y %H:%M")
trainPML$new_window <- factor(trainPML$new_window)
trainPML$num_window <- factor(trainPML$num_window)
trainPML$classe <- factor(trainPML$classe)

trainSmall <- trainPML[,c(1,7:10,36:48,59:67,83:85,101,112:123,139,150:159)]

##PCA
typeColor <- (trainSmall[,54])
PCA <- preProcess(trainSmall[,c(-1,-54)], method="pca", thresh = 0.8)
predictPCA <- predict(PCA, trainSmall[,c(-1,-54)])

plot(predictPCA[,1], predictPCA[,2], col=typeColor)

##Split Test into Test/Train sets for model building and testing (90% train, 10% test)
set.seed(198082)
inTrain <- createDataPartition(trainSmall$classe, p = 0.9)[[1]]
train1 <- trainSmall[inTrain,]
test1 <- trainSmall[-inTrain,]

##rpart 
modelFitA <- train(classe ~ ., method = "rpart", data=train1)
fancyRpartPlot(modelFitA$finalModel)
predictions <- predict(modelFitA, train1)
CMA <- confusionMatrix(predictions, train1$classe)
print(CMA)


modelFit1 <- train(classe ~ ., method = "rf", data=train1, type=2, 
                   importance=TRUE, prox=TRUE)

predictions1 <- predict(modelFit1, newdata = test1)
CM1 <- confusionMatrix(predictions1, test1$classe)


modelFit2 <- train(classe ~ ., method = "rf", data=train1, type=2, 
                   importance=TRUE, prox=TRUE, trControl=trainControl(method="repeatedcv", number=10))

predictions2 <- predict(modelFit2, newdata = test1)
CM2 <- confusionMatrix(predictions2, test1$classe)
CM2norm <- confusionMatrix(predictions2, test1$classe, norm=)

##heatmapped confusion matrix
df <- data.frame(CM2$table)
df <- ddply(df, .(Reference), transform, normFreq = Freq / sum(Freq))

myPalette <- colorRampPalette((brewer.pal(9, "YlOrRd")))
colors <- scale_fill_gradientn(colours = myPalette(100), limits=c(0, 1))
p2 <- ggplot(data.frame(df)) + geom_tile(aes(Reference, Prediction, fill=normFreq)) + 
    colors + labs(title="Normalized Confusion Matrix, Model 2")

p2 <- p2 + geom_text(data = data.frame(df), 
                     aes(label = round(normFreq,4), y = Prediction, x = Reference), 
                     color = "black") + guides(fill=guide_legend(title="Normalized Frequency"))

p2


##testing model
testPML <- read.csv(file=dataPath2, row.names=NULL, na.strings=c("NA","#DIV/0!"), as.is=TRUE)
testPML <- testPML[,-1]

##Clean Data
testPML$user_name <- factor(testPML$user_name)
testPML$cvtd_timestamp <- strptime(testPML$cvtd_timestamp[1], format="%m/%d/%Y %H:%M")
testPML$new_window <- factor(testPML$new_window)
testPML$num_window <- factor(testPML$num_window)

##predict
answers <- predict(modelFit2, newdata = testPML)
answers <- as.character(answers)

##create submission files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}
    
    pml_write_files(answers)
