#Practical Machine Learning Script
#creates algorithim on training data, runs it on 20 test cases
#outputs single character txt files with results
#
#Development of algorithm detailed in Practical_Machine_Learning.Rmd in same repository

#train on train.csv
library(lattice); library(ggplot2); library(caret); library(psych)
if (!file.exists("train.csv")){
    fileURL<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv'
    download.file(fileURL, destfile = "./train.csv", method = "curl")
}
train<-read.csv('train.csv', na.strings = c("NA", NULL, '#DIV/0!'))
set.seed(0)
inTrain = createDataPartition(train$class, p = 0.6)[[1]]
trainingPreprocess = train[ inTrain,]
testingPreprocess = train[-inTrain,]

numericCols<-sapply(trainingPreprocess, is.numeric)
training<-trainingPreprocess[numericCols] #drop non-numeric cols
described<-describe(training)
training<-training[described$n>0.1*length(training[,1])] #drop mostly empty cols
training<- training[4:length(training)] #drop col index and timestamps

classe<-trainingPreprocess$classe
training<-cbind(training, classe)
mod_rf<-randomForest(classe~., data=training)

#test training on test.csv
if (!file.exists("test.csv")){
    fileURL<-'https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv'
    download.file(fileURL, destfile = "./test.csv", method = "curl")
}
test<-read.csv('test.csv', na.strings = c("NA", NULL, '#DIV/0!'))
predTest <- predict(mod_rf,newdata=test) 

#write each result out as single character in .txt file with problem id as title
#formatted for submission for coursera
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE, eol="")
    }
}
pml_write_files(predTest)