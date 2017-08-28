head(dataframe)
set.seed(123)
vk <- sample.split(Y=dataframe,SplitRatio = 0.7)
traindata <- dataframe[vk,]
testdata <- dataframe[!vk,]

bestmtry <- tuneRF(traindata,traindata$low,stepFactor = 1.2,
                   improve = 0.01, trace = T,plot = T)
wt_randfrst <- randomForest(traindata$low~.,data = traindata)
wt_randfrst

importance(wt_randfrst)
plot(wt_randfrst)
varImpPlot(wt_randfrst)

perdict_wt <- predict(wt_randfrst,testdata,type = 'class')
perdict_wt
matrixxx <- table(predicted=perdict_wt,actual=testdata$low)
matrixxx
library(caret)
install.packages("caret")
sum(diag(matrixxx))/sum(matrixxx)
