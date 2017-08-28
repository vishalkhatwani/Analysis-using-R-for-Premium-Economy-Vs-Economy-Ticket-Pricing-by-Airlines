str(airline)
View(airline)
str(airline)

library(psych)
describe(airline)

apply(airline,2,function(x) length(unique(x)))

xname <- c("Airline","Aircraft","TravelMonth","IsInternational","PitchEconomy","PitchPremium","WidthEconomy","WidthPremium","pricerel.fact","PitchDifference","WidthDifference")
y <- as.vector(xname)
y <- unlist(xname)
is.vector(v)
for(i in y)
{airline[,i] <- as.factor(airline[,i])}
i
airline$WidthDifference <- as.factor(airline$WidthDifference)

str(airline)

set.seed(123)

peoj <- sample.split(Y=airline,SplitRatio = 0.7)
training <- airline[peoj,]
test <- airline[!peoj,]
test$IsInternational <- as.factor(test$IsInternational)
str(test)
mtries <- tuneRF(training,training$pricerel.fact,stepFactor = 1.5,improve = 0.01,
                 trace = T,plot = T)

newran <- randomForest(training$pricerel.fact~.,data = training)
importance(newran)
varImpPlot(newran)

predict.test <- predict(newran,test,type = 'Class')
answer <- table(predicted=predict.test,actual=test$pricerel.fact)
answer
sum(diag(answer))/sum(answer)
