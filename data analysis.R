View(airline)
corrgram(airline,lower.panel = panel.shade,diag.panel = panel.minmax,upper.panel = panel.pie)

test <- ggplot(data = airline,aes(x=airline$WidthDifference,y=airline$PriceRelative))

test+geom_jitter(aes(colour=PriceRelative))#+geom_boxplot(aes(group=WidthDifference),alpha=0.3)
library(psych)
describe(airline)
trial <- lm(airline$PriceRelative~airline$PitchDifference+airline$WidthDifference+airline$FlightDuration
            +airline$PercentPremiumSeats+airline$IsInternational)
summary(trial)
test+geom_jitter(aes(col=airline$FlightDuration))
plot(airline$PitchDifference,airline$WidthDifference)

cor.test(airline$PitchDifference,airline$WidthDifference)
cor.test(airline$PriceRelative, airline$PercentPremiumSeats)
cor.test(airline$PriceRelative, airline$FlightDuration)


set.seed(123)

dataframe <- birthwt
help("birthwt")
View(dataframe)

str(dataframe)

apply(dataframe,2,function(x) length(unique(x)))
attach(dataframe)
colfac <- c("low","race","smoke","ptl","ht","ui","ftv")
is.array(colfac)
for(i in colfac)
{dataframe[,i]=as.factor(dataframe[,i])}
str(dataframe)

ind <- sample.split(Y=dataframe$low, SplitRatio = 0.7)
train.df <- dataframe[ind,]
test.df <- dataframe[!ind,]

modelrand <- randomForest(low~.,data=train.df,mtry=3,ntree=20)
modelrand

importance(modelrand)
varImpPlot(modelrand)
plot(modelrand)
prdictclass <- predict(modelrand, test.df,type = 'class')
prdictclass
t <- table(predictions = prdictclass,actual = test.df$low)
t
sum((diag(t)/sum(t))) #accuracy of the model
library(pROC)


prediction.prob <- predict(modelrand,test.df, type='prob')
prediction.prob
area.under.curve <- auc(test.df$low,prediction.prob[,2])
area.under.curve
plot(roc(test.df$low,prediction.prob[,2]))

bestmtry <- tuneRF(train.df,train.df$low,ntreeTry = 200, stepFactor = 1.2, improve = 0.01,
                   trace = T,plot = T)
