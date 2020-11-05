
install.packages('MLmetrics')
install.packages('rpart')
library(e1071)
library(lattice)
library(ggplot2)
library(caret)
library(MLmetrics)


dataset_n <- read.csv("E:/UTA-MSCS/Data Mining/Project1/data_file.csv")
#dataset_n = read.csv('data_file.csv')

#removing rows with unknown values
dataset_n[dataset_n == " ?"] <- NA
dataset_n <- na.omit(dataset_n)

#splitting the dataset in to training and test sets

set.seed(10)
sampledata <- dataset_n[sample(nrow(dataset_n), 2000, replace = FALSE),]
sample_size = floor(0.60*nrow(sampledata))
nrow(sampledata)
#View(train)

train_ind = sample(seq_len(nrow(sampledata)),size = sample_size)
train = sampledata[train_ind,]
test = sampledata[-train_ind,]


#implementing Naive bayes
model=naiveBayes(X~.,data = train)
pred=predict(model,test)

#ConfusionMatrix
confusionMatrix(pred, test$X)
#table(pred)
#table(test$X)

# tabulating to calculate precision and recall
xtab <- table(pred, factor(test$X))
precision_lessthan_50 <- xtab[1]/(xtab[1]+xtab[3])
recall_lessthan_50 <- xtab[1]/(xtab[1]+xtab[2])
precision_lessthan_50
recall_lessthan_50

#calculating F1 score
F1_Score(factor(test$X), pred, positive = NULL)

#plot ROC

Rpred <- prediction(as.numeric(pred),as.numeric(test$X))
perf_N <- performance(Rpred, measure='tpr', x.measure='fpr')
plot(perf_N)

