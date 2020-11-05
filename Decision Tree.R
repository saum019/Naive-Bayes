
#install rpart library (Recursive partitioning for classification, regression and survival trees)
install.packages('rpart')
install.packages('rattle')
install.packages('RColorBrewer')
install.packages('ggplot2')
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
install.packages('MLmetrics')
library(MLmetrics)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)
library(caret)

#reading from CSV file
# dataset = read.csv('data_file.csv', colClasses=c("integer", "character", "integer",
#                                                  "character", "integer", "character",
#                                                   "character", "character", "character",
#                                                   "character", "integer", "integer",
#                                                  "integer", "character", "character"),)
dataset = read.csv('data_file.csv')
dataset

#removing rows with unknown values
dataset[dataset == " ?"] <- NA
dataset <- na.omit(dataset)

#splitting the dataset in to training and test sets
set.seed(10)
sampledata <- dataset[sample(nrow(dataset), 2000, replace = FALSE),]
sample_size = floor(0.60*nrow(sampledata))
nrow(sampledata)

#sample_size = floor(0.80*nrow(sampleda))
train_ind = sample(seq_len(nrow(sampledata)),size = sample_size)
train_set = sampledata[train_ind,]
test_set = sampledata[-train_ind,]

#Split using Information Gain with all attributes
IGregressor = rpart(formula = X ~ .,
                  data = train_set,
                  parms = list(split = 'information'), 
                  method = "class")

#Witholding the attribute relationship  
IGregressor = rpart(formula = X ~ education.num + Age + Workclass + education + fnlwgt + occupation + maritial.status + race + sex + capital.gain + capital.loss + hours.per.week + native.country ,
                    data = train_set,
                    parms = list(split = 'information'), 
                    method = "class")

#Witholding the attribute education.number,relationship 
IGregressor = rpart(formula = X ~ Age + Workclass + education + occupation  + race + sex + capital.gain + capital.loss + hours.per.week + native.country + fnlwgt + maritial.status ,
                    data = train_set,
                    parms = list(split = 'information'), 
                    method = "class")




#removing the country because the levels do not match
#test_set[test_set == " France"] <- NA
#test_set[test_set == " Laos"] <- NA
#test_set[test_set == " Peru"] <- NA
#test_set[test_set == " Poland"] <- NA
#test_set[test_set == " Thailand"] <- NA
#test_set <- na.omit(test_set)


#plotting model
fancyRpartPlot(IGregressor, caption = "Information Gain")
IGpred = predict(IGregressor,test_set, type="class")

# confusion matrix and accuracy
confusionMatrix(IGpred, factor(test_set$X), positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)

# tabulating to calculate precision and recall
xtab <- table(IGpred, factor(test_set$X))
precision_lessthan_50 <- xtab[1]/(xtab[1]+xtab[3])
recall_lessthan_50 <- xtab[1]/(xtab[1]+xtab[2])
precision_lessthan_50
recall_lessthan_50

#calculating F1 score
F1_Score(factor(test_set$X), IGpred, positive = NULL)


#Split using GINI impurity
GINIregressor = rpart(formula = X ~ .,
                      data = train_set,
                      method = "class")
fancyRpartPlot(GINIregressor, caption = "GiniRegressor")
plot(GINIregressor)
text(GINIregressor)
GINIpred = predict(GINIregressor,test_set)
fancyRpartPlot(IGregressor, caption = "predicted GINI")

confusionMatrix(GINIpred, train_set, positive = NULL, 
                dnn = c("Prediction", "Reference"), 
                prevalence = NULL)
table(train_set)
table(GINIpred)


