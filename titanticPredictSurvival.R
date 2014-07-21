setwd('/Users/zurich/Google Drive/FactMachine-SITE/FM-Site-STRUCTURE/10-CART/code/CART-TitanicPrediction')
#rm(list=setdiff(ls(), "mTitanicAll"))
rm(list=ls(all=TRUE))
library(tree)
library(RCurl)

#mTitanicAll <- read.csv('mTitanicAge.csv',  header = TRUE, sep = ",")

#need to make sure that the dependant variable is a factor


x <- getURL("https://raw.githubusercontent.com/thefactmachine/CART-TitanicPrediction/master/mTitanicAge.csv")

mTitanicAll <- read.csv(text = x,  header = TRUE, sep = ",")

mTitanicAll$X <- NULL
mTitanicAll$survived <- as.factor(mTitanicAll$survived)



#load('mTitanicAge.RData')
#lets just do a check for any weird variables
table(complete.cases(mTitanicAll))
#this ensures things are reproducable
set.seed(13)

#baseline metric. 809 people died, 500 lived.
#pick a person at random and predict they died and you will be correct 61.8% 
#split into training and test. Vector of 1100 numbers, range = 1:1309
trainSet <- sample(1:nrow(mTitanicAll), 1100)
#Following evaluates to 33.02. Checking training set includes same rows. 
#mean(mTitanicAll[trainSet, "fare"])
#this version excludes title
treeTrain <- tree(survived~. -title , mTitanicAll, subset = trainSet)
#treeTrain <- tree(survived~. , mTitanicAll, subset = trainSet)

#now run cross validation on the training set
#cvTreeTrain <- cv.tree(treeTrain, FUN=prune.misclass)
#plot the results
#par(mfrow = c(1,1))
#plot(cvTreeTrain$size, cvTreeTrain$dev, type = 'b')


#lets use nodes = 6. 
mTitanicTest <- mTitanicAll[-trainSet, ]
mTitanicTestPred <- predict(treeTrain, newdata = mTitanicTest, type = "class")
tab <- table(mTitanicTestPred, mTitanicTest$survived)
#print accuracy
(tab[1] + tab[4]) / sum(tab)



plot(treeTrain)
text(treeTrain, pretty = 0)
train <- mTitanicAll[trainSet,]


#fit the tree to everything
#treeAll <- tree(survived~. -title , mTitanicAll)
#plot(treeAll)
#text(treeAll, pretty = 0)
