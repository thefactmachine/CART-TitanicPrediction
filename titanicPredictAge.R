rm(list=ls(all=TRUE))
library(tree)
library(RCurl)
mTitanic <- read.csv('titanicProcessed.csv',  header = TRUE, sep = ",")

x <- getURL("https://raw.githubusercontent.com/thefactmachine/CART-TitanicPrediction/master/titanicProcessed.csv")
mTitanic <- read.csv(text = x,  header = TRUE, sep = ",")

#Partition into records with Age and Records without Age
mTitanicNoAge <- mTitanic[is.na(mTitanic$age),]
mTitanicAge <- mTitanic[!is.na(mTitanic$age),]

#Age records = 1046, NoAge records = 263. Total 1309
#Lets determine baseline figure
mean((mTitanicAge$age - mean(mTitanicAge$age))^2)
#MSE is 207.55. sqrt(MSE) = 14.406
#split into training and test. Vector of 900 numbers, range = 1:1046

set.seed(13)
trainSet <- sample(1:nrow(mTitanicAge), 900)
#CHECK following should be 26909.08
#sum(mTitanicAge[trainSet, "age"])

treeTrain <- tree(age~. -survived, mTitanicAge, subset = trainSet)
#now run cross validation on the training set
cvTreeTrain <- cv.tree(treeTrain)
#plot the results
par(mfrow = c(1,1))
plot(cvTreeTrain$size, cvTreeTrain$dev, type = 'b')

#select the tree: greater than 4 nodes doesn't add much
pruneTreeTrain <- prune.tree(treeTrain, best = 4)

#lets evaluate on the test set, to get an estimate of error
agePredictTest <- predict(pruneTreeTrain, newdata = mTitanicAge[-trainSet,])
ageTest <- mTitanicAge[-trainSet, "age"]

mean((ageTest - agePredictTest)^2)
#MSE is 98.37 or 9.91

#or using the mean, it is:
mean((ageTest - mean(mTitanicAge$age))^2)
#mean results in 197.91. 


#lets do this on the training set
agePredictTrain <- predict(pruneTreeTrain, newdata = mTitanicAge[trainSet,])
mean((mTitanicAge[trainSet,"age"] - agePredictTrain) ^2)
#result is 125


#We have our estimate of error, now use all the data. to make predictions
treeAll <- tree(age~. -survived, mTitanicAge)
cvTreeAll <- cv.tree(treeAll)
#plot the results
par(mfrow = c(1,1))
plot(cvTreeAll$size, cvTreeAll$dev, type = 'b')
#still use 4 nodes
pruneTreeAll <- prune.tree(treeAll, best = 4)
#predictions for observations without age
agePredictAll <- predict(pruneTreeAll, newdata = mTitanicNoAge)

plot(pruneTreeAll)
text(pruneTreeAll, pretty = 0)

#we have the predictions...lets stick the data sets together
mTitanicNoAge$age <- agePredictAll
mTitanicAll <- rbind(mTitanicNoAge, mTitanicAge)

#save(mTitanicAll, file = "mTitanicAge.RData")
write.csv(mTitanicAll, file = "mTitanicAge.csv")
