rm(list=ls(all=TRUE))
setwd('/Users/zurich/Google Drive/FactMachine-SITE/FM-Site-STRUCTURE/10-CART/code/CART-TitanicPrediction')
#titanicData.csv has been previously saved.
#titanic3.tab has been previously saved (for python)
#titanicTitle.csv has been processed by python using titanic3.tab
dfTitanic3 <- read.csv('titanicData.csv', header = TRUE, sep = ",")



#export the titanic3 dataset, process in python and then import title file
#title file is a list of titles for each person Mr, Miss, Sir...etc
title <- read.csv('titanicTitle.csv', header = FALSE, sep = ",")

names(title) <- c("id", "title")
#merge the titles and main dataset together
mTitanic <- merge(dfTitanic3, title, by="row.names", all=TRUE)
#exclude the following columns
colsExclude <- c("boat","body","home.dest",
                 "ticket", "cabin", "name", "Row.names","id")
mTitanic <- mTitanic[, !(names(mTitanic) %in% colsExclude)]


#convert to ordered factor. 1st Class, 2nd Class...etc has a natural order
mTitanic$pclass <- 
    factor(mTitanic$pclass, levels = c("1st", "2nd", "3rd"), ordered = TRUE)

#create new feature Solo =1 if travelling solo
mTitanic$solo[mTitanic$sibsp == 0 & mTitanic$parch == 0] <- 1
mTitanic$solo[mTitanic$sibsp != 0 | mTitanic$parch != 0] <- 0
mTitanic$solo <- factor(mTitanic$solo)

#assign 2 missing embarked values to modal class
mTitanic$embarked[mTitanic$embarked == ""] <- "Southampton"
mTitanic$embarked[mTitanic$embarked == ""] <- "Southampton"

#there is one single NA...set it to the mean

mTitanic$fare[is.na(mTitanic$fare)] <- mean(mTitanic$fare, na.rm = TRUE)

#turn survived into factor
mTitanic$survived <- factor(mTitanic$survived)


write.csv(mTitanic, file = "titanicProcessed.csv", row.names = FALSE)



