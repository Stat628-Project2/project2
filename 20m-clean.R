library(recommenderlab)
library(dplyr)

#Author: Kejia Shi
#Note: This script creates a giant rating matrix for specific given data subset of users and movies.

#Remove previous data space
rm(list=ls())

setwd("/Users/KJ/OneDrive/Projects/628Recommender/data/")

#Build a giant rating matrix
##load movie list and rating list
dataTbl <- read.csv(file="ml-20m/reduced.csv", header = TRUE, sep = ",")
itemTbl <- read.csv(file="ml-20m/movies.csv", header = TRUE, sep = ",")
dataTbl <- subset(dataTbl, select = -c(timestamp))

userNum <- dataTbl[nrow(dataTbl), 1]
itemNum <- nrow(itemTbl)

#Create itemId instead of using movingID
itemId <- as.numeric(rownames(itemTbl))
itemTbl <- cbind(itemId,itemTbl)
##add such column in rating table as well
itemTbl_temp <- itemTbl[,-c(3,4)]
dataTbl <- left_join(dataTbl, itemTbl_temp, by = "movieId")

#create augmented rating table
ratings_aug <- matrix(NA, nrow = userNum * itemNum, ncol = 3)
colnames(ratings_aug) <- c("userId", "itemId", "user.item")
ratings_aug[,2] <- row(ratings_aug)[,2] %% itemNum
ratings_aug[,1] <- (row(ratings_aug)[,1]-1) %/% itemNum + 1
ratings_aug[,3] <- row(ratings_aug)[,3]
dataTbl <- dataTbl %>% mutate(user.item=(userId-1)*itemNum+itemId)
ratings_aug <- data.frame(ratings_aug)
ratings_aug <- left_join(ratings_aug, dataTbl, by="user.item")
ratings_aug <- ratings_aug[, -c(3,4,5,7)]
colnames(ratings_aug) <- c("userId", "itemId", "rating")

#Build a rating matrix
##setup an empty matrix
ratings <- matrix(NA, nrow = userNum, ncol = itemNum)

#rating <- ratings_aug$rating


colnames(ratings) <- col(ratings)[1,]
rownames(ratings) <- row(ratings)[,1]
##copying ratings
ratings[,] <- ratings_aug$rating[(row(ratings)-1)*itemNum+dataTbl$itemId]
#title <- itemTbl$title
#colnames(ratings) <- title

#ratings[( as.numeric(rownames(ratings)) -1 ) * itemNum + as.numeric(colnames(ratings))] <- ratings_aug$rating[( as.numeric(rownames(ratings)) -1 ) * itemNum + as.numeric(colnames(ratings))]
#ratings[as.numeric(rownames(ratings)-1) * itemNum + as.numeric(colnames(ratings))] <- ratings_aug$rating
ratings[ratings_aug$userId, ratings_aug$itemId] <- ratings_aug$rating[(ratings_aug$userId-1)*itemNum+ratings_aug$itemId]

