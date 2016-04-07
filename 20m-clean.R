library(recommenderlab)
library(dplyr)

#Author: Kejia Shi
#Note: This script creates a giant rating matrix for specific given data subset of users and movies.
#(Apr 6) V2.0 Update: shorten steps creating rating matrix significantly

#Remove previous data space
rm(list=ls())

setwd("/Users/KJ/OneDrive/Projects/628Recommender/data/")

#Build a giant rating matrix
##load movie list and rating list
dataTbl <- read.csv(file="ml-20m/reduced.csv", header = TRUE, sep = ",")
itemTbl <- read.csv(file="ml-20m/movies.csv", header = TRUE, sep = ",")
dataTbl <- subset(dataTbl, select = -c(timestamp))

ratings_ori <- dataTbl[complete.cases(dataTbl), ]
full <- setNames(with(ratings_ori, expand.grid(sort(unique(userId)), sort(unique(movieId)))),c("userId", "movieId"))
ratings_aug <- merge(full, ratings_ori, all = TRUE)
ratings <- matrix(ratings_aug$rating, nrow = length(unique(ratings_aug$userId)), byrow = TRUE)

rownames(ratings) <- unique(ratings_aug$userId)
colId <- unique(ratings_aug$movieId)
tmp <- data.frame(movieId=colId, flag=1)
tmp <- inner_join(itemTbl, tmp, by = "movieId", copy = TRUE)
colnames(ratings) <- tmp$title

affinity.matrix<- as(ratings,"realRatingMatrix")
