library(dplyr)
library(reshape2)

data <- read.csv("unifiedMLDataMulti.csv", header=TRUE)
u_num <- unique(data$user_id)

## users' demographic characteristics

# age
u.age <- data %>% 
    select(user_id, age) %>%
    distinct() %>%
    arrange(user_id)
age_group_det <- function(num) {
    if (num %in% 0:7) {
        return(0)
    } else if (num %in% 8:13) {
        return(1)
    } else if (num %in% 14:16) {
        return(2)
    } else if (num %in% 17:19) {
        return(3)
    } else if (num %in% 20:25) {
        return(4)
    } else if (num %in% 26:35) {
        return(5)
    } else if (num %in% 36:45) {
        return(6)
    } else if (num %in% 46:50) {
        return(7)
    } else if (num %in% 51:60) {
        return(8)
    } else if (num %in% 61:70) {
        return(9)
    } else {
        return(10)
    }
}

age <- data.frame(u.age$age)
u.age$age_group <- apply(age,1,age_group_det)

u.num <- dim(u.sex)[1]

u.age.group <- u.age %>% 
    select(user_id, age_group) %>%
    distinct() %>%
    arrange(user_id)

age_similarity <- as.data.frame(matrix(rep(NA, times=u.num*u.num),ncol=u.num))
for (i in 1:u.num) {
    for (j in 1:u.num) {
        if (u.age.group$age_group[i] == u.age.group$age_group[j]) {
            age_similarity[i,j] = 1
        } else {
            age_similarity[i,j] = 1/abs(u.age.group$age_group[i]-u.age.group$age_group[j])
        }
    }
}
write.csv(age_similarity, "age_similarity.csv", row.names=FALSE)

# sex
u.sex <- data %>% 
    select(user_id, gender) %>%
    distinct() %>%
    arrange(user_id)

sex_similarity <- as.data.frame(matrix(rep(0, times=u.num*u.num),ncol=u.num))
for (i in 1:u.num) {
    for (j in 1:u.num) {
        if (u.sex$gender[i] == u.sex$gender[j]) {
            sex_similarity[i,j] = 1
        }
    }
}
write.csv(sex_similarity, "sex_similarity.csv", row.names=FALSE)

# occupation

u.occupation <- data %>% 
    select(user_id, occupation) %>%
    distinct() %>%
    arrange(user_id)

occupation_similarity <- as.data.frame(matrix(rep(0, times=u.num*u.num),ncol=u.num))
for (i in 1:u.num) {
    for (j in 1:u.num) {
        if (u.occupation$occupation[i] == u.occupation$occupation[j]) {
            occupation_similarity[i,j] = 1
        }
    }
}
write.csv(occupation_similarity, "occupation_similarity.csv", row.names=FALSE)

## users' preference

# items' features matrix
data_items <- data[,2:3]
dat_items <- data_items %>% 
    unique()

mt <- as.numeric(dat_items$movie_title)
dat_items$item_id <- mt
head(dat_items)
dat_items$value <- rep(1,times=dim(dat_items)[1])
item_attr <- dcast(dat_items, item_id~genre)
head(item_attr)

write.csv(item_attr,"item_attribute.csv",row.names=FALSE)

# users' features rating total

head(data)

user_genre_rating <- data %>%
    select(user_id, genre, rating)

r_total <- dcast(user_genre_rating,user_id~genre,sum)
write.csv(r_total, "rating_total.csv", row.names=FALSE)

# users' features rating number

r_num <- dcast(user_genre_rating,user_id~genre,length)
write.csv(r_num, "rating_count.csv", row.names=FALSE)

# users' preference features

gamma=10
topics <- matrix(ncol=gamma+1,nrow=dim(r_total)[1])
for (i in 1: dim(r_total)[1]) {
    total_10 <- names(sort(r_total[i,-1],decreasing=TRUE)[1:gamma])
    num_10 <- names(sort(r_num[i,-1],decreasing=TRUE)[1:gamma])
    top <- c(r_total[i,1],intersect(total_10,num_10))
    if (length(top) == 11) {
        topics[i,] = top
    } else {
        topics[i,] = c(top,rep(NA, times=11-length(top)))
    }
}

head(topics)
colnames(topics) = c("user_id", paste0("topic",1:10))
write.csv(topics,"topics.csv",row.names=FALSE)

# users' features table

head(data)
user_demo <- data %>% 
    select(user_id, gender, age, occupation) %>%
    unique()

topics <- as.data.frame(topics)
topics$user_id <- as.numeric(topics$user_id)

users_features <- merge(user_demo,topics)

users_features$age_group <- apply(data.frame(users_features$age),1,age_group_det)
write.csv(users_features, "users_features.csv", row.names=FALSE)

##gender
gender_topics <- users_features[,c(2,5:14)]
head(gender_topics)

gender_features <- matrix(ncol=gamma+1,nrow=2)
names(gender_topics)
for (i in 2:11) {
    gender_features[1,i] <- names(which(table(gender_topics$gender,gender_topics[,i])[1,]==max(table(gender_topics$gender,gender_topics[,i])[1,])))
    gender_features[2,i] <- names(which(table(gender_topics$gender,gender_topics[,i])[2,]==max(table(gender_topics$gender,gender_topics[,i])[2,])))
}
gender_features[,1]=c("F","M")

gender_features<-as.data.frame(gender_features)
colnames(gender_features) <-c("gender", paste0("topic",1:10))

write.csv(gender_features,"gender_features.csv",row.names=FALSE)

## age_group
names(users_features)
age_topics <- users_features[,c(15,5:14)]
head(age_topics)

age_features <- matrix(ncol=gamma+1,nrow=11)
names(age_topics)
for(j in 1:11) {
    for (i in 2:11) {
        top <- names(which(table(age_topics$age_group,age_topics[,i])[j,]==max(table(age_topics$age_group,age_topics[,i])[j,])))
        if (length(top) == 1) {
            age_features[j,i] = top
        } else {
            age_features[j,i] = sample(top,1)
        }
        }
}

age_features[,1]=0:10

age_features<-as.data.frame(age_features)
colnames(age_features) <-c("age", paste0("topic",1:10))

write.csv(age_features,"age_features.csv",row.names=FALSE)

## occupation
names(users_features)
occupation_topics <- users_features[,c(4,5:14)]
head(occupation_topics)

occupation_uniq <- data %>%
    select(occupation) %>%
    distinct()

occupation_features <- matrix(ncol=gamma+1,nrow=dim(occupation_uniq)[1])
names(occupation_topics)
for(j in 1:dim(occupation_uniq)[1]) {
    for (i in 2:11) {
        top <- names(which(table(occupation_topics$occupation,occupation_topics[,i])[j,]==max(table(occupation_topics$occupation,occupation_topics[,i])[j,])))
        if (length(top) == 1) {
            occupation_features[j,i] = top
        } else {
            occupation_features[j,i] = sample(top,1)
        }
    }
}

occupation_features[,1]=levels(occupation_topics$occupation)

occupation_features<-as.data.frame(occupation_features)
colnames(occupation_features) <-c("occupation", paste0("topic",1:10))

write.csv(occupation_features,"occupation_features.csv",row.names=FALSE)


