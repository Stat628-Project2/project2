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

# users' topics similarities

topic <- read.csv("topics.csv", colClasses = "character")
topic_similarity <- matrix(ncol=dim(topic)[1],nrow=dim(topic)[1])
for (i in 1:(dim(topic)[1]-1)) {
    for (j in (i+1):dim(topic)[1]) {
        comb <- length(unique(unlist(unname(c(topic[i,-1],topic[j,-1])))))
        inters <- length(intersect(as.vector(as.matrix(unname(topic[i,-1]))),as.vector(as.matrix(unname(topic[j,-1])))))
        topic_similarity[i,j] <- inters/comb
    }
}

for (i in 1:dim(topic)[1]) {
    topic_similarity[i,i] <- 1
}

write.csv(topic_similarity, "topic_similarity.csv", row.names=FALSE)


# determine alpha, beta
agesim<- read.csv("age_similarity.csv")
occsim<-read.csv("occupation_similarity.csv")
gensim<-read.csv("sex_similarity.csv")
diag(agesim) <- 0
diag(occsim) <- 0
diag(gensim) <- 0
write.csv(agesim, "age_similarity.csv", row.names=FALSE)
write.csv(occsim, "occupation_similarity.csv", row.names=FALSE)
write.csv(gensim, "sex_similarity.csv", row.names=FALSE)

agesim[1:3,1:3]
set.seed(11)
demosim_sample<-sample(1:dim(agesim)[1],size=1)
sim_u<-as.numeric()
sample_rating <- data %>%
    filter(user_id == demosim_sample) %>%
    select(user_id, item_id, rating) %>%
    unique()
p_a_bar <- mean(sample_rating$rating)
K=60 # 10 is too small all sim=1, then no difference of choosing different alpha and beta

p_b_bar <- numeric(length=K)
S_a <- matrix(ncol=dim(item_attr)[1],nrow=1)
data_mt <- as.numeric(data$movie_title)
data$item_id <- data_mt
mae_test<-matrix(ncol=8,nrow=8)
mae_test<-as.data.frame(mae_test)
colnames(mae_test)<-c(paste0("alpha=",(1:8)/10))
rownames(mae_test)<-c(paste0("beta=",(1:8)/10))
for(p in 1:8){
    for(q in 1:(10-p-1)){
        alpha<-p/10
        beta<-q/10
        # sample users' similarity with other users
        # select the top K users based on similarity
        for(k in 1:dim(agesim)[1]){
            sim_u[k]<-alpha*agesim[demosim_sample,k]+beta*gensim[demosim_sample,k]+(1-alpha-beta)*occsim[demosim_sample,k]
            neighbor_ind <- order(sim_u,decreasing=TRUE)[1:K]
        }
        # calculate p_b_bar for neighboring users
        for (i in 1:length(neighbor_ind)) {
            neigh_rating <- data %>%
                filter(user_id == neighbor_ind[i]) %>%
                select(user_id, item_id, rating) %>%
                unique()
            p_b_bar[i] <- mean(neigh_rating$rating)
        }
        # calculate S_a
        for (h in 1:dim(item_attr)[1]) {
            d <- data %>%
                filter(user_id %in% neighbor_ind) %>%
                select(item_id, user_id, rating) %>%
                unique()
            # select id for neighboring users who have ratings for item h
            neighbor_ind_r <- d %>% 
                filter(item_id == h) %>%
                select(user_id) %>%
                unname() %>%
                as.matrix() %>%
                as.numeric()
            if (length(neighbor_ind_r)==0) {
                S_a[h] <- p_a_bar
            } else {
                # select p_b_h
                d_bh <- d %>%
                    filter(user_id %in% neighbor_ind_r, item_id==h) %>%
                    arrange(user_id)
                # calculate p_b_bar_filter
                p_b_bar_filter <- data %>%
                    filter(user_id %in% neighbor_ind_r) %>%
                    select(user_id, item_id, rating) %>%
                    unique() %>%
                    group_by(user_id) %>%
                    mutate(pbbar=mean(rating)) %>%
                    select(user_id, pbbar) %>%
                    unique() %>%
                    arrange(user_id)
                
                numerator = sim_u[sort(neighbor_ind_r)] %*% (d_bh$rating-p_b_bar_filter$pbbar)
                S_a[h] <- p_a_bar + numerator/sum(sim_u[neighbor_ind])
            }
        }
        #fill unrated items rating with 0 (use all ratings,the difference seems not clear, so only use ratings that the sample user rated)
        #sample_rating_full<-matrix(nrow=length(S_a),ncol=3)
        #sample_rating_full<-as.data.frame(sample_rating_full)
        #colnames(sample_rating_full)<-c("use_id","item_id","rating")
        #for (i in 1:length(S_a)) {
        #if (!(i %in% sample_rating$item_id)){
        #sample_rating_full[i,1] <- demosim_sample
        #sample_rating_full[i,2] <- i
        #sample_rating_full[i,3] <- 0
        # }
        #else{
        #sample_rating_full[i,1] <- demosim_sample
        #sample_rating_full[i,2] <- i
        #sample_rating_full[i,3] <- sample_rating$rating[sample_rating$item_id==i]
        #}
        #}
        ind_r<-sample_rating$item_id
        pred_r<-S_a[ind_r]
        real_r<-sample_rating$rating
        #calculate mae
        mae=sum(abs(real_r-pred_r))/length(real_r)
        mae_test[q,p]=mae
    }
}
write.csv(mae_test,"mae_test.csv")

mae_test_demo <- read.csv("mae_test.csv")
min(mae_test_demo[,-1],na.rm=TRUE)
which(mae_test_demo == min(mae_test_demo[,-1],na.rm=TRUE), arr.ind=TRUE)

beta_demo <- 0.1
alpha_demo <-0.1
sim_demo <- matrix(ncol=length(user_demo$user_id), nrow=length(user_demo$user_id))
for (i in 1:length(user_demo$user_id)) {
    for (j in 1:length(user_demo$user_id)) {
        sim_demo[i,j] <- alpha_demo*agesim[i,j]+beta_demo*gensim[i,j]+(1-alpha_demo-beta_demo)*occsim[i,j]
    }
}

sim_demo <- as.data.frame(sim_demo)
colnames(sim_demo) <- paste0("U",1:length(user_demo$user_id))
write.csv(sim_demo,"similarity_demo.csv", row.names=FALSE)

# calculate users' overall similarities 

sim_rating <- read.csv("similarity_user.csv")

na.ind <- as.data.frame(which(is.na(sim_rating),arr.ind=TRUE))

for (i in 1:dim(na.ind)[1]) {
    if (is.na(sim_rating[na.ind[i,2], na.ind[i,1]])) {
        sim_rating[na.ind[i,1], na.ind[i,2]] = 0
    } else {
        sim_rating[na.ind[i,1], na.ind[i,2]] = sim_rating[na.ind[i,2], na.ind[i,1]]
    }
}

set.seed(27)

overallsim_sample<-sample(1:dim(agesim)[1],size=1)

sim_zi<-as.numeric()

sample_rating_zi <- data %>%
    filter(user_id == overallsim_sample) %>%
    select(user_id, item_id, rating) %>%
    unique()

p_a_bar_zi <- mean(sample_rating_zi$rating)

K=60 

p_b_bar_zi <- numeric(length=K)
S_b <- matrix(ncol=dim(item_attr)[1],nrow=1) # rating of user b for all items

mae_test_zi <- matrix(ncol=8,nrow=8)
mae_test_zi <- as.data.frame(mae_test_zi)
colnames(mae_test_zi) <- c(paste0("alpha=",(1:8)/10))
rownames(mae_test_zi) <- c(paste0("beta=",(1:8)/10))

topic_similarity <- read.csv("topic_similarity.csv")
sim_topics <- topic_similarity + t(topic_similarity)
diag(sim_topics) <- 0

for(p in 1:8){
    for(q in 1:(10-p-1)){
        alpha <- p/10
        beta <- q/10
        # sample users' similarity with other users
        # select the top K users based on similarity
        for(k in 1:length(user_demo$user_id)){
            sim_zi[k] <- alpha*sim_demo[overallsim_sample,k]+beta*sim_topics[overallsim_sample,k]+(1-alpha-beta)*sim_rating[overallsim_sample,k]
            neighbor_ind_zi <- order(sim_zi,decreasing=TRUE)[1:K]
        }
        # calculate p_b_bar for neighboring users
        for (i in 1:length(neighbor_ind_zi)) {
            neigh_rating_zi <- data %>%
                filter(user_id == neighbor_ind_zi[i]) %>%
                select(user_id, item_id, rating) %>%
                unique()
            p_b_bar_zi[i] <- mean(neigh_rating_zi$rating)
        }
        # calculate S_b
        for (h in 1:dim(item_attr)[1]) {
            d <- data %>%
                filter(user_id %in% neighbor_ind_zi) %>%
                select(item_id, user_id, rating) %>%
                unique()
            # select id for neighboring users who have ratings for item h
            neighbor_ind_r <- d %>% 
                filter(item_id == h) %>%
                select(user_id) %>%
                unname() %>%
                as.matrix() %>%
                as.numeric()
            if (length(neighbor_ind_r)==0) {
                S_b[h] <- p_a_bar_zi
            } else {
                # select p_b_h
                d_bh <- d %>%
                    filter(user_id %in% neighbor_ind_r, item_id==h) %>%
                    arrange(user_id)
                # calculate p_b_bar_filter
                p_b_bar_filter <- data %>%
                    filter(user_id %in% neighbor_ind_r) %>%
                    select(user_id, item_id, rating) %>%
                    unique() %>%
                    group_by(user_id) %>%
                    mutate(pbbar=mean(rating)) %>%
                    select(user_id, pbbar) %>%
                    unique() %>%
                    arrange(user_id)
                
                numerator = sim_zi[sort(neighbor_ind_r)] %*% (d_bh$rating-p_b_bar_filter$pbbar)
                S_b[h] <- p_a_bar + numerator/sum(sim_u[neighbor_ind_zi])
            }
        }
        ind_r_zi <- sample_rating_zi$item_id
        pred_r_zi <- S_b[ind_r]
        real_r_zi <- sample_rating_zi$rating
        #calculate mae
        mae_zi <- sum(abs(real_r_zi-pred_r_zi))/length(real_r_zi)
        mae_test_zi[q,p]=mae_zi
    }
}
write.csv(mae_test_zi,"mae_test_zi.csv")

min(mae_test_zi,na.rm=TRUE)
which(mae_test_zi == min(mae_test_zi,na.rm=TRUE), arr.ind=TRUE)

beta_total <- 0.5
alpha_total <-0.4
sim_total <- matrix(ncol=length(user_demo$user_id), nrow=length(user_demo$user_id))
for (i in 1:length(user_demo$user_id)) {
    for (j in 1:length(user_demo$user_id)) {
        sim_total[i,j] <- alpha_total*sim_demo[i,j]+beta_total*sim_topics[i,j]+(1-alpha_total-beta_total)*sim_rating[i,j]
    }
}

sim_total <- as.data.frame(sim_total)
colnames(sim_total) <- paste0("U",1:length(user_demo$user_id))
write.csv(sim_total,"similarity_hybrid_final.csv", row.names=FALSE)

# predict for the old users

set.seed(29)
presim_old <- sample(1:length(user_demo$user_id),size=3)
presim_old <- sort(presim_old)

presim_data <- data %>%
    filter(user_id %in% presim_old) %>%
    select(user_id, rating) %>%
    unique() 

P_a_old <- presim_data %>%
    group_by(user_id) %>%
    summarise(ave_rating = mean(presim_data$rating))


# calculate p_b_bar for neighboring users

P_b_old <- data %>%
    filter(user_id %in% presim_old) %>%
    select(user_id, item_id, rating) %>%
    unique() %>%
    group_by(user_id) %>%
    summarise(ave_rating=mean(rating))

# calculate S_old
S_old <- matrix(ncol=dim(item_attr)[1],nrow=length(presim_old))

neighbor_ind_old <- matrix(ncol=K,nrow=length(presim_old))
for (i in 1:length(presim_old)) {
    neighbor_ind_old[i,] <- order(sim_total[presim_old[i],],decreasing=TRUE)[1:K]
}

for (i in 1:length(presim_old)) {
    for (h in 1:dim(item_attr)[1]) {
        d <- data %>%
            filter(user_id %in% neighbor_ind_old[i,]) %>%
            select(item_id, user_id, rating) %>%
            unique()
        # select id for neighboring users who have ratings for item h
        neighbor_ind_r <- d %>% 
            filter(item_id == h) %>%
            select(user_id) %>%
            unname() %>%
            as.matrix() %>%
            as.numeric()
        if (length(neighbor_ind_r)==0) {
            S_old[i,h] <- P_a_old$ave_rating[i]
        } else {
            # select p_b_h
            d_bh <- d %>%
                filter(user_id %in% neighbor_ind_r, item_id==h) %>%
                arrange(user_id)
            numerator = as.matrix(sim_total[presim_old[i],sort(neighbor_ind_r)]) %*% as.matrix(d_bh$rating-P_b_old$ave_rating[i])
            S_old[i,h] <- P_a_old$ave_rating[i] + numerator/sum(sim_total[presim_old[i],sort(neighbor_ind_r)])
        }
    }
}

# Recommend 5 films for each user
movie_recommend <- list()
for (i in 1:length(presim_old)) {
    item_recommend <- which(S_old[i,] %in% sort(S_old[i,], decreasing=TRUE)[1:10])
    movie_recommend[[i]] <- as.character(item_uniq$movie_title[item_uniq$item_id %in% item_recommend])
}
 
# predict for the new users






