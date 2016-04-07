library(dplyr)
library(ggplot2)
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

age_division <- read.csv("age_group_division.csv")
seg_p <- unique(age_division$point)[-11]
seg_y <- seq(0,9)
seg <- data.frame(seg_p, seg_y)

p <- ggplot(data=age_division, aes(x=Age, y=Age.Group)) +
    geom_segment(aes(yend=Age.Group, xend=xend)) +
    scale_x_continuous(limits=c(min(age_division$Age),max(age_division$Age)),breaks=unique(age_division$xend)) + 
    scale_y_continuous(limits=c(min(age_division$Age.Group),max(age_division$Age.Group)),breaks=unique(age_division$Age.Group))

p + geom_point(data=seg, aes(y=seg_y,x=seg_p), size=1, shape=1)
    
age <- data.frame(u.age$age)
u.age$age_group <- apply(age,1,age_group_det)

u.num <- dim(u.age)[1]

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

gamma=5
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
male <- gender_topics %>%
    filter(gender == "M") %>%
    melt(id="gender") %>%
    mutate(gender = as.character(gender)) %>%
    select(gender, value) 
female <- gender_topics %>%
    filter(gender == "F") %>%
    melt(id="gender") %>%
    mutate(gender = as.character(gender)) %>%
    select(gender, value) 
    
gender_features[1,] <- c("F", levels(data$genre)[order(table(female),decreasing=TRUE)][1:5])
gender_features[2,] <- c("M", levels(data$genre)[order(table(male),decreasing=TRUE)][1:5])

gender_features<-as.data.frame(gender_features)
colnames(gender_features) <-c("gender", paste0("topic",1:5))

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
                S_b[h] <- p_a_bar + numerator/sum(sim_zi[neighbor_ind_zi])
            }
        }
        ind_r_zi <- sample_rating_zi$item_id
        pred_r_zi <- S_b[ind_r_zi]
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
    summarise(ave_rating = mean(presim_data$rating)) %>%
    ungroup()


# calculate p_b_bar for neighboring users

P_b_old <- data %>%
    filter(user_id %in% presim_old) %>%
    select(user_id, item_id, rating) %>%
    unique() %>%
    group_by(user_id) %>%
    summarise(ave_rating=mean(rating))

# calculate S_old
S_old <- matrix(ncol=dim(item_attr)[1],nrow=length(presim_old))

sim_total <- read.csv("similarity_hybrid_final.csv")
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

# Recommend 10+ films for each user
item_uniq <- data %>%
    select(item_id,movie_title) %>%
    unique() %>%
    arrange(item_id)
movie_recommend <- list()
for (i in 1:length(presim_old)) {
    rating_item_id<- data %>%
        filter(user_id %in% presim_old[i]) %>%
        select(user_id, rating,item_id) %>%
        unique() %>%
        select(item_id)
    item_recommend <- which(S_old[i,] %in% sort(S_old[i,-rating_item_id$item_id], decreasing=TRUE)[1:10])
    movie_recommend[[i]] <- as.character(item_uniq$movie_title[item_uniq$item_id %in% item_recommend])
}
 
# predict for the new users

set.seed(37)
presim_new <- sample(1:length(user_demo$user_id),size=3)
presim_new <- sort(presim_new)

age_features <- read.csv("age_features.csv", colClasses="character")
occupation_features <- read.csv("occupation_features.csv", colClasses="character")
gender_features <- read.csv("gender_features.csv", colClasses="character")

new_user <- data %>%
    filter(user_id %in% presim_new) %>%
    select(user_id, age, gender, occupation) %>%
    unique()

write.csv(new_user, "new_user.csv", row.names=FALSE)
head(new_user)

sim_zx <- matrix(ncol=dim(topic)[1],nrow=dim(new_user)[1])

for (i in 1:dim(new_user)[1]) {
    age_g <- age_group_det(new_user[i,]$age)
    age_topic <- age_features %>%
        filter(age==age_g)
    gender_topic <- gender_features[gender_features$gender == as.character(new_user[i,]$gender),] 
    occupation_topic <- occupation_features[occupation_features$occupation == as.character(new_user[i,]$occupation),] 
    topics_new_user <- intersect(c(as.vector(as.matrix(age_topic[,c(-1,-12)])),as.vector(as.matrix(gender_topic[,-1]))),as.vector(as.matrix(occupation_topic[,-1])))
    
    for (j in 1:dim(sim_zx)[2]) {
        if (j == i) {
            sim_zx[i,j] = 0
        } else {
            neigh_user <- data %>%
                select(user_id, age, gender, occupation) %>%
                unique() %>%
                arrange(user_id)
            comb <- length(unique(unlist(unname(c(topics_new_user,topic[j,-1],new_user[i,-1],neigh_user[j,-1])))))
            inters <- length(intersect(c(new_user[i,-1],as.vector(as.matrix(unname(topics_new_user)))),c(neigh_user[j,-1],as.vector(as.matrix(unname(topic[j,-1]))))))
            sim_zx[i,j] <- inters/comb
        }
    }
}

S_new <- matrix(nrow=length(presim_new),ncol=dim(item_attr)[1])
neighbor_ind<-matrix(nrow=length(presim_new),ncol=K)
for(i in 1:length(presim_new)){
    neighbor_ind[i,] <- order(sim_zx[i,],decreasing=TRUE)[1:K]
}
data$item_id<-as.numeric(data$movie_title)
for(i in 1:length(presim_new)){
    for (h in 1:dim(item_attr)[1]) {
        d <- data %>%
            filter(user_id %in% neighbor_ind[i,]) %>%
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
            S_new[i,h] <- 0
        } else {
            # select p_b_h
            d_bh <- d %>%
                filter(user_id %in% neighbor_ind_r, item_id==h) %>%
                arrange(user_id)
            
            userid<-as.numeric(d_bh$user_id)
            
            numerator = sim_zx[i,userid] %*% (d_bh$rating)
            S_new[i,h] <-  numerator/sum(sim_zx[i,neighbor_ind[i,]])
        }
    }
}
write.csv(S_old,"rating_old.csv",row.names=FALSE)
write.csv(S_new,"rating_new.csv",row.names=FALSE)

# Recommend 10+ films for each user

movie_recommend_new <- list()
for (i in 1:length(presim_new)) {
    rating_item_id<- data %>%
        filter(user_id %in% presim_new[i]) %>%
        select(user_id, rating,item_id, movie_title) %>%
        unique() %>%
        arrange(rating)
    item_recommend <- which(S_new[i,] %in% sort(S_new[i,], decreasing=TRUE)[1:10])
    movie_recommend[[i]] <- as.character(item_uniq$movie_title[item_uniq$item_id %in% item_recommend])
}

# Compare algorithms using MAE

mae_sim_new <- matrix(nrow=3,ncol=2)
mae_sim_new <- as.data.frame(mae_sim_new)
colnames(mae_sim_new) <- c("user_id","mae")
mae_sim_new$user_id <- presim_new
for(i in 1:3){
    sample_rating <- data %>%
        filter(user_id == presim_new[i]) %>%
        select(user_id, item_id, rating) %>%
        unique()
    ind_r <- sample_rating$item_id
    pred_r < -S_a[i,ind_r]
    real_r  <-sample_rating$rating
    #calculate mae
    mae = sum(abs(real_r-pred_r))/length(real_r)
    mae_sim_new[i,2]=mae
}
write.csv(mae_sim_new,"mae_new.csv")

pre_rating<-read.csv("rating_old.csv")
mae_sim_old<-matrix(nrow=3,ncol=2)
mae_sim_old<-as.data.frame(mae_sim_old)
colnames(mae_sim_old)<-c("user_id","mae")
mae_sim_old$user_id<-presim_old
for(i in 1:3){
    sample_rating <- data %>%
        filter(user_id == presim_old[i]) %>%
        select(user_id, item_id, rating) %>%
        unique()
    ind_r<-sample_rating$item_id
    pred_r<-pre_rating[i,ind_r]
    real_r<-sample_rating$rating
    #calculate mae
    mae=sum(abs(real_r-pred_r))/length(real_r)
    mae_sim_old[i,2]=mae
}
write.csv(mae_sim_old,"mae_old.csv")


mov<-read.csv("ratingMatrix.cs")
rat<-read.csv("ratingMatrix.csv",header=TRUE)
pre<- rat[presim_old,-1]
for (i in 1:dim(pre)[1]){
    for (j in 1:dim(pre)[2]){
        if (is.na(pre[i,j]))
            pre[i,j]=0
    }
}
x<-colnames(rat)[-1]
y<-x[order(x)]
y<-read.csv("y.csv",colClasses ="character",header=TRUE)
y[1644,2]<-as.character(ind[1,1])
z<-y[,2]
pre1<-pre[,order(x)]
pre2<-pre1[,order(z)]
x<-colnames(pre)
ind <- data %>% 
    select(movie_title,item_id) %>%
    unique()

mae_r<-matrix(nrow=3,ncol=2)
mae_r<-as.data.frame(mae_r)
colnames(mae_r)<-c("user_id","mae")
mae_r$user_id<-presim_old
for(i in 1:3){
    sample_rating <- data %>%
        filter(user_id == presim_old[i]) %>%
        select(user_id, item_id, rating,movie_title) %>%
        unique()
    ind_r<-sample_rating$item_id
    pred_r<-pre2[i,ind_r]
    real_r<-sample_rating$rating
    #calculate mae
    mae=sum((real_r-pred_r)^2)/length(real_r)
    mae_r[i,2]=mae
}
write.csv(mae_r,"mae_r.csv")
mae_sim_old<-read.csv("mae_old.csv",header=T)[,-1]
mae_r<-read.csv("mae_r.csv",header=T)[,-1]
mae<-rbind(mae_sim_old,mae_r)
mae$group<-c(rep("MAE_Hybrid",3),rep("MAE_UBCF",3))
mae_r$maeh<-mae_sim_old$mae
library(ggplot2)
mae$user_id<-as.character(mae$user_id)
a<-ggplot(mae,aes(x=user_id,y=MAE,fill=type))+geom_bar(stat="identity",position="dodge")
ggsave("plot.png",width=5,height=5)

mae_new<-read.csv("mae_new.csv",header=T)[,-1]
colnames(mae_new)[2]<-"MAE"
mae_new$user_id<-as.character(mae_new$user_id)
b<-ggplot(mae_new,aes(x=user_id,y=MAE))+geom_bar(stat="identity",size=3,position="dodge")
ggsave("plot1.png",width=5,height=5)

mae_final <- read.csv("mae_test_final.csv")
rnames <- mae_final[,1]
mae_final <- mae_final[,-1]
rownames(mae_final) <- rnames

na_ind <- which(is.na(mae_final),arr.ind=TRUE)
for (i in 1: dim(na_ind)[1]) {
    mae_final[na_ind[i,1], na_ind[i,2]] <- 99
}

min_mae_test <- apply(mae_final,2, min)

plot_mae_test <- data.frame(min_MAE=unname(min_mae_test))

for(i in 1:length(min_mae_test)) {
    ind <- which(mae_final == plot_mae_test$min_MAE[i], arr.ind=TRUE)
    b <- as.character(rnames[ind[,1]])
    a <- as.character(colnames(mae_final)[ind[,2]])
    a_paste <- paste0("alpha=0.", unlist(strsplit(a, split=""))[length(unlist(strsplit(a, split="")))])
    plot_mae_test$parameter[i] = paste0(a_paste, ",\n", b)
}
plot_mae_test$x <- seq(1,8)

ggplot(plot_mae_test, aes(x=x,y=min_MAE)) +
    geom_point(color="blue") +
    geom_line() +
    scale_x_continuous(limits=c(0.5,8.5), breaks=seq(1,8), labels=plot_mae_test$parameter)


