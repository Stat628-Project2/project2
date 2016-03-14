library(dplyr)
data <- read.csv("unifiedMLDataMulti.csv", header=TRUE)
name <- names(data)
colnames(data) <- gsub("_", ".", name)
fname <- "MLData.csv"
write.csv(data, fname, row.names=FALSE)

dat <- read.csv(fname, header=TRUE)
length(dat)
type <- c("b","b","b","d","b","n","b","b","b")
des <- data.frame(V1=as.character(1:length(dat)),V2=names(dat),V3=type)
filename <- data.frame(V1=c(fname,"NA","2"), V2=rep("NA",3),V3=rep("NA",3))
des_file <- full_join(filename,des)
write.table(des_file,"description_file.txt",row.names=FALSE, col.names=FALSE,quote=FALSE)
  