## ----setup, include=FALSE---------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------------------------
courseRating <- read.csv("/Users/alireza/Desktop/DTI/Semester 1/Fundamentals of Applied Data Science/Optional assignment/Resources/courserating (1) (1).csv", header = T)
row.names(courseRating) <- courseRating[,1]
courseRating <- courseRating[,-1]
courseRating


## ---------------------------------------------------------------------------------------------------------------------
data <- courseRating[c(1,2,3,4,5,15),]
#data <- as.matrix(data)
data



## ---------------------------------------------------------------------------------------------------------------------
cor(t(data[]), use='pairwise.complete.obs')


## ---------------------------------------------------------------------------------------------------------------------
library(proxy)
data <- as.matrix(data)

result <- proxy::dist(data, method = "cosine", na.option = "mean")

print(result)



## ---------------------------------------------------------------------------------------------------------------------
library(lsa)
data2 <- as.matrix(courseRating)
data3 <- data2
data3[is.na(data3)] = 0

d <- cosine(data3)
print(d)


## ---------------------------------------------------------------------------------------------------------------------
library(recommenderlab)
d <- as(data2, "realRatingMatrix")
rec <- Recommender(d, "IBCF")
pred <- predict(rec, d, type = "ratings")
as(pred, "matrix")


## ---------------------------------------------------------------------------------------------------------------------
library(arules)
ct.df <- read.csv("/Users/alireza/Desktop/DTI/Semester 1/Fundamentals of Applied Data Science/Optional assignment/Resources/Coursetopics (1).csv")

ct.mat <- as(ct.df, "matrix")

ct.trans <- as(ct.mat, "transactions")



## ---------------------------------------------------------------------------------------------------------------------
itemFrequencyPlot(ct.trans)


## ---------------------------------------------------------------------------------------------------------------------
rules <- apriori(ct.trans, parameter = list(support = 0.01, confidence = 0.5, target = "rules"))

ruleshead <- inspect(head(sort(rules, by = "lift"), 10))

rules 

ruleshead


## ---------------------------------------------------------------------------------------------------------------------
farmingham.df <- read.csv("/Users/alireza/Desktop/DTI/Semester 1/Fundamentals of Applied Data Science/Optional assignment/Resources/framingham (1).csv")

sexAge.df <- farmingham.df[,c(1,2)]

sexAge.norm <- sexAge.df
sexAge.norm[,2] <- as.data.frame(scale(sexAge.norm[, 2]))

head(sexAge.norm)


## ---------------------------------------------------------------------------------------------------------------------
library(factoextra)

set.seed(123)
k4 <- kmeans(sexAge.norm, centers = 4, nstart = 10)

fviz_cluster(k4, data = sexAge.norm)


## ---------------------------------------------------------------------------------------------------------------------
set.seed(777)
fviz_nbclust(sexAge.norm, kmeans, method = "wss")


## ---------------------------------------------------------------------------------------------------------------------
fviz_nbclust(sexAge.norm, kmeans, method = "silhouette")


## ---------------------------------------------------------------------------------------------------------------------
churn.df <- read.csv("/Users/alireza/Desktop/DTI/Semester 1/Fundamentals of Applied Data Science/Optional assignment/Resources/customer_churn.csv")

head(churn.df)



## ---------------------------------------------------------------------------------------------------------------------
library(RColorBrewer)
set.seed(111)
dt = sort(sample(nrow(churn.df), nrow(churn.df)*0.67))
train<-churn.df[dt,]
test<-churn.df[-dt,]

tt <- data.frame(Test = nrow(test), Train = nrow(train))
tt <- as.matrix(tt)

coul <- brewer.pal(5, "Set2") 
barplot(height=tt[1,], col=coul )



## ---------------------------------------------------------------------------------------------------------------------
t1 <- table(train$Churn)
t2 <- table(test$Churn)
t3 <- table(churn.df$Churn)



t1.ratio <- t1[2]/(t1[1] + t1[2])
t1.ratio

t2.ratio <- t2[2]/(t2[1] + t2[2])
t2.ratio


## ---------------------------------------------------------------------------------------------------------------------
library(ROSE)
churn.df$Churn <- as.factor(churn.df$Churn)

rebalanced <- ovun.sample(Churn~., data = churn.df, method = "both", p = 0.212, seed = 213)$data
t1n <- table(rebalanced$Churn)

t1n.ratio <- t1n[2]/(t1n[1] + t1n[2])
t1n.ratio



## ---------------------------------------------------------------------------------------------------------------------
set.seed(313)
re.dt = sort(sample(nrow(rebalanced), nrow(rebalanced)*0.67))
re.train<-rebalanced[dt,]
re.test<-rebalanced[-dt,]


## ---------------------------------------------------------------------------------------------------------------------

re.train$Churn<-ifelse(re.train$Churn=="Yes",1,0)

table(re.train$Churn)

re.test$Churn<-ifelse(re.test$Churn=="Yes",1,0)

table(re.test$Churn)



## ---------------------------------------------------------------------------------------------------------------------


