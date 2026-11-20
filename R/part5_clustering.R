source("R/00_setup.R")
source("R/part1_cleaning_eda_full_model.R")
#add code below
###### remove rows with missing entries from everything or just from the predictior vars?

library("cluster")

# making new dataset with dummy variables and standardised variables
train_final <- data.frame(Pclass1=ifelse(train_clean$Pclass == 1, 1, 0),
                          Pclass2=ifelse(train_clean$Pclass == 2, 1, 0),
                          Pclass3=ifelse(train_clean$Pclass == 3, 1, 0),
                          Sex=ifelse(train_clean$Sex == "female", 1, 0),
                          Age=scale(train_clean$Age),
                          SibSp=scale(train_clean$SibSp),
                          Parch=scale(train_clean$Parch),
                          Fare=scale(train_clean$Fare))

head(train_final)

# Euclidean distance matrix
dist_matrix <- dist(train_final, method = "euclidean") 
dist_matrix

# Agglomerative clustering using Average linkage
train_clust <- agnes(dist_matrix, method = "average")

# Dendogram
plot(train_clust, main = "Agglomerative Clustering with Average Linkage", xlab = "Passengers", ylab = "Distance")

# Into 4 clusters
clusters4 <- cutree(train_clust, k=4)
train_final$Cluster4 <- clusters4
train_final$Survived <- train_clean$Survived 
table(train_final$Cluster4, train_final$Survived)
survival_prop4 <- prop.table(table(train_final$Cluster4, train_final$Survived), margin=1)*100
survival_prop4

# Into 5 clusters
clusters5<- cutree(train_clust, k=5)
train_final$Cluster5 <- clusters5
table(train_final$Cluster5, train_final$Survived)
survival_prop5 <- prop.table(table(train_final$Cluster5, train_final$Survived), margin=1)*100
survival_prop5
