pkgs<-c("factoextra", "NbClust")
install.packages(pkgs)
library(factoextra)
library(NbClust)
setwd("E:/R Language/Data-Science/Practice/K-Means")
data<-read.csv("USArrests.csv", header= TRUE,sep=",")
data$rownames<-as.numeric(as.factor(data$rownames))
data
scale(data)
class(data)
fviz_nbclust(data, kmeans, method = "wss")

km <- kmeans(data, centers = 4, nstart = 50)

df_member <- cbind(data, cluster = km$cluster)
df_member
fviz_cluster(km, data = data)
aggregate(USArrests, by=list(cluster=km$cluster), mean)



