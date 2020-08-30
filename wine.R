# Loading wine data
wine <- read.csv('D:/WORK/PCA/wine.csv')
View(wine)
sum(is.na(wine))
sum(is.null(wine))
# mydata[-1] -> Considering only numerical values for applying PCA
wine1 <- wine[-1]
View(wine1) 
cor(wine1)
# Model Building#
pwine<-princomp(wine1, cor = TRUE, scores = TRUE, covmat = NULL)
summary(pwine)
str(pwine)

plot(pwine) # graph showing importance of principal components 
biplot(pwine)
pwine$scores[,1:8]

# cbind used to bind the data in column wise
# Considering top 4 principal component scores and binding them with mydata
wine <- data.frame(wine,pwine$scores[,1:8])

###
# preparing data for clustering (considering only pca scores as they represent the entire data)

#Hierarchical 
clust <- wine[,15:22]
nclust <- scale(clust)
dclust <- dist(nclust,method = 'euclidean')
mclust <- hclust(dclust,method = 'complete')
plot1 <- plot(mclust,hang = -1)
groups <- cutree(mclust,k=4)
fclust <- data.frame(groups,wine)
table(groups)
aggregate(wine[,2:22],by=list(fclust$groups),FUN = mean)
library(dendextend)
mclustdendo<- as.dendrogram(mclust)
cd1 <- color_branches(mclustdendo,k=4)
plot(cd1)
rect.hclust(mclust,k=4,border = 'green')

##### K - MEANS ###
#kselection
library(kselection)
k <- kselection(nclust,parallel = T,k_threshold = 0.85,max_centers = 20)
k
plot(k)
opk <- num_clusters(k)
opk
rk <- num_clusters_all(k)
rk
#scree plot
wss=(nrow(nclust)-1)*sum(apply(nclust,2,var))
for(i in 2:15)
  {
  wss[i]=sum(kmeans(nclust,centers = i)$withinss)
}
plot(1:15,wss,type = 'b')
fitwine <- kmeans(nclust,11)# 11 cluster solution
fitwine
str(fitwine)### WSS<BSS so we can conclude that it is the best 
library(animation)
awine <- kmeans.ani(nclust, 11)
awine
kfinal <- data.frame(fitwine$cluster,wine)
aggregate(wine[,2:22],by=list(kfinal$fitwine.cluster),FUN = mean)
