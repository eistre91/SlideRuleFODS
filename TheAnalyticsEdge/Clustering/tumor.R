healthy <- read.csv("healthy.csv", header=FALSE)
healthy_matrix <- as.matrix(healthy)
healthy_vector <- as.vector(healthy_matrix)

image(healthy_matrix, axes=FALSE, col=grey(seq(0,1,length=256)))

k <- 5
set.seed(1)
KMC <- kmeans(healthy_vector, centers=k, iter.max=1000)
str(KMC)

healthy_clusters <- KMC$cluster
dim(healthy_clusters) <- c(nrow(healthy_matrix),
                           ncol(healthy_matrix))
image(healthy_clusters, axes=FALSE, col=rainbow(k))

tumor <- read.csv("tumor.csv", header=FALSE)
tumor_matrix <- as.matrix(tumor)
tumor_vector <- as.vector(tumor_matrix)

install.packages("flexclust")
library(flexclust)

KMC.kcaa <- as.kcca(KMC, healthy_vector)
tumor_clusters <- predict(KMC.kcaa, newdata=tumor_vector)
dim(tumor_clusters) <- c(nrow(tumor_matrix),
                         ncol(tumor_matrix))
image(tumor_clusters, axes=FALSE, col=rainbow(k))

#creating a scree plot needs the following and a numclusters sequence 2:10
SumWithinss = sapply(2:10, 
                     function(x) 
                       sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))

plot(NumClusters, SumWithinss, type="b")