flower <- read.csv("flower.csv", header=FALSE)
flower_matrix <- as.matrix(flower)
flower_vector <- as.vector(flower_matrix)

distance <- dist(flower_vector, method="euclidean")
cluster_intensity <- hclust(distance, method="ward.D")
plot(cluster_intensity)
rect.hclust(cluster_intensity, k=3, border="red")

flower_clusters <- cutree(cluster_intensity, k=3)
flower_clusters
tapply(flower_vector, flower_clusters, mean)

dim(flower_clusters) <- c(50,50)

image(flower_clusters, axes=FALSE)
image(flower_matrix, axes=FALSE, col=grey(seq(0,1,length=256)))
