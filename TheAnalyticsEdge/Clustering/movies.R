movies <- read.table("movielens.txt", header=FALSE, sep="|", quote="\"")
str(movies)

colnames(movies) <-
  c("ID", "Title", "ReleaseData", "VideoReleaseDate",
    "IMDB", "Unknown", "Action", "Adventure", "Animation",
    "Childrens", "Comedy", "Crime", "Documentary", "Drama",
    "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery",
    "Romance", "SciFi", "Thriller", "War", "Western")
str(movies)

movies$ID <- NULL
movies$ReleaseData <- NULL
movies$VideoReleaseDate <- NULL
movies$IMDB <- NULL
movies <- unique(movies)

table(movies$Comedy)
table(movies$Western)
table(movies$Romance, movies$Drama)

distances <- dist(movies[2:20], method="euclidean")
clusterMovies <- hclust(distances, method="ward.D")
plot(clusterMovies)

clusterGroups <- cutree(clusterMovies, k=10)
#get genre means per cluster
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

#get genre means for an entire cluster
colMeans(subset(movies[2:20], clusterGroups == 1))

#split by clusters
spl <- split(movies[2:20], clusterGroups)
#access clusters by indexing
spl[[1]]
#this is the same as subset(movies[2:20], clusterGroups==1)
#gives the centroid of cluster 1
colMeans(spl[[1]])
#get cluster centroids for all clusters
lapply(spl, colMeans)


subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 <- subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

clusterGroups2 <- cutree(clusterMovies, k=2)
subset(movies, clusterGroups2 == 2)[1:10,]
