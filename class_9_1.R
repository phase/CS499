#N.pixels <- 16
#number.grids.list <- list()

#for (num in 1:10) {
#  number.grids.list[[paste(num)]] <- table(
#    row = rep(N.pixels:1, each = N.pixels),
#    col = rep(1:N.pixels, times = N.pixels),
#    intensity = data.mat[num,],
#    grid.number = num
#  )
#}
# rbind(dt1, dt2, dt3, ...)
#number.grids <- do.call(rbind, number.grids.list)
#number.grids.mat <- as.matrix(number.grids)

#ggplot() +
#  geom_tile(aes(x = col, y = row, fill = intensity))

###############################################################################

library(data.table)
library(ggplot2)
library(pdfCluster)

head(iris)
data.mat <- as.matrix(iris[, c("Petal.Width", "Petal.Length")])

ggplot() +
  geom_point(aes(
    x = Petal.Length, 
    y = Petal.Width,
    color = Species
  ), data = iris)

kmeans.result <- stats::kmeans(data.mat, 3)
kmeans.result.dt <- data.table(data.mat, cluster=factor(kmeans.result$cluster))

ggplot() +
  geom_point(aes(
    x = Petal.Length, 
    y = Petal.Width,
    color = cluster
  ), data = kmeans.result.dt)

## 9/3/21 vvv

iris.n.clusters.list <- list()
for (n.clusters in 1:10) {
  kmeans.result <- stats::kmeans(data.mat, n.clusters)
  pdfCluster::adj.rand.index(iris$Species, kmeans.result$cluster)
  iris.n.clusters.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    error = kmeans.result[["tot.withinss"]]
  )
}
# rbind(iris.n.clusters.list[[1]], iris.n.clusters.list[[2]], ...)
iris.n.clusters <- do.call(rbind, iris.n.clusters.list)

# plot errors
ggplot() +
  geom_point(aes(
    x = n.clusters,
    y = error),
    data = iris.n.clusters)
