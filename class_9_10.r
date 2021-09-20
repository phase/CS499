library(data.table)
library(ggplot2)
library(pdfCluster)
library(mclust)

head(iris)
data.mat <- as.matrix(iris[, c("Petal.Width", "Petal.Length")])
n.clusters <- 3
mclust.result <- mclust::Mclust(
  data.mat, n.clusters, "VVI", verbose=FALSE
)
mclust.result$loglik
mclust.result$classification
mclust.options("subset")

big.data.mat <- data.mat[rep(1:nrow(data.mat), 10),]
str(big.data.mat)
system.time({
  mclust.result <- mclust::Mclust(
    big.data.mat, n.clusters, "VVI", verbose=FALSE,
    initialization = list(subset=1:150)
  )
})

(n.data.vec <- as.integer())