library(data.table)
library(ggplot2)
library(pdfCluster)
library(mclust)
library(ggdendro)

head(iris)
data.mat <- as.matrix(
  iris[, c("Petal.Width", "Petal.Length")]
)
head(data.mat)
str(data.mat)
# distance matrix
dist.mat <- stats::dist(data.mat)
N <- nrow(data.mat)
N*(N-1)/2 # number of pairs of N objects


hclust.err.values.list <- list()

linkage.list <- list("single", "complete")
for (linkage in linkage.list) {
  hc.tree <- stats::hclust(
    # distance matrix we computed
    dist.mat,
    linkage
  )
  hc.tree.gg.list <- ggdendro::dendro_data(hc.tree)

  # cut the tree and get cluster assignment vectors
  n.clusters.vec <- 1:20
  cut.result <- stats::cutree(hc.tree, k=n.clusters.vec)

  for (n.clusters in n.clusters.vec) {
    ari <- pdfCluster::adj.rand.index(cut.result[, n.clusters], iris$Species)
    hclust.err.values.list[[paste(linkage, n.clusters)]] <- data.table(n.clusters, linkage, ari)
  }
}
hclust.err.values <- do.call(rbind, hclust.err.values.list)

ggplot() +
  geom_line(
    aes(x = n.clusters, y = ari, color = linkage),
    data = hclust.err.values
  )

labels.dt <- data.table(hc.tree.gg.list)
labels.dt[, Species := iris$Species[as.integer(label)]]

ggplot() + 
  geom_segment(
    aes(
      x=y,
      y=x,
      xend=yend,
      yend=xend),
    data = hc.tree.gg.list$segments
  ) + 
  geom_text(
    aes(x=y,y=x,label=label),
    data=hc.tree.gg.list$labels
  )