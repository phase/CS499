# CS 499 - Unsupervised Machine Learning
# Homework 4
# Jadon Fowler


# Needed before running
library(data.table)
library(ggplot2)
library(reshape2)
library(mclust)
library(microbenchmark)

if (!requireNamespace("R.utils")) {
  install.packages("R.utils")
}

if (!requireNamespace("animint2")) {
  if (!requireNamespace("remotes")) install.packages("remotes")
  remotes::install_github("tdhock/animint2")
}
library(animint2)
if (!requireNamespace("servr")) install.packages("servr")
library(servr)

# download dataset
if (!file.exists("homework4.gz")) {
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "homework4.gz"
  )
}

# read dataset without the first column
dataset.dt <- data.table::fread("homework3.gz", data.table=TRUE, drop = 1)
# read only the first column
digit.labels <- data.table::fread("homework3.gz", data.table=TRUE)[["V1"]]
big.data.mat <- dataset.dt[rep(1:nrow(dataset.dt), 10),]

n.clusters <- 10
n.clusters.vec <- 1:n.clusters

max.dataset.size <- 800
subset.sizes <- as.integer(10^seq(1, log10(max.dataset.size), l=5))

clust.timings.list <- list()
# iterate over every size we want to test
for (subset.size in subset.sizes) {
  dataset.dt.cutoff <- dataset.dt[0:dataset.count]
  n.data.mat <- big.data.mat[1:subset.size,]
  # benchmark kmeans & hclust
  n.timings <- microbenchmark::microbenchmark(
    kmeans = {
      kmeans.result <- stats::kmeans(dataset.dt, n.clusters)
    },
    hclust = {
      dist.mat <- stats::dist(dataset.dt.cutoff)
      hc.tree <- stats::hclust(dist.mat, "single")
      cut.result <- stats::cutree(hc.tree, k=n.clusters.vec)
    },
    times = 3
  )
  clust.timings.list[[paste(subset.size)]] <- data.table(
    subset.size,
    n.timings)
}

clust.timings <- do.call(rbind, clust.timings.list)

# make subset.size vs time plot
ggplot() +
  geom_point(
    aes(x = subset.size, y = time, color = expr),
    data = clust.timings
  ) +
  scale_x_log10() +
  scale_y_log10()

dataset.dt.cutoff <- dataset.dt[0:max.dataset.size]
data.mat <- as.matrix(dataset.dt.cutoff)
dataset.dist <- stats::dist(data.mat)

hclust.err.values.list <- list()
# run hclust for single average linkage
linkage.list <- list("single", "average")
for (linkage in linkage.list) {
  hc.tree <- stats::hclust(
    # distance matrix we computed
    dataset.dist,
    linkage
  )
  hc.tree.drendo.list <- ggdendro::dendro_data(hc.tree)
  
  # cut the tree and get cluster assignment vectors
  n.clusters.vec <- 1:20
  cut.result <- stats::cutree(hc.tree, k=n.clusters.vec)
  
  for (n.clusters in n.clusters.vec) {
    ari <- pdfCluster::adj.rand.index(
      cut.result[, n.clusters], 
      digit.labels[0:max.dataset.size]
      )
    hclust.err.values.list[[paste(linkage, n.clusters)]] <- data.table(
      n.clusters, linkage, ari
    )
  }
}
# kmeans clustering
for (n.clusters in n.clusters.vec) {
  kmeans.result <- stats::kmeans(dataset.dt.cutoff, n.clusters)
  ari <- pdfCluster::adj.rand.index(digit.labels[0:max.dataset.size], kmeans.result$cluster)
  linkage <- "kmeans"
  hclust.err.values.list[[paste("kmeans", n.clusters)]] <- data.table(n.clusters, linkage, ari)
}
hclust.err.values <- do.call(rbind, hclust.err.values.list)

ggplot() +
  geom_line(
    aes(x = n.clusters, y = ari, color = linkage),
    data = hclust.err.values
  )

