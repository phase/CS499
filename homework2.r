# CS 499 - Unsupervised Machine Learning
# Homework 2
# Jadon Fowler


# Needed before running
library(data.table)
library(ggplot2)
library(reshape2)

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
if (!file.exists("homework1.gz")) {
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "homework1.gz"
  )
}

# read dataset without the first column
dataset.dt <- data.table::fread("homework1.gz", data.table=TRUE, drop = 1)
# read only the first column
digit.labels <- data.table::fread("homework1.gz", data.table=TRUE)[["V1"]]

n.clusters.list <- list()
ari.clusters.list <- list()
for (n.clusters in 1:10) {
  # compute kmeans
  kmeans.result <- stats::kmeans(dataset.dt, n.clusters)
  # compute ari
  ari.clusters.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    value = pdfCluster::adj.rand.index(digit.labels, kmeans.result$cluster),
    metric = "ari"
  )
  n.clusters.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    value = kmeans.result[["tot.withinss"]],
    metric = "error"
  )
}
n.clusters <- do.call(rbind, n.clusters.list)
ari.clusters <- do.call(rbind, ari.clusters.list)
combined.dt <- rbind(ari.clusters, n.clusters)

ggplot() +
  geom_point(aes(
    x = n.clusters,
    y = value),
    data = combined.dt) +
  facet_grid(metric ~ ., scales="free")
