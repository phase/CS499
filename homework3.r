# CS 499 - Unsupervised Machine Learning
# Homework 3
# Jadon Fowler


# Needed before running
library(data.table)
library(ggplot2)
library(reshape2)
library(mclust)

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
if (!file.exists("homework3.gz")) {
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "homework3.gz"
  )
}

# read dataset without the first column
dataset.count <- 500
dataset.dt <- data.table::fread("homework3.gz", 
                                data.table=TRUE, drop = 1)[0:dataset.count]
# read only the first column
digit.labels <- data.table::fread("homework3.gz",
                                  data.table=TRUE)[["V1"]][0:dataset.count]

kmeans.ari.list <- list()
mclust.ari.list <- list()
for (n.clusters in 1:20) {
  # compute kmeans
  kmeans.result <- stats::kmeans(dataset.dt, n.clusters)
  
  # compute model-based clustering
  rand.pairs <- mclust::hcRandomPairs(dataset.dt)
  mclust.result <- mclust::Mclust(
    dataset.dt, n.clusters, "EII", verbose=FALSE,
    initialization = list(hcPairs=rand.pairs)
  )
  
  # compute ari for kmeans & mclust
  kmeans.ari <- pdfCluster::adj.rand.index(
    digit.labels,
    kmeans.result$cluster
  )
  mclust.ari <- pdfCluster::adj.rand.index(
    digit.labels,
    mclust.result$classification
  )
  kmeans.ari.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    value = kmeans.ari,
    algorithm = "kmeans"
  )
  mclust.ari.list[[paste(n.clusters)]] <- data.table(
    n.clusters,
    value = mclust.ari,
    algorithm = "mclust"
  )
}
kmeans.result <- do.call(rbind, kmeans.ari.list)
mclust.result <- do.call(rbind, mclust.ari.list)
combined.dt <- rbind(mclust.result, kmeans.result)

ggplot() +
  geom_line(aes(
    x = n.clusters,
    y = value,
    color = algorithm),
    data = combined.dt)
