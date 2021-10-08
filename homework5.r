# CS 499 - Unsupervised Machine Learning
# Homework 5
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
if (!file.exists("zip.test.gz")) {
  download.file(
    "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/zip.test.gz",
    "zip.test.gz"
  )
}

dataset.count <- 500
# read dataset without the first column
dataset.dt <- data.table::fread("zip.test.gz", 
                                data.table=TRUE, drop = 1)[0:dataset.count]
# read only the first column
digit.labels <- data.table::fread("zip.test.gz", 
                                  data.table=TRUE)[["V1"]][0:dataset.count]
n.folds <- 4 # 75% train, 25% validation
(uniq.folds <- 1:n.folds)

## Part 1
# Get random cluster assignments
set.seed(1234)
fold.id.vec <- sample(rep(uniq.folds, l=nrow(dataset.dt)))

valid.fold <- 1
is.valid <- fold.id.vec == valid.fold
is.train <- !is.valid
(set.vec <- ifelse(is.train, "train", "validation"))
table(set.vec, digit.labels)

train.set <- dataset.dt[is.train]
valid.set <- dataset.dt[is.valid]

kmeans.cluster.errors.list <- list()
mclust.cluster.errors.list <- list()
for (n.clusters in seq(1, 51, 10)) {
  # get centers using kmeans
  kmeans.result <- kmeans(train.set, n.clusters)
  centers.mat <- kmeans.result[["centers"]]

  pair.dt <- data.table(expand.grid(
    center.i=1:nrow(centers.mat),
    data.i=1:nrow(dataset.dt)
  ))
  # DataTable[i/WHERE, j/SELECT, by/GROUP BY]
  pair.dt[, dist := {
    rowSums((dataset.dt[data.i,] - centers.mat[center.i,]) ^ 2)
  }]

  # .SD subset of data table corresponding to one unique value of by=data.i
  closest.dt <- pair.dt[, .SD[which.min(dist)], by = data.i]
  # compute within clusters sum of squares
  closest.dt[, set := set.vec]
  total.errors <- closest.dt[, .(total.error=sum(dist)), by = set]
  train.error <- total.errors[1]$total.error
  validation.error <- total.errors[2]$total.error

  kmeans.cluster.errors.list[[paste(n.clusters, "validation")]] <- data.table(
    n.clusters,
    error = validation.error,
    set = "validation"
  )
  
  kmeans.cluster.errors.list[[paste(n.clusters, "train")]] <- data.table(
    n.clusters,
    error = train.error,
    set = "train"
  )

  # compute model-based clustering
  mclust.result <- mclust::Mclust(train.set, n.clusters, "EII")
  log.lik <- mclust::dens(
    mclust.result$modelName,
    dataset.dt,
    logarithm = TRUE,
    parameters = mclust.result$parameters)

  (lik.dt <- data.table(
    set=set.vec,
    log.lik))
  mclust.total.errors <- lik.dt[, .(total.error=sum(-log.lik)), by=set]
  mclust.train.error <- mclust.total.errors[1]$total.error
  mclust.validation.error <- mclust.total.errors[2]$total.error

  mclust.cluster.errors.list[[paste(n.clusters, "train")]] <- data.table(
    n.clusters,
    log.lik = mclust.train.error,
    set = "train"
  )

  mclust.cluster.errors.list[[paste(n.clusters, "valid")]] <- data.table(
    n.clusters,
    log.lik = mclust.validation.error,
    set = "valid"
  )
}

kmeans.cluster.errors <- do.call(rbind, kmeans.cluster.errors.list)
mclust.cluster.errors <- do.call(rbind, mclust.cluster.errors.list)

# make n.clusters vs kmeans tot.withinss
ggplot() +
  geom_line(
    aes(x = n.clusters, y = error, color = set),
    data = kmeans.cluster.errors
  )

# mclust based error plot
ggplot() +
  geom_line(
    aes(x = n.clusters, y = log.lik, color = set),
    data = mclust.cluster.errors
  )
