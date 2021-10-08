# class demo 9/22

data.mat <- as.matrix(iris[,1:4])
n.folds <- 4 # 75% train, 25% validation
fold.id.vec <- sample(rep(1:n.folds, l=nrow(iris)))
valid.fold <- 1
is.valid <- fold.id.vec == valid.fold
is.train <- !is.valid
(set.vec <- ifelse(is.train, "train", "validation"))
table(set.vec, iris$Species)
n.clusters <- 5
kmeans.result <- kmeans(data.mat[is.train], n.clusters)
centers.mat <- kmeans.result[["centers"]]
library(data.table)
pair.dt <- data.table(expand.grid(
  center.i=1:nrow(centers.mat),
  data.i=1:nrow(data.mat)
))
# DataTable[i/WHERE, j/SELECT, by/GROUP BY]
pair.dt[, dist := {
  rowSums((data.mat[data.i,] - centers.mat[center.i,]) ^ 2)
}]
#pair.dt[, data.table(closest=center.i[which.min(dist)]), by=data.i]
# .SD subset of data table corresponding to one unique value of by=data.i
closest.dt <- pair.dt[, .SD[which.min(dist)], by = data.i]/
# compute within clusters sum of squares
closest.dt[, set := set.vec]
closest.dt[, .(total.error=sum(dist)), by = set]
