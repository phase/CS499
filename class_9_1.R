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