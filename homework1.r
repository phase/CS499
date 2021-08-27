# CS 499 - Unsupervised Machine Learning
# Homework 1
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

# pixel width & height of the images
size <- 16

# function to create a data table from an index
create.number.dt <- function(index, row, col) {
  # create array from elements in a row
  pixel.data <- array(unlist(dataset.dt[index]))
  values <- seq(1, size)
  # generate X & Y lists for the dataframe
  ffillv <- function(idx) rep(values[idx], size)
  # 1 2 3 ... 15 16 1 2 3 ...
  X <- rep(values, size)
  # 1 1 1 ... 1 1 2 2 2 ...
  Y <- c(sapply(seq_len(length(values)), ffillv))
  # create the data table
  ret <- data.table(index, row, col, "X" = X, "Y" = Y, "Pixels" = pixel.data)

  # string output of the pixel matrix
  str(matrix(ret$Pixels, nrow = size, byrow = TRUE))
  # What is the number of rows/observations/example digits?
  #  2007!
  # What is the number of columns/features/pixels per example?
  #  16 x 16 = 256 pixels per number

  ret
}

# collect number data tables into one data table
numbers.dt.list <- list()
# must be a perfect square
numbers.to.display <- 25
for (show.number in seq(1, numbers.to.display)) {
  row <- show.number %% sqrt(numbers.to.display)
  col <- ceiling(show.number / sqrt(numbers.to.display))
  numbers.dt.list[[show.number]] <- create.number.dt(show.number, row, col)
}
numbers.dt <- do.call(rbind, numbers.dt.list)

# display the dataframe
number.plot <- ggplot(numbers.dt, aes(X, Y, fill = Pixels)) +
  geom_tile(aes(fill = Pixels)) +
  facet_grid(row ~ col)

#animint(number.plot)
number.plot
