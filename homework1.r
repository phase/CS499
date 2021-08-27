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

size <- 16

# function to create a dataframe from an index
create.dataframe <- function(index) {
  # create array from elements in a row
  pixel.data <- array(unlist(dataset.dt[index]))
  values <- seq(1, size)
  # generate X & Y lists for the dataframe
  ffillv <- function(idx) rep(values[idx], size)
  # 1 2 3 ... 15 16 1 2 3 ...
  X <- rep(values, size)
  # 1 1 1 ... 1 1 2 2 2 ...
  Y <- c(sapply(seq_len(length(values)), ffillv))
  # create the dataframe
  data.frame("X" = X, "Y" = Y, "Pixels" = pixel.data)
}
# get the 29th image as a dataframe
dataset.df <- create.dataframe(29)
# display pixels as matrix
matrix(dataset.df$Pixels, nrow = size, byrow = TRUE)
# display the dataframe
number.plot <- ggplot(dataset.df, aes(X, Y, fill = dataset.df$Pixels)) +
  geom_tile(aes(fill = dataset.df$Pixels))

#animint(number.plot)
number.plot
