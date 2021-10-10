# CS 499 - Unsupervised Machine Learning
# Homework week 7
# Jadon Fowler

library(data.table)
library(ggplot2)
library(binsegRcpp)
data(neuroblastoma, package="neuroblastoma")

nb.dt <- data.table(neuroblastoma$profiles)
data.dt <- nb.dt[profile.id == "4" & chromosome == "2"]

ggplot() +
  geom_point(
    aes(
      x = position, 
      y = logratio
    ),
    data = data.dt
  )

max.segments <- 20
normal.model <- binsegRcpp::binseg_normal(
  data.dt[["logratio"]], max.segments
)

plot(normal.model)

selected.segments <- 4L # "long integer"
segs.dt <- coef(normal.model, selected.segments)
vline.dt <- segs.dt[start > 1]
model.color = "blue"
ggplot() +
  geom_point(
    aes(
      x = seq_along(logratio), 
      y = logratio
    ),
    data = data.dt
  ) +
  geom_segment(
    aes(
      x = start,
      y = mean,
      xend = end,
      yend = mean
    ),
    # constant colors go outside aes() call
    color = model.color,
    size = 1.2,
    data = segs.dt
  ) +
  geom_vline(
    aes(
      xintercept = start - 0.5
    ),
    size = 1.1,
    color = model.color,
    data = vline.dt
  )
