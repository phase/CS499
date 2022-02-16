library(data.table)
library(ggplot2)
library(binsegRcpp)
library(jointseg)
library(depmixS4)
library(changepoint)
data(neuroblastoma, package="neuroblastoma")

nb.dt <- data.table(neuroblastoma$profiles)
data.dt <- nb.dt[profile.id == "79" & chromosome == "2"]

N.data <- nrow(data.dt)
sampled.data <- sample(data.dt$logratio, N.data / 2)
plot(sampled.data)

set.seed(1)
# split into subtrain and validation sets
data.dt[, set := sample(rep(c("subtrain", "validation"), l = .N))]
data.dt[, orig.row := .I]
subtrain.dt <- data.dt[set == "subtrain"]

ggplot() +
  geom_point(
    aes(
      position,
      logratio,
      color = set
    ),
    data = data.dt
  )

# calculate indices
change.indices <- subtrain.dt[, c(floor(orig.row[-.N] + diff(orig.row) / 2) + 0.5)]
start.indices <- c(1, change.indices)
end.indices <- c(change.indices, nrow(data.dt))

max.segments <- 10
(binseg.model <- binsegRcpp::binseg_normal(subtrain.dt$logratio, max.segments))

(segments.dt <- coef(binseg.model))
segments.dt[, start.index := start.indices[start]]
segments.dt[, end.index := end.indices[end]]

model.color <- "blue"
ggplot() +
  geom_point(
    aes(
      x = orig.row,
      y = logratio,
      color = set
    ),
    data = data.dt
  ) +
  geom_segment(
    aes(
      x = start.index,
      y = mean,
      xend = end.index,
      yend = mean
    ),
    color = model.color,
    size = 1,
    data = segments.dt
  ) +
  facet_grid(segments ~ .)

names(data.dt)
names(segments.dt)
# DT1[DT2, on=.(columnFromDT1 <=/==/>= columnFromDT2, ...)] is a JOIN
(data.and.means <- data.dt[
  segments.dt, 
  data.table(logratio, mean, segments, set), 
  on = .(orig.row >= start.index, orig.row <= end.index)
])
# O (S * N) S=models, N=data
nrow(data.dt) * max.segments
error.dt <- data.and.means[
  , .(loss = sum((mean-logratio)^2)),
  by = .(set, segments)
]

# sanity check
subtrain.loss <- error.dt[set == "subtrain", loss]
if (!all.equal(binseg.model$loss, subtrain.loss)) {
  stop("subtrain loss not correct!")
}

ggplot() +
  geom_line(
    aes(
      x = segments,
      y = loss,
      color = set
    ),
    data = error.dt
  )

# join AND summarize in 1 step
error.segs <- data.and.means <- data.dt[set == "validation"][
  segments.dt, 
  data.table(segments, loss.seg = sum((mean - logratio)^2)),
  by = .EACHI, # summarize by each row in segments.dt
  on = .(orig.row >= start.index, orig.row <= end.index)
]

error.efficient <- error.segs[, .(
  loss = sum(loss.seg)
), by = segments]

# part 4
# AIC / BIC
# use full data set

(full.model <- binsegRcpp::binseg_normal(data.dt$logratio, max.segments))
penalty.values <- c("AIC" = 2, "BIC" = log(nrow(data.dt)))
criteria.table.list <- list()
for (penalty.name in c("AIC", "BIC")) {
  penalty <- as.double(penalty.values[penalty.name])
  criteria.table <- data.table(
    kind = penalty.name,
    segments = seq(1, 10),
    value = full.model[, loss + penalty * segments]
  )
  criteria.table.list[[paste(penalty.name)]] <- criteria.table
}
combined.criteria <- do.call(rbind, criteria.table.list)

ggplot() +
  geom_line(
    aes(
      x = segments,
      y = value,
      color = kind
    ),
    data = combined.criteria
  )