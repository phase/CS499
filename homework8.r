#
# Homework week 8
# Jadon Fowler
#
library(data.table)
library(ggplot2)
library(binsegRcpp)

# part 1

data.file.name <- "signals.csv.gz"
if (!file.exists(data.file.name)) {
  download.file(
    "https://raw.githubusercontent.com/tdhock/LOPART-paper/master/data-for-LOPART-signals.csv.gz",
    data.file.name
  )
}

dataset.dt <- data.table::fread(data.file.name, data.table=TRUE)
seq.ids <- unique(dataset.dt$sequenceID)

# calc the row counts per sequenceID
row.counts <- list()
for (seq.id in seq.ids) {
  # probably a better way to do this?
  row.counts <- append(row.counts, nrow(dataset.dt[sequenceID == seq.id]))
}

# print out min & max
(min(unlist(row.counts)))
(max(unlist(row.counts)))

# part 2

selected.seq <- "20167.22"
max.segments <- 20
selected.data <- dataset.dt[sequenceID == selected.seq]

DP.model <- jointseg::Fpsn(selected.data$data.i, max.segments)
binseg.model <- binsegRcpp::binseg_normal(selected.data$data.i, max.segments)

DP.loss <- data.table(
  algorithm="DP",
  segments=1:max.segments,
  loss=DP.model$J.est)

binseg.loss <- data.table(
  algorithm="binseg",
  segments=1:max.segments,
  loss=binseg.model$loss)

both.loss <- rbind(DP.loss, binseg.loss)

ggplot()+
  geom_line(aes(
    x = segments,
    y = loss,
    size = algorithm,
    color = algorithm),
    data = both.loss) +
  scale_size_manual(values = c(binseg = 2, DP = 1)) +
  scale_color_manual(values = c(binseg = "red", DP = "black"))

# DP does have a lower loss!

# part 3

selected.segments <- 9L
binseg.coef <- coef(binseg.model, selected.segments)

DP.model <- jointseg::Fpsn(selected.data$data.i, max.segments)
binseg.model <- binsegRcpp::binseg_normal(selected.data$data.i, max.segments)

(end.mat <- DP.model$t.est)
(n.segments.mat <- row(end.mat))
DP.segs <- data.table(
  segments=as.integer(n.segments.mat),
  end=as.integer(end.mat)
)[!is.na(end)][order(segments, end)]

# data table has segments, end columns, also need mean, start columns to plot with geom_segment.
for (segment in DP.segs$segments){
  print(segment)
}
cum.vec <- c(0, cumsum(selected.data$logratio))
DP.segs[, start := 1 + c(0, end[-.N]), by = segments]
DP.segs[, sum := cum.vec[end + 1] - cum.vec[start]]
DP.segs[, mean := sum / (end - start + 1)]

DP.selected.segments <- DP.segs[segments == selected.segments]
DP.selected.segments[, kind := "DP"]
binseg.selected.segments <- coef(binseg.model, selected.segments)
binseg.selected.segments[, sum := cum.vec[end + 1] - cum.vec[start]]
binseg.selected.segments[, mean := sum / (end - start + 1)]
binseg.selected.segments[, kind := "binseg"]

combined.dt <- rbind(DP.selected.segments, binseg.selected.segments)

ggplot() +
  geom_point(
    aes(
      x = data.i,
      y = logratio
    ),
    data = selected.data
  ) +
  geom_segment(
    aes(
      x = start, 
      y = mean,
      xend = end,
      yend = mean,
      color = kind
    ),
    data = combined.dt,
    size = 1
  ) +
  geom_vline(
    aes(
      xintercept = start-0.5,
      color = kind
    ),
    size = 1,
    data = combined.dt
  ) +
  scale_color_manual(values = c(binseg = "red", DP = "blue")) +
  facet_grid(kind ~ ., scales="free")

