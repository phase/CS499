library(data.table)
library(ggplot2)
library(binsegRcpp)
library(jointseg)
library(depmixS4)
library(changepoint)
data(neuroblastoma, package="neuroblastoma")

nb.dt <- data.table(neuroblastoma$profiles)
data.dt <- nb.dt[profile.id == "79" & chromosome == "2"]

set.seed(5)
lik.dt.list <- list()
# try different n.states out
for (n.states in seq(1, 8)) {
  # compute the hmm
  hmm.spec <- depmixS4::depmix(logratio ~ 1, data=data.dt, nstates=n.states)
  # fit it
  hmm.learned <- depmixS4::fit(hmm.spec)
  learned.parameters <- depmixS4::getpars(hmm.learned)
  # store in list
  lik.dt <- data.table(
    n.states,
    neg.log.lik = -depmixS4::logLik(hmm.learned)
  )
  lik.dt.list[[paste(n.states)]] <- lik.dt
}
# combine data tables together
lik.dt.total <- do.call(rbind, lik.dt.list)

# Part 2: graph negative log likelihood vs. n.states
ggplot() +
  geom_point(
    aes(
      x = n.states,
      y = neg.log.lik
    ),
    data = lik.dt.total
  )

# selected model size
n.states <- 4
# recompute the hmm
hmm.spec <- depmixS4::depmix(logratio ~ 1, data=data.dt, nstates=n.states)
# fit it
hmm.learned <- depmixS4::fit(hmm.spec)
learned.parameters <- depmixS4::getpars(hmm.learned)

# index the rows
data.dt[, data.i := 1:.N ]
data.dt[, best.state := factor(hmm.learned@posterior[,1]) ]

rle.list <- rle(as.integer(data.dt$best.state))
cumsum(rle.list$lengths)
change.dt <- data.table(
  end=which(diff(as.integer(data.dt$best.state)) != 0))

(mean.sd.params <- learned.parameters[
  names(learned.parameters) %in% c("(Intercept)", "sd")])
param.mat <- matrix(
  mean.sd.params, n.states, byrow = TRUE,
  dimnames = list(
    state = 1:n.states,
    parameter = c("mean", "sd")
  )
)
seg.dt <- data.table(
  state = rle.list$values,
  end = cumsum(rle.list$lengths),
  param.mat[rle.list$values,]
)
seg.dt <- with(rle.list, data.table(
  state = values,
  end = cumsum(lengths),
  param.mat[values,]
))
seg.dt[, start := 1 + c(0, end[-.N])]

# plot
ggplot() +
  # - best.state for each obs
  geom_point(
    aes(
      x = data.i, 
      y = logratio,
      color = best.state
    ),
    data = data.dt
  ) +
  # - changepoints
  geom_vline(
    aes(
      xintercept = end + 0.5
    ),
    data = change.dt
  ) +
  # - mean
  geom_segment(
    aes(
      x = start - 0.5,
      xend = end + 0.5,
      y = mean, 
      yend = mean
    ),
    data = seg.dt
  ) +
  # - standard deviation
  geom_rect(
    aes(
      xmin = start - 0.5,
      xmax = end + 0.5,
      ymin = mean - sd,
      ymax = mean + sd
    ),
    alpha = 0.5,
    data = seg.dt
  )

cpt.model <- changepoint::cpt.meanvar(
  data.dt$logratio,
  method = "SegNeigh",
  penalty = "Manual",
  Q = nrow(seg.dt)
)
segment.ends <- cpt.model@cpts
cpt.segs <- do.call(data.table, cpt.model@param.est)
cpt.segs[, sd := sqrt(variance)]
cpt.segs[, end := segment.ends]
cpt.segs[, start := 1 + c(0, end[-.N])]
cpt.segs

# plot
ggplot() +
  # - best.state for each obs
  geom_point(
    aes(
      x = data.i, 
      y = logratio,
      color = best.state
    ),
    data = data.dt
  ) +
  # - changepoints
  geom_vline(
    aes(
      xintercept = end + 0.5
    ),
    data = cpt.segs
  ) +
  # - mean
  geom_segment(
    aes(
      x = start - 0.5,
      xend = end + 0.5,
      y = mean, 
      yend = mean
    ),
    data = cpt.segs
  ) +
  # - standard deviation
  geom_rect(
    aes(
      xmin = start - 0.5,
      xmax = end + 0.5,
      ymin = mean - sd,
      ymax = mean + sd
    ),
    alpha = 0.5,
    data = cpt.segs
  )
