library(data.table)
library(ggplot2)
library(binsegRcpp)
library(jointseg)
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

DP.model <- jointseg::Fpsn(data.dt$logratio, max.segments)
DP.loss <- data.table(
  algorithm="DP",
  segments=1:max.segments,
  loss=DP.model$J.est
)

binseg.model <- binsegRcpp::binseg_normal(data.dt$logratio, max.segments)
binseg.loss <- data.table(
  algorithm="binseg",
  segments=1:max.segments,
  loss=binseg.model$loss
)

both.loss <- rbind(DP.loss, binseg.loss)

ggplot() +
  geom_line(
    aes(x = segments, y = loss, color = algorithm),
    data = both.loss,
  ) +
  # specify colors manually
  scale_color_manual(values=c(binseg="red", DP="black"))

selected.segments <- 3L
coef(binseg.model, selected.segments)

end.mat <- X
data.table(
  segments=as.integer(row(end.mat)),
  end=as.integer(DP.model$t.est)
  
)[!is.na(end)][order(segments, end)]
