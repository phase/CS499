library(data.table)
library(ggplot2)
library(binsegRcpp)
library(jointseg)
library(depmixS4)
data(neuroblastoma, package="neuroblastoma")

nb.dt <- data.table(neuroblastoma$profiles)
data.dt <- nb.dt[profile.id == "79" & chromosome == "2"]

for (n.state in seq(1, 10)) {
  mod <- depmixS4::depmix(response = position ~ logratio, data = data.frame(data.dt), nstates = n.state)
}
