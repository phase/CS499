# CS 499 - Unsupervised Machine Learning
# Class 8/27/21
# Jadon Fowler

library(ggplot2)
data(WorldBank, package="animint2")
wb.dt <- data.table(WorldBank)
wb1975 <- wb.dt[year==1975]
wb1970.75 <- wb.dt[1970 <= year & year <= 1975]

ggplot() +
  geom_point(aes(
    x = life.expectancy,
    # y = fertility.rate,
    y = log10(GDP.per.capita.Current.USD),
    color = region
  ),
  data = wb1975) +
  geom_path(aes(
  x = life.expectancy,
  # y = fertility.rate,
  y = log10(GDP.per.capita.Current.USD),
   color = region,
   group = country
 ),
 data = wb1970.75)


ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate,
  # y = log10(GDP.per.capita.Current.USD),
  color = region
)) +
  # make points
  geom_point(aes(color = region), data = wb1975) +
  # make lines from many points
  geom_path(aes(group = country), data = wb1970.75)


# accumulating data tables
#  list is a k/v store
points.dt.list <- list()
path.dt.list <- list()
for (show.year in c(1975, 1985, 1995)) {
  wb.dt.year <- wb.dt[year == show.year]
  wb.dt.year.range <- wb.dt[show.year - 5 <= year & year <= show.year]

  points.dt.list[[paste(show.year)]] <- 
    data.table(show.year, wb.dt.year)
  path.dt.list[[paste(show.year)]] <-
    data.table(show.year, wb.dt.year.range)
}
points.dt <- do.call(rbind, points.dt.list)
path.dt <- do.call(rbind, path.dt.list)

ggplot(mapping=aes(
  x = life.expectancy,
  y = fertility.rate,
  # y = log10(GDP.per.capita.Current.USD),
  color = region
)) +
  # make points
  geom_point(aes(color = region), data = points.dt) +
  # make lines from many points
  geom_path(aes(group = country), data = path.dt) +
  #facet_grid(. ~ show.year)
  facet_grid(show.year ~ region)
