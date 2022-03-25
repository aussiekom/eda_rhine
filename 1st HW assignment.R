temperatures <- c(3, 6, 10, 14)
weights <- c(1, 0.8, 1.2, 1)
library(data.table)
aa <- function(x, y) {
  x * y
}
results <- aa(temperatures, weights)




runoff_ts <- data.frame(time = as.Date(1:90, origin = '2020/12/31'), 
                        value = sample(c(130, 135, 140), 
                                       size = 90, replace = T))
head(runoff_ts)
library(data.table)

runoff_dt <- data.table(runoff_ts)
select(runoff_ts, c(1:3))
runoff_ts

runoff_dt[, mon := month(time)]
runoff_dt[, mon_mean := mean(value), by = mon]
runoff_mean_by_month <- runoff_dt[, .(mon, mon_mean)]
runoff_mean_by_month
unique_by_month <- unique(runoff_mean_by_month)

persantage <- function(v1, v2){
  answer <- ((v2-v1)/v1)*100
}

jan_difference <- persantage(unique_by_month[1, mon_mean], unique_by_month[2, mon_mean])
jan_difference

feb_difference <- persantage(unique_by_month[2, mon_mean], unique_by_month[3, mon_mean])
feb_difference

unique_by_month[, persantage := c(0, jan_difference, feb_difference)]
unique_by_month