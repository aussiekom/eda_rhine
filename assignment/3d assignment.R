library(data.table)
library(ggplot2)

#1
runoff_day <- readRDS('data/runoff_day.rds')

runoff_stations <- runoff_day[, .(mean_day = round(mean(value), 0),
                                  sd_day = round(sd(value), 0),
                                  min_day = round(min(value), 0),
                                  max_day = round(max(value), 0)), by = sname]

runoff_stations_tidy <- melt(runoff_stations, id.vars = 'sname', variable.name = 'stats', value.name = 'runoff')

ggplot(runoff_stations_tidy, aes(sname, runoff, shape = stats, col = stats)) + 
  geom_point(aes(col = stats, shape = stats))

#2
runoff_stations[, variation_coefficient := sd_day / mean_day, by = sname]
dt_variation_coefficient_skewness <- runoff_stations[, .(sname, skewness, variation_coefficient)]

#3
runoff_month <- readRDS('./data/runoff_month.rds')
runoff_summary <- readRDS('./data/runoff_summary.rds')

runoff_classes <- runoff_summary[, .(sname, runoff_class)]
runoff_monthly_class <- runoff_month[runoff_classes, on = 'sname']

ggplot(runoff_monthly_class, aes(x = factor(month), y = value, fill = runoff_class)) +
  geom_boxplot() +
  facet_wrap(~sname, scales = 'free')

#4
ggplot(runoff_day, aes(x = sname, y = value)) +
  geom_boxplot()
# More runoff means more outliers, so boxplots are not suitable for representing this data due to the distribution 

#5
colours <-  c("red", "orange", "beige", "black")

runoff_summary[, area_class := factor('small')]
runoff_summary[area >= 10000 & area < 110000, area_class := factor('medium')]
runoff_summary[area >= 110000, area_class := factor('large')]

runoff_summary[, alt_class := factor('low')]
runoff_summary[altitude >= 50 & altitude < 350, alt_class := factor('medium')]
runoff_summary[altitude >= 350, alt_class := factor('high')]
runoff_summary

dt <- runoff_summary[, .(sname, area, alt_class)]
area_and_altitude <- runoff_stations[dt, on = 'sname']

ggplot(area_and_altitude, aes(x = mean_day, y = area, col = sname, cex = alt_class)) +
  geom_point() +
  scale_color_manual(values = colorRampPalette(colours)(17)) +
  theme_bw()
