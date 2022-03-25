colset_4 <-  c("#D35C37", "#BF9A77", "#D6C6B9", "#97B8C2")

runoff_stations <- readRDS('data/runoff_stations.rds')
runoff_day <- readRDS('data/runoff_day.rds')

# Summary statistics -----------------------------------------------

runoff_stats <- runoff_day[, .(mean_day = round(mean(value), 0),
                               sd_day = round(sd(value), 0),
                               min_day = round(min(value), 0),
                               max_day = round(max(value), 0)), by = sname]
head(runoff_stats, 4)

ggplot(to_plot, aes(x = mean_day, y = area, col = sname, cex = alt_class)) +
  geom_point() +
  scale_color_manual(values = colorRampPalette(colset_4)(n_stations)) +
  theme_bw()