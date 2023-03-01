library(tidyverse)
library(lubridate)

# rawdata = read_csv('processed_data/raw_data.csv')
cluster = read_csv('processed_data/data_feb13.csv')
obsraw = read_csv('processed_data/observed_raw_data.csv') |> 
  rename(nhdhr_id = nhd_lake_id, Date = ActivityStartDate,
         Depth = ActivityDepthHeightMeasure.MeasureValue, o2 = ResultMeasureValue) |> 
  mutate(Date = as.Date(Date))

obsraw

cluster2 = cluster |> filter(obs_hypo > 1) |> 
  mutate(cluster = case_when(ct == 'Heavy consumption' ~ 1, 
                             ct == 'Low consumption' ~ 3)) |> 
  group_by(nhdhr_id) |> 
  summarise(cluster = median(cluster, na.rm = T)) |> 
  mutate(cluster = if_else(cluster > 1 & cluster < 3, 2, cluster)) |> 
  arrange(cluster)

table(cluster2$cluster)



lakenames = unique(cluster2$nhdhr_id)
plots.out.list = list()
for (i in 1:length(lakenames)){
  
  ct = cluster2 |> filter(nhdhr_id %in% lakenames[i])
  # rd = rawdata2 |> filter(lake %in% lakenames[1:10])
  # ct = cluster2 |> filter(cluster == 2)
  
  rd = obsraw |> filter(nhdhr_id %in% ct$nhdhr_id) |> 
    arrange(Date, Depth) |> 
    filter(month(Date) %in% c(7,8))
  
  cols = c('#c95f4f', '#e2eb9d', '#7799c9')
  clustercol = cols[pull(ct, cluster)]
  
  plots.out.list[[i]] = ggplot(rd) +
    geom_point(aes(x = o2, y = Depth), shape = 21, fill = clustercol, stroke  = 0.2) +
    geom_path(aes(x = o2, y = Depth, group = Date), linewidth = 0.2, color = clustercol) +
    scale_y_reverse(limits = c(20,0)) +
    xlab('o2 (mg/L)') + ylab('Depth(m)') +
    xlim(0,20) +
    labs(title = paste0('cluster = ', ct$cluster)) +
    theme_bw(base_size = 7)
  
  # ggsave(plot = p1, 'figs/raw_clusters_low.pdf', width = 20, height = 20, dpi = 500)
}

library(gridExtra)
plots <- marrangeGrob(plots.out.list, nrow = 4, ncol = 6)
ggsave( filename="Figs/rawDataCheck.pdf", plots, width = 12, height= 8, units="in")
