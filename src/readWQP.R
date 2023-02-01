
### Station ###
station = read_csv('station.csv') |>
  select(OrganizationIdentifier:MonitoringLocationDescriptionText,
         Lat = LatitudeMeasure, Lon = LongitudeMeasure)
# table(station$MonitoringLocationTypeName)
station2 = station |> filter(MonitoringLocationTypeName %in% c('Lake','Reservoir'))

### Data ###
df = read_csv('narrowresult.csv')
names(df)
df2 = df |> select(OrganizationIdentifier:ResultIdentifier,
                   depth = `ResultDepthHeightMeasure/MeasureValue`,
                   depthUnit = `ResultDepthHeightMeasure/MeasureUnitCode`,
                   result = ResultMeasureValue,
                  resultUnit = `ResultMeasure/MeasureUnitCode`)

head(df2)
table(df2$depthUnit)

df3 = df2 |> filter(depthUnit %in% c('ft','m')) |>
  filter(resultUnit %in% c('mg/l', 'ppm')) |>
  mutate(depth = if_else(depthUnit == 'ft', depth *0.3048, depth)) |>
  mutate(depthUnit = if_else(depthUnit == 'ft', 'm', depthUnit)) |>
  filter(!is.na(depth)) |>
  group_by(MonitoringLocationIdentifier) |>
  filter(max(depth) >= 10) |>
  filter(n() > 5) |>
  ungroup() |>
  mutate(result = as.numeric(result)) |>
  filter(result < 30, depth < 200)

#### Join sites and data ####

df4 = station2 |> inner_join(df3) |>
  arrange(Lat, MonitoringLocationIdentifier, ActivityStartDate, depth)

locations = unique(df4$MonitoringLocationIdentifier)

for (i in 1:(length(locations)/6)) {
  ii = i*6-5

  usedata = df4 |> filter(MonitoringLocationIdentifier %in% locations[ii:(ii+5)])

  usedata |>
    ggplot() +
    geom_point(aes(x = result, y = depth, col = ActivityStartDate, group = ActivityStartDate)) +
    geom_path(aes(x = result, y = depth, col = ActivityStartDate, group = ActivityStartDate)) +
    scale_y_reverse() +
    xlab('DO (mg/L)') + ylab('Depth (m)') +
    # labs(title = usedata$MonitoringLocationName[1]) +
    facet_wrap(~MonitoringLocationName, scales = 'free') +
    theme_bw(base_size = 7)

  ggsave(paste0('WQP_WI_figs/',i,'.png'), width = 6, height = 4, dpi = 500)
}

### lake link ###
link = read_csv('~/Documents/LAGOS_US/Locus/lake_link.csv')
names(link)
locations

link.wqp = link |> filter(wqp_monitoringlocationidentifier %in% locations) |>
  select(lagoslakeid, lake_nhdid, nhdhr_gnisid, wqp_monitoringlocationidentifier, lake_lat_decdeg, lake_lon_decdeg)
write_csv(link.wqp, 'WQPdolakes.csv')


test = read_csv('~/Documents/LAGOS_US/Geo/zone_atmosphere.csv')
head(test)
table(test$variable_name)
