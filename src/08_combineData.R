library(odem.data)

setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes/')

library(tidyverse)
library(sf)

consumption <- read.csv('processed_data/consumptiontype_jan30.csv') %>%
  rename(nhdhr_id = lake)
str(consumption)

landuse <- read.csv('processed_data/landuse.csv') %>%
  mutate(nhdhr_id = paste0('nhdhr_', lake_nhdid))
trophic <- read.csv('processed_data/trophic.csv') %>%
  rename(nhdhr_id = site_id)
morphometry <- read.csv("processed_data/morphometry.csv") %>%
  rename(nhdhr_id = lake)
hydLakes <- read_sf(dsn = "inst/extdata/HydroLAKES/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.shp")


df_conTro <- merge(consumption, trophic, by = c('nhdhr_id', 'year'), all = T)
df_conTro_landuse <- merge(df_conTro, landuse, by = c('nhdhr_id', 'year'), all = T)
df_conTro_landuse_morph <- merge(df_conTro_landuse, morphometry, by = c('nhdhr_id'), all = T)
df <- merge(df_conTro_landuse_morph, hydLakes, by = c('Hylak_id'), all = T)

df1 = consumption %>%
  full_join(trophic, by = c('nhdhr_id', 'year'))
df2 = df1 %>%
  full_join(landuse, by = c('nhdhr_id', 'year'))
df3 = df2 %>%
  inner_join(morphometry, by = c('nhdhr_id'))
df4 = df3 %>%
  inner_join(hydLakes, by = c('Hylak_id'))
nrow(df1)
nrow(df2)
nrow(df3)
nrow(df4)

df <- df4

nrow(df)
nrow(as.data.frame(na.omit(df)))
# data = as.data.frame(na.omit(df))

write_csv(x = df, file = 'processed_data/data_jan30.csv', col_names = T)



### FIGURE 1

data <- read_csv('processed_data/data_jan30.csv', col_names = T)
df.long <- read.csv("processed_data/cluster.csv")

## get model performance
all.dne <- list.files('metabolism_model//')
all.dne_all <- all.dne[grepl('nhdhr', all.dne)]

info.df <- c()
for (idx in all.dne_all){
  if (file.exists(paste0('metabolism_model/',idx,'/lakeinfo.txt'))){
    info.df <- rbind(info.df,read.csv(paste0('metabolism_model/',idx,'/lakeinfo.txt')))
  }
}

info.df <- info.df %>%
  rename(nhdhr_id  = lake_id, fit_all = fit_tall)
data <- merge(data, info.df, by = 'nhdhr_id')

mean_train = data %>%
  pull(fit_train) %>%
  mean() %>%
  signif(3)

mean_test = data %>%
  pull(fit_test) %>%
  mean(na.rm = T) %>%
  signif(3)

mean_all = data %>%
  pull(fit_all) %>%
  mean(na.rm = T) %>%
  signif(3)

plot1 <- ggplot(data) +
  geom_density(aes(x = fit_train, fill = '1_Calibration'), alpha = 0.3) +
  geom_density(aes(x = fit_test, fill = '2_Validation'), alpha = 0.3) +
  geom_density(aes(x = fit_all, fill = '3_Total'), alpha = 0.3) +
  geom_vline(xintercept= mean_train, size=1., col = c("#00AFBB"), linetype = 'dashed') +
  annotate(geom = 'text', x = 3, y = 0.6, label = paste0("Calibration: ",mean_train,
                                                         '\nValidation: ',mean_test,
                                                         '\nTotal: ',mean_all),
           hjust = 0) +
  geom_vline(xintercept= mean_test, size=1.,  col = c("#E7B800"), linetype = 'dashed') +
  geom_vline(xintercept= mean_all, size=1.,  col = c("#FC4E07"), linetype = 'dashed') +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"), name = '') +
  theme_minimal(base_size = 15) +
  xlab('RMSE (g m-3)') + ylab('Density')

plot4 <- ggplot(df.long) +
  geom_line(aes(depth, value, color = name), size = 1.5) +
  scale_color_manual(values = c('red4','lightblue1','gold','red1','red4'), name = 'Cluster',
                     labels = cluster.labels) +
  xlab('Stratification duration [%]') +
  ylab('Ratio of Hypolimnion to \nSaturation DO [-]') +
  theme_minimal(base_size = 15)

# unique(data$lake[which(data$lake[which(data$fit_train < 2)] %in% data$lake[which(data$ct == 'Heavy consumption')])])
# unique(data$lake[which(data$lake[which(data$fit_train < 2)] %in% data$lake[which(data$ct == 'Low consumption')])])

id_high = 'nhdhr_120018107' #'nhdhr_120018097'
id_low = 'nhdhr_120019016'# 'nhdhr_120020350'#"nhdhr_120018089"

# name_high = data %>% filter(lake == id_high) %>% select(Hylak_id)
# match(unique(name_high),lake_shapes$Hylak_id)
# lake_shapes %>% filter(Hylak_id > 1041660 & Hylak_id < 1041665)

plot_time <- function(id, time1, time2, main){
  load(paste0('metabolism_model/',id,'/modeled_o2.RData'))
  
  o2_data = o2$df_kgml
  idx = which(duplicated(colnames(o2_data)) == T)
  o2_data <- o2_data[, - idx]
  ggplot(o2_data %>% filter(year >= time1 & year <= time2)) +
    geom_line(aes(datetime, o2_epi/1000, col = 'Epilimnion sim.')) +
    geom_point(aes(datetime, obs_epi, col = 'Epilimnion obs.'), size = 2) +
    geom_line(aes(datetime, o2_hyp/1000, col = 'Hypolimnion sim.')) +
    geom_point(aes(datetime, obs_hyp, col = 'Hypolimnion obs.'), size = 2) +
    geom_point(aes(datetime, obs_tot, col = 'Total obs.'), size = 2) +
    # facet_wrap(~year) +
    ylab(expression("Conc. [g DO"*~m^{-3}*"]")) +
    scale_color_manual(values = c('red1','red4','lightblue3','lightblue1','gold')) +
    xlab('') + ggtitle(main) +
    ylim(c(-2,25)) +  theme_minimal()+
    theme(legend.text = element_text(size = 11), axis.text.x= element_text(size = 20), plot.title = element_text(size = 20),
          axis.text.y= element_text(size = 20), text = element_text(size = 20), legend.title = element_blank(), strip.text =element_text(size = 20),
          legend.position = 'bottom') +
    guides(col=guide_legend(nrow=3,byrow=TRUE))
}
# 2 3 4
# for (id_test in unique(data$lake[which(data$lake[which(data$fit_train < 2)] %in% data$lake[which(data$ct == 'Low consumption')])])){
#   p <- plot_time(id_test, 1979, 2018, paste0('Low consumption: ', id_test))
#   print(p)
#   readline(prompt="Press [enter] to continue")
# }

plot2 <- plot_time(id_high, 2010, 2013, paste0('High consumption: ', id_high))
plot3 <- plot_time(id_low, 2008, 2011,  paste0('Low consumption: ', id_low))



to_plot <- plot1 / (plot2 / plot3 + plot_layout(guides = 'collect') )/ plot4 +
  plot_annotation(tag_levels = 'A')
ggsave(file = 'figs/Figure1.png', to_plot,  dpi = 600, width =13, height = 15)


