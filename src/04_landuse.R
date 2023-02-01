library(odem.data)

setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes/')


library(parallel)
library(MASS)
library(tidyverse)


library(ggplot2)
library(RColorBrewer)
library(patchwork)


link <- read.csv("inst/extdata/LAGOS/LAGOSNE_lakeslocus101.csv")
zone_landuse <- read.csv("inst/extdata/LAGOS/zone_landuse.csv") %>%
  mutate('water' = nlcd_openwater11_pct + nlcd_icesnow12_pct,
           'developed' =  nlcd_devopen21_pct + nlcd_devlow22_pct +
             nlcd_devmed23_pct + nlcd_devhi24_pct,
           'barren' = nlcd_barren31_pct,
           'forest' = nlcd_fordec41_pct + nlcd_forcon42_pct +
             nlcd_formix43_pct,
           'shrubland' = nlcd_shrub52_pct,
           'herbaceous' = nlcd_grass71_pct ,
           'cultivated' = nlcd_past81_pct + nlcd_cultcrop82_pct,
           'wetlands' = nlcd_wetwood90_pct + nlcd_wetemerg95_pct) %>%
  filter(spatial_division == 'ws')
zone_id <- read.csv('inst/extdata/LAGOS/zone_information.csv')
source_table <- read.csv('inst/extdata/LAGOS/source_table_geo.csv')
lake_information <- read.csv('inst/extdata/LAGOS/lake_information.csv') %>%
  select(lagoslakeid, lake_nhdid, ws_zoneid) %>%
  rename(zoneid = ws_zoneid)

df <- merge(zone_landuse, lake_information, by = 'zoneid')

write.csv(df, file = 'processed_data//landuse.csv', quote = F, row.names = F)
# 
# 
# id.of.interest <- gsub(".*_","",lstm.sites$site_id)
# id.model.information <- match(id.of.interest, lake.information$lake_nhdid)
# 
# id.of.landuse <- match(lake.information$ws_zoneid[id.model.information], lake.landuse$zoneid)
# 
# 
# 
# landuse.of.interest <- landuse[id.of.landuse,]
# 
# landuse.df <- data.frame('nhdr_id' = lstm.sites$site_id)
# landuse.df <- cbind(landuse.df, landuse.of.interest)
# landuse.df$LANDUSE <- colnames(landuse.df[2:9])[apply(landuse.df[,2:9],1,which.max)]
# 
# write.csv(landuse.df, file = 'lstm/landuse.csv', quote = F, row.names = F)
# 
# landuse.df[match('nhdhr_143249470', landuse.df$nhdr_id),]
# 
# mendota <-  "143249470"
# lake.information[ match(mendota, lake.information$lake_nhdid), ]
# 
# trout <- "69886228"
# lake.information[ match(trout, lake.information$lake_nhdid), ]
