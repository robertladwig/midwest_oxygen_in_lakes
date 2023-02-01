library(odem.data)

setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes/')

library(tidyverse)

lts_data <- read.csv('inst/extdata/LTS/ensemble_predictions.csv')
link <- read_csv('inst/extdata/LTS/nhd_hydrolakes_nla.csv') %>%
  select(Hylak_id, site_id)

df <- merge(lts_data, link, by = 'Hylak_id') %>%
  mutate(eutro = `mean_prob_eumixo`,
         oligo = mean_prob_oligo,
         dys = mean_prob_dys)

write.csv(df, file = 'processed_data/trophic.csv', quote = F, row.names = F)
# 
# types$Hylak_id <- link$Hylak_id[match(types$lake, link$site_id)]
# 
# troph_df <- troph %>%
#   rename(eutro = `mean_prob_prob_eu/mixo`,
#          oligo = mean_prob_prob_oligo,
#          dys = mean_prob_prob_dys) %>%
#   select(Hylak_id, year, eutro, oligo, dys)