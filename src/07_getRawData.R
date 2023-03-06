setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes')

source('R/helper.R')

if (!exists("password")){
  password <- as.character(read.delim('sensitive/password.txt', header = FALSE, stringsAsFactor = FALSE))
}


source("src/99_packages.R")
source('R/helper.R')
library(tidyverse)

###
odbc::odbcListDrivers()
drv = dbDriver("PostgreSQL")

tryCatch({
  con <- dbConnect(drv, dbname = "UW_data_science",
                   host = '144.92.62.199', port = 5432,
                   user = "postgres", password = password)
}, error = function(err) {
  stop(print(paste("Server error:  ",err)))
})

all.dne <- list.files('metabolism_model/')
all.dne_all <- all.dne[grepl('nhdhr', all.dne)]

all_obs_data <- data.frame('nhd_lake_id' = NULL,
                      'ActivityStartDate' = NULL,
                      'ActivityDepthHeightMeasure.MeasureValue' = NULL,
                      'ResultMeasureValue' = NULL)

for (lake_id in all.dne_all){
  print(paste0('Running ',lake_id))
  print(paste0(match(lake_id, all.dne_all), ' out of ',length(all.dne_all)))
  
  nhdid <- lake_id
  
  wq_var_1 <- 'Dissolved oxygen (DO)'
  data_pull_variable_1 <- dbGetQuery(con,
                                     paste("select * from data.pgml_lakes_interest_vars where \"nhd_lake_id\" = '",nhdid,"\' and \"CharacteristicName\" = \'",wq_var_1,"\'", sep = ''),
                                     stringsAsFactors = FALSE)
  
  wq_var_2 <- 'Dissolved oxygen'
  data_pull_variable_2 <- dbGetQuery(con,
                                     paste("select * from data.pgml_lakes_interest_vars where \"nhd_lake_id\" = '",nhdid,"\' and \"CharacteristicName\" = \'",wq_var_2,"\'", sep = ''),
                                     stringsAsFactors = FALSE)
  
  data_pull_variable <- rbind(data_pull_variable_1, data_pull_variable_2) %>%
    arrange(ActivityStartDate) %>%
    mutate(CharacteristicName = 'Dissolved oxygen (DO)')
  
  # dbDisconnect(con)
  
  depth_unit <- which(data_pull_variable$ActivityDepthHeightMeasure.MeasureUnitCode != 'm')
  to_remove_depth_unit = c()
  for (depth_iter in depth_unit){
    # if (data_pull_variable$ActivityDepthHeightMeasure.MeasureUnitCode[depth_iter] == ''){
    #   to_remove_depth_unit = append(to_remove_depth_unit, depth_iter) }
    if (data_pull_variable$ActivityDepthHeightMeasure.MeasureUnitCode[depth_iter] == 'ft'){
      data_pull_variable$ResultDepthHeightMeasure.MeasureValue[depth_iter] = 
        data_pull_variable$ResultDepthHeightMeasure.MeasureValue[depth_iter] * 0.3048
    }  else if (data_pull_variable$ActivityDepthHeightMeasure.MeasureUnitCode[depth_iter] == 'cm'){
      data_pull_variable$ResultDepthHeightMeasure.MeasureValue[depth_iter] = 
        data_pull_variable$ResultDepthHeightMeasure.MeasureValue[depth_iter] / 100
    }
  }
  
  eg_nml <- read_nml(paste0('inst/extdata/pball_nml/pball_nml_files/pball_',lake_id,'.nml'))
  H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H)) # DEPTH
  to_remove_depth_high <- which(data_pull_variable$ActivityDepthHeightMeasure.MeasureValue > max(H))

  
  to_remove_do_unit <- which(data_pull_variable$ResultMeasure.MeasureUnitCode != 'mg/l' & 
                     data_pull_variable$ResultMeasure.MeasureUnitCode != 'g/m3' & 
                       data_pull_variable$ResultMeasure.MeasureUnitCode != 'ppm')
  
  to_remove_all <- unique(c(to_remove_depth_high, to_remove_do_unit,to_remove_depth_unit))
  if (length(to_remove_all) != 0){
    wq_data = data_pull_variable[-to_remove_all,]
  } else {
    wq_data = data_pull_variable
  }
  obs     <- NULL
  
  raw_obs = wq_data
  if ('ActivityDepthHeighMeasure.MeasureValue' %in% colnames(raw_obs)){
    wq <- raw_obs %>%
      dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
      dplyr::select(
        c('nhd_lake_id','ActivityStartDate', 'ActivityDepthHeighMeasure.MeasureValue',
          'ResultMeasureValue'))
    wq <- rename(wq,
                 'ActivityDepthHeightMeasure.MeasureValue' =
                   'ActivityDepthHeighMeasure.MeasureValue')
  } else {
    wq <- raw_obs %>%
      dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
      dplyr::select(
        c('nhd_lake_id','ActivityStartDate', 'ActivityDepthHeightMeasure.MeasureValue',
          'ResultMeasureValue'))
  }
  
  obs = wq
  obs$ActivityStartDate <- as.POSIXct(obs$ActivityStartDate)
  
  
  obs = obs %>%
    group_by(ActivityStartDate) %>%
    arrange((ActivityDepthHeightMeasure.MeasureValue))
  
  obs = obs %>%
    arrange(ActivityStartDate)
  
  # Observations violate ODEMs pre-processing routine.
  # if (any(is.na(obs$ActivityDepthHeightMeasure.MeasureValue)) == FALSE){
  idtt <- which(is.na(obs$ActivityDepthHeightMeasure.MeasureValue))
  if (length(idtt) != 0){
    obs <- obs[-idtt,]
  }
  
  if (is.factor(obs$ActivityDepthHeightMeasure.MeasureValue)){
    obs$ActivityDepthHeightMeasure.MeasureValue <-
      as.numeric(as.character(obs$ActivityDepthHeightMeasure.MeasureValue))
  }
  if (is.factor(obs$ResultMeasureValue)){
    obs$ResultMeasureValue <-
      as.numeric(as.character(obs$ResultMeasureValue))
  }
  
  if (is.character(obs$ResultMeasureValue)){
    obs$ResultMeasureValue <-
      as.numeric(as.character(obs$ResultMeasureValue))
  }
  
  # outlier detection
  # if (length(unique(obs$ActivityStartDate)) > 10){
    outlier_values              <- which(obs$ResultMeasureValue > 25)
      #boxplot.stats(obs$ResultMeasureValue)$out
    # uvx                         <- match(outlier_values, obs$ResultMeasureValue)
    if (length(outlier_values) != 0){
    obs <- obs[-outlier_values,]
    }
  # }
  # 
  # remove duplicated rows
  id_duplicated <- !duplicated(obs)
  obs <- obs[id_duplicated, ]
  
  df = as.data.frame(obs)
  
  all_obs_data <- rbind(all_obs_data, df)
}

dbDisconnect(con)

write_csv(x = all_obs_data, file = 'processed_data/observed_raw_data.csv', col_names = T)
