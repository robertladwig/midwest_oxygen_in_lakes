setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes')

source('R/helper.R')

if (!exists("password")){
  password <- as.character(read.delim('sensitive/password.txt', header = FALSE, stringsAsFactor = FALSE))
}


source("src/99_packages.R")
source('R/helper.R')


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
  
  wq_data = data_pull_variable
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
  
  df = as.data.frame(obs)
  
  all_obs_data <- rbind(all_obs_data, df)
}

dbDisconnect(con)

write_csv(x = all_obs_data, file = 'processed_data/observed_raw_data.csv', col_names = T)
