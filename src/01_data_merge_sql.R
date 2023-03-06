if (!exists("password")){
  password <- as.character(read.delim('sensitive/password.txt', header = FALSE, stringsAsFactor = FALSE))
}

args <- commandArgs(trailingOnly=TRUE)
if (length(args) > 0){
  lake_id   <- args[1]
  if (!exists("password")){
    password   <- args[2]
  }
}else{
  if (!exists("lake_id")){
    stop("Must set the variable 'lake_id' to a NHDHR ID.")
  }
  if (!exists("password")){
    stop("Must set the variable 'password' to the POSTGRES server password.")
  }
}


source("src/99_packages.R")
source('R/helper.R')

print(paste0('Running ',lake_id))

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


nhdid <- lake_id
#grab calibrated predicted tempereature data for nhdid
wtr.data <- dbGetQuery(con,
                       paste("select * from data.predicted_temps_calibrated where \"nhd_lake_id\" = '",nhdid,"\'", sep = ''),
                       stringsAsFactors = FALSE)

if (nrow(wtr.data) != 0){
  meteo.data <- dbGetQuery(con,
                           paste0("select * from data.met_input_data where meteofile in (select distinct(meteo_filename) from data.nhd_met_relation where nhdid = '",lake_id,"\')"),
                           stringsAsFactors = FALSE)

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
  
  
  

  
  if (nrow(data_pull_variable) != 0){

  ###
  dbDisconnect(con)
    
    
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

  data = wtr.data
  meteo = meteo.data



  raw_obs = wq_data
  if ('ActivityDepthHeighMeasure.MeasureValue' %in% colnames(raw_obs)){
    wq <- raw_obs %>%
      dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
      dplyr::select(
        c('ActivityStartDate', 'ActivityDepthHeighMeasure.MeasureValue',
          'ResultMeasureValue'))
    wq <- rename(wq,
                 'ActivityDepthHeightMeasure.MeasureValue' =
                   'ActivityDepthHeighMeasure.MeasureValue')
  } else {
    wq <- raw_obs %>%
      dplyr::filter(CharacteristicName== "Dissolved oxygen (DO)") %>%
      dplyr::select(
        c('ActivityStartDate', 'ActivityDepthHeightMeasure.MeasureValue',
          'ResultMeasureValue'))
  }

  obs = wq
  obs$ActivityStartDate <- as.POSIXct(obs$ActivityStartDate)
  # }

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
  # outlier detection
  # if (length(unique(obs$ActivityStartDate)) > 10){
  outlier_values              <- which(obs$ResultMeasureValue > 25)
  #boxplot.stats(obs$ResultMeasureValue)$out
  # uvx                         <- match(outlier_values, obs$ResultMeasureValue)
  if (length(outlier_values) != 0){
    obs <- obs[-outlier_values,]
  }
    
    # remove duplicated rows
    id_duplicated <- !duplicated(obs)
    obs <- obs[id_duplicated, ]

    eg_nml <- read_nml(paste0('inst/extdata/pball_nml/pball_nml_files/pball_',lake_id,'.nml'))

    H <- abs(eg_nml$morphometry$H - max(eg_nml$morphometry$H)) # DEPTH
    A <- eg_nml$morphometry$A # AREA
    
    

    p.data <- c()
    for (ki in sort(unique(data$date))){
      yi <- which(data$date == ki)
      p.data <- rbind(p.data, rev(data$Value[yi]))
    }

    data2 <- as.data.frame(cbind(rep(1, length(unique(data$date))), p.data))
    colnames(data2) <- c('date',paste0('temp_',sort(unique(data$Depth))))
    data2$date = as.Date(sort(unique(data$date)))

    meteo <- meteo[order(meteo$time),]

    chidx   <- match((as.Date(data2$date)),as.Date(meteo$time))
    wind    <- meteo$WindSpeed[chidx]
    airtemp <- meteo$AirTemp[chidx]


    input.values         <- input(wtemp = data2, H = H, A = A)
    input.values$year    <- year(input.values$datetime)
    input.values$doy     <- yday(input.values$datetime)
    input.values$wind    <- wind
    input.values$airtemp <- airtemp

    input.values <-
      dplyr::rename(input.values,
                    datetime = datetime,
                    thermocline_depth = thermocline_depth,
                    temperature_epi = temperature_epi,
                    temperature_hypo   = temperature_hypo,
                    temperature_total  = temperature_total,
                    volume_total = volume_total,
                    volume_epi = volume_epi,
                    volume_hypo = volume_hypo,
                    td_area = td_area,
                    area_surface = area_surface,
                    upper_meta = upper.metalim,
                    lower_meta = lower.metalim,
                    year = year,
                    day_of_year = doy,
                    max.d = max.d,
                    wind = wind,
                    airtemp = airtemp)
    
    if (!all(is.na(input.values$volume_hypo))){

    w.obs <- weigh_obs(obs,input.values = input.values, H, A)
    obs_long <- w.obs[[1]]
    obs_weigh <- w.obs[[2]]
    obs_weigh_df <- data.frame(matrix(vector(), ncol(obs_weigh), 4,
                                      dimnames=list(c(), c("Date", "Tot", "Epi", "Hyp"))),
                               stringsAsFactors=F)
    obs_weigh_df$Date <- unique(obs_long$ActivityStartDate)
    obs_weigh_df$Tot <- obs_weigh[2,]
    obs_weigh_df$Epi <- obs_weigh[3,]
    obs_weigh_df$Hyp <- obs_weigh[4,]

    if (file.exists(paste0('metabolism_model/',lake_id))){
      folder_name <- paste0('metabolism_model/',lake_id)
    } else {
      dir.create(file.path(paste0('metabolism_model/',lake_id)))
      folder_name <- paste0('metabolism_model/',lake_id)
    }
    write.table(input.values, paste0(folder_name, '/input.txt'),
                append = FALSE, sep = ",", dec = ".",
                row.names = FALSE, col.names = TRUE) # input data --> B-ODEM

    startdate = 1
    enddate = nrow(input.values)

    elevation = max(eg_nml$morphometry$H)

    ### manual test of code
    nep = 1000
    min = -50
    sed = 3000
    khalf <- 500

    init.val = c(5, 5, 5, 5)
    target.iter = 100
    lb <<- c(10, -1000, 100, 100)
    ub <<- c(5000, +1000, 5000, 3000)

    print('Start optimization')

    smp_size = floor(0.7 * nrow(obs_weigh_df))

    error <- try(


      modelopt <- odem_static_v2(input.values = input.values,
                        nep = nep,
                        min = min,
                        sed = sed,
                        wind = wind,
                        khalf = khalf,
                        startdate = startdate,
                        enddate = enddate,
                        field.values = obs_weigh_df[1:smp_size,],
                        obs_weigh_df = obs_weigh_df[1:smp_size,],
                        elev = elevation)
    )

    if (!grepl('Error',error[1])){
      modelopt <- pureCMAES(par = init.val, fun = optim_odem_static_v2, lower = rep(0,4),
                            upper = rep(10,4), sigma = 0.5,
                            stopfitness = -Inf,
                            stopeval = target.iter,
                            input.values = input.values,
                            field.values = obs_weigh_df[1:smp_size,],
                            obs_weigh_df = obs_weigh_df[1:smp_size,],
                            wind = wind, elev = elevation,
                            verbose = verbose, startdate = startdate, enddate = enddate)

      modelopt$xmin_unscale <- lb+(ub - lb)/(10)*(modelopt$xmin)
      save(modelopt, file = paste0(folder_name, '/calibration_fit.RData'))

      training <- round(modelopt$fmin,2)

      o2 <- odem_static_v2(input.values = input.values,
                        nep = modelopt$xmin_unscale[1],
                        min = modelopt$xmin_unscale[2],
                        sed = modelopt$xmin_unscale[3],
                        wind = wind,
                        khalf = modelopt$xmin_unscale[4],
                        startdate = startdate,
                        enddate = enddate,
                        field.values = obs_weigh_df,
                        obs_weigh_df = obs_weigh_df,
                        elev = elevation)

      all <- round(o2$fit,2)

      o2$df$nhdhr_id <- lake_id

      save(o2, file = paste0(folder_name, '/modeled_o2.RData'))

      ggsave(file = paste0(folder_name, '/timeseries_',round(o2$fit,2),'.png'), o2$plot, dpi=300,width=350,height=300,units='mm')
      ggsave(file = paste0(folder_name, '/scatterplot_',round(o2$fit,2),'.png'), o2$scatterplot, dpi=300,width=350,height=300,units='mm')

      write_feather(o2$df_kgml, paste0(folder_name,'/',lake_id,'.feather'))

      o2 <- odem_static_v2(input.values = input.values,
                        nep = modelopt$xmin_unscale[1],
                        min = modelopt$xmin_unscale[2],
                        sed = modelopt$xmin_unscale[3],
                        wind = wind,
                        khalf = modelopt$xmin_unscale[4],
                        startdate = startdate,
                        enddate = enddate,
                        field.values = obs_weigh_df[(smp_size+1):(nrow(obs_weigh_df)),],
                        obs_weigh_df = obs_weigh_df[(smp_size+1):(nrow(obs_weigh_df)),],
                        elev = elevation)

      testing <- round(o2$fit,2)

      write.table(data.frame('lake_id'=lake_id,'depth'= mean(input.values$max.d),'area'= mean(input.values$area_surface),'fit_train'= training,'fit_test'= testing,'fit_tall'= all,'n_obs'= nrow(obs_weigh_df)), paste0(folder_name, '/lakeinfo.txt'),
                  append = FALSE, sep = ",", dec = ".",
                  row.names = FALSE, col.names = TRUE) # input data --> B-ODEM

      print(o2$fit)
    } else {
      unlink(folder_name, recursive = TRUE, force = TRUE)
      print('Optimization failed.')
    }


    }
  }
} else {
  print('lake_id is probably wrong or non-existent in the database.')
  dbDisconnect(con)
}
