setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes')

source('R/helper.R')

all.nml <- list.files('inst/extdata/pball_nml/pball_nml_files/')
all.nml_short <- sub(".*pball_", "", all.nml)
all.nml_short2 <- sub(".nml.*", "", all.nml_short)

library(parallel)
library(MASS)



run_all <- function(x){
  lake_id <<- x
  source("src/01_data_merge_sql.R")
}

numCores <- detectCores()


all.dne <- list.files('metabolism_model/')
all.dne_all <- all.dne[grepl('nhdhr', all.dne)]

idx <- match(all.dne_all, all.nml_short2)
idy <- seq(1:length(all.nml_short2))

if (length(idx) == 0){
  all.nml_missing <- all.nml_short2[idy]
} else {
  all.nml_missing <- all.nml_short2[idy[-c(idx)]]
}

# system.time(
#   results <- mclapply(all.nml_missing, run_all, mc.cores = numCores)
# )

# Windows
library(parallel)


cl <- makeCluster(detectCores())
# cl <- clusterEvalQ(cl, { library(sf) })  # you need to export packages as well
# cl <- clusterExport(cl, "st_linestring")  # each worker is a new environment, you will need to export variables/functions to
system.time(
  # ll <- parallel::parLapply(cl = cl, all.nml_missing, run_all)    # if st_linestring takes arguments then st_linestring(x)
  results <- lapply(all.nml_missing, run_all)
)
stopCluster(cl)

# check 120018108

#low do nhdhr_120019016 
# problem: nhdhr_121622760
# 
# [1] "Running nhdhr_{4EA76133-68CD-4DA8-A82B-0B568CD9C9B2}"
# [1] "lake_id is probably wrong or non-existent in the database."
# [1] "Running nhdhr_{4F6A1C28-CFA9-45C9-B597-E2E6526275B8}"
# [1] "lake_id is probably wrong or non-existent in the database."
# [1] "Running nhdhr_{62B48949-8CCC-4BA3-989E-AA1FE86C363D}"
# [1] "lake_id is probably wrong or non-existent in the database."
# [1] "Running nhdhr_{E940A362-4076-4895-A23F-1B8CCC905DEE}"
# [1] "lake_id is probably wrong or non-existent in the database."
# [1] "Running nhdhr_120018007"
# [1] "Running nhdhr_120018981"
# [1] "Running nhdhr_121859022"
# [1] "Running nhdhr_143396594"
# [1] "Running nhdhr_143418891"
# [1] "Running nhdhr_151870593"
# [1] "Running nhdhr_152335372"
# [1] "Running nhdhr_152517931"
# [1] "Running nhdhr_152707734"
# [1] "Running nhdhr_155417639"
# [1] "Running nhdhr_155635994"
# [1] "Running nhdhr_155636695"
# [1] "Running nhdhr_155637270"
# [1] "Running nhdhr_155637595"
# [1] "Running nhdhr_34122432"
# [1] "Running nhdhr_34131807"
# [1] "Running nhdhr_69886228"
# [1] "Running nhdhr_72863203"
# [1] "Running nhdhr_74926805"
# [1] "Running nhdhr_91594833"
# [1] "Start optimization"