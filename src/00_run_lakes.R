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



