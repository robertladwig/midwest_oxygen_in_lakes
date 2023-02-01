library(odem.data)

setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes/')

library(MuMIn)
library(broom)
library(parallel)
library(MASS)
library(factoextra)
library(cluster)
library(tidyverse)
library(LakeMetabolizer)
library(dtw)
library(zoo)
library(patchwork)
library(rnaturalearth)
library(sf)
library(ggExtra)
library(ggmosaic)
# library(mapview)
library(data.table)
library(mlr)
library(nnet)
library(glmulti)
library(performance)# checks and compares quality of models
library(effects)
library(flextable)
library(vegan)
library(pROC)
library(multiROC)


data <- read_csv( 'processed_data/data_jan30.csv', col_names = T)





developed_interp <- c()
cultivated_interp <- c()
for (lakeid in unique(data$Hylak_id)){
  df <- data %>% filter(Hylak_id == lakeid)

  id.na_dev <- which(!is.na(df$developed))
  id.na_cul <- which(!is.na(df$cultivated))

  if (length(id.na_dev) != 0){
    interpolated_data_dev <- approx(df$year[id.na_dev], df$developed[id.na_dev], df$year, rule = 2, method = 'constant')$y
  } else {
    interpolated_data_dev <- rep(0, length(df$year))
  }
  if (length(id.na_cul) != 0){
    interpolated_data_cul <- approx(df$year[id.na_cul], df$cultivated[id.na_cul], df$year, rule = 2, method = 'constant')$y
  } else {
    interpolated_data_cul <-  rep(0, length(df$year))
  }


  developed_interp <- append(developed_interp, interpolated_data_dev)
  cultivated_interp <- append(cultivated_interp, interpolated_data_cul)
}

data$human_impact <- developed_interp + cultivated_interp

idx <- which(is.na(data$ct))
data <- data[-idx, ]


data_new <- data %>%
  mutate(ct = as.numeric(as.factor(data$ct)) - 1,
         depth = log10(depth),
         RT = log10(Res_time),
         WshA = (Wshd_area),
         area = log10(area)) %>%
  select(ct, human_impact, area ,depth,
            eutro ,RT)
data_new <- na.omit(data_new)


data_plot <- data %>%
  mutate(ct = as.numeric(as.factor(data$ct)) - 1,
         area = log10(area),
         depth = log10(depth),
         RT = log10(Res_time),
         WshA = log10(Wshd_area),
         ct = ifelse(ct == 0, 'High consumption', 'Low consumption'))
data_melt <- reshape2::melt(data_plot, id = 'ct')

landuse_plot <- ggplot(data_plot) +
  geom_density(aes(human_impact,  fill = ct, group = ct), alpha = 0.3) +
  scale_fill_manual(values = c('red4','lightblue1'), name = 'Cluster') +
  xlab('Human impact: developed + cultivated') +
  ylab('Density') +
  theme_minimal(base_size = 15)

area_plot <- ggplot(data_plot) +
  geom_density(aes(area,  fill = ct, group = ct), alpha = 0.3) +
  scale_fill_manual(values = c('red4','lightblue1'), name = 'Cluster') +
  xlab('log Area') +
  ylab('Density') +
  theme_minimal(base_size = 15)

depth_plot <- ggplot(data_plot) +
  geom_density(aes(depth,  fill = ct, group = ct), alpha = 0.3) +
  scale_fill_manual(values = c('red4','lightblue1'), name = 'Cluster') +
  xlab('log Depth') +
  ylab('Density') +
  theme_minimal(base_size = 15)

RT_plot <- ggplot(data_plot) +
  geom_density(aes(RT, fill = ct, group = ct), alpha = 0.3) +
  scale_fill_manual(values = c('red4','lightblue1'), name = 'Cluster') +
  xlab('log Residence time') +
  ylab('Density') +
  theme_minimal(base_size = 15)

eutro_plot <- ggplot(data_plot) +
  geom_density(aes(eutro, fill = ct, group = ct), alpha = 0.3) +
  scale_fill_manual(values = c('red4','lightblue1'), name = 'Cluster') +
  xlab('Eutrophic probability') +
  ylab('Density') +
  theme_minimal(base_size = 15)

fig2 <- landuse_plot +area_plot +depth_plot +eutro_plot +RT_plot + plot_layout(guides = 'collect')  +
  plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave(file = 'figs/Figure2.png', fig2,  dpi = 600, width =13, height = 8)


models_exhaust <- glmulti(ct ~ human_impact + area + (depth) +
          eutro + RT,
        data   = data_new,
        # crit   = aicc,       # AICC corrected AIC for small samples
        level  = 1,          # 2 with interactions, 1 without
        method = "h",        # "d", or "h", or "g"
        family = "binomial",
        fitfunction = glm,   # Type of model (LM, GLM etc.)
        confsetsize = 100)   # Keep 100 best models

plot(effects::allEffects(models_exhaust@objects[[1]]),
     lines = list(multiline = T),
     confint = list(style = "auto"))

weightable(models_exhaust)[1:10,] %>%
  regulartable() %>%       # beautifying tables
  autofit()

plot(models_exhaust)
plot(models_exhaust, type = "s")


model_averaged <- model.avg(object = models_exhaust@objects[c(1:2)])
# model_averaged <- models_exhaust@objects[c(1)]

# predicted data
data_new$prediction <- stats::predict(model_averaged, type = "response")

# create roc curve
roc_object <- pROC::roc(data_new$ct, data_new$prediction)

table <- table(Reality = data_new$ct,
               Prediction = ifelse(as.numeric(data_new$prediction) <= 0.50, 0, 1) )
(table[1,1]+table[2+2])/sum(table) * 100

data_new = data_new %>%
  mutate(prediction_ct = ifelse(as.numeric(data_new$prediction) <= 0.50, 0, 1),
         flag = ifelse(ct == prediction_ct, T, F))
summary(data_new$flag)


landuse_plot_flag <- ggplot(data_new) +
  geom_histogram(aes(human_impact,  fill = flag, group = flag), alpha = 0.3, position="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = '') +
  xlab('Human impact: developed + cultivated') +
  ylab('Density') +
  theme_minimal(base_size = 15)

area_plot_flag <- ggplot(data_new) +
  geom_histogram(aes(area,  fill = flag, group = flag), alpha = 0.3, position="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = '') +
  xlab('log Area') +
  ylab('Density') +
  theme_minimal(base_size = 15)

depth_plot_flag <- ggplot(data_new) +
  geom_histogram(aes(depth,  fill = flag, group = flag), alpha = 0.3, position="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = '') +
  xlab('log Depth') +
  ylab('Density') +
  theme_minimal(base_size = 15)

RT_plot_flag <- ggplot(data_new) +
  geom_histogram(aes(RT, fill = flag, group = flag), alpha = 0.3, position="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = '') +
  xlab('log Residence time') +
  ylab('Density') +
  theme_minimal(base_size = 15)

eutro_plot_flag <- ggplot(data_new) +
  geom_histogram(aes(eutro, fill = flag, group = flag), alpha = 0.3, position="identity") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = '') +
  xlab('Eutrophic probability') +
  ylab('Density') +
  theme_minimal(base_size = 15)

fig4 <- landuse_plot_flag +area_plot_flag +depth_plot_flag +eutro_plot_flag +RT_plot_flag+ plot_layout(guides = 'collect')  +
  plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom')
ggsave(file = 'figs/Figure4.png', fig4,  dpi = 600, width =13, height = 8)


### permutation analysis
train_fraction <- 0.7
reduced_model_data_interation <- 5000

model_result <- models_exhaust
# Run the specified number of model averaging iterations
for(rdmi in 1:reduced_model_data_interation){

  # Define data subset for modeling and sample a fraction for model training
  temp_lake_dat <- data_new %>%
    sample_frac(size = train_fraction,
                weight = as.factor(ct))

  # model_result <- glmulti(ct ~ human_impact + log10(area) + log10(depth) +
  #                           eutro + log10(RT),
  #                         data   = temp_lake_dat,
  #                         # crit   = aicc,       # AICC corrected AIC for small samples
  #                         level  = 1,          # 2 with interactions, 1 without
  #                         method = "h",        # "d", or "h", or "g"
  #                         family = "binomial",
  #                         fitfunction = glm,   # Type of model (LM, GLM etc.)
  #                         confsetsize = 100)   # Keep 100 best models

  #num_models <- length(model_result@crits[model_result@crits <= min(model_result@crits)+2])

  num_models <- 24

  AIC <- rep(0, length(model_result@formulas[c(1:num_models)]))
  MODEL <- rep(NA, length(model_result@formulas[c(1:num_models)]))
  AUC <- rep(0, length(model_result@formulas[c(1:num_models)]))
  RSQUARED <- rep(0, length(model_result@formulas[c(1:num_models)]))
  ACCURACY <- rep(0, length(model_result@formulas[c(1:num_models)]))
  PVALUE <- rep(0, length(model_result@formulas[c(1:num_models)]))
  for(i in 1:length(model_result@formulas[c(1:num_models)])){
    fit <- glm(paste(as.character(model_result@formulas[i])),
               data = temp_lake_dat,
               family = binomial)
    MODEL[i] <- paste(as.character(model_result@formulas[i]))
    AIC[i] <- fit$aic
    predictpr <- predict(fit, type = "response")
    ROC <- pROC::roc(temp_lake_dat$ct ~ predictpr)
    temp_lake_dat$PREDICTION <- predictpr
    AUC[i] <- pROC::auc(ROC)
    RSQUARED[i] <- 1 - (fit$deviance/fit$null.deviance)

    temp_lake_dat_permute <- temp_lake_dat %>%
      dplyr::mutate(PREDICTION = ifelse(as.numeric(PREDICTION) < 0.65, 0, 1))
    table <- table(Reality = temp_lake_dat_permute$ct,
                   Prediction = temp_lake_dat_permute$PREDICTION)
    ACCURACY[i] <- (table[1,1]+table[2+2])/sum(table)

    j <- 1
    nreps <- 1000
    AUC.repo <- rep(0, nreps)

    for(j in 1:nreps) {
      temp_lake_dat_permute$ct <- sample(temp_lake_dat_permute$ct,
                                                   size = length(temp_lake_dat_permute$ct),
                                                   replace = FALSE)
      predictpr <- predict(fit, type = "response")
      ROC <- pROC::roc(temp_lake_dat_permute$ct ~ predictpr, quiet = TRUE,
                       levels = c(0,1), direction = "<")
      AUC.repo[j] <- pROC::auc(ROC)
    }
    PVALUE[i] <- length(AUC.repo[AUC.repo > AUC[i]])/length(AUC.repo)

  }
  INDEX <- seq(1:length(model_result@formulas[c(1:num_models)]))
  lake_fits <- data.frame(INDEX, MODEL, AIC, RSQUARED, AUC, ACCURACY, PVALUE)
  lake_fits$MODEL <- as.character(lake_fits$MODEL)
  lake_fits$AIC <- as.numeric(lake_fits$AIC)
  lake_fits$RSQUARED <- as.numeric(lake_fits$RSQUARED)
  lake_fits$AUC <- as.numeric(lake_fits$AUC)
  lake_fits$ACCURACY <- as.numeric(lake_fits$ACCURACY)
  lake_fits$PVALUE <- as.numeric(lake_fits$PVALUE)

  lake_top_mod <- lake_fits %>%
    filter(AIC <= (min(AIC)+2)) %>%
    filter(RSQUARED >= median(RSQUARED),
           AUC >= median(AUC))

  lake_top_mod$MODEL <- gsub(pattern = "log00", replacement = "log10",
                             x = lake_top_mod$MODEL)


  lake_mod_fits <- map(.x = lake_top_mod$MODEL,
                      .f = ~ glm(formula = .x,
                                 family = "binomial",
                                 data = temp_lake_dat))

  out_path <- "permute_odem_model"
  # Create export folder if it doesn't exist
  ifelse(!dir.exists(file.path(out_path)),
         dir.create(file.path(out_path), recursive = TRUE), FALSE)

  # Export results
  if(length(lake_mod_fits) == 1) {

    results <- tidy(lake_mod_fits[[1]])

    write.csv(x = results,
              file = paste0(out_path, "/run_",
                            rdmi, ".csv"),
              row.names = FALSE)

  } else if (length(lake_mod_fits) == 0){

    tryCatch(write.csv(x = results,
                       file = paste0(out_path, "/run_",
                                     rdmi, ".csv"),
                       row.names = FALSE), error = function(e) NULL)

  } else {

    all_average <- model.avg(lake_mod_fits)

    results <- data.frame(summary(all_average)$coefmat.subset) %>%
      rename(estimate = Estimate,
             std.error = Std..Error,
             statistic = z.value,
             p.value = Pr...z..)
    results$term <- rownames(results)

    write.csv(x = results,
              file = paste0(out_path, "/run_",
                            rdmi, ".csv"),
              row.names = FALSE)
  }

}

run_files <- list.files(path = "permute_odem_model",
                        pattern = "run_", full.names = TRUE)

run_results <- map_df(.x = run_files,
                      .f = ~ read_csv(.x) %>%
                        select(estimate, term) %>%
                        mutate(run_number = .x,
                               run_number = gsub(pattern = "permute_odem_model/",
                                                 replacement = "", x = run_number),
                               run_number = gsub(pattern = ".csv",
                                                 replacement = "", x = run_number),
                               term = gsub(pattern = ".1",
                                           replacement = "", x = term)))


paramter_counts <- run_results %>%
  # filter(estimate >= -20,
  #        estimate <= 20) %>%
  unique() %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_replace(pattern = ":", replacement = ".", string = term),
         term = ifelse(term == "lo0(area)", "area", term),
         term = ifelse(term == "lo0(RT)", "RT", term),
         term = ifelse(term == "lo0(depth)", "depth", term)) %>%
  group_by(term) %>%
  count() #%>%
  #mutate(label = paste("Number of models:", n))

run_results_filtered <- run_results %>%
  ungroup() %>%
  # filter(estimate >= -10,
  #        estimate <= 10) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = str_replace(pattern = ":", replacement = ".", string = term),
         term = ifelse(term == "lo0(area)", "area", term),
         term = ifelse(term == "lo0(RT)", "RT", term),
         term = ifelse(term == "lo0(depth)", "depth", term),
         #term = str_replace(pattern = "", replacement = "", string = term),
         est_prob = exp(estimate)/(1+exp(estimate))) %>%
  unique() %>%
  inner_join(x = .,
             y = paramter_counts) %>%
  mutate(facet_label = paste(term, "(n =", n, ")"))

complete_results <- (model_averaged$coefficients)%>%
  data.frame() %>%
  rownames_to_column() %>%
  filter(rowname == "full") %>%
  rename(#"depth" = log10.depth.,
         "RT" = log10.RT.,
         "area" = log10.area.) %>%
  pivot_longer(cols = c(human_impact:RT), names_to = "term", values_to = "estimate")

run_results_filtered_plot <- run_results_filtered %>%
  mutate(term = ifelse(term == 'human_impact', 'Human impact', term),
         term = ifelse(term == 'area', 'Area', term),
         term = ifelse(term == 'depth', 'Depth', term),
         term = ifelse(term == 'eutro', 'Eutrophic probability', term),
         term = ifelse(term == 'RT', 'Residence time', term))

complete_results_plot <- complete_results %>%
  mutate(term = ifelse(term == 'human_impact', 'Human impact', term),
         term = ifelse(term == 'area', 'Area', term),
         term = ifelse(term == 'depth', 'Depth', term),
         term = ifelse(term == 'eutro', 'Eutrophic probability', term),
         term = ifelse(term == 'RT', 'Residence time', term))

all_plot <- ggplot() +
  geom_histogram(data = run_results_filtered_plot, aes(x = estimate)) +
  #geom_label(data = paramter_counts, aes(label = label, x = 0, y = 1000)) +
  geom_vline(data = complete_results_plot, aes(xintercept = estimate)) +
  ggtitle("Distribution of subsampled estimate values") +
  #xlim(-25, 25) +
  facet_wrap(vars(term), scales = "free_x")+ xlab('Parameter estimate') + ylab('Count') +
  theme_minimal(base_size = 15)

ggsave(file = 'analysis/figures/Figure3.png',
        dpi = 600, height = 6, width = 8)

