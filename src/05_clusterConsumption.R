        library(odem.data)

        setwd('C:/Users/ladwi/Documents/Projects/R/midwest_oxygen_in_lakes/')


        if (!exists("password")){
          password <- as.character(read.delim('sensitive/password.txt', header = FALSE, stringsAsFactor = FALSE))
        }

        all.nml <- list.files('inst/extdata/pball_nml/pball_nml_files/')
        all.nml_short <- sub(".*pball_", "", all.nml)
        all.nml_short2 <- sub(".nml.*", "", all.nml_short)

        library(parallel)
        library(MASS)
        library(factoextra)
        library(cluster)


        all.dne <- list.files('metabolism_model/')
        all.dne_all <- all.dne[grepl('nhdhr', all.dne)]


        library(tidyverse)
        library(LakeMetabolizer)
        library(dtw)
        library(zoo)
        library(patchwork)


        anoxDym <- list()
        anoxLab <- c()
        a=1
        lake.list <- all.dne_all
        morph <- data.frame('lake' = NULL,
                            'depth' = NULL,
                            'area' = NULL,
                            'evel' = NULL)
        for (i in lake.list){
          if (file.exists(paste0('metabolism_model/',i,'/modeled_o2.RData')) == FALSE){
            next
          }
          info <- read.csv(paste0('metabolism_model/',i,'/lakeinfo.txt'))
          if (info$fit_tall > 3 ){ #fit_train, 5
            next
          }
          load(paste0('metabolism_model/',i,'/modeled_o2.RData'))# load('Allequash/Allequash.Rda')
          data <- o2$df_kgml

          if (max(data$o2_hyp) > 20000 |  length(data$obs_hyp[!is.na(data$obs_hyp)]) < 5){
                  next
          }

          nml = glmtools::read_nml(paste0('inst/extdata/pball_nml/pball_nml_files/pball_',i,'.nml'))

          morph <- rbind(morph, data.frame('lake'=i,
                                           'depth'=mean(data$max.d),
                                           'area' = mean(data$area_surface),
                                           'elev' = max(nml$morphometry$H)))

          for (an in unique(data$year)){
            dataAnn = data[ which(data$year %in% an),]
            dataStrat = dataAnn[which(dataAnn$strat == 1),]
            if ((max(dataStrat$doy) - min(dataStrat$doy)) <2){
              next
            }
            dataStrat$Sat_hypo <- o2.at.sat.base(temp = dataStrat$temperature_hypo , altitude = max(nml$morphometry$H)) * 1000
            dataStrat$dist <- (dataStrat$doy - min(dataStrat$doy)) / (max(dataStrat$doy) - min(dataStrat$doy))
            dataStrat$normalDO <- dataStrat$o2_hyp / dataStrat$Sat_hypo
            scaledDOdiff <- approx(dataStrat$dist, dataStrat$normalDO , seq(0,1,0.01))

            if (max(scaledDOdiff$y) > 2){
                    print(i)
                    print(dataStrat$normalDO )
                    print(dataStrat$o2_hyp)
                    print(dataStrat$Sat_hypo)
                    print(scaledDOdiff$y)
                    readline(prompt="Press [enter] to continue")
            }

            anoxDym[[a]] = scaledDOdiff$y
            a=a+1
            anoxLab <- append(anoxLab, paste0(i,'_',an))
          }
        }

        anoxDym2 = lapply(anoxDym,rollmean,k = 10)

        mydata = (as.matrix(as.data.frame(anoxDym2)))
        wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))

        for (i in 2:10) wss[i] <- sum(kmeans(mydata,
                                             centers=i)$withinss)
        plot(1:10, wss, type='b', xlab='Number of Clusters',
             ylab='Within groups sum of squares')

        df = mydata
        g.elb <- fviz_nbclust(df, kmeans, method = 'wss')
        print(g.elb)
        avg_sil <- function(k) {
          km.res <- kmeans(df, centers = k, nstart = 25)
          ss <- silhouette(km.res$cluster, dist(df))
          mean(ss[, 3])
        }
        # Compute and plot wss for k = 2 to k = 15
        k.values <- 2:15
        # extract avg silhouette for 2-15 clusters
        avg_sil_values <- map_dbl(k.values, avg_sil)
        plot(k.values, avg_sil_values,
             type = "b", pch = 19, frame = FALSE,
             xlab = "Number of clusters K",
             ylab = "Average Silhouettes")
        g.sil <- fviz_nbclust(df, kmeans, method = "silhouette")
        print(g.sil)

        distMatrix <- dist(anoxDym2, method= 'euclidean')
        hc <- hclust(distMatrix, method='ward.D')
        plot(hc, main='')
        groups <- cutree(hc, k=2) #k=5) # cut tree into 7 clusters

        rect.hclust(hc, k=2,border='red')#k=7
        indMycl = unique(groups)
        dataGroups = list()
        dataGroups_var = list()
        idz = as.numeric(table(groups))
        for (i in 1:length(indMycl)){
          idy = which(groups == i)
          data = anoxDym2[idy]

          data.df = as.data.frame(do.call(cbind, data))
          dataGroups[[i]] = apply(data.df,1, mean)
          dataGroups_var[[i]] = apply(data.df,1, sd)

          data.long = data.df %>%
            mutate(x = 1:92) %>%
            pivot_longer(-x)

          # Individual Cluster Plots
          p1 = ggplot(data.long, aes(x, value, colour=name)) +
            geom_line() +
            theme(legend.position = "none") +
            labs(title = paste0('Cluster = ',i,' n = ',idz[i]))
          print(p1)
        }

        df = as.data.frame(dataGroups)
        names(df) = c("Heavy consumption", "Low consumption")#c('Semi-bad','Good','Bad')#c('Semi-bad','Good','Bad')#,'Convex')
        
        df_sd = as.data.frame(dataGroups_var)
        names(df_sd) =names(df)
        
        nameVec = names(df)
        df$depth = seq(1,nrow(df))
        df_sd$depth = df$depth
        table(groups)
        lakeinv <- nameVec[unique(which(table(groups) > 0))]# >5

        table(groups)[1]

        # Pivot wide for plotting
        df.long = df %>%
          dplyr::select(lakeinv, depth) %>%
          pivot_longer(lakeinv) %>%
          mutate(name = fct_relevel(name,  "Heavy consumption", "Low consumption"))
        
        df_sd_upper <- df + df_sd
        df_sd_upper$depth = df$depth
        # Pivot wide for plotting
        df.upper = df_sd_upper %>%
          dplyr::select(lakeinv, depth) %>%
          pivot_longer(lakeinv) %>%
          mutate(name = fct_relevel(name,  "Heavy consumption", "Low consumption"))

        
        df_sd_lower <- df - df_sd
        df_sd_lower$depth = df$depth
        # Pivot wide for plotting
        df.lower = df_sd_lower %>%
          dplyr::select(lakeinv, depth) %>%
          pivot_longer(lakeinv) %>%
          mutate(name = fct_relevel(name,  "Heavy consumption", "Low consumption"))
        
        # Cluster lables
        cluster.labels = NA

        order = match(lakeinv, c("Heavy consumption", "Low consumption"))
        for (i in 1:3){
          j = order[i]
          cluster.labels[j] = paste0(lakeinv[i],' (n = ',table(groups)[i],')')
        }

        # Cluster plotting
        g.cluster = ggplot(df.long) +
          geom_line(aes(depth, value, color = name)) +
          geom_line(data = df.upper, aes(depth, value, color = name), linetype = 'dashed') +
          geom_line(data = df.lower, aes(depth, value, color = name), linetype = 'dashed') +
          scale_color_manual(values = c('red4','lightblue1','gold','red1','red4'), name = 'Cluster',
                             labels = cluster.labels) +
          xlab('Stratification duration [%]') +
          ylab('Ratio of Hypolimnion to \nSaturation DO [-]') +
          theme_minimal(base_size = 8) +
          theme(axis.title.y = element_text(vjust = -10)); g.cluster

        # Grid Plot
        df.grd <-  setNames(data.frame(matrix(ncol = 1+length(seq(1979, 2018,1)), nrow = length(lake.list))), c('lake',
                                                                                                  as.character(seq(1979,2018,1))))
        df.grd$lake <- lake.list

        for (i in 1:2) {
          dff = anoxLab[which(groups == i)]
          name = lakeinv[i]
          dmlst <- lake.list[!is.na(match(lake.list, unlist(substr(dff,1,nchar(dff)-5))))]
          for (j in dmlst){
            xrow = which(df.grd$lake == j)
            whpatt = grep(j, dff)
            whyrs = gsub(".*_","",dff[grep(j, dff)])
            df.grd[xrow, !is.na(match(colnames(df.grd),whyrs))] <- name
          }
        }

        m.df.grd <- reshape2::melt(df.grd, id.vars = 'lake')

        g1 <- ggplot(m.df.grd, aes(x = variable, y = lake, fill = as.factor(value))) +
          scale_fill_manual(values = c('red4','lightblue1','gold','red1','red4'), name = 'Cluster',
                            breaks = c("Heavy consumption", "Low consumption")) +
          geom_tile(color = 'black', width = 0.8, height = 0.8, size = 0.5) +
          labs(x = 'Time', y = '') +
          theme_minimal(base_size = 8) +
          theme(axis.text.x = element_text(angle = 45, size = 5),
                axis.title.x = element_blank()); g1

        # ggplot(m.df.grd, aes(x = as.numeric(variable), y = as.numeric(as.factor(value)), col = as.factor(lake))) +
        #   scale_fill_manual(values = c('red4','gold','lightblue1','red1','red4'), name = 'Cluster',
        #                     breaks = c('SemiBad','Good','Bad')) +
        #   geom_line()+
        #   labs(x = 'Time', y = '') +
        #   theme_minimal(base_size = 8) +
        #   theme(axis.text.x = element_text(angle = 45, size = 5),
        #         axis.title.x = element_blank(),
        #         legend.position = 'none')

        g <- g.cluster / g1 + plot_layout(heights = c(1.5,2))  + plot_annotation(tag_levels = 'A'); g
        # ggsave(file = 'analysis/figures/cluster.png', g.cluster, dpi = 500, width =6, height = 4)
        # ggsave(file = 'analysis/figures/cannual.png', g1, dpi = 600, width =30, height = 20)


        types = m.df.grd %>%
          group_by(lake, variable) %>%
          mutate(type = (factor(value))) %>%
          rename(year = variable) %>%
          summarize(ct = names(which.max(table(type)))) # constant convex linear
        
        write_csv(x = types, file = 'processed_data/consumptiontype_jan30.csv', col_names = T)
        write_csv(x = morph, file = 'processed_data/morphometry.csv', col_names = T)
        write_csv(x = df.long, file = 'processed_data/cluster.csv', col_names = T)
        
        
        # 
        # 
        # 
        # troph <- read_csv("inst/extdata/reflectance/ensemble_preds.csv")
        # link <- read_csv('inst/extdata/reflectance/nhd_hydrolakes_nla.csv')
        # 
        # 
        # types$Hylak_id <- link$Hylak_id[match(types$lake, link$site_id)]
        # 
        # troph_df <- troph %>%
        #   rename(eutro = `mean_prob_prob_eu/mixo`,
        #          oligo = mean_prob_prob_oligo,
        #          dys = mean_prob_prob_dys) %>%
        #   select(Hylak_id, year, eutro, oligo, dys)
        # 
        # types <- merge(types, troph_df, by = c("Hylak_id", "year"))
        # 
        # library(sf)
        # hydLakes <- read_sf(dsn = "inst/extdata/HydroLAKES/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.shp")
        # 
        # types$RT <- (hydLakes$Res_time[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])
        # types$WshA <- (hydLakes$Wshd_area[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])
        # types$dep_avg <- (hydLakes$Depth_avg[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])
        # 
        # col <- read_csv('inst/extdata/reflectance/limnosat_redux_raw_rel_reflectance_ptl_color.csv')
        # 
        # data <- data %>%
        #   mutate(trophic = case_when(eutro > oligo & eutro > dys  ~ 'eutro', # & eutro >= 0.75
        #                              oligo > eutro & oligo > dys ~ 'oligo', #  & oligo  >= 0.75
        #                              dys > eutro & dys > oligo   ~ 'dys')) %>% #& dys  >= 0.75
        #   mutate(trophic = ifelse(is.na(trophic), 'gray', trophic))
        # 
        # data = as.data.frame(na.omit(types))
        # 
        # write_csv(x = types, file = 'processed_data/data_jan30.csv', col_names = T)
        
        
        
        
        
        
        
        
