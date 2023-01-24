        library(odem.data)

        setwd('~/Documents/DSI/odem.data/')


        if (!exists("password")){
          password <- as.character(read.delim('analysis/scripts/password.txt', header = FALSE, stringsAsFactor = FALSE))
        }

        all.nml <- list.files('inst/extdata/pball_nml/pball_nml_files/')
        all.nml_short <- sub(".*pball_", "", all.nml)
        all.nml_short2 <- sub(".nml.*", "", all.nml_short)

        library(parallel)
        library(MASS)
        library(factoextra)
        library(cluster)


        all.dne <- list.files('analysis/')
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
          if (file.exists(paste0('analysis/',i,'/modeled_o2.RData')) == FALSE){
            next
          }
          info <- read.csv(paste0('analysis/',i,'/lakeinfo.txt'))
          if (info$fit_tall > 3 | info$n_obs <= 10){ #fit_train, 5
            next
          }
          load(paste0('analysis/',i,'/modeled_o2.RData'))# load('Allequash/Allequash.Rda')
          data <- o2$df_kgml

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
        groups <- cutree(hc, k=3) #k=5) # cut tree into 7 clusters

        rect.hclust(hc, k=3,border='red')#k=7
        indMycl = unique(groups)
        dataGroups = list()
        idz = as.numeric(table(groups))
        for (i in 1:length(indMycl)){
          idy = which(groups == i)
          data = anoxDym2[idy]

          data.df = as.data.frame(do.call(cbind, data))
          dataGroups[[i]] = apply(data.df,1, mean)

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
        names(df) = c('Semi-bad','Good','Bad')#c('Semi-bad','Good','Bad')#,'Convex')

        nameVec = names(df)
        df$depth = seq(1,nrow(df))
        table(groups)
        lakeinv <- nameVec[unique(which(table(groups) > 0))]# >5

        table(groups)[1]

        # Pivot wide for plotting
        df.long = df %>%
          dplyr::select(lakeinv, depth) %>%
          pivot_longer(lakeinv) %>%
          mutate(name = fct_relevel(name,  'Semi-bad','Good','Bad'))

        # Cluster lables
        cluster.labels = NA

        order = match(lakeinv, c('Semi-bad','Good','Bad'))
        for (i in 1:3){
          j = order[i]
          cluster.labels[j] = paste0(lakeinv[i],' (n = ',table(groups)[i],')')
        }

        # Cluster plotting
        g.cluster = ggplot(df.long) +
          geom_line(aes(depth, value, color = name)) +
          scale_color_manual(values = c('red4','gold','lightblue1','red1','red4'), name = 'Cluster',
                             labels = cluster.labels) +
          xlab('Stratification duration [%]') +
          ylab('Ratio of Hypolimnion to \nSaturation DO [-]') +
          theme_minimal(base_size = 8) +
          theme(axis.title.y = element_text(vjust = -10)); g.cluster

        # Grid Plot
        df.grd <-  setNames(data.frame(matrix(ncol = 1+length(seq(1979, 2018,1)), nrow = 207)), c('lake',
                                                                                                  as.character(seq(1979,2018,1))))
        df.grd$lake <- lake.list

        for (i in 1:3) {
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
          scale_fill_manual(values = c('red4','gold','lightblue1','red1','red4'), name = 'Cluster',
                            breaks = c('Semi-bad','Good','Bad')) +
          geom_tile(color = 'black', width = 0.8, height = 0.8, size = 0.5) +
          labs(x = 'Time', y = '') +
          theme_minimal(base_size = 8) +
          theme(axis.text.x = element_text(angle = 45, size = 5),
                axis.title.x = element_blank()); g1

        ggplot(m.df.grd, aes(x = as.numeric(variable), y = as.numeric(as.factor(value)), col = as.factor(lake))) +
          # scale_fill_manual(values = c('red4','gold','lightblue1','red1','red4'), name = 'Cluster',
                            # breaks = c('SemiBad','Good','Bad')) +
          geom_line()+
          labs(x = 'Time', y = '') +
          theme_minimal(base_size = 8) +
          theme(axis.text.x = element_text(angle = 45, size = 5),
                axis.title.x = element_blank(),
                legend.position = 'none')

        g <- g.cluster / g1 + plot_layout(heights = c(1.5,2))  + plot_annotation(tag_levels = 'A'); g
        ggsave(file = 'analysis/figures/cluster.png', g.cluster, dpi = 500, width =6, height = 4)
        ggsave(file = 'analysis/figures/cannual.png', g1, dpi = 600, width =30, height = 20)


        types = m.df.grd %>%
          group_by(lake) %>%
          mutate(type = (factor(value))) %>%
          summarize(ct = names(which.max(table(type)))) # constant convex linear

        landuse = read_csv('lstm/landuse.csv')
        types$lndu <- factor(landuse$LANDUSE[match(types$lake,landuse$nhdr_id)])
        types$ct = factor(types$ct)

        types$developed <- landuse$developed[match(types$lake,landuse$nhdr_id)]/100
        types$forest <- landuse$forest[match(types$lake,landuse$nhdr_id)]/100
        types$cultivated <- landuse$cultivated[match(types$lake,landuse$nhdr_id)]/100
        types$wetlands <- landuse$wetlands[match(types$lake,landuse$nhdr_id)]/100

        morph$depthf <- cut(morph$depth, c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 80, 250, Inf))
        morph$areaf <- cut(morph$area, c(0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e9))

        types$depthf = morph$depthf[match(types$lake,morph$lake)]
        types$areaf = morph$areaf[match(types$lake,morph$lake)]

        types$depth = (morph$depth[match(types$lake,morph$lake)])
        types$area = (morph$area[match(types$lake,morph$lake)])

        types$elev = (morph$elev[match(types$lake,morph$lake)])

        # 187 lakes
        ## 75% of the sample size
        # troph_old <- read_csv('analysis/figures/ts_oxygen_join.csv')
        # troph <- read_csv('analysis/figures/complete_lake_predictions.csv')
        troph <- read_csv("lstm/ensemble_preds.csv")
        link <- read_csv('analysis/figures/nhd_hydrolakes_nla.csv')

        types$Hylak_id <- link$Hylak_id[match(types$lake, link$site_id)]

        # sum(unique(match(troph$Hylak_id, link$Hylak_id)), na.rm = T)
        # # 975470004
        # length(unique(link$site_id[match(troph$Hylak_id, link$Hylak_id)]))
        # # 39777
        # sum(!is.na(match(types$lake,link$site_id[match(troph$Hylak_id, link$Hylak_id)])))
        # # 170
        # sum(!is.na( link$Hylak_id[match(types$lake,link$site_id[match(troph$Hylak_id, link$Hylak_id)])]))
        # # 18
        # types$site_id <- link$Hylak_id[match(types$lake,link$site_id[match(troph$Hylak_id, link$Hylak_id)])]
        # types$site_id <- link$Hylak_id[match(link$site_id[match(troph$Hylak_id, link$Hylak_id)],types$lake)]

        types$eutro <- troph$`mean_prob_prob_eu/mixo`[match(types$Hylak_id,troph$Hylak_id)]
        types$dys <- troph$mean_prob_prob_dys[match(types$Hylak_id,troph$Hylak_id)]
        types$oligo <- troph$mean_prob_prob_oligo[match(types$Hylak_id,troph$Hylak_id)]

        # residence times
        library(sf)
        hydLakes <- read_sf(dsn = "inst/extdata/HydroLAKES/HydroLAKES_points_v10_shp/HydroLAKES_points_v10.shp")

        types$RT <- (hydLakes$Res_time[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])
        types$WshA <- (hydLakes$Wshd_area[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])
        types$dep_avg <- (hydLakes$Depth_avg[match(troph$Hylak_id[match(types$Hylak_id,troph$Hylak_id)], hydLakes$Hylak_id)])

        col <- read_csv('analysis/figures/limnosat_redux_raw_rel_reflectance_ptl_color.csv')
        # types$NDVI <- (col$Nir_raw[match(troph$Hylak_id[match(types$lake,troph$site_id)], col$Hylak_id)] - col$Red_raw[match(troph$Hylak_id[match(types$lake,troph$site_id)], col$Hylak_id)]) /
        #   (col$Nir_raw[match(troph$Hylak_id[match(types$lake,troph$site_id)], col$Hylak_id)] + col$Red_raw[match(troph$Hylak_id[match(types$lake,troph$site_id)], col$Hylak_id)])



        data = as.data.frame(na.omit(types))

        ################
        library(RPostgreSQL)
        library(rnaturalearth)
        # library(mapview)
        require('RPostgreSQL')
        # dbDisconnect(con)
        drv <- dbDriver("PostgreSQL") ##this is where you call the name of your ODBC connection

        ##create connection (I use the server id here but the url name works too)
        con <- dbConnect(drv,
                         dbname = "UW_data_science",
                         host = '144.92.62.199',
                         port = 5432,
                         user = "postgres",
                         password = password)
        ###try and make map of all lakes from Jordan's data
        lake_metrics <- dbGetQuery(con,'select * from data.lake_metrics', stringsAsFactors = FALSE)
        lake_metrics_reduced <- lake_metrics[!is.na(match(lake_metrics$nhd_lake_id, data$lake)),]


        data$longitude <- lake_metrics$longitude[!is.na(match(lake_metrics$nhd_lake_id, data$lake))]
        data$latitude <- lake_metrics$latitude[!is.na(match(lake_metrics$nhd_lake_id, data$lake))]

        data <- data %>%
          mutate(trophic = case_when(eutro > oligo & eutro > dys  ~ 'eutro', # & eutro >= 0.75
                                     oligo > eutro & oligo > dys ~ 'oligo', #  & oligo  >= 0.75
                                     dys > eutro & dys > oligo   ~ 'dys')) %>% #& dys  >= 0.75
          mutate(trophic = ifelse(is.na(trophic), 'gray', trophic))

        write_csv(x = types, file = 'analysis/figures/rawdata_nov18.csv', col_names = T)
        write_csv(x = data.frame('nhdhr' = types$lake), file = 'analysis/figures/my_nhdhr.csv', col_names = T)
        write_csv(x = data, file = 'analysis/figures/data_nov18.csv', col_names = T)

        #
        # data <- read_csv('analysis/figures/data_nov18.csv', col_names = T)
        #
        # gmap <- ggplot() +
        #   # geom_sf(color = "black") +
        #   geom_polygon(data = us, aes(x=long, y=lat,
        #                               group = group), color = "black", fill = 'white',
        #                size =.5, alpha = 0) +
        #   # geom_point(data = data, aes(longitude, latitude, col = lndu), size = 3) +
        #   geom_point(data = data, aes(longitude, latitude, col = lndu, shape = ct)) +
        #   # scale_color_gradient(low="darkgreen", high="red") +
        #   coord_sf(xlim = c(-98, -84), ylim = c(42, 50), expand = FALSE) +
        #   xlab('Longitude') + ylab('Latitude') +
        #   theme_light(); gmap
        # ggsave(file = 'analysis/figures/map.png', gmap, dpi = 600, width =15, height = 10)
        #
        # map_data[which(data$lake == 'nhdhr_143249470'),]
        #
        #
        #
        #
        # # m = mapview(map_data, xcol = "longitude", ycol = "latitude", zcol = "eutro",cex = 2, crs = 4269, at = seq(0,1,0.1),legend = TRUE,grid = FALSE); m
        # # mapshot(m, file = 'analysis/figures/map.png', remove_controls = c("homeButton", "layersControl"), selfcontained = FALSE)
        # ################
        #
        #
        #
        # normalize <- function(x) {
        #   num <- x - min(x)
        #   denom <- max(x) - min(x)
        #   return (num/denom)
        # }
        #
        #
        # # smp_size <- floor(0.75 * nrow(data))
        # smp_size_1 <- floor(0.75 * nrow(data[which(data$ct == 'Bad'),]))
        # smp_size_2 <- floor(0.75 * nrow(data[which(data$ct == 'Good'),]))
        # smp_size_3 <- floor(0.75 * nrow(data[which(data$ct == 'SemiBad'),]))
        #
        # ## set the seed to make your partition reproducible
        # set.seed(123)
        # # train_ind <- sample(seq_len(nrow(data)), size = smp_size)
        # train_ind_1 <- sample(seq_len(nrow(data[which(data$ct == 'Bad'),])), size = smp_size_1)
        # train_ind_2 <- sample(seq_len(nrow(data[which(data$ct == 'Good'),])), size = smp_size_2)
        # train_ind_3 <- sample(seq_len(nrow(data[which(data$ct == 'SemiBad'),])), size = smp_size_3)
        #
        # train <- data[c(train_ind_1,train_ind_2), ]
        # test <- data[-c(train_ind_1,train_ind_2), ]
        #
        # library(vegan)
        # # https://jkzorz.github.io/2020/04/04/NMDS-extras.html
        # df_data = data[,c("ct", "developed", "forest", "cultivated", "wetlands",
        #                   "depth", "area", "elev", "eutro", "dys", "oligo", "RT", "WshA",
        #                   "dep_avg")]
        # ext = df_data[, c("developed", "forest", "cultivated", "wetlands")]
        # int = df_data[, c("ct", "depth", "area", "elev", "eutro", "dys", "oligo", "RT","dep_avg","WshA")]
        #
        # ext_com = as.matrix(ext)
        #
        # #nmds code
        # set.seed(123)
        # nmds = metaMDS(ext_com, distance = "bray", k =3)
        # nmds
        #
        # en = envfit(nmds, int, permutations = 999, na.rm = TRUE)
        # en
        #
        # plot(nmds)
        # plot(en)
        #
        # data.scores = as.data.frame(scores(nmds))
        # data.scores$trophic <- data$ct
        #
        # en_coord_cont = as.data.frame(scores(en, "vectors")) * ordiArrowMul(en)
        # en_coord_cat = as.data.frame(scores(en, "factors")) * ordiArrowMul(en)
        #
        # g.nmds_1d <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS2)) +
        #   geom_point(data = data.scores, aes(colour = trophic), size = 3, alpha = 0.5) +
        #   # scale_colour_manual(values = c("orange", "steelblue", "red4"))  +
        #   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
        #                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
        #   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2),
        #              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
        #   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04),
        #             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") +
        #   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30",
        #             fontface = "bold", label = row.names(en_coord_cont)) +
        #   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"),
        #         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(),
        #         legend.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         legend.text = element_text(size = 9, colour = "grey30")) +
        #   labs(colour = "Landuse")
        #
        # g.nmds_2d <- ggplot(data = data.scores, aes(x = NMDS1, y = NMDS3)) +
        #   geom_point(data = data.scores, aes(colour = trophic), size = 3, alpha = 0.5) +
        #   # scale_colour_manual(values = c("orange", "steelblue", "red4"))  +
        #   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
        #                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
        #   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2),
        #              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
        #   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04),
        #             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") +
        #   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30",
        #             fontface = "bold", label = row.names(en_coord_cont)) +
        #   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"),
        #         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(),
        #         legend.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         legend.text = element_text(size = 9, colour = "grey30")) +
        #   labs(colour = "Landuse")
        #
        # g.nmds_3d <- ggplot(data = data.scores, aes(x = NMDS2, y = NMDS3)) +
        #   geom_point(data = data.scores, aes(colour = trophic), size = 3, alpha = 0.5) +
        #   # scale_colour_manual(values = c("orange", "steelblue", "red4"))  +
        #   geom_segment(aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
        #                data = en_coord_cont, size =1, alpha = 0.5, colour = "grey30") +
        #   geom_point(data = en_coord_cat, aes(x = NMDS1, y = NMDS2),
        #              shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
        #   geom_text(data = en_coord_cat, aes(x = NMDS1, y = NMDS2+0.04),
        #             label = row.names(en_coord_cat), colour = "navy", fontface = "bold") +
        #   geom_text(data = en_coord_cont, aes(x = NMDS1, y = NMDS2), colour = "grey30",
        #             fontface = "bold", label = row.names(en_coord_cont)) +
        #   theme(axis.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         panel.background = element_blank(), panel.border = element_rect(fill = NA, colour = "grey30"),
        #         axis.ticks = element_blank(), axis.text = element_blank(), legend.key = element_blank(),
        #         legend.title = element_text(size = 10, face = "bold", colour = "grey30"),
        #         legend.text = element_text(size = 9, colour = "grey30")) +
        #   labs(colour = "Landuse")
        #
        # g.nmds = g.nmds_1d + g.nmds_2d + g.nmds_3d; g.nmds
        # ggsave(file = 'analysis/figures/nmds.png', g.nmds, dpi = 600, width =14, height = 5)
        #
        #
        # library(data.table)
        # library(mlr)
        # library(nnet)
        # library(glmulti)
        # library(performance)# checks and compares quality of models
        # library(effects)
        # library(flextable)
        # # library(sjPlot)
        #
        # glmulti(ct   ~ lndu + developed + forest + cultivated + wetlands + depth + area + elev + eutro + dys + oligo + log10(RT) + log10(WshA) + log10(dep_avg) +
        #           trophic,
        #         data   = data,
        #         # crit   = aicc,       # AICC corrected AIC for small samples
        #         level  = 1,          # 2 with interactions, 1 without
        #         method = "d",        # "d", or "h", or "g"
        #         # family = gaussian,
        #         fitfunction = multinom,   # Type of model (LM, GLM etc.)
        #         confsetsize = 100)   # Keep 100 best models
        #
        # glmulti(ct   ~ lndu + developed + forest + cultivated + wetlands + depth + area + elev + eutro + dys + oligo + RT + WshA + dep_avg ,
        #         data   = data,
        #         # crit   = aicc,       # AICC corrected AIC for small samples
        #         level  = 2,          # 2 with interactions, 1 without
        #         method = "d",        # "d", or "h", or "g"
        #         # family = gaussian,
        #         fitfunction = multinom,   # Type of model (LM, GLM etc.)
        #         confsetsize = 100)   # Keep 100 best models
        #
        # glmulti(ct   ~ lndu + depth + area + elev + eutro + dys + oligo + RT + WshA + dep_avg  ,
        #         data   = data,
        #         # crit   = aicc,       # AICC corrected AIC for small samples
        #         level  = 1,          # 2 with interactions, 1 without
        #         method = "d",        # "d", or "h", or "g"
        #         # family = gaussian,
        #         fitfunction = multinom,   # Type of model (LM, GLM etc.)
        #         confsetsize = 100)   # Keep 100 best models
        #
        # # lndu + developed + forest + cultivated + wetlands + depth + area + elev + eutro + dys + oligo + RT + WshA + dep_avg
        # test_h <- glmulti(ct   ~lndu + developed + forest + cultivated + wetlands + depth + area + elev + eutro + dys + oligo + RT + WshA + dep_avg,
        #                   data   = data,
        #                   # crit   = aicc,       # AICC corrected AIC for small samples
        #                   level  = 1,          # 2 with interactions, 1 without
        #                   method = "h",        # "d", or "h", or "g"
        #                   # family = gaussian,
        #                   fitfunction = multinom,   # Type of model (LM, GLM etc.)
        #                   confsetsize = 100)   # Keep 100 best models
        #
        # test_h <- glmulti(ct   ~ lndu + developed + forest + cultivated + wetlands + depth + area + elev +
        #                     eutro + dys + oligo + log10(RT) + log10(WshA) + log10(dep_avg) + trophic,# + trophic,
        #                   data   = data,
        #                   # crit   = aicc,       # AICC corrected AIC for small samples
        #                   level  = 1,          # 2 with interactions, 1 without
        #                   method = "h",        # "d", or "h", or "g"
        #                   # family = gaussian,
        #                   fitfunction = multinom,   # Type of model (LM, GLM etc.)
        #                   confsetsize = 100)   # Keep 100 best models
        #
        # # test_h <- glmulti(ct   ~ lndu + developed + forest + cultivated + wetlands + depth + area + elev +
        # #                     eutro + dys + oligo + log10(RT) + log10(WshA) + log10(dep_avg),# + trophic,
        # #                   data   = data,
        # #                   # crit   = aicc,       # AICC corrected AIC for small samples
        # #                   level  = 1,          # 2 with interactions, 1 without
        # #                   method = "h",        # "d", or "h", or "g"
        # #                   family = binomial,
        # #                   fitfunction = glm,   # Type of model (LM, GLM etc.)
        # #                   confsetsize = 100)   # Keep 100 best models
        #
        # optimal_model_glmulti_exhaustive <- test_h@objects[[1]]
        # print(optimal_model_glmulti_exhaustive)
        #
        # png(file='analysis/figures/effects.png', width = 25, height = 20, units = "cm",res = 600)
        # plot(effects::allEffects(test_h@objects[[1]]),
        #      lines = list(multiline = T),
        #      confint = list(style = "auto"))
        # dev.off()
        #
        # plot(test_h)
        #
        # weightable(test_h)[1:6,] %>%
        #   regulartable() %>%       # beautifying tables
        #   autofit()
        #
        # plot(test_h, type = "s")
        #
        # best_model <- test_h@objects[[2]]
        #
        # car::Anova(best_model)
        #
        # # plot_model(best_model, type = "int") %>%
        # #   plot_grid()
        #
        # model <- multinom(ct ~ dys * lndu * depth * eutro * oligo * area/WshA + RT, data = train)
        # model <- multinom(ct ~ normalize(area)/normalize(WshA)  * lndu * normalize(RT) + eutro * oligo * dys + normalize(depth) , data = train)
        # model <- multinom(ct ~ lndu * normalize(RT) + eutro * oligo * dys + normalize(depth) , data = train)
        # model <- multinom(ct ~ developed + forest + cultivated + wetlands + depth + area + oligo + trophic, data = train)
        #
        #
        # # model <- multinom(ct ~ dys * eutro * oligo + lndu + depth * area/WshA * RT, data = train)
        #
        # # model <- nnet(ct ~ dys + lndu + depth  + eutro + oligo + area + RT + elev +
        # #                 Red * Green * Blue * Nir, data = train, size = 10)
        #
        # # model <- multinom(ct ~  developed *forest * cultivated * wetlands + depth * area + eutro * oligo * dys + Red * Green * Blue * Nir + RT + elev, data = train)
        #
        # summary(model)
        # summary(model)$AIC
        # z <- summary(model)$coefficients/summary(model)$standard.errors
        # p <- (1 - pnorm(abs(z), 0, 1)) * 2
        # print(p)
        #
        # # https://datasciencebeginners.com/2018/12/20/multinomial-logistic-regression-using-r/
        # # https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
        # ## extracting coefficients from the model and exponentiate
        # exp(coef(model))
        #
        # head(probability.table <- fitted(model))
        #
        # # Predicting the values for train dataset
        # train$precticed <- predict(model, newdata = train, "class")
        #
        # # Building classification table
        # ctable <- table(train$ct, train$precticed)
        #
        # # Calculating accuracy - sum of diagonal elements divided by total obs
        # round((sum(diag(ctable))/sum(ctable))*100,2)
        #
        # # Predicting the values for train dataset
        # test$precticed <- predict(model, newdata = test, "class")
        #
        # # Building classification table
        # ctable <- table(test$ct, test$precticed)
        #
        # # Calculating accuracy - sum of diagonal elements divided by total obs
        # round((sum(diag(ctable))/sum(ctable))*100,2)
        #
        # pscl::pR2(model)["McFadden"]
        #
        # library(pROC)
        # library(multiROC)
        #
        # # predicted data
        # prediction <- predict(model, test, type="probs")
        #
        #
        # # create roc curve
        # roc_object <- multiclass.roc( test$ct, prediction)
        #
        #
        #
        # truerel <-data.frame(var = test$ct, value = 1) %>%
        #   rownames_to_column() %>%
        #   pivot_wider(names_from = var, values_from = value) %>%
        #   replace_na(list(Bad = 0,
        #                   Good = 0,
        #                   #  Convex = 0,
        #                   SemiBad = 0)) %>%
        #   select(-rowname) %>%
        #   rename(Bad_true = Bad,
        #          Good_true = Good,
        #          # Convex_true = Convex,
        #          SemiBad_true = SemiBad) %>%
        #   data.frame() %>%
        #   bind_cols(., prediction %>%
        #               data.frame() %>%
        #               select(Bad, Good, SemiBad) %>%
        #               rename(Bad_pred_1 = Bad,
        #                      Good_pred_1 = Good,
        #                      #   Convex_pred_1 = Convex,
        #                      SemiBad_pred_1 = SemiBad))
        #
        # roc_object <- multi_roc(data.frame(truerel))
        #
        # plot_roc_df <- plot_roc_data(roc_object)
        #
        # aucs <- plot_roc_df %>%
        #   select(AUC, Method, Group) %>%
        #   filter(!Group %in% c('Micro','Macro'))%>%
        #   distinct()
        #
        # print(aucs)
        #
        # g <- plot_roc_df %>%
        #   filter(!Group %in% c('Micro','Macro'))%>%
        #   ggplot(., aes(x=1-Specificity,y=Sensitivity,color = Method)) +
        #   geom_step() +
        #   # geom_text(data=aucs[aucs$Method=='logit',], aes(x=.6,y=.3, label=sprintf('AUC = %.3f',AUC))) +
        #   # geom_text(data=aucs[aucs$Method=='mlp',], aes(x=.6,y=.5, label=sprintf('AUC = %.3f',AUC))) +
        #   # geom_text(data=aucs[aucs$Method=='xg',], aes(x=.6,y=.7, label=sprintf('AUC = %.3f',AUC))) +
        #   scale_color_viridis_d(end=.8) +
        #   facet_wrap(~Group) +
        #   theme_bw(); g
        # ggsave(file = 'analysis/figures/auc.png', g, dpi = 600, width =6, height = 3)