# Read 5-fold CV results of the monthly model
read_perfm <- function(poll){
   csv_names <- lapply(paste0('5cv_monthly_',2000:2019,'_sep_', poll), function(fname){
      list.files('data/workingData/', fname)
   }) %>% unlist()  %>% unique
   all_test <- lapply(paste0("data/workingData/", csv_names), read.csv)
   ## Remove NA (For some folds in some months, there are NAs because of the error of 'error: inv(): matrix seems singular')
   lapply(all_test, function(all_test_df){
      all_test_df[!is.na(all_test_df$gwr),]
   })
}
# Read 5-fold CV results of the annual model 
read_perfm2 <- function(poll){
   csv_names <- gsub('SLR_result_all_', '', 
                     list.files('../EXPANSE_algorithm/data/workingData/', 
                                paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   years <- csv_names %>% substr(., nchar(csv_names)-3, nchar(csv_names)) %>% 
      as.numeric() %>% as.list()
   
   all_test <- lapply(paste0("../EXPANSE_algorithm/data/workingData/5cv_", csv_names, ".csv"), read.csv)
   all_test
}
show_EM <- function(all_test, all_df_i, region=F, month=F){
   
   slr=all_test[[all_df_i]]$slr
   rf=all_test[[all_df_i]]$rf
   gwr=all_test[[all_df_i]]$gwr
   df_all <- data.frame(slr=error_matrix(all_test[[all_df_i]]$obs, slr),
                        gwr=error_matrix(all_test[[all_df_i]]$obs, gwr),
                        rf=error_matrix(all_test[[all_df_i]]$obs, rf))[c(1,2,5,7,8),]
   df_all$EM = row.names(df_all)
   df_all$year = unique(all_test[[all_df_i]]$year)
   if(region&(!month)){
      df_all$climate_zone <- paste0(unique(all_test[[all_df_i]]$climate_zone), 
                                    '_',
                                    nrow(all_test[[all_df_i]]))
      
      
   }
   if(month&(!region)){
      df_all$month <- paste0('m', unique(all_test[[all_df_i]]$month), 
                             '_',
                             nrow(all_test[[all_df_i]]))
   }
   if(month&region){
      # Performance matrix in each region in each month in each year
      df_all$monthNo <- paste0('m', unique(all_test[[all_df_i]]$month), 
                               '_',
                               nrow(all_test[[all_df_i]]))
      df_all$month <- unique(all_test[[all_df_i]]$month)
      df_all$climate_zone <- unique(all_test[[all_df_i]]$climate_zone)
   }
   
   df_all
}
comp_rf_slr <- function(csv_i, poll){
   files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][5]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   p1 <- ggplot(slr_result)+
      geom_col(aes(x=reorder(variables, incredR2), y=incredR2), 
               position = "dodge2", fill='khaki')+
      coord_flip() +
      theme_light()+
      labs(x = 'variable', y = 'increased R2',
           title = paste0(yr, "_fold_", nfold))+
      # facet_grid(EM~., scales ='free')+
      # labs(title=years[[i]])+
      theme(axis.title = element_text(size = 11),
            axis.text = element_text(size = 11),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 11),
            strip.text.y = element_text(size = 11))
   # rf_vi <- read.csv(paste0("data/workingData/", rf_name))
   source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
   p2 <- plot_rf_vi(csv_name, var_no = 20)
   grid.arrange(p1, p2, nrow=1, ncol=2)
}
extract_slr_var <- function(csv_i, poll, files){
   # files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_name <- files[csv_i]
   
   yr <- strsplit(slr_name, "_")[[1]][5]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   slr_var <- slr_result$variables[-1]
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   data.frame(var_name=slr_var, yr=yr, nfold=nfold)
}
extract_rf_var <- function(csv_i, poll, files){
   # files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   
   var_importance <- read.csv(paste0('data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   rf_var10 <- (var_importance %>% top_n(10, vi))$var_name
   
   if(any(grepl(met_str, rf_var10))){
      rf_var10[grepl(met_str, rf_var10)] <- gsub('(\\_\\d+).*', '', rf_var10[grepl(met_str, rf_var10)])  
   }
   data.frame(var_name=rf_var10, yr=yr, nfold=nfold)
}
extract_slr_var_annual <- function(csv_i, poll, files){
   slr_name <- files[csv_i]
   
   yr <- strsplit(slr_name, "_")[[1]][5]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("../EXPANSE_algorithm/data/workingData/", slr_name), header=T)
   slr_result$yr <- yr
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   slr_var <- slr_result$variables[-1]
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   data.frame(var_name=slr_var, yr=yr, nfold=nfold)
}
extract_rf_var_annual <- function(csv_i, poll, files){
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][6]
   nfold <- (strsplit(slr_name, "_")[[1]][8] %>% strsplit(., ".csv"))[[1]] %>% as.numeric()
   
   var_importance <- read.csv(paste0('../EXPANSE_algorithm//data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   rf_var10 <- (var_importance %>% top_n(10, vi))$var_name
   
   if(any(grepl(met_str, rf_var10))){
      rf_var10[grepl(met_str, rf_var10)] <- gsub('(\\_\\d+).*', '', rf_var10[grepl(met_str, rf_var10)])  
   }
   data.frame(var_name=rf_var10, yr=yr, nfold=nfold)
}
extract_slr_var_monthly <- function(csv_i, poll, files){
   # files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_name <- files[csv_i]
   
   yr <- strsplit(slr_name, "_")[[1]][5]
   nfold <- (strsplit(slr_name, "_")[[1]][9]) %>% as.numeric()
   
   month_i <- (strsplit(slr_name, "_")[[1]][10] %>% 
                  strsplit(., ".csv"))[[1]] %>% 
      gsub('m', '', .) %>% as.numeric()
   
   # rf_name <- paste0("RF_vi_o_", yr, "_fold_", nfold, ".csv")
   slr_result <- read.csv(paste0("data/workingData/", slr_name), header=T)
   slr_result$month <- month_i
   slr_result$nfold <- nfold
   slr_result$increR2[1]=0
   slr_result$incredR2 <- c(0, diff(slr_result$increR2))
   slr_var <- slr_result$variables[-1]
   if(any(grepl(met_str, slr_var))){
      slr_var[grepl(met_str, slr_var)] <- gsub('(\\_\\d+).*', '', slr_var[grepl(met_str, slr_var)])  
   }
   data.frame(var_name=slr_var, yr=yr, nfold=nfold, month=month_i)
}
extract_rf_var_monthly <- function(csv_i, poll, files){
   # files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_name <- files[csv_i]
   csv_name <- gsub('.csv', '', substr(slr_name, 19, nchar(slr_name)))
   yr <- strsplit(slr_name, "_")[[1]][5]
   nfold <- (strsplit(slr_name, "_")[[1]][9]) %>% as.numeric()
   
   month_i <- (strsplit(slr_name, "_")[[1]][10] %>% 
                  strsplit(., ".csv"))[[1]] %>% 
      gsub('m', '', .) %>% as.numeric()
   
   var_importance <- read.csv(paste0('data/workingData/RF_vi_', csv_name, '.csv'), 
                              header = T)
   rf_var10 <- (var_importance %>% top_n(10, vi))$var_name
   
   if(any(grepl(met_str, rf_var10))){
      rf_var10[grepl(met_str, rf_var10)] <- gsub('(\\_\\d+).*', '', rf_var10[grepl(met_str, rf_var10)])  
   }
   data.frame(var_name=rf_var10, yr=yr, nfold=nfold, month=month_i)
}
create_heatmap_monthly <- function(poll){
   # Annual
   files <- list.files("../EXPANSE_algorithm//data/workingData/", paste0("SLR_summary_model_o3_", poll, '_2010'))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_vars_annual <- lapply(seq_along(files), extract_slr_var_annual, poll=poll, files=files) %>% do.call(rbind, .)
   rf_vars_annual <- lapply(seq_along(files), extract_rf_var_annual, poll=poll, files=files) %>% do.call(rbind, .)
   
   
   # 12-month
   files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_vars <- lapply(seq_along(files), extract_slr_var, poll=poll, files=files) %>% do.call(rbind, .)
   rf_vars <- lapply(seq_along(files), extract_rf_var, poll=poll, files=files) %>% do.call(rbind, .)
   slr_tbl <- with(slr_vars, table(var_name, yr))
   rf_tbl <- with(rf_vars, table(var_name, yr))
   slr_tbl <- slr_tbl[mixedorder(row.names(slr_tbl), decreasing=T),]
   rf_tbl <- rf_tbl[mixedorder(row.names(rf_tbl), decreasing=T),]
   # 1-month
   files_monthly <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_sep_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_sep_", poll)))]
   files_monthly <- files_monthly[mixedorder(files_monthly)]
   slr_vars_m <- lapply(seq_along(files_monthly), extract_slr_var_monthly, poll=poll, files=files_monthly) %>% do.call(rbind, .)
   rf_vars_m <- lapply(seq_along(files_monthly), extract_rf_var_monthly, poll=poll, files=files_monthly) %>% do.call(rbind, .)
   
   
   # All
   slr_vars_all <- rbind(slr_vars_m, slr_vars %>% mutate(month='12-month'),
                         slr_vars_annual %>% mutate(month='annual'))
   rf_vars_all <- rbind(rf_vars_m, rf_vars %>% mutate(month='12-month'),
                        rf_vars_annual %>% mutate(month='annual'))
   
   slr_tbl_m <- with(slr_vars_all, table(var_name, month))
   rf_tbl_m <- with(rf_vars_all, table(var_name, month))
   slr_tbl_m <- slr_tbl_m[mixedorder(row.names(slr_tbl_m), decreasing=T), mixedorder(colnames(slr_tbl_m))]
   rf_tbl_m <- rf_tbl_m[mixedorder(row.names(rf_tbl_m), decreasing=T), mixedorder(colnames(rf_tbl_m))]
   
   print(heatmap(rf_tbl_m, Colv = NA, Rowv = NA, scale="column", main=paste0('RF: ', poll)))
   print(heatmap(slr_tbl_m, Colv = NA, Rowv = NA, scale="column", main=paste0('SLR: ', poll)))
}

## Overall monthly 5-fold model performance

# Model performance for each month and year and climate zone
overall_perfm <- function(target_poll){
   # Monthly models (built for each month)
   all_test_sep <- read_perfm(target_poll)
   # em_df1_sep <- lapply(seq_along(all_test_sep), show_EM, all_test=all_test_sep)
   em_df1_sep <- lapply(all_test_sep, function(sep_df){
      df_all <- read.csv('data/processed/climate_code.csv', header = T)
      
      df_all <- inner_join(df_all,
                           read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
                           by='zoneID')
      df_all <- df_all[!duplicated(df_all$sta_code),]
      all_test2 <- inner_join(sep_df, df_all, by=c('sta_code'))
      
      lapply(unique(all_test2$month), function(target_m){
         all_test3 <- all_test2 %>% filter(month==target_m)
         lapply(unique(all_test3$climate_zone), function(target_climatezone){
            show_EM(list(all_test3 %>% filter(climate_zone==target_climatezone)), 1, T, T)
         }) %>% do.call(rbind, .)
      }) %>% do.call(rbind, .)
   }) %>% do.call(rbind, .)
   
   em_df <- em_df1_sep
   # em_df$month <- unlist(lapply(strsplit(em_df1_sep$month, '_'), '[[', 1)) %>% 
   #    gsub('m','',.) %>% 
   #    as.numeric()
   em_df$count <- unlist(lapply(strsplit(em_df1_sep$monthNo, '_'), '[[', 2)) %>% 
      as.numeric()
   em_df$date <- as.Date(as.yearmon(paste0(em_df$year, '-', em_df$month)))
   em_df$poll <- target_poll
   em_df
}
# Model performance for each month and year and
overall_perfm_allClimates <- function(target_poll){
   # Monthly models (built for each month)
   all_test_sep <- read_perfm(target_poll)
   # em_df1_sep <- lapply(seq_along(all_test_sep), show_EM, all_test=all_test_sep)
   em_df1_sep <- lapply(all_test_sep, function(sep_df){
      df_all <- read.csv('data/processed/climate_code.csv', header = T)
      
      df_all <- inner_join(df_all,
                           read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
                           by='zoneID')
      df_all <- df_all[!duplicated(df_all$sta_code),]
      all_test2 <- inner_join(sep_df, df_all, by=c('sta_code'))
      
      df_out <- lapply(unique(all_test2$month), function(target_m){
         all_test3 <- all_test2 %>% filter(month==target_m)
         show_EM(list(all_test3), 1, F, T)
      }) %>% do.call(rbind, .)
      # testing whether the RMS relative error works:
      # https://stats.stackexchange.com/questions/260615/what-is-the-difference-between-rrmse-and-rmsre/
      # lapply(unique(all_test2$month), function(target_m){
      #    all_test3 <- all_test2 %>% filter(month==target_m)
      #    # df_out_sub <- df_out[grepl(paste0('m', target_m, '_'), df_out$month), ]
      #    pred <- all_test3$gwr
      #    obs <- all_test3$obs
      #    rmsre <- sqrt(mean(((pred-obs)/obs)^2))
      #    df_out_sub$gwr[df_out_sub$EM=='RMSE']
      # }) %>% do.call(rbind, .)
      df_out
   }) %>% do.call(rbind, .)
   
   em_df <- em_df1_sep
   em_df$month <- unlist(lapply(strsplit(em_df1_sep$month, '_'), '[[', 1)) %>%
      gsub('m','',.) %>%
      as.numeric()
   em_df$count <- unlist(lapply(strsplit(em_df1_sep$month, '_'), '[[', 2)) %>% 
      as.numeric()
   em_df$date <- as.Date(as.yearmon(paste0(em_df$year, '-', em_df$month)))
   em_df$poll <- target_poll
   em_df
}

show_EM_temp <- function(all_test, all_df_i, region=F, month=F){
   slr=all_test[[all_df_i]]$slr
   rf=all_test[[all_df_i]]$rf
   gwr=all_test[[all_df_i]]$gwr
   df_all <- data.frame(slr=error_matrix(all_test[[all_df_i]]$obs, slr),
                        gwr=error_matrix(all_test[[all_df_i]]$obs, gwr),
                        rf=error_matrix(all_test[[all_df_i]]$obs, rf))[c(1,2,5,7,8),]
   df_all$EM = row.names(df_all)
   df_all$month = unique(all_test[[all_df_i]]$month)
   # if(region&(!month)){
   #    df_all$climate_zone <- paste0(unique(all_test[[all_df_i]]$climate_zone), 
   #                                  '_',
   #                                  nrow(all_test[[all_df_i]]))
   #       
   #    
   # }
   # if(month&(!region)){
   #    df_all$month <- paste0('m', unique(all_test[[all_df_i]]$month), 
   #                           '_',
   #                           nrow(all_test[[all_df_i]]))
   # }
   if(month&region){
      # df_all$month <- paste0('m', unique(all_test[[all_df_i]]$month), 
      #                        '_',
      #                        nrow(all_test[[all_df_i]]))
      df_all$climate_zone <- unique(all_test[[all_df_i]]$climate_zone)
   }
   df_all
}
# Long-term model performance for each month
temp_perfm <- function(target_poll){
   all_test_sep <- read_perfm(target_poll)
   all_test_sep_df <- do.call(rbind, all_test_sep)
   all_test_sep_m <- lapply(unique(all_test_sep_df$month), function(m_i){
      all_test_sep_df %>% filter(month==m_i)
   })
   em_df1_sep <- lapply(seq_along(all_test_sep_m), show_EM_temp, 
                        all_test=all_test_sep_m) %>% do.call(rbind, .)
   
   em_df <- em_df1_sep
   em_df$poll <- target_poll
   em_df
   # Test
   # table(all_test_sep_df$month)
}
get_climate_code <- function(taget_poll){
   if(target_poll=='PM2.5'){
      df_all <- lapply(paste0('data/raw/gee/pred_PM25_', 2006:2019,'_monthly.csv'), 
                       function(df_filename){
                          df_all <- read.csv(df_filename) %>% dplyr::select(zoneID, sta_code)
                          df_all <- df_all[!duplicated(df_all$sta_code),]
                       }) %>% do.call(rbind, .)
   }else{
      df_all <- lapply(paste0('data/raw/gee/pred_', target_poll,'_', 2000:2019,'_monthly.csv'), 
                       function(df_filename){
                          df_all <- read.csv(df_filename) %>% dplyr::select(zoneID, sta_code)
                          df_all <- df_all[!duplicated(df_all$sta_code),]
                       }) %>% do.call(rbind, .)
   }
   df_all
}



read_dflist <- function(target_poll){
   all_test <- read_perfm(target_poll)
   all_test <- do.call(rbind, all_test)
   all_test$poll <- target_poll
   df_all <- read.csv('data/processed/climate_code.csv', header = T)
   
   df_all <- inner_join(df_all,
                        read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
                        by='zoneID')
   df_all <- df_all[!duplicated(df_all$sta_code),]
   all_test2 <- inner_join(all_test, df_all, by=c('sta_code'))
   id_df <- expand.grid(month=unique(all_test2$month), climate_zone=unique(all_test2$climate_zone))
   all_test3 <- lapply(1:nrow(id_df), function(i){
      id=id_df[i, 'month']
      climate_id=id_df[i, 'climate_zone']
      all_test2 %>% filter(month==id, climate_zone==climate_id)
   })
   all_test3  # Return list
}
read_dflist_new <- function(target_poll){
   all_test <- read_perfm(target_poll)
   all_test <- do.call(rbind, all_test)
   all_test$poll <- target_poll
   all_test
}## something wrong with read_dflist causing less observations presented

temp_region_perfm <- function(target_poll){
   all_test3 <- read_dflist(target_poll)
   
   em_df1 <- lapply(seq_along(all_test3), show_EM_temp, all_test=all_test3, region=T, month=T)
   em_df1 <- do.call(rbind, em_df1)
   em_df1$poll <- target_poll
   em_df1
   
   ## Test
   # table(all_test2$month, all_test2$climate_zone)
   # ggplot(all_test2 %>% filter(climate_zone%in%c( 'Alpine')))+  #Northern
   #    geom_point(aes(x=gwr, y=obs))+
   #    facet_wrap(month~climate_zone)+
   #    geom_abline(slope=1, intercept = 0)
}

plotM <- function(data_df, time_var, colorZone=T){
   no2_2010_season <- data_df %>% 
      group_by(.dots=list(as.symbol(time_var)), sta_code, climate_zone) %>% 
      summarise(obs=mean(obs, na.rm = T))
   no2_wide2 <- dcast(no2_2010_season[, c('sta_code', 'obs', 'climate_zone', time_var)],
                      as.formula(paste0('sta_code+climate_zone~', time_var)), 
                      value.var = 'obs')
   
   upper.panel <- function(x, y){
      points(x, y,xlim=c(-2,220), ylim=c(-2,220), cex=1.4, cex.lab=1.5, cex.axis=0.75)
      abline(0,1, col='red')
   }
   lowerFn <- function(data, mapping, ...) {
      p <- ggplot(data = data, mapping = mapping) +
         geom_point(colour = "black") +
         geom_abline(intercept=0, slope=1, col='red')+
         lims(x=c(-2, 150),y=c(-2, 150))
      p
   }
   if(colorZone){
      ggpairs(data=no2_wide2[,-1], lower = list(continuous = wrap(lowerFn, size=9)), upper = list(continuous = wrap("cor", size=7)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)),
              mapping=ggplot2::aes(colour=climate_zone))+
         ggplot2::theme(strip.text=element_text(size=18),
                        axis.text=element_text(size=16))
   }else{
      ggpairs(data=no2_wide2[,-1], lower = list(continuous = wrap(lowerFn, size=9)),upper = list(continuous = wrap("cor", size=7)),
              diag=list(discrete="barDiag", 
                        continuous = wrap("densityDiag", alpha=0.5)))+
         ggplot2::theme(strip.text=element_text(size=18),
                        axis.text=element_text(size=16))
   }
   
}

read_temp_region_df <- function(target_poll){
   all_test <- read_perfm(target_poll)
   all_test <- do.call(rbind, all_test)
   df_all <- read.csv('data/processed/climate_code.csv', header = T)
   
   df_all <- inner_join(df_all,
                        read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
                        by='zoneID')
   df_all <- df_all[!duplicated(df_all$sta_code),]
   all_test2 <- inner_join(all_test, df_all, by=c('sta_code'))
   all_test2 <- all_test2 %>% 
      mutate(season=case_when(month%in%(3:5)~'spring',
                              month%in%(6:8)~'summer',
                              month%in%(9:11)~'fall',
                              month%in%c(12,1:2)~'winter'),
             mc=ifelse(month%in%(4:9), 'warm', 'cold'))
   plotM(all_test2, 'season')
   # id_df <- expand.grid(month=unique(all_test2$month), climate_zone=unique(all_test2$climate_zone))
   # all_test3 <- lapply(1:nrow(id_df), function(i){
   #    id=id_df[i, 'month']
   #    climate_id=id_df[i, 'climate_zone']
   #    all_test2 %>% filter(month==id, climate_zone==climate_id)
   # })
   all_test3
}

