# PM10 year 2000 fold 4
nfold <- 5
if(!dir.exists('data/workingData')) dir.create('data/workingData')
library(doParallel)
library(foreach)
cluster_no <- detectCores()
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
foreach(fold_i=1:nfold)%dopar%{
   tuneRF_b = T
   source("../EXPANSE_algorithm/scr/fun_call_lib.R")
   source("src/00_fun_read_monthly_data_gee.R")
   source("../expanse_multiyear/src/00_fun_create_fold.R")
   source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_calibr_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_output_gwr_result.R")
   for(target_yr in 2000:2019){
      for(target_poll in c('NO2', 'O3', 'PM10', 'PM2.5')){  #
         csv_name <- paste0('monthly_', target_yr,'_sep_', target_poll)  
         print("********************************************")
         print(csv_name)
         print(target_poll)
         
         df_sub <- read_data(target_poll,  target_yr)
         # for year 2000
         names(df_sub)[names(df_sub)%in%c('ugr','tbu','res','nat','por','ind')] <- paste0(names(df_sub)[names(df_sub)%in%c('ugr','tbu','res','nat','por','ind')], '_2000')
         
         exc_names <- c('system.index', 'obs', 'sta_code', 'component_caption', '.geo', 
                        'year', 'date', 'month', ## exclude month first
                        'cntr_code', 'xcoord', 'ycoord', 'sta_type', 'valid', 'time_type')
         pred_c <- names(df_sub)[!(names(df_sub)%in%exc_names)]
         
         if(nrow(df_sub)>200&any(table(df_sub$month)>200)){
            
            # clusterExport(cl, "csv_names", envir = .GlobalEnv)
            # clusterExport(cl, "nfold", envir = .GlobalEnv)
            # clusterExport(cl, "years", envir = .GlobalEnv)
            # clusterExport(cl, "yr_i", envir = .GlobalEnv)
            
            
            # data_all <- create_fold(df_sub, seed, strt_group=c("sta_type", "zoneID"), 
            #                         nfold = nfold)
            
            data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                                    multiyear_group = c("sta_code", "month"),
                                    nfold = 5, m_var='month')
            print('finish creating fold')
            
            
            csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
            
            #f# SLR: select predictors
            test_sub <- data_all[data_all$nfold==fold_i,]
            train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
            
            #------------------Above code is needed for all algorithms----------------------
            #---------#f# GWR: train GWR -----------
            if(all(unlist(lapply(paste0('data/workingData/SLR_result_all_', csv_name_fold, '_m', 1:12, '.csv'), file.exists)))&
               !all(unlist(lapply(paste0('data/workingData/GWR_result_all_', csv_name_fold, '_m', 1:12, '.csv'), file.exists)))
               ){
               # The folds are spatially leave-out so that there is no overlapping 
               # between each data
               # What we need to do is to build model for each month
               gwr_month <- function(train_df, csv_name, sub_month){
                  train_sub_m <- train_df %>% filter(month==sub_month)
                  csv_name_fold_m <- paste0(csv_name, '_m', sub_month)
                  setup <- setup_gwr(train_sub_m, eu_bnd,
                                     cellsize = 200000, local_crs = local_crs, 
                                     xcoord="xcoord", ycoord="ycoord")
                  sp_train <- setup[[1]]
                  grd <- setup[[2]]
                  DM <- setup[[3]]
                  nngb <- tryCatch(calibr_gwr(sp_train, csv_name_fold_m), 
                                   error=function(e) T)
                  if(typeof(nngb)!='logical'){
                     print(paste0("nngb: ", nngb))
                     gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name_fold_m)
                     gwr_model
                  }
               }
               gwr_models_m <- lapply(1:12, gwr_month, train_df=train_sub, csv_name=csv_name_fold)
               #f# SLR: test SLR
               gwr_polls <- lapply(seq_along(gwr_models_m), function(model_i){
                  train_sub_m <- train_sub %>% filter(month==model_i)
                  test_sub_m <- test_sub %>% filter(month==model_i)
                  csv_name_fold_m <- paste0(csv_name_fold, '_m', model_i)
                  output_gwr_result(gwr_models_m[[model_i]], train_sub_m, test_sub_m, 
                                    local_crs,
                                    output_filename = csv_name_fold_m, 
                                    xcoord="xcoord", ycoord="ycoord",
                                    outputselect = c("sta_code", "gwr", "obs", "res",
                                                     "nfold", "df_type", "year", "index", "month"))
               })
            }
         }
      }
   }
}
parallel::stopCluster(cl)



