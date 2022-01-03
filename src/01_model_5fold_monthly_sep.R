# For PM2.5 year 2006 fold 4 (vif problem)

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
            data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                                    multiyear_group = c("sta_code", "month"),
                                    nfold = 5, m_var='month')
            print('finish creating fold')
            
            
            csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
            
            #f# SLR: select predictors
            test_sub <- data_all[data_all$nfold==fold_i,]
            train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
            
            #------------------Above code is needed for all algorithms----------------------
            #---------#f# SLR: train SLR -----------
            source("../EXPANSE_algorithm/scr//fun_slr_for.R")
            # check the predictor variables
            if(!file.exists(paste0('data/workingData/SLR_result_all_', csv_name_fold, '_m12', '.csv'))){
               # The folds are spatially leave-out so that there is no overlapping 
               # between each data
               # What we need to do is to build model for each month
               # sub_month <- 1
               slr_month <- function(train_df, csv_name, sub_month){
                  train_sub_m <- train_df %>% filter(month==sub_month)
                  csv_name_fold_m <- paste0(csv_name, '_m', sub_month)
                  slr_result <- slr(train_sub_m$obs, as.data.frame(train_sub_m[, pred_c]),
                                    cv_n = csv_name_fold_m, 
                                    R2thres = ifelse(target_poll%in%c('PM2.5', 'PM10'), 0.0, 0.01))
                  slr_model <- slr_result[[3]]
                  # debug (why macc is not included)
                  return(slr_model)
               }
               slr_models_m <- lapply(1:12, slr_month, train_df=train_sub, csv_name=csv_name_fold)
               #f# SLR: test SLR
               source("../EXPANSE_algorithm/scr/fun_output_slr_result.R")
               slr_polls <- lapply(seq_along(slr_models_m), function(model_i){
                  train_sub_m <- train_sub %>% filter(month==model_i)
                  test_sub_m <- test_sub %>% filter(month==model_i)
                  csv_name_fold_m <- paste0(csv_name_fold, '_m', model_i)
                  
                  output_slr_result(slr_models_m[[model_i]], test_df=test_sub_m, 
                                    train_df=train_sub_m, output_filename = csv_name_fold_m, 
                                    obs_varname = 'obs',
                                    outputselect = c("sta_code", "slr", "obs", "res",
                                                     "nfold", "df_type", "year", "index", "month"))
               })
            }
            
            #--------- RF--------
            if(!file.exists(paste0('data/workingData/RF_vi_', csv_name_fold, '_m12', '.csv'))){
               print("--------------- RF ---------------")
               set.seed(seed)
               
               source('src/fun_run_rf.R')
               source("../EXPANSE_algorithm/scr//fun_tune_rf.R")
               tuneRF_month <- function(sub_month, train_df, test_df, csv_name, x_varname){
                  train_df_m <- train_df %>% filter(month==sub_month)
                  test_df_m <- test_df %>% filter(month==sub_month)
                  csv_name_fold_m <- paste0(csv_name, '_m', sub_month)
                  
                  if(tuneRF_b){
                     #f# RF: tune hyperparameter
                     hyper_grid <- expand.grid(
                        mtry = seq(floor(sqrt(length(x_varname))), length(x_varname), by=20),
                        ntrees = seq(200, 800, by=200),
                        OOB_RMSE = 0,
                        OOB_R2 = 0,
                        valid_RMSE = 0,
                        valid_R2 = 0
                     )
                     
                     if(!file.exists(paste0('data/workingData/rf_hyper_grid_', csv_name_fold_m, '.csv'))){
                        hyper_grid <- tune_rf(train_df_m, test_df_m, #valid_df,
                                              y_varname='obs',
                                              x_varname,
                                              csv_name_fold_m, hyper_grid)
                     }
                     
                     #f# RF: train the model
                     hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold_m,".csv"))
                     # Run the RF
                     source('src/fun_run_rf.R')
                     rf_model <- run_rf(train_df_m, y_varname='obs', x_varname,
                                        hyper_grid, seed=123, tuneRF=tuneRF_b)
                  }
                  return(rf_model)
                  
               }
               rf_models <- lapply(1:12, tuneRF_month, train_df=train_sub, test_df=test_sub,
                                   csv_name=csv_name_fold, x_varname=pred_c)
               source('src/fun_output_rf.R')
               lapply(seq_along(rf_models), function(model_i){
                  sub_month <- model_i
                  train_df_m <- train_sub %>% filter(month==sub_month)
                  test_df_m <- test_sub %>% filter(month==sub_month)
                  csv_name_fold_m <- paste0(csv_name_fold, '_m', sub_month)
                  
                  output_rf(rf_model=rf_models[[model_i]], df_train=train_df_m, 
                            df_test=test_df_m, 
                            y_varname='obs', csv_name=csv_name_fold_m, 
                            outputselect = c("sta_code", "rf", "obs", "res",
                                             "nfold", "df_type", "year", "index"))
               })
            }
            
         }else{
            if(file.exists('data/temp/failed_5fold.txt')){
               write.table(csv_name, 'data/temp/failed_5fold.txt', row.names = F, col.names = F,
                           append = T)
            }else{
               write.table(csv_name, 'data/temp/failed_5fold.txt', row.names = F, col.names = F)
            }
            
            print('Not enough data for building the models')
         }
         
      }
   }
}
parallel::stopCluster(cl)



