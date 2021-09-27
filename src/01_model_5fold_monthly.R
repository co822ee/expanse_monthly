# This script run the three models for multiple single years or multiple years
# for supervised linear regression model, geographically weighted regression (stepwise),
# and random forests.
# 03062021 Run the code and exclude/include zoneID as a predictor and with tuning RF
# 0624 finished running for NO2 and PM2.5
source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
target_poll = 'PM2.5'
tuneRF_b = T
# Multiple single years
csv_name <- paste0('monthly_2010_', target_poll)   #2008:2012
# o_ and o2_ differences are whether to include data after 2013
target_yr <- 2010
nfold <- 5
if(!dir.exists('data/workingData')) dir.create('data/workingData')
source("src/00_fun_read_monthly_data_gee.R")
source("../expanse_multiyear/src/00_fun_create_fold.R")
library(doParallel)
library(foreach)

print("********************************************")
print(csv_name)
print(target_poll)

df_sub <- read_data(target_poll,  target_yr)
exc_names <- c('system.index', 'obs', 'sta_code', 'component_caption', '.geo', 
               'year', 'date', 'month', ## exclude month first
               'cntr_code', 'xcoord', 'ycoord', 'sta_type', 'valid', 'time_type')
pred_c <- names(df_sub)[!(names(df_sub)%in%exc_names)]

if(nrow(df_sub)>200){
   
   # clusterExport(cl, "csv_names", envir = .GlobalEnv)
   # clusterExport(cl, "nfold", envir = .GlobalEnv)
   # clusterExport(cl, "years", envir = .GlobalEnv)
   # clusterExport(cl, "yr_i", envir = .GlobalEnv)
   if(!file.exists(paste0('data/workingData/RF_vi_', csv_name, '_fold_1', '.csv'))){
      
      # data_all <- create_fold(df_sub, seed, strt_group=c("sta_type", "zoneID"), 
      #                         nfold = nfold)
      
      data_all <- create_fold(df_sub, seed, strt_group=c("n_obs", "sta_type", "zoneID"), 
                              multiyear_group = c("sta_code", "month"),
                              nfold = 5, m_var='month')
      
      cluster_no <- 5
      cl <- parallel::makeCluster(cluster_no)
      doParallel::registerDoParallel(cl)
      foreach(fold_i=1:nfold)%dopar%{
         source("../EXPANSE_algorithm/scr/fun_call_lib.R")
         csv_name_fold <- paste0(csv_name, "_fold_", fold_i)
         
         #f# SLR: select predictors
         test_sub <- data_all[data_all$nfold==fold_i,]
         train_sub <- data_all[-test_sub$index, ] #data_all$index starts from 1 to the length.
         
         #------------------Above code is needed for all algorithms----------------------
         #---------#f# SLR: train SLR -----------
         source("../EXPANSE_algorithm/scr//fun_slr_for.R")
         # check the predictor variables
         print("SLR predictors:")
         train_sub %>% dplyr::select(pred_c) %>% names()
         slr_result <- slr(train_sub$obs, as.data.frame(train_sub[, c( c(pred_c))]),
                           cv_n = csv_name_fold, 
                           R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
         slr_model <- slr_result[[3]]
         # debug (why macc is not included)
         
         slr_model %>% summary
         
         #f# SLR: test SLR
         source("../EXPANSE_algorithm/scr/fun_output_slr_result.R")
         slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                                       output_filename = csv_name_fold, obs_varname = 'obs',
                                       outputselect = c("sta_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index", 'month'))
         
         slr_df <- slr_poll[[1]]
         
         ### new: Add year for multiple year modelling
         slr_model2 <- lm(as.formula(paste0('obs~', paste(c(names(slr_model$coefficients[-1]), 'month'), collapse = '+'))),
                          data = train_sub)
         slr_model2 %>% summary
         slr_output <- summary(slr_model2)$coefficients %>% as.data.frame()
         slr_output <- cbind(row.names(slr_output), slr_output)
         slr_output[1,1] <- 'Final'
         colnames(slr_output) <- c("variables", "beta", "Std.Error", "t", "P")
         write.csv(slr_output, paste0('data/workingData/SLR_summary_model_month_', csv_name_fold, '.csv'), row.names=F)
         
         source("../EXPANSE_algorithm/scr/fun_gen_pred_df.R")
         output_slr_result <- function(model, test_df, train_df, output_filename, obs_varname,
                                       outputselect = c("station_european_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index")){
            slr_poll_test <- gen_pred_df(model, test_df, obs_varname)
            slr_poll_train <- gen_pred_df(model, train_df, obs_varname)
            eval_test <- error_matrix(slr_poll_test[, obs_varname], slr_poll_test$slr)
            eval_train <- error_matrix(slr_poll_train[, obs_varname], slr_poll_train$slr)
            
            slr_poll <- rbind(slr_poll_train %>% mutate(df_type = 'train'),
                              slr_poll_test %>% mutate(df_type = 'test'))
            slr_poll <- slr_poll[, outputselect]
            write.csv(slr_poll,
                      paste0('data/workingData/SLR_result_all_', output_filename, '.csv'),
                      row.names = F)
            return(list(slr_poll, eval_train=eval_train, eval_test=eval_test))
         }
         # Use the one with months used as indicator
         slr_poll <- output_slr_result(slr_model, test_df = test_sub, train_df = train_sub,
                                       output_filename = csv_name_fold, obs_varname = 'obs',
                                       outputselect = c("sta_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index", 'month'))
         
         slr_poll <- output_slr_result(slr_model2, test_df = test_sub, train_df = train_sub,
                                       output_filename = paste0('month_', csv_name_fold), obs_varname = 'obs',
                                       outputselect = c("sta_code", "slr", "obs", "res",
                                                        "nfold", "df_type", "year", "index", 'month'))
         
         slr_df <- slr_poll[[1]]
         
         slr_poll$eval_train %>% print()
         slr_poll$eval_test %>% print()
         
         #--------- RF--------
         print("--------------- RF ---------------")
         set.seed(seed)
         
         train_df <- train_sub
         test_df <- test_sub
         pred_c_rf <- c(pred_c, 'month') #"x_trun", "y_trun"  ,  "cntr_code"
         x_varname = names(data_all %>% dplyr::select(pred_c_rf))
         print("RF predictors:")
         print(x_varname)
         ## LLO CV (small test for multiple years)
         
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
            source("../EXPANSE_algorithm/scr//fun_tune_rf.R")
            hyper_grid <- tune_rf(train_df, test_df, #valid_df,
                                  y_varname='obs',
                                  x_varname,
                                  csv_name_fold, hyper_grid)
            
            #f# RF: train the model
            hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name_fold,".csv"))
         }
         source("../EXPANSE_algorithm/scr//fun_opt_rf.R")
         # If tuneRF is False, a 500 number of trees and sqrt(x_varname no) of mtry woul be used
         rf_result <- opt_rf(train_df, test_df,
                             y_varname='obs',
                             x_varname = x_varname,
                             csv_name_fold, hyper_grid, tuneRF_b,
                             outputselect = c("sta_code", "rf", "obs", "res",
                                              "nfold", "df_type", "year", "index"))
         source("../EXPANSE_algorithm/scr/fun_plot_rf_vi.R")
         plot_rf_vi(csv_name_fold, var_no = 15)
         
      }
      parallel::stopCluster(cl)
   }
   
}else{
   print('Not enough data for building the models')
}
   

