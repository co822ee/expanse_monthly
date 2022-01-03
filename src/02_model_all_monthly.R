source("../EXPANSE_algorithm/scr/fun_call_lib.R")
# Whether to tune RF
target_poll = 'PM10'
tuneRF_b = T
# Multiple single years
csv_name <- paste0('all_monthly_2010_', target_poll)   #2008:2012
# o_ and o2_ differences are whether to include data after 2013
target_yr <- 2010
if(!dir.exists('data/workingData')) dir.create('data/workingData')
source("src/00_fun_read_monthly_data_gee.R")

print("********************************************")
print(csv_name)
print(target_poll)

df_sub <- read_data(target_poll,  target_yr)
exc_names <- c('system.index', 'obs', 'sta_code', 'component_caption', '.geo', 
               'year', 'date', 'month', ## exclude month first
               'cntr_code', 'xcoord', 'ycoord', 'sta_type', 'valid', 'time_type')
pred_c <- names(df_sub)[!(names(df_sub)%in%exc_names)]

if(nrow(df_sub)>200){
   source("../EXPANSE_algorithm/scr/fun_call_lib.R")
   
   #------------------Above code is needed for all algorithms----------------------
   #---------#f# SLR: train SLR -----------
   if(!file.exists(paste0('data/workingData/SLR_summary_model_month_', csv_name, '.csv'))){
      source("../EXPANSE_algorithm/scr//fun_slr_for.R")
      # check the predictor variables
      print("SLR predictors:")
      df_sub %>% dplyr::select(pred_c) %>% names()
      slr_result <- slr(df_sub$obs, as.data.frame(df_sub[, c( c(pred_c))]),
                        cv_n = csv_name, 
                        R2thres = ifelse(target_poll=='PM2.5', 0.0, 0.01))
      slr_model <- slr_result[[3]]
      
      ### new: Add year for multiple year modelling
      slr_model2 <- lm(as.formula(paste0('obs~', paste(c(names(slr_model$coefficients[-1]), 'month'), collapse = '+'))),
                       data = df_sub)
      slr_model2 %>% summary
      slr_output <- summary(slr_model2)$coefficients %>% as.data.frame()
      slr_output <- cbind(row.names(slr_output), slr_output)
      slr_output[1,1] <- 'Final'
      colnames(slr_output) <- c("variables", "beta", "Std.Error", "t", "P")
      write.csv(slr_output, paste0('data/workingData/SLR_summary_model_month_', csv_name, '.csv'), row.names=F)
   }
   
   #--------- RF--------
   if(!file.exists(paste0('data/workingData/RF_vi_', csv_name, '.csv'))){
      print("--------------- RF ---------------")
      set.seed(seed)
      
      pred_c_rf <- c(pred_c, 'month') #"x_trun", "y_trun"  ,  "cntr_code"
      x_varname = names(df_sub %>% dplyr::select(pred_c_rf))
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
         if(!file.exists(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))){
            hyper_grid <- tune_rf(df_sub, df_sub, #valid_df,
                                  y_varname='obs',
                                  x_varname,
                                  csv_name, hyper_grid)
         }
         #f# RF: train the model
         hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
      }
   }
}else{
   print('Not enough data for building the models')
}
csv_names <- c(paste0('all_monthly_2010_', c('NO2', 'O3', 'PM10','PM2.5')),
              paste0('all_monthly_2010_sep_', c('NO2', 'O3', 'PM10','PM2.5')))   #2008:2012)   #2008:2012
### Output parameter outcome (unfinished)
hyper_grid <- read.csv(paste0("data/workingData/rf_hyper_grid_", csv_name,".csv"))
hyper_grid$poll <- strsplit(csv_name, '_') %>% lapply(., `[[`, 4) %>% unlist
hyper_grid$yr_str <- strsplit(csv_name, '_') %>% lapply(., `[[`, 3) %>% unlist
hyper_grid$m_str = ifelse(grepl('sep', csv_name), '1-month', '12-month')
hyperAll <- hyper_grid[which.min(hyper_grid$OOB_RMSE),] %>% mutate(csv_name=csv_name)

outputSlrCoef <- function(csv_name){
   if(!grepl('sep', csv_name)){
      # 12-month
      slr_summary <- read.csv(paste0("data/workingData/SLR_summary_model_month_", csv_name, ".csv") )[, c('variables', 'beta')]
      slr_summary$poll <- strsplit(csv_name, '_') %>% lapply(., `[[`, 4) %>% unlist
      slr_summary$csv_name = csv_name
      slr_summary$m_str = ifelse(grepl('sep', csv_name), '1-month', '12-month')
      slrCoefAll <- slr_summary
      slrCoefAll$index <- 1:nrow(slrCoefAll)
      slrCoefs1 <- slrCoefAll %>% filter(!grepl('month', variables))  # without year indicator
      
      temp <- left_join(slrCoefs1, 
                        data.frame(m_str=rep(slrCoefs1$m_str, 12),
                                   month=rep(1:12, each=nrow(slrCoefs1)),
                                   variables=rep(slrCoefs1$variables, 12)),
                        by=c('variables', 'm_str'))
      # part 2 (only year indicators)
      slrCoefs2 <- slrCoefAll %>% filter(grepl('month', variables)) %>% 
         mutate(month = as.numeric(gsub('month', '', variables))) %>% 
         rbind(., slrCoefAll %>% filter(grepl('Final', variables)) %>% mutate(month=1))
      slrCoefs2 <- slrCoefs2[order(slrCoefs2$index), ]
      
      # Combine the two
      slrCoefsAll <- rbind(temp %>% filter(variables=='Final'), 
                           slrCoefs2 %>% filter(variables!='Final')) 
      realFinal <- slrCoefsAll %>% filter(month==1)
      baselineFinal <- slrCoefsAll %>% filter(month!=1&variables=='Final')
      relativeFinal <- slrCoefsAll %>% filter(month!=1&variables!='Final')
      trueFinal <- baselineFinal
      trueFinal$beta <- trueFinal$beta+relativeFinal$beta
      
      AllCoefs <- rbind(realFinal,
                        trueFinal,
                        temp %>% filter(variables!='Final')) %>% arrange(index)  ## AllCoefs and temp
      AllCoefs
   }else{
      # 1-month
      filenames <- list.files('data/workingData/', paste0('SLR_summary_model_', csv_name))[mixedorder(list.files('data/workingData/', paste0('SLR_summary_model_', csv_name)))]
      slrCoefAll <- lapply(filenames, function(fname){
         slr_summary <- read.csv(paste0('data/workingData/', fname))[, c('variables', 'beta')]
         
         slr_summary$poll <- strsplit(fname, '_')[[1]][8] ##%>% lapply(., `[[`, 8) %>% unlist
         slr_summary$csv_name = gsub('SLR_summary_model_', '', fname) %>% gsub('.csv', '', .)
         slr_summary$m_str = ifelse(grepl('sep', fname), '1-month', '12-month')
         slr_summary
      }) %>% do.call(rbind, .)
      slrCoefAll$index <- 1:nrow(slrCoefAll)
      slrCoefAll$month = strsplit(slrCoefAll$csv_name, '_')%>% lapply(., `[[`, 6) %>% unlist %>% gsub('m','',.) 
      slrCoefAll
   }
}
slrCoefs <- lapply(csv_names, outputSlrCoef)
slrCoefs <- do.call(rbind, slrCoefs)
write.csv(slrCoefs, 'data/processed/SLRcoef_all_monthly.csv', row.names = F)
