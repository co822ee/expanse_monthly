source("../EXPANSE_algorithm/scr/fun_call_lib.R")
target_poll = c('PM2.5', 'PM10', 'NO2', 'O3')
csv_names <- lapply(paste0('SLR_result_all_monthly_',2000:2019,'_'), function(fname){
   list.files('data/workingData/', fname)
}) %>% unlist() %>% 
   gsub('SLR_result_all_', '', .) %>% 
   strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
csvnamesRF <- lapply(paste0('RF_result_all_monthly_',2000:2019,'_'), function(fname){
   list.files('data/workingData/', fname)
}) %>% unlist() %>% 
   gsub('RF_result_all_', '', .) %>% 
   strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
csvnamesGWR <- lapply(paste0('GWR_result_all_monthly_',2000:2019,'_'), function(fname){
   list.files('data/workingData/', fname)
}) %>% unlist() %>% 
   gsub('GWR_result_all_', '', .) %>% 
   strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
csvnamesRF[!csvnamesRF%in%csvnamesGWR]


# year_i=1
csv_names
nfold=5

write_output_5csv <- function(year_i){
   print(year_i)
   print(csv_names[year_i])
   
   if(grepl('sep', csv_names[year_i])){
      # Get all the monthly results
      # filenames <- list.files('data/workingData/', paste0('RF_result_all_', csv_names[year_i]))
      print('slr')
      slr_l <- lapply(paste0("data/workingData/SLR_result_all_", csv_names[year_i], 
                           "_fold_", seq(1,nfold), '_m', rep(1:12, nfold), ".csv"), 
                    read.csv)
      print('rf')
      rf_l <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], 
                    "_fold_", seq(1,nfold), '_m', rep(1:12, nfold), ".csv"), 
             read.csv)
      print('gwr')
      gwr_l <- lapply(paste0("data/workingData/GWR_result_all_", csv_names[year_i], 
                            "_fold_", seq(1,nfold), '_m', rep(1:12, nfold), ".csv"), 
                     read.csv)
   }else{
      print('slr')
      slr_l <- lapply(paste0("data/workingData/SLR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                    read.csv)
      print('rf')
      rf_l <- lapply(paste0("data/workingData/RF_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                   read.csv)
      print('gwr')
      gwr_l <- lapply(paste0("data/workingData/GWR_result_all_", csv_names[year_i], "_fold_", seq(1,nfold), ".csv"), 
                     read.csv)
   }
   
   slr_test <- lapply(slr_l, function(df_data) df_data %>% filter(df_type=='test'))
   
   rf_test <- lapply(rf_l, function(df_data) df_data %>% filter(df_type=='test'))
   gwr_test <- lapply(gwr_l, function(df_data) df_data %>% filter(df_type=='test'))
   
   slr_test <- do.call(rbind, slr_test)
   # gwr_rf_test <- do.call(rbind, gwr_rf_test)
   rf_test <- do.call(rbind, rf_test)
   gwr_test <- do.call(rbind, gwr_test)
   all_test <- cbind(rf=rf_test$rf, 
                     gwr=gwr_test$gwr,
                     slr_test)
   write.csv(all_test, paste0("data/workingData/5cv_", csv_names[year_i], ".csv"), row.names = F)
}
lapply(seq_along(csv_names), write_output_5csv)
# dim(read.csv('data/workingData/5cv_monthly_2010_NO2.csv'))
# dim(read.csv('data/workingData/5cv_monthly_2010_sep_NO2.csv'))
