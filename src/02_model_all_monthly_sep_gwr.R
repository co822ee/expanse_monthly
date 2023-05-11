if(!dir.exists('data/workingData')) dir.create('data/workingData')
if(!dir.exists('data/processed')) dir.create('data/processed')
library(doParallel)
library(foreach)
cluster_no <- detectCores()
cl <- parallel::makeCluster(cluster_no)
doParallel::registerDoParallel(cl)
# foreach(target_poll=c('NO2', 'O3', 'PM10', 'PM2.5'))%dopar%{
foreach(target_yr=2000:2019)%dopar%{
   tuneRF_b = T
   run_i=1
   source("../EXPANSE_algorithm/scr/fun_call_lib.R")
   source("src/00_fun_read_monthly_data_gee.R")
   source("../expanse_multiyear/src/00_fun_create_fold.R")
   source("../EXPANSE_algorithm/scr/fun_setupt_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_calibr_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_gwr.R")
   source("../EXPANSE_algorithm/scr/fun_output_gwr_result.R")
   # for(target_yr in 2000:2019){
   for(target_poll in c('NO2', 'O3', 'PM10', 'PM2.5')){
      csv_name <- paste0('all_monthly_', target_yr,'_sep_', target_poll)  
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
         
         
         #---------#f# GWR: train GWR -----------
         if(all(unlist(lapply(paste0('data/workingData/SLR_summary_model_', csv_name, '_m', 1:12, '.csv'), file.exists)))){
            if(!file.exists(paste0('data/processed/GWRcoef_', csv_name, '.tiff'))){
               # The folds are spatially leave-out so that there is no overlapping 
               # between each data
               # What we need to do is to build model for each month
               gwr_month <- function(df_sub, csv_name, sub_month){
                  train_sub_m <- df_sub %>% filter(month==sub_month)
                  csv_name_m <- paste0(csv_name, '_m', sub_month)
                  setup <- setup_gwr(train_sub_m, eu_bnd,
                                     cellsize = 200000, local_crs = local_crs, 
                                     xcoord="xcoord", ycoord="ycoord")
                  sp_train <- setup[[1]]
                  grd <- setup[[2]]
                  DM <- setup[[3]]
                  nngb <- tryCatch(calibr_gwr(sp_train, csv_name_m), 
                                   error=function(e) T)
                  if(typeof(nngb)!='logical'){
                     print(paste0("nngb: ", nngb))
                     gwr_model <- gwr(sp_train, grd, DM, nngb, csv_name_m)
                     gwr_model
                  }
               }
               gwr_models_m <- lapply(1:12, gwr_month, df_sub=df_sub, csv_name=csv_name)
               #f# SLR: test SLR
               coef_stack_l <- lapply(seq_along(gwr_models_m), function(model_i){
                  gwr_model <- gwr_models_m[[model_i]]
                  coef_stack <- stack(gwr_model$SDF)
                  names(coef_stack) <- paste0('X', target_yr, '_m', model_i, 
                                              '_', seq_along(names(coef_stack)))
                  coef_stack
               })
               coef_stack <- do.call(stack, coef_stack_l)
               writeRaster(coef_stack, paste0('data/processed/GWRcoef_', csv_name, '.tif'),
                           overwrite=T)
               
            }else{
               coef_stack <- stack(paste0('data/processed/GWRcoef_', csv_name, '.tif'))
            }
            
            if(run_i==1){
               gwrCoefAll <- coef_stack
               
            }else{
               gwrCoefAll <- stack(gwrCoefAll, coef_stack)
            }
            run_i <- run_i+1
            
         }
      }
   }
}
parallel::stopCluster(cl)


