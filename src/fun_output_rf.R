output_rf <- function(rf_model, df_train, df_test, y_varname, csv_name, 
                   outputselect = c("station_european_code", "rf", "obs", "res", 
                                    "nfold", "df_type", "year", "index")){
   df_all <- rbind(df_train %>% mutate(df_type = 'train'), 
                   df_test %>% mutate(df_type = 'test'))
   rf_result <- data.frame(rf = predict(rf_model, df_all) %>% predictions(),
                           obs = df_all[, y_varname]) %>% 
      mutate(res = obs - rf) %>% 
      cbind(df_all %>% dplyr::select(-all_of(y_varname))) 
   rf_poll_train <- rf_result[rf_result$df_type=='train', ]
   rf_poll_test <- rf_result[rf_result$df_type=='test', ]
   
   eval_train <- error_matrix(rf_poll_train$obs, rf_poll_train$rf)
   eval_test <- error_matrix(rf_poll_test$obs, rf_poll_test$rf)
   
   rf_result <- rf_result[, outputselect]
   
   write.csv(rf_result, 
             paste0('data/workingData/RF_result_all_', csv_name, '.csv'), 
             row.names = F)
   # Variable importance
   var_importance <- data.frame(var_name = rf_model$variable.importance %>% names, 
                                vi = rf_model$variable.importance %>% as.numeric())
   var_importance <- var_importance[with(var_importance, order(-vi)), ]
   
   
   write.csv(var_importance, paste0('data/workingData/RF_vi_', csv_name, '.csv'), 
             row.names = F)
   return(list(rf_result=rf_result, eval_train=eval_train, eval_test=eval_test))
   
}