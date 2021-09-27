run_rf <- function(df_train, y_varname, x_varname,
                   hyper_grid, seed=123, tuneRF=F){
   if(tuneRF){
      mtry <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$mtry
      ntrees <- hyper_grid[which.min(hyper_grid$OOB_RMSE),]$ntrees
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = ntrees,
         mtry = mtry,
         seed = seed,
         importance = 'impurity'          # 'permutation'
      )
   }else{
      
      eq <- as.formula(paste(y_varname, "~", paste(x_varname,collapse='+'), sep = ""))
      rf_model <- ranger(
         formula = eq,
         data = df_train,
         num.trees = 500,
         mtry = floor(sqrt(length(x_varname))),
         seed = seed,
         importance = 'impurity'          # 'permutation'
      )
   }
   return(rf_model)
   
}
