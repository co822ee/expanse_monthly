source("../EXPANSE_algorithm/scr/fun_call_lib.R")
library(reshape2)
library(GGally)
library(APMtools)

eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
met_str <- 'precip|temp|wind|pressure'
years <- 2000:2019
nfold=5
####### Useful functions ##########
read_perfm <- function(poll, sep=F){
   if(sep){
      csv_names <- gsub('SLR_result_all_', '', 
                        list.files('data/workingData/', 
                                   paste0('SLR_result_all_monthly_2010_sep_', poll))) %>% 
         strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   }else{
      csv_names <- gsub('SLR_result_all_', '', 
                        list.files('data/workingData/', 
                                   paste0('SLR_result_all_monthly_2010_', poll))) %>% 
         strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   }
   

   all_test <- lapply(paste0("data/workingData/5cv_", csv_names, ".csv"), read.csv)
   all_test
}


read_perfm2 <- function(poll){
   csv_names <- gsub('SLR_result_all_', '', 
                     list.files('../EXPANSE_algorithm/data/workingData/', 
                                paste0('SLR_result_all_o3_', poll))) %>% 
      strsplit(., '_fold_') %>% lapply(., `[[`, 1) %>% unlist() %>% unique
   years <- csv_names %>% substr(., nchar(csv_names)-3, nchar(csv_names)) %>% 
      as.numeric() %>% as.list()
   
   all_test <- lapply(paste0("../EXPANSE_algorithm/data/workingData/5cv_", csv_names[unlist(years)==2010], ".csv"), read.csv)
   all_test
}
show_EM <- function(all_test, all_df_i){
   
   slr=all_test[[all_df_i]]$slr
   rf=all_test[[all_df_i]]$rf
   df_all <- data.frame(slr=error_matrix(all_test[[all_df_i]]$obs, slr),
                        rf=error_matrix(all_test[[all_df_i]]$obs, rf))[c(1,5,7),]
   df_all$EM = row.names(df_all)
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
   files <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_", poll)))]
   slr_vars <- lapply(seq_along(files), extract_slr_var, poll=poll, files=files) %>% do.call(rbind, .)
   rf_vars <- lapply(seq_along(files), extract_rf_var, poll=poll, files=files) %>% do.call(rbind, .)
   slr_tbl <- with(slr_vars, table(var_name, yr))
   rf_tbl <- with(rf_vars, table(var_name, yr))
   slr_tbl <- slr_tbl[mixedorder(row.names(slr_tbl), decreasing=T),]
   rf_tbl <- rf_tbl[mixedorder(row.names(rf_tbl), decreasing=T),]
   
   files_monthly <- list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_sep_", poll))[grepl("fold", list.files("data/workingData/", paste0("SLR_summary_model_monthly_2010_sep_", poll)))]
   files_monthly <- files_monthly[mixedorder(files_monthly)]
   slr_vars_m <- lapply(seq_along(files_monthly), extract_slr_var_monthly, poll=poll, files=files_monthly) %>% do.call(rbind, .)
   rf_vars_m <- lapply(seq_along(files_monthly), extract_rf_var_monthly, poll=poll, files=files_monthly) %>% do.call(rbind, .)
   
   slr_vars_all <- rbind(slr_vars_m, slr_vars %>% mutate(month='12-month'))
   rf_vars_all <- rbind(rf_vars_m, rf_vars %>% mutate(month='12-month'))
   
   slr_tbl_m <- with(slr_vars_all, table(var_name, month))
   rf_tbl_m <- with(rf_vars_all, table(var_name, month))
   slr_tbl_m <- slr_tbl_m[mixedorder(row.names(slr_tbl_m), decreasing=T), mixedorder(colnames(slr_tbl_m))]
   rf_tbl_m <- rf_tbl_m[mixedorder(row.names(rf_tbl_m), decreasing=T), mixedorder(colnames(rf_tbl_m))]
   
   print(heatmap(rf_tbl_m, Colv = NA, Rowv = NA, scale="column", main=paste0('RF: ', poll)))
   print(heatmap(slr_tbl_m, Colv = NA, Rowv = NA, scale="column", main=paste0('SLR: ', poll)))
}

target_poll = 'NO2'
# Monthly models (built for all 12 months)
all_test <- read_perfm(target_poll)
em_df1 <- lapply(seq_along(all_test), show_EM, all_test=all_test)
em_df1 <- do.call(rbind, em_df1)
em_df1$config <- '12-month'

# Monthly models (built for each month)
all_test_sep <- read_perfm(target_poll, T)
em_df1_sep <- lapply(seq_along(all_test_sep), show_EM, all_test=all_test_sep)
em_df1_sep <- do.call(rbind, em_df1_sep)
em_df1_sep$config <- '1-month'

# Annual models (bult for a year)
all_test2 <- read_perfm2(target_poll)
em_df2 <- lapply(seq_along(all_test2), show_EM, all_test=all_test2)
em_df2 <- do.call(rbind, em_df2)
em_df2$config <- 'annual'

em_df <- rbind(em_df1, em_df1_sep, em_df2)
# Overall
ggplot(em_df %>% filter(EM=='rsq') %>%  gather("model", "values", -c("EM", 'config')), 
       aes(x=model, y=values, fill=config))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="R squared")+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   geom_hline(aes(yintercept=0.575))

ggplot(em_df %>% filter(EM=='RMSE') %>%  gather("model", "values", -c("EM", 'config')), 
       aes(x=model, y=values, fill=config))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="RMSE")+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   geom_hline(aes(yintercept=09.51))

# Aggregate the monthly 5-fold CV predictions into annually
all_m <- (all_test[[1]])
all_my <- all_m %>% group_by(sta_code, year) %>% summarise(rf=mean(rf), slr=mean(slr))


all_m_sep <- (all_test_sep[[1]])
all_m_sepy <- all_m_sep %>% group_by(sta_code, year) %>% summarise(rf_m_sep=mean(rf), slr_m_sep=mean(slr))


all_my <- all_my %>% rename(rf_m=rf, slr_m=slr)
all_y <- all_test2[[1]]
names(all_y)
all_y <- inner_join(all_y, all_my, by=c('sta_code', 'year'))
all_y <- inner_join(all_y, all_m_sepy, by=c('sta_code', 'year'))

# 
plot(all_y$rf, all_y$rf_m)
plot(all_y$rf, all_y$rf_m_sep)
plot(all_y$rf_m, all_y$obs)
plot(all_y$rf, all_y$obs)
abline(0, 1)
plot(all_y$slr, all_y$obs)
plot(all_y$slr_m_sep, all_y$obs)
plot(all_y$slr, all_y$slr_m)

em_all <- data.frame(slr=error_matrix(all_y$obs, all_y$slr),
                     rf=error_matrix(all_y$obs, all_y$rf),
                     slr_m=error_matrix(all_y$obs, all_y$slr_m),
                     rf_m=error_matrix(all_y$obs, all_y$rf_m),
                     slr_m_sep=error_matrix(all_y$obs, all_y$slr_m_sep),
                     rf_m_sep=error_matrix(all_y$obs, all_y$rf_m_sep)
                     )[c(1,5,7),]
em_all$EM = row.names(em_all)

# Aggregate the predictions into annual scale
ggplot(em_all %>% filter(EM=='rsq') %>%  gather("model", "values", -c("EM")), aes(x=model, y=values))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="R squared")+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   geom_hline(aes(yintercept=0.575))

ggplot(em_all %>% filter(EM=='RMSE') %>%  gather("model", "values", -c("EM")), aes(x=model, y=values))+
   geom_bar(stat="identity", position = "dodge2")+
   # facet_grid(EM~., scales ='free')+
   # labs(title=years[[i]])+
   labs(y="RMSE")+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   geom_hline(aes(yintercept=09.51))

library(reshape2)
library(GGally)
#------importance variable------
create_heatmap_monthly(target_poll)


## The most influential variables are the time-varying variables, 
## but then it shows that it is still difficult for RF to capture the temporal variations for some stations,
## and also the correlation of predictions between months is quite high
##----------- Correlation in the predictions from different months: -----------

plot_cor <- function(all_df, var_c){
   
   no2_wide2 <- dcast(all_df[, c(var_c, 'month', 'sta_code')],
                      as.formula(paste0('sta_code~', 'month')), 
                      value.var = var_c)
   
   upper.panel <- function(x, y){
      points(x, y,xlim=c(-5,220), ylim=c(-5,220))
      abline(0,1, col='red')
   }
   lowerFn <- function(data, mapping, ...) {
      p <- ggplot(data = data, mapping = mapping) +
         geom_point(colour = "black") +
         geom_abline(intercept=0, slope=1, col='red')+
         lims(x=c(-5, 220),y=c(-5, 220))
      p
   }
   png(file = paste0("results/figures/corr_obs", 'corr_', target_poll, "_2010_", var_c, '.png'), 
       res = 150, width = 1300, height = 1300)
   print(ggpairs(data=no2_wide2[,-c(1)], lower = list(continuous = wrap(lowerFn)),
                 title = var_c,
                 diag=list(discrete="barDiag", 
                           continuous = wrap("densityDiag", alpha=0.5))))
   dev.off()
   return(0)
}

plot_cor(all_m, 'obs')
# plot_cor(all_m_sep, 'obs')

plot_cor(all_m, 'slr')
plot_cor(all_m_sep, 'slr')

plot_cor(all_m, 'rf')
plot_cor(all_m_sep, 'rf')



### 
library(sm)
all_m_wide <- all_m %>% gather('model', 'values', c('slr', 'obs', 'rf'))
# create value labels
all_m_wide$model <- factor(all_m_wide$model, levels= c('slr', 'rf', 'obs'),
                labels = c("SLR", "RF", "observations"))
# sm.density.compare(all_m_wide$values, all_m_wide$model, 
#                    xlab="NO2 concentrations")

ggplot(all_m_wide, aes(x=values, fill=model))+
   geom_density(alpha=.6)  +
   facet_wrap(month~.)

ggplot(all_m_wide, aes(x=model, y=values))+
   geom_boxplot(alpha=.6)  +
   facet_wrap(month~.)

## Questions: why the correlations between months are so high in RF predictions?
##            but then still RF performs better than SLR...
## Is it because the current validation method is spatial leave-out CV instead of temporal CV?
# Should I use models trained by all data instead?
#------- Plot the time series first:----------
library(lubridate)
all_mt <- all_m %>% mutate(date=as.Date(make_datetime(year, month)),
                 cntr=substr(sta_code, 1, 2))
ggplot(all_mt %>% filter(sta_code%in%unique(all_mt$sta_code)[1:24]) %>% 
          gather('model', 'values', c('slr','obs','rf')))+
   geom_line(aes(x=date, y=values, col=model))+
   facet_wrap(sta_code~.)

