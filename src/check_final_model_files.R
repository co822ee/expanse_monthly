filenames <- list.files('data/workingData/', 'SLR_summary_model_all')
filenames[grep('PM2.5', filenames)] %>% length
filenames[grep('NO2', filenames)] %>% length
filenames[grep('O3', filenames)] %>% length
filenames[grep('PM10', filenames)] %>% length


filenames <- list.files('data/processed//', 'GWRcoef_all')[!grepl('.aux', list.files('data/processed/', 'GWRcoef_all_'))]
filenames[grep('PM2.5', filenames)] %>% length
filenames[grep('NO2', filenames)] %>% length
filenames[grep('O3', filenames)] %>% length
filenames[grep('PM10', filenames)] %>% length

list.files('data/workingData/', '5cv_monthly_')
