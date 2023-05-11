seed <- 123
local_crs <- CRS("EPSG:3035")
# target_poll = 'NO2'
eu_bnd <- st_read("../expanse_shp/eu_expanse2.shp")
read_data <- function(target_poll, yr){
   # Read in observations and predictor values
   if(target_poll=='PM2.5'){
      target_str='pm25'
   }else{
      target_str=tolower(target_poll)
   }
   
   df_all <- read.csv(paste0('data/raw/gee/pred_', target_str, '_',
                             yr, '_monthly.csv'))
   # Read in station metadata
   sta <- read.csv("../EXPANSE_APM/data/processed/all_sta.csv")
   # # There are 3 missing rows and 7 rows with unknown values for sta_type
   # # Remove them
   # sta <- sta %>% filter(sta_type!="Unknown", sta_type!="")
   df_all_subset <- df_all %>% filter(year%in%yr)
   df_all_subset$year <- as.factor(df_all_subset$year)
   df_all_subset$month <- as.factor(df_all_subset$month)
   inner_join(df_all_subset, sta %>% dplyr::select(-sta_type, -cntr_code), by='sta_code')
}
