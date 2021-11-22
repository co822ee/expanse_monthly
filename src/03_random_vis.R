library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(wesanderson)
target_poll <- 'O3'
all_files <- list.files('data/raw/gee/', 'random')
poll_df <- lapply(paste0('data/raw/gee/', all_files[grep(target_poll, all_files)]), 
                  fread) %>% do.call(rbind,.)
names(poll_df)

zoneID <- lapply(paste0('data/raw/gee/', list.files('data/raw/gee/', 'zoneID')),
                 fread) %>% do.call(rbind, .) %>% rename(zoneID=b1)

all_df <- inner_join(zoneID[, c('zoneID', 'system:index')], 
                     poll_df, by='system:index')
all_df <- inner_join(
           read.csv('../EXPANSE_predictor/data/processed/climate_code.csv'),
           all_df,
           by='zoneID')
exc_name <- names(all_df)[!grepl('diff|ratio|slr', names(all_df))]
model_name <- names(all_df)[grepl('diff|ratio|slr', names(all_df))]

month <- strsplit(model_name, '_') %>% lapply(., `[[`, 2) %>% unlist %>% as.numeric()
model <- strsplit(model_name, '_') %>% lapply(., `[[`, 1) %>% unlist 
df_months <- lapply(sort(unique(month)), function(month_i){
   sub_df <- all_df[, model_name[month==month_i]]
   names(sub_df) <- model[month==month_i]
   cbind(all_df[, exc_name], sub_df)
})

lapply(df_months, names)
cor_months <- lapply(seq_along(df_months), function(month_i){
   df_sub <- df_months[[month_i]]
   # calculate the correlations for each climate zone in each month
   df_sub %>% 
      group_by(climate_zone) %>% 
      summarise(ratio_diff=cor(diff, ratio), 
                ratio_slr1=cor(ratio, `slr1-month`),
                ratio_slr12=cor(ratio, `slr12-month`)) %>% 
      mutate(month=month_i)
})
cor_months <- cor_months %>% do.call(rbind, .)
names(cor_months)

heat_df <- cor_months %>% gather('model', 'values', c(-'month', -'climate_zone')) %>% 
   mutate(month=as.factor(month))

pal <- wes_palette("Zissou1", 100, type = "continuous")
p1 <- ggplot(heat_df, aes(x=month, y=model, fill=values))+
   geom_tile()+
   facet_wrap(climate_zone~.)+
   labs(x='month', y='', fill='R2', title=target_poll)+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))+
   scale_fill_gradientn(colours = pal)
p1
ggplot(heat_df, aes(x=month, y=values, fill=model))+
   geom_bar(stat="identity", position = "dodge2")+
   facet_wrap(climate_zone~.)+
   labs(x='month', y='', fill='R2', title=target_poll)+
   theme(axis.title = element_text(size = 18),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 90),
         legend.title = element_text(size = 16),
         legend.text = element_text(size = 16),
         strip.text.y = element_text(size = 15))

