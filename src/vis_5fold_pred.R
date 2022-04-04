#### !!!!! visualize the predictors selected by the final monthly LUR models!!!!!!
library(ggplot2)
library(dplyr)
library(wesanderson)
library(gtools)
output_pred_freq_df <- function(poll){
   filenames <- list.files('data/workingData/', 'SLR_summary_model_all_monthly_')
   subfiles <- filenames[grepl(poll, filenames)]
   subfiles <- subfiles[!grepl('_fold_', subfiles)]
   sub_df <- lapply(subfiles, function(subfile){
      read.csv(paste0('data/workingData/', subfile)) %>% 
         mutate(month=as.numeric(gsub('m', '', strsplit(subfile, '_')[[1]][9]) %>% gsub('.csv','',.)),  #10
                year=as.numeric(strsplit(subfile, '_')[[1]][6]), #5
                # fold=as.numeric(strsplit(subfile, '_')[[1]][9]),
                poll=poll)
      
   })
   sub_dfs <- do.call(rbind, sub_df)
   sub_dfs <- sub_dfs[sub_dfs$variables!='Final',]
   freq_dfs <- sub_dfs %>% group_by(month, variables, poll) %>% count()
   class_pred <- data.frame(class=c('met','LULC','pop or topo', 'SAT or CTM', 'allRoads', 'majorRoads'), 
                            pred=c('temp|wind|precip', 'imd|por|res|ugr|ind|nat',
                                   'pop|alt', 'dehm|MACC|omi|AOD', 'allRoad', 'majorRoads'))
   
   dflist1 <- lapply(sapply(class_pred$pred, function(i) grep(i, freq_dfs$variables)), 
                     function(i){
                        freq_dfs[i,]
                     }) 
   
   dflistnrow <- lapply(sapply(class_pred$pred, function(i) grep(i, freq_dfs$variables)), 
                     function(i){
                        nrow(freq_dfs[i,])
                     }) 
   freq_dfs <- lapply(seq_along(dflist1), function(i){
      dflist1[[i]] <- dflist1[[i]][mixedorder(dflist1[[i]]$variables),]
      
      dflist1[[i]]$nameorder <- 1:nrow(dflist1[[i]])
      dflist1[[i]] %>% mutate(class=(i-1)*dflistnrow[[i]]+nameorder)
   }) %>% do.call(rbind,.)
   freq_dfs
}
freq_dfs <- lapply(c('NO2','O3','PM10', 'PM2.5'), output_pred_freq_df)
freq_dfs2 <- do.call(rbind, freq_dfs)
pal <- wes_palette("Zissou1", 100, type = "continuous")
freq_dfs3 <- rbind(freq_dfs2 %>% group_by(variables, poll) %>% summarise(sum=sum(n))%>% filter(poll!='PM2.5') %>% filter(sum>20),
                   freq_dfs2 %>% group_by(variables, poll) %>% summarise(sum=sum(n))%>% filter(poll=='PM2.5') %>% filter(sum>13))
freq_dfs_final <- left_join(freq_dfs2, freq_dfs3, by=c("variables", 'poll'))
freq_dfs_final <- freq_dfs_final[!is.na(freq_dfs_final$sum),]
# freq_dfs2 <- rbind(freq_dfs2 %>% filter(poll!='PM2.5') %>% filter(n>5),
#                   freq_dfs2 %>% filter(poll=='PM2.5'))

ggplot(freq_dfs_final %>% filter(n>3))+
   geom_tile(aes(x=as.factor(month), y=reorder(variables, class), fill=n))+
   facet_wrap(poll~., scales = 'free_y')+
   scale_fill_gradientn(colours = pal)+
   labs(x='month', y='', fill='count')+
   theme(axis.title = element_text(size = 13),
         axis.text = element_text(size = 11),
         legend.title = element_text(size = 13),
         legend.text = element_text(size = 13),
         strip.text = element_text(size = 12))
ggsave('results/figures/slr_predictors_monthly.tiff', width=10, height=9,
       units='in', dpi=300)

