cleansed_data_before_outlier<-
  cleansed_data_before_outlier %>%
  select(Item,  Category...2, Category...3, Type , data_type, ...6,which(names(.)==names(sales_cleansed)[7]):ncol(.)) %>% 
  rbind(
    sales_cleansed[which(sales_cleansed$Item%not.in%cleansed_data_before_outlier$Item==T),] %>% 
      select(1:which(names(.)==data_cleansing_end_date)) # to modify
  ) %>% cbind(select(Sales_raw_def %>% filter(Category...2=="Statistical"),
                     names(Sales_raw_def)[which(names(Sales_raw_def)%not.in%names(cleansed_data_before_outlier))]))



cleaned_data_before_outier