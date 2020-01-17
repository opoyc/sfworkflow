cleansed_data_before_outlier<-readxl::read_xlsx(paste0(path.Excel, "Statistical Outliers Cleansing.xlsx"),
                                                sheet = 1,col_names = T,skip=1) %>%
  rename("data_type"=...7) %>%
  filter(data_type=="Cleansed History") %>%
  rename("Item"=Id, "Category...2"=Forecast, "Category...3"=Historical)%>%
  mutate(Type=NA) %>%
  select(-c("Summary","Configuration")) %>%
  select(Item,Category...2,Category...3,Type, data_type, everything() ) %>%
  filter(Item%not.in%Angola.products$Item) %>% 
  filter(Item%not.in%pruned.products$Item)

get_cleaned_data_before_outier <- function(stat_outlier_clean, angola_prods, prunned_prods){
  stat_outlier_clean %>% 
    select(-stat_outlier_sum, -stat_outlier_conf) %>% 
    filter(category_2=="Cleansed History") %>% 
    mutate(type=NA) %>% 
    filter(forecast_item %nin% c(angola_prods$forecast_item, prunned_prods$forecast_item))
}

  


angola_prods <- fcst_conf %>% 
  filter(stringr::str_detect(forecast_item, "PT-AO:"))
prunned_prods <- fcst_conf %>% 
  filter(forecast_item %nin% angola_prods$forecast_item) %>% 
  select(forecast_item, forecast_stop) %>%
  filter(forecast_stop < lubridate::floor_date(today(),"month")) %>% 
  filter(forecast_stop!=lubridate::ymd("1995-01-01"))


get_angola_prods <- function(fcst_conf){
  fcst_conf %>% 
    filter(stringr::str_detect(forecast_item, "PT-AO:"))
}

get_prunned_prods <- function(fcst_conf, angola_prods){
  fcst_conf %>% 
    filter(forecast_item %nin% angola_prods$forecast_item) %>% 
    select(forecast_item, forecast_stop) %>%
    filter(forecast_stop < lubridate::floor_date(today(),"month")) %>% 
    filter(forecast_stop!=lubridate::ymd("1995-01-01"))
}




