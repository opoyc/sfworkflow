# utils

"%nin%" <- Negate("%in%")


# reading files ----

read_knx <- function(file_path){
  # labels ----
  forecast_item_conf_label <- c("abc", "xyz", "total_buckets", "forecast_item", "active_baseline"
                                , "category", "unit_measure_forecast", "unit_measure_col_x8"
                                , "unit_measure_control_set", "actuals_category", "item_usage_rule"
                                , "item_status_update", "col_x13", "configured", "col_x15"
                                , "product_lifecycle_type", "col_x17", "set_param_set", "set_history_skip_leading_zeros"
                                , "set_holdout_period_usage_rule", "set_forecast_model", "set_trend_decay_factor"
                                , "set_fit_measure", "set_model_constant_usage", "set_calendar", "col_x26"
                                , "intervals_historical", "intervals_forecast", "intervals_season_cycle"
                                , "intervals_moving_average", "confidence_level", "col_x32", "best_fit_model_set"
                                , "best_fit_holdout_period", "best_fit_forecast_lag", "col_x36", "arima_constant"
                                , "arima_terms_ar", "arima_terms_ma", "arima_terms_diff", "col_x41", "r_forecast_param_set"
                                , "r_forecast_elastic_net_weight", "r_forecast_elastic_net_regularization"
                                , "r_forecast_arimax_constant", "r_forecast_arimax_difference_level", "autocorr_conf_level"
                                , "autocorr_conf_level_apply", "col_x49", "hist_demand_start_date", "hist_demand_items_actuals"
                                , "hist_demand_actuals_from_other_items", "hist_demand_actuals_from_count", "col_x54"
                                , "hist_demand_actuals_by_other_items", "col_x56", "adjustments_start", "adjustments_profile"
                                , "adjustments_quantity", "adjustments_multiplier", "col_x61", "forecast_start", "forecast_stop"
                                , "override_forecast_start", "override_forecast_stop", "outlier_type", "outlier_view"
                                , "outlier_has_outliers", "outlier_detec_set_rules_data", "outlier_detec_set_rules_detec"
                                , "outlier_detec_set_threshold", "outlier_detec_set_ma_window", "output_errors"
                                , "output_characteristics")
  active_reg_sum_label <- c("forecast_item", "forecast_category", "forecast_model", "regressor", "lag", "regressor_action"
                            , "estimate", "p_value", "standard_error", "t_statistic")
  edit_reg_value_label <- c("regressor", "category", "date", "quantity")
  fcst_items_label <- c("select", "x", "forecast_item", "x_2", "forecast_category", "x_3"
                        , "r_forecast_reg_total", "r_forecast_reg_active"
                        , "r_forecast_reg_weight_decay_factor", "r_forecast_elastic_net_weight"
                        , "r_forecast_elastic_net_regu", "r_forecast_arimax_constant", "r_forecast_arimax_diff_level"
                        , "x_4", "set_history_skip_leading_zeros", "set_parameter_set"
                        , "set_param_model_constant_usage", "set_forecast_model", "set_trend_decay_factor"
                        , "set_autocorr_cl", "x_5", "actuals_category"
                        , "x_6", "intervals_historical", "intervals_forecast", "intervals_season_cycle")
  abc_xyz_calc_label <- c("forecast_item", "abc", "abc_volume", "abc_revenue", "xyz", "total_volume"
                          , "total_volume_perc", "total_volume_cum", "total_revenue", "total_revenue_perc"
                          , "total_revenue_cum", "cov")
  reg_usage_sum_label <- c("forecast_item", "regressor", "processing_rule", "lag")
  regs_label <- c("select", "name", "category")
  stat_out_clean_label <- c("forecast_item", "associated_cat_forecast", "associated_cat_historical"
                            , "stat_outlier_conf", "stat_outlier_sum", "category_1", "category_2")
  fcst_comp_label <- c("forecast_item", "forecast_category", "actuals_category"
                       , "product_lifecycle", "category_1", "category_2")
  causal_factor_label <- c("forecast_item", "category_1", "category_2")
  
  
  
  reg_exclude <- c("intercept","(Intercept)","season_01","season_02","season_03"
                   ,"season_04","season_05","season_06","season_07","season_08"
                   ,"season_09","season_10","season_11","trend","ar1","ar2"
                   ,"ar3","ar4","ar5",
                   "ma1","ma2","ma3","ma4","ma5","drift")
  
  # file names ----
  
  files <- data.frame(file=c("[Forecast Item] - ABC XYZ Calculation.xlsx", "[Forecast Item] Configuration.xlsx"
                             , "Active Regressor Summary.xlsx", "Causal Factor Cleansing - Summary.xlsx"
                             , "Edit Regressor Values.xlsx", "Forecast Comparison.xlsx"
                             , "Forecast Items.xlsx", "life savings.xlsx", "Regressor Usage Summary.xlsx"
                             , "Regressors.xlsx", "Statistical Outliers Cleansing.xlsx")
                      , index=1:11, stringsAsFactors = F)
  
  # active file ----
  
  file <- tail(unlist(strsplit(file_path, split = "/")), 1)
  if((file %in% files$file)==F){
    stop("File name does not match with existing configuration")
  }
  active <- files$file[(which(grepl(pattern = file, x = files$file, fixed = T)==T))]
  
  # reading files ----
  
  if(active %in% c("[Forecast Item] Configuration.xlsx", "life savings.xlsx")){
    return(
      readxl::read_xlsx(file_path, skip = 4, col_names = forecast_item_conf_label, progress = T)
    )
  }
  if(active=="Active Regressor Summary.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip = 2, col_names = active_reg_sum_label) %>% 
        filter(!(regressor %in% reg_exclude)) %>% 
        dplyr::select(-6)
    )
  }
  if(active=="Edit Regressor Values.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip=1, col_names = edit_reg_value_label)
    )
  }
  if(active=="Forecast Items.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip=3, col_names = fcst_items_label) %>% 
        filter(forecast_category=="Statistical")
    )
  }
  if(active=="Forecast Comparison.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip=2) %>% 
        janitor::clean_names() %>% 
        setNames(nm = c(fcst_comp_label, names(.)[7:length(names(.))])) %>% 
        filter(forecast_category=="Statistical")
    )
  }
  if(active=="[Forecast Item] - ABC XYZ Calculation.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip=2, col_names = abc_xyz_calc_label)
    )
  }
  if(active=="Regressor Usage Summary.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip = 1, col_names = reg_usage_sum_label)
    )
  }
  if(active=="Regressors.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip = 1, col_names = regs_label)
    )
  }
  if(active=="Causal Factor Cleansing - Summary.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip=1) %>% 
        janitor::clean_names() %>% 
        setNames(nm = c(causal_factor_label, names(.)[4:length(names(.))])) %>% 
        slice(7:n())
    )
  }
  if(active=="Statistical Outliers Cleansing.xlsx"){
    return(
      readxl::read_xlsx(file_path, skip = 1) %>% 
        janitor::clean_names() %>% 
        setNames(nm = c(stat_out_clean_label, names(.)[8:length(names(.))])) %>% 
        fill(category_1, .direction = "down")
    )
  }
}


# Wrangling ---- 


get_reg_table <- function(act_reg_sum, edit_reg_val){
  reg_table <- act_reg_sum %>% 
    left_join(edit_reg_val, by = "regressor", suffix=c("_sum", "_values")) %>%
    mutate(date = lubridate::ymd(date)) %>%
    arrange(forecast_item, regressor, date) %>%
    group_by(regressor, forecast_item, forecast_category, forecast_model, lag, estimate
             , p_value, standard_error, t_statistic, category) %>%
    {if (nrow(.)!=0) tidyr::complete(., date = seq.Date(from =  min(.$date)
                                                        , to = max(.$date)
                                                        , by = "month") ) else .} %>% 
    fill(quantity) %>%
    ungroup()
  return(reg_table)
}

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

get_cleaned_data_before_outlier <- function(stat_outlier_clean, angola_prods, prunned_prods){
  stat_outlier_clean %>% 
    select(-stat_outlier_sum, -stat_outlier_conf) %>% 
    filter(category_2=="Cleansed History") %>% 
    mutate(type=NA) %>% 
    filter(forecast_item %nin% c(angola_prods$forecast_item, prunned_prods$forecast_item))
}

get_sales_raw_def <- function(fcst_comp){
  # NEW ACTUALS CATEGORY FOUND IN KINAXIS WITH PATIENT BASED COMMERCIAL EQUIVALENT 
  fcst_comp %>% 
    filter(category_1=="Raw Actuals"
           , forecast_category=="Statistical"
           , actuals_category=="HistoricalSales")
}

get_sales_cleansed <- function(fcst_comp){
  fcst_comp %>% 
    filter(category_1=="Cleansed History")
}

## Model params -----

get_model_params <- function(fcst_conf, gbu, fcst_items_params){
  model_params <- fcst_conf %>% 
    filter(category=="Statistical") %>% 
    dplyr::select(forecast_item, intervals_season_cycle, forecast_stop, hist_demand_start_date
                  , forecast_start, active_baseline, set_param_set, set_forecast_model
                  , set_trend_decay_factor, arima_constant, arima_terms_ar
                  , arima_terms_ma, arima_terms_diff
                  , r_forecast_param_set, hist_demand_actuals_from_other_items
                  , outlier_type, outlier_detec_set_rules_data, outlier_detec_set_rules_detec
                  , outlier_detec_set_threshold, outlier_detec_set_ma_window) %>% 
    mutate_at(vars(forecast_stop, hist_demand_start_date, forecast_start), .funs = ~as.Date(.x)) %>% 
    {if (length(grep(x =.$forecast_item, pattern = "PT-AO"))!=0) dplyr::slice(.,-grep(x = forecast_item
                                                                                      , pattern = "PT-AO")) else .} %>%
    mutate(loc=stringr::str_extract(forecast_item, "[A-Z]{2}")
           , gmid=stringr::str_extract(forecast_item, pattern = "(?<=([A-Z]{2}: ))[0-9]+")
           , description=stringr::str_extract(forecast_item, "(?<=-).*")
           , key=paste0(loc,": ", gmid)
           , set_forecast_model=case_when(
             set_forecast_model=="Rforecast" ~ "ETS"
             , set_forecast_model=="CrostonMethod" ~ "Croston"
             , set_forecast_model=="MultipleLinearRegression" ~ "MLR"
             , set_param_set=="ARIMA" ~ "ARIMA Default"
             , TRUE~set_forecast_model)
           , parameter=case_when(
             set_forecast_model=="ETS" ~ paste(r_forecast_param_set, "Trend Decay: ", as.numeric(set_trend_decay_factor))
             , set_forecast_model=="MLR" ~ paste("MLR Trend Decay: ", as.numeric(set_trend_decay_factor))
             , set_forecast_model=="ARIMA" ~ paste("(", arima_terms_ar, arima_terms_diff, arima_terms_ma, ") Constant: ", arima_constant)
             , set_forecast_model=="ARIMA Default" ~ "NA"
             , TRUE ~ set_forecast_model)
           , to_plot=case_when(
             forecast_stop<=today() ~ "NO"
             , outlier_type=="Default" & is.na(hist_demand_actuals_from_other_items) ~ "NO"
             , outlier_detec_set_rules_data=="Moving Average Error" & is.na(hist_demand_actuals_from_other_items) ~ "NO"
             , TRUE ~ "YES")
           , new_code=ifelse(is.na(hist_demand_actuals_from_other_items), "NO", "YES")
    ) %>% 
    dplyr::select(loc, gmid, description, key, forecast_item, active_baseline
                  , set_forecast_model, parameter, everything()
                  , -forecast_start,-set_param_set,-set_trend_decay_factor
                  , -arima_constant,-arima_terms_ar,-arima_terms_ma
                  , -arima_terms_diff,-r_forecast_param_set, -outlier_type) %>% 
    left_join(gbu, by=c("key"), suffix=c("_origin", "_gbu")) %>% 
    left_join(fcst_items_params, by=c("forecast_item")) %>% 
    mutate(gbu=ifelse(gbu %in% c("PRC", "CHC", "GZ", "VAC", "CEM"), "", gbu))
  
  return(model_params)
}