source("functions.R")
source("load_packages.R")

plan <- drake_plan(
  # inputs
  abc_xyz = read_knx(file_path = file_in('data/[Forecast Item] - ABC XYZ Calculation.xlsx'))
  , fcst_conf = read_knx(file_path = file_in('data/[Forecast Item] Configuration.xlsx'))
  , act_reg_sum = read_knx(file_path = file_in('data/Active Regressor Summary.xlsx'))
  , causal_fact_clean = read_knx(file_path = file_in('data/Causal Factor Cleansing - Summary.xlsx'))
  , edit_reg_val = read_knx(file_path = file_in('data/Edit Regressor Values.xlsx'))
  , fcst_comp = read_knx(file_path = file_in('data/Forecast Comparison.xlsx'))
  , fcst_items_params = read_knx(file_path = file_in('data/Forecast Items.xlsx'))
  , life_savings = read_knx(file_path = file_in('data/life savings.xlsx'))
  , reg_usage_sum = read_knx(file_path = file_in('data/Regressor Usage Summary.xlsx'))
  , regressors = read_knx(file_path = file_in('data/Regressors.xlsx'))
  , stat_outlier_clean = read_knx(file_path = file_in('data/Statistical Outliers Cleansing.xlsx'))
  , gbu = load_gbu(on_globalenv = FALSE)
  
  # wrangling and converting
  , reg_table = get_reg_table(act_reg_sum, edit_reg_val)
  , model_params = get_model_params(fcst_conf, gbu, fcst_items_params)
  , angola_prods = get_angola_prods(fcst_conf)
  , prunned_prods = get_prunned_prods(fcst_conf, angola_prods)
  , cleaned_data_before_outier <- get_cleaned_data_before_outlier(stat_outlier_clean, angola_prods, prunned_prods)
  , sales_raw_def = get_sales_raw_def(fcst_comp)
  , sales_cleansed = get_sales_cleansed(fcst_comp)
)

config <- drake_config(plan)
vis_drake_graph(config)
make(plan)
