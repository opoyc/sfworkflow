## MOnthly Data RData generation

library(tidyverse)
library(lubridate)
library(forecast)
library(stlplus)
library(stringi)

Sys.setlocale("LC_TIME", "English")
"%not.in%"<-Negate("%in%")

#Reading functions to extract and upload data from Kinaxis rapid response
# source("//sinsdfs01/regional$/APJ-SC-HUB/2 - Forecasts/10 - Kinaxis Operating Cycle/3 - Functions/webservice/R Forecast.R")

# Market, Date and Paths --------------------------------------------------

market <- "LeadMarkets"
Date <- "Jan - 2020"
Date.KPI <- "Dec - 2019" # date - 1 month

path.Excel <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/0 - Kinaxis data extractions/", market, "/", Date, "/")
path.Save <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/2 - Monthly RDatas (Monthly)/", market, "/", Date, "/")
path.KPI <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/0 - Kinaxis data extractions/", market, "/", Date.KPI, "/")

path.GBU <- "//sinsdfs01/regional$/APJ-SC-HUB/SC.DATA/DATA/Active/"

run.in = "BCN" #BCN, SG
prune.Date <- "1995-01-01" #This is the date set by the forecaster to force the stop of the forecast

forecast_comparison_end_date <- "2022/12/01"
  #paste0(year(today())+3,"/",month(today())-1,"/01")
forecast_comparison_end_date_prev_month <- "2022/11/01"
  #paste0(year(today())+3,"/",month(today())-2,"/01")
data_cleansing_end_date<- "Aug-2020"
  #paste0(lubridate::month(lubridate::today()+months(7), label = T, abbr = T), "-", year(lubridate::today())+1)

############################# GBU #################################################################

product_segmentation_MD<-readxl::read_xlsx(paste0(path.Excel, "[Forecast Item] Configuration.xlsx"),
                                           sheet = 1,col_names = T,skip=3)

if (run.in == "BCN") {
  product_segmentation_MD<-product_segmentation_MD %>% rename("Stop"=Stop...63,"Start__1"=Start...62)
}

Angola.products<-product_segmentation_MD %>% 
  dplyr::slice(grep(x = Item,pattern = "PT-AO:")) 

#pruned products without Disc Date = 1995/01/01 --> this is manually set to stop the forecasts for NULL focus
pruned.products<-product_segmentation_MD %>% 
  filter(Item%not.in%Angola.products$Item) %>% 
  select(Item, "Stop_Date"=Stop) %>% 
  mutate(Stop_Date=as.Date(Stop_Date)) %>% 
  #filter(Stop_Date<today()) %>% 
  filter(Stop_Date < lubridate::floor_date(today(),"month")) %>% 
  filter(Stop_Date!=ymd(prune.Date))

############################# GBU #################################################################

load(paste0(path.GBU, "Specific.GBU.RData"))

GBU<-GBU[grep(GBU$Market.Code,pattern = "01"),]

#############################Regressor tables#################################################################

to.exclude <- c("intercept","(Intercept)","season_01","season_02","season_03","season_04","season_05","season_06","season_07","season_08","season_09","season_10","season_11","trend","ar1","ar2","ar3","ar4","ar5",
                "ma1","ma2","ma3","ma4","ma5","drift")

reg.summary <- readxl::read_xlsx(paste0(path.Excel, "Active Regressor Summary.xlsx"),sheet = 1,col_names = T,skip=1) %>% 
  filter(!(Regressor %in% to.exclude)) %>% 
  dplyr::select(-`...6`)

# API SOLUTION
# Extract_Regressors(server="PROD")
# 
# reg.values<-regressors_kinaxis %>% 
#   filter(Forecast.Item!="") #REVIEW WHEN WE HAVE ARIMAX
# 
# regressor.table <- merge(reg.summary,reg.values,by="Regressor",all.x=TRUE) %>%
#   mutate(Date = dmy(Date)) %>%
#   arrange(Item,Regressor,Date) %>% 
#   tibble

reg.values <- readxl::read_xlsx(paste0(path.Excel, "Edit Regressor Values.xlsx"),sheet = 1,col_names = T,skip=0)


regressor.table <- merge(reg.summary,reg.values,by="Regressor",all.x=TRUE) %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Item,Regressor,Date) %>%
  group_by(Regressor,Item,Category.x,`Forecast Model`,Lag,Estimate,`p Value`,`Standard Error`,`t Statistic`,`Category.y`) %>%
  {if (nrow(.)!=0) tidyr::complete(., Date = seq.Date(from =  min(.$Date,na.rm=T),to = max(.$Date,na.rm=T), by = "month") ) else .} %>% 
  fill(Quantity) %>%
  ungroup()

#regressor.table <- regressor.table [-which(is.na(regressor.table$Date)),]

save(regressor.table,file=paste0(path.Save, "regressor.table.RData"))


#Model info table + outlier methodology table


model_params_outlier_method<-readxl::read_xlsx(paste0(path.Excel, "[Forecast Item] Configuration.xlsx"),
                                   sheet = 1,col_names = T,skip=3) 

time_weight <- readxl::read_xlsx(paste0(path.Excel, "Forecast Items.xlsx"),
                                 sheet = 1,col_names = T,skip=2) %>%
  rename("Category"=Category...5) %>% 
  filter(Category=="Statistical") %>% 
  select(Item, "TimeWeight" = `Decay Factor`)

if (run.in=="BCN") {
  
  model_params_outlier_method <- model_params_outlier_method %>% 
    rename("Stop"=Stop...63,"Start__1"=Start...62,"Set"=Set...18,"Category"=Category...6,"Level__1"=Level...40,"Set__1"=Set...42,"Constant"=Constant...37) #readxl 1.3.1 column name issue
}

model_params_outlier_method <- model_params_outlier_method %>% 
  filter(Category=="Statistical") %>% 
  dplyr::select(Item,"seasonal" = Cycle, Stop,`Start Date`,Start__1,`Active Baseline`,Set,Model,Factor,Constant,`Auto Regressive`,`Moving Average`,Level__1,Set__1,"other_items"=`Other Items`,"Outlier_type"=Type...66,Data,Detection, Threshold,Window,`Active Baseline`) %>% 
  #Constant...37 and level__1 refer to ARIMA
  
  mutate(Stop=as.Date(Stop),Start=as.Date(Start__1),`Start Date`=as.Date(`Start Date`)) %>% 
  {if (length(grep(x =.$Item,pattern = "PT-AO"))!=0) dplyr::slice(.,-grep(x = Item,pattern = "PT-AO")) else .} %>%
  # slice(-grep(x = Item, pattern = "Market:")) %>%
  mutate(LOC=substring(Item,1,2)) %>% 
  rowwise() %>% 
  mutate(GMID=unlist(strsplit(unlist(strsplit(Item,split = "-")),split=" "))[2]) %>% #create GMID removing new ID structure 
  mutate(DESCRIPT=unlist(stri_split_fixed(str = Item, pattern = "-", n = 2))[2]) %>%
  ungroup() %>% 
  # mutate(ModelType=ifelse(Model=="Rforecast",
  #                         ifelse(Set=="R Model - Trend Decay",paste("ETS",Set__1,"Trend Decay:",Factor),paste("ETS",Set__1)),
  #                         ifelse(Model=="ARIMA",paste("ARIMA (",`Auto Regressive`,Level__1,`Moving Average`,")","Constant:",Constant),
  #                                ifelse(Model=="CrostonsMethod","Croston",Model)))) %>% 
  mutate(Model=ifelse(Model=="Rforecast","ETS",
                      ifelse(Model=="CrostonsMethod","Croston",
                             ifelse(Model == "MultipleLinearRegression", "MLR", Model)))) %>% 
  
  mutate(Model=ifelse(Set=="ARIMA","ARIMA Default",Model)) %>% 
  mutate(Parameter=ifelse(Model=="ETS",paste(Set__1,"Trend Decay:",as.numeric(Factor)),
                          ifelse(Model=="MLR",paste("MLR Trend Decay:",as.numeric(Factor)),
                                 ifelse(Model=="ARIMA",paste("(",`Auto Regressive`,Level__1,`Moving Average`,")","Constant:",Constant),
                                        ifelse(Model=="ARIMA Default",NA,Model))))) %>% 
  dplyr::select(-Start__1,-Set,-Factor,-Constant,-`Auto Regressive`,-`Moving Average`,-Level__1,-Set__1) %>% 
  dplyr::select(LOC,GMID,DESCRIPT,Item,`Active Baseline`,Model,Parameter,everything()) %>% 
  mutate(to_plot=ifelse(Stop<=today(),yes = "NO",no = 
                          ifelse(
                            Outlier_type=="Default" & is.na(other_items),yes = "NO",no = 
                              ifelse(
                                Data=="Moving Average Error" & is.na(other_items) ,yes = "NO",no = "YES"
                              )
                          )
                        )) %>% 
  select(-c("Outlier_type")) %>% 
  mutate(key=paste0(LOC,"-",GMID)) %>% 
  left_join(
    GBU %>% select(GMID.Code,Market.Code, GBU) %>% mutate(LOC=substring(Market.Code,1,2))%>% mutate(key=paste0(LOC,"-",GMID.Code)),
    by=c("key"="key")
  ) %>% 
  select("LOC"=LOC.x,GBU,everything()) %>% 
  select(-c("key","GMID.Code","Market.Code","LOC.y")) %>%
  mutate( GBU=ifelse(is.na(GBU),"",GBU)) %>% 
  mutate(new_code=ifelse(is.na(other_items),yes="NO",no="YES")) %>% 
  select(-c("other_items")) %>% 
  left_join(time_weight, by = c("Item"="Item"))




# Saving ------------------------------------------------------------------

save(model_params_outlier_method, file = paste0(path.Save, "model_params_outlier_method.RData"))


# Sales -------------------------------------------------------------------

#reading volume last 12 months per Forecast Item====
Sales_data<-readxl::read_xlsx(paste0(path.Excel, "Forecast Comparison.xlsx"),
                              sheet = 1,col_names = T,skip=2) %>%
  filter(!is.na(`Item`)) %>% 
  filter(Item%not.in%Angola.products$Item) %>% 
  filter(Item%not.in%pruned.products$Item)
  

if (run.in=="BCN") {
  
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
  
  
  # cleansed_data_before_outlier<-readxl::read_xlsx("//sinsdfs01/regional$/APJ-SC-HUB/2 - Forecasts/9 - Innovation Incubator/Spain test/outlier detection slides/Causal Factor Cleansing - Summary.xlsx",
  #                                                 sheet = 1,col_names = T,skip=1) %>% 
  #   rename("Item"=`Forecast Item`) %>% 
  #   filter(!is.na(Item)) %>% 
  #   filter(Item!="= TOTAL =",
  #          ...3=="CLEANSED") %>% 
  #   mutate(Category...2="Statistical",
  #          Category...3="HistoricalSales",
  #          Type=NA,
  #          data_type="Cleansed History",
  #          "...6"=NA) %>% 
  #   select(-c("...2","...3")) %>% 
  #   select(Item,Category...2,Category...3,Type,data_type,...6,everything())
  
} else {
  
  cleansed_data_before_outlier<-readxl::read_xlsx(paste0(path.Excel, "Statistical Outliers Cleansing.xlsx"),
                                                  sheet = 1,col_names = T,skip=1) %>% 
    rename("data_type"=X__2) %>% 
    filter(data_type=="Cleansed History") %>% 
    rename("Item"=Id, "Category"=Forecast, "Category__1"=Historical)%>% 
    mutate(Type=NA) %>% 
    select(-c("Summary","Configuration")) %>% 
    select(Item,Category,Category__1,Type, data_type, everything() ) %>% 
    rename("X__2"=X__1) %>% 
    filter(Item%not.in%Angola.products$Item) 
  
}
# cleansed_data_before_outlier %>% cbind(select(Sales_raw_def,names(Sales_raw_def)[which(names(Sales_raw_def)%not.in%names(cleansed_data_before_outlier))]))

if (run.in=="BCN") {
  
  Sales_raw_def<- Sales_data %>% 
    rename_at(vars(...5),funs(paste0("data_type"))) %>% 
    filter(data_type=="Raw Actuals") %>% 
    filter(Category...2=="Statistical") %>% 
    filter(Category...3 == "HistoricalSales")  #NEW ACTUALS CATEGORY FOUND IN KINAXIS WITH PATIENT BASED COMMERCIAL EQUIVALENT  
  
  
  sales_cleansed<- Sales_data %>% 
    rename_at(vars(...5),funs(paste0("data_type"))) %>%
    filter(data_type=="Cleansed History")
  
} 



############ FROM HEERE
#in the table outlier detection, there are some gmids without cleansed data.... adding cleansed data coming from forecast comparison into this table

if (run.in=="BCN") {
 
    cleansed_data_before_outlier<-
      cleansed_data_before_outlier %>%
      select(Item,  Category...2, Category...3, Type , data_type, ...6,which(names(.)==names(sales_cleansed)[7]):ncol(.)) %>% 
      rbind(
        sales_cleansed[which(sales_cleansed$Item%not.in%cleansed_data_before_outlier$Item==T),] %>% 
          select(1:which(names(.)==data_cleansing_end_date)) # to modify
      ) %>% cbind(select(Sales_raw_def %>% filter(Category...2=="Statistical"),
                         names(Sales_raw_def)[which(names(Sales_raw_def)%not.in%names(cleansed_data_before_outlier))]))
  
} else {
  
  cleansed_data_before_outlier<-
    cleansed_data_before_outlier %>%
    select(Item,  Category, Category__1, Type , data_type, X__2,which(names(.)==names(sales_cleansed)[7]):ncol(.)) %>% 
    rbind(
      sales_cleansed[which(sales_cleansed$Item%not.in%cleansed_data_before_outlier$Item==T),] %>% 
        select(1:which(names(.)==data_cleansing_end_date))
    ) %>% cbind(select(Sales_raw_def %>% filter(Category=="Statistical"),
                       names(Sales_raw_def)[which(names(Sales_raw_def)%not.in%names(cleansed_data_before_outlier))]))
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#new products are those with less than 12 months and not having realignment
#for some it's false. KINAXIS buckets consider all the points after 1st non 0 point, even if are full 0

new.prod<-product_segmentation_MD %>% 
  filter(Buckets<12 & is.na(`Other Items`)) %>% 
  filter(Item%not.in%Angola.products$Item) 

#if FCST item is a realignment, then data from old is considered cleansed
new.codes<-product_segmentation_MD %>% 
  filter(Buckets<12 & !is.na(`Other Items`))

temp_2<-sales_cleansed %>% 
  filter(Item%in%new.codes$Item) %>% 
  filter(Category...2=="Statistical")

#FOR NEW CODES AFTER REALIGNMENT, SALES OF THE OLD CODE IS SET AS CLEANSED DATA, NOT AS RAW. INCLUDING CLEANSED DATA FOR REALIGNMENTS CODES (new)
#NOT ROBUST IN THE SENSE THAT BOTH TABLES must have THE SAME SORTING
#Consider cleansed data for new codes is not correct, may be cleansed in part

Sales_raw_def[which(Sales_raw_def$Item%in%temp_2$Item),7:ncol(Sales_raw_def)]<-temp_2[,7:ncol(Sales_raw_def)]
cleansed_data_before_outlier[which(cleansed_data_before_outlier$Item%in%temp_2$Item),7:ncol(cleansed_data_before_outlier)]<-temp_2[,7:ncol(cleansed_data_before_outlier)]

#Reading newly updated forecast
raw<-readxl::read_xlsx(paste0(path.Excel, "Forecast Comparison.xlsx"),
                       sheet = "Forecast Comparison",col_names = T,skip=2) 

if (run.in=="BCN") {
  
  raw <- raw %>% 
  rename("X__1"=...5,"X__2"=...6,"Category"=Category...2,"Category__1"=Category...3)  #readxl 1.3.1 column name issue

}

raw <- raw %>% 
  filter(Category=="Statistical") %>% 
  filter(!is.na(Item))  %>% #removing strange GMID without name in Kinaxis
  #filter(X__1=="Forecast") %>% #removing forecast line
  {if (length(grep(x =.$Item,pattern = "PT-AO"))!=0) dplyr::slice(.,-grep(x = Item,pattern = "PT-AO")) else .} %>% #removing angola products
  # slice(-grep(x = Item, pattern = "Market:")) %>%
  mutate(LOC=substring(Item,1,2)) %>% 
  #gsub(unlist(strsplit(unlist(strsplit(a,split = "-")),split=" "))[1],pattern = ":",replacement = "")
  rowwise() %>% 
  mutate(GMID=unlist(strsplit(unlist(strsplit(Item,split = "-")),split=" "))[2]) %>% #create GMID removing new ID structure 
  mutate(DESCRIPT=unlist(stri_split_fixed(str = Item, pattern = "-", n = 2))[2]) %>%
  ungroup() %>% 
  dplyr::select(-c(Category,Category__1,Type,X__2)) %>% 
  dplyr::select(LOC,GMID,DESCRIPT,Item,Type=X__1,names(.)[2:ncol(.)]) %>% 
  filter(paste0(LOC,GMID,DESCRIPT) %in% paste0(model_params_outlier_method$LOC,model_params_outlier_method$GMID,model_params_outlier_method$DESCRIPT))


#raw<-raw[-which(raw$LOC=="Ma"),]# badly configured item code (Market: LU), consider deleting when fixed in Kinaxis

raw$GMID<-as.numeric(raw$GMID) 

col.n <- seq.Date(from=ymd("2014/01/01"),to=ymd(forecast_comparison_end_date),by="month") #make general
col.n <- as.character(col.n)
col.n <- c("LOC","GMID","DESCRIPT","Item","Type",col.n)

colnames(raw) <- col.n

current_Period <- floor_date((Sys.Date()), "month") 
current.date <- floor_date((Sys.Date()), "month") - months(1)


temp_cleansed_before_outlier<-cleansed_data_before_outlier %>% 
  mutate(LOC=substring(Item,1,2)) %>% 
  rowwise() %>% 
  mutate(GMID=unlist(strsplit(unlist(strsplit(Item,split = "-")),split=" "))[2]) %>% #create GMID removing new ID structure 
  mutate(DESCRIPT=unlist(stri_split_fixed(str = Item, pattern = "-", n = 2))[2]) %>%
  ungroup() %>% 
  select(-c("Category...2","Category...3","data_type",...6)) %>% 
  select(LOC, GMID, DESCRIPT, Item, everything()) %>% 
  mutate(Type="Cleansed history before outliers")

#temp_cleansed_before_outlier<-temp_cleansed_before_outlier[-which(temp_cleansed_before_outlier$LOC=="Ma"),]# badly configured item code (Market: LU), consider deleting when fixed in Kinaxis

temp_cleansed_before_outlier$GMID<-as.numeric(temp_cleansed_before_outlier$GMID) 

col.n <- seq.Date(from=ymd("2014/01/01"),to=ymd(forecast_comparison_end_date),by="month") #make general
col.n <- as.character(col.n)
col.n <- c("LOC","GMID","DESCRIPT","Item","Type",col.n)

colnames(temp_cleansed_before_outlier) <- col.n


full_sales <- raw %>% #filter (Type == "Cleansed History") %>% 
  rbind(temp_cleansed_before_outlier) %>%   #binding the data: Raw+Causal factor inclusion by planners
  group_by(LOC,GMID,DESCRIPT,Type,Item) %>% 

  gather("Date","MSales",6:ncol(raw)) %>% 
  ungroup() %>% 
  filter(Type%not.in%c("Forecast")) %>% 
  mutate(Date=ymd(Date)) %>% 
  #dplyr::select(-Type) %>% 
  filter(Date<ymd(current_Period)) %>% 
  mutate(MSales=ifelse(is.na(MSales) | MSales <0,0,MSales)) %>% 
  filter(MSales!=0) %>% #Remove all zeros including those leading zero
  arrange(Date,GMID) %>% 
  group_by(LOC, GMID, DESCRIPT,Type,Item) %>% 
  tidyr::complete(Date = seq.Date(min(Date), current.date, by = "month")) %>% #replace all inbetween zeros
  replace_na(list(MSales=0)) %>%     # replace the NA with 0
  ungroup() %>% 
  mutate(DESCRIPT=ifelse(is.na(DESCRIPT),"NA",DESCRIPT)) %>% 
  select(LOC,GMID,DESCRIPT,Item,Date,Type,MSales) %>% 
  
  mutate(key=paste0(LOC,"-",GMID)) %>% 
  left_join(
    GBU %>% select(GMID.Code,Market.Code, GBU) %>% mutate(LOC=substring(Market.Code,1,2))%>% mutate(key=paste0(LOC,"-",GMID.Code)),
    by=c("key"="key")
  ) %>% 
  select("LOC"=LOC.x,GBU,everything()) %>% 
  select(-c("key","GMID.Code","Market.Code","LOC.y")) %>% 
  mutate( GBU=ifelse(is.na(GBU),"",GBU))



# %>% 
#   spread(Type,MSales) %>% 
#   select(-`<NA>`) %>% 
#   mutate(`Cleansed History`=ifelse(is.na(`Cleansed History`),0,`Cleansed History`)) %>% 
#   mutate(`Raw Actuals`=ifelse(is.na(`Raw Actuals`),0,`Raw Actuals`))




full_forecast <- raw %>% filter (Type == "Forecast") %>% 
  group_by(LOC,GMID,DESCRIPT,Type,Item) %>% 
  gather("Date","MForecast",6:ncol(raw)) %>% 
  ungroup() %>% 
  mutate(Date=ymd(Date),MForecast=round(as.numeric(MForecast))) %>% 
  dplyr::select(-Type) %>% 
  mutate(MForecast=ifelse(Date>(ymd(current_Period)-months(1)),MForecast,NA)) %>% 
  #filter(Date>=ymd(min(full.sales$Date)))
  filter(Date>=ymd(current_Period)) %>% 
  mutate(key=paste0(LOC,"-",GMID)) %>% 
  left_join(
    GBU %>% select(GMID.Code,Market.Code, GBU) %>% mutate(LOC=substring(Market.Code,1,2))%>% mutate(key=paste0(LOC,"-",GMID.Code)),
    by=c("key"="key")
  )%>% 
  select("LOC"=LOC.x,GBU,everything()) %>% 
  select(-c("key","GMID.Code","Market.Code","LOC.y")) %>% 
  mutate( GBU=ifelse(is.na(GBU),"",GBU))


save(full_sales,file=paste0(path.Save, "full_sales.RData"))
save(full_forecast,file=paste0(path.Save, "full_forecast.RData"))


# Past forecast table -------------------------------------------------------
prev.fcst <- readxl::read_xlsx(paste0(path.KPI, "Forecast Comparison.xlsx"),
                               sheet = "Forecast Comparison",col_names = T,skip=2) %>% 
  filter(!is.na(Item))  %>% 
  rename("X__1"=...5,"X__2"=...6,"Category"=Category...2, "Category__1"=Category...3)  %>% 
  filter(X__1=="Forecast") %>% 
  dplyr::select(Item,Type=X__1,names(.)[7:ncol(.)]) 
  
col.pre <- seq.Date(from=ymd("2014/01/01"),to=ymd(forecast_comparison_end_date_prev_month),by="month") #make general
col.pre <- as.character(col.pre)
col.pre <- c("Item","Type",col.pre)

colnames(prev.fcst) <- col.pre

prev_fcst <- prev.fcst %>% 
  group_by(Item,Type) %>% 
  gather("Date","KForecast",3:ncol(prev.fcst)) %>% 
  ungroup() %>% 
  mutate(Date=ymd(Date),KForecast=round(as.numeric(KForecast))) %>% 
  dplyr::select(-Type) %>% 
  filter(Date>current.date)

save(prev_fcst,file=paste0(path.Save, "prev_fcst.Rdata"))
