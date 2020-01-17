# Model fitted on market cleaned data, outlier detection applied on raw data 
# Packages Loading ====
library(lubridate)
library(tidyverse)
library(stringr)
library(forecast)
library(RDCOMClient)
library(officer)
library(ggplot2);library(ggrepel)
library(stlplus)
library(flextable)

# Market, Date and Paths --------------------------------------------------

market <- "LeadMarkets"
Date <- "Jan - 2020" 
DateQ <- "Dec - 2019"

Sys.setenv(TZ='Asia/Kuala_Lumpur')
Sys.setlocale("LC_TIME", "English")

country <- c("ES", "PT") #Change Location "BE", "ES" ,"Ma" ,"NL", "PT"
gbu <- c("", "GZ", "PRC", "CHC") # "", "CHC", "GZ", "PRC"

path.RDataQ <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/1 - Segmentation_MasterData (Quarterly)/", market, "/", DateQ, "/")
path.RDataM <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/2 - Monthly RDatas (Monthly)/", market, "/", Date, "/")
path.Excel <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/0 - Data/0 - Kinaxis data extractions/", market, "/", Date, "/")
path.Save <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/1 - Monthly Cycle/4 - Outlier Slides Monthly/slides/", market, "/", Date, "/")
path.Funs <- paste0("//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/3 - Functions/")
#path.PPT <- "//sinsdfs01/regional$/APJ-SC-HUB/2 - Forecasts/9 - Innovation Incubator/Spain test/outlier detection slides/OfficeR_Template_v2.pptx" #Template Deprecated
path.PPT <- "//E21flsbcnschub/bcn_sc_hub/3 - Forecast/10 - Kinaxis Operating Cycle/3 - Functions/officer_template_230919.pptx"

# Load data ---------------------------------------------------------------

load(paste0(path.RDataQ, "Segmentation_MasterData.RData"))
load(paste0(path.RDataM, "full_sales.RData"))
load(paste0(path.RDataM, "model_params_outlier_method.RData"))
load(paste0(path.RDataM, "full_forecast.RData"))
load(paste0(path.RDataM, "regressor.table.RData"))
load(paste0(path.RDataM, "total_forecast.RData"))
source(paste0(path.Funs, "apply outlier method only.R"))
source(paste0(path.Funs,"generatefitted.R"))



# Slides generation -------------------------------------------------------


for(TLOC in country){
  for(TGBU in gbu){
  
  cat("Doing ",TLOC," ",TGBU,"\r")

    
    current_Period <- floor_date((Sys.Date()), "month") # current month 
    GROUP<-c("D")      #Change Demand Group
    
    PPT.name<-c(paste0('Outliers ',TLOC[1], " ",TGBU," ",current_Period,".pptx")) 
    date.stamp <- paste(month(Sys.Date(),label = T), year(Sys.Date()))
    start.date.test  = ymd(current_Period)
    end.date.test    = ymd(current_Period) + months(19)
    
    Masterdata = filter(Segmentation_MasterData, LOC %in% TLOC, GBU %in% TGBU) 
    
    full_sales2  = filter(full_sales, LOC %in% TLOC, GBU %in% TGBU) %>% 
      mutate(GMID = as.character(GMID))
    
    details = filter(model_params_outlier_method,  LOC%in%TLOC, GBU %in% TGBU)
    
    full.data = left_join(full_sales2,Masterdata, by= c("Item","LOC","GBU","GMID") ) %>% 
      left_join(., details,   by= c("Item","LOC","GBU","GMID")) %>% 
      filter(Forecastable == "Yes") %>% #this remove focus NULL
      mutate(focus = factor(focus)) %>% 
      arrange(focus) %>%      # to ensure exceptional item appear first in the PPT
      group_by(DESCRIPT) %>% 
      arrange(Date) %>% 
      ungroup()
    
    full.totalfcst = total_forecast %>% 
      filter(LOC==TLOC,Category=="Total") %>% 
      mutate(Date=ymd(Date)) %>% 
      filter(Date>=current_Period)
    
    # ruptures2 = filter(ruptures, LOC %in% TLOC, GBU %in% TGBU)
    
    #Filter out GMID with stop date in the past.
    
    past.stopdate <- filter(full.data, Stop <= start.date.test)
    
    # write.csv(past.stopdate, "Possible Pruned.csv")
    
    full.data <- filter(full.data, !(Item %in% past.stopdate$Item))
    
    # PowerPoint Preparation ====

    Title <- Footer <- paste0("Sales Outlier ",TLOC,"-",TGBU) 
    Outlierpres <- read_pptx(path=path.PPT) 
    
    Outlierpres <- Outlierpres %>%
      add_slide(layout = "Title Slide", master = "Sanofi_Presentation_Blue") %>%
     #ph_with_text(type = "ctrTitle", str = Title) %>%
     #ph_with_text(type = "subTitle", str = date.stamp) %>%
     #ph_with_text(type = "ftr", str = Footer) #Deprecated after a change in the function 
	 ph_with(location = ph_location_type(type = "ctrTitle"), value = Title) %>%
      ph_with(location = ph_location_type(type = "subTitle"), value = date.stamp) %>%
      ph_with(location = ph_location_type(type = "ftr"), value = Footer)
    
    # Sanofi Colors ====
    
    sanofi_Grey <- c(rgb(241, 241, 241, maxColorValue=255),
                     rgb(202, 208, 206, maxColorValue=255),
                     rgb(154, 154, 155, maxColorValue=255),
                     rgb( 89,  97, 105, maxColorValue=255),
                     rgb(  0,   0,   0, maxColorValue=255),
                     rgb(232, 227, 222, maxColorValue=255),
                     rgb(215, 208, 204, maxColorValue=255),
                     rgb(175, 180, 185, maxColorValue=255),
                     rgb( 45,  64,  62, maxColorValue=255),
                     rgb(255, 255, 255, maxColorValue=255))
    sanofi_Secondary <- c(rgb( 62, 144, 208, maxColorValue=255),
                          rgb(143, 110, 170, maxColorValue=255),
                          rgb(190,   0, 107, maxColorValue=255),
                          rgb(  0, 165, 144, maxColorValue=255),
                          rgb( 71, 103,  40, maxColorValue=255),
                          rgb(107, 116, 123, maxColorValue=255))
    sanofi_Theme <- c(rgb( 82,  92, 163, maxColorValue=255),
                      rgb(188, 188,  28, maxColorValue=255),
                      rgb(202, 174, 122, maxColorValue=255))
    
    
    
    full.data.fake<-full.data %>% arrange(focus)
    # Plot each time series for each SKU and inserting them into a PowerPoint slide ====
    
    
    # start of loop where each iteration is each GMID 
    counter = 0
    
    for (GMID_ in unique(full.data$Item)) {    
      
      if(GMID_ == "ES: 471442-CAMPATH 30MG/1ML INJ VL1 US") next
      
      counter = counter + 1
      cat(length(unique(full.data$Item)) - counter,"GMID(s) remaining...","In progress ->", GMID_, "\r")
      
      GMID.details = GMID_
      
      # rupture.dat = filter(ruptures2,  Item  == GMID_) %>% 
      #   select(-c( LOC,GMID,DESCRIPT,Item)) %>% 
      #   rename(QTY = Qty_Rup, TYPE =  Type_of_Rup) %>% 
      #   mutate(Date = paste0(month.abb[month(Date)] ,"'", str_sub(year(Date),3,4)), # mutate Date into a shorter format
      #          QTY = as.character(QTY))  
      
      GMID.dat = filter(full.data,Item == GMID_) %>% 
        mutate(focus.seg = paste0("Focus ",focus))
      
      start.date = max(min(min(GMID.dat$Date[GMID.dat$Type == "Cleansed history before outliers"]),
                           min(GMID.dat$Date[GMID.dat$Type == "Cleansed History"])) 
                       ,GMID.dat$`Start Date`[1],na.rm= T)      # find the start date of the model 
      end.date  = ymd(current_Period+400)                       # End date of forecast draft
      
      end.date.test = ymd(current_Period) + months(19)
      
      # to handle SKU (e.g. discontinue) that has shorter FCST horizon than end.date.test
      
      # end.date.test = min(end.date.test, GMID.dat$Stop)      #do not need to consider GMID.dat$Stop as source file alr consider it.
      
      # If CF exist ====
      GMID.regressor = filter(regressor.table, Item == GMID_ & Date>= start.date)%>% 
        select( Regressor,Date,Quantity ) %>% 
        spread(key = Regressor, value =  Quantity ) %>%        # convert to wide format, one col for each cf
        tidyr::complete(Date = seq.Date(start.date,  current_Period + months(35), by = "month") ) %>%     # add rows for dates with cf value = 0
        replace(is.na(.),0)     # to append zero to all the other months
      
      if (ncol(GMID.regressor)>1) { #regressor exist
        
        ex.CF.date = filter(regressor.table, Item == GMID_ & Date>= start.date & Date<ymd(current_Period)) %>% 
          select(Regressor,Category.y,Date,Quantity ) %>% 
          rowwise() %>%
          mutate(dummy.detect= unlist(strsplit(Regressor, " "))[1]) %>%
          mutate(detect.reg = ifelse(dummy.detect=="Seas",Seasonal,
                                     ifelse(dummy.detect=="Trend",Trend,"True Regressor"))) %>% 
          ungroup()
        
        CF.date = ex.CF.date[ex.CF.date$Quantity!=0 & ex.CF.date$detect.reg=="True Regressor","Date"] 
        
        CF.table = ex.CF.date %>%
          slice(which(!is.na(.$Quantity))) %>% 
          slice(which(.$Quantity!=0)) %>% 
          group_by(Regressor) %>% 
          summarise(StartDate = as.character(min(Date,na.rm=T)), EndDate= as.character(max(Date,na.rm=T))) %>% 
          ungroup() %>% 
          as.data.frame() 
          
        
      } else {
        
        CF.date = NULL
        
      }
      
      #Forecast
      GMID.forecast = filter(.data = full_forecast,Item == GMID_)
      
      Future  <- data.frame( Startdate = filter(GMID.forecast,Date <= end.date.test) %>% pull(Date),
                             fcst      = filter(GMID.forecast,Date <= end.date.test) %>% pull(MForecast))
      
      TotalFcst <- filter(full.totalfcst, Item==GMID_) %>% 
        dplyr::select(Startdate=Date,TotalFcst=MForecast)
      
      # Raw Data
      
      Past.raw = filter(full.data,Item == GMID_ & Type == "Raw Actuals" & Date >= ymd(start.date)) %>% 
        select(GMID = Item , Startdate = Date, Qty = MSales)
      
      # Mkt.Cleaned.outlier -->	Raw Data + market cleansed ( using causal factor) + outlier detection/handling by kinaxis (obey user input Start date)  
      
      Mkt.Cleaned.outlier = filter(full.data,Item == GMID_ & Type == "Cleansed History" & Date >= ymd(start.date)) %>% 
        select(GMID = Item , Startdate = Date, Qty = MSales)
      
      Mkt.Cleaned.outlier.ts = ts(Mkt.Cleaned.outlier$Qty, 
                                  start = c(year(min(Mkt.Cleaned.outlier$Startdate)), month(min(Mkt.Cleaned.outlier$Startdate))),
                                  end   = c(year(current_Period) , month(current_Period)-1),
                                  frequency = 12)
      
      # Mkt.Cleaned.only  --> 2)	Raw Data + market cleansed ( using manual adjustment)   
      
      Mkt.Cleaned.only = filter(full.data,Item == GMID_ & Type == "Cleansed history before outliers" & Date >= ymd(start.date)) %>% 
        select(GMID = Item , Startdate = Date, Qty = MSales)
      
      Mkt.Cleaned.only.ts = ts(Mkt.Cleaned.only$Qty, 
                                  start = c(year(min(Mkt.Cleaned.only$Startdate)), month(min(Mkt.Cleaned.only$Startdate))),
                                  end   = c(year(current_Period) , month(current_Period)-1),
                                  frequency = 12)
      
      # Generate fitted values  =====
      
      if (GMID.dat$Model[1]=="Croston") {
        
        x_lm = ts(rep(Future$fcst[1],length(Mkt.Cleaned.outlier.ts)), 
                  start = c(year(min(Mkt.Cleaned.outlier$Startdate)), month(min(Mkt.Cleaned.outlier$Startdate))),
                  end   = c(year(current_Period) , month(current_Period)-1),
                  frequency = 12)
        
      } else {
        
        Rgen_lm = generate.fitted(timeseries = Mkt.Cleaned.outlier.ts,
                                  parameter  = GMID.dat$Parameter[1],
                                  model      = GMID.dat$Model[1], 
                                  TimeWeight = unique(GMID.dat$TimeWeight),
                                  seasonal = ifelse(unique(GMID.dat$seasonal) == 12, T , F ),
                                  regressor = GMID.regressor)
        
        x_lm = ts(Rgen_lm$fitted,
                  frequency = 12, 
                  start = c(year(min(Mkt.Cleaned.outlier$Startdate)), month(min(Mkt.Cleaned.outlier$Startdate))),
                  end   = c(year(current_Period-months(1)) , month(current_Period-months(1))))
        
      }
      
      #Outlier based on cleansed data before outlier
      
      ts.df = tibble(Date = Mkt.Cleaned.only$Startdate,Cleansed = as.numeric(Mkt.Cleaned.only.ts)) %>%                            # merge the raw data with the cf
        mutate(cf.exist = ifelse(Date %in% CF.date$Date,"CF","No CF")) %>%  # Identify points with cf
        mutate(no.cf.ts = ifelse( cf.exist =="CF" , NA, Cleansed))    # identify points with CF and replace with NA
      
      # check if there is at least one data point w/o cf for each month 
      
      # check = mutate( ts.df, month = month(Date) ) %>% 
      #   filter(cf.exist =="No CF")
      
      # missing.data = !all( (1:12) %in% check$month)
      
      
      if (nrow(ts.df)!=0) {                  # if there is no error in the outlier detection/handling done by kinaxis, run the same outlier detection in R
        # Mkt.Cleaned.only.ts = ts(Mkt.Cleaned.only$Qty,
        #                          start = c(year(min(Mkt.Cleaned.only$Startdate)), month(min(Mkt.Cleaned.only$Startdate))),
        #                          end   = c(year(current_Period) , month(current_Period)-1),
        #                          frequency = 12)

        ts.df.ts = ts(ts.df$no.cf.ts,
                                 start = c(year(min(ts.df$Date)), month(min(ts.df$Date))),
                                 end   = c(year(current_Period) , month(current_Period)-1),
                                 frequency = 12)
        
        # Outlier handling =====
        
        initial.DEFICIENCY=try(apply.outlier.method(time.series    = ts.df.ts,
                                                    outlier.method = GMID.dat$Detection[1] ,
                                                    data.rule      = GMID.dat$Data[1],
                                                    threshold.val  = GMID.dat$Threshold[1],
                                                    MovAvg.n       = GMID.dat$Window[1],
                                                    causal.factor  = length(GMID.regressor)>1    # Boolean for presence of cf
        ) ,silent = TRUE) 
        
        if(class(initial.DEFICIENCY) != "try-error") {
          
          cleaned.outlier = apply.outlier.method(time.series = ts.df.ts,
                                                 outlier.method = GMID.dat$Detection[1] ,
                                                 data.rule      = GMID.dat$Data[1],
                                                 threshold.val  = GMID.dat$Threshold[1],
                                                 MovAvg.n       = GMID.dat$Window[1],
                                                 causal.factor  = length(GMID.regressor)>1)      # Boolean for presence of cf
          
        } else {
          
          cleaned.outlier = NULL
          
          cleaned.outlier$upper = ts(NA,start = c(year(min(Mkt.Cleaned.only$Startdate)), month(min(Mkt.Cleaned.only$Startdate))),
                                     end   = c(year(current_Period) , month(current_Period)-1),
                                     frequency = 12)
          
          cleaned.outlier$lower = ts(NA,start = c(year(min(Mkt.Cleaned.only$Startdate)), month(min(Mkt.Cleaned.only$Startdate))),
                                     end   = c(year(current_Period) , month(current_Period)-1),
                                     frequency = 12)
          
        }
        
        # Combine fitted and CI into a dataframe with the entlre length of Mkt.Cleaned.outlier, truncate  Mkt.Cleaned.only.ts if it's longer than Mkt.Cleaned.outlier,
        
        fitted = data.frame( fitted =as.numeric(x_lm),
                             StartDate =  seq.Date(min(Mkt.Cleaned.outlier$Startdate), ymd(start.date.test)-months(1), by="m") ) %>% 
          left_join(., data.frame(upper = cleaned.outlier$upper,
                                  ConfidenceLevel = cleaned.outlier$lower,
                                  StartDate = seq.Date(min(Mkt.Cleaned.outlier$Startdate) , ymd(start.date.test)-months(1), by="m")),
          by = "StartDate")
        
      } else {                          # if there is error in the outlier detection/handling done by kinaxis, no outlier detection is run in R too
        
        fitted  <- data.frame( 
          fitted = as.numeric(x_lm),    
          upper = NA,   
          ConfidenceLevel = NA, 
          StartDate =  seq.Date(min(Mkt.Cleaned.outlier$Startdate), ymd(start.date.test)-months(1), by="m") )
        
      }
      
      dtf.sales <- full_join(Mkt.Cleaned.outlier,Mkt.Cleaned.outlier,by=c("GMID","Startdate")) %>% 
        rename(Raw = Qty.x, Cleansed = Qty.y)
      
      if (nrow(Past.raw) != 0) {
      
      dtf.sales[dtf.sales$Startdate==(ymd(current_Period)-months(1)),"Raw"]=Past.raw[Past.raw$Startdate==(ymd(current_Period)-months(1)),"Qty"] 
         
      }
      
      dtf.sales <- dtf.sales %>% mutate(is.cleansed = ifelse(Raw!=Cleansed,1,0)) 
      
      dtf <- full_join(dtf.sales, Future, by="Startdate") %>%
        transmute(Startdate = Startdate,is.cleansed=is.cleansed, Raw=Raw, Cleansed = Cleansed,Forecast = fcst)
      
      truncated.dtf =  filter(dtf, Startdate >= (current_Period - months(3*12)))   # Display history only up to the past 3 years
      
      combined.dat = merge(truncated.dtf , fitted , by.x = "Startdate", by.y = "StartDate", all.x = T) %>%
        merge(.,TotalFcst,by="Startdate",all.x=T) %>% 
        mutate( outlier = ifelse(  Raw >  upper |  Raw < ConfidenceLevel,
                                   yes  = Raw,
                                   no = NA)) %>% 
        gather("Type","QTY",3:9) %>% 
        mutate(Type = ifelse( Type == "Cleansed", "zCleansed" , Type)) %>% 
        mutate(cf.exist= ifelse(Startdate %in% CF.date$Date & Type=="zCleansed","CF","No CF")) %>% 
        mutate(QTY=ifelse(Type=="outlier" & Startdate<(ymd(current_Period)-months(1)),NA,QTY)) %>% 
        mutate(QTY=ifelse(Type=="zCleansed"&Startdate==(ymd(current_Period)-months(1)),NA,QTY))
      
      
      Qty_Range = range(c(truncated.dtf$Raw, truncated.dtf$Forecast,truncated.dtf$Cleansed,truncated.dtf$TotalFcst), na.rm=TRUE)
      Qty_Range[1] = Qty_Range[1] - (0.1 * Qty_Range[1])
      Qty_Range[2] = Qty_Range[2] + (0.1* Qty_Range[2])
      
      #start plot====
      
      
      temp.plot<- ggplot(combined.dat, mapping = aes(x = Startdate, y = QTY)) +
        geom_line ( aes(color = Type, linetype= Type) , size = 0.6 ) +
        coord_cartesian(ylim = Qty_Range)+ 
        scale_linetype_manual(values= c("dashed","solid","solid","solid","solid" ,"dashed","solid"),# choose the design of the line  
                              guide = F ) +                                                         # hide the legend for linetype
        geom_point( data = filter(combined.dat , Type %in% c("Raw","zCleansed")) ,aes(color = cf.exist), shape = 18 ,  size = 2.5) +
        geom_text(data = filter(combined.dat, cf.exist == "CF", Type == "zCleansed") , aes(label = cf.exist), nudge_x = 20) +
        theme_bw()  +   
        theme(legend.position=c(0.5,0.95),legend.margin=margin(t = 0,b = 0, unit='mm'),
              legend.box = "horizontal",
              legend.direction = "horizontal", legend.key.size= unit(0.9, "cm"), legend.title=element_blank(),
              legend.background = element_rect(fill="grey97"),
              legend.key = element_rect(fill = "transparent", colour = "transparent"),
              legend.text  = element_text(size=10, face="bold"),
              axis.title.y = element_text(colour = sanofi_Theme[1], face = "bold"),
              axis.title.x = element_blank(),
              panel.grid.minor = element_line(colour="white", size=1),
              panel.grid.major.x = element_line(size=1),
              panel.grid.major.y = element_blank(),
              axis.text.x = element_text(color = sanofi_Theme[1], angle=90, hjust = 0.5 ), 
              axis.text.y = element_text(color = sanofi_Theme[1]),
              plot.title  = element_text(color = sanofi_Theme[1], face="bold", size = 14,hjust = 0.5))+
        scale_x_date(date_breaks = "3 month", date_labels =  "%b %y",expand = c(0,0)) +
        geom_vline( aes(xintercept = as.numeric(ymd(start.date.test)))) +
        scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
      
      
      
      if("CF" %in% combined.dat$cf.exist ){     # if there is cf(s) for the months that will appear in the plot 
        temp.plot  = temp.plot +
          scale_colour_manual(breaks = c("ConfidenceLevel","fitted","Raw","zCleansed","Forecast","TotalFcst"),      # to choose which Type to appear in legend
                              labels = c("ConfidenceLevel","Fitted","Raw","Cleansed","BaseFcst","TotalFcst"),       # state the name to appear on the legend
                              values=c("darkgreen","red", "grey65",sanofi_Secondary[3],sanofi_Secondary[1],"green3","steelblue1","red", sanofi_Secondary[1])) + # choose colour
          guides(colour=guide_legend(nrow=1)) 
      } else {               # if no cf for the months that will appear in the plot  #"darkgreen","red","black", sanofi_Secondary[3],sanofi_Secondary[1],sanofi_Secondary[1],sanofi_Secondary[1], "red"
        temp.plot  = temp.plot +
          scale_colour_manual(breaks = c("ConfidenceLevel","fitted","Raw","zCleansed","Forecast","TotalFcst"),      # to choose which Type to appear in legend
                              labels = c("ConfidenceLevel","Fitted","Raw","Cleansed","BaseFcst","TotalFcst"),       # state the name to appear on the legend
                              values=c("red", "grey65",sanofi_Secondary[3],sanofi_Secondary[1],"green3","steelblue1","red", sanofi_Secondary[1])) + # choose colour
          guides(colour=guide_legend(nrow=1))
      }
    
      # If presence of raw data being cleansed by market
      
      # if( any(combined.dat$is.cleansed !=0,na.rm = T)){
      #   temp.plot = temp.plot +
      #     geom_text(data = filter(combined.dat, Type == "Raw",  is.cleansed==1 ) , aes(label = "raw"), nudge_y = -abs(diff(Qty_Range))*0.01,size= 3,fontface = "bold")
      # }
      
      # Cleansed data labeling and outliers
      if(  any( !is.na(filter(combined.dat,Type== "outlier")$QTY) ,na.rm = T)  &    
           (nrow(Mkt.Cleaned.only)!=0)){ 
        
        
        temp.plot<- temp.plot+
          geom_point(aes(alpha = "",x = Startdate,y = -100000),pch = 0 , colour = "red", size= 6,stroke = 2) +   # Hack to make the outlier appear as legend
          geom_point(      data = filter(combined.dat,Type== "outlier", !is.na(QTY)), aes(x = Startdate, y = QTY  ), pch = 0 , colour = "red", size= 6,stroke = 1.5) +
          scale_alpha_manual(values = 1,labels="outlier") +
          labs(alpha = "outlier")
      }
      
      
      # Horizontal line for QTY = 0
      if(any(filter(combined.dat,Type=="zCleansed")$QTY ==0,na.rm=T)){
        temp.plot<-temp.plot+
          geom_hline(yintercept =0,lwd=0.5)
      }
      
      
      # If there is rupture or OOS 
      
      
      #extracting additional information
      
      if (GMID.dat$LSD[1]=="Yes") {
        
        add.info <-c(paste('Model:',ifelse(GMID.dat$Model[1]=="MultipleLinearRegression", "MLR", GMID.dat$Model[1]),
                           "| Outlier:",paste(GMID.dat$Data[1],GMID.dat$Detection[1]),
                           "|", "LSD",
                           "|", paste(GMID.dat$focus.seg[1]),
                           "|", paste(GMID.dat$Segmentation[1]),
                           "| New Code:", paste(GMID.dat$new_code[1]),
                           "| Baseline:",paste(GMID.dat$`Active Baseline`[1])))
        
      } else {
        
        add.info <-c(paste('Model:',ifelse(GMID.dat$Model[1]=="MultipleLinearRegression", "MLR", GMID.dat$Model[1]),
                           "| Outlier:",paste(GMID.dat$Data[1],GMID.dat$Detection[1]),
                           "|", paste(GMID.dat$focus.seg[1]),
                           "| New Code: ", paste(GMID.dat$new_code[1]),
                           "| Baseline:",paste(GMID.dat$`Active Baseline`[1])))
        
      }
      
      # Write the graph on a slide
      
      #OfficeR
      Outlierpres <- Outlierpres %>%
        add_slide(layout = "Two Content Row", master = "Sanofi_Presentation_Blue") %>%
        ph_with(location = ph_location_type(type = "title"), value = GMID.details) %>%
        ph_with_gg(value = temp.plot,type = "body",index = 4) %>%
        #ph_with(location = ph_location_type(type = "body"), value = add.info, index = 1) %>%   # seems not working
		ph_with(value = add.info, location= ph_location_label(ph_label = "Text Placeholder 7")) %>%
        #ph_with(location = ph_location_type(type = "body"), value = "Outlier detected, please leave your comment below",index = 2) 
		ph_with(value = "Outlier detected, please leave your comment below", location = ph_location_type(position_right=F))
      
      
      
      # if( nrow(rupture.dat)!=0) {
      #   flex.rupture = flextable(data = rupture.dat,cheight = c(0.001)) %>% 
      #     align( align = "center", part = c("all")) %>%
      #     fontsize(part ="header",size = 9) %>%
      #     fontsize(part ="all",size = 7)%>%
      #     bold( part = "all")%>% 
      #     padding(padding.top = 0.01,padding.bottom = 0.01, part ="all") %>%
      #     width(width = c(0.5,0.5,0.6,0.7,3.1)) %>% 
      #     border_outer(border = fp_border(width=1,color="black"), part ="all")
      #   
      #   Outlierpres <- Outlierpres %>% ph_with_flextable(value = flex.rupture,type = "body",index = 3)
      # }
      
      if(length(GMID.regressor)>1) {
        flex.CF = flextable(data = CF.table,cheight = c(0.001)) %>%
          align( align = "center", part = c("all")) %>%
          fontsize(part ="header",size = 9) %>%
          fontsize(part ="all",size = 7)%>%
          bold( part = "all")%>%
          padding(padding.top = 0.01,padding.bottom = 0.01, part ="all") %>%
          width(width = c(2.5,0.8,0.8)) %>%
          border_outer(border = fp_border(width=1,color="black"), part ="all")

        Outlierpres <- Outlierpres %>% ph_with_flextable(value = flex.CF,type = "body",index = 3)
      }
      
      
    }# end of loop
    
    # Write the Powepoint Document =====
    print(Outlierpres,target=paste0(path.Save,PPT.name))
    
  }
  
  # # JIRA ticket
  # project<-"APJ"
  # summary<-paste0("Base Forecast Communication ","-",TLoc,"-", date.stamp)
  # description<-list(paste0("Causal Factor Collection and validation","-",TLoc,"-", date.stamp),
  #                   "This email is generated automatically by R to raise tickets in JIRA.")
  # Forecasting.selection<-"Causal Factor Collection and validation"
  # Requestor.Country<-substring(TLoc[1],1,2)
  # Multiplicator<-1
  # 
  # #loading JIRA open and close ticket function====
  # source("//sinsdfs01/regional$/APJ-SC-HUB/2 - Forecasts/9 - Innovation Incubator/Projects/JIRA/Fcst open and close.R")
  # 
  # Fcst.open.and.close(
  #   project = project,
  #   summary = summary,
  #   description = description,
  #   Forecasting.selection = Forecasting.selection,
  #   Requestor.Country = Requestor.Country,
  #   Multiplicator = Multiplicator,Full.path = T,
  #   verbose = T
  # )
}



