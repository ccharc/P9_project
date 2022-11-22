library(furrr)
library(data.table)
library(future)
library(dplyr)
library(highfrequency)
library(xts)
library(ggplot2)

get_data_df = function(){
  AUDCAD= read.csv("Data/DAT_NT_AUDCAD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  AUDCHF= read.csv("Data/DAT_NT_AUDCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  AUDJPY= read.csv("Data/DAT_NT_AUDJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  AUDNZD= read.csv("Data/DAT_NT_AUDNZD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  AUDUSD= read.csv("Data/DAT_NT_AUDUSD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  CADCHF= read.csv("Data/DAT_NT_CADCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  CADJPY= read.csv("Data/DAT_NT_CADJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  CHFJPY= read.csv("Data/DAT_NT_CHFJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURAUD= read.csv("Data/DAT_NT_EURAUD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURCAD= read.csv("Data/DAT_NT_EURCAD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURCHF= read.csv("Data/DAT_NT_EURCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURCZK= read.csv("Data/DAT_NT_EURCZK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURGBP= read.csv("Data/DAT_NT_EURGBP_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURHUF= read.csv("Data/DAT_NT_EURHUF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURJPY= read.csv("Data/DAT_NT_EURJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURNOK= read.csv("Data/DAT_NT_EURNOK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURNZD= read.csv("Data/DAT_NT_EURNZD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURPLN= read.csv("Data/DAT_NT_EURPLN_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURSEK= read.csv("Data/DAT_NT_EURSEK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURTRY= read.csv("Data/DAT_NT_EURTRY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  EURUSD= read.csv("Data/DAT_NT_EURUSD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPAUD= read.csv("Data/DAT_NT_GBPAUD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPCAD= read.csv("Data/DAT_NT_GBPCAD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPCHF= read.csv("Data/DAT_NT_GBPCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPJPY= read.csv("Data/DAT_NT_GBPJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPNZD= read.csv("Data/DAT_NT_GBPNZD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  GBPUSD= read.csv("Data/DAT_NT_GBPUSD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  NZDCAD= read.csv("Data/DAT_NT_NZDCAD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  NZDCHF= read.csv("Data/DAT_NT_NZDCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  NZDJPY= read.csv("Data/DAT_NT_NZDJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  NZDUSD= read.csv("Data/DAT_NT_NZDUSD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  SGDJPY= read.csv("Data/DAT_NT_SGDJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDCAD= read.csv("Data/DAT_NT_USDCAD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDCHF= read.csv("Data/DAT_NT_USDCHF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDCZK= read.csv("Data/DAT_NT_USDCZK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDHUF= read.csv("Data/DAT_NT_USDHUF_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDJPY= read.csv("Data/DAT_NT_USDJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDNOK= read.csv("Data/DAT_NT_USDNOK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDPLN= read.csv("Data/DAT_NT_USDPLN_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDSEK= read.csv("Data/DAT_NT_USDSEK_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDSGD= read.csv("Data/DAT_NT_USDSGD_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDTRY= read.csv("Data/DAT_NT_USDTRY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  USDZAR= read.csv("Data/DAT_NT_USDZAR_T_LAST_201801.csv", sep=";", header = FALSE)
  
  ZARJPY= read.csv("Data/DAT_NT_ZARJPY_T_LAST_201801.csv", sep=";", header = FALSE)
  
  
  DF = list("AUDCAD" =AUDCAD,"AUDCHF" = AUDCHF,"AUDJPY" = AUDJPY,"AUDNZD"=AUDNZD,"AUDUSD" = AUDUSD,"CADCHF"=CADCHF,"CADJPY"=CADJPY,"CHFJPY"=CHFJPY,"EURAUD"=EURAUD,"EURCAD"=EURCAD,"EURCHF"=EURCHF,"EURCZK"=EURCZK,"EURGBP"=EURGBP,"EURHUF"=EURHUF,"EURJPY"=EURJPY,"EURNOK"=EURNOK,"EURNZD"=EURNZD,"EURPLN"=EURPLN,"EURSEK"=EURSEK,"EURTRY"=EURTRY,"EURUSD"=EURUSD,"GBPAUD"=GBPAUD,"GBPCAD"=GBPCAD,"GBPJPY"=GBPJPY,"GBPNZD"=GBPNZD,"GBPUSD"=GBPUSD,"NZDCAD"=NZDCAD,"NZDCHF"=NZDCHF,"NZDJPY"=NZDJPY,"NZDUSD"=NZDUSD,"SGDJPY"=SGDJPY,"USDCAD"=USDCAD,"USDCHF"=USDCHF,"USDCZK"=USDCZK,"USDHUF"=USDHUF,"USDJPY"=USDJPY,"USDNOK"=USDNOK,"USDPLN"=USDPLN,"USDSEK"=USDSEK,"USDSGD"=USDSGD,"USDTRY"=USDTRY,"ZARJPY"=ZARJPY)
  
  #Log af DF
  #for (i in 1:length(DF)) {
  #DF[[i]][[2]] <- log(DF[[i]][[2]])
  # i=i+1
  #}
  
  
  ###Mutate datoer, vælger to første vektorer, samt vælger første dag i datasættet
  mutate_data = function(data){
    data %>% dplyr::mutate(
      V1 = as.POSIXct(V1, format= "%Y%m%d %H%M%S")
    ) %>% 
      dplyr::select(DT = V1, Price = V2) %>% 
      # dplyr::filter(as.Date(DT)==as.Date("2018-01-01")) %>% 
      data.table()
  }
  
  df_true = future_map(DF,mutate_data)
  
  df_true
}

future::plan(multisession(workers= availableCores()))
data = get_data_df()

selected_assets = list("AUDNZD", "EURNOK", "EURUSD", "GBPJPY", "GBPUSD", "SGDJPY", "EURPLN", "NZDCAD", "USDCHF", "ZARJPY")


## Plots and saves all dataa
## Plots and saves all data
#for (i in  1:length(df_true)){
 #p = ggplot(data = df_true[[i]], aes(x = DT, y = Price)) +
  #geom_line() + 
  #ggtitle(names(df_true)[i])
  
  #ggsave(p,file=paste0("plot_", names(df_true)[i],".png"), width = 14, height = 10, units = "cm")
#}



# tranforming onto an equidistant grid
get_equi_prices = function(pricetable){ 
  
  # equidistant grid for each asset
  meantime = pricetable %>% pull(DT) %>% diff() %>% mean()
  
  # price process on equidistant grid
  prices = pricetable %>% pull(Price)
  initial_price = prices[1]
  stepfunc = stepfun(x = pricetable %>% pull(DT), y = c(initial_price, prices), f = 0, right = 0)
  
  datetime = as.POSIXct("2018-01-01 17:00:00", tz = "EST") + lubridate::seconds((0:(nrow(pricetable)-1))*meantime)
  tibble( "DT" =  datetime, "Price" = stepfunc(datetime))
  
}


ggplot(data = get_equi_prices(df_true$AUDCAD), aes(x = DT, y = Price)) +
  geom_line()
ggplot(data = df_true$AUDCAD, aes(x = DT, y = Price)) +
  geom_line()


df_equi= future_map(df_true,get_equi_prices)

plot(df_equi$ZARJPY, type  ="l")



### Estimation of integrated volatility
refreshTime(a)

rMRCov(df_true)


HY_func = function(data){
  data %>% rHYCov(data, alignBy = "secs", alignPeriod = 60)
}

HY_true = future_map(df_true,HY_func)
