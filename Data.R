# library(furrr)
library(data.table)
library(tidyverse)
library(highfrequency)
source("functions/get_data_df.R")

future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
data = get_data_df()
tictoc::toc()

asset_names = names(data)


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
