library(rmgarch)

df_return1= list()
for (i in 1:length(df_equi)){
    df_temp = df_equi[[i]][[2]] %>% makeReturns()
    df_return1[[i]] = df_temp
}


df_return= list()
for (i in 1:length(df_equi)){
  df_temp1 = mutate(df_equi[[i]],Price = df_return1[[i]])
  df_return[[i]] = df_temp1
}


garch(df_return[[1]][[2]])


