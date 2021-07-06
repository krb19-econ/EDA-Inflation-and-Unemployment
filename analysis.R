library(astsa)
library(tseries)
library(tidyverse)
library(forecast)
library(lmtest)
library(readxl)

Inflation <- read_excel("C:/Users/Administrator/Desktop/Project_TS/Inflation.xlsx")
Unemployment <- read_excel("C:/Users/Administrator/Desktop/Project_TS/Unemployment.xlsx")


df_inf <- Inflation
df_unemp <- Unemployment

rm(Unemployment)
rm(Inflation)

View(df_inf)
View(df_unemp)

#One clear issue is that inflation is not taken as rates, but rather as CPI.
#So we need to convert it into rates

#Will also need to convert them into time series objects
#First convert year column into the row names

df_inf <- as.matrix(df_inf)
rownames(df_inf) <- df_inf[,1]
df_inf <- subset(df_inf, select = -Year)

vec_inf <- as.vector(t(df_inf))
vec_inf

ts_cpi <- ts(vec_inf, start = c(2000,1), end =c(2021,4) , frequency = 12)


#Now to obtain inflation rates
df_inf2 <- as.data.frame(vec_inf)
df_inf2 <- df_inf2 %>% dplyr::mutate((pct_change = vec_inf/lag(vec_inf) - 1) * 100)

#Converting new vector of inflation into time series
ts_inf<- ts(df_inf2$`(pct_change = vec_inf/lag(vec_inf) - 1) * 100`, start = c(2000,1), end =c(2021,4) , frequency = 12)
ts_inf <- na.remove(ts_inf)



#Similarly with unemployment data
df_unemp <- as.matrix(df_unemp)
rownames(df_unemp) <- df_unemp[,1]
df_unemp <- subset(df_unemp, select = -Year)

vec_unemp <- as.vector(t(df_unemp))
vec_unemp


ts_unemp <- ts(vec_unemp, start = c(2000,1), end =c(2021,4) , frequency = 12)
ts_unemp <- na.remove(ts_unemp)
ts_unemp




#PLOTS

#CPI
plot.ts(ts_cpi, main = "CPI (base '84)")
plot.ts(ts_inf, main = 'Inflation Rates')
plot.ts(ts_unemp, main = 'Unemployment Rates')

#Combined 
plot.ts(cbind(ts_inf,ts_unemp), plot.type = "single", col = c('blue', 'red'))





#SEASONAL PLOTS
ggseasonplot(ts_inf)
ggseasonplot(ts_unemp)

#Can also do
ggseasonplot(ts_inf, polar = T)




#SCATTERPLOTS
plot(x = ts_unemp, y = ts_inf)



#STATIONARITY

tseries::adf.test(ts_inf)#ADF TEST
tseries::adf.test(ts_inf, k =0)#DF TEST
tseries::pp.test(ts_inf)#PP TEST

#Rejected in all tests. INF is stationary

tseries::adf.test(ts_unemp)#ADF TEST
tseries::adf.test(ts_unemp, k =0)#DF TEST
tseries::pp.test(ts_unemp)#PP TEST
#Not rejected. Need to make unemployment data stationary

#Converting into stationary time series
diff_unemp <- ts(as.vector(ts_unemp) - lag(as.vector(ts_unemp)),
               start = c(2000,1), end =c(2021,4) , frequency = 12)
diff_unemp <- na.remove(diff_unemp)

#Testing the diff data

tseries::adf.test(diff_unemp)#ADF TEST
tseries::adf.test(diff_unemp, k =0)#DF TEST
tseries::pp.test(diff_unemp)#PP TEST
#Rejected for all. Hence this is stationary

#Now unemployment time series is also stationary

plot.ts(cbind(diff_unemp, ts_inf), plot.type = 'single', col=c('blue','red'))

plot.ts(cbind(diff_unemp,ts_unemp), plot.type = 'single', col = c('red','blue'))





#AUTOCORRELATION

#Inflation data
acf(x = ts_inf) #Shows in fraction of years. So 0.5 means 6 months, 1 means a year and so on.
pacf(x = ts_inf)

Acf(ts_inf[1:length(ts_inf)], lag.max = 100) #This would depict the same but inn integers or in number of months
pacf(ts_inf[1:length(ts_inf)], lag.max = 100)

#Can also do
ggAcf(ts_inf, lag.max = 100)



#Suppose did the same for cpi data
Acf(ts_inf[1:length(ts_cpi)], lag.max = 100, na.action = na.pass) 
pacf(ts_inf[1:length(ts_cpi)], lag.max = 100, na.action = na.pass)
#Will find no change. Same as inflation rates 



#For unemployment would be using  the diff data which is stationary
Acf(diff_unemp[1:length(diff_unemp)], lag.max = 84)[1:20] 
ggAcf(diff_unemp, lag.max = 84)
pacf(diff_unemp[1:length(diff_unemp)], lag.max = 84)

#NOTE: By using Acf rather  than acf, remove zero lag. 
