library(ggplot2)
library(rio)
library(readxl)
library(zoo)
library(forecast)
library(DataExplorer)
library(tidyverse)
library(tseries)
library(tidyr)

unemployment <- read_excel("C:/Users/Administrator/Desktop/Inflation and Unemployment/unemp.xlsx")

inflation <- read_excel("C:/Users/Administrator/Desktop/Inflation and Unemployment/inf.xlsx")

#Transforming into long format


df <- unemployment %>% tidyr::gather(year, unemp, -month)

df1 <- inflation %>% tidyr::gather(year, value , -month)

df$cpi <- df1[3]

rm(df1)
#To get the inflation rates 
vec_cpi <- as.vector(as.matrix(df$cpi))
df2 <- as.data.frame(vec_cpi)
df2 <- df2 %>% dplyr::mutate(rate = ((vec_cpi/dplyr::lag(vec_cpi)) - 1) * 100)

df$inf <- df2[2]

rm(df2)
rm(vec_cpi)


#Creating EDA Report
df %>% DataExplorer::create_report(
  output_file = 'inf_unemp_eda_report',
  output_dir = "C:/Users/Administrator/Desktop/Inflation and Unemployment",
  report_title = 'EDA Report - US Inflation and Unemployment Rates'
)

str(df)
#It is a tibble. Convert into dataframe
data1 <- data.frame(df)

str(data1)

summary(data1)


#EDA CPI

#Now create a time series
cpi <- ts(data1$cpi, start = c(2000,1), end = c(2021,4), frequency = 12)


#Graphs - CPI
plot.ts(cpi)

#Will now create a yearly moving average

data1$cpi_ma_year <- ma(data1$cpi, order = 12)

#Make a time series for the moving average also
cpi_ma <- ts(data1$cpi_ma_year, start = c(2000,1), end = c(2021,4), frequency = 12)

plot.ts(cpi_ma)

#Plotting them together

plot.ts(cbind(cpi,cpi_ma), plot.type = 'single', col = c('red','blue'))
legend('topleft', legend = c('CPI', 'CPI_Yearly MA'),
       col = c('red','blue'), lty = 1 )



#Decomposition of CPI

decomp = stl(cpi, "periodic")
#Also take the deseasonalised dara
deseason_cpi <- seasadj(decomp)

plot(decomp)
plot(stl(deseason_cpi,"periodic"))

#However we can't be sure of the share in explanation in differences
#The bars on the right do that. Trend accounts for almost all the variation

apply(stl(cpi, 'periodic')$time, 2, var)/var(cpi)
#0.99 explained by trend

#Doing the same for deseasonalised data
apply(stl(deseason_cpi, 'periodic')$time, 2, var)/var(deseason_cpi)
#Again most of it is explained by the trend




#STATIONARITY
adf.test(cpi)

#Will need to perform differencing 
cpi_diff1 <- diff(cpi, differences = 1)
cpi_diff1
adf.test(cpi_diff1)

#For thew deasonalised data
adf.test(deseason_cpi)

cpi_deseas_diff <- diff(deseason_cpi, differences = 1)

adf.test(cpi_deseas_diff)
#Looking at these plots
plot.ts(cpi_diff1)
plot.ts(cpi_deseas_diff)




#Looking at ACF
Acf(cpi)
#Most probably a first lag. Geometric plots often sign of AR 1 process
#Further look at pacf
pacf(cpi)
#Now clear that it is the effect of the first lag. Other were due to the combined effect


#Taking a look at the differenced data of cpi
Acf(cpi_diff1, lag.max = 100)
Pacf(cpi_diff1, lag.max = 100)
#Here there is a significant positive correlation in a 1 period lag and negative in 2 month lag. 
#Large correlation in ACF and PACF for 1 period lag




#Now, we'll move to working on the inflation data

inf <- ts(data1$inf, start = c(2000,1), end = c(2021,4), frequency = 12)

#Looking at its plot
plot.ts(inf)
#Note that it shows quite a similar pattern as that of differenced CPI data

#Similar to CPI, form a moving average for inflation
data1$inf_ma_year <- ma(data1$inf, order = 12)

#Make a time series for the moving average also
inf_ma <- ts(data1$inf_ma_year, start = c(2000,1), end = c(2021,4), frequency = 12)

plot.ts(inf_ma)

#Plotting them together

plot.ts(cbind(inf,inf_ma), plot.type = 'single', col = c('red','blue'))
legend('topleft', legend = c('Inflation Rate', 'Inflation Yearly MA'),
       col = c('red','blue'), lty = 1 )

#DECOMPOSITION
decomp_inf = stl(na.omit(inf), "periodic")
plot(decomp_inf)

apply(stl(na.omit(inf), 'periodic')$time, 2, var)/var(na.omit(inf))
#Now a large portion is explained by random fluctuations


#Also take the deseasonalised data
deseason_inf <- seasadj(decomp_inf)

apply(stl(deseason_inf, 'periodic')$time, 2, var)/var(deseason_inf)
#Here, random fluctuations again account for most of the variations

plot(stl(deseason_inf,"periodic"))




#STATIONARITY
adf.test(na.omit(inf))
#It is sttionary already


#ACF & PACF Plots

Acf(inf)
pacf(na.omit(inf))
#As expected similar to that of CPI differenced data since both measuring change



#One thing we left out. What happens when we look at ACF of deasonalized data



#                     UNEMPLOYMENT RATES

unemp <- ts(data1$unemp, start = c(2000,1), end = c(2021,4), frequency = 12)

#Looking at its plot
plot.ts(unemp)
#Huge incrceases in 2009-10 and 2020. Indicative of GFC and Covid

#Moving average
data1$unemp_ma_year <- ma(data1$unemp, order = 12)

#Make a time series for the moving average also
unemp_ma <- ts(data1$unemp_ma_year, start = c(2000,1), end = c(2021,4), frequency = 12)

plot.ts(unemp_ma)

#Plotting them together

plot.ts(cbind(unemp,unemp_ma), plot.type = 'single', col = c('red','blue'))
legend('topleft', legend = c('Unemployment Rate', 'Unemployment Yearly MA'),
       col = c('red','blue'), lty = 1 )

#DECOMPOSITION
decomp_unemp = stl(unemp, "periodic")
plot(decomp_inf)

apply(stl(na.omit(inf), 'periodic')$time, 2, var)/var(na.omit(inf))
#Now a large portion is explained by random fluctuations


#Also take the deseasonalised data
deseason_unemp <- seasadj(decomp_unemp)

apply(stl(deseason_inf, 'periodic')$time, 2, var)/var(deseason_inf)
#Here, random fluctuations again account for most of the variations

plot(stl(deseason_inf,"periodic"))




#STATIONARITY
adf.test(unemp)
#The unemployment rates do not come out to be stationary.
#Will need to takae difference
unemp_diff1 <- diff(unemp, differences = 1)
plot.ts(unemp_diff1)
#This actually shows changes in unemployment rates each month

adf.test(unemp_diff1)

#ACF & PACF Plots

Acf(unemp)
pacf(unemp)
#Highly correlated with 1 period lag

Acf(unemp_diff1)
pacf(unemp_diff1)
#None of the differenced data comes out to be significant
