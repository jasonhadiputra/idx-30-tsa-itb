library(readxl)
library(TSA)
library(lmtest)
library(tseries)
library(forecast)
library(FinTS)
library(tidyverse)
library(lubridate)
library(forecast)
library(rugarch)

# Input
(ori_data <- read_excel('C:/Users/jason/Google Drive/Projects/2281AK Time Series Analysis/IDX 30 Historical Data.xlsx'))
data <- ori_data$Close

# Plot
(price <- ggplot(ori_data,aes(x = Date,y = Close)) +
    geom_line() +
    labs(x = "", y = "", caption = "")) + theme(plot.background = element_rect(fill = "magenta")) +labs(x = NULL, y = NULL, caption = NULL)
##price + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2281AK Time Series Analysis/Plot/1 Diagram Garis Data Mingguan Harga Penutupan IDX30.png',
##               width = 4, height = 3, type = 'cairo', dpi = 1000)

# Descriptive statistics
summary(data)

# Checking stationarity
adf.test(data)

# Differentiate to make it stationary
data_diff <- diff(data)
ori_data_diff <- tibble(
  Date = ori_data$Date[-1],
  diff_Close = data_diff
)

(diff_price <- ggplot(ori_data_diff,aes(x = Date,y = diff_Close)) +
    geom_line() +
    labs(x = "", y = "", caption = "")) + theme(plot.background = element_rect(fill = "magenta")) +labs(x = NULL, y = NULL, caption = NULL)
diff_price + ggsave(filename = 'C:/Users/jason/Google Drive/Projects/2281AK Time Series Analysis/Plot/2 Diagram Garis Data Mingguan Hasil Diferensiasi Harga Penutupan IDX30.png',
               width = 4, height = 3, type = 'cairo', dpi = 1000)

adf.test(data_diff)

# Check ACF, PACF, EACF
acf(data_diff,main = 'ACF Harga Penutupan Hasil Diferensi')
pacf(data_diff,main = 'PACF Harga Penutupan Hasil Diferensi')
eacf(data_diff)

# Several models
model_011 <- arima(data, order = c(0,1,1))
arima(data, order = c(1,1,1))
arima(data, order = c(5,1,0))
arima(data, order = c(0,1,5))
arima(data, order = c(5,1,1))
arima(data, order = c(1,1,5))

# Coefficient
coeftest(model_011)

# Diagnostics
acf(residuals(model_011),main='ACF Residu Model ARIMA(0,1,1)')
pacf(residuals(model_011),main='PACF Residu Model ARIMA(0,1,1)')

checkresiduals(model_011)
Box.test(residuals(model_011))

# Check for heteroscedasticity
resid <- residuals(model_011)
ArchTest(resid)

# Check stationarity, ACF and PACF of the squared residuals
resid2 <- resid^2
plot(resid2)
adf.test(resid2)
acf(resid2,main='ACF Kuadrat Residu Model ARIMA(0,1,1)')
pacf(resid2,main='PACF Kuadrat Residu Model ARIMA(0,1,1)')
eacf(resid2)

# Model the residuals as ARCH(2)
(spec <- ugarchspec(variance.model = list(garchOrder = c(2,0)),mean.model = list(armaOrder = c(0,0))))
(modelresid_02 <- ugarchfit(spec,resid))

(modelresid_02 <- garch(resid,order = c(0,2)))
AIC(garch(resid,order = c(0,1)))
AIC(garch(resid,order = c(1,1)))
AIC(garch(resid,order = c(0,2)))
AIC(garch(resid,order = c(0,3)))
AIC(garch(resid,order = c(3,2)))
AIC(garch(resid,order = c(3,3)))

summary(modelresid_02)

# Diagnostics
checkresiduals(modelresid_02)
Box.test(residuals(modelresid_02))
ArchTest(residuals(modelresid_02))

# Recheck the residuals
acf(residuals(modelresid_02),na.action = na.pass)
pacf(residuals(modelresid_02),na.action = na.pass)

# Forecasting
autoplot(forecast(object=data[seq(length(data)-52,length(data))],model=model_011))
forecast(object=data[seq(length(data)-52,length(data))],model=model_011)
forecast(object=data,model=model_011)
ugarchforecast(modelresid_02)
plot(ugarchforecast(modelresid_02))

diff(diff(c(1,2,3,4,5),lag=2))
