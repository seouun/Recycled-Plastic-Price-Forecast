library(dplyr)
library(lubridate)
library(stringr)
library(lmtest)
library(portes)
library(astsa)
library(tseries)

df <- read.csv("recycled_resource_plastic_price_fixed.csv",
               colClasses = c(date = "character"))
df$date <- as.character(df$date)

df_summary <- df %>%
  group_by(date) %>%
  summarise(mean_plastic_price = mean(national_avg, na.rm = TRUE)) %>%
  arrange(date) %>%
  mutate(
    Year = as.integer(str_extract(date, "^\\d{4}")),
    Month = as.integer(str_extract(date, "(?<=\\.)\\d{1,2}"))
  )
df_summary
table(df_summary$Month)


# ts 객체 생성 (시작 연도, 월, 월단위 freq=12)
plastic_ts <- ts(df_summary$mean_plastic_price, start = c(2003,1), frequency = 12)

# 시계열 그래프 그리기
ts.plot(plastic_ts, ylab = "Plastic Price", main = "Monthly Average Plastic Price")

# ACF, PACF 그리기
acf(plastic_ts, main = "ACF of Plastic Price")
pacf(plastic_ts, main = "PACF of Plastic Price")

# 로그변환
log_plas_ts = log(plastic_ts)
ts.plot(log_plas_ts, main = "log plastic_ts")
acf(log_plas_ts, main = "ACF of log plastic_ts")
pacf(log_plas_ts, main = "PACF of log plastic_ts")

#1차 차분
plastic_diff = diff(plastic_ts)
ts.plot(plastic_diff, main = "diff plastic")
acf(plastic_diff, main = "diff ACF")
pacf(plastic_diff, main = "diff PACF")

ts.plot(diff(log_plas_ts), main = 'diff log plastic')
acf(diff(log_plas_ts))
pacf(diff(log_plas_ts))

#############################모델 적합#################################
# ---------------------------
# 원시 시계열: ARIMA(0,1,2)
# ---------------------------
fit_p01 <- arima(plastic_ts, order = c(0,1,2))
fit_p01
coeftest(fit_p01)
acf(resid(fit_p01), lag.max = 40)
tsdiag(fit_p01, gof.lag = 40)
LjungBox(fit_p01, lags = seq(6,40,6))

# ---------------------------
# 원시 시계열: ARIMA(1,1,1)
# ---------------------------
fit_p02 <- arima(plastic_ts, order = c(1,1,1))
fit_p02
coeftest(fit_p02)
acf(resid(fit_p02), lag.max = 40)
tsdiag(fit_p02, gof.lag = 40)
LjungBox(fit_p02, lags = seq(6,40,6))

# ---------------------------
# 원시 시계열: ARIMA(1,1,2)
# ---------------------------
fit_p03 <- arima(plastic_ts, order = c(1,1,2))
fit_p03
coeftest(fit_p03)
acf(resid(fit_p03), lag.max = 40)
tsdiag(fit_p03, gof.lag = 40)
LjungBox(fit_p03, lags = seq(6,40,6))

# ---------------------------
# 원시 시계열: ARIMA(2,1,0)
# ---------------------------
fit_p04 <- arima(plastic_ts, order = c(2,1,0))
fit_p04
coeftest(fit_p04)
acf(resid(fit_p04), lag.max = 40)
tsdiag(fit_p04, gof.lag = 40)
LjungBox(fit_p04, lags = seq(6,40,6))

# ---------------------------
# 원시 시계열: ARIMA(2,1,1)
# ---------------------------
fit_p05 <- arima(plastic_ts, order = c(2,1,1))
fit_p05
coeftest(fit_p05)
acf(resid(fit_p05), lag.max = 40)
tsdiag(fit_p05, gof.lag = 40)
LjungBox(fit_p05, lags = seq(6,40,6))

# ---------------------------
# 원시 시계열: ARIMA(2,1,2)
# ---------------------------
fit_p06 <- arima(plastic_ts, order = c(2,1,2))
fit_p06
coeftest(fit_p06)
acf(resid(fit_p06), lag.max = 40)
tsdiag(fit_p06, gof.lag = 40)
LjungBox(fit_p06, lags = seq(6,40,6))

# ===========================
# 로그 시계열: ARIMA(0,1,2)
# ===========================
fit_l01 <- arima(log_plas_ts, order = c(0,1,2))
fit_l01
coeftest(fit_l01)
acf(resid(fit_l01), lag.max = 40)
tsdiag(fit_l01, gof.lag = 40)
LjungBox(fit_l01, lags = seq(6,40,6))

# ---------------------------
# 로그 시계열: ARIMA(1,1,1)
# ---------------------------
fit_l02 <- arima(log_plas_ts, order = c(1,1,1))
fit_l02
coeftest(fit_l02)
acf(resid(fit_l02), lag.max = 40)
tsdiag(fit_l02, gof.lag = 40)
LjungBox(fit_l02, lags = seq(6,40,6))

# ---------------------------
# 로그 시계열: ARIMA(1,1,2)
# ---------------------------
fit_l03 <- arima(log_plas_ts, order = c(1,1,2))
fit_l03
coeftest(fit_l03)
acf(resid(fit_l03), lag.max = 40)
tsdiag(fit_l03, gof.lag = 40)
LjungBox(fit_l03, lags = seq(6,40,6))

# ---------------------------
# 로그 시계열: ARIMA(2,1,0)
# ---------------------------
fit_l04 <- arima(log_plas_ts, order = c(2,1,0))
fit_l04
coeftest(fit_l04)
acf(resid(fit_l04), lag.max = 40)
tsdiag(fit_l04, gof.lag = 40)
LjungBox(fit_l04, lags = seq(6,40,6))

# ---------------------------
# 로그 시계열: ARIMA(2,1,1)
# ---------------------------
fit_l05 <- arima(log_plas_ts, order = c(2,1,1))
fit_l05
coeftest(fit_l05)
acf(resid(fit_l05), lag.max = 40)
tsdiag(fit_l05, gof.lag = 40)
LjungBox(fit_l05, lags = seq(6,40,6))

# ---------------------------
# 로그 시계열: ARIMA(2,1,2)
# ---------------------------
fit_l06 <- arima(log_plas_ts, order = c(2,1,2))
fit_l06
coeftest(fit_l06)
acf(resid(fit_l06), lag.max = 40)
tsdiag(fit_l06, gof.lag = 40)
LjungBox(fit_l06, lags = seq(6,40,6))


confint(fit_l01)  # 로그 ARIMA(0,1,2) 모형
library(car)
qqPlot(residuals(fit_l01), main = "Q-Q Plot of Residuals")

library(forecast)
fc <- forecast(fit_l01, h = 11)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)

summary(fc)
plot(fc)


# 예측 (로그 단위)
sarima.for(log_plas_ts, 11, 0,1,2, main = "Forecast of Plastic Price (log scale)", plot.all = TRUE)

######## 추가 (2,1,0)모형 예측
confint(fit_l04)  # 로그 ARIMA(2,1,0) 모형
library(car)
qqPlot(residuals(fit_l04), main = "Q-Q Plot of Residuals")

library(forecast)
fc <- forecast(fit_l04, h = 11)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)

summary(fc)
plot(fc)


# 예측 (로그 단위)
sarima.for(log_plas_ts, 11, 2,1,0, main = "Forecast of Plastic Price (log scale)", plot.all = TRUE)

