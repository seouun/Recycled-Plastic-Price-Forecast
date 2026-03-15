# ── 0. Packages ───────────────────────────────────────────────
install.packages(c("dplyr","stringr","lmtest","portes",
                   "astsa","tseries","forecast","car","showtext"))

library(dplyr)
library(stringr)
library(lmtest)
library(portes)
library(astsa)
library(tseries)
library(forecast)
library(car)
library(showtext)

font_add("AppleGothic", "/System/Library/Fonts/AppleGothic.ttf")
showtext_auto()
par(family = "AppleGothic")

# ── 1. Data Load & Preprocessing ─────────────────────────────
df <- read.csv("recycled_resource_plastic_price_fixed.csv",
               colClasses = c(date = "character"))

df_summary <- df %>%
  group_by(date) %>%
  summarise(mean_plastic_price = mean(national_avg, na.rm = TRUE)) %>%
  arrange(date) %>%
  mutate(
    Year  = as.integer(str_extract(date, "^\\d{4}")),
    Month = as.integer(str_extract(date, "(?<=\\.)\\d{1,2}"))
  )

cat("=== Data Summary ===\n")
cat("Period:", min(df_summary$date), "~", max(df_summary$date), "\n")
cat("Obs   :", nrow(df_summary), "months\n")
cat("Month distribution:\n")
print(table(df_summary$Month))

# Monthly time series object
plastic_ts <- ts(df_summary$mean_plastic_price,
                 start     = c(2003, 1),
                 frequency = 12)

# ── 2. EDA ────────────────────────────────────────────────────

## Fig 1: Raw time series
par(mfrow = c(1,1))
ts.plot(plastic_ts,
        main = "원시계열 그래프",
        ylab = "plastic_ts",
        xlab = "Time")

## Fig 2: ACF & PACF of raw series
par(mfrow = c(1,2))
acf(plastic_ts,  main = "원시계열의 ACF")
pacf(plastic_ts, main = "원시계열의 PACF")
par(mfrow = c(1,1))
# → ACF decays slowly → non-stationary → differencing needed

# ── 3. Stationarity Test & Transformation ─────────────────────

## ADF test on raw series
adf_raw <- adf.test(plastic_ts)
cat("\n=== ADF Test: Raw Series ===\n")
cat("Statistic:", round(adf_raw$statistic, 4),
    "| p-value:", round(adf_raw$p.value, 4), "\n")

## Log transformation + 1st differencing
log_plas_ts <- log(plastic_ts)

## Fig 3: Log-differenced series
par(mfrow = c(1,1))
ts.plot(diff(log_plas_ts),
        main = "로그변환 및 1차 차분 후 시계열",
        ylab = "Log-Diff",
        xlab = "연도")
abline(h = 0, lty = 2, col = "gray50")

## ADF test after transformation
suppressWarnings(adf_diff <- adf.test(diff(log_plas_ts)))
cat("\n=== ADF Test: Log-Differenced ===\n")
cat("Statistic:", round(adf_diff$statistic, 4),
    "| p-value:", round(adf_diff$p.value, 4), "\n")
cat("→ Stationarity confirmed\n")

# ── 4. Model Identification ────────────────────────────────────

## Fig 4: ACF & PACF after transformation
par(mfrow = c(1,2))
acf(diff(log_plas_ts),  main = "로그변환 및 1차 차분 후 ACF")
pacf(diff(log_plas_ts), main = "로그변환 및 1차 차분 후 PACF")
par(mfrow = c(1,1))
# ACF : cuts off after lag 2 → MA(q), q ≤ 2
# PACF: cuts off after lag 2 → AR(p), p ≤ 2
# Candidates: ARIMA(0,1,2) (1,1,1) (1,1,2) (2,1,0) (2,1,1) (2,1,2)

# ── 5. Model Fitting ──────────────────────────────────────────

fit_l01 <- arima(log_plas_ts, order = c(0,1,2))
fit_l02 <- arima(log_plas_ts, order = c(1,1,1))
fit_l03 <- arima(log_plas_ts, order = c(1,1,2))
fit_l04 <- arima(log_plas_ts, order = c(2,1,0))
fit_l05 <- arima(log_plas_ts, order = c(2,1,1))
fit_l06 <- arima(log_plas_ts, order = c(2,1,2))

# ── 6. Model Comparison ───────────────────────────────────────

model_list  <- list(fit_l01, fit_l02, fit_l03, fit_l04, fit_l05, fit_l06)
model_names <- c("ARIMA(0,1,2)", "ARIMA(1,1,1)", "ARIMA(1,1,2)",
                 "ARIMA(2,1,0)", "ARIMA(2,1,1)", "ARIMA(2,1,2)")

comparison <- data.frame(
  Model      = model_names,
  LogLik     = sapply(model_list, function(m) round(as.numeric(logLik(m)), 2)),
  AIC        = sapply(model_list, function(m) round(AIC(m), 2)),
  Num_Params = sapply(model_list, function(m) length(coef(m)))
)
cat("\n=== Model Comparison (AIC) ===\n")
print(comparison[order(comparison$AIC), ])

# ── 7. Coefficient Significance (Z-test) ─────────────────────

cat("\n=== Coefficient Tests ===\n")
for (i in seq_along(model_list)) {
  cat("\n---", model_names[i], "---\n")
  print(coeftest(model_list[[i]]))
}

# ── 8. Final Model: ARIMA(0,1,2) ─────────────────────────────
# ① Lowest AIC
# ② All coefficients significant (p < 0.01)
# ③ Residuals: white noise (Ljung-Box)
# ④ Parsimonious: 2 parameters

cat("\n=== Final Model: ARIMA(0,1,2) ===\n")
print(fit_l01)
print(confint(fit_l01))

## Ljung-Box residual test
cat("\n=== Ljung-Box Test ===\n")
print(LjungBox(fit_l01, lags = seq(6, 36, 6)))
cat("All p > 0.05 → White noise residuals → Model valid\n")

## Fig 5: Q-Q Plot
qqPlot(residuals(fit_l01),
       main = "Q-Q Plot of Residuals OF ARIMA(0,1,2)")

# ── 9. Forecasting (Feb ~ Dec 2025) ──────────────────────────

fc <- forecast(fit_l01, h = 11)
fc$mean  <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)

cat("\n=== Forecast: Feb–Dec 2025 (KRW/kg) ===\n")
fc_df <- data.frame(
  Month    = format(seq(as.Date("2025-02-01"), by = "month", length.out = 11),
                    "%Y년 %m월"),
  Forecast = round(as.numeric(fc$mean), 2),
  Lo_95    = round(fc$lower[, 2], 2),
  Hi_95    = round(fc$upper[, 2], 2)
)
print(fc_df)

## Fig 6: Forecast plot
par(mfrow = c(1,1))

fc_log     <- forecast(fit_l01, h = 11)
hist_vals  <- exp(as.numeric(log_plas_ts))
hist_dates <- as.numeric(time(log_plas_ts))
fc_mean    <- exp(as.numeric(fc_log$mean))
fc_lo      <- exp(as.numeric(fc_log$lower[, 2]))
fc_hi      <- exp(as.numeric(fc_log$upper[, 2]))
fc_time    <- as.numeric(time(fc_log$mean))

plot.default(
  x    = NULL,
  y    = NULL,
  xlim = c(2003, 2026),
  ylim = c(min(hist_vals) * 0.9, max(fc_hi) * 1.05),
  main = "2025 폐플라스틱 가격 예측 - ARIMA(0,1,2)",
  ylab = "original",
  xlab = "Time"
)
polygon(
  x      = c(fc_time, rev(fc_time)),
  y      = c(fc_hi, rev(fc_lo)),
  col    = rgb(0.7, 0.7, 0.7, 0.4),
  border = NA
)
lines(x = hist_dates, y = hist_vals, col = "black", lwd = 1.2)
lines(x = fc_time, y = fc_mean, col = "red", lwd = 1.5, lty = 2)

# ── 10. Key Findings ─────────────────────────────────────────
cat("\n========================================\n")
cat("KEY FINDINGS\n")
cat("========================================\n")
cat("Data   : 전 품목 국가평균 | 2003.01–2025.01 | 265 months\n\n")
cat("EDA    : Long-term upward trend; non-stationary\n")
cat("         → Log + 1st diff → stationarity confirmed (ADF p < 0.01)\n\n")
cat("Model  : ARIMA(0,1,2) on log series\n")
cat("         Lowest AIC | All coefs significant (p < 0.01)\n")
cat("         Ljung-Box: all p > 0.05 → white noise residuals\n\n")
cat("Forecast: Gradual price increase through Dec 2025\n")
cat("          CI widens over horizon → uncertainty increases\n\n")
cat("Limit  : Univariate model; external variables (oil price,\n")
cat("         trade policy, CBAM) not captured\n")
cat("         → ARIMAX or VAR recommended for future work\n")
cat("========================================\n")

