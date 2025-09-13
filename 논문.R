install.packages("dplyr")
library(dplyr)
library(readr)

data = read_csv("/Users/seoyun/Desktop/학교/4-1/시계열/productivity-statistics-1978-2023.csv")
data =  data %>% select(-Series_title_4, -Series_title_5)

# 불필요한 열 제거
data <- data %>%
  select(-STATUS, -MAGNTUDE, -Subject)

# Labour Index만 필터링
labour_df <- data %>%
  filter(Series_title_1 == "Labour",
         Series_title_3 == "Index",
         !is.na(Data_value),
         Data_value > 0)  # 로그를 위해 음수 제거

# -------------------------------
# Construction
industry_name1 <- "Construction"

df_ind <- labour_df %>%
  filter(Series_title_2 == industry_name1) %>%
  mutate(Year = floor(Period)) %>%
  group_by(Year) %>%
  summarise(Value = mean(Data_value)) %>%
  arrange(Year)

ts_ind1 <- ts(df_ind$Value,
              start = min(df_ind$Year),
              end = max(df_ind$Year),
              frequency = 1)

ts.plot(ts_ind1, main = paste("Labour Index -", industry_name1))
acf(ts_ind1, main = paste("ACF -", industry_name1))
pacf(ts_ind1, main = paste("PACF -", industry_name1))

# 로그 변환
log_ts_ind1 <- log(ts_ind1)
log_diff_ind1 <- diff(log_ts_ind1)
ts.plot(log_diff_ind1, main = paste("Log-Diff -", industry_name1))
acf(log_diff_ind1)
pacf(log_diff_ind1)

# -------------------------------
# Accommodation and Food Services
industry_name2 <- "Accommodation and Food Services"

df_ind <- labour_df %>%
  filter(Series_title_2 == industry_name2) %>%
  mutate(Year = floor(Period)) %>%
  group_by(Year) %>%
  summarise(Value = mean(Data_value)) %>%
  arrange(Year)

ts_ind2 <- ts(df_ind$Value,
              start = min(df_ind$Year),
              end = max(df_ind$Year),
              frequency = 1)

ts.plot(ts_ind2, main = paste("Labour Index -", industry_name2))
acf(ts_ind2, main = paste("ACF -", industry_name2))
pacf(ts_ind2, main = paste("PACF -", industry_name2))

# 로그 변환
log_ts_ind2 <- log(ts_ind2)
log_diff_ind2 <- diff(log_ts_ind2)
ts.plot(log_diff_ind2, main = paste("Log-Diff -", industry_name2))
acf(log_diff_ind2)
pacf(log_diff_ind2)

# -------------------------------
# Financial and Insurance Services
industry_name3 <- "Financial and Insurance Services"

df_ind <- labour_df %>%
  filter(Series_title_2 == industry_name3) %>%
  mutate(Year = floor(Period)) %>%
  group_by(Year) %>%
  summarise(Value = mean(Data_value)) %>%
  arrange(Year)

ts_ind3 <- ts(df_ind$Value,
              start = min(df_ind$Year),
              end = max(df_ind$Year),
              frequency = 1)

ts.plot(ts_ind3, main = paste("Labour Index -", industry_name3))
acf(ts_ind3, main = paste("ACF -", industry_name3))
pacf(ts_ind3, main = paste("PACF -", industry_name3))

# 로그 변환
log_ts_ind3 <- log(ts_ind3)
log_diff_ind3 <- diff(log_ts_ind3)
ts.plot(log_diff_ind3, main = paste("Log-Diff -", industry_name3))
acf(log_diff_ind3,lag.max = 40)
pacf(log_diff_ind3)
