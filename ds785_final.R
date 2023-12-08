# ==========================
#  import required libraries
# ==========================
library(tidyverse)
library(forecast)
library(fUnitRoots)
library(lmtest)

# ==========================
# import data set
# ==========================
reads <- read.csv("C:/Users/krisk/Documents/UWEC/Cap/data/alltime.csv", header = TRUE)
View(reads)

#########################
### EXPLORATION
#########################

# reformat the date column
reads <- reads %>% 
  mutate(time = as.Date(time))
# filter out barcodes that do not equal 2
# filter out dates greater than 10/31
# - filtering out because the data is incomplete
# - November data was not fully avail at time of capture
reads <- reads %>% 
  filter(barcode == 2) %>% 
  filter(time <= "2023-10-31")
# drop barcode column
reads <- reads %>% 
  subset(select = c("time","count"))

# plot count on timeline
# obvious downward trend and variation in counts throughout the week
reads %>% 
  ggplot(aes(time, count))+
  geom_point(size = 3, alpha = 0.5)+
  geom_smooth(method = lm, se = F)+
  #facet_wrap(~Type)+
  labs(x = "Date",
       y = "Coupon Count",
       title = "Coupon Count by Day")+
  theme_bw()


# =============================
# create time-series object
#==============================

# get start week number and start day for first row in data
start_week <- strftime(reads$time[1], "%V")
start_day <- strftime(reads$time[1], "%u")
# create time series object
# using start week/day objects from above 
# using frequency of 7 for weekly
ts <- ts(reads$count, start = c(start_week,start_day), frequency = 7)
# ts object starts on 29th week, 5th day of the year
# ts object ends on 43rd week, 5th day of the year
# frequency is number of observations per unit of time; 7 for weekly
ts


# ========================
# TIME SERIES EXPLORATION
# ======================== 

# time plot
# notice trend
# patterns due to submission differences throughout the week
# trend may indicate seasonality
autoplot(ts) +
  ggtitle("Time Plot: Coupon Submissions per Day") +
  ylab("Coupons Submitted")

# observed, trend, seasonality, random plot (decomposition of additive time series)
comp_ts <- decompose(ts)
plot(comp_ts)

# due to trend, investigate transformations
# first diff used to remove trend
dy <- diff(ts)
autoplot(dy) +
  ggtitle("Time Plot: Coupon Submissions per Day") +
  ylab("Coupons Submitted")

# with stationary, look for seasonality
# fluctuations happening at same intervals or irregular
# each week gets its own line
# x axis is the day of the week
# high submissions on Monday; makes sense as coupons are submitted on weekend
# remaining week is rather stationary
ggseasonplot(dy) +
  ggtitle("Seasonal Plot: Change in daily submissions") +
  ylab("Change in Submissions")

# mean of data is different per day
# shows change in submissions per day
ggsubseriesplot(dy) +
  ggtitle("Seasonal Plot: Average Submission Change by Day") +
  ylab("Change in Submissions")


# =========================
# MODEL FITTING
# =========================

# ==============
# Seasonal Naive
# ==============

# check residual sd
fit_naive <- snaive(dy)
print(summary(fit_naive))
checkresiduals(fit_naive)
plot(fit_naive)

# ==============
# Expoential Smoothing
# ==============

# ets
# can use original data 
# check sigma
fit_ets <- ets(ts)
print(summary(fit_ets))
checkresiduals(fit_ets)
plot(fit_ets)


# ===============
# ARIMA
# ===============

# can use regular data even if there is a trend
# d=1 is before model is fit, take first diff
# stepwise/approximation: used to save time; dont neet that with one ts
# check sqrt of sigma^2
fit_arima <- auto.arima(ts, d=1, D=1, stepwise=FALSE, approximation=FALSE, trace=TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
plot(fit_arima)

# forecast arima
forecast_arima <- forecast(fit_arima, h=28)
autoplot(forecast_arima)
# zoom in on plot
autoplot(forecast_arima, include=20)

print(summary(forecast_arima))

#================
# Holt Winters
# ===============

fit_hw <- HoltWinters(ts)
print(summary(fit_hw))
checkresiduals(fit_hw)
plot(fit_hw)
# using the forecast library
fit_hw_forecast <- forecast(fit_hw, h=15, level=c(80,95))
plot(fit_hw_forecast)

