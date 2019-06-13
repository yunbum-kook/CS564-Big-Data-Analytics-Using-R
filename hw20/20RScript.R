### Time Series Data Analysis ###
# Library
library(hflights); library(data.table); library(tseries); library(forecast);
# subsection 6 is optional

## 1. Introduction to Time Series

## 2. Creating Time Series Objects
# (1) Creating a simple time series
## Using May 1954 as start date:
x = 1:36; td = runif(x)
mytsd = ts(td, start=c(1954, 7), frequency = 12)
mytsd
plot(mytsd) # using 'plot.ts' for time-series plot

# (2) Sample Dataset: hflights
# Reading time series data
head(hflights, 2)
dt = data.table(hflights)
dt[, date := ISOdate(Year, Month, DayofMonth)]
head(dt, 2)

daily <- dt[, list( N = .N,
                    Delays    = sum(ArrDelay, na.rm=TRUE),
                    Cancelled = sum(Cancelled),
                    Distance  = mean(Distance) ), by=date]
head(daily)

## 3. Time Series Visualization
#sorting daily data by date
setorder(daily, date)
plot(ts(daily))

#decomposition of Time Series Data
daily7 = decompose(ts(daily$N, frequency = 7))
plot(daily7)

#weekly seasonality
weeks = setNames(daily7$figure, weekdays(daily$date[1:7]))
weeks

barplot(weeks, col=c('cyan', 'gray', 'magenta',
                     'gray', 'gray', 'magenta', 'magenta'))

## 4. Time Series Stationarity
#Sample Dataset: AirPassengers
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
sm = summary(AirPassengers); sm
boxplot(sm, col='cyan')
abline(h=sm[4])

plot(AirPassengers)
abline(reg=lm(AirPassengers~time(AirPassengers)),
       col='red')

#Autocorrelation Function
acf(AirPassengers)

#Unit root tests
kpss.test(AirPassengers)

#Differencing
AirPass_Diff = diff(AirPassengers)
kpss.test(AirPass_Diff)

## 5. Time Series and Forecasting
# (1) Reading Time Series Data
co2 #time series data

#plot co2 time series
plot(co2, xlab='Year', ylab='CO2 Concentration (ppm)')

# (2) Seasonal Decomposition
fit = st1(co2, s.window="period")
plot(fit)

# (3) Forecasting Time Series
install.packages("forecast")
library(forecast)
m = HoltWinters(co2)
p = predict(m, 50, prediction.interval=TRUE)
plot(m, p)
