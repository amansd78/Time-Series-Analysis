# Time Series Analysis and Forecasting

# Difference Function in R
g <- c(2, 3, 5, 18, 4, 6, 4)
diff(g, lag = 1, differences = 1)
diff(g, lag = 2, differences = 1)
diff(g, lag = 1, differences = 2)

# Basic ARIMA Simulation for Stationary Time Series
xt <- arima.sim(n = 100, list(ar = c(0.5, -0.1), ma = 0.2), sd = 3)
class(xt)
plot(xt)

# Deterministic Trend in Time Series
n <- 50
a <- 1805
c <- 0.5
yt <- arima.sim(list(ar = 0.9, ma = -0.2), n = n)
gt <- a + c * seq(1, n) + yt
plot(gt)

# Convert Dataframe into Time Series
Xt <- read.csv("Xt.csv")
xt <- ts(Xt)
xt1 <- ts(Xt, start = c(2011, 3), end = c(2015, 5), frequency = 12)

# ACF and PACF Analysis
acf(Yt)
pacf(Yt)

# Theoretical ACF and PACF Calculation
ARMAacf(ar = c(0.5, -0.1), ma = 0.2, lag.max = 20)

# Stationarity Analysis and Differencing
t.s <- read.table("testing.stationarity.txt")
acf(t.s)
PP.test(t.s)
XT <- diff(t.s, lag = 1, differences = 1)

# Choosing the Order of Differencing
var(t.s)
var(XT)

# Forecasting and Time Series Decomposition
XX <- arima.sim(n = 1000, list(ar = 0.9))
YY <- 2000 + cumsum(XX)
plot(YY)

decompose(ldeaths, type = "additive")

# Least Square Trend Removal
t <- seq(1, 1000)
z <- lm(YY ~ t)
Y <- 2094.844 - 0.375 * t
lines(t, Y)

# Seasonality Identification using Spectral Analysis
spectrum(ldeaths)
acf(ldeaths)
pacf(ldeaths)

# Forecasting Models: ARIMA and Exponential Smoothing
fit <- arima(data, order = c(0, 0, 3))
p <- predict(fit, n.ahead = 20)

HW <- HoltWinters(series, alpha = 0.7)
es <- predict(HW, n.ahead = 8)

# Multivariate Time Series Analysis
A <- matrix(c(0.2, 0.1, 0.4, 0.3), nrow = 2)
L <- eigen(A)

# Additional Analysis and Extensions (complex scenarios, multivariate models, etc.) can be added here.

# Advanced Time Series Analysis and Forecasting Extensions

# Seasonal ARIMA Modeling
sarima_fit <- auto.arima(data, seasonal = TRUE)
sarima_forecast <- forecast(sarima_fit, h = 24)
plot(sarima_forecast)

# Multivariate GARCH Modeling
library(rugarch)
set.seed(123)
n <- 1000
eps1 <- rnorm(n)
eps2 <- rnorm(n)
data <- matrix(c(eps1, eps2), ncol = 2)
multivariate_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                                mean.model = list(armaOrder = c(2, 2), include.mean = TRUE))
multivariate_garch <- ugarchfit(spec = multivariate_spec, data = data)
summary(multivariate_garch)

# Dynamic Regression in Time Series Forecasting
library(forecast)
data <- msts(data, seasonal.periods = c(12, 24))
fit_reg <- auto.arima(data)
fit_reg_forecast <- forecast(fit_reg, h = 24, xreg = cbind(external_data_for_reg))
plot(fit_reg_forecast)


# Multivariate Time Series Creation
set.seed(123)
data <- matrix(rnorm(200), ncol = 2)
ts_data <- ts(data, start = c(2010, 1), frequency = 12)

# ARIMA Modeling for Multivariate Time Series
fit_multivariate <- VAR(ts_data, p = 2)
summary(fit_multivariate)

# Forecasting with VAR Model
forecast_VAR <- predict(fit_multivariate, n.ahead = 12)

# Plotting Multivariate Forecast
plot(ts_data)
lines(forecast_VAR$fcst[, "V1"], col = "red")
lines(forecast_VAR$fcst[, "V2"], col = "blue")

# Multivariate Time Series Decomposition
decompose_multivariate <- stl(ts_data, s.window = "periodic")

# Plotting Decomposition Components
plot(decompose_multivariate)